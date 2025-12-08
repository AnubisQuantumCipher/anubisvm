--  Drone Registry Contract Implementation
pragma SPARK_Mode (On);

package body Drone_Registry with
   SPARK_Mode => On,
   Refined_State => (Registry_State => (
      Drones, Drone_Count, Flight_Logs, Log_Count,
      Geofences, Geofence_Count, Admin_Addr))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   type Drone_Index is range 0 .. Max_Drones - 1;
   type Drone_Array is array (Drone_Index) of Drone_Record;

   type Log_Index is range 0 .. Max_Flight_Logs - 1;
   type Log_Array is array (Log_Index) of Flight_Log_Entry;

   type Geofence_Index is range 0 .. Max_Geofences - 1;
   type Geofence_Array is array (Geofence_Index) of Geofence_Zone;

   Null_Drone : constant Drone_Record := (
      Serial       => Null_Serial,
      Public_Key   => (others => 0),
      Owner        => (others => 0),
      Operator     => (others => 0),
      Status       => Status_Inactive,
      Registered   => 0,
      Last_Update  => 0,
      Flight_Count => 0,
      Valid        => False
   );

   Null_Geofence : constant Geofence_Zone := (
      Center   => Null_Coordinate,
      Radius   => 0,
      Max_Alt  => 0,
      Min_Alt  => 0,
      Active   => False
   );

   Drones         : Drone_Array := (others => Null_Drone);
   Drone_Count    : Natural := 0;
   Flight_Logs    : Log_Array := (others => Null_Log_Entry);
   Log_Count      : Natural := 0;
   Geofences      : Geofence_Array := (others => Null_Geofence);
   Geofence_Count : Natural := 0;
   Admin_Addr     : Contract_Address := (others => 0);

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Drone (Serial : Serial_Number) return Natural is
   begin
      for I in Drone_Index loop
         if Drones (I).Valid and then Drones (I).Serial = Serial then
            return Natural (I);
         end if;
      end loop;
      return Natural'Last;
   end Find_Drone;

   function Find_Empty_Drone_Slot return Natural is
   begin
      for I in Drone_Index loop
         if not Drones (I).Valid then
            return Natural (I);
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Drone_Slot;

   function Find_Empty_Log_Slot return Natural is
   begin
      for I in Log_Index loop
         if not Flight_Logs (I).Valid then
            return Natural (I);
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Log_Slot;

   function Find_Empty_Geofence_Slot return Natural is
   begin
      for I in Geofence_Index loop
         if not Geofences (I).Active then
            return Natural (I);
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Geofence_Slot;

   --  Calculate distance between two coordinates (simplified)
   function Distance_Meters (A, B : GPS_Coordinate) return Natural is
      Lat_Diff : constant Integer := abs (A.Latitude - B.Latitude);
      Lon_Diff : constant Integer := abs (A.Longitude - B.Longitude);
      --  Simplified: 1 degree ~= 111km at equator
      Lat_Meters : constant Natural := Natural (Lat_Diff) * 111 / 10_000_000;
      Lon_Meters : constant Natural := Natural (Lon_Diff) * 111 / 10_000_000;
   begin
      --  Approximate distance using max of lat/lon difference
      return Natural'Max (Lat_Meters, Lon_Meters);
   end Distance_Meters;

   function Is_Admin (Addr : Contract_Address) return Boolean is
   begin
      return Addr = Admin_Addr;
   end Is_Admin;

   function Serial_To_Hash (Serial : Serial_Number) return Hash256 is
      Result : Hash256 := (others => 0);
   begin
      for I in 0 .. Serial_Length - 1 loop
         Result (I) := Serial (I);
      end loop;
      return Result;
   end Serial_To_Hash;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (Admin : in Contract_Address) is
   begin
      Drones := (others => Null_Drone);
      Drone_Count := 0;
      Flight_Logs := (others => Null_Log_Entry);
      Log_Count := 0;
      Geofences := (others => Null_Geofence);
      Geofence_Count := 0;
      Admin_Addr := Admin;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Drone Registration
   ---------------------------------------------------------------------------

   procedure Register_Drone (
      Serial     : in     Serial_Number;
      Public_Key : in     MLDSA_Public_Key;
      Owner      : in     Contract_Address;
      Caller     : in     Contract_Address;
      Success    : out    Boolean;
      Error      : out    Registry_Error
   ) is
      Slot : Natural;
   begin
      Success := False;
      Error := Error_None;

      --  Check if already registered
      if Find_Drone (Serial) /= Natural'Last then
         Error := Error_Already_Registered;
         return;
      end if;

      --  Find empty slot
      Slot := Find_Empty_Drone_Slot;
      if Slot = Natural'Last then
         Error := Error_Registry_Full;
         return;
      end if;

      --  Register drone
      Drones (Drone_Index (Slot)) := (
         Serial       => Serial,
         Public_Key   => Public_Key,
         Owner        => Owner,
         Operator     => Owner,  --  Initially same as owner
         Status       => Status_Inactive,
         Registered   => 1,  --  TODO: actual timestamp
         Last_Update  => 1,
         Flight_Count => 0,
         Valid        => True
      );

      Drone_Count := Drone_Count + 1;
      Success := True;
   end Register_Drone;

   procedure Set_Operator (
      Serial   : in     Serial_Number;
      Operator : in     Contract_Address;
      Caller   : in     Contract_Address;
      Success  : out    Boolean;
      Error    : out    Registry_Error
   ) is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      Success := False;
      Error := Error_None;

      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      --  Only owner can set operator
      if Drones (Drone_Index (Idx)).Owner /= Caller then
         Error := Error_Unauthorized;
         return;
      end if;

      Drones (Drone_Index (Idx)).Operator := Operator;
      Drones (Drone_Index (Idx)).Last_Update := 1;
      Success := True;
   end Set_Operator;

   procedure Transfer_Ownership (
      Serial    : in     Serial_Number;
      New_Owner : in     Contract_Address;
      Caller    : in     Contract_Address;
      Success   : out    Boolean;
      Error     : out    Registry_Error
   ) is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      Success := False;
      Error := Error_None;

      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      if Drones (Drone_Index (Idx)).Owner /= Caller then
         Error := Error_Unauthorized;
         return;
      end if;

      Drones (Drone_Index (Idx)).Owner := New_Owner;
      Drones (Drone_Index (Idx)).Last_Update := 1;
      Success := True;
   end Transfer_Ownership;

   procedure Set_Status (
      Serial  : in     Serial_Number;
      Status  : in     Drone_Status;
      Caller  : in     Contract_Address;
      Success : out    Boolean;
      Error   : out    Registry_Error
   ) is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      Success := False;
      Error := Error_None;

      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      --  Owner or operator can update status
      if Drones (Drone_Index (Idx)).Owner /= Caller and then
         Drones (Drone_Index (Idx)).Operator /= Caller
      then
         Error := Error_Unauthorized;
         return;
      end if;

      if Drones (Drone_Index (Idx)).Status = Status_Revoked then
         Error := Error_Drone_Revoked;
         return;
      end if;

      Drones (Drone_Index (Idx)).Status := Status;
      Drones (Drone_Index (Idx)).Last_Update := 1;
      Success := True;
   end Set_Status;

   procedure Revoke_Drone (
      Serial  : in     Serial_Number;
      Caller  : in     Contract_Address;
      Success : out    Boolean;
      Error   : out    Registry_Error
   ) is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      Success := False;
      Error := Error_None;

      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      --  Only admin or owner can revoke
      if not Is_Admin (Caller) and then
         Drones (Drone_Index (Idx)).Owner /= Caller
      then
         Error := Error_Unauthorized;
         return;
      end if;

      Drones (Drone_Index (Idx)).Status := Status_Revoked;
      Drones (Drone_Index (Idx)).Last_Update := 1;
      Success := True;
   end Revoke_Drone;

   ---------------------------------------------------------------------------
   --  Flight Logging
   ---------------------------------------------------------------------------

   procedure Log_Position (
      Serial    : in     Serial_Number;
      Position  : in     GPS_Coordinate;
      Speed     : in     Natural;
      Heading   : in     Natural;
      Vertical  : in     Integer;
      Battery   : in     Natural;
      Signature : in     MLDSA_Signature;
      Timestamp : in     Unsigned_64;
      Success   : out    Boolean;
      Error     : out    Registry_Error
   ) is
      Drone_Idx : constant Natural := Find_Drone (Serial);
      Log_Slot  : Natural;
   begin
      Success := False;
      Error := Error_None;

      if Drone_Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      if Drones (Drone_Index (Drone_Idx)).Status = Status_Revoked then
         Error := Error_Drone_Revoked;
         return;
      end if;

      --  Check geofence
      if Check_Geofence (Position) then
         Error := Error_Geofence_Violation;
         --  Still log but flag violation
      end if;

      --  Find log slot
      Log_Slot := Find_Empty_Log_Slot;
      if Log_Slot = Natural'Last then
         Error := Error_Log_Full;
         return;
      end if;

      --  TODO: Verify signature with drone's public key

      --  Log position
      Flight_Logs (Log_Index (Log_Slot)) := (
         Drone_Hash => Serial_To_Hash (Serial),
         Timestamp  => Timestamp,
         Position   => Position,
         Speed      => Speed,
         Heading    => Heading,
         Vertical   => Vertical,
         Battery    => Battery,
         Valid      => True
      );

      Log_Count := Log_Count + 1;
      Drones (Drone_Index (Drone_Idx)).Last_Update := Timestamp;

      Success := True;
   end Log_Position;

   procedure Start_Flight (
      Serial    : in     Serial_Number;
      Position  : in     GPS_Coordinate;
      Signature : in     MLDSA_Signature;
      Caller    : in     Contract_Address;
      Success   : out    Boolean;
      Error     : out    Registry_Error
   ) is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      Success := False;
      Error := Error_None;

      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      if Drones (Drone_Index (Idx)).Status = Status_Revoked then
         Error := Error_Drone_Revoked;
         return;
      end if;

      if Drones (Drone_Index (Idx)).Operator /= Caller then
         Error := Error_Unauthorized;
         return;
      end if;

      Drones (Drone_Index (Idx)).Status := Status_Active;
      Drones (Drone_Index (Idx)).Flight_Count :=
         Drones (Drone_Index (Idx)).Flight_Count + 1;
      Drones (Drone_Index (Idx)).Last_Update := 1;

      Success := True;
   end Start_Flight;

   procedure End_Flight (
      Serial    : in     Serial_Number;
      Position  : in     GPS_Coordinate;
      Signature : in     MLDSA_Signature;
      Caller    : in     Contract_Address;
      Success   : out    Boolean;
      Error     : out    Registry_Error
   ) is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      Success := False;
      Error := Error_None;

      if Idx = Natural'Last then
         Error := Error_Not_Registered;
         return;
      end if;

      if Drones (Drone_Index (Idx)).Operator /= Caller then
         Error := Error_Unauthorized;
         return;
      end if;

      Drones (Drone_Index (Idx)).Status := Status_Inactive;
      Drones (Drone_Index (Idx)).Last_Update := 1;

      Success := True;
   end End_Flight;

   ---------------------------------------------------------------------------
   --  Geofence Management
   ---------------------------------------------------------------------------

   procedure Add_Geofence (
      Zone    : in     Geofence_Zone;
      Caller  : in     Contract_Address;
      Zone_ID : out    Natural;
      Success : out    Boolean;
      Error   : out    Registry_Error
   ) is
      Slot : Natural;
   begin
      Success := False;
      Error := Error_None;
      Zone_ID := 0;

      if not Is_Admin (Caller) then
         Error := Error_Unauthorized;
         return;
      end if;

      Slot := Find_Empty_Geofence_Slot;
      if Slot = Natural'Last then
         Error := Error_Registry_Full;
         return;
      end if;

      Geofences (Geofence_Index (Slot)) := Zone;
      Geofences (Geofence_Index (Slot)).Active := True;
      Geofence_Count := Geofence_Count + 1;
      Zone_ID := Slot;
      Success := True;
   end Add_Geofence;

   procedure Remove_Geofence (
      Zone_ID : in     Natural;
      Caller  : in     Contract_Address;
      Success : out    Boolean;
      Error   : out    Registry_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      if not Is_Admin (Caller) then
         Error := Error_Unauthorized;
         return;
      end if;

      if Zone_ID >= Max_Geofences then
         Error := Error_Not_Registered;
         return;
      end if;

      Geofences (Geofence_Index (Zone_ID)).Active := False;
      Geofence_Count := Geofence_Count - 1;
      Success := True;
   end Remove_Geofence;

   function Check_Geofence (Position : GPS_Coordinate) return Boolean is
   begin
      for I in Geofence_Index loop
         if Geofences (I).Active then
            declare
               Dist : constant Natural :=
                  Distance_Meters (Position, Geofences (I).Center);
            begin
               --  Check if inside restricted zone
               if Dist <= Geofences (I).Radius and then
                  Position.Altitude >= Geofences (I).Min_Alt and then
                  Position.Altitude <= Geofences (I).Max_Alt
               then
                  return True;  --  Violation
               end if;
            end;
         end if;
      end loop;
      return False;  --  No violation
   end Check_Geofence;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Is_Registered (Serial : Serial_Number) return Boolean is
   begin
      return Find_Drone (Serial) /= Natural'Last;
   end Is_Registered;

   procedure Get_Drone (
      Serial  : in     Serial_Number;
      Drone   : out    Drone_Record;
      Found   : out    Boolean
   ) is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      if Idx = Natural'Last then
         Drone := Null_Drone;
         Found := False;
      else
         Drone := Drones (Drone_Index (Idx));
         Found := True;
      end if;
   end Get_Drone;

   function Get_Owner (Serial : Serial_Number) return Contract_Address is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      if Idx = Natural'Last then
         return (others => 0);
      end if;
      return Drones (Drone_Index (Idx)).Owner;
   end Get_Owner;

   function Get_Status (Serial : Serial_Number) return Drone_Status is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      if Idx = Natural'Last then
         return Status_Inactive;
      end if;
      return Drones (Drone_Index (Idx)).Status;
   end Get_Status;

   function Get_Flight_Count (Serial : Serial_Number) return Natural is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      if Idx = Natural'Last then
         return 0;
      end if;
      return Drones (Drone_Index (Idx)).Flight_Count;
   end Get_Flight_Count;

   procedure Get_Last_Position (
      Serial   : in     Serial_Number;
      Position : out    GPS_Coordinate;
      Time     : out    Unsigned_64;
      Found    : out    Boolean
   ) is
      Drone_Hash : constant Hash256 := Serial_To_Hash (Serial);
      Latest_Time : Unsigned_64 := 0;
      Latest_Pos  : GPS_Coordinate := Null_Coordinate;
   begin
      Found := False;

      for I in Log_Index loop
         if Flight_Logs (I).Valid and then
            Flight_Logs (I).Drone_Hash = Drone_Hash and then
            Flight_Logs (I).Timestamp > Latest_Time
         then
            Latest_Time := Flight_Logs (I).Timestamp;
            Latest_Pos := Flight_Logs (I).Position;
            Found := True;
         end if;
      end loop;

      Position := Latest_Pos;
      Time := Latest_Time;
   end Get_Last_Position;

   function Total_Drones return Natural is
   begin
      return Drone_Count;
   end Total_Drones;

   function Active_Drones return Natural is
      Count : Natural := 0;
   begin
      for I in Drone_Index loop
         if Drones (I).Valid and then Drones (I).Status = Status_Active then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Active_Drones;

   ---------------------------------------------------------------------------
   --  Signature Verification
   ---------------------------------------------------------------------------

   function Verify_Position_Signature (
      Serial    : Serial_Number;
      Position  : GPS_Coordinate;
      Timestamp : Unsigned_64;
      Signature : MLDSA_Signature
   ) return Boolean is
      Idx : constant Natural := Find_Drone (Serial);
   begin
      if Idx = Natural'Last then
         return False;
      end if;

      --  TODO: Implement actual ML-DSA-87 signature verification
      --  using the drone's registered public key
      --  For now, return True as placeholder
      return True;
   end Verify_Position_Signature;

end Drone_Registry;
