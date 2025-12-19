pragma SPARK_Mode (On);

package body IoT_Device_Registry with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function Account_Equal (A, B : Account_ID) return Boolean is
      Result : Boolean := True;
   begin
      for I in Account_ID_Index loop
         if A (I) /= B (I) then
            Result := False;
         end if;
         pragma Loop_Invariant
           (Result = (for all J in Account_ID_Index'First .. I => A (J) = B (J)));
      end loop;
      return Result;
   end Account_Equal;

   function Hardware_Equal (A, B : Hardware_ID) return Boolean is
      Result : Boolean := True;
   begin
      for I in A'Range loop
         if A (I) /= B (I) then
            Result := False;
         end if;
      end loop;
      return Result;
   end Hardware_Equal;

   function Device_Type_String (DT : Device_Type) return String is
   begin
      case DT is
         when Unknown       => return "Unknown";
         when Pico_W        => return "Pico W";
         when Pico_2W       => return "Pico 2 W";
         when ESP32         => return "ESP32";
         when ESP32_S3      => return "ESP32-S3";
         when STM32_F4      => return "STM32-F4";
         when STM32_H7      => return "STM32-H7";
         when NRF52840      => return "nRF52840";
         when RISC_V_Generic => return "RISC-V";
         when Custom        => return "Custom";
      end case;
   end Device_Type_String;

   ---------------------------------------------------------------------------
   --  Registry Management
   ---------------------------------------------------------------------------

   procedure Initialize (
      State   : in Out Registry_State;
      Admin   : Account_ID;
      Timeout : Unsigned_64)
   is
   begin
      State.Initialized := True;
      State.Admin := Admin;
      State.Total_Devices := 0;
      State.Online_Count := 0;
      State.Total_Readings := 0;
      State.Heartbeat_Timeout := Timeout;
      State.Cert_Level := Platinum;

      for I in Device_ID loop
         State.Devices (I) := Empty_Device;
      end loop;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Device Registration
   ---------------------------------------------------------------------------

   procedure Register_Device (
      State       : in Out Registry_State;
      Owner       : Account_ID;
      Hardware    : Hardware_ID;
      Device_Kind : Device_Type;
      Timestamp   : Unsigned_64;
      Did         : out Device_ID;
      Success     : out Boolean)
   is
   begin
      --  Check capacity
      if State.Total_Devices >= Unsigned_64 (Max_Devices) then
         Did := 0;
         Success := False;
         return;
      end if;

      --  Check hardware ID is not zero
      if Hardware = Hash256_Zero then
         Did := 0;
         Success := False;
         return;
      end if;

      --  Allocate device slot
      Did := Device_ID (State.Total_Devices);

      State.Devices (Did) := (
         Status          => Inactive,
         Device_Kind     => Device_Kind,
         Owner           => Owner,
         Hardware        => Hardware,
         Firmware        => (Major => 1, Minor => 0, Patch => 0),
         Sensors         => (others => (Sensor => None, Enabled => False, Interval => 0)),
         Last_Heartbeat  => 0,
         Heartbeat_Count => 0,
         Data_Count      => 0,
         Registered_At   => Timestamp,
         Location_Hash   => Hash256_Zero);

      State.Total_Devices := State.Total_Devices + 1;
      Success := True;
   end Register_Device;

   procedure Update_Firmware (
      State   : in Out Registry_State;
      Did     : Device_ID;
      Caller  : Account_ID;
      Version : Firmware_Version;
      Success : out Boolean)
   is
      D : Device renames State.Devices (Did);
   begin
      --  Only owner or admin can update firmware
      if not (Account_Equal (Caller, D.Owner) or
              Account_Equal (Caller, State.Admin)) then
         Success := False;
         return;
      end if;

      --  Device must be registered
      if D.Status = Inactive and D.Registered_At = 0 then
         Success := False;
         return;
      end if;

      State.Devices (Did).Firmware := Version;
      Success := True;
   end Update_Firmware;

   procedure Configure_Sensor (
      State      : in Out Registry_State;
      Did        : Device_ID;
      Caller     : Account_ID;
      Sensor_Idx : Unsigned_8;
      Config     : Sensor_Config;
      Success    : out Boolean)
   is
      D : Device renames State.Devices (Did);
   begin
      --  Only owner can configure sensors
      if not Account_Equal (Caller, D.Owner) then
         Success := False;
         return;
      end if;

      --  Validate sensor index
      if Sensor_Idx < 1 or Sensor_Idx > Max_Sensors then
         Success := False;
         return;
      end if;

      State.Devices (Did).Sensors (Integer (Sensor_Idx)) := Config;
      Success := True;
   end Configure_Sensor;

   procedure Transfer_Ownership (
      State     : in Out Registry_State;
      Did       : Device_ID;
      Caller    : Account_ID;
      New_Owner : Account_ID;
      Success   : out Boolean)
   is
      D : Device renames State.Devices (Did);
   begin
      --  Only current owner can transfer
      if not Account_Equal (Caller, D.Owner) then
         Success := False;
         return;
      end if;

      State.Devices (Did).Owner := New_Owner;
      Success := True;
   end Transfer_Ownership;

   procedure Decommission_Device (
      State   : in Out Registry_State;
      Did     : Device_ID;
      Caller  : Account_ID;
      Success : out Boolean)
   is
      D : Device renames State.Devices (Did);
   begin
      --  Only owner or admin can decommission
      if not (Account_Equal (Caller, D.Owner) or
              Account_Equal (Caller, State.Admin)) then
         Success := False;
         return;
      end if;

      --  Update online count if device was online
      if D.Status = Online and State.Online_Count > 0 then
         State.Online_Count := State.Online_Count - 1;
      end if;

      State.Devices (Did).Status := Decommissioned;
      Success := True;
   end Decommission_Device;

   ---------------------------------------------------------------------------
   --  Device Operations
   ---------------------------------------------------------------------------

   procedure Heartbeat (
      State        : in Out Registry_State;
      Did          : Device_ID;
      Device_Auth  : Account_ID;
      Timestamp    : Unsigned_64;
      Success      : out Boolean)
   is
      D : Device renames State.Devices (Did);
   begin
      --  Verify device authorization (device signs with its key)
      if not Account_Equal (Device_Auth, D.Owner) then
         Success := False;
         return;
      end if;

      --  Can't heartbeat decommissioned or compromised devices
      if D.Status = Decommissioned or D.Status = Compromised then
         Success := False;
         return;
      end if;

      --  Update online count
      if D.Status /= Online then
         State.Online_Count := State.Online_Count + 1;
      end if;

      --  Update device state
      State.Devices (Did).Status := Online;
      State.Devices (Did).Last_Heartbeat := Timestamp;
      State.Devices (Did).Heartbeat_Count := D.Heartbeat_Count + 1;

      Success := True;
   end Heartbeat;

   procedure Submit_Reading (
      State       : in Out Registry_State;
      Did         : Device_ID;
      Device_Auth : Account_ID;
      Reading     : Sensor_Reading;
      Success     : out Boolean)
   is
      D : Device renames State.Devices (Did);
   begin
      --  Verify device authorization
      if not Account_Equal (Device_Auth, D.Owner) then
         Success := False;
         return;
      end if;

      --  Device must be online
      if D.Status /= Online then
         Success := False;
         return;
      end if;

      --  Validate sensor index
      if Reading.Sensor_Idx < 1 or Reading.Sensor_Idx > Max_Sensors then
         Success := False;
         return;
      end if;

      --  Check sensor is enabled
      if not D.Sensors (Integer (Reading.Sensor_Idx)).Enabled then
         Success := False;
         return;
      end if;

      --  Record the reading (in real impl, would store in buffer)
      State.Devices (Did).Data_Count := D.Data_Count + 1;
      State.Total_Readings := State.Total_Readings + 1;

      Success := True;
   end Submit_Reading;

   procedure Report_Compromise (
      State       : in Out Registry_State;
      Did         : Device_ID;
      Device_Auth : Account_ID;
      Success     : out Boolean)
   is
      D : Device renames State.Devices (Did);
   begin
      --  Device reports itself compromised (tamper detection)
      if not Account_Equal (Device_Auth, D.Owner) then
         Success := False;
         return;
      end if;

      --  Update online count
      if D.Status = Online and State.Online_Count > 0 then
         State.Online_Count := State.Online_Count - 1;
      end if;

      State.Devices (Did).Status := Compromised;
      Success := True;
   end Report_Compromise;

   ---------------------------------------------------------------------------
   --  Admin Operations
   ---------------------------------------------------------------------------

   procedure Set_Device_Status (
      State      : in Out Registry_State;
      Did        : Device_ID;
      Caller     : Account_ID;
      New_Status : Device_Status;
      Success    : out Boolean)
   is
      D : Device renames State.Devices (Did);
   begin
      --  Only admin can force status changes
      if not Account_Equal (Caller, State.Admin) then
         Success := False;
         return;
      end if;

      --  Update online count
      if D.Status = Online and New_Status /= Online then
         if State.Online_Count > 0 then
            State.Online_Count := State.Online_Count - 1;
         end if;
      elsif D.Status /= Online and New_Status = Online then
         State.Online_Count := State.Online_Count + 1;
      end if;

      State.Devices (Did).Status := New_Status;
      Success := True;
   end Set_Device_Status;

   procedure Update_Timeout (
      State       : in Out Registry_State;
      Caller      : Account_ID;
      New_Timeout : Unsigned_64;
      Success     : out Boolean)
   is
   begin
      --  Only admin can update timeout
      if not Account_Equal (Caller, State.Admin) then
         Success := False;
         return;
      end if;

      State.Heartbeat_Timeout := New_Timeout;
      Success := True;
   end Update_Timeout;

   ---------------------------------------------------------------------------
   --  Query Functions
   ---------------------------------------------------------------------------

   function Get_Device_Status (
      State : Registry_State;
      Did   : Device_ID) return Device_Status
   is
   begin
      return State.Devices (Did).Status;
   end Get_Device_Status;

   function Get_Device_Type (
      State : Registry_State;
      Did   : Device_ID) return Device_Type
   is
   begin
      return State.Devices (Did).Device_Kind;
   end Get_Device_Type;

   function Get_Device_Owner (
      State : Registry_State;
      Did   : Device_ID) return Account_ID
   is
   begin
      return State.Devices (Did).Owner;
   end Get_Device_Owner;

   function Get_Last_Heartbeat (
      State : Registry_State;
      Did   : Device_ID) return Unsigned_64
   is
   begin
      return State.Devices (Did).Last_Heartbeat;
   end Get_Last_Heartbeat;

   function Get_Firmware (
      State : Registry_State;
      Did   : Device_ID) return Firmware_Version
   is
   begin
      return State.Devices (Did).Firmware;
   end Get_Firmware;

   function Get_Total_Devices (State : Registry_State) return Unsigned_64 is
   begin
      return State.Total_Devices;
   end Get_Total_Devices;

   function Get_Online_Count (State : Registry_State) return Unsigned_64 is
   begin
      return State.Online_Count;
   end Get_Online_Count;

   function Get_Total_Readings (State : Registry_State) return Unsigned_64 is
   begin
      return State.Total_Readings;
   end Get_Total_Readings;

   function Is_Device_Online (
      State        : Registry_State;
      Did          : Device_ID;
      Current_Time : Unsigned_64) return Boolean
   is
      D : Device renames State.Devices (Did);
   begin
      if D.Status /= Online then
         return False;
      end if;

      --  Check if heartbeat is within timeout
      if D.Last_Heartbeat = 0 then
         return False;
      end if;

      if Current_Time >= D.Last_Heartbeat and
         Current_Time - D.Last_Heartbeat <= State.Heartbeat_Timeout
      then
         return True;
      else
         return False;
      end if;
   end Is_Device_Online;

end IoT_Device_Registry;
