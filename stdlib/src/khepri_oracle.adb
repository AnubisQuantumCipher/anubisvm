--  KHEPRI Oracle Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Khepri_Oracle with
   SPARK_Mode => On,
   Refined_State => (Oracle_State =>
      (Admin_Addr, Feeds, Reporters, Prices, Current_Time))
is

   ---------------------------------------------------------------------------
   --  Internal Types
   ---------------------------------------------------------------------------

   type Feed_Entry is record
      Config   : Feed_Config;
      Active   : Boolean;
      Round    : Round_ID;
      Used     : Boolean;
   end record;

   type Feed_Array is array (0 .. Max_Feeds - 1) of Feed_Entry;

   type Reporter_Array is array (0 .. Max_Reporters - 1) of Reporter_Info;

   type Price_Entry is record
      Feed_Id : Natural;
      Data    : Price_Data;
      Used    : Boolean;
   end record;

   type Price_Array is array (0 .. Max_Rounds - 1) of Price_Entry;

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Admin_Addr   : Address := Zero_Address;
   Current_Time : Timestamp := 0;

   Feeds : Feed_Array := (others => (
      Config => Default_Feed_Config,
      Active => False,
      Round  => 0,
      Used   => False
   ));

   Reporters : Reporter_Array := (others => (
      Reporter_Addr => Zero_Address,
      Status        => Status_Inactive,
      Last_Report   => 0,
      Report_Count  => 0,
      Used          => False
   ));

   Prices : Price_Array := (others => (
      Feed_Id => 0,
      Data    => Null_Price_Data,
      Used    => False
   ));

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   function Find_Feed (Feed_Id : Natural) return Natural is
   begin
      if Feed_Id < Max_Feeds and then Feeds (Feed_Id).Used then
         return Feed_Id;
      end if;
      return Natural'Last;
   end Find_Feed;

   function Find_Empty_Feed return Natural is
   begin
      for I in Feeds'Range loop
         if not Feeds (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Feed;

   function Find_Reporter (Reporter : Address) return Natural is
   begin
      for I in Reporters'Range loop
         if Reporters (I).Used
            and then Reporters (I).Reporter_Addr = Reporter
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Reporter;

   function Find_Empty_Reporter return Natural is
   begin
      for I in Reporters'Range loop
         if not Reporters (I).Used then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Empty_Reporter;

   function Find_Price (Feed_Id : Natural; Round_Num : Round_ID) return Natural is
   begin
      for I in Prices'Range loop
         if Prices (I).Used
            and then Prices (I).Feed_Id = Feed_Id
            and then Prices (I).Data.Round_Num = Round_Num
         then
            return I;
         end if;
      end loop;
      return Natural'Last;
   end Find_Price;

   function Find_Latest_Price (Feed_Id : Natural) return Natural is
      Latest_Slot : Natural := Natural'Last;
      Latest_Time : Timestamp := 0;
   begin
      for I in Prices'Range loop
         if Prices (I).Used
            and then Prices (I).Feed_Id = Feed_Id
            and then Prices (I).Data.Updated_At > Latest_Time
         then
            Latest_Slot := I;
            Latest_Time := Prices (I).Data.Updated_At;
         end if;
      end loop;
      return Latest_Slot;
   end Find_Latest_Price;

   function Find_Empty_Price return Natural is
   begin
      for I in Prices'Range loop
         if not Prices (I).Used then
            return I;
         end if;
      end loop;
      --  If full, overwrite oldest
      return 0;
   end Find_Empty_Price;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize (
      Admin   : in Address;
      Success : out Boolean
   ) is
   begin
      if Admin_Addr /= Zero_Address then
         Success := False;
         return;
      end if;

      Admin_Addr := Admin;
      Success := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Feed Management
   ---------------------------------------------------------------------------

   procedure Create_Feed (
      Caller  : in     Address;
      Config  : in     Feed_Config;
      Feed_Id : out    Natural;
      Success : out    Boolean;
      Error   : out    Oracle_Error
   ) is
      Slot : Natural;
   begin
      if Caller /= Admin_Addr then
         Feed_Id := 0;
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Slot := Find_Empty_Feed;
      if Slot = Natural'Last then
         Feed_Id := 0;
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Feeds (Slot) := (
         Config => Config,
         Active => True,
         Round  => 0,
         Used   => True
      );

      Feed_Id := Slot;
      Success := True;
      Error := Error_None;
   end Create_Feed;

   procedure Update_Feed_Config (
      Caller  : in     Address;
      Feed_Id : in     Natural;
      Config  : in     Feed_Config;
      Success : out    Boolean;
      Error   : out    Oracle_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      if Feed_Id >= Max_Feeds or else not Feeds (Feed_Id).Used then
         Success := False;
         Error := Error_Feed_Not_Found;
         return;
      end if;

      Feeds (Feed_Id).Config := Config;
      Success := True;
      Error := Error_None;
   end Update_Feed_Config;

   procedure Disable_Feed (
      Caller  : in     Address;
      Feed_Id : in     Natural;
      Success : out    Boolean;
      Error   : out    Oracle_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      if Feed_Id >= Max_Feeds or else not Feeds (Feed_Id).Used then
         Success := False;
         Error := Error_Feed_Not_Found;
         return;
      end if;

      Feeds (Feed_Id).Active := False;
      Success := True;
      Error := Error_None;
   end Disable_Feed;

   ---------------------------------------------------------------------------
   --  Reporter Management
   ---------------------------------------------------------------------------

   procedure Add_Reporter (
      Caller   : in     Address;
      Reporter : in     Address;
      Success  : out    Boolean;
      Error    : out    Oracle_Error
   ) is
      Slot : Natural;
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      --  Check if already exists
      if Find_Reporter (Reporter) /= Natural'Last then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Slot := Find_Empty_Reporter;
      if Slot = Natural'Last then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Reporters (Slot) := (
         Reporter_Addr => Reporter,
         Status        => Status_Active,
         Last_Report   => 0,
         Report_Count  => 0,
         Used          => True
      );

      Success := True;
      Error := Error_None;
   end Add_Reporter;

   procedure Remove_Reporter (
      Caller   : in     Address;
      Reporter : in     Address;
      Success  : out    Boolean;
      Error    : out    Oracle_Error
   ) is
      Slot : constant Natural := Find_Reporter (Reporter);
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      if Slot = Natural'Last then
         Success := False;
         Error := Error_Reporter_Not_Found;
         return;
      end if;

      Reporters (Slot).Used := False;
      Success := True;
      Error := Error_None;
   end Remove_Reporter;

   procedure Suspend_Reporter (
      Caller   : in     Address;
      Reporter : in     Address;
      Success  : out    Boolean;
      Error    : out    Oracle_Error
   ) is
      Slot : constant Natural := Find_Reporter (Reporter);
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      if Slot = Natural'Last then
         Success := False;
         Error := Error_Reporter_Not_Found;
         return;
      end if;

      Reporters (Slot).Status := Status_Suspended;
      Success := True;
      Error := Error_None;
   end Suspend_Reporter;

   procedure Reinstate_Reporter (
      Caller   : in     Address;
      Reporter : in     Address;
      Success  : out    Boolean;
      Error    : out    Oracle_Error
   ) is
      Slot : constant Natural := Find_Reporter (Reporter);
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      if Slot = Natural'Last then
         Success := False;
         Error := Error_Reporter_Not_Found;
         return;
      end if;

      Reporters (Slot).Status := Status_Active;
      Success := True;
      Error := Error_None;
   end Reinstate_Reporter;

   ---------------------------------------------------------------------------
   --  Price Submission
   ---------------------------------------------------------------------------

   procedure Submit_Price (
      Caller  : in     Address;
      Feed_Id : in     Natural;
      Price   : in     Price_Value;
      Success : out    Boolean;
      Error   : out    Oracle_Error
   ) is
      Reporter_Slot : constant Natural := Find_Reporter (Caller);
      Price_Slot    : Natural;
      New_Round     : Round_ID;
   begin
      --  Check reporter is valid
      if Reporter_Slot = Natural'Last then
         Success := False;
         Error := Error_Reporter_Not_Found;
         return;
      end if;

      if Reporters (Reporter_Slot).Status /= Status_Active then
         Success := False;
         Error := Error_Reporter_Suspended;
         return;
      end if;

      --  Check feed exists and is active
      if Feed_Id >= Max_Feeds or else not Feeds (Feed_Id).Used then
         Success := False;
         Error := Error_Feed_Not_Found;
         return;
      end if;

      if not Feeds (Feed_Id).Active then
         Success := False;
         Error := Error_Feed_Not_Found;
         return;
      end if;

      --  Check price is valid
      if Equal (Price, U256_Zero) then
         Success := False;
         Error := Error_Invalid_Price;
         return;
      end if;

      --  Increment round
      New_Round := Feeds (Feed_Id).Round + 1;
      Feeds (Feed_Id).Round := New_Round;

      --  Store price
      Price_Slot := Find_Empty_Price;

      Prices (Price_Slot) := (
         Feed_Id => Feed_Id,
         Data    => (
            Round_Num   => New_Round,
            Answer      => Price,
            Started_At  => Current_Time,
            Updated_At  => Current_Time,
            Answered_In => New_Round
         ),
         Used    => True
      );

      --  Update reporter stats
      Reporters (Reporter_Slot).Last_Report := Current_Time;
      Reporters (Reporter_Slot).Report_Count :=
         Reporters (Reporter_Slot).Report_Count + 1;

      Success := True;
      Error := Error_None;
   end Submit_Price;

   ---------------------------------------------------------------------------
   --  Price Query
   ---------------------------------------------------------------------------

   function Latest_Round_Data (Feed_Id : Natural) return Price_Data is
      Slot : constant Natural := Find_Latest_Price (Feed_Id);
   begin
      if Slot /= Natural'Last then
         return Prices (Slot).Data;
      else
         return Null_Price_Data;
      end if;
   end Latest_Round_Data;

   function Get_Round_Data (
      Feed_Id   : Natural;
      Round_Num : Round_ID
   ) return Price_Data is
      Slot : constant Natural := Find_Price (Feed_Id, Round_Num);
   begin
      if Slot /= Natural'Last then
         return Prices (Slot).Data;
      else
         return Null_Price_Data;
      end if;
   end Get_Round_Data;

   function Latest_Answer (Feed_Id : Natural) return Price_Value is
   begin
      return Latest_Round_Data (Feed_Id).Answer;
   end Latest_Answer;

   function Latest_Timestamp (Feed_Id : Natural) return Timestamp is
   begin
      return Latest_Round_Data (Feed_Id).Updated_At;
   end Latest_Timestamp;

   function Latest_Round (Feed_Id : Natural) return Round_ID is
   begin
      if Feed_Id < Max_Feeds and then Feeds (Feed_Id).Used then
         return Feeds (Feed_Id).Round;
      else
         return 0;
      end if;
   end Latest_Round;

   function Get_Answer (
      Feed_Id   : Natural;
      Round_Num : Round_ID
   ) return Price_Value is
   begin
      return Get_Round_Data (Feed_Id, Round_Num).Answer;
   end Get_Answer;

   function Get_Timestamp (
      Feed_Id   : Natural;
      Round_Num : Round_ID
   ) return Timestamp is
   begin
      return Get_Round_Data (Feed_Id, Round_Num).Updated_At;
   end Get_Timestamp;

   ---------------------------------------------------------------------------
   --  Feed Information
   ---------------------------------------------------------------------------

   function Get_Feed_Config (Feed_Id : Natural) return Feed_Config is
   begin
      if Feed_Id < Max_Feeds and then Feeds (Feed_Id).Used then
         return Feeds (Feed_Id).Config;
      else
         return Default_Feed_Config;
      end if;
   end Get_Feed_Config;

   function Feed_Decimals (Feed_Id : Natural) return Natural is
   begin
      if Feed_Id < Max_Feeds and then Feeds (Feed_Id).Used then
         return Feeds (Feed_Id).Config.Decimals;
      else
         return 0;
      end if;
   end Feed_Decimals;

   function Feed_Description (Feed_Id : Natural) return Feed_Name is
   begin
      if Feed_Id < Max_Feeds and then Feeds (Feed_Id).Used then
         return Feeds (Feed_Id).Config.Name;
      else
         return (Data => (others => ' '), Length => 0);
      end if;
   end Feed_Description;

   function Feed_Count return Natural is
      Count : Natural := 0;
   begin
      for I in Feeds'Range loop
         if Feeds (I).Used then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Feed_Count;

   function Is_Feed_Active (Feed_Id : Natural) return Boolean is
   begin
      return Feed_Id < Max_Feeds
         and then Feeds (Feed_Id).Used
         and then Feeds (Feed_Id).Active;
   end Is_Feed_Active;

   function Is_Price_Stale (Feed_Id : Natural) return Boolean is
      Latest : Price_Data;
      Heartbeat : Timestamp;
   begin
      if Feed_Id >= Max_Feeds or else not Feeds (Feed_Id).Used then
         return True;
      end if;

      Latest := Latest_Round_Data (Feed_Id);
      Heartbeat := Feeds (Feed_Id).Config.Heartbeat;

      if Latest.Updated_At = 0 then
         return True;
      end if;

      return Current_Time > Latest.Updated_At + Heartbeat;
   end Is_Price_Stale;

   ---------------------------------------------------------------------------
   --  Reporter Information
   ---------------------------------------------------------------------------

   function Get_Reporter_Info (Reporter : Address) return Reporter_Info is
      Slot : constant Natural := Find_Reporter (Reporter);
   begin
      if Slot /= Natural'Last then
         return Reporters (Slot);
      else
         return (
            Reporter_Addr => Zero_Address,
            Status        => Status_Inactive,
            Last_Report   => 0,
            Report_Count  => 0,
            Used          => False
         );
      end if;
   end Get_Reporter_Info;

   function Is_Reporter (Reporter : Address) return Boolean is
   begin
      return Find_Reporter (Reporter) /= Natural'Last;
   end Is_Reporter;

   function Reporter_Count return Natural is
      Count : Natural := 0;
   begin
      for I in Reporters'Range loop
         if Reporters (I).Used then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Reporter_Count;

   ---------------------------------------------------------------------------
   --  Admin Functions
   ---------------------------------------------------------------------------

   function Contract_Admin return Address is
   begin
      return Admin_Addr;
   end Contract_Admin;

   procedure Transfer_Admin (
      Caller    : in     Address;
      New_Admin : in     Address;
      Success   : out    Boolean;
      Error     : out    Oracle_Error
   ) is
   begin
      if Caller /= Admin_Addr then
         Success := False;
         Error := Error_Not_Authorized;
         return;
      end if;

      Admin_Addr := New_Admin;
      Success := True;
      Error := Error_None;
   end Transfer_Admin;

end Khepri_Oracle;
