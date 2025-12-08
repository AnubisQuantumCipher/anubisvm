--  KHEPRI Embedded Contract Profile Implementation
pragma SPARK_Mode (On);

package body Khepri_Embedded with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Memory Usage Percentage Calculations
   ---------------------------------------------------------------------------

   function Stack_Usage_Percent (Usage : Memory_Usage) return Natural is
      Percent : Natural;
   begin
      if Usage.Stack_Max = 0 then
         return 0;
      end if;

      --  Calculate percentage, clamping to 100
      if Usage.Stack_Used >= Usage.Stack_Max then
         Percent := 100;
      else
         Percent := (Usage.Stack_Used * 100) / Usage.Stack_Max;
      end if;

      return Percent;
   end Stack_Usage_Percent;

   function Data_Usage_Percent (Usage : Memory_Usage) return Natural is
      Percent : Natural;
   begin
      if Usage.Data_Max = 0 then
         return 0;
      end if;

      if Usage.Data_Used >= Usage.Data_Max then
         Percent := 100;
      else
         Percent := (Usage.Data_Used * 100) / Usage.Data_Max;
      end if;

      return Percent;
   end Data_Usage_Percent;

   function Code_Usage_Percent (Usage : Memory_Usage) return Natural is
      Percent : Natural;
   begin
      if Usage.Code_Max = 0 then
         return 0;
      end if;

      if Usage.Code_Used >= Usage.Code_Max then
         Percent := 100;
      else
         Percent := (Usage.Code_Used * 100) / Usage.Code_Max;
      end if;

      return Percent;
   end Code_Usage_Percent;

   ---------------------------------------------------------------------------
   --  Deployment Validation
   ---------------------------------------------------------------------------

   function Can_Deploy (
      Contract_Stack : Natural;
      Contract_Data  : Natural;
      Contract_Code  : Natural;
      Device         : Device_Descriptor
   ) return Deploy_Result is
      Result : Deploy_Result;
      Usage  : Memory_Usage;
   begin
      --  Initialize usage tracking
      Usage := (
         Stack_Used => Contract_Stack,
         Stack_Max  => Device.Memory.Max_Stack_Size,
         Data_Used  => Contract_Data,
         Data_Max   => Device.Memory.Max_Data_Size,
         Code_Used  => Contract_Code,
         Code_Max   => Device.Memory.Max_Code_Size,
         Heap_Used  => 0,
         Heap_Max   => Device.Memory.Max_Heap_Size
      );

      Result.Usage := Usage;

      --  Check stack size
      if Contract_Stack > Device.Memory.Max_Stack_Size then
         Result.Success := False;
         Result.Error := Deploy_Memory_Exceeded;
         return Result;
      end if;

      --  Check data size
      if Contract_Data > Device.Memory.Max_Data_Size then
         Result.Success := False;
         Result.Error := Deploy_Memory_Exceeded;
         return Result;
      end if;

      --  Check code size
      if Contract_Code > Device.Memory.Max_Code_Size then
         Result.Success := False;
         Result.Error := Deploy_Code_Too_Large;
         return Result;
      end if;

      --  Check if networking is required but unavailable
      if Device.Runtime.Comm.Network_Stack = False
         and then Device.Capabilities.Has_Networking
      then
         --  Network capability exists but stack not included
         --  This is a warning, not an error for now
         null;
      end if;

      --  All checks passed
      Result.Success := True;
      Result.Error := Deploy_OK;
      return Result;
   end Can_Deploy;

   ---------------------------------------------------------------------------
   --  Watchdog Operations
   ---------------------------------------------------------------------------

   procedure Kick_Watchdog is
   begin
      --  In a real embedded system, this would:
      --  1. Write to a hardware watchdog timer register
      --  2. Reset the countdown to prevent system reset
      --
      --  For SPARK verification purposes, this is a no-op placeholder.
      --  Actual implementation would be target-specific.
      null;
   end Kick_Watchdog;

end Khepri_Embedded;
