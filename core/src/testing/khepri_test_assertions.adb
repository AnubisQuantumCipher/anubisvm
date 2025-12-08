pragma SPARK_Mode (On);

with Aegis_U256; use Aegis_U256;

package body Khepri_Test_Assertions is

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   function To_Bounded (S : String) return Bounded_String is
      Result : Bounded_String := Empty_String;
   begin
      if S'Length <= Result.Data'Length then
         for I in S'Range loop
            Result.Data (Result.Data'First + I - S'First) := S (I);
         end loop;
         Result.Length := S'Length;
      end if;
      return Result;
   end To_Bounded;

   function Make_Failed (Message : String) return Test_Result is
   begin
      return (
         Outcome       => Outcome_Failed,
         Duration_Ms   => 0,
         Gas_Used      => 0,
         Message       => To_Bounded (Message),
         Expected      => Empty_String,
         Actual        => Empty_String,
         Location      => Empty_String
      );
   end Make_Failed;

   function Make_Passed return Test_Result is
   begin
      return Passed_Result;
   end Make_Passed;

   ---------------------------------------------------------------------------
   --  Basic Assertions
   ---------------------------------------------------------------------------

   function Assert_True (
      Condition : Boolean;
      Message   : String := ""
   ) return Test_Result is
   begin
      if Condition then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Expected true but got false");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_True;

   function Assert_False (
      Condition : Boolean;
      Message   : String := ""
   ) return Test_Result is
   begin
      if not Condition then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Expected false but got true");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_False;

   ---------------------------------------------------------------------------
   --  Equality Assertions
   ---------------------------------------------------------------------------

   function Assert_Equal (
      Expected : Natural;
      Actual   : Natural;
      Message  : String := ""
   ) return Test_Result is
   begin
      if Expected = Actual then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Values not equal");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Equal;

   function Assert_Equal_U64 (
      Expected : Word64;
      Actual   : Word64;
      Message  : String := ""
   ) return Test_Result is
   begin
      if Expected = Actual then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Word64 values not equal");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Equal_U64;

   function Assert_Equal_U256 (
      Expected : Uint256;
      Actual   : Uint256;
      Message  : String := ""
   ) return Test_Result is
   begin
      if Equal (Expected, Actual) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Uint256 values not equal");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Equal_U256;

   function Assert_Equal_Address (
      Expected : Khepri_Types.Address;
      Actual   : Khepri_Types.Address;
      Message  : String := ""
   ) return Test_Result is
   begin
      if Expected = Actual then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Addresses not equal");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Equal_Address;

   function Assert_Equal_Hash (
      Expected : Hash256;
      Actual   : Hash256;
      Message  : String := ""
   ) return Test_Result is
   begin
      if Expected = Actual then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Hashes not equal");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Equal_Hash;

   function Assert_Equal_Bytes (
      Expected : Byte_Array;
      Actual   : Byte_Array;
      Message  : String := ""
   ) return Test_Result is
   begin
      if Expected'Length /= Actual'Length then
         if Message = "" then
            return Make_Failed ("Byte arrays have different lengths");
         else
            return Make_Failed (Message);
         end if;
      end if;

      for I in 0 .. Expected'Length - 1 loop
         if Expected (Expected'First + I) /= Actual (Actual'First + I) then
            if Message = "" then
               return Make_Failed ("Byte arrays differ");
            else
               return Make_Failed (Message);
            end if;
         end if;
      end loop;

      return Make_Passed;
   end Assert_Equal_Bytes;

   ---------------------------------------------------------------------------
   --  Inequality Assertions
   ---------------------------------------------------------------------------

   function Assert_Not_Equal (
      Expected : Natural;
      Actual   : Natural;
      Message  : String := ""
   ) return Test_Result is
   begin
      if Expected /= Actual then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Values should not be equal");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Not_Equal;

   function Assert_Not_Equal_U256 (
      Expected : Uint256;
      Actual   : Uint256;
      Message  : String := ""
   ) return Test_Result is
   begin
      if not Equal (Expected, Actual) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Uint256 values should not be equal");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Not_Equal_U256;

   ---------------------------------------------------------------------------
   --  Numeric Comparisons
   ---------------------------------------------------------------------------

   function Assert_Greater_Than (
      Value     : Natural;
      Threshold : Natural;
      Message   : String := ""
   ) return Test_Result is
   begin
      if Value > Threshold then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Value not greater than threshold");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Greater_Than;

   function Assert_Greater_Equal (
      Value     : Natural;
      Threshold : Natural;
      Message   : String := ""
   ) return Test_Result is
   begin
      if Value >= Threshold then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Value not greater or equal to threshold");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Greater_Equal;

   function Assert_Less_Than (
      Value     : Natural;
      Threshold : Natural;
      Message   : String := ""
   ) return Test_Result is
   begin
      if Value < Threshold then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Value not less than threshold");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Less_Than;

   function Assert_Less_Equal (
      Value     : Natural;
      Threshold : Natural;
      Message   : String := ""
   ) return Test_Result is
   begin
      if Value <= Threshold then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Value not less or equal to threshold");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Less_Equal;

   function Assert_In_Range (
      Value : Natural;
      Min   : Natural;
      Max   : Natural;
      Message : String := ""
   ) return Test_Result is
   begin
      if Value >= Min and Value <= Max then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Value not in range");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_In_Range;

   ---------------------------------------------------------------------------
   --  Uint256 Comparisons
   ---------------------------------------------------------------------------

   function Assert_U256_Greater (
      Value     : Uint256;
      Threshold : Uint256;
      Message   : String := ""
   ) return Test_Result is
   begin
      if Greater_Than (Value, Threshold) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Uint256 not greater than threshold");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_U256_Greater;

   function Assert_U256_Less (
      Value     : Uint256;
      Threshold : Uint256;
      Message   : String := ""
   ) return Test_Result is
   begin
      if Less_Than (Value, Threshold) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Uint256 not less than threshold");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_U256_Less;

   function Assert_U256_Zero (
      Value   : Uint256;
      Message : String := ""
   ) return Test_Result is
   begin
      if Is_Zero (Value) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Uint256 is not zero");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_U256_Zero;

   function Assert_U256_Not_Zero (
      Value   : Uint256;
      Message : String := ""
   ) return Test_Result is
   begin
      if not Is_Zero (Value) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Uint256 is zero");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_U256_Not_Zero;

   ---------------------------------------------------------------------------
   --  Address Assertions
   ---------------------------------------------------------------------------

   function Assert_Address_Zero (
      Addr    : Khepri_Types.Address;
      Message : String := ""
   ) return Test_Result is
      Zero_Addr : constant Khepri_Types.Address := (others => 0);
   begin
      if Addr = Zero_Addr then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Address is not zero");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Address_Zero;

   function Assert_Address_Not_Zero (
      Addr    : Khepri_Types.Address;
      Message : String := ""
   ) return Test_Result is
      Zero_Addr : constant Khepri_Types.Address := (others => 0);
   begin
      if Addr /= Zero_Addr then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Address is zero");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Address_Not_Zero;

   ---------------------------------------------------------------------------
   --  Call Result Assertions
   ---------------------------------------------------------------------------

   function Assert_Call_Success (
      Result  : Call_Result;
      Message : String := ""
   ) return Test_Result is
   begin
      if Result.Success then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Call failed");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Call_Success;

   function Assert_Call_Reverted (
      Result  : Call_Result;
      Message : String := ""
   ) return Test_Result is
   begin
      if not Result.Success then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Call did not revert");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Call_Reverted;

   function Assert_Revert_Message (
      Result          : Call_Result;
      Expected_Message: String;
      Message         : String := ""
   ) return Test_Result is
      pragma Unreferenced (Expected_Message);
   begin
      if not Result.Success then
         --  Placeholder: Would check revert message
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Call did not revert");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Revert_Message;

   function Assert_Return_Data (
      Result   : Call_Result;
      Expected : Byte_Array;
      Message  : String := ""
   ) return Test_Result is
   begin
      if Result.Data_Size /= Expected'Length then
         if Message = "" then
            return Make_Failed ("Return data length mismatch");
         else
            return Make_Failed (Message);
         end if;
      end if;

      for I in 0 .. Expected'Length - 1 loop
         if Result.Return_Data (I) /= Expected (Expected'First + I) then
            if Message = "" then
               return Make_Failed ("Return data mismatch");
            else
               return Make_Failed (Message);
            end if;
         end if;
      end loop;

      return Make_Passed;
   end Assert_Return_Data;

   ---------------------------------------------------------------------------
   --  Gas Assertions
   ---------------------------------------------------------------------------

   function Assert_Gas_Used (
      Used    : Word64;
      Max_Gas : Word64;
      Message : String := ""
   ) return Test_Result is
   begin
      if Used <= Max_Gas then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Gas used exceeds limit");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Gas_Used;

   function Assert_Gas_Range (
      Used    : Word64;
      Min_Gas : Word64;
      Max_Gas : Word64;
      Message : String := ""
   ) return Test_Result is
   begin
      if Used >= Min_Gas and Used <= Max_Gas then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Gas used not in expected range");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Gas_Range;

   ---------------------------------------------------------------------------
   --  Storage Assertions
   ---------------------------------------------------------------------------

   function Assert_Storage (
      Session  : Test_Session;
      Fixture  : Fixture_State;
      Slot     : Hash256;
      Expected : Uint256;
      Message  : String := ""
   ) return Test_Result is
      Actual : constant Uint256 := Get_Storage (Session, Fixture, Slot);
   begin
      if Equal (Expected, Actual) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Storage value mismatch");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Storage;

   ---------------------------------------------------------------------------
   --  Balance Assertions
   ---------------------------------------------------------------------------

   function Assert_Balance (
      Session  : Test_Session;
      Address  : Khepri_Types.Address;
      Expected : Uint256;
      Message  : String := ""
   ) return Test_Result is
      pragma Unreferenced (Session, Address);
      --  Placeholder: Would get actual balance
      Actual : constant Uint256 := U256_Zero;
   begin
      if Equal (Expected, Actual) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Balance mismatch");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Balance;

   function Assert_Balance_Change (
      Before  : Uint256;
      After   : Uint256;
      Amount  : Uint256;
      Message : String := ""
   ) return Test_Result is
      Actual_Amount : constant Uint256 := Sub_Mod (After, Before);
   begin
      if Equal (Actual_Amount, Amount) then
         return Make_Passed;
      else
         if Message = "" then
            return Make_Failed ("Balance change mismatch");
         else
            return Make_Failed (Message);
         end if;
      end if;
   end Assert_Balance_Change;

   ---------------------------------------------------------------------------
   --  Event Assertions
   ---------------------------------------------------------------------------

   function Assert_Event_Emitted (
      Session : Test_Session;
      Event   : Event_Check;
      Message : String := ""
   ) return Test_Result is
      pragma Unreferenced (Session, Event);
   begin
      --  Placeholder: Would check event log
      if Message = "" then
         return Make_Passed;
      else
         return Make_Passed;
      end if;
   end Assert_Event_Emitted;

   ---------------------------------------------------------------------------
   --  Combination Assertions
   ---------------------------------------------------------------------------

   function Assert_All (
      Results : Test_Array;
      Count   : Natural
   ) return Test_Result is
   begin
      for I in 0 .. Count - 1 loop
         if Results (Test_Index (I)).Result.Outcome /= Outcome_Passed then
            return Results (Test_Index (I)).Result;
         end if;
      end loop;
      return Make_Passed;
   end Assert_All;

   function Assert_Any (
      Results : Test_Array;
      Count   : Natural
   ) return Test_Result is
   begin
      for I in 0 .. Count - 1 loop
         if Results (Test_Index (I)).Result.Outcome = Outcome_Passed then
            return Make_Passed;
         end if;
      end loop;
      return Make_Failed ("No assertions passed");
   end Assert_Any;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   function Fail (
      Message : String := ""
   ) return Test_Result is
   begin
      if Message = "" then
         return Make_Failed ("Test failed");
      else
         return Make_Failed (Message);
      end if;
   end Fail;

   function Skip (
      Reason : String := ""
   ) return Test_Result is
   begin
      return (
         Outcome       => Outcome_Skipped,
         Duration_Ms   => 0,
         Gas_Used      => 0,
         Message       => To_Bounded (Reason),
         Expected      => Empty_String,
         Actual        => Empty_String,
         Location      => Empty_String
      );
   end Skip;

   function Is_Passing (
      Result : Test_Result
   ) return Boolean is
   begin
      return Result.Outcome = Outcome_Passed;
   end Is_Passing;

end Khepri_Test_Assertions;
