pragma SPARK_Mode (On);

package body Aegis_Gas with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Gas Operations
   ---------------------------------------------------------------------------

   function Apply_Discount (
      Base_Gas : Gas_Amount;
      Discount : Discount_Factor
   ) return Gas_Amount is
      --  Precondition guarantees Base_Gas <= 922_337_203_685_477
      --  Max discount is 10000
      --  Max product = 922_337_203_685_477 * 10000 = 9,223,372,036,854,770,000
      --  Long_Long_Integer'Last = 9,223,372,036,854,775,807 (safe margin)
      Base_LLI : constant Long_Long_Integer := Long_Long_Integer (Base_Gas);
      Disc_LLI : constant Long_Long_Integer := Long_Long_Integer (Discount);
      Product  : Long_Long_Integer;
   begin
      --  Help prover: bounds from precondition and type
      pragma Assert (Base_LLI >= 0 and then Base_LLI <= 922_337_203_685_477);
      pragma Assert (Disc_LLI >= 7000 and then Disc_LLI <= 10000);

      Product := Base_LLI * Disc_LLI;
      return Gas_Amount (Product / 10000);
   end Apply_Discount;

   function Has_Gas (
      Context  : Gas_Context;
      Required : Gas_Amount
   ) return Boolean is
   begin
      if Context.Gas_Used > Context.Gas_Limit then
         return False;
      end if;
      return Context.Gas_Limit - Context.Gas_Used >= Required;
   end Has_Gas;

   procedure Consume_Gas (
      Context  : in out Gas_Context;
      Amount   : in     Gas_Amount;
      Success  : out    Boolean
   ) is
   begin
      if Has_Gas (Context, Amount) then
         Context.Gas_Used := Context.Gas_Used + Amount;
         Success := True;
      else
         Success := False;
      end if;
   end Consume_Gas;

   procedure Consume_Gas_Discounted (
      Context  : in Out Gas_Context;
      Base_Gas : in     Gas_Amount;
      Success  : out    Boolean
   ) is
      Discounted : constant Gas_Amount := Apply_Discount (Base_Gas, Context.Discount);
   begin
      Consume_Gas (Context, Discounted, Success);
   end Consume_Gas_Discounted;

   function Remaining_Gas (Context : Gas_Context) return Gas_Amount is
   begin
      if Context.Gas_Used <= Context.Gas_Limit then
         return Context.Gas_Limit - Context.Gas_Used;
      else
         return 0;
      end if;
   end Remaining_Gas;

   procedure Refund_Gas (
      Context : in Out Gas_Context;
      Amount  : in     Gas_Amount
   ) is
   begin
      Context.Gas_Used := Context.Gas_Used - Amount;
   end Refund_Gas;

   ---------------------------------------------------------------------------
   --  Complex Gas Calculations
   ---------------------------------------------------------------------------

   function Gas_Hash (
      Byte_Length : Natural
   ) return Gas_Amount is
      --  Precondition: Byte_Length <= Natural'Last - 31
      --  So Byte_Length + 31 <= Natural'Last (no overflow)
      pragma Assert (Byte_Length <= Natural'Last - 31);
      Words : constant Natural := (Byte_Length + 31) / 32;
      --  Max Words = Natural'Last / 32 = 67108863 (on 32-bit Natural)
      --  Gas_Amount(Words) <= 67108863
      --  Gas_Amount(Words) * 6 <= 402653178
      --  30 + 402653178 = 402653208 << Max_Safe_Base_Gas
      Gas_Words : constant Gas_Amount := Gas_Amount (Words);
      Gas_Per_Word : constant Gas_Amount := Gas_Words * Gas_SHA3_Per_Word;
   begin
      pragma Assert (Words <= 67_108_863);
      pragma Assert (Gas_Words <= 67_108_863);
      pragma Assert (Gas_Per_Word <= 402_653_178);
      pragma Assert (Gas_SHA3_Base + Gas_Per_Word <= Max_Safe_Base_Gas);
      return Gas_SHA3_Base + Gas_Per_Word;
   end Gas_Hash;

   function Gas_Memory_Expansion (
      Current_Words : Natural;
      New_Words     : Natural
   ) return Gas_Amount is
   begin
      if New_Words <= Current_Words then
         return 0;
      end if;

      --  Memory cost formula: words * 3 + words^2 / 512
      --  Expansion cost is difference between new and current
      declare
         function Mem_Cost (Words : Natural) return Gas_Amount is
            W : constant Gas_Amount := Gas_Amount (Words);
         begin
            return W * 3 + (W * W) / 512;
         end Mem_Cost;

         New_Cost : constant Gas_Amount := Mem_Cost (New_Words);
         Old_Cost : constant Gas_Amount := Mem_Cost (Current_Words);
      begin
         return New_Cost - Old_Cost;
      end;
   end Gas_Memory_Expansion;

   function Gas_Call_Complex (
      Has_Value    : Boolean;
      Is_New       : Boolean;
      Available    : Gas_Amount
   ) return Gas_Amount is
      Cost : Gas_Amount := Gas_Call_Base;
   begin
      if Has_Value then
         Cost := Cost + Gas_Call_Value;
      end if;

      if Is_New then
         Cost := Cost + Gas_Call_New_Account;
      end if;

      --  Cap at available gas (leave 1/64 for caller)
      declare
         Max_For_Call : constant Gas_Amount := Available - Available / 64;
      begin
         if Cost > Max_For_Call then
            return Max_For_Call;
         else
            return Cost;
         end if;
      end;
   end Gas_Call_Complex;

   function Gas_Log (
      Topic_Count : Natural;
      Data_Length : Natural
   ) return Gas_Amount is
      --  Precondition guarantees:
      --    Topic_Count <= 4, Data_Length <= 1_000_000
      --  Maximum result:
      --    Gas_Log_Base + 4 * Gas_Log_Topic + 1_000_000 * Gas_Log_Per_Byte
      --    = 375 + 4 * 375 + 1_000_000 * 8
      --    = 375 + 1500 + 8_000_000 = 8_001_875 << Max_Safe_Base_Gas
      Topic_Cost : constant Gas_Amount := Gas_Amount (Topic_Count) * Gas_Log_Topic;
      Data_Cost  : constant Gas_Amount := Gas_Amount (Data_Length) * Gas_Log_Per_Byte;
   begin
      --  Help prover: bounds from precondition
      pragma Assert (Topic_Count <= 4);
      pragma Assert (Data_Length <= Max_Log_Data_Length);

      --  Help prover: intermediate bounds
      pragma Assert (Topic_Cost <= 1_500);
      pragma Assert (Data_Cost <= 8_000_000);
      pragma Assert (Gas_Log_Base + Topic_Cost + Data_Cost <= 8_001_875);
      pragma Assert (Gas_Log_Base + Topic_Cost + Data_Cost <= Max_Safe_Base_Gas);

      return Gas_Log_Base + Topic_Cost + Data_Cost;
   end Gas_Log;

   function Gas_Create_Contract (
      Code_Length : Natural
   ) return Gas_Amount is
   begin
      return Gas_Create + Gas_Amount (Code_Length) * Gas_Create_Per_Byte;
   end Gas_Create_Contract;

   function Gas_STARK_Batch_Verify (
      Num_Signatures : Natural
   ) return Gas_Amount is
   begin
      return Gas_STARK_Verify_Base + Gas_Amount (Num_Signatures) * Gas_STARK_Verify_Per_Sig;
   end Gas_STARK_Batch_Verify;

   function Gas_Privacy_Operation (
      Op_Type     : Natural;
      Data_Length : Natural
   ) return Gas_Amount is
      Base_Cost : Gas_Amount;
      Per_Byte  : constant Gas_Amount := 10;  -- Additional per-byte cost
   begin
      case Op_Type is
         when 0 =>
            Base_Cost := Gas_Private_Load;
         when 1 =>
            Base_Cost := Gas_Private_Store;
         when 2 =>
            Base_Cost := Gas_Shield;
         when 3 =>
            Base_Cost := Gas_Unshield;
         when others =>
            Base_Cost := Gas_Verify_Proof;  -- Default to verify
      end case;

      return Base_Cost + Gas_Amount (Data_Length) * Per_Byte;
   end Gas_Privacy_Operation;

   ---------------------------------------------------------------------------
   --  WCET Integration
   ---------------------------------------------------------------------------

   function Cycles_To_Gas (Cycles : Natural) return Gas_Amount is
   begin
      return Gas_Amount (Cycles / Cycles_Per_Gas);
   end Cycles_To_Gas;

   function Validate_WCET_Bound (
      WCET_Gas  : Gas_Amount;
      Gas_Limit : Gas_Amount
   ) return Boolean is
   begin
      return WCET_Gas <= Gas_Limit;
   end Validate_WCET_Bound;

end Aegis_Gas;
