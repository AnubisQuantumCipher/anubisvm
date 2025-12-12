pragma SPARK_Mode (On);

package body Gas_Estimation with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Gas Estimation Functions
   ---------------------------------------------------------------------------

   function Estimate_Calldata_Gas (
      Zero_Bytes    : Natural;
      NonZero_Bytes : Natural
   ) return Gas_Amount is
   begin
      return Gas_Amount (Zero_Bytes) * Tx_Data_Zero_Cost +
             Gas_Amount (NonZero_Bytes) * Tx_Data_Nonzero_Cost;
   end Estimate_Calldata_Gas;

   function Estimate_Memory_Gas (
      Old_Size : Natural;
      New_Size : Natural
   ) return Gas_Amount is
      Old_Words : constant Natural := (Old_Size + 31) / 32;
      New_Words : constant Natural := (New_Size + 31) / 32;
      Linear_Cost : Gas_Amount;
      Old_Quadratic : Gas_Amount;
      New_Quadratic : Gas_Amount;
   begin
      if New_Words <= Old_Words then
         return 0;
      end if;

      --  Linear cost
      Linear_Cost := Gas_Amount (New_Words - Old_Words) * Memory_Cost_Per_Word;

      --  Quadratic cost (prevents excessive memory allocation)
      Old_Quadratic := Gas_Amount (Old_Words * Old_Words) / Memory_Quadratic_Divisor;
      New_Quadratic := Gas_Amount (New_Words * New_Words) / Memory_Quadratic_Divisor;

      return Linear_Cost + (New_Quadratic - Old_Quadratic);
   end Estimate_Memory_Gas;

   function Estimate_Storage_Gas (
      Is_Cold     : Boolean;
      Is_New_Slot : Boolean;
      Clear_Slot  : Boolean
   ) return Gas_Amount is
      Base_Cost : Gas_Amount;
   begin
      --  Cold access cost
      if Is_Cold then
         Base_Cost := Storage_Load_Cold;
      else
         Base_Cost := Storage_Load_Warm;
      end if;

      --  Write cost
      if Is_New_Slot then
         Base_Cost := Base_Cost + Storage_Set_Cost;
      else
         Base_Cost := Base_Cost + Storage_Update_Cost;
      end if;

      --  Note: Refund for clearing (reduces gas) is handled at end of transaction
      --  The Clear_Slot parameter is used for gas estimation purposes only
      pragma Unreferenced (Clear_Slot);

      return Base_Cost;
   end Estimate_Storage_Gas;

   function Estimate_SHA3_Gas (
      Data_Size : Natural
   ) return Gas_Amount is
      Words : constant Natural := (Data_Size + 31) / 32;
   begin
      return SHA3_Base_Cost + Gas_Amount (Words) * SHA3_Word_Cost;
   end Estimate_SHA3_Gas;

   function Estimate_Event_Gas (
      Topic_Count : Natural;
      Data_Size   : Natural
   ) return Gas_Amount is
   begin
      return Event_Base_Cost +
             Gas_Amount (Topic_Count) * Event_Topic_Cost +
             Gas_Amount (Data_Size) * Event_Data_Byte_Cost;
   end Estimate_Event_Gas;

   ---------------------------------------------------------------------------
   --  Certification-Adjusted Costs
   ---------------------------------------------------------------------------

   function Apply_Discount (
      Base_Cost : Gas_Amount;
      Level     : Certification_Level
   ) return Gas_Amount is
      Discount : constant Discount_Factor := Get_Discount (Level);
   begin
      --  Discount is in basis points (10000 = 100%)
      --  Platinum = 7000 (70%), Gold = 8000 (80%), etc.
      return (Base_Cost * Gas_Amount (Discount)) / 10000;
   end Apply_Discount;

   ---------------------------------------------------------------------------
   --  Gas Estimation for Contract Operations
   ---------------------------------------------------------------------------

   function Estimate_Deploy_Gas (
      Code_Size : Natural;
      Init_Storage_Slots : Natural
   ) return Gas_Amount is
      Code_Cost : constant Gas_Amount := Gas_Amount (Code_Size) * 200;
      Storage_Cost : constant Gas_Amount := Gas_Amount (Init_Storage_Slots) * Storage_Set_Cost;
   begin
      return Tx_Base_Cost + Tx_Create_Cost + Code_Cost + Storage_Cost;
   end Estimate_Deploy_Gas;

   function Estimate_Invoke_Overhead (
      Args_Size     : Natural;
      Is_Cold_Call  : Boolean;
      Has_Value     : Boolean
   ) return Gas_Amount is
      Total : Gas_Amount := Tx_Base_Cost + Call_Base_Cost;
   begin
      --  Calldata cost (assume mixed zero/non-zero)
      Total := Total + Gas_Amount (Args_Size) * Tx_Data_Nonzero_Cost;

      --  Cold account access
      if Is_Cold_Call then
         Total := Total + Account_Cold_Access;
      else
         Total := Total + Account_Warm_Access;
      end if;

      --  Value transfer
      if Has_Value then
         Total := Total + Call_Value_Transfer;
      end if;

      return Total;
   end Estimate_Invoke_Overhead;

   ---------------------------------------------------------------------------
   --  Intrinsic Gas Calculation
   ---------------------------------------------------------------------------

   function Intrinsic_Gas (
      Data_Size     : Natural;
      Is_Create     : Boolean;
      Zero_Bytes    : Natural;
      NonZero_Bytes : Natural
   ) return Gas_Amount is
      pragma Unreferenced (Data_Size);  -- Used in precondition only
      Total : Gas_Amount := Tx_Base_Cost;
   begin
      --  Calldata cost
      Total := Total + Estimate_Calldata_Gas (Zero_Bytes, NonZero_Bytes);

      --  Contract creation cost
      if Is_Create then
         Total := Total + Tx_Create_Cost;
      end if;

      return Total;
   end Intrinsic_Gas;

end Gas_Estimation;
