--  Contract Template for AnubisVM
--  Replace CONTRACT_TEMPLATE with your contract name
--
--  SPARK/Ada Smart Contract Implementation

pragma SPARK_Mode (On);

with Khepri_State; use Khepri_State;

package body Contract_Template with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Storage Layout
   --
   --  Define your storage slots here. Each slot is 32 bytes.
   --  Use consistent slot numbering to avoid collisions.
   ---------------------------------------------------------------------------

   --  Slot 0: Total supply or other global value
   Total_Supply_Slot : constant Slot_Index := 0;

   --  Slots 1-999: Reserved for contract metadata
   Owner_Slot        : constant Slot_Index := 1;
   Name_Slot         : constant Slot_Index := 2;

   --  Slots 1000+: Dynamic storage (e.g., balances, mappings)
   Balance_Base_Slot : constant Slot_Index := 1000;

   ---------------------------------------------------------------------------
   --  Helper Functions
   ---------------------------------------------------------------------------

   --  Hash an address to a storage slot (for mappings)
   function Address_To_Slot (
      Base : Slot_Index;
      Addr : Address
   ) return Slot_Index
   with
      SPARK_Mode => On,
      Pre  => Base < Slot_Index'Last - 2**20,
      Post => Address_To_Slot'Result >= Base
   is
      --  Simple hash: use first 4 bytes of address mod 2^20
      Hash : constant Natural :=
         Natural (Addr (0)) * 256 * 256 * 256 +
         Natural (Addr (1)) * 256 * 256 +
         Natural (Addr (2)) * 256 +
         Natural (Addr (3));
   begin
      return Base + Slot_Index (Hash mod 2**20);
   end Address_To_Slot;

   ---------------------------------------------------------------------------
   --  Entry Point Implementations
   ---------------------------------------------------------------------------

   procedure Initialize (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) is
      Initial_Supply : U256;
   begin
      --  Decode initial supply from calldata
      if Context.Calldata_Length < 32 then
         Result := Make_Error (Error_Invalid_Calldata, 5000);
         return;
      end if;

      Initial_Supply := Decode_U256 (Context.Calldata (0 .. 31));

      --  Store total supply
      Store_U256 (State, Total_Supply_Slot, Initial_Supply);

      --  Store owner (deployer)
      Store_Address (State, Owner_Slot, Context.Sender);

      --  Give all tokens to deployer
      Store_U256 (
         State,
         Address_To_Slot (Balance_Base_Slot, Context.Sender),
         Initial_Supply
      );

      Result := Make_Success (Gas_Used => 50_000);
   end Initialize;

   procedure Transfer (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) is
      To           : Address;
      Amount       : U256;
      From_Slot    : Slot_Index;
      To_Slot      : Slot_Index;
      From_Balance : U256;
      To_Balance   : U256;
   begin
      --  Decode parameters: to (32 bytes) + amount (32 bytes)
      if Context.Calldata_Length < 64 then
         Result := Make_Error (Error_Invalid_Calldata, 5000);
         return;
      end if;

      To     := Decode_Address (Context.Calldata (0 .. 31));
      Amount := Decode_U256 (Context.Calldata (32 .. 63));

      --  Get storage slots
      From_Slot := Address_To_Slot (Balance_Base_Slot, Context.Sender);
      To_Slot   := Address_To_Slot (Balance_Base_Slot, To);

      --  Load current balances
      From_Balance := Load_U256 (State, From_Slot);
      To_Balance   := Load_U256 (State, To_Slot);

      --  Check sufficient balance
      if From_Balance < Amount then
         Result := Make_Error (Error_Insufficient_Balance, 10_000);
         return;
      end if;

      --  Check for overflow (defensive)
      if To_Balance > U256'Last - Amount then
         Result := Make_Error (Error_Overflow, 10_000);
         return;
      end if;

      --  Update balances
      Store_U256 (State, From_Slot, From_Balance - Amount);
      Store_U256 (State, To_Slot, To_Balance + Amount);

      --  Return success with amount transferred
      Result := Make_Success (
         Gas_Used    => 30_000,
         Return_Data => Encode_U256 (Amount)
      );
   end Transfer;

   procedure Balance_Of (
      Context : in     Call_Context;
      State   : in Out State_Array;
      Result  :    out Exec_Result
   ) is
      Account : Address;
      Slot    : Slot_Index;
      Balance : U256;
   begin
      --  Decode account address
      if Context.Calldata_Length < 32 then
         Result := Make_Error (Error_Invalid_Calldata, 2000);
         return;
      end if;

      Account := Decode_Address (Context.Calldata (0 .. 31));
      Slot    := Address_To_Slot (Balance_Base_Slot, Account);
      Balance := Load_U256 (State, Slot);

      Result := Make_Success (
         Gas_Used    => 5_000,
         Return_Data => Encode_U256 (Balance)
      );
   end Balance_Of;

   ---------------------------------------------------------------------------
   --  CVM Registration
   ---------------------------------------------------------------------------

   function Get_Descriptor return CVM_Descriptor is
   begin
      return (
         Name       => "Contract_Template                       ",  --  40 chars
         Version    => 1,
         Methods    => (
            1 => (Selector => 16#00000001#, Name => "Initialize"),
            2 => (Selector => 16#00000002#, Name => "Transfer"),
            3 => (Selector => 16#00000003#, Name => "Balance_Of")
         ),
         Capability => Cap_Read_State or Cap_Write_State
      );
   end Get_Descriptor;

end Contract_Template;
