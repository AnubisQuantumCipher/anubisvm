--  KHEPRI State Trie Implementation
pragma SPARK_Mode (On);

with Interfaces; use Interfaces;

package body Khepri_State_Trie with
   SPARK_Mode => On,
   Refined_State => (State_Trie_Data => (Main_Trie, Initialized))
is

   ---------------------------------------------------------------------------
   --  State Variables
   ---------------------------------------------------------------------------

   Main_Trie   : Trie_ID := Null_Trie;
   Initialized : Boolean := False;

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   --  Convert Node_Value to Byte_Array
   function Node_Value_To_Bytes (
      V      : Node_Value;
      Length : Natural
   ) return Byte_Array is
      Result : Byte_Array (0 .. Length - 1) := (others => 0);
   begin
      for I in 0 .. Length - 1 loop
         if I <= V'Last then
            Result (I) := V (I);
         end if;
      end loop;
      return Result;
   end Node_Value_To_Bytes;

   ---------------------------------------------------------------------------
   --  RLP Encoding for Account
   ---------------------------------------------------------------------------

   procedure Encode_Account (
      Account : in     Account_State;
      Output  : out    Byte_Array;
      Length  : out    Natural
   ) is
      Pos : Natural := 0;
   begin
      Output := (others => 0);

      --  RLP list header (placeholder, we"ll fix length after)
      Output (0) := 16#F8#;  --  Long list
      Pos := 2;

      --  Encode nonce
      if Account.Nonce = 0 then
         Output (Pos) := 16#80#;
         Pos := Pos + 1;
      elsif Account.Nonce < 128 then
         Output (Pos) := Unsigned_8 (Account.Nonce);
         Pos := Pos + 1;
      elsif Account.Nonce < 256 then
         Output (Pos) := 16#81#;
         Output (Pos + 1) := Unsigned_8 (Account.Nonce);
         Pos := Pos + 2;
      else
         Output (Pos) := 16#88#;  --  8 bytes
         for I in reverse 0 .. 7 loop
            Output (Pos + 1 + (7 - I)) := Unsigned_8 (
               Shift_Right (Account.Nonce, I * 8) and 16#FF#);
         end loop;
         Pos := Pos + 9;
      end if;

      --  Encode balance (U256 - up to 32 bytes)
      declare
         Balance_Hash  : constant Hash256 := To_Bytes_BE (Account.Balance);
         Balance_Bytes : Byte_Array (0 .. 31);
         Balance_Len   : Natural := 0;
      begin
         --  Convert Hash256 to Byte_Array
         for I in 0 .. 31 loop
            Balance_Bytes (I) := Balance_Hash (I);
         end loop;

         --  Find first non-zero
         for I in Balance_Bytes'Range loop
            if Balance_Bytes (I) /= 0 then
               Balance_Len := 32 - I;
               exit;
            end if;
         end loop;

         if Balance_Len = 0 then
            Output (Pos) := 16#80#;
            Pos := Pos + 1;
         elsif Balance_Len = 1 and then Balance_Bytes (31) < 128 then
            Output (Pos) := Balance_Bytes (31);
            Pos := Pos + 1;
         else
            Output (Pos) := 16#80# + Unsigned_8 (Balance_Len);
            Pos := Pos + 1;
            for I in 32 - Balance_Len .. 31 loop
               Output (Pos) := Balance_Bytes (I);
               Pos := Pos + 1;
            end loop;
         end if;
      end;

      --  Encode storage root (32 bytes)
      Output (Pos) := 16#A0#;  --  32-byte string
      Pos := Pos + 1;
      for I in Hash_Index loop
         Output (Pos) := Account.Storage_Root (I);
         Pos := Pos + 1;
      end loop;

      --  Encode code hash (32 bytes)
      Output (Pos) := 16#A0#;
      Pos := Pos + 1;
      for I in Hash_Index loop
         Output (Pos) := Account.Code_Hash (I);
         Pos := Pos + 1;
      end loop;

      --  Fix list length
      declare
         Content_Len : constant Natural := Pos - 2;
      begin
         Output (1) := Unsigned_8 (Content_Len);
      end;

      Length := Pos;
   end Encode_Account;

   procedure Decode_Account (
      Input   : in     Byte_Array;
      Account : out    Account_State;
      Success : out    Boolean
   ) is
      Pos : Natural := 0;
   begin
      Account := Empty_Account;
      Success := False;

      if Input'Length < 4 then
         return;
      end if;

      --  Skip list header
      if Input (Input'First) >= 16#F8# then
         Pos := 2;
      elsif Input (Input'First) >= 16#C0# then
         Pos := 1;
      else
         return;
      end if;

      --  Decode nonce
      if Input (Input'First + Pos) < 16#80# then
         Account.Nonce := Word64 (Input (Input'First + Pos));
         Pos := Pos + 1;
      elsif Input (Input'First + Pos) = 16#80# then
         Account.Nonce := 0;
         Pos := Pos + 1;
      elsif Input (Input'First + Pos) <= 16#88# then
         declare
            Len : constant Natural := Natural (Input (Input'First + Pos) - 16#80#);
         begin
            Account.Nonce := 0;
            Pos := Pos + 1;
            for I in 0 .. Len - 1 loop
               Account.Nonce := Shift_Left (Account.Nonce, 8) or
                  Word64 (Input (Input'First + Pos + I));
            end loop;
            Pos := Pos + Len;
         end;
      end if;

      --  Decode balance (simplified - assume it"s there)
      if Input (Input'First + Pos) < 16#80# then
         Account.Balance := From_Word64 (Word64 (Input (Input'First + Pos)));
         Pos := Pos + 1;
      elsif Input (Input'First + Pos) = 16#80# then
         Account.Balance := U256_Zero;
         Pos := Pos + 1;
      elsif Input (Input'First + Pos) <= 16#A0# then
         declare
            Len : constant Natural := Natural (Input (Input'First + Pos) - 16#80#);
         begin
            Pos := Pos + 1;
            --  Build U256 from bytes (simplified)
            Account.Balance := U256_Zero;
            for I in 0 .. Len - 1 loop
               Account.Balance := Shift_Left (Account.Balance, 8);
               Account.Balance := Bit_Or (Account.Balance,
                  From_Word64 (Word64 (Input (Input'First + Pos + I))));
            end loop;
            Pos := Pos + Len;
         end;
      end if;

      --  Decode storage root (32 bytes)
      if Input (Input'First + Pos) = 16#A0# then
         Pos := Pos + 1;
         for I in Hash_Index loop
            Account.Storage_Root (I) := Input (Input'First + Pos + I);
         end loop;
         Pos := Pos + 32;
      end if;

      --  Decode code hash (32 bytes)
      if Input (Input'First + Pos) = 16#A0# then
         Pos := Pos + 1;
         for I in Hash_Index loop
            Account.Code_Hash (I) := Input (Input'First + Pos + I);
         end loop;
      end if;

      Success := True;
   end Decode_Account;

   ---------------------------------------------------------------------------
   --  Initialization
   ---------------------------------------------------------------------------

   procedure Initialize is
      Success : Boolean;
   begin
      Create_Trie (Main_Trie, Success);
      Initialized := Success;
   end Initialize;

   procedure Initialize_From_Root (
      Root    : in     Hash_256;
      Success : out    Boolean
   ) is
   begin
      Load_Trie (Root, Main_Trie, Success);
      Initialized := Success;
   end Initialize_From_Root;

   ---------------------------------------------------------------------------
   --  Account Operations
   ---------------------------------------------------------------------------

   procedure Get_Account (
      Addr    : in     Address;
      Account : out    Account_State;
      Found   : out    Boolean;
      Error   : out    State_Error
   ) is
      Value     : Value_Data;
      MPT_Found : Boolean;
      MPT_Error : Khepri_MPT_Types.MPT_Error;
      Key       : Byte_Array (0 .. 19);
   begin
      Account := Empty_Account;
      Found := False;
      Error := Error_None;

      if not Initialized then
         Error := Error_Invalid_State;
         return;
      end if;

      --  Convert address to key
      for I in Key'Range loop
         Key (I) := Addr (I);
      end loop;

      Get (Main_Trie, Key, Value, MPT_Found, MPT_Error);

      if MPT_Error /= Khepri_MPT_Types.Error_None and then
         MPT_Error /= Khepri_MPT_Types.Error_Key_Not_Found
      then
         Error := Error_Trie_Error;
         return;
      end if;

      if not MPT_Found then
         Found := False;
         return;
      end if;

      --  Decode account from value
      declare
         Decode_Success : Boolean;
         Val_Bytes      : constant Byte_Array :=
            Node_Value_To_Bytes (Value.Bytes, Value.Length);
      begin
         Decode_Account (Val_Bytes, Account, Decode_Success);
         Found := Decode_Success;
      end;
   end Get_Account;

   procedure Set_Account (
      Addr    : in     Address;
      Account : in     Account_State;
      Success : out    Boolean;
      Error   : out    State_Error
   ) is
      Encoded   : Byte_Array (0 .. 127) := (others => 0);
      Enc_Len   : Natural;
      Key       : Byte_Array (0 .. 19);
      MPT_Error : Khepri_MPT_Types.MPT_Error;
   begin
      Success := False;
      Error := Error_None;

      if not Initialized then
         Error := Error_Invalid_State;
         return;
      end if;

      --  Convert address to key
      for I in Key'Range loop
         Key (I) := Addr (I);
      end loop;

      --  Encode account
      Encode_Account (Account, Encoded, Enc_Len);

      --  Store in trie
      Put (Main_Trie, Key, Encoded (0 .. Enc_Len - 1), Success, MPT_Error);

      if MPT_Error /= Khepri_MPT_Types.Error_None then
         Error := Error_Trie_Error;
         Success := False;
      end if;
   end Set_Account;

   procedure Delete_Account (
      Addr    : in     Address;
      Success : out    Boolean;
      Error   : out    State_Error
   ) is
      Key       : Byte_Array (0 .. 19);
      MPT_Error : Khepri_MPT_Types.MPT_Error;
   begin
      Success := False;
      Error := Error_None;

      if not Initialized then
         Error := Error_Invalid_State;
         return;
      end if;

      for I in Key'Range loop
         Key (I) := Addr (I);
      end loop;

      Delete (Main_Trie, Key, Success, MPT_Error);

      if MPT_Error /= Khepri_MPT_Types.Error_None then
         Error := Error_Trie_Error;
      end if;
   end Delete_Account;

   function Account_Exists (Addr : Address) return Boolean is
      Key : Byte_Array (0 .. 19);
   begin
      if not Initialized then
         return False;
      end if;

      for I in Key'Range loop
         Key (I) := Addr (I);
      end loop;

      return Contains (Main_Trie, Key);
   end Account_Exists;

   ---------------------------------------------------------------------------
   --  Balance Operations
   ---------------------------------------------------------------------------

   function Get_Balance (Addr : Address) return U256 is
      Account : Account_State;
      Found   : Boolean;
      Error   : State_Error;
   begin
      Get_Account (Addr, Account, Found, Error);
      if Found then
         return Account.Balance;
      else
         return U256_Zero;
      end if;
   end Get_Balance;

   procedure Add_Balance (
      Addr    : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Error
   ) is
      Account    : Account_State;
      Found      : Boolean;
      New_Bal    : U256;
      Overflow   : Boolean;
   begin
      Get_Account (Addr, Account, Found, Error);

      if Error /= Error_None then
         Success := False;
         return;
      end if;

      if not Found then
         Account := Empty_Account;
      end if;

      Add (Account.Balance, Amount, New_Bal, Overflow);

      if Overflow then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Account.Balance := New_Bal;
      Set_Account (Addr, Account, Success, Error);
   end Add_Balance;

   procedure Sub_Balance (
      Addr    : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Error
   ) is
      Account : Account_State;
      Found   : Boolean;
   begin
      Get_Account (Addr, Account, Found, Error);

      if Error /= Error_None then
         Success := False;
         return;
      end if;

      if not Found then
         Success := False;
         Error := Error_Account_Not_Found;
         return;
      end if;

      if Less_Than (Account.Balance, Amount) then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Account.Balance := Sub_Mod (Account.Balance, Amount);
      Set_Account (Addr, Account, Success, Error);
   end Sub_Balance;

   procedure Transfer (
      From    : in     Address;
      To      : in     Address;
      Amount  : in     U256;
      Success : out    Boolean;
      Error   : out    State_Error
   ) is
   begin
      --  Subtract from sender
      Sub_Balance (From, Amount, Success, Error);
      if not Success then
         return;
      end if;

      --  Add to recipient
      Add_Balance (To, Amount, Success, Error);
      if not Success then
         --  Rollback (add back to sender)
         declare
            Rollback_Success : Boolean;
            Rollback_Error   : State_Error;
         begin
            Add_Balance (From, Amount, Rollback_Success, Rollback_Error);
         end;
      end if;
   end Transfer;

   ---------------------------------------------------------------------------
   --  Nonce Operations
   ---------------------------------------------------------------------------

   function Get_Nonce (Addr : Address) return Word64 is
      Account : Account_State;
      Found   : Boolean;
      Error   : State_Error;
   begin
      Get_Account (Addr, Account, Found, Error);
      if Found then
         return Account.Nonce;
      else
         return 0;
      end if;
   end Get_Nonce;

   procedure Increment_Nonce (
      Addr    : in     Address;
      Success : out    Boolean;
      Error   : out    State_Error
   ) is
      Account : Account_State;
      Found   : Boolean;
   begin
      Get_Account (Addr, Account, Found, Error);

      if Error /= Error_None then
         Success := False;
         return;
      end if;

      if not Found then
         Account := Empty_Account;
      end if;

      if Account.Nonce = Word64'Last then
         Success := False;
         Error := Error_Overflow;
         return;
      end if;

      Account.Nonce := Account.Nonce + 1;
      Set_Account (Addr, Account, Success, Error);
   end Increment_Nonce;

   procedure Set_Nonce (
      Addr    : in     Address;
      Nonce   : in     Word64;
      Success : out    Boolean;
      Error   : out    State_Error
   ) is
      Account : Account_State;
      Found   : Boolean;
   begin
      Get_Account (Addr, Account, Found, Error);

      if Error /= Error_None then
         Success := False;
         return;
      end if;

      if not Found then
         Account := Empty_Account;
      end if;

      Account.Nonce := Nonce;
      Set_Account (Addr, Account, Success, Error);
   end Set_Nonce;

   ---------------------------------------------------------------------------
   --  Code Operations
   ---------------------------------------------------------------------------

   function Get_Code_Hash (Addr : Address) return Hash_256 is
      Account : Account_State;
      Found   : Boolean;
      Error   : State_Error;
   begin
      Get_Account (Addr, Account, Found, Error);
      if Found then
         return Account.Code_Hash;
      else
         return Empty_Code_Hash;
      end if;
   end Get_Code_Hash;

   procedure Set_Code_Hash (
      Addr      : in     Address;
      Code_Hash : in     Hash_256;
      Success   : out    Boolean;
      Error     : out    State_Error
   ) is
      Account : Account_State;
      Found   : Boolean;
   begin
      Get_Account (Addr, Account, Found, Error);

      if Error /= Error_None then
         Success := False;
         return;
      end if;

      if not Found then
         Account := Empty_Account;
      end if;

      Account.Code_Hash := Code_Hash;
      Set_Account (Addr, Account, Success, Error);
   end Set_Code_Hash;

   function Is_Contract (Addr : Address) return Boolean is
      Code_Hash_Val : constant Hash_256 := Get_Code_Hash (Addr);
   begin
      return not Hash_Equal (Code_Hash_Val, Empty_Code_Hash);
   end Is_Contract;

   ---------------------------------------------------------------------------
   --  Storage Root Operations
   ---------------------------------------------------------------------------

   function Get_Storage_Root (Addr : Address) return Hash_256 is
      Account : Account_State;
      Found   : Boolean;
      Error   : State_Error;
   begin
      Get_Account (Addr, Account, Found, Error);
      if Found then
         return Account.Storage_Root;
      else
         return Empty_Storage_Root;
      end if;
   end Get_Storage_Root;

   procedure Set_Storage_Root (
      Addr         : in     Address;
      Storage_Root : in     Hash_256;
      Success      : out    Boolean;
      Error        : out    State_Error
   ) is
      Account : Account_State;
      Found   : Boolean;
   begin
      Get_Account (Addr, Account, Found, Error);

      if Error /= Error_None then
         Success := False;
         return;
      end if;

      if not Found then
         Account := Empty_Account;
      end if;

      Account.Storage_Root := Storage_Root;
      Set_Account (Addr, Account, Success, Error);
   end Set_Storage_Root;

   ---------------------------------------------------------------------------
   --  State Root
   ---------------------------------------------------------------------------

   function State_Root return Hash_256 is
   begin
      if not Initialized then
         return Empty_Hash;
      end if;
      return Root_Hash (Main_Trie);
   end State_Root;

   procedure Commit (
      New_Root : out Hash_256
   ) is
   begin
      New_Root := State_Root;
   end Commit;

   ---------------------------------------------------------------------------
   --  Snapshots
   ---------------------------------------------------------------------------

   procedure Create_Snapshot (
      Snapshot_ID : out Natural;
      Success     : out Boolean
   ) is
   begin
      if not Initialized then
         Snapshot_ID := 0;
         Success := False;
         return;
      end if;

      Khepri_MPT.Create_Snapshot (Main_Trie, Snapshot_ID, Success);
   end Create_Snapshot;

   procedure Revert_To_Snapshot (
      Snapshot_ID : in     Natural;
      Success     : out    Boolean
   ) is
      Error : Khepri_MPT_Types.MPT_Error;
   begin
      if not Initialized then
         Success := False;
         return;
      end if;

      Restore_Snapshot (Main_Trie, Snapshot_ID, Success, Error);
   end Revert_To_Snapshot;

   procedure Discard_Snapshot (
      Snapshot_ID : in Natural
   ) is
   begin
      if Initialized then
         Khepri_MPT.Discard_Snapshot (Main_Trie, Snapshot_ID);
      end if;
   end Discard_Snapshot;

   ---------------------------------------------------------------------------
   --  Proofs
   ---------------------------------------------------------------------------

   procedure Generate_Account_Proof (
      Addr    : in     Address;
      Proof   : out    Merkle_Proof;
      Success : out    Boolean
   ) is
      Key   : Byte_Array (0 .. 19);
      Error : Khepri_MPT_Types.MPT_Error;
   begin
      if not Initialized then
         Proof := (
            Nodes  => (others => (Data => (others => 0), Length => 0)),
            Depth  => 0,
            Key    => Empty_Nibble_Key,
            Value  => Empty_Value,
            Exists => False
         );
         Success := False;
         return;
      end if;

      for I in Key'Range loop
         Key (I) := Addr (I);
      end loop;

      Generate_Proof (Main_Trie, Key, Proof, Success, Error);
   end Generate_Account_Proof;

   procedure Verify_Account_Proof (
      Root    : in     Hash_256;
      Addr    : in     Address;
      Proof   : in     Merkle_Proof;
      Account : out    Account_State;
      Valid   : out    Boolean
   ) is
      Error : Khepri_MPT_Types.MPT_Error;
   begin
      Account := Empty_Account;

      Verify_Proof (Root, Proof, Valid, Error);

      if Valid and then Proof.Exists then
         declare
            Decode_Success : Boolean;
            Val_Bytes      : constant Byte_Array :=
               Node_Value_To_Bytes (Proof.Value.Bytes, Proof.Value.Length);
         begin
            Decode_Account (Val_Bytes, Account, Decode_Success);
            Valid := Decode_Success;
         end;
      end if;
   end Verify_Account_Proof;

end Khepri_State_Trie;
