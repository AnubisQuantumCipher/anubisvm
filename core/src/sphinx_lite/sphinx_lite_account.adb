--  SPHINX Lite Account Implementation

pragma SPARK_Mode (On);

package body Sphinx_Lite_Account with
   SPARK_Mode => On,
   Refined_State => (Account_State => (
      Trusted_Root_State, Trusted_Block_State, Initialized_Flag,
      Proofs_Verified, Proofs_Successful, Total_Proof_Bytes))
is

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   Trusted_Root_State   : Hash256 := (others => 0);
   Trusted_Block_State  : Unsigned_64 := 0;
   Initialized_Flag     : Boolean := False;
   Proofs_Verified      : Natural := 0;
   Proofs_Successful    : Natural := 0;
   Total_Proof_Bytes    : Natural := 0;

   ---------------------------------------------------------------------------
   --  Internal Helpers
   ---------------------------------------------------------------------------

   --  Simple hash function placeholder (uses Keccak in production)
   function Simple_Hash (Data : Contract_Address) return Hash256 is
      Result : Hash256 := (others => 0);
   begin
      for I in Data'Range loop
         Result (I mod 32) := Result (I mod 32) xor Data (I);
      end loop;
      return Result;
   end Simple_Hash;

   function Simple_Hash_256 (Data : Hash256) return Hash256 is
      Result : Hash256 := (others => 0);
   begin
      for I in Data'Range loop
         Result ((I + 1) mod 32) := Result ((I + 1) mod 32) xor Data (I);
      end loop;
      return Result;
   end Simple_Hash_256;

   --  Combine two hashes (for Merkle nodes)
   function Combine_Hashes (Left, Right : Hash256) return Hash256 is
      Result : Hash256;
   begin
      for I in Hash256'Range loop
         Result (I) := Left (I) xor Right (I);
      end loop;
      return Simple_Hash_256 (Result);
   end Combine_Hashes;

   ---------------------------------------------------------------------------
   --  Initialize
   ---------------------------------------------------------------------------

   procedure Initialize (
      Trusted_Root : in Hash256;
      Block_Number : in Unsigned_64
   ) is
   begin
      Trusted_Root_State := Trusted_Root;
      Trusted_Block_State := Block_Number;
      Proofs_Verified := 0;
      Proofs_Successful := 0;
      Total_Proof_Bytes := 0;
      Initialized_Flag := True;
   end Initialize;

   ---------------------------------------------------------------------------
   --  Is_Initialized
   ---------------------------------------------------------------------------

   function Is_Initialized return Boolean is
   begin
      return Initialized_Flag;
   end Is_Initialized;

   ---------------------------------------------------------------------------
   --  Verify_Account_Existence
   ---------------------------------------------------------------------------

   procedure Verify_Account_Existence (
      Address     : in  Contract_Address;
      State_Root  : in  Hash256;
      Proof       : in  Merkle_Proof;
      Exists      : out Boolean;
      Error       : out Sphinx_Lite_Error
   ) is
      Key          : constant Hash256 := Compute_Account_Key (Address);
      Valid        : Boolean;
   begin
      Exists := False;
      Error := Error_None;

      --  Verify the Merkle proof
      Verify_Merkle_Branch (
         Leaf  => Key,
         Root  => State_Root,
         Proof => Proof,
         Key   => Key,
         Valid => Valid
      );

      if not Valid then
         Error := Error_Invalid_Proof;
         return;
      end if;

      --  Proof verified, account exists if leaf is non-null
      Exists := Proof.Leaf_Hash /= Hash256'(others => 0);
   end Verify_Account_Existence;

   ---------------------------------------------------------------------------
   --  Verify_Account_State
   ---------------------------------------------------------------------------

   procedure Verify_Account_State (
      Account     : in  Account_State_Data;
      State_Root  : in  Hash256;
      Proof       : in  Merkle_Proof;
      Result      : out Verification_Result
   ) is
      Key        : constant Hash256 := Compute_Account_Key (Account.Address);
      Valid      : Boolean;
   begin
      Result := (Success => False, Error => Error_None);

      --  Verify Merkle proof
      Verify_Merkle_Branch (
         Leaf  => Proof.Leaf_Hash,
         Root  => State_Root,
         Proof => Proof,
         Key   => Key,
         Valid => Valid
      );

      if not Valid then
         Result.Error := Error_Invalid_Proof;
         return;
      end if;

      --  Additional checks could verify RLP-encoded account data
      --  For now, trust the proof verification
      Result := Sphinx_Lite_Types.Success_Result;
   end Verify_Account_State;

   ---------------------------------------------------------------------------
   --  Verify_Balance
   ---------------------------------------------------------------------------

   procedure Verify_Balance (
      Address     : in  Contract_Address;
      Balance     : in  U256;
      State_Root  : in  Hash256;
      Proof       : in  Merkle_Proof;
      Valid       : out Boolean;
      Error       : out Sphinx_Lite_Error
   ) is
      pragma Unreferenced (Balance);
      Key : constant Hash256 := Compute_Account_Key (Address);
   begin
      Valid := False;
      Error := Error_None;

      --  Verify Merkle proof
      Verify_Merkle_Branch (
         Leaf  => Proof.Leaf_Hash,
         Root  => State_Root,
         Proof => Proof,
         Key   => Key,
         Valid => Valid
      );

      if not Valid then
         Error := Error_Invalid_Proof;
         return;
      end if;

      --  In production, would decode RLP and verify balance matches
      Valid := True;
   end Verify_Balance;

   ---------------------------------------------------------------------------
   --  Verify_Account_Proof
   ---------------------------------------------------------------------------

   procedure Verify_Account_Proof (
      Proof   : in  Account_Proof;
      Result  : out Verification_Result
   ) is
   begin
      Result := (Success => False, Error => Error_None);

      --  Check basic validity
      if not Proof.Valid then
         Result.Error := Error_Invalid_Proof;
         return;
      end if;

      --  Verify against trusted root
      if Proof.State_Root /= Trusted_Root_State then
         Result.Error := Error_State_Root_Mismatch;
         return;
      end if;

      --  Verify account state
      if Proof.Account.Exists then
         --  Check proof depth to satisfy precondition
         if Proof.Proof.Depth > Max_Proof_Depth then
            Result.Error := Error_Invalid_Proof;
            return;
         end if;

         Verify_Account_State (
            Account    => Proof.Account,
            State_Root => Proof.State_Root,
            Proof      => Proof.Proof,
            Result     => Result
         );
      else
         Result := Sphinx_Lite_Types.Success_Result;
      end if;

      --  Update statistics
      if Proofs_Verified < Natural'Last then
         Proofs_Verified := Proofs_Verified + 1;
      end if;

      if Result.Success then
         if Proofs_Successful < Natural'Last then
            Proofs_Successful := Proofs_Successful + 1;
         end if;
      end if;
   end Verify_Account_Proof;

   ---------------------------------------------------------------------------
   --  Verify_Storage_Slot
   ---------------------------------------------------------------------------

   procedure Verify_Storage_Slot (
      Account      : in  Account_State_Data;
      Slot_Key     : in  Hash256;
      Slot_Value   : in  Hash256;
      Storage_Proof: in  Merkle_Proof;
      Valid        : out Boolean;
      Error        : out Sphinx_Lite_Error
   ) is
      pragma Unreferenced (Slot_Value);
      Key : constant Hash256 := Compute_Storage_Key (Slot_Key);
   begin
      Valid := False;
      Error := Error_None;

      --  Verify against storage root
      Verify_Merkle_Branch (
         Leaf  => Storage_Proof.Leaf_Hash,
         Root  => Account.Storage_Root,
         Proof => Storage_Proof,
         Key   => Key,
         Valid => Valid
      );

      if not Valid then
         Error := Error_Invalid_Proof;
      end if;
   end Verify_Storage_Slot;

   ---------------------------------------------------------------------------
   --  Verify_Storage_Slots
   ---------------------------------------------------------------------------

   procedure Verify_Storage_Slots (
      Account      : in  Account_State_Data;
      Slots        : in  Storage_Slots;
      Storage_Root : in  Hash256;
      Proof        : in  Merkle_Proof;
      Valid_Count  : out Natural;
      Error        : out Sphinx_Lite_Error
   ) is
      pragma Unreferenced (Storage_Root, Proof);
      Count : Natural := 0;
   begin
      Valid_Count := 0;
      Error := Error_None;

      --  Count valid slots
      for I in Slots'Range loop
         if Slots (I).Valid then
            --  In production, verify each slot with its own proof
            if Count < Max_Storage_Slots then
               Count := Count + 1;
            end if;
         end if;

         pragma Loop_Invariant (Count <= I + 1);
         pragma Loop_Invariant (Count <= Max_Storage_Slots);
      end loop;

      --  Verify storage root matches account
      if Account.Storage_Root = (Hash256'(others => 0)) then
         if Count > 0 then
            Error := Error_Invalid_Proof;
            return;
         end if;
      end if;

      Valid_Count := Count;
   end Verify_Storage_Slots;

   ---------------------------------------------------------------------------
   --  Update_Trusted_Root
   ---------------------------------------------------------------------------

   procedure Update_Trusted_Root (
      New_Root     : in  Hash256;
      Block_Number : in  Unsigned_64;
      Success      : out Boolean;
      Error        : out Sphinx_Lite_Error
   ) is
   begin
      Success := False;
      Error := Error_None;

      --  Block number must be greater than current
      if Block_Number <= Trusted_Block_State then
         Error := Error_Invalid_Block_Number;
         return;
      end if;

      --  Update trusted state
      Trusted_Root_State := New_Root;
      Trusted_Block_State := Block_Number;
      Success := True;
   end Update_Trusted_Root;

   ---------------------------------------------------------------------------
   --  Get_Trusted_Root
   ---------------------------------------------------------------------------

   function Get_Trusted_Root return Hash256 is
   begin
      return Trusted_Root_State;
   end Get_Trusted_Root;

   ---------------------------------------------------------------------------
   --  Get_Trusted_Block
   ---------------------------------------------------------------------------

   function Get_Trusted_Block return Unsigned_64 is
   begin
      return Trusted_Block_State;
   end Get_Trusted_Block;

   ---------------------------------------------------------------------------
   --  Create_Account_Proof
   ---------------------------------------------------------------------------

   procedure Create_Account_Proof (
      Address      : in  Contract_Address;
      State_Root   : in  Hash256;
      Block_Number : in  Unsigned_64;
      Proof        : out Account_Proof;
      Success      : out Boolean;
      Error        : out Sphinx_Lite_Error
   ) is
   begin
      Proof := Null_Account_Proof;
      Success := False;
      Error := Error_None;

      --  Build proof structure
      Proof.Account.Address := Address;
      Proof.State_Root := State_Root;
      Proof.Block_Number := Block_Number;

      --  In production, would construct Merkle proof from MPT
      --  For now, create a placeholder
      Proof.Proof.Leaf_Hash := Compute_Account_Key (Address);
      Proof.Proof.Root := State_Root;
      Proof.Proof.Depth := 1;

      Proof.Valid := True;
      Success := True;
   end Create_Account_Proof;

   ---------------------------------------------------------------------------
   --  Compute_Account_Key
   ---------------------------------------------------------------------------

   function Compute_Account_Key (Address : Contract_Address) return Hash256 is
   begin
      return Simple_Hash (Address);
   end Compute_Account_Key;

   ---------------------------------------------------------------------------
   --  Compute_Storage_Key
   ---------------------------------------------------------------------------

   function Compute_Storage_Key (Slot_Key : Hash256) return Hash256 is
   begin
      return Simple_Hash_256 (Slot_Key);
   end Compute_Storage_Key;

   ---------------------------------------------------------------------------
   --  Verify_Merkle_Branch
   ---------------------------------------------------------------------------

   procedure Verify_Merkle_Branch (
      Leaf       : in  Hash256;
      Root       : in  Hash256;
      Proof      : in  Merkle_Proof;
      Key        : in  Hash256;
      Valid      : out Boolean
   ) is
      Current : Hash256 := Leaf;
      Bit_Pos : Natural;
   begin
      Valid := False;

      --  Walk up the Merkle tree
      for I in 0 .. Proof.Depth - 1 loop
         exit when I >= Max_Proof_Depth;

         Bit_Pos := I mod 8;

         --  Determine if we"re left or right child based on key bit
         if (Key (I / 8) and Shift_Left (Unsigned_8'(1), Bit_Pos)) = 0 then
            --  We"re left child, sibling is right
            Current := Combine_Hashes (Current, Proof.Path (I).Hash);
         else
            --  We"re right child, sibling is left
            Current := Combine_Hashes (Proof.Path (I).Hash, Current);
         end if;
      end loop;

      --  Verify computed root matches expected
      Valid := Current = Root;
   end Verify_Merkle_Branch;

   ---------------------------------------------------------------------------
   --  Serialize_Account_Proof
   ---------------------------------------------------------------------------

   procedure Serialize_Account_Proof (
      Proof : in  Account_Proof;
      Bytes : out Serialized_Account_Proof;
      Size  : out Natural
   ) is
      Idx : Natural := 0;
   begin
      Bytes := (others => 0);
      Size := 0;

      --  State root (32 bytes)
      for I in Proof.State_Root'Range loop
         exit when Idx >= Max_Serialized_Proof_Size;
         Bytes (Idx) := Proof.State_Root (I);
         Idx := Idx + 1;
      end loop;

      --  Block number (8 bytes, little-endian)
      for I in 0 .. 7 loop
         exit when Idx >= Max_Serialized_Proof_Size;
         Bytes (Idx) := Unsigned_8 (
            Shift_Right (Proof.Block_Number, I * 8) and 16#FF#);
         Idx := Idx + 1;
      end loop;

      --  Address (32 bytes)
      for I in Proof.Account.Address'Range loop
         exit when Idx >= Max_Serialized_Proof_Size;
         Bytes (Idx) := Proof.Account.Address (I);
         Idx := Idx + 1;
      end loop;

      --  Proof depth (1 byte)
      if Idx < Max_Serialized_Proof_Size then
         Bytes (Idx) := Unsigned_8 (Proof.Proof.Depth mod 256);
         Idx := Idx + 1;
      end if;

      --  Valid flag (1 byte)
      if Idx < Max_Serialized_Proof_Size then
         Bytes (Idx) := (if Proof.Valid then 1 else 0);
         Idx := Idx + 1;
      end if;

      Size := Idx;
   end Serialize_Account_Proof;

   ---------------------------------------------------------------------------
   --  Deserialize_Account_Proof
   ---------------------------------------------------------------------------

   procedure Deserialize_Account_Proof (
      Bytes   : in  Serialized_Account_Proof;
      Size    : in  Natural;
      Proof   : out Account_Proof;
      Success : out Boolean
   ) is
      Idx : Natural := 0;
      Block_Num : Unsigned_64 := 0;
   begin
      Proof := Null_Account_Proof;
      Success := False;

      if Size < 74 then  -- Minimum size
         return;
      end if;

      --  State root
      for I in Proof.State_Root'Range loop
         exit when Idx >= Size;
         Proof.State_Root (I) := Bytes (Idx);
         Idx := Idx + 1;
      end loop;

      --  Block number
      for I in 0 .. 7 loop
         exit when Idx >= Size;
         Block_Num := Block_Num or
            Shift_Left (Unsigned_64 (Bytes (Idx)), I * 8);
         Idx := Idx + 1;
      end loop;
      Proof.Block_Number := Block_Num;

      --  Address
      for I in Proof.Account.Address'Range loop
         exit when Idx >= Size;
         Proof.Account.Address (I) := Bytes (Idx);
         Idx := Idx + 1;
      end loop;

      --  Proof depth
      if Idx < Size then
         Proof.Proof.Depth := Natural (Bytes (Idx) mod 33);  -- Cap at 32
         Idx := Idx + 1;
      end if;

      --  Valid flag
      if Idx < Size then
         Proof.Valid := Bytes (Idx) /= 0;
      end if;

      Success := True;
   end Deserialize_Account_Proof;

   ---------------------------------------------------------------------------
   --  Total_Proofs_Verified
   ---------------------------------------------------------------------------

   function Total_Proofs_Verified return Natural is
   begin
      return Proofs_Verified;
   end Total_Proofs_Verified;

   ---------------------------------------------------------------------------
   --  Successful_Verifications
   ---------------------------------------------------------------------------

   function Successful_Verifications return Natural is
   begin
      return Proofs_Successful;
   end Successful_Verifications;

   ---------------------------------------------------------------------------
   --  Average_Proof_Size
   ---------------------------------------------------------------------------

   function Average_Proof_Size return Natural is
   begin
      if Proofs_Verified = 0 then
         return 0;
      end if;

      --  Return average, capped at max
      if Total_Proof_Bytes / Proofs_Verified > Max_Account_Proof_Size then
         return Max_Account_Proof_Size;
      else
         return Total_Proof_Bytes / Proofs_Verified;
      end if;
   end Average_Proof_Size;

end Sphinx_Lite_Account;
