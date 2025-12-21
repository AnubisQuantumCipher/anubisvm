--  SPHINX RV32 Syscall Implementation: Real Crypto Operations

pragma SPARK_Mode (Off);

with Sphinx_RV32_Context; use Sphinx_RV32_Context;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;
with Anubis_Types;

package body Sphinx_RV32_Syscall_Impl is

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   procedure Storage_Load (
      Context   : in     System.Address;
      Slot_Data : in     Byte_Array;
      Value     : out    Byte_Array;
      Success   : out    Boolean
   ) is
      Ctx : Interpreter_Context_Ptr;
      Key : Storage_Key_Bytes;
      Val : Storage_Value_Bytes;
   begin
      Value := (others => 0);

      if not Is_Valid_Context (Context) then
         Success := False;
         return;
      end if;

      Ctx := To_Context_Ptr (Context);

      --  Copy slot data to key
      for I in Key'Range loop
         Key (I) := Slot_Data (Slot_Data'First + I);
      end loop;

      --  Call storage via context
      Context_SLoad (Ctx, Key, Val, Success);

      --  Copy result to output
      if Success then
         for I in Value'Range loop
            Value (I) := Val (I - Value'First);
         end loop;
      end if;
   end Storage_Load;

   procedure Storage_Store (
      Context    : in     System.Address;
      Slot_Data  : in     Byte_Array;
      Value_Data : in     Byte_Array;
      Success    : out    Boolean
   ) is
      Ctx : Interpreter_Context_Ptr;
      Key : Storage_Key_Bytes;
      Val : Storage_Value_Bytes;
   begin
      if not Is_Valid_Context (Context) then
         Success := False;
         return;
      end if;

      Ctx := To_Context_Ptr (Context);

      --  Copy data to key/value
      for I in Key'Range loop
         Key (I) := Slot_Data (Slot_Data'First + I);
      end loop;

      for I in Val'Range loop
         Val (I) := Value_Data (Value_Data'First + I);
      end loop;

      --  Call storage via context
      Context_SStore (Ctx, Key, Val, Success);
   end Storage_Store;

   ---------------------------------------------------------------------------
   --  Crypto: SHA3-256
   ---------------------------------------------------------------------------

   procedure Compute_SHA3_256 (
      Input   : in     Byte_Array;
      Output  : out    Byte_Array;
      Success : out    Boolean
   ) is
      --  Convert to Anubis types
      Input_Copy : Anubis_Types.Byte_Array (0 .. Input'Length - 1);
      Digest     : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Copy input
      for I in Input'Range loop
         Input_Copy (I - Input'First) := Anubis_Types.Byte (Input (I));
      end loop;

      --  Compute hash
      Anubis_SHA3.SHA3_256 (Input_Copy, Digest);

      --  Copy output
      for I in Output'Range loop
         Output (I) := Aegis_VM_Types.Byte (Digest (I - Output'First));
      end loop;

      Success := True;
   exception
      when others =>
         Output := (others => 0);
         Success := False;
   end Compute_SHA3_256;

   procedure Compute_Keccak_256 (
      Input   : in     Byte_Array;
      Output  : out    Byte_Array;
      Success : out    Boolean
   ) is
      --  Convert to Anubis types
      Input_Copy : Anubis_Types.Byte_Array (0 .. Input'Length - 1);
      Digest     : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Copy input
      for I in Input'Range loop
         Input_Copy (I - Input'First) := Anubis_Types.Byte (Input (I));
      end loop;

      --  Compute hash
      Anubis_SHA3.Keccak_256 (Input_Copy, Digest);

      --  Copy output
      for I in Output'Range loop
         Output (I) := Aegis_VM_Types.Byte (Digest (I - Output'First));
      end loop;

      Success := True;
   exception
      when others =>
         Output := (others => 0);
         Success := False;
   end Compute_Keccak_256;

   ---------------------------------------------------------------------------
   --  Crypto: ML-DSA-87
   ---------------------------------------------------------------------------

   procedure Verify_MLDSA_Signature (
      Public_Key : in     Byte_Array;
      Message    : in     Byte_Array;
      Signature  : in     Byte_Array;
      Valid      : out    Boolean;
      Success    : out    Boolean
   ) is
      use Anubis_MLDSA_Types;
      use Anubis_MLDSA;

      PK  : Anubis_MLDSA_Types.Public_Key;
      Msg : Anubis_Types.Byte_Array (0 .. Message'Length - 1);
      Sig : Anubis_MLDSA_Types.Signature;
   begin
      --  Copy public key
      for I in PK'Range loop
         PK (I) := Anubis_Types.Byte (Public_Key (Public_Key'First + I));
      end loop;

      --  Copy message
      for I in Msg'Range loop
         Msg (I) := Anubis_Types.Byte (Message (Message'First + I));
      end loop;

      --  Copy signature
      for I in Sig'Range loop
         Sig (I) := Anubis_Types.Byte (Signature (Signature'First + I));
      end loop;

      --  Verify
      Valid := Anubis_MLDSA.Verify (PK, Msg, Sig);
      Success := True;
   exception
      when others =>
         Valid := False;
         Success := False;
   end Verify_MLDSA_Signature;

   ---------------------------------------------------------------------------
   --  Crypto: ML-KEM-1024
   ---------------------------------------------------------------------------

   procedure Decapsulate_MLKEM (
      Secret_Key    : in     Byte_Array;
      Ciphertext    : in     Byte_Array;
      Shared_Secret : out    Byte_Array;
      Success       : out    Boolean
   ) is
      use Anubis_MLKEM_Types;
      use Anubis_MLKEM;

      DK : Anubis_MLKEM_Types.Decapsulation_Key;
      CT : Anubis_MLKEM_Types.MLKEM_Ciphertext;
      SS : Anubis_MLKEM_Types.Shared_Secret;
   begin
      --  Copy decapsulation key
      for I in DK'Range loop
         DK (I) := Anubis_Types.Byte (Secret_Key (Secret_Key'First + I));
      end loop;

      --  Copy ciphertext
      for I in CT'Range loop
         CT (I) := Anubis_Types.Byte (Ciphertext (Ciphertext'First + I));
      end loop;

      --  Decapsulate
      Anubis_MLKEM.Decaps (DK, CT, SS);

      --  Copy shared secret
      for I in Shared_Secret'Range loop
         Shared_Secret (I) := Aegis_VM_Types.Byte (SS (I - Shared_Secret'First));
      end loop;

      Success := True;
   exception
      when others =>
         Shared_Secret := (others => 0);
         Success := False;
   end Decapsulate_MLKEM;

   ---------------------------------------------------------------------------
   --  Environment Access
   ---------------------------------------------------------------------------

   procedure Get_Caller (
      Context : in     System.Address;
      Address : out    Byte_Array;
      Success : out    Boolean
   ) is
      Ctx : Interpreter_Context_Ptr;
   begin
      Address := (others => 0);

      if not Is_Valid_Context (Context) then
         Success := False;
         return;
      end if;

      Ctx := To_Context_Ptr (Context);
      Get_Caller_Bytes (Ctx, Address);
      Success := True;
   exception
      when others =>
         Address := (others => 0);
         Success := False;
   end Get_Caller;

   procedure Get_Self_Address (
      Context : in     System.Address;
      Address : out    Byte_Array;
      Success : out    Boolean
   ) is
      Ctx : Interpreter_Context_Ptr;
   begin
      Address := (others => 0);

      if not Is_Valid_Context (Context) then
         Success := False;
         return;
      end if;

      Ctx := To_Context_Ptr (Context);
      Get_Address_Bytes (Ctx, Address);
      Success := True;
   exception
      when others =>
         Address := (others => 0);
         Success := False;
   end Get_Self_Address;

   procedure Get_Calldata (
      Context  : in     System.Address;
      Offset   : in     Natural;
      Length   : in     Natural;
      Buffer   : out    Byte_Array;
      Actual   : out    Natural;
      Success  : out    Boolean
   ) is
      Ctx : Interpreter_Context_Ptr;
   begin
      Buffer := (others => 0);
      Actual := 0;

      if not Is_Valid_Context (Context) then
         Success := False;
         return;
      end if;

      Ctx := To_Context_Ptr (Context);
      Sphinx_RV32_Context.Get_Calldata (Ctx, Offset, Length, Buffer, Success);
      if Success then
         Actual := Length;
      end if;
   exception
      when others =>
         Buffer := (others => 0);
         Actual := 0;
         Success := False;
   end Get_Calldata;

   procedure Get_Calldata_Size (
      Context : in     System.Address;
      Size    : out    Natural;
      Success : out    Boolean
   ) is
      Ctx : Interpreter_Context_Ptr;
   begin
      Size := 0;

      if not Is_Valid_Context (Context) then
         Success := False;
         return;
      end if;

      Ctx := To_Context_Ptr (Context);
      Size := Sphinx_RV32_Context.Get_Calldata_Size (Ctx);
      Success := True;
   exception
      when others =>
         Size := 0;
         Success := False;
   end Get_Calldata_Size;

   procedure Get_Block_Number (
      Context : in     System.Address;
      Value   : out    Word;
      Success : out    Boolean
   ) is
      Ctx : Interpreter_Context_Ptr;
   begin
      Value := 0;

      if not Is_Valid_Context (Context) then
         Success := False;
         return;
      end if;

      Ctx := To_Context_Ptr (Context);
      Value := Get_Block_Number_32 (Ctx);
      Success := True;
   exception
      when others =>
         Value := 0;
         Success := False;
   end Get_Block_Number;

   procedure Get_Timestamp (
      Context : in     System.Address;
      Value   : out    Word;
      Success : out    Boolean
   ) is
      Ctx : Interpreter_Context_Ptr;
   begin
      Value := 0;

      if not Is_Valid_Context (Context) then
         Success := False;
         return;
      end if;

      Ctx := To_Context_Ptr (Context);
      Value := Get_Timestamp_32 (Ctx);
      Success := True;
   exception
      when others =>
         Value := 0;
         Success := False;
   end Get_Timestamp;

end Sphinx_RV32_Syscall_Impl;
