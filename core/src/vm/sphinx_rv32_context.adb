--  SPHINX RV32 Context: Implementation

pragma SPARK_Mode (Off);

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Deallocation;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLKEM;
with Anubis_MLDSA_Types;
with Anubis_MLKEM_Types;
with Anubis_Types;

package body Sphinx_RV32_Context is

   ---------------------------------------------------------------------------
   --  Address Conversion
   ---------------------------------------------------------------------------

   package Context_Conversions is new System.Address_To_Access_Conversions
     (Object => Interpreter_Context);

   function To_Context_Ptr (Addr : System.Address) return Interpreter_Context_Ptr is
   begin
      if Addr = System.Null_Address then
         return null;
      end if;
      return Interpreter_Context_Ptr (Context_Conversions.To_Pointer (Addr));
   end To_Context_Ptr;

   function Is_Valid_Context (Addr : System.Address) return Boolean is
      Ptr : Interpreter_Context_Ptr;
   begin
      if Addr = System.Null_Address then
         return False;
      end if;
      Ptr := To_Context_Ptr (Addr);
      return Ptr /= null and then Ptr.Is_Valid;
   end Is_Valid_Context;

   ---------------------------------------------------------------------------
   --  Context-Aware Storage Operations
   ---------------------------------------------------------------------------

   procedure Context_SLoad (
      Ctx     : in     Interpreter_Context_Ptr;
      Key     : in     Storage_Key_Bytes;
      Value   : out    Storage_Value_Bytes;
      Success : out    Boolean
   ) is
   begin
      --  Initialize output
      Value := (others => 0);

      if Ctx = null or else not Ctx.Is_Valid then
         Success := False;
         return;
      end if;

      --  Check if storage callback is set
      if Ctx.Storage_Load_Callback = System.Null_Address then
         --  No storage available - return zeros
         Success := True;
         return;
      end if;

      --  Call the storage callback
      declare
         Callback : Storage_Load_Proc;
         for Callback'Address use Ctx.Storage_Load_Callback;
      begin
         Callback (Key, Value, Success);
      end;
   end Context_SLoad;

   procedure Context_SStore (
      Ctx     : in     Interpreter_Context_Ptr;
      Key     : in     Storage_Key_Bytes;
      Value   : in     Storage_Value_Bytes;
      Success : out    Boolean
   ) is
   begin
      if Ctx = null or else not Ctx.Is_Valid then
         Success := False;
         return;
      end if;

      --  Check if static mode (no writes allowed)
      if Ctx.Is_Static then
         Success := False;
         return;
      end if;

      --  Check if storage callback is set
      if Ctx.Storage_Store_Callback = System.Null_Address then
         --  No storage available - silently succeed (for testing)
         Success := True;
         return;
      end if;

      --  Call the storage callback
      declare
         Callback : Storage_Store_Proc;
         for Callback'Address use Ctx.Storage_Store_Callback;
      begin
         Callback (Key, Value, Success);
      end;
   end Context_SStore;

   ---------------------------------------------------------------------------
   --  Crypto Operations
   ---------------------------------------------------------------------------

   procedure Context_SHA3 (
      Input   : in     Byte_Array;
      Output  : out    Byte_Array;
      Success : out    Boolean
   ) is
      --  Convert to Anubis_Types.Byte_Array
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
         Output (I) := Byte (Digest (I - Output'First));
      end loop;

      Success := True;
   exception
      when others =>
         Output := (others => 0);
         Success := False;
   end Context_SHA3;

   procedure Context_Keccak256 (
      Input   : in     Byte_Array;
      Output  : out    Byte_Array;
      Success : out    Boolean
   ) is
      --  Convert to Anubis_Types.Byte_Array
      Input_Copy : Anubis_Types.Byte_Array (0 .. Input'Length - 1);
      Digest     : Anubis_SHA3.SHA3_256_Digest;
   begin
      --  Copy input
      for I in Input'Range loop
         Input_Copy (I - Input'First) := Anubis_Types.Byte (Input (I));
      end loop;

      --  Compute hash (Keccak-256)
      Anubis_SHA3.Keccak_256 (Input_Copy, Digest);

      --  Copy output
      for I in Output'Range loop
         Output (I) := Byte (Digest (I - Output'First));
      end loop;

      Success := True;
   exception
      when others =>
         Output := (others => 0);
         Success := False;
   end Context_Keccak256;

   procedure Context_MLDSA_Verify (
      Public_Key : in     Byte_Array;
      Message    : in     Byte_Array;
      Signature  : in     Byte_Array;
      Valid      : out    Boolean;
      Success    : out    Boolean
   ) is
      use Anubis_MLDSA_Types;
      use Anubis_MLDSA;

      --  Convert to ML-DSA types
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

      --  Verify signature
      Valid := Anubis_MLDSA.Verify (PK, Msg, Sig);
      Success := True;
   exception
      when others =>
         Valid := False;
         Success := False;
   end Context_MLDSA_Verify;

   procedure Context_MLKEM_Decaps (
      Secret_Key    : in     Byte_Array;
      Ciphertext    : in     Byte_Array;
      Shared_Secret : out    Byte_Array;
      Success       : out    Boolean
   ) is
      use Anubis_MLKEM_Types;
      use Anubis_MLKEM;

      --  Convert to ML-KEM types
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
         Shared_Secret (I) := Byte (SS (I - Shared_Secret'First));
      end loop;

      Success := True;
   exception
      when others =>
         Shared_Secret := (others => 0);
         Success := False;
   end Context_MLKEM_Decaps;

   ---------------------------------------------------------------------------
   --  Environment Access
   ---------------------------------------------------------------------------

   procedure Get_Caller_Bytes (
      Ctx   : in     Interpreter_Context_Ptr;
      Bytes : out    Byte_Array
   ) is
   begin
      if Ctx = null or else not Ctx.Is_Valid then
         Bytes := (others => 0);
         return;
      end if;

      for I in Bytes'Range loop
         Bytes (I) := Ctx.Caller (I - Bytes'First);
      end loop;
   end Get_Caller_Bytes;

   procedure Get_Address_Bytes (
      Ctx   : in     Interpreter_Context_Ptr;
      Bytes : out    Byte_Array
   ) is
   begin
      if Ctx = null or else not Ctx.Is_Valid then
         Bytes := (others => 0);
         return;
      end if;

      for I in Bytes'Range loop
         Bytes (I) := Ctx.Self (I - Bytes'First);
      end loop;
   end Get_Address_Bytes;

   procedure Get_Calldata (
      Ctx     : in     Interpreter_Context_Ptr;
      Offset  : in     Natural;
      Length  : in     Natural;
      Buffer  : out    Byte_Array;
      Success : out    Boolean
   ) is
   begin
      Buffer := (others => 0);

      if Ctx = null or else not Ctx.Is_Valid then
         Success := False;
         return;
      end if;

      --  Check bounds
      if Offset >= Ctx.Calldata_Length then
         --  Offset beyond calldata - return zeros
         Success := True;
         return;
      end if;

      --  Copy available bytes
      declare
         Available : constant Natural := Ctx.Calldata_Length - Offset;
         To_Copy   : constant Natural := Natural'Min (Length, Available);
      begin
         for I in 0 .. To_Copy - 1 loop
            Buffer (Buffer'First + I) := Ctx.Calldata (Offset + I);
         end loop;
      end;

      Success := True;
   end Get_Calldata;

   function Get_Calldata_Size (
      Ctx : Interpreter_Context_Ptr
   ) return Natural is
   begin
      if Ctx = null or else not Ctx.Is_Valid then
         return 0;
      end if;
      return Ctx.Calldata_Length;
   end Get_Calldata_Size;

   function Get_Block_Number_32 (
      Ctx : Interpreter_Context_Ptr
   ) return Word is
   begin
      if Ctx = null or else not Ctx.Is_Valid then
         return 0;
      end if;
      --  Return lower 32 bits of block number
      return Word (Ctx.Block_Number.Limbs (0) and 16#FFFF_FFFF#);
   end Get_Block_Number_32;

   function Get_Timestamp_32 (
      Ctx : Interpreter_Context_Ptr
   ) return Word is
   begin
      if Ctx = null or else not Ctx.Is_Valid then
         return 0;
      end if;
      --  Return lower 32 bits of timestamp
      return Word (Ctx.Timestamp.Limbs (0) and 16#FFFF_FFFF#);
   end Get_Timestamp_32;

   ---------------------------------------------------------------------------
   --  Context Creation
   ---------------------------------------------------------------------------

   function Create_Context return Interpreter_Context_Ptr is
      Ctx : Interpreter_Context_Ptr;
   begin
      Ctx := new Interpreter_Context'(
         Caller      => Address_Zero,
         Self        => Address_Zero,
         Origin      => Address_Zero,
         Call_Value  => U256_Zero,
         Block_Number => U256_Zero,
         Timestamp   => U256_Zero,
         Chain_ID    => U256_Zero,
         Gas_Price   => U256_Zero,
         Calldata    => (others => 0),
         Calldata_Length => 0,
         Return_Data => (others => 0),
         Return_Data_Length => 0,
         Storage_Load_Callback  => System.Null_Address,
         Storage_Store_Callback => System.Null_Address,
         Is_Static   => False,
         Is_Valid    => False
      );
      return Ctx;
   end Create_Context;

   procedure Initialize_Context (
      Ctx          : in out Interpreter_Context_Ptr;
      Caller       : in     Contract_Address;
      Self         : in     Contract_Address;
      Origin       : in     Contract_Address;
      Call_Value   : in     U256;
      Block_Number : in     U256;
      Timestamp    : in     U256;
      Chain_ID     : in     U256;
      Gas_Price    : in     U256;
      Is_Static    : in     Boolean
   ) is
   begin
      if Ctx = null then
         return;
      end if;

      Ctx.Caller := Caller;
      Ctx.Self := Self;
      Ctx.Origin := Origin;
      Ctx.Call_Value := Call_Value;
      Ctx.Block_Number := Block_Number;
      Ctx.Timestamp := Timestamp;
      Ctx.Chain_ID := Chain_ID;
      Ctx.Gas_Price := Gas_Price;
      Ctx.Is_Static := Is_Static;
      Ctx.Is_Valid := True;
   end Initialize_Context;

   procedure Set_Calldata (
      Ctx  : in Out Interpreter_Context_Ptr;
      Data : in     Byte_Array
   ) is
   begin
      if Ctx = null then
         return;
      end if;

      Ctx.Calldata_Length := Data'Length;
      for I in Data'Range loop
         Ctx.Calldata (I - Data'First) := Data (I);
      end loop;
   end Set_Calldata;

   procedure Free_Context_Impl is new Ada.Unchecked_Deallocation
     (Object => Interpreter_Context, Name => Interpreter_Context_Ptr);

   procedure Free_Context (Ctx : in Out Interpreter_Context_Ptr) is
   begin
      if Ctx /= null then
         Free_Context_Impl (Ctx);
      end if;
   end Free_Context;

end Sphinx_RV32_Context;
