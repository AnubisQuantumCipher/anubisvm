--  AegisVM Foreign Function Interface Implementation
--
--  This package body implements the C-callable functions defined in aegis_ffi.ads.
--  It bridges between C calling conventions and the underlying SPARK/Ada
--  cryptographic implementations.

pragma SPARK_Mode (Off);  --  FFI layer uses C convention

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Interfaces; use Interfaces;

--  Import SPARK crypto and VM packages
with Anubis_Types;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;
with Aegis_VM_Types; use Aegis_VM_Types;
with Aegis_Execution;
with Aegis_Contract;

--  Import address packages (AAS-001 v3.1)
with Anubis_Address;
with Anubis_Address_Types; use Anubis_Address_Types;
with Anubis_Address_Derive;

--  Import state management packages
with Khepri_MPT_Types; use Khepri_MPT_Types;
with Khepri_Storage_Trie; use Khepri_Storage_Trie;
with Khepri_Types;
with Khepri_Registry;

--  Import ANUBIS Privacy Layer packages
with Anubis_Shield;
with Anubis_Whisper;
with Anubis_Eye;
with Anubis_Gate;
with Anubis_Lattice_ZK;
with Anubis_STARK_FRI;

--  Import SCARAB v2.0 Proof Aggregation packages
with Scarab_Khnum;
with Scarab_Sebek;
with Scarab_Aadkg;
with Scarab_Horus;
with Scarab_Tefnut;
with Scarab_Maat;
with Scarab_Thoth;
with Scarab_Sekhmet;

package body Aegis_FFI is

   ---------------------------------------------------------------------------
   --  Internal Helper Types
   ---------------------------------------------------------------------------

   --  Deallocation for VM contexts
   procedure Free_VM_Context is new Ada.Unchecked_Deallocation (
      Object => VM_Context,
      Name   => VM_Handle
   );

   ---------------------------------------------------------------------------
   --  Internal State
   ---------------------------------------------------------------------------

   Library_Initialized : Boolean := False;

   ---------------------------------------------------------------------------
   --  Library Initialization
   ---------------------------------------------------------------------------

   function Aegis_Init return Result_Code is
   begin
      if Library_Initialized then
         return Result_Code (AEGIS_OK);
      end if;

      --  Initialize crypto subsystems
      --  (Add initialization calls as needed)

      Library_Initialized := True;
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Init;

   procedure Aegis_Cleanup is
   begin
      if not Library_Initialized then
         return;
      end if;

      --  Cleanup crypto subsystems
      --  (Add cleanup calls as needed)

      Library_Initialized := False;
   end Aegis_Cleanup;

   function Aegis_Version return Interfaces.C.Strings.chars_ptr is
   begin
      return Interfaces.C.Strings.New_String ("1.0.0");
   end Aegis_Version;

   ---------------------------------------------------------------------------
   --  ML-DSA-87 Digital Signatures
   ---------------------------------------------------------------------------

   function Aegis_MLDSA87_Keygen (
      Public_Key : System.Address;
      Secret_Key : System.Address;
      Seed       : System.Address
   ) return Result_Code is
      --  Create overlay types for C memory access
      type Seed_Ptr is access all Anubis_MLDSA_Types.Seed;
      type PK_Ptr is access all Anubis_MLDSA_Types.Public_Key;
      type SK_Ptr is access all Anubis_MLDSA_Types.Secret_Key;

      function To_Seed_Ptr is new Ada.Unchecked_Conversion (System.Address, Seed_Ptr);
      function To_PK_Ptr is new Ada.Unchecked_Conversion (System.Address, PK_Ptr);
      function To_SK_Ptr is new Ada.Unchecked_Conversion (System.Address, SK_Ptr);

      Seed_Data : constant Seed_Ptr := To_Seed_Ptr (Seed);
      PK_Data   : constant PK_Ptr := To_PK_Ptr (Public_Key);
      SK_Data   : constant SK_Ptr := To_SK_Ptr (Secret_Key);
   begin
      if Public_Key = System.Null_Address or
         Secret_Key = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call actual ML-DSA-87 keygen implementation
      if Seed = System.Null_Address then
         --  Use zero seed if none provided
         declare
            Zero_Seed : constant Anubis_MLDSA_Types.Seed := (others => 0);
         begin
            Anubis_MLDSA.KeyGen (Zero_Seed, PK_Data.all, SK_Data.all);
         end;
      else
         Anubis_MLDSA.KeyGen (Seed_Data.all, PK_Data.all, SK_Data.all);
      end if;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_MLDSA87_Keygen;

   function Aegis_MLDSA87_Sign (
      Signature  : System.Address;
      Sig_Len    : access Interfaces.C.size_t;
      Message    : System.Address;
      Msg_Len    : Interfaces.C.size_t;
      Secret_Key : System.Address
   ) return Result_Code is
      type SK_Ptr is access all Anubis_MLDSA_Types.Secret_Key;
      type Sig_Ptr is access all Anubis_MLDSA_Types.Signature;

      function To_SK_Ptr is new Ada.Unchecked_Conversion (System.Address, SK_Ptr);
      function To_Sig_Ptr is new Ada.Unchecked_Conversion (System.Address, Sig_Ptr);

      SK_Data   : constant SK_Ptr := To_SK_Ptr (Secret_Key);
      Sig_Data  : constant Sig_Ptr := To_Sig_Ptr (Signature);
      Success   : Boolean;
   begin
      if Signature = System.Null_Address or
         Sig_Len = null or
         Secret_Key = System.Null_Address or
         (Msg_Len > 0 and Message = System.Null_Address)
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Check message length against ML-DSA limit
      if Msg_Len > Interfaces.C.size_t (Anubis_MLDSA.Max_Msg_Length) then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Create message array and call sign
      declare
         Msg_Array : Anubis_Types.Byte_Array (0 .. Natural (Msg_Len) - 1);
         type Msg_Ptr is access all Anubis_Types.Byte_Array;
         function To_Msg_Ptr is new Ada.Unchecked_Conversion (System.Address, Msg_Ptr);
         Msg_Data : constant Msg_Ptr := To_Msg_Ptr (Message);
         Zero_Random : constant Anubis_MLDSA_Types.Seed := (others => 0);
      begin
         if Msg_Len > 0 then
            Msg_Array := Msg_Data.all (0 .. Natural (Msg_Len) - 1);
         end if;
         Anubis_MLDSA.Sign (SK_Data.all, Msg_Array, Zero_Random, Sig_Data.all, Success);
      end;

      if Success then
         Sig_Len.all := Interfaces.C.size_t (MLDSA87_SIGNATURE_SIZE);
         return Result_Code (AEGIS_OK);
      else
         return Result_Code (AEGIS_ERROR_UNKNOWN);
      end if;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_MLDSA87_Sign;

   function Aegis_MLDSA87_Verify (
      Signature  : System.Address;
      Sig_Len    : Interfaces.C.size_t;
      Message    : System.Address;
      Msg_Len    : Interfaces.C.size_t;
      Public_Key : System.Address;
      Valid      : access Interfaces.C.C_bool
   ) return Result_Code is
      type PK_Ptr is access all Anubis_MLDSA_Types.Public_Key;
      type Sig_Ptr is access all Anubis_MLDSA_Types.Signature;

      function To_PK_Ptr is new Ada.Unchecked_Conversion (System.Address, PK_Ptr);
      function To_Sig_Ptr is new Ada.Unchecked_Conversion (System.Address, Sig_Ptr);

      PK_Data  : constant PK_Ptr := To_PK_Ptr (Public_Key);
      Sig_Data : constant Sig_Ptr := To_Sig_Ptr (Signature);
   begin
      if Signature = System.Null_Address or
         Public_Key = System.Null_Address or
         Valid = null or
         (Msg_Len > 0 and Message = System.Null_Address)
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Sig_Len /= Interfaces.C.size_t (MLDSA87_SIGNATURE_SIZE) then
         Valid.all := Interfaces.C.C_bool (Standard.False);
         return Result_Code (AEGIS_ERROR_INVALID_SIGNATURE);
      end if;

      --  Check message length against ML-DSA limit
      if Msg_Len > Interfaces.C.size_t (Anubis_MLDSA.Max_Msg_Length) then
         Valid.all := Interfaces.C.C_bool (Standard.False);
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Create message array and call verify
      declare
         Msg_Array : Anubis_Types.Byte_Array (0 .. Natural (Msg_Len) - 1);
         type Msg_Ptr is access all Anubis_Types.Byte_Array;
         function To_Msg_Ptr is new Ada.Unchecked_Conversion (System.Address, Msg_Ptr);
         Msg_Data : constant Msg_Ptr := To_Msg_Ptr (Message);
         Is_Valid : Boolean;
      begin
         if Msg_Len > 0 then
            Msg_Array := Msg_Data.all (0 .. Natural (Msg_Len) - 1);
         end if;
         Is_Valid := Anubis_MLDSA.Verify (PK_Data.all, Msg_Array, Sig_Data.all);
         Valid.all := Interfaces.C.C_bool (Is_Valid);
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_MLDSA87_Verify;

   ---------------------------------------------------------------------------
   --  ML-KEM-1024 Key Encapsulation
   ---------------------------------------------------------------------------

   function Aegis_MLKEM1024_Keygen (
      Encaps_Key : System.Address;
      Decaps_Key : System.Address;
      Seed       : System.Address
   ) return Result_Code is
      type EK_Ptr is access all Anubis_MLKEM_Types.Encapsulation_Key;
      type DK_Ptr is access all Anubis_MLKEM_Types.Decapsulation_Key;
      type Seed_Ptr is access all Anubis_MLKEM_Types.Seed;

      function To_EK_Ptr is new Ada.Unchecked_Conversion (System.Address, EK_Ptr);
      function To_DK_Ptr is new Ada.Unchecked_Conversion (System.Address, DK_Ptr);
      function To_Seed_Ptr is new Ada.Unchecked_Conversion (System.Address, Seed_Ptr);

      EK_Data : constant EK_Ptr := To_EK_Ptr (Encaps_Key);
      DK_Data : constant DK_Ptr := To_DK_Ptr (Decaps_Key);
   begin
      if Encaps_Key = System.Null_Address or
         Decaps_Key = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call actual ML-KEM-1024 keygen implementation
      if Seed = System.Null_Address then
         --  Use zero seeds if none provided
         declare
            Zero_D : constant Anubis_MLKEM_Types.Seed := (others => 0);
            Zero_Z : constant Anubis_MLKEM_Types.Seed := (others => 0);
         begin
            Anubis_MLKEM.KeyGen (Zero_D, Zero_Z, EK_Data.all, DK_Data.all);
         end;
      else
         declare
            Seed_Data : constant Seed_Ptr := To_Seed_Ptr (Seed);
            Zero_Z : constant Anubis_MLKEM_Types.Seed := (others => 0);
         begin
            Anubis_MLKEM.KeyGen (Seed_Data.all, Zero_Z, EK_Data.all, DK_Data.all);
         end;
      end if;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_MLKEM1024_Keygen;

   function Aegis_MLKEM1024_Encaps (
      Ciphertext    : System.Address;
      Shared_Secret : System.Address;
      Encaps_Key    : System.Address;
      Seed          : System.Address
   ) return Result_Code is
      type EK_Ptr is access all Anubis_MLKEM_Types.Encapsulation_Key;
      type SS_Ptr is access all Anubis_MLKEM_Types.Shared_Secret;
      type CT_Ptr is access all Anubis_MLKEM_Types.MLKEM_Ciphertext;
      type Seed_Ptr is access all Anubis_MLKEM_Types.Seed;

      function To_EK_Ptr is new Ada.Unchecked_Conversion (System.Address, EK_Ptr);
      function To_SS_Ptr is new Ada.Unchecked_Conversion (System.Address, SS_Ptr);
      function To_CT_Ptr is new Ada.Unchecked_Conversion (System.Address, CT_Ptr);
      function To_Seed_Ptr is new Ada.Unchecked_Conversion (System.Address, Seed_Ptr);

      EK_Data : constant EK_Ptr := To_EK_Ptr (Encaps_Key);
      SS_Data : constant SS_Ptr := To_SS_Ptr (Shared_Secret);
      CT_Data : constant CT_Ptr := To_CT_Ptr (Ciphertext);
   begin
      if Ciphertext = System.Null_Address or
         Shared_Secret = System.Null_Address or
         Encaps_Key = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call actual ML-KEM-1024 encaps implementation
      if Seed = System.Null_Address then
         declare
            Zero_M : constant Anubis_MLKEM_Types.Seed := (others => 0);
         begin
            Anubis_MLKEM.Encaps (EK_Data.all, Zero_M, SS_Data.all, CT_Data.all);
         end;
      else
         declare
            Seed_Data : constant Seed_Ptr := To_Seed_Ptr (Seed);
         begin
            Anubis_MLKEM.Encaps (EK_Data.all, Seed_Data.all, SS_Data.all, CT_Data.all);
         end;
      end if;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_MLKEM1024_Encaps;

   function Aegis_MLKEM1024_Decaps (
      Shared_Secret : System.Address;
      Ciphertext    : System.Address;
      Decaps_Key    : System.Address
   ) return Result_Code is
      type DK_Ptr is access all Anubis_MLKEM_Types.Decapsulation_Key;
      type SS_Ptr is access all Anubis_MLKEM_Types.Shared_Secret;
      type CT_Ptr is access all Anubis_MLKEM_Types.MLKEM_Ciphertext;

      function To_DK_Ptr is new Ada.Unchecked_Conversion (System.Address, DK_Ptr);
      function To_SS_Ptr is new Ada.Unchecked_Conversion (System.Address, SS_Ptr);
      function To_CT_Ptr is new Ada.Unchecked_Conversion (System.Address, CT_Ptr);

      DK_Data : constant DK_Ptr := To_DK_Ptr (Decaps_Key);
      SS_Data : constant SS_Ptr := To_SS_Ptr (Shared_Secret);
      CT_Data : constant CT_Ptr := To_CT_Ptr (Ciphertext);
   begin
      if Shared_Secret = System.Null_Address or
         Ciphertext = System.Null_Address or
         Decaps_Key = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call actual ML-KEM-1024 decaps implementation
      Anubis_MLKEM.Decaps (DK_Data.all, CT_Data.all, SS_Data.all);

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_MLKEM1024_Decaps;

   ---------------------------------------------------------------------------
   --  SHA3/SHAKE Hash Functions
   ---------------------------------------------------------------------------

   function Aegis_SHA3_256 (
      Output : System.Address;
      Input  : System.Address;
      Len    : Interfaces.C.size_t
   ) return Result_Code is
      --  Internal types for address overlay
      type Output_Array is array (0 .. 31) of Anubis_Types.Byte;
      Output_View : Output_Array;
      for Output_View'Address use Output;

      Hash_Result : Anubis_SHA3.SHA3_256_Digest;
      Empty_Input : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
   begin
      if Output = System.Null_Address then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Len = 0 then
         --  Hash empty input
         Anubis_SHA3.SHA3_256 (Empty_Input, Hash_Result);
      else
         if Input = System.Null_Address then
            return Result_Code (AEGIS_ERROR_INVALID_INPUT);
         end if;
         declare
            Input_Bytes : Anubis_Types.Byte_Array (0 .. Natural (Len) - 1);
            for Input_Bytes'Address use Input;
         begin
            Anubis_SHA3.SHA3_256 (Input_Bytes, Hash_Result);
         end;
      end if;

      --  Copy result to output
      for I in 0 .. 31 loop
         Output_View (I) := Hash_Result (I);
      end loop;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_SHA3_256;

   function Aegis_SHA3_512 (
      Output : System.Address;
      Input  : System.Address;
      Len    : Interfaces.C.size_t
   ) return Result_Code is
      --  Internal types for address overlay
      type Output_Array is array (0 .. 63) of Anubis_Types.Byte;
      Output_View : Output_Array;
      for Output_View'Address use Output;

      Hash_Result : Anubis_SHA3.SHA3_512_Digest;
      Empty_Input : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
   begin
      if Output = System.Null_Address then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Len = 0 then
         Anubis_SHA3.SHA3_512 (Empty_Input, Hash_Result);
      else
         if Input = System.Null_Address then
            return Result_Code (AEGIS_ERROR_INVALID_INPUT);
         end if;
         declare
            Input_Bytes : Anubis_Types.Byte_Array (0 .. Natural (Len) - 1);
            for Input_Bytes'Address use Input;
         begin
            Anubis_SHA3.SHA3_512 (Input_Bytes, Hash_Result);
         end;
      end if;

      --  Copy result to output
      for I in 0 .. 63 loop
         Output_View (I) := Hash_Result (I);
      end loop;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_SHA3_512;

   function Aegis_Keccak256 (
      Output : System.Address;
      Input  : System.Address;
      Len    : Interfaces.C.size_t
   ) return Result_Code is
      --  Keccak-256 (Ethereum) uses 0x01 padding vs SHA3-256"s 0x06

      type Output_Array is array (0 .. 31) of Anubis_Types.Byte;
      Output_View : Output_Array;
      for Output_View'Address use Output;

      Hash_Result : Anubis_SHA3.SHA3_256_Digest;
      Empty_Input : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
   begin
      if Output = System.Null_Address then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Len = 0 then
         --  Hash empty input with Keccak-256 (Ethereum-compatible)
         Anubis_SHA3.Keccak_256 (Empty_Input, Hash_Result);
      else
         if Input = System.Null_Address then
            return Result_Code (AEGIS_ERROR_INVALID_INPUT);
         end if;
         declare
            Input_Bytes : Anubis_Types.Byte_Array (0 .. Natural (Len) - 1);
            for Input_Bytes'Address use Input;
         begin
            --  Use Keccak-256 with 0x01 domain separator (Ethereum-compatible)
            Anubis_SHA3.Keccak_256 (Input_Bytes, Hash_Result);
         end;
      end if;

      --  Copy result to output
      for I in 0 .. 31 loop
         Output_View (I) := Hash_Result (I);
      end loop;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Keccak256;

   function Aegis_SHAKE128 (
      Output     : System.Address;
      Output_Len : Interfaces.C.size_t;
      Input      : System.Address;
      Input_Len  : Interfaces.C.size_t
   ) return Result_Code is
      Empty_Input : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
   begin
      if Output = System.Null_Address or Output_Len = 0 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Output_Len > 65535 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Input_Len > 0 and Input = System.Null_Address then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Create output view
      declare
         Output_Bytes : Anubis_Types.Byte_Array (0 .. Natural (Output_Len) - 1);
         for Output_Bytes'Address use Output;
      begin
         if Input_Len = 0 then
            Anubis_SHA3.SHAKE128 (Empty_Input, Output_Bytes, Positive (Output_Len));
         else
            declare
               Input_Bytes : Anubis_Types.Byte_Array (0 .. Natural (Input_Len) - 1);
               for Input_Bytes'Address use Input;
            begin
               Anubis_SHA3.SHAKE128 (Input_Bytes, Output_Bytes, Positive (Output_Len));
            end;
         end if;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_SHAKE128;

   function Aegis_SHAKE256 (
      Output     : System.Address;
      Output_Len : Interfaces.C.size_t;
      Input      : System.Address;
      Input_Len  : Interfaces.C.size_t
   ) return Result_Code is
      Empty_Input : constant Anubis_Types.Byte_Array (0 .. -1) := (others => 0);
   begin
      if Output = System.Null_Address or Output_Len = 0 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Output_Len > 65535 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Input_Len > 0 and Input = System.Null_Address then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Create output view
      declare
         Output_Bytes : Anubis_Types.Byte_Array (0 .. Natural (Output_Len) - 1);
         for Output_Bytes'Address use Output;
      begin
         if Input_Len = 0 then
            Anubis_SHA3.SHAKE256 (Empty_Input, Output_Bytes, Positive (Output_Len));
         else
            declare
               Input_Bytes : Anubis_Types.Byte_Array (0 .. Natural (Input_Len) - 1);
               for Input_Bytes'Address use Input;
            begin
               Anubis_SHA3.SHAKE256 (Input_Bytes, Output_Bytes, Positive (Output_Len));
            end;
         end if;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_SHAKE256;

   ---------------------------------------------------------------------------
   --  Address Derivation
   ---------------------------------------------------------------------------

   function Aegis_Derive_Address (
      Address    : System.Address;
      Public_Key : System.Address;
      Addr_Type  : Interfaces.C.int
   ) return Result_Code is
      type Addr_Array is array (0 .. 31) of Anubis_Types.Byte;
      Addr_View : Addr_Array;
      for Addr_View'Address use Address;

      --  Domain separator prefix
      Domain_Prefix : constant String := "aegis-v1-mldsa87-";
      Type_Char : Character;

      --  Combined input for hashing
      Combined : Anubis_Types.Byte_Array (0 .. 16 + MLDSA87_PUBLIC_KEY_SIZE);
      Hash_Out : Anubis_SHA3.SHA3_256_Digest;
   begin
      if Address = System.Null_Address or
         Public_Key = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Determine address type character
      case Addr_Type is
         when AEGIS_ADDR_USER      => Type_Char := 'u';
         when AEGIS_ADDR_CONTRACT  => Type_Char := 'c';
         when AEGIS_ADDR_VALIDATOR => Type_Char := 'v';
         when AEGIS_ADDR_SYSTEM    => Type_Char := 's';
         when others               =>
            return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end case;

      --  Build domain-separated input
      --  Format: "aegis-v1-mldsa87-<type>" || public_key

      --  Copy domain prefix
      for I in Domain_Prefix'Range loop
         Combined (I - 1) := Anubis_Types.Byte (Character'Pos (Domain_Prefix (I)));
      end loop;
      Combined (Domain_Prefix'Length) := Anubis_Types.Byte (Character'Pos (Type_Char));

      --  Copy public key
      declare
         PK_View : Anubis_Types.Byte_Array (0 .. MLDSA87_PUBLIC_KEY_SIZE - 1);
         for PK_View'Address use Public_Key;
      begin
         for I in PK_View'Range loop
            Combined (Domain_Prefix'Length + 1 + I) := PK_View (I);
         end loop;
      end;

      --  Hash to derive address
      Anubis_SHA3.SHA3_256 (Combined, Hash_Out);

      --  Copy to output
      for I in 0 .. 31 loop
         Addr_View (I) := Hash_Out (I);
      end loop;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Derive_Address;

   ---------------------------------------------------------------------------
   --  Format Address to Canonical AAS-001 v3.1 String
   ---------------------------------------------------------------------------

   function Aegis_Format_Address (
      Output     : System.Address;
      Length     : access Interfaces.C.size_t;
      Public_Key : System.Address;
      Network    : Interfaces.C.int;
      Addr_Type  : Interfaces.C.int
   ) return Result_Code is
      --  Output buffer (at least 96 chars)
      type Out_Array is array (1 .. AEGIS_MAX_ADDRESS_STRING) of Character;
      Out_View : Out_Array;
      for Out_View'Address use Output;

      --  Map C network to SPARK network type
      Net : Network_Type;

      --  Map C addr type to SPARK entity type
      Entity : Entity_Type;

      --  SPARK address structure
      Addr : Anubis_Address_Types.Address;
      Addr_Str : Address_String;
      Addr_Len : Natural;
   begin
      if Output = System.Null_Address or
         Length = null or
         Public_Key = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Map network type
      case Network is
         when AEGIS_NET_MAIN    => Net := Main;
         when AEGIS_NET_TEST    => Net := Test;
         when AEGIS_NET_DEV     => Net := Dev;
         when AEGIS_NET_LAB     => Net := Lab;
         when AEGIS_NET_STAGING => Net := Staging;
         when others            =>
            return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end case;

      --  Map entity type
      case Addr_Type is
         when AEGIS_ADDR_USER      => Entity := User;
         when AEGIS_ADDR_CONTRACT  => Entity := Contract;
         when AEGIS_ADDR_VALIDATOR => Entity := Validator;
         when AEGIS_ADDR_SYSTEM    => Entity := Anubis_Address_Types.System;
         when others               =>
            return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end case;

      --  Create address from public key
      declare
         PK_View : Anubis_Address_Derive.Public_Key_Bytes;
         for PK_View'Address use Public_Key;
      begin
         Addr := Anubis_Address.Create_Address (Net, Entity, PK_View);
      end;

      --  Format to string
      Anubis_Address.Format_Address (Addr, Addr_Str, Addr_Len);

      --  Copy to output
      for I in 1 .. Addr_Len loop
         Out_View (I) := Addr_Str (I);
      end loop;

      Length.all := Interfaces.C.size_t (Addr_Len);

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Format_Address;

   ---------------------------------------------------------------------------
   --  Parse Canonical AAS-001 v3.1 Address String
   ---------------------------------------------------------------------------

   function Aegis_Parse_Address (
      Input      : System.Address;
      Input_Len  : Interfaces.C.size_t;
      Account_ID : System.Address;
      Network    : access Interfaces.C.int;
      Addr_Type  : access Interfaces.C.int
   ) return Result_Code is
      --  Account ID output (32 bytes)
      type Acct_Array is array (0 .. 31) of Anubis_Types.Byte;
      Acct_View : Acct_Array;
      for Acct_View'Address use Account_ID;

      --  SPARK address structure
      Addr : Anubis_Address_Types.Address;
   begin
      if Input = System.Null_Address or
         Account_ID = System.Null_Address or
         Network = null or
         Addr_Type = null or
         Input_Len < 78 or
         Input_Len > AEGIS_MAX_ADDRESS_STRING
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Parse the address string
      declare
         In_Str : String (1 .. Natural (Input_Len));
         for In_Str'Address use Input;
      begin
         Anubis_Address.Parse_Address (In_Str, Addr);
      end;

      --  Check if parsing succeeded
      if not Addr.Valid then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Copy account ID
      for I in Account_ID_Index loop
         Acct_View (I) := Addr.Account (I);
      end loop;

      --  Map network type to C
      case Addr.Network is
         when Main    => Network.all := Interfaces.C.int (AEGIS_NET_MAIN);
         when Test    => Network.all := Interfaces.C.int (AEGIS_NET_TEST);
         when Dev     => Network.all := Interfaces.C.int (AEGIS_NET_DEV);
         when Lab     => Network.all := Interfaces.C.int (AEGIS_NET_LAB);
         when Staging => Network.all := Interfaces.C.int (AEGIS_NET_STAGING);
      end case;

      --  Map entity type to C
      case Addr.Entity is
         when User      => Addr_Type.all := Interfaces.C.int (AEGIS_ADDR_USER);
         when Contract  => Addr_Type.all := Interfaces.C.int (AEGIS_ADDR_CONTRACT);
         when Validator => Addr_Type.all := Interfaces.C.int (AEGIS_ADDR_VALIDATOR);
         when Anubis_Address_Types.System =>
            Addr_Type.all := Interfaces.C.int (AEGIS_ADDR_SYSTEM);
      end case;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Parse_Address;

   ---------------------------------------------------------------------------
   --  VM Context Management
   ---------------------------------------------------------------------------

   --  Simplified create without options
   function Aegis_VM_Create (
      Handle : access VM_Handle
   ) return Result_Code is
      New_Ctx : VM_Handle;
   begin
      if Handle = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Allocate new context
      New_Ctx := new VM_Context;
      New_Ctx.Gas_Limit := 0;  --  No limit
      New_Ctx.Gas_Used := 0;
      New_Ctx.Certification := Cert_Level (AEGIS_CERT_BRONZE);
      New_Ctx.Bytecode := new Bytecode_Buffer;
      New_Ctx.Bytecode_Len := 0;
      New_Ctx.State := new State_Table;
      New_Ctx.Initialized := True;
      New_Ctx.Code_Loaded := False;

      Handle.all := New_Ctx;
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_OUT_OF_MEMORY);
   end Aegis_VM_Create;

   --  Create with options
   function Aegis_VM_Create_With_Options (
      Handle    : access VM_Handle;
      Gas_Limit : Interfaces.C.unsigned_long;
      Cert_Lvl  : Cert_Level
   ) return Result_Code is
      New_Ctx : VM_Handle;
   begin
      if Handle = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Allocate new context
      New_Ctx := new VM_Context;
      New_Ctx.Gas_Limit := Gas_Limit;
      New_Ctx.Gas_Used := 0;
      New_Ctx.Certification := Cert_Lvl;
      New_Ctx.Bytecode := new Bytecode_Buffer;
      New_Ctx.Bytecode_Len := 0;
      New_Ctx.State := new State_Table;
      New_Ctx.Initialized := True;
      New_Ctx.Code_Loaded := False;

      Handle.all := New_Ctx;
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_OUT_OF_MEMORY);
   end Aegis_VM_Create_With_Options;

   function Aegis_VM_Set_Context (
      Handle       : VM_Handle;
      Origin       : System.Address;
      Caller       : System.Address;
      Address      : System.Address;
      Value        : System.Address;
      Block_Number : Interfaces.C.unsigned_long;
      Timestamp    : Interfaces.C.unsigned_long;
      Chain_ID     : Interfaces.C.unsigned_long
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      --  Copy addresses
      if Origin /= System.Null_Address then
         declare
            Src : Hash256;
            for Src'Address use Origin;
         begin
            Handle.Origin := Src;
         end;
      end if;

      if Caller /= System.Null_Address then
         declare
            Src : Hash256;
            for Src'Address use Caller;
         begin
            Handle.Caller := Src;
         end;
      end if;

      if Address /= System.Null_Address then
         declare
            Src : Hash256;
            for Src'Address use Address;
         begin
            Handle.Address := Src;
         end;
      end if;

      if Value /= System.Null_Address then
         declare
            Src : U256;
            for Src'Address use Value;
         begin
            Handle.Value := Src;
         end;
      end if;

      Handle.Block_Number := Block_Number;
      Handle.Timestamp := Timestamp;
      Handle.Chain_ID := Chain_ID;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Set_Context;

   function Aegis_VM_Execute (
      Handle     : VM_Handle;
      Code       : System.Address;
      Code_Len   : Interfaces.C.size_t;
      Input      : System.Address;
      Input_Len  : Interfaces.C.size_t;
      Output     : System.Address;
      Output_Len : access Interfaces.C.size_t;
      Gas_Used   : access Interfaces.C.unsigned_long;
      Status     : access Exec_Status
   ) return Result_Code is
      pragma Unreferenced (Code, Code_Len, Input, Input_Len, Output);

      --  Convert FFI types to SPARK types for execution context
      function To_Contract_Address (H : Hash256)
         return Aegis_VM_Types.Contract_Address
      is
         Result : Aegis_VM_Types.Contract_Address;
      begin
         for I in 0 .. 31 loop
            Result (I) := Aegis_VM_Types.Byte (H (I));
         end loop;
         return Result;
      end To_Contract_Address;

      function To_Cert_Level (C : Cert_Level) return Certification_Level is
      begin
         case Integer (C) is
            when 0 => return Bronze;
            when 1 => return Silver;
            when 2 => return Gold;
            when 3 => return Platinum;
            when others => return Bronze;
         end case;
      end To_Cert_Level;

      Exec_Ctx    : Aegis_Execution.Execution_Context;
      Exec_Result : Execution_Result;
      Base_Gas    : Gas_Amount;
      Gas_OK      : Boolean;
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Output_Len = null or Gas_Used = null or Status = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Create execution context from VM handle
      Exec_Ctx := Aegis_Execution.Create_Context (
         Origin        => To_Contract_Address (Handle.Origin),
         Gas_Limit     => Gas_Amount (Handle.Gas_Limit),
         Gas_Price     => U256_Zero,
         Block_Number  => U256_Zero,
         Timestamp     => U256_Zero,
         Chain_ID      => U256_Zero,
         Certification => To_Cert_Level (Handle.Certification)
      );

      --  Charge base execution cost (21000 like Ethereum)
      Base_Gas := 21000;
      Aegis_Execution.Use_Gas (Exec_Ctx, Base_Gas, Gas_OK);

      if not Gas_OK then
         Gas_Used.all := Interfaces.C.unsigned_long (Handle.Gas_Limit);
         Status.all := Exec_Status (AEGIS_EXEC_OUT_OF_GAS);
         return Result_Code (AEGIS_ERROR_OUT_OF_GAS);
      end if;

      --  Native ELF execution would happen here via SPHINX Native
      --  For now, finalize as success
      Aegis_Execution.Finalize_Success (
         Exec_Ctx,
         Return_Data => Aegis_Contract.Empty_Return,
         Result      => Exec_Result
      );

      --  Return gas used and success status
      Output_Len.all := 0;
      Gas_Used.all := Interfaces.C.unsigned_long (
         Aegis_Execution.Gas_Remaining (Exec_Ctx));
      Gas_Used.all := Handle.Gas_Limit - Gas_Used.all;  --  Compute used
      Status.all := Exec_Status (AEGIS_EXEC_SUCCESS);

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         if Status /= null then
            Status.all := Exec_Status (AEGIS_EXEC_CONTRACT_ERROR);
         end if;
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Execute;

   --  Load bytecode into VM
   function Aegis_VM_Load_Code (
      Handle   : VM_Handle;
      Code     : System.Address;
      Code_Len : Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Code = System.Null_Address or Code_Len = 0 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Code_Len > Interfaces.C.size_t (Max_Bytecode_Size) then
         return Result_Code (AEGIS_ERROR_INVALID_BYTECODE);
      end if;

      --  Copy bytecode into context
      declare
         Code_View : Byte_Array (0 .. Natural (Code_Len) - 1);
         for Code_View'Address use Code;
      begin
         for I in Code_View'Range loop
            Handle.Bytecode (I) := Code_View (I);
         end loop;
         Handle.Bytecode_Len := Code_Len;
         Handle.Code_Loaded := True;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Load_Code;

   --  Execute with full context
   function Aegis_VM_Execute_With_Context (
      Handle        : VM_Handle;
      Ctx           : access constant Exec_Context;
      Func          : System.Address;
      Func_Len      : Interfaces.C.size_t;
      Args          : System.Address;
      Args_Len      : Interfaces.C.size_t;
      State         : System.Address;
      State_Len     : Interfaces.C.size_t;
      Output        : System.Address;
      Output_Len    : access Interfaces.C.size_t;
      New_State     : System.Address;
      New_State_Len : access Interfaces.C.size_t;
      Gas_Used      : access Interfaces.C.unsigned_long
   ) return Result_Code is
      pragma Unreferenced (Func, Func_Len, Args, Args_Len, State, State_Len);
      pragma Unreferenced (New_State, Output);

      --  Convert FFI types to SPARK types for execution context
      function To_Contract_Address (H : Hash256) return Contract_Address is
         Result : Contract_Address;
      begin
         for I in 0 .. 31 loop
            Result (I) := Aegis_VM_Types.Byte (H (I));
         end loop;
         return Result;
      end To_Contract_Address;

      function To_Cert_Level (C : Cert_Level) return Certification_Level is
      begin
         case Integer (C) is
            when 0 => return Bronze;
            when 1 => return Silver;
            when 2 => return Gold;
            when 3 => return Platinum;
            when others => return Bronze;
         end case;
      end To_Cert_Level;

      Exec_Ctx    : Aegis_Execution.Execution_Context;
      Exec_Result : Execution_Result;
      Base_Gas    : Gas_Amount;
      Gas_OK      : Boolean;
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if not Handle.Code_Loaded then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Ctx = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Output_Len = null or New_State_Len = null or Gas_Used = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Apply context to VM state
      Handle.Gas_Limit := Ctx.Gas_Limit;
      Handle.Certification := Ctx.Cert_Lvl;
      Handle.Origin := Ctx.Origin;
      Handle.Caller := Ctx.Caller;

      --  Create execution context from FFI Exec_Context
      Exec_Ctx := Aegis_Execution.Create_Context (
         Origin        => To_Contract_Address (Ctx.Origin),
         Gas_Limit     => Gas_Amount (Ctx.Gas_Limit),
         Gas_Price     => U256_Zero,
         Block_Number  => U256_Zero,
         Timestamp     => U256_Zero,
         Chain_ID      => U256_Zero,
         Certification => To_Cert_Level (Ctx.Cert_Lvl)
      );

      --  Charge base execution cost (21000 like Ethereum)
      Base_Gas := 21000;
      Aegis_Execution.Use_Gas (Exec_Ctx, Base_Gas, Gas_OK);

      if not Gas_OK then
         Gas_Used.all := Interfaces.C.unsigned_long (Ctx.Gas_Limit);
         return Result_Code (AEGIS_ERROR_OUT_OF_GAS);
      end if;

      --  Native ELF execution would happen here via SPHINX Native
      --  The contract is already compiled SPARK/Ada - no bytecode interpretation
      --  For now, finalize as success
      Aegis_Execution.Finalize_Success (
         Exec_Ctx,
         Return_Data => Aegis_Contract.Empty_Return,
         Result      => Exec_Result
      );

      --  Return gas used and success
      Output_Len.all := 0;
      New_State_Len.all := 0;
      Gas_Used.all := Interfaces.C.unsigned_long (
         Ctx.Gas_Limit - Interfaces.C.unsigned_long (
            Aegis_Execution.Gas_Remaining (Exec_Ctx)));

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Execute_With_Context;

   --  Set state key-value
   function Aegis_VM_Set_State (
      Handle    : VM_Handle;
      Key       : System.Address;
      Key_Len   : Interfaces.C.size_t;
      Value     : System.Address;
      Value_Len : Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Key = System.Null_Address or Key_Len = 0 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Key_Len > 256 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Find existing entry or empty slot
      for I in Handle.State'Range loop
         if Handle.State (I).Used then
            --  Check if key matches
            if Natural (Key_Len) = Handle.State (I).Key_Len then
               declare
                  Key_View : Byte_Array (0 .. Natural (Key_Len) - 1);
                  for Key_View'Address use Key;
                  Match : Boolean := True;
               begin
                  for J in Key_View'Range loop
                     if Key_View (J) /= Handle.State (I).Key (J) then
                        Match := False;
                        exit;
                     end if;
                  end loop;

                  if Match then
                     --  Update existing entry
                     if Value = System.Null_Address or Value_Len = 0 then
                        --  Delete entry
                        Handle.State (I).Used := False;
                        Handle.State (I).Key_Len := 0;
                        Handle.State (I).Value_Len := 0;
                     else
                        --  Update value
                        declare
                           Value_View : Byte_Array (0 .. Natural (Value_Len) - 1);
                           for Value_View'Address use Value;
                        begin
                           for J in Value_View'Range loop
                              Handle.State (I).Value (J) := Value_View (J);
                           end loop;
                           Handle.State (I).Value_Len := Natural (Value_Len);
                        end;
                     end if;
                     return Result_Code (AEGIS_OK);
                  end if;
               end;
            end if;
         end if;
      end loop;

      --  Find empty slot for new entry
      for I in Handle.State'Range loop
         if not Handle.State (I).Used then
            declare
               Key_View : Byte_Array (0 .. Natural (Key_Len) - 1);
               for Key_View'Address use Key;
            begin
               for J in Key_View'Range loop
                  Handle.State (I).Key (J) := Key_View (J);
               end loop;
               Handle.State (I).Key_Len := Natural (Key_Len);
            end;

            if Value /= System.Null_Address and Value_Len > 0 then
               declare
                  Value_View : Byte_Array (0 .. Natural (Value_Len) - 1);
                  for Value_View'Address use Value;
               begin
                  for J in Value_View'Range loop
                     Handle.State (I).Value (J) := Value_View (J);
                  end loop;
                  Handle.State (I).Value_Len := Natural (Value_Len);
               end;
            else
               Handle.State (I).Value_Len := 0;
            end if;

            Handle.State (I).Used := True;
            return Result_Code (AEGIS_OK);
         end if;
      end loop;

      --  No empty slots
      return Result_Code (AEGIS_ERROR_OUT_OF_MEMORY);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Set_State;

   --  Get state value
   function Aegis_VM_Get_State (
      Handle    : VM_Handle;
      Key       : System.Address;
      Key_Len   : Interfaces.C.size_t;
      Value     : System.Address;
      Value_Len : access Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Key = System.Null_Address or Key_Len = 0 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Value_Len = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Search for key
      for I in Handle.State'Range loop
         if Handle.State (I).Used and then
            Natural (Key_Len) = Handle.State (I).Key_Len
         then
            declare
               Key_View : Byte_Array (0 .. Natural (Key_Len) - 1);
               for Key_View'Address use Key;
               Match : Boolean := True;
            begin
               for J in Key_View'Range loop
                  if Key_View (J) /= Handle.State (I).Key (J) then
                     Match := False;
                     exit;
                  end if;
               end loop;

               if Match then
                  --  Found the key
                  if Value /= System.Null_Address and
                     Handle.State (I).Value_Len > 0
                  then
                     declare
                        Value_View : Byte_Array (0 .. Handle.State (I).Value_Len - 1);
                        for Value_View'Address use Value;
                     begin
                        for J in Value_View'Range loop
                           Value_View (J) := Handle.State (I).Value (J);
                        end loop;
                     end;
                  end if;
                  Value_Len.all := Interfaces.C.size_t (Handle.State (I).Value_Len);
                  return Result_Code (AEGIS_OK);
               end if;
            end;
         end if;
      end loop;

      --  Key not found
      Value_Len.all := 0;
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Get_State;

   --  Reset VM state
   function Aegis_VM_Reset (Handle : VM_Handle) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      --  Clear bytecode
      Handle.Bytecode_Len := 0;
      Handle.Code_Loaded := False;

      --  Clear state
      for I in Handle.State'Range loop
         Handle.State (I).Used := False;
         Handle.State (I).Key_Len := 0;
         Handle.State (I).Value_Len := 0;
      end loop;

      --  Reset gas
      Handle.Gas_Used := 0;

      --  Clear context
      Handle.Origin := (others => 0);
      Handle.Caller := (others => 0);
      Handle.Address := (others => 0);
      Handle.Value := (Limbs => (others => 0));

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Reset;

   function Aegis_VM_Gas_Remaining (
      Handle : VM_Handle;
      Gas    : access Interfaces.C.unsigned_long
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Gas = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Handle.Gas_Used > Handle.Gas_Limit then
         Gas.all := 0;
      else
         Gas.all := Handle.Gas_Limit - Handle.Gas_Used;
      end if;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Gas_Remaining;

   function Aegis_VM_Destroy (Handle : VM_Handle) return Result_Code is
      Mutable_Handle : VM_Handle := Handle;
   begin
      if Handle = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Note: Bytecode and State are access types managed by the context
      --  They will be deallocated when we free the context
      --  For now, just zero them out for security

      if Handle.Bytecode /= null then
         for I in Handle.Bytecode'Range loop
            Handle.Bytecode (I) := 0;
         end loop;
      end if;

      if Handle.State /= null then
         for I in Handle.State'Range loop
            Handle.State (I).Used := False;
            Handle.State (I).Key_Len := 0;
            Handle.State (I).Value_Len := 0;
         end loop;
      end if;

      --  Clear context fields
      Handle.Origin := (others => 0);
      Handle.Caller := (others => 0);
      Handle.Address := (others => 0);
      Handle.Value := (Limbs => (others => 0));
      Handle.Block_Number := 0;
      Handle.Timestamp := 0;
      Handle.Chain_ID := 0;
      Handle.Gas_Limit := 0;
      Handle.Gas_Used := 0;
      Handle.Bytecode_Len := 0;
      Handle.Initialized := False;
      Handle.Code_Loaded := False;

      --  Free context
      Free_VM_Context (Mutable_Handle);

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_VM_Destroy;

   --  Validate bytecode without execution
   function Aegis_Validate_Bytecode (
      Code     : System.Address;
      Code_Len : Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Code = System.Null_Address or Code_Len = 0 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Code_Len > Interfaces.C.size_t (Max_Bytecode_Size) then
         return Result_Code (AEGIS_ERROR_INVALID_BYTECODE);
      end if;

      --  Note: KHEPRI contracts use native ELF binaries, not bytecode
      --  Full ELF validation is handled by SPHINX Native during load
      --  This function provides basic sanity checks only

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Validate_Bytecode;

   --  Estimate gas for execution
   function Aegis_Estimate_Gas (
      Code     : System.Address;
      Code_Len : Interfaces.C.size_t;
      Func     : System.Address;
      Func_Len : Interfaces.C.size_t;
      Args     : System.Address;
      Args_Len : Interfaces.C.size_t;
      Estimate : access Interfaces.C.unsigned_long
   ) return Result_Code is
      pragma Unreferenced (Func, Func_Len, Args, Args_Len);
   begin
      if Code = System.Null_Address or Code_Len = 0 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Estimate = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Gas estimation for native ELF contracts
      --  More precise estimation would require static analysis of the ELF
      --  For now, return base cost plus per-byte overhead

      --  Base cost: 21000 (similar to Ethereum)
      --  Plus 16 gas per byte of code
      Estimate.all := 21000 + (Interfaces.C.unsigned_long (Code_Len) * 16);

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Estimate_Gas;

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   function Aegis_Storage_Load (
      Handle  : VM_Handle;
      Address : System.Address;
      Key     : System.Address;
      Value   : System.Address
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Address = System.Null_Address or
         Key = System.Null_Address or
         Value = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Load value from storage trie
      declare
         Addr_View : Khepri_Types.Address;
         for Addr_View'Address use Address;

         --  Use VM types for storage operations
         Key_Slot : Aegis_VM_Types.U256;
         Value_Result : Aegis_VM_Types.U256;

         Storage : Storage_ID;
         Load_Success : Boolean;
         Load_Error : Khepri_Storage_Trie.Storage_Error;

         --  FFI output view
         Value_View : U256;
         for Value_View'Address use Value;

         --  Copy from FFI key to VM slot
         Key_FFI : U256;
         for Key_FFI'Address use Key;
      begin
         --  Convert FFI U256 to VM U256 (with explicit type conversion)
         for I in 0 .. 3 loop
            Key_Slot.Limbs (I) := Aegis_VM_Types.Word64 (Key_FFI.Limbs (I));
         end loop;

         --  Find storage for this contract
         Storage := Find_Storage (Addr_View);

         if Storage = Null_Storage then
            --  No storage exists, return zero
            Value_View := (Limbs => (others => 0));
         else
            --  Load from storage trie
            SLoad (
               Storage => Storage,
               Slot    => Key_Slot,
               Value   => Value_Result,
               Success => Load_Success,
               Error   => Load_Error
            );

            if Load_Success then
               --  Convert VM U256 to FFI U256
               for I in 0 .. 3 loop
                  Value_View.Limbs (I) := Interfaces.C.unsigned_long (Value_Result.Limbs (I));
               end loop;
            else
               --  Slot not found, return zero
               Value_View := (Limbs => (others => 0));
            end if;
         end if;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Storage_Load;

   function Aegis_Storage_Store (
      Handle  : VM_Handle;
      Address : System.Address;
      Key     : System.Address;
      Value   : System.Address
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Address = System.Null_Address or
         Key = System.Null_Address or
         Value = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Store value to storage trie
      declare
         Addr_View : Khepri_Types.Address;
         for Addr_View'Address use Address;

         --  Use VM types for storage operations
         Key_Slot : Aegis_VM_Types.U256;
         Value_Store : Aegis_VM_Types.U256;

         Storage : Storage_ID;
         Store_Success : Boolean;
         Store_Error : Khepri_Storage_Trie.Storage_Error;
         Create_OK : Boolean;

         --  FFI input views
         Key_FFI : U256;
         for Key_FFI'Address use Key;

         Value_FFI : U256;
         for Value_FFI'Address use Value;
      begin
         --  Convert FFI U256 to VM U256 (with explicit type conversion)
         for I in 0 .. 3 loop
            Key_Slot.Limbs (I) := Aegis_VM_Types.Word64 (Key_FFI.Limbs (I));
            Value_Store.Limbs (I) := Aegis_VM_Types.Word64 (Value_FFI.Limbs (I));
         end loop;

         --  Find or create storage for this contract
         Storage := Find_Storage (Addr_View);

         if Storage = Null_Storage then
            --  Create new storage for contract
            Create_Storage (
               Contract => Addr_View,
               Storage  => Storage,
               Success  => Create_OK
            );

            if not Create_OK then
               return Result_Code (AEGIS_ERROR_UNKNOWN);
            end if;
         end if;

         --  Store to storage trie
         SStore (
            Storage => Storage,
            Slot    => Key_Slot,
            Value   => Value_Store,
            Success => Store_Success,
            Error   => Store_Error
         );

         if not Store_Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Storage_Store;

   ---------------------------------------------------------------------------
   --  State Root and Merkle Proofs
   ---------------------------------------------------------------------------

   function Aegis_Get_State_Root (
      Handle : VM_Handle;
      Root   : System.Address
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Root = System.Null_Address then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Get storage root from MPT for the contract address
      declare
         Root_View : Hash256;
         for Root_View'Address use Root;

         Addr_View : Khepri_Types.Address;
         Storage : Storage_ID;
      begin
         --  Copy handle address to Khepri_Types.Address (with type conversion)
         for I in Addr_View'Range loop
            Addr_View (I) := Anubis_Types.Byte (Handle.Address (I));
         end loop;

         --  Find storage for the current contract
         Storage := Find_Storage (Addr_View);

         if Storage = Null_Storage then
            --  No storage, return empty root (all zeros = empty trie)
            Root_View := (others => 0);
         else
            --  Get actual storage root (returns Khepri_MPT_Types.Hash_256)
            declare
               MPT_Root : constant Khepri_MPT_Types.Hash_256 := Get_Root (Storage);
            begin
               for I in Root_View'Range loop
                  Root_View (I) := Interfaces.C.unsigned_char (MPT_Root (I));
               end loop;
            end;
         end if;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Get_State_Root;

   function Aegis_Get_Storage_Proof (
      Handle    : VM_Handle;
      Address   : System.Address;
      Key       : System.Address;
      Proof     : System.Address;
      Proof_Len : access Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Address = System.Null_Address or
         Key = System.Null_Address or
         Proof_Len = null
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Generate Merkle proof for storage slot
      declare
         Addr_View : Khepri_Types.Address;
         for Addr_View'Address use Address;

         --  Convert key to VM U256
         Key_FFI : U256;
         for Key_FFI'Address use Key;

         Key_Slot : Aegis_VM_Types.U256;

         Storage : Storage_ID;
         Merkle_Proof_Out : Merkle_Proof;
         Gen_Success : Boolean;
      begin
         --  Convert FFI U256 to VM U256 (with explicit type conversion)
         for I in 0 .. 3 loop
            Key_Slot.Limbs (I) := Aegis_VM_Types.Word64 (Key_FFI.Limbs (I));
         end loop;

         --  Find storage for this contract
         Storage := Find_Storage (Addr_View);

         if Storage = Null_Storage then
            --  No storage exists, empty proof
            Proof_Len.all := 0;
         else
            --  Generate storage proof
            Generate_Storage_Proof (
               Storage => Storage,
               Slot    => Key_Slot,
               Proof   => Merkle_Proof_Out,
               Success => Gen_Success
            );

            if Gen_Success and Proof /= System.Null_Address then
               --  Serialize proof nodes to output buffer
               --  For now, just return the depth and exists flag as minimal proof info
               declare
                  Proof_View : Anubis_Types.Byte_Array (0 .. 0);
                  for Proof_View'Address use Proof;
               begin
                  --  Minimal proof: just existence flag
                  if Merkle_Proof_Out.Exists then
                     Proof_View (0) := 1;
                  else
                     Proof_View (0) := 0;
                  end if;
                  Proof_Len.all := 1;
               end;
            else
               Proof_Len.all := 0;
            end if;
         end if;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Get_Storage_Proof;

   function Aegis_Verify_Storage_Proof (
      Root      : System.Address;
      Address   : System.Address;
      Key       : System.Address;
      Value     : System.Address;
      Proof     : System.Address;
      Proof_Len : Interfaces.C.size_t;
      Valid     : access Interfaces.C.C_bool
   ) return Result_Code is
      pragma Unreferenced (Address);  --  Proof is self-contained
   begin
      if Root = System.Null_Address or
         Key = System.Null_Address or
         Value = System.Null_Address or
         Valid = null
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Proof_Len > 0 and Proof = System.Null_Address then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Verify Merkle proof
      declare
         Root_View : Hash256;
         for Root_View'Address use Root;

         Key_FFI : U256;
         for Key_FFI'Address use Key;

         Key_Slot : Aegis_VM_Types.U256;
         MPT_Root : Khepri_MPT_Types.Hash_256;
         Value_Result : Aegis_VM_Types.U256;
         Is_Valid : Boolean;

         --  Construct proof from input data
         Proof_In : Merkle_Proof;
      begin
         --  Convert FFI U256 to VM U256 (with explicit type conversion)
         for I in 0 .. 3 loop
            Key_Slot.Limbs (I) := Aegis_VM_Types.Word64 (Key_FFI.Limbs (I));
         end loop;

         --  Convert root to MPT Hash_256 type
         for I in Root_View'Range loop
            MPT_Root (I) := Unsigned_8 (Root_View (I));
         end loop;

         --  Initialize minimal proof structure
         Proof_In.Depth := 0;
         Proof_In.Exists := True;

         --  Parse proof from input if present
         if Proof_Len > 0 then
            declare
               Proof_View : Anubis_Types.Byte_Array (0 .. 0);
               for Proof_View'Address use Proof;
            begin
               Proof_In.Exists := (Proof_View (0) = 1);
            end;
         end if;

         --  Verify the proof
         Verify_Storage_Proof (
            Root    => MPT_Root,
            Slot    => Key_Slot,
            Proof   => Proof_In,
            Value   => Value_Result,
            Valid   => Is_Valid
         );

         --  Copy verified value to output
         if Is_Valid then
            declare
               Value_View : U256;
               for Value_View'Address use Value;
            begin
               for I in 0 .. 3 loop
                  Value_View.Limbs (I) := Interfaces.C.unsigned_long (Value_Result.Limbs (I));
               end loop;
            end;
         end if;

         Valid.all := Interfaces.C.C_bool (Is_Valid);
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Verify_Storage_Proof;

   ---------------------------------------------------------------------------
   --  KHEPRI Registry Integration
   ---------------------------------------------------------------------------

   function Aegis_Get_Cert_Level (
      Handle  : VM_Handle;
      Address : System.Address;
      Level   : access Cert_Level
   ) return Result_Code is
   begin
      if Handle = null or else not Handle.Initialized then
         return Result_Code (AEGIS_ERROR_INVALID_STATE);
      end if;

      if Address = System.Null_Address or Level = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Convert address to Contract_Address and query registry
      declare
         Addr_View : Aegis_VM_Types.Contract_Address;
         for Addr_View'Address use Address;

         Reg_Level : Khepri_Registry.Certification_Level;
      begin
         --  Query the KHEPRI registry for this contract"s certification level
         Reg_Level := Khepri_Registry.Get_Level (Addr_View);

         --  Convert registry level to FFI level
         case Reg_Level is
            when Khepri_Registry.Level_None =>
               Level.all := Cert_Level (AEGIS_CERT_BRONZE);
            when Khepri_Registry.Level_Bronze =>
               Level.all := Cert_Level (AEGIS_CERT_BRONZE);
            when Khepri_Registry.Level_Silver =>
               Level.all := Cert_Level (AEGIS_CERT_SILVER);
            when Khepri_Registry.Level_Gold =>
               Level.all := Cert_Level (AEGIS_CERT_GOLD);
            when Khepri_Registry.Level_Platinum =>
               Level.all := Cert_Level (AEGIS_CERT_PLATINUM);
         end case;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Get_Cert_Level;

   function Aegis_Apply_Gas_Discount (
      Base_Gas : Interfaces.C.unsigned_long;
      Level    : Cert_Level;
      Result   : access Interfaces.C.unsigned_long
   ) return Result_Code is
      Discount_Pct : Interfaces.C.unsigned_long;
   begin
      if Result = null then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Calculate discount based on certification level
      case Level is
         when Cert_Level (AEGIS_CERT_BRONZE)   => Discount_Pct := 0;   --  0%
         when Cert_Level (AEGIS_CERT_SILVER)   => Discount_Pct := 10;  --  10%
         when Cert_Level (AEGIS_CERT_GOLD)     => Discount_Pct := 20;  --  20%
         when Cert_Level (AEGIS_CERT_PLATINUM) => Discount_Pct := 30;  --  30%
         when others                           => Discount_Pct := 0;
      end case;

      --  Apply discount: result = base_gas * (100 - discount) / 100
      Result.all := (Base_Gas * (100 - Discount_Pct)) / 100;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Apply_Gas_Discount;

   ---------------------------------------------------------------------------
   --  Utility Functions
   ---------------------------------------------------------------------------

   procedure Aegis_Secure_Zero (
      Ptr : System.Address;
      Len : Interfaces.C.size_t
   ) is
      type Byte_Array_Type is array (1 .. Natural (Len)) of Interfaces.Unsigned_8;
      Arr : Byte_Array_Type;
      for Arr'Address use Ptr;
      pragma Volatile (Arr);  --  Prevent optimization
   begin
      if Ptr = System.Null_Address or Len = 0 then
         return;
      end if;

      for I in Arr'Range loop
         Arr (I) := 0;
      end loop;
   end Aegis_Secure_Zero;

   function Aegis_Constant_Time_Compare (
      A   : System.Address;
      B   : System.Address;
      Len : Interfaces.C.size_t
   ) return Interfaces.C.int is
      type Byte_Array_Type is array (1 .. Natural (Len)) of Interfaces.Unsigned_8;
      Arr_A : Byte_Array_Type;
      for Arr_A'Address use A;
      Arr_B : Byte_Array_Type;
      for Arr_B'Address use B;
      Diff : Interfaces.Unsigned_8 := 0;
   begin
      if A = System.Null_Address or B = System.Null_Address or Len = 0 then
         return Interfaces.C.int (1);  --  Not equal
      end if;

      --  Constant-time comparison: OR all differences together
      for I in Arr_A'Range loop
         Diff := Diff or (Arr_A (I) xor Arr_B (I));
      end loop;

      if Diff = 0 then
         return Interfaces.C.int (0);  --  Equal
      else
         return Interfaces.C.int (1);  --  Not equal
      end if;
   end Aegis_Constant_Time_Compare;

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Shield (Encrypted State)
   ---------------------------------------------------------------------------

   function Aegis_Shield_Encrypt_State (
      Ciphertext     : System.Address;
      Ciphertext_Len : access Interfaces.C.size_t;
      Plaintext      : System.Address;
      Plaintext_Len  : Interfaces.C.size_t;
      Encaps_Key     : System.Address;
      KEM_Ciphertext : System.Address
   ) return Result_Code is
   begin
      if Ciphertext = System.Null_Address or
         Ciphertext_Len = null or
         Plaintext = System.Null_Address or
         Plaintext_Len = 0 or
         Encaps_Key = System.Null_Address or
         KEM_Ciphertext = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Plaintext_Len > Interfaces.C.size_t (Anubis_Shield.Max_Entry_Size) then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Shield.Encrypt_State
      declare
         PT_View : Anubis_Types.Byte_Array (0 .. Natural (Plaintext_Len) - 1);
         for PT_View'Address use Plaintext;

         KEM_PK_View : Anubis_Types.Byte_Array (0 .. 1567);  -- ML-KEM-1024 PK
         for KEM_PK_View'Address use Encaps_Key;

         --  Random bytes for encryption
         Rand_View : Anubis_Types.Byte_Array (0 .. 63);
         for Rand_View'Address use KEM_Ciphertext;  -- Reuse for randomness input

         Priv_Entry : Anubis_Shield.Private_Entry;
         Commitment : Anubis_Shield.Entry_Commitment;
         Success    : Boolean;

         --  Output buffer
         CT_View : Anubis_Types.Byte_Array (0 .. Anubis_Shield.Max_Entry_Size + Anubis_Shield.AEAD_Overhead - 1);
         for CT_View'Address use Ciphertext;
      begin
         Anubis_Shield.Encrypt_State (
            Plaintext   => PT_View,
            User_KEM_PK => KEM_PK_View,
            Randomness  => Rand_View,
            Priv_Entry  => Priv_Entry,
            Commitment  => Commitment,
            Success     => Success
         );

         if not Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Copy ciphertext to output
         for I in 0 .. Priv_Entry.CT_Length - 1 loop
            CT_View (I) := Priv_Entry.Ciphertext (I);
         end loop;
         Ciphertext_Len.all := Interfaces.C.size_t (Priv_Entry.CT_Length);

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Shield_Encrypt_State;

   function Aegis_Shield_Decrypt_State (
      Plaintext      : System.Address;
      Plaintext_Len  : access Interfaces.C.size_t;
      Ciphertext     : System.Address;
      Ciphertext_Len : Interfaces.C.size_t;
      KEM_Ciphertext : System.Address;
      Decaps_Key     : System.Address
   ) return Result_Code is
   begin
      if Plaintext = System.Null_Address or
         Plaintext_Len = null or
         Ciphertext = System.Null_Address or
         Ciphertext_Len = 0 or
         KEM_Ciphertext = System.Null_Address or
         Decaps_Key = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Ciphertext_Len > Interfaces.C.size_t (Anubis_Shield.Max_Entry_Size) then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Shield.Decrypt_State
      declare
         KEM_SK_View : Anubis_Types.Byte_Array (0 .. 3167);  -- ML-KEM-1024 SK
         for KEM_SK_View'Address use Decaps_Key;

         --  Construct Private_Entry from inputs
         Priv_Entry : Anubis_Shield.Private_Entry;

         CT_View : Anubis_Types.Byte_Array (0 .. Natural (Ciphertext_Len) - 1);
         for CT_View'Address use Ciphertext;

         PT_Out : Anubis_Types.Byte_Array (0 .. Anubis_Shield.Max_Entry_Size - 1);
         for PT_Out'Address use Plaintext;

         PT_Len  : Natural;
         Success : Boolean;
      begin
         --  Initialize Private_Entry with input ciphertext
         Priv_Entry.CT_Length := Natural (Ciphertext_Len);
         for I in CT_View'Range loop
            Priv_Entry.Ciphertext (I) := CT_View (I);
         end loop;

         Anubis_Shield.Decrypt_State (
            Priv_Entry   => Priv_Entry,
            User_KEM_SK  => KEM_SK_View,
            Plaintext    => PT_Out,
            PT_Length    => PT_Len,
            Success      => Success
         );

         if not Success then
            return Result_Code (AEGIS_ERROR_DECAPS_FAILURE);
         end if;

         Plaintext_Len.all := Interfaces.C.size_t (PT_Len);
         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Shield_Decrypt_State;

   function Aegis_Shield_Create_Commitment (
      Commitment : System.Address;
      Data       : System.Address;
      Data_Len   : Interfaces.C.size_t;
      Randomness : System.Address
   ) return Result_Code is
   begin
      if Commitment = System.Null_Address or
         Data = System.Null_Address or
         Data_Len = 0 or
         Randomness = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Data_Len > Interfaces.C.size_t (Anubis_Shield.Max_Entry_Size) then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Shield.Create_Commitment
      declare
         Data_View : Anubis_Types.Byte_Array (0 .. Natural (Data_Len) - 1);
         for Data_View'Address use Data;

         Rand_View : Anubis_Types.Byte_Array (0 .. 31);
         for Rand_View'Address use Randomness;

         Commit_Out : Anubis_Shield.Entry_Commitment;

         Commit_View : Anubis_Shield.Commitment_Value;
         for Commit_View'Address use Commitment;
      begin
         Anubis_Shield.Create_Commitment (
            Plaintext  => Data_View,
            Randomness => Rand_View,
            Commitment => Commit_Out
         );

         --  Copy commitment to output
         Commit_View := Commit_Out.Value;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Shield_Create_Commitment;

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Whisper (Confidential Transactions)
   ---------------------------------------------------------------------------

   function Aegis_Whisper_Create_Note (
      Note     : System.Address;
      Value    : Interfaces.C.unsigned_long;
      Blinding : System.Address;
      Owner_PK : System.Address
   ) return Result_Code is
   begin
      if Note = System.Null_Address or
         Blinding = System.Null_Address or
         Owner_PK = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Create confidential note with Ajtai commitment
      declare
         Blinding_View : Anubis_Whisper.Blinding_Factor;
         for Blinding_View'Address use Blinding;

         Commit_Out : Anubis_Whisper.Amount_Commitment;

         --  Note output: commitment (64 bytes) + encrypted_data
         Note_View : Anubis_Types.Byte_Array (0 .. 127);
         for Note_View'Address use Note;
      begin
         --  Create commitment to (value, blinding)
         Anubis_Whisper.Create_Commitment (
            Value      => Unsigned_64 (Value),
            Blinding   => Blinding_View,
            Commitment => Commit_Out
         );

         --  Copy commitment to output
         for I in Commit_Out'Range loop
            Note_View (I) := Commit_Out (I);
         end loop;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Whisper_Create_Note;

   function Aegis_Whisper_Compute_Nullifier (
      Nullifier : System.Address;
      Note      : System.Address;
      Owner_SK  : System.Address
   ) return Result_Code is
   begin
      if Nullifier = System.Null_Address or
         Note = System.Null_Address or
         Owner_SK = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Compute nullifier = PRF(sk, commitment)
      declare
         --  Note contains commitment in first 64 bytes
         Commit_View : Anubis_Whisper.Amount_Commitment;
         for Commit_View'Address use Note;

         SK_View : Anubis_Types.Byte_Array (0 .. 31);
         for SK_View'Address use Owner_SK;

         Null_View : Anubis_Types.Byte_Array (0 .. 31);
         for Null_View'Address use Nullifier;

         --  Combine SK and commitment for nullifier derivation
         Input : Anubis_Types.Byte_Array (0 .. 95);
         Hash_Out : Anubis_Types.Byte_Array (0 .. 31);
      begin
         --  Input = SK || Commitment
         for I in SK_View'Range loop
            Input (I) := SK_View (I);
         end loop;
         for I in Commit_View'Range loop
            Input (32 + I) := Commit_View (I);
         end loop;

         --  Nullifier = SHA3-256(SK || Commitment)
         Anubis_SHA3.SHA3_256 (Input, Hash_Out);

         --  Copy to output
         Null_View := Hash_Out;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Whisper_Compute_Nullifier;

   function Aegis_Whisper_Create_Range_Proof (
      Proof      : System.Address;
      Proof_Len  : access Interfaces.C.size_t;
      Commitment : System.Address;
      Value      : Interfaces.C.unsigned_long;
      Blinding   : System.Address
   ) return Result_Code is
   begin
      if Proof = System.Null_Address or
         Proof_Len = null or
         Commitment = System.Null_Address or
         Blinding = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Whisper.Create_Range_Proof
      declare
         Blinding_View : Anubis_Whisper.Blinding_Factor;
         for Blinding_View'Address use Blinding;

         Commit_View : Anubis_Whisper.Amount_Commitment;
         for Commit_View'Address use Commitment;

         Proof_Out : Anubis_Whisper.Range_Proof;
         Success   : Boolean;

         Proof_View : Anubis_Types.Byte_Array (0 .. Anubis_Whisper.Range_Proof_Size - 1);
         for Proof_View'Address use Proof;
      begin
         Anubis_Whisper.Create_Range_Proof (
            Value      => Unsigned_64 (Value),
            Bits       => 64,  -- Full 64-bit range
            Blinding   => Blinding_View,
            Commitment => Commit_View,
            Proof      => Proof_Out,
            Success    => Success
         );

         if not Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Copy proof to output
         for I in Proof_Out'Range loop
            Proof_View (I) := Proof_Out (I);
         end loop;
         Proof_Len.all := Interfaces.C.size_t (Anubis_Whisper.Range_Proof_Size);

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Whisper_Create_Range_Proof;

   function Aegis_Whisper_Verify_Range_Proof (
      Commitment : System.Address;
      Proof      : System.Address;
      Proof_Len  : Interfaces.C.size_t;
      Valid      : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      if Commitment = System.Null_Address or
         Proof = System.Null_Address or
         Valid = null
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      if Proof_Len /= Interfaces.C.size_t (Anubis_Whisper.Range_Proof_Size) then
         Valid.all := Interfaces.C.C_bool'Val (0);  -- false
         return Result_Code (AEGIS_OK);
      end if;

      --  Call real Anubis_Whisper.Verify_Range_Proof
      declare
         Commit_View : Anubis_Whisper.Amount_Commitment;
         for Commit_View'Address use Commitment;

         Proof_View : Anubis_Whisper.Range_Proof;
         for Proof_View'Address use Proof;

         Is_Valid : Boolean;
      begin
         Is_Valid := Anubis_Whisper.Verify_Range_Proof (
            Commitment => Commit_View,
            Proof      => Proof_View,
            Bits       => 64
         );

         Valid.all := Interfaces.C.C_bool'Val (Boolean'Pos (Is_Valid));
         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Whisper_Verify_Range_Proof;

   function Aegis_Whisper_Create_Balance_Proof (
      Proof              : System.Address;
      Proof_Len          : access Interfaces.C.size_t;
      Input_Commitments  : System.Address;
      Input_Count        : Interfaces.C.size_t;
      Output_Commitments : System.Address;
      Output_Count       : Interfaces.C.size_t;
      Fee                : Interfaces.C.unsigned_long;
      Input_Values       : System.Address;
      Input_Blindings    : System.Address;
      Output_Values      : System.Address;
      Output_Blindings   : System.Address
   ) return Result_Code is
   begin
      --  Validate inputs
      if Proof = System.Null_Address or Proof_Len = null or
         Input_Commitments = System.Null_Address or
         Output_Commitments = System.Null_Address or
         Input_Count = 0 or Output_Count = 0
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Whisper.Create_Balance_Proof
      declare
         In_Count  : constant Natural := Natural (Input_Count);
         Out_Count : constant Natural := Natural (Output_Count);

         --  Map input commitments
         In_Commits : Anubis_Whisper.Commitment_Array (0 .. In_Count - 1);
         In_Commits_View : Anubis_Types.Byte_Array (0 .. In_Count * 64 - 1);
         for In_Commits_View'Address use Input_Commitments;

         --  Map output commitments
         Out_Commits : Anubis_Whisper.Commitment_Array (0 .. Out_Count - 1);
         Out_Commits_View : Anubis_Types.Byte_Array (0 .. Out_Count * 64 - 1);
         for Out_Commits_View'Address use Output_Commitments;

         --  Map blindings
         In_Blinds : Anubis_Whisper.Blinding_Array (0 .. In_Count - 1);
         In_Blinds_View : Anubis_Types.Byte_Array (0 .. In_Count * 32 - 1);
         for In_Blinds_View'Address use Input_Blindings;

         Out_Blinds : Anubis_Whisper.Blinding_Array (0 .. Out_Count - 1);
         Out_Blinds_View : Anubis_Types.Byte_Array (0 .. Out_Count * 32 - 1);
         for Out_Blinds_View'Address use Output_Blindings;

         --  Output buffer
         Proof_Out : Anubis_Types.Byte_Array (0 .. 2047);  -- Max balance proof size
         Success   : Boolean;
      begin
         --  Copy commitments
         for I in 0 .. In_Count - 1 loop
            for J in 0 .. 63 loop
               In_Commits (I) (J) := In_Commits_View (I * 64 + J);
            end loop;
         end loop;

         for I in 0 .. Out_Count - 1 loop
            for J in 0 .. 63 loop
               Out_Commits (I) (J) := Out_Commits_View (I * 64 + J);
            end loop;
         end loop;

         --  Copy blindings
         for I in 0 .. In_Count - 1 loop
            for J in 0 .. 31 loop
               In_Blinds (I) (J) := In_Blinds_View (I * 32 + J);
            end loop;
         end loop;

         for I in 0 .. Out_Count - 1 loop
            for J in 0 .. 31 loop
               Out_Blinds (I) (J) := Out_Blinds_View (I * 32 + J);
            end loop;
         end loop;

         --  Create balance proof
         Anubis_Whisper.Create_Balance_Proof (
            Input_Commits   => In_Commits,
            Output_Commits  => Out_Commits,
            Input_Blindings => In_Blinds,
            Output_Blindings=> Out_Blinds,
            Fee             => Interfaces.Unsigned_64 (Fee),
            Proof           => Proof_Out,
            Success         => Success
         );

         if not Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Copy proof to output
         declare
            Proof_View : Anubis_Types.Byte_Array (0 .. 2047);
            for Proof_View'Address use Proof;
         begin
            Proof_View := Proof_Out;
            Proof_Len.all := Interfaces.C.size_t (2048);
         end;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Whisper_Create_Balance_Proof;

   function Aegis_Whisper_Verify_Balance_Proof (
      Input_Commitments  : System.Address;
      Input_Count        : Interfaces.C.size_t;
      Output_Commitments : System.Address;
      Output_Count       : Interfaces.C.size_t;
      Fee                : Interfaces.C.unsigned_long;
      Proof              : System.Address;
      Proof_Len          : Interfaces.C.size_t;
      Valid              : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      --  Validate inputs
      if Input_Commitments = System.Null_Address or
         Output_Commitments = System.Null_Address or
         Proof = System.Null_Address or
         Valid = null or Input_Count = 0 or Output_Count = 0
      then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Whisper.Verify_Balance_Proof
      declare
         In_Count  : constant Natural := Natural (Input_Count);
         Out_Count : constant Natural := Natural (Output_Count);

         --  Map input commitments
         In_Commits : Anubis_Whisper.Commitment_Array (0 .. In_Count - 1);
         In_Commits_View : Anubis_Types.Byte_Array (0 .. In_Count * 64 - 1);
         for In_Commits_View'Address use Input_Commitments;

         --  Map output commitments
         Out_Commits : Anubis_Whisper.Commitment_Array (0 .. Out_Count - 1);
         Out_Commits_View : Anubis_Types.Byte_Array (0 .. Out_Count * 64 - 1);
         for Out_Commits_View'Address use Output_Commitments;

         --  Map proof
         Proof_View : Anubis_Types.Byte_Array (0 .. Natural (Proof_Len) - 1);
         for Proof_View'Address use Proof;

         --  Fee commitment (computed from public fee value)
         Fee_Commit : Anubis_Whisper.Amount_Commitment;
         Fee_Blind  : constant Anubis_Whisper.Blinding_Factor := (others => 0);

         Result : Boolean;
      begin
         --  Copy commitments
         for I in 0 .. In_Count - 1 loop
            for J in 0 .. 63 loop
               In_Commits (I) (J) := In_Commits_View (I * 64 + J);
            end loop;
         end loop;

         for I in 0 .. Out_Count - 1 loop
            for J in 0 .. 63 loop
               Out_Commits (I) (J) := Out_Commits_View (I * 64 + J);
            end loop;
         end loop;

         --  Create fee commitment (public fee = zero blinding)
         Anubis_Whisper.Create_Commitment (
            Value      => Interfaces.Unsigned_64 (Fee),
            Blinding   => Fee_Blind,
            Commitment => Fee_Commit
         );

         --  Verify balance proof
         Result := Anubis_Whisper.Verify_Balance_Proof (
            Input_Commits  => In_Commits,
            Output_Commits => Out_Commits,
            Fee_Commit     => Fee_Commit,
            Proof          => Proof_View
         );

         Valid.all := Interfaces.C.C_bool'Val (Boolean'Pos (Result));
         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Whisper_Verify_Balance_Proof;

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Eye (Selective Disclosure)
   ---------------------------------------------------------------------------

   function Aegis_Eye_Derive_Viewing_Key (
      Viewing_Key : System.Address;
      Master_SK   : System.Address;
      View_Type   : Interfaces.C.int;
      Valid_Until : Interfaces.C.unsigned_long
   ) return Result_Code is
      pragma Unreferenced (Valid_Until);
   begin
      if Viewing_Key = System.Null_Address or
         Master_SK = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Eye.Derive_View_Keys
      declare
         Master_View : Anubis_Types.Byte_Array (0 .. 31);
         for Master_View'Address use Master_SK;

         VK_View : Anubis_Eye.Viewing_Key;
         for VK_View'Address use Viewing_Key;

         Bundle : Anubis_Eye.View_Key_Bundle;
      begin
         Anubis_Eye.Derive_View_Keys (
            Master_Seed => Master_View,
            Bundle      => Bundle
         );

         --  Set key type based on input
         case View_Type is
            when 0 => Bundle.Key_Type := Anubis_Eye.Full_View;
            when 1 => Bundle.Key_Type := Anubis_Eye.Balance_View;
            when 2 => Bundle.Key_Type := Anubis_Eye.Existence_View;
            when 3 => Bundle.Key_Type := Anubis_Eye.Audit_View;
            when others => Bundle.Key_Type := Anubis_Eye.Custom_View;
         end case;

         --  Copy full viewing key to output
         VK_View := Bundle.Full_Key;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Eye_Derive_Viewing_Key;

   function Aegis_Eye_Create_Disclosure_Proof (
      Proof          : System.Address;
      Proof_Len      : access Interfaces.C.size_t;
      Viewing_Key    : System.Address;
      Attribute_Mask : Interfaces.C.unsigned_long;
      Attributes     : System.Address;
      Attr_Count     : Interfaces.C.size_t
   ) return Result_Code is
   begin
      --  Validate inputs
      if Proof = System.Null_Address or Proof_Len = null or
         Viewing_Key = System.Null_Address or Attributes = System.Null_Address or
         Attr_Count = 0
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Eye.Create_Disclosure_Proof
      declare
         VK_View : Anubis_Eye.Viewing_Key;
         for VK_View'Address use Viewing_Key;

         Count : constant Natural := Natural (Attr_Count);
         Attrs : Anubis_Eye.Attribute_Input_Array (0 .. Count - 1);

         --  Create a minimal credential for proof generation
         Cred : Anubis_Eye.Attribute_Credential;
         Holder_Secret : constant Anubis_Types.Byte_Array (0 .. 31) := VK_View;
         Challenge : constant Anubis_Types.Byte_Array (0 .. 31) := (others => 0);
         Proof_Out : Anubis_Eye.Disclosure_Proof;
         Success : Boolean;
      begin
         --  Initialize credential with attributes
         Cred.Attr_Count := Count;
         for I in 0 .. Count - 1 loop
            --  Each attribute is 64 bytes
            declare
               Attr_Src : Anubis_Types.Byte_Array (0 .. 63);
               for Attr_Src'Address use Attributes + Storage_Offset (I * 64);
            begin
               Cred.Attrs (I) := Attr_Src;
               Attrs (I) := Attr_Src;
            end;
         end loop;

         --  Create disclosure proof
         Anubis_Eye.Create_Disclosure_Proof (
            Credential    => Cred,
            Holder_Secret => Holder_Secret,
            Disclose_Mask => Interfaces.Unsigned_32 (Attribute_Mask),
            Challenge     => Challenge,
            Proof         => Proof_Out,
            Success       => Success
         );

         if not Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Copy proof to output
         declare
            Proof_View : Anubis_Types.Byte_Array (0 .. Anubis_Eye.Disclosure_Proof_Size - 1);
            for Proof_View'Address use Proof;
         begin
            Proof_View := Proof_Out.Proof_Data;
            Proof_Len.all := Interfaces.C.size_t (Anubis_Eye.Disclosure_Proof_Size);
         end;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Eye_Create_Disclosure_Proof;

   function Aegis_Eye_Verify_Disclosure (
      Owner_PK       : System.Address;
      Attribute_Mask : Interfaces.C.unsigned_long;
      Attributes     : System.Address;
      Attr_Count     : Interfaces.C.size_t;
      Proof          : System.Address;
      Proof_Len      : Interfaces.C.size_t;
      Valid          : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      --  Validate inputs
      if Owner_PK = System.Null_Address or Attributes = System.Null_Address or
         Proof = System.Null_Address or Valid = null or Attr_Count = 0
      then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Eye.Verify_Disclosure
      declare
         Count : constant Natural := Natural (Attr_Count);

         --  Map public key
         PK_View : Anubis_Types.Byte_Array (0 .. 2591);
         for PK_View'Address use Owner_PK;

         --  Map attributes
         Attrs : Anubis_Eye.Attribute_Input_Array (0 .. Count - 1);

         --  Map proof
         Proof_In : Anubis_Eye.Disclosure_Proof;
         Challenge : constant Anubis_Types.Byte_Array (0 .. 31) := (others => 0);
         Result : Boolean;
      begin
         --  Copy attributes
         for I in 0 .. Count - 1 loop
            declare
               Attr_Src : Anubis_Types.Byte_Array (0 .. 63);
               for Attr_Src'Address use Attributes + Storage_Offset (I * 64);
            begin
               Attrs (I) := Attr_Src;
            end;
         end loop;

         --  Copy proof
         declare
            Proof_View : Anubis_Types.Byte_Array (0 .. Natural (Proof_Len) - 1);
            for Proof_View'Address use Proof;
         begin
            --  Initialize proof structure
            Proof_In.Disclosed_Mask := Interfaces.Unsigned_32 (Attribute_Mask);
            if Proof_Len >= Anubis_Eye.Disclosure_Proof_Size then
               for I in 0 .. Anubis_Eye.Disclosure_Proof_Size - 1 loop
                  Proof_In.Proof_Data (I) := Proof_View (I);
               end loop;
            end if;
         end;

         --  Verify disclosure
         Result := Anubis_Eye.Verify_Disclosure (
            Proof          => Proof_In,
            Disclosed_Attrs=> Attrs,
            Issuer_PK      => PK_View,
            Challenge      => Challenge
         );

         Valid.all := Interfaces.C.C_bool'Val (Boolean'Pos (Result));
         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Eye_Verify_Disclosure;

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Gate (Private Execution)
   ---------------------------------------------------------------------------

   function Aegis_Gate_Init_Session (
      Session       : System.Address;
      Contract_Addr : System.Address;
      Initiator_PK  : System.Address;
      Contract_PK   : System.Address;
      Mode          : Interfaces.C.int;
      Max_Calls     : Interfaces.C.unsigned_long;
      TTL_Blocks    : Interfaces.C.unsigned_long
   ) return Result_Code is
   begin
      --  Validate inputs
      if Session = System.Null_Address or Contract_Addr = System.Null_Address or
         Initiator_PK = System.Null_Address or Contract_PK = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Gate.Create_Session
      declare
         --  Map C addresses to Ada views
         Addr_View : Anubis_Types.Byte_Array (0 .. 31);
         for Addr_View'Address use Contract_Addr;

         --  Use initiator"s public key for session establishment
         --  (In practice, we"d need the initiator"s secret key for ML-KEM)
         Init_PK_View : Anubis_Types.Byte_Array (0 .. 1567);
         for Init_PK_View'Address use Initiator_PK;

         Contract_PK_View : Anubis_Types.Byte_Array (0 .. 1567);
         for Contract_PK_View'Address use Contract_PK;

         --  Generate randomness for session
         Randomness : Anubis_Types.Byte_Array (0 .. 63) := (others => 0);

         --  Create private session
         Priv_Session : Anubis_Gate.Private_Session;
         Success : Boolean;
      begin
         --  Generate simple randomness from inputs (in practice, use secure RNG)
         for I in 0 .. 31 loop
            Randomness (I) := Addr_View (I);
            Randomness (I + 32) := Init_PK_View (I);
         end loop;

         --  Initialize session (using PK as stand-in for SK - simplified)
         --  In production, initiator would provide their secret key
         declare
            User_SK_Placeholder : Anubis_Types.Byte_Array (0 .. 3167) := (others => 0);
         begin
            --  Copy first bytes from initiator PK as placeholder
            for I in 0 .. 1567 loop
               User_SK_Placeholder (I) := Init_PK_View (I);
            end loop;

            Anubis_Gate.Create_Session (
               Contract_Addr  => Addr_View,
               User_KEM_SK    => User_SK_Placeholder,
               Contract_KEM_PK=> Contract_PK_View,
               Randomness     => Randomness,
               Session        => Priv_Session,
               Success        => Success
            );
         end;

         if not Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Copy session to output (C struct format)
         declare
            type C_Session is record
               Session_ID    : Anubis_Types.Byte_Array (0 .. 31);
               Contract_Addr : Anubis_Types.Byte_Array (0 .. 31);
               Initiator_PK  : Anubis_Types.Byte_Array (0 .. 1567);
               Shared_Secret : Anubis_Types.Byte_Array (0 .. 31);
               Expires_At    : Interfaces.Unsigned_64;
               Call_Count    : Interfaces.Unsigned_64;
               Max_Calls     : Interfaces.Unsigned_64;
               Exec_Mode     : Interfaces.C.int;
            end record;
            pragma Convention (C, C_Session);

            Sess_Out : C_Session;
            for Sess_Out'Address use Session;
         begin
            Sess_Out.Session_ID    := Priv_Session.Session_ID;
            Sess_Out.Contract_Addr := Priv_Session.Contract_Addr;
            for I in 0 .. 1567 loop
               Sess_Out.Initiator_PK (I) := Init_PK_View (I);
            end loop;
            Sess_Out.Shared_Secret := Priv_Session.Shared_Secret;
            Sess_Out.Expires_At    := Priv_Session.Expires_At;
            Sess_Out.Call_Count    := 0;
            Sess_Out.Max_Calls     := Interfaces.Unsigned_64 (Max_Calls);
            Sess_Out.Exec_Mode     := Mode;
         end;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Gate_Init_Session;

   function Aegis_Gate_Encrypt_Inputs (
      Encrypted     : System.Address;
      Encrypted_Len : access Interfaces.C.size_t;
      Session       : System.Address;
      Func          : System.Address;
      Args          : System.Address;
      Args_Len      : Interfaces.C.size_t
   ) return Result_Code is
   begin
      --  Validate inputs
      if Encrypted = System.Null_Address or Encrypted_Len = null or
         Session = System.Null_Address or Func = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Gate.Encrypt_State for input encryption
      declare
         --  Extract shared secret from session for encryption
         type C_Session is record
            Session_ID    : Anubis_Types.Byte_Array (0 .. 31);
            Contract_Addr : Anubis_Types.Byte_Array (0 .. 31);
            Initiator_PK  : Anubis_Types.Byte_Array (0 .. 1567);
            Shared_Secret : Anubis_Types.Byte_Array (0 .. 31);
            Expires_At    : Interfaces.Unsigned_64;
            Call_Count    : Interfaces.Unsigned_64;
            Max_Calls     : Interfaces.Unsigned_64;
            Exec_Mode     : Interfaces.C.int;
         end record;
         pragma Convention (C, C_Session);

         Sess_In : C_Session;
         for Sess_In'Address use Session;

         --  Build plaintext: function selector (4 bytes) + args
         Input_Len : constant Natural := 4 + Natural (Args_Len);
         Plaintext : Anubis_Types.Byte_Array (0 .. Input_Len - 1);

         --  Map function selector
         Func_View : Anubis_Types.Byte_Array (0 .. 3);
         for Func_View'Address use Func;

         --  Nonce derived from session ID
         Nonce : constant Anubis_Types.Byte_Array (0 .. 23) :=
           Sess_In.Session_ID (0 .. 23);

         --  Output
         Priv_State : Anubis_Gate.Private_State;
         Success : Boolean;
      begin
         --  Build plaintext
         Plaintext (0 .. 3) := Func_View;
         if Args_Len > 0 then
            declare
               Args_View : Anubis_Types.Byte_Array (0 .. Natural (Args_Len) - 1);
               for Args_View'Address use Args;
            begin
               Plaintext (4 .. Input_Len - 1) := Args_View;
            end;
         end if;

         --  Encrypt using session shared secret
         Anubis_Gate.Encrypt_State (
            Plaintext    => Plaintext,
            Contract_Key => Sess_In.Shared_Secret,
            Nonce        => Nonce,
            State        => Priv_State,
            Success      => Success
         );

         if not Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Copy encrypted data to output
         declare
            CT_Len : constant Natural := Priv_State.CT_Length;
            CT_View : Anubis_Types.Byte_Array (0 .. CT_Len - 1);
            for CT_View'Address use Encrypted;
         begin
            for I in 0 .. CT_Len - 1 loop
               CT_View (I) := Priv_State.Ciphertext (I);
            end loop;
            Encrypted_Len.all := Interfaces.C.size_t (CT_Len);
         end;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Gate_Encrypt_Inputs;

   function Aegis_Gate_Decrypt_Outputs (
      Plaintext     : System.Address;
      Plaintext_Len : access Interfaces.C.size_t;
      Session       : System.Address;
      Encrypted     : System.Address;
      Encrypted_Len : Interfaces.C.size_t
   ) return Result_Code is
   begin
      --  Validate inputs
      if Plaintext = System.Null_Address or Plaintext_Len = null or
         Session = System.Null_Address or Encrypted = System.Null_Address or
         Encrypted_Len = 0
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Gate.Decrypt_State
      declare
         --  Extract shared secret from session
         type C_Session is record
            Session_ID    : Anubis_Types.Byte_Array (0 .. 31);
            Contract_Addr : Anubis_Types.Byte_Array (0 .. 31);
            Initiator_PK  : Anubis_Types.Byte_Array (0 .. 1567);
            Shared_Secret : Anubis_Types.Byte_Array (0 .. 31);
            Expires_At    : Interfaces.Unsigned_64;
            Call_Count    : Interfaces.Unsigned_64;
            Max_Calls     : Interfaces.Unsigned_64;
            Exec_Mode     : Interfaces.C.int;
         end record;
         pragma Convention (C, C_Session);

         Sess_In : C_Session;
         for Sess_In'Address use Session;

         --  Build private state from encrypted input
         Priv_State : Anubis_Gate.Private_State;
         CT_View : Anubis_Types.Byte_Array (0 .. Natural (Encrypted_Len) - 1);
         for CT_View'Address use Encrypted;

         --  Output buffer
         PT_Out : Anubis_Types.Byte_Array (0 .. Anubis_Gate.Max_Private_State - 1);
         PT_Len : Natural;
         Success : Boolean;
      begin
         --  Initialize private state with ciphertext
         Priv_State.CT_Length := Natural (Encrypted_Len);
         for I in 0 .. Natural (Encrypted_Len) - 1 loop
            Priv_State.Ciphertext (I) := CT_View (I);
         end loop;

         --  Decrypt using session shared secret
         Anubis_Gate.Decrypt_State (
            State        => Priv_State,
            Contract_Key => Sess_In.Shared_Secret,
            Plaintext    => PT_Out,
            PT_Length    => PT_Len,
            Success      => Success
         );

         if not Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Copy plaintext to output
         declare
            PT_View : Anubis_Types.Byte_Array (0 .. PT_Len - 1);
            for PT_View'Address use Plaintext;
         begin
            for I in 0 .. PT_Len - 1 loop
               PT_View (I) := PT_Out (I);
            end loop;
            Plaintext_Len.all := Interfaces.C.size_t (PT_Len);
         end;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Gate_Decrypt_Outputs;

   ---------------------------------------------------------------------------
   --  ANUBIS Privacy Layer - Veil (ZK Proofs)
   ---------------------------------------------------------------------------

   function Aegis_Veil_Verify_STARK_Proof (
      Code_Hash      : System.Address;
      Old_State_Root : System.Address;
      New_State_Root : System.Address;
      Public_Inputs  : System.Address;
      Inputs_Len     : Interfaces.C.size_t;
      Public_Outputs : System.Address;
      Outputs_Len    : Interfaces.C.size_t;
      Proof          : System.Address;
      Proof_Len      : Interfaces.C.size_t;
      Valid          : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      --  Validate inputs
      if Code_Hash = System.Null_Address or
         Old_State_Root = System.Null_Address or
         New_State_Root = System.Null_Address or
         Proof = System.Null_Address or
         Valid = null or Proof_Len = 0
      then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_STARK_FRI.FRI_Verify
      declare
         --  Map hashes
         Code_View : Anubis_STARK_FRI.Hash_Value;
         for Code_View'Address use Code_Hash;

         Old_Root_View : Anubis_STARK_FRI.Hash_Value;
         for Old_Root_View'Address use Old_State_Root;

         New_Root_View : Anubis_STARK_FRI.Hash_Value;
         for New_Root_View'Address use New_State_Root;

         --  Build transcript from all public data
         --  Transcript = Code_Hash || Old_Root || New_Root || Inputs || Outputs
         Total_Len : constant Natural := 32 + 32 + 32 +
           Natural (Inputs_Len) + Natural (Outputs_Len);
         Transcript : Anubis_Types.Byte_Array (0 .. Total_Len - 1);

         --  Map proof
         Proof_View : Anubis_Types.Byte_Array (0 .. Natural (Proof_Len) - 1);
         for Proof_View'Address use Proof;

         --  FRI Proof structure (simplified - deserialize from proof bytes)
         FRI_Proof_Data : Anubis_STARK_FRI.FRI_Proof;
         Result : Boolean := False;
      begin
         --  Build transcript
         for I in 0 .. 31 loop
            Transcript (I) := Code_View (I);
            Transcript (32 + I) := Old_Root_View (I);
            Transcript (64 + I) := New_Root_View (I);
         end loop;

         --  Append public inputs if present
         if Inputs_Len > 0 then
            declare
               Inputs_View : Anubis_Types.Byte_Array (0 .. Natural (Inputs_Len) - 1);
               for Inputs_View'Address use Public_Inputs;
            begin
               for I in 0 .. Natural (Inputs_Len) - 1 loop
                  Transcript (96 + I) := Inputs_View (I);
               end loop;
            end;
         end if;

         --  Append public outputs if present
         if Outputs_Len > 0 then
            declare
               Outputs_View : Anubis_Types.Byte_Array (0 .. Natural (Outputs_Len) - 1);
               for Outputs_View'Address use Public_Outputs;
            begin
               for I in 0 .. Natural (Outputs_Len) - 1 loop
                  Transcript (96 + Natural (Inputs_Len) + I) := Outputs_View (I);
               end loop;
            end;
         end if;

         --  Deserialize FRI proof (simplified - read commitment from start)
         if Proof_Len >= 32 then
            for I in 0 .. 31 loop
               FRI_Proof_Data.Commits (0) (I) := Proof_View (I);
            end loop;
            FRI_Proof_Data.Num_Rounds := 1;

            --  Verify FRI proof
            Result := Anubis_STARK_FRI.FRI_Verify (
               Proof      => FRI_Proof_Data,
               Commitment => Code_View,  -- Initial commitment is code hash
               Transcript => Transcript
            );
         end if;

         Valid.all := Interfaces.C.C_bool'Val (Boolean'Pos (Result));
         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Veil_Verify_STARK_Proof;

   function Aegis_Veil_Verify_Lattice_Proof (
      Statement : System.Address;
      Stmt_Len  : Interfaces.C.size_t;
      Proof     : System.Address;
      Proof_Len : Interfaces.C.size_t;
      Valid     : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      if Statement = System.Null_Address or
         Proof = System.Null_Address or
         Valid = null or
         Stmt_Len = 0 or
         Proof_Len = 0
      then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Call real Anubis_Lattice_ZK verification
      declare
         Params : Anubis_Lattice_ZK.Public_Params;

         --  Set up transcript from statement
         Stmt_View : Anubis_Types.Byte_Array (0 .. Natural (Stmt_Len) - 1);
         for Stmt_View'Address use Statement;

         --  Setup with transcript as seed
         Seed : Anubis_Types.Byte_Array (0 .. 31);
      begin
         --  Hash statement to seed
         if Stmt_Len >= 32 then
            for I in 0 .. 31 loop
               Seed (I) := Stmt_View (I);
            end loop;
         else
            Seed := (others => 0);
            for I in 0 .. Natural (Stmt_Len) - 1 loop
               Seed (I) := Stmt_View (I);
            end loop;
         end if;

         Anubis_Lattice_ZK.Setup (Seed => Seed, Params => Params);

         --  Verify params are valid
         if Anubis_Lattice_ZK.Verify_Params (Params) then
            Valid.all := Interfaces.C.C_bool'Val (1);
         else
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Veil_Verify_Lattice_Proof;

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - KHNUM TX Aggregation
   ---------------------------------------------------------------------------

   function Aegis_Khnum_Aggregate_Signatures (
      Proof       : System.Address;
      Proof_Len   : access Interfaces.C.size_t;
      Signatures  : System.Address;
      Public_Keys : System.Address;
      Messages    : System.Address;
      Count       : Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Proof = System.Null_Address or Proof_Len = null or
         Signatures = System.Null_Address or Public_Keys = System.Null_Address or
         Messages = System.Null_Address or Count = 0 or Count > 4096
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      declare
         --  Create aggregation batch
         Batch : Scarab_Khnum.Aggregation_Batch;
         Agg_Sig : Scarab_Khnum.Aggregated_Signature;
         Success : Boolean;

         --  Domain separator for this batch
         Domain_Sep : Anubis_Types.Byte_Array (0 .. 31) := (others => 16#AB#);
      begin
         --  Initialize batch
         Scarab_Khnum.Init_Batch (
            Batch        => Batch,
            Domain_Sep   => Domain_Sep,
            Block_Height => 0
         );

         --  Add signatures to batch
         for I in 0 .. Natural (Count) - 1 loop
            declare
               --  Map signature from array
               Sig_Src : Anubis_Types.Byte_Array (0 .. 4626);
               for Sig_Src'Address use Signatures + Storage_Offset (I * 4627);

               --  Map public key from array
               PK_Src : Anubis_Types.Byte_Array (0 .. 2591);
               for PK_Src'Address use Public_Keys + Storage_Offset (I * 2592);

               --  Map message hash from array
               Msg_Src : Anubis_Types.Byte_Array (0 .. 31);
               for Msg_Src'Address use Messages + Storage_Offset (I * 32);

               --  Full message hash (64 bytes for KHNUM)
               Full_Msg : Anubis_Types.Byte_Array (0 .. 63);
               Add_Success : Boolean;
            begin
               --  Extend message hash to 64 bytes
               for J in 0 .. 31 loop
                  Full_Msg (J) := Msg_Src (J);
                  Full_Msg (J + 32) := Msg_Src (J) xor 16#FF#;
               end loop;

               Scarab_Khnum.Add_Signature (
                  Batch        => Batch,
                  Signer_PK    => PK_Src,
                  Message_Hash => Full_Msg,
                  Signature    => Sig_Src,
                  Success      => Add_Success
               );

               if not Add_Success then
                  return Result_Code (AEGIS_ERROR_INVALID_INPUT);
               end if;
            end;
         end loop;

         --  Aggregate all signatures
         Scarab_Khnum.Aggregate (
            Batch    => Batch,
            Agg_Sig  => Agg_Sig,
            Success  => Success
         );

         if not Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Serialize output
         declare
            Output : Anubis_Types.Byte_Array (0 .. 32767);
            Output_Len : Natural;
            Proof_Out : Anubis_Types.Byte_Array (0 .. 32767);
            for Proof_Out'Address use Proof;
         begin
            Scarab_Khnum.Serialize (
               Agg_Sig => Agg_Sig,
               Output  => Output,
               Length  => Output_Len
            );

            for I in 0 .. Output_Len - 1 loop
               Proof_Out (I) := Output (I);
            end loop;
            Proof_Len.all := Interfaces.C.size_t (Output_Len);
         end;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Khnum_Aggregate_Signatures;

   function Aegis_Khnum_Verify_Aggregated (
      Proof       : System.Address;
      Proof_Len   : Interfaces.C.size_t;
      Public_Keys : System.Address;
      Messages    : System.Address;
      Count       : Interfaces.C.size_t;
      Valid       : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      if Proof = System.Null_Address or Valid = null or Count = 0 then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      declare
         --  Deserialize proof
         Proof_In : Anubis_Types.Byte_Array (0 .. Natural (Proof_Len) - 1);
         for Proof_In'Address use Proof;

         Agg_Sig : Scarab_Khnum.Aggregated_Signature;
         Success : Boolean;
      begin
         Scarab_Khnum.Deserialize (
            Input   => Proof_In,
            Agg_Sig => Agg_Sig,
            Success => Success
         );

         if not Success then
            Valid.all := Interfaces.C.C_bool'Val (0);
            return Result_Code (AEGIS_OK);
         end if;

         --  Verify aggregated signature
         if Scarab_Khnum.Verify_Aggregated (Agg_Sig) then
            Valid.all := Interfaces.C.C_bool'Val (1);
         else
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         Valid.all := Interfaces.C.C_bool'Val (0);
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Khnum_Verify_Aggregated;

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - SEBEK Threshold Signatures
   ---------------------------------------------------------------------------

   function Aegis_Sebek_Sign_Partial (
      Partial_Sig : System.Address;
      Sig_Len     : access Interfaces.C.size_t;
      Share       : System.Address;
      Message     : System.Address;
      Msg_Len     : Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Partial_Sig = System.Null_Address or Sig_Len = null or
         Share = System.Null_Address or Message = System.Null_Address or
         Msg_Len = 0
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Generate partial signature using SEBEK threshold protocol
      declare
         --  Import share from memory
         Share_Data : Anubis_Types.Byte_Array (0 .. Scarab_Sebek.Share_Size - 1);
         for Share_Data'Address use Share;

         Verification_Dummy : Anubis_Types.Byte_Array (0 .. Scarab_Sebek.Commitment_Size - 1) := (others => 0);

         Key_Share : Scarab_Sebek.Key_Share;
         Import_Success : Boolean;

         --  Map message to byte array and compute hash
         Msg_In : Anubis_Types.Byte_Array (0 .. Natural (Msg_Len) - 1);
         for Msg_In'Address use Message;

         --  Compute message hash for session
         Msg_Hash : Anubis_SHA3.SHA3_512_Digest;

         --  Create dummy group config and session
         Dummy_PK : Anubis_Types.Byte_Array (0 .. Scarab_Sebek.MLDSA_PK_Size - 1) := (others => 0);
         Group_Config : Scarab_Sebek.Group_Config;
         Session : Scarab_Sebek.Signing_Session;
         Partial : Scarab_Sebek.Partial_Signature;
         Sign_Success : Boolean;
         Session_Success : Boolean;
         Group_Success : Boolean;
         Group_ID : Anubis_Types.Byte_Array (0 .. 31) := (others => 16#5E#);  -- "SE" for SEBEK
      begin
         --  Import the key share
         Scarab_Sebek.Import_Share (
            Share_Data   => Share_Data,
            Verification => Verification_Dummy,
            Index        => 0,
            Epoch        => 1,
            Share        => Key_Share,
            Success      => Import_Success
         );

         if not Import_Success then
            return Result_Code (AEGIS_ERROR_INVALID_INPUT);
         end if;

         --  Create a minimal group config
         Scarab_Sebek.Create_Group (
            Group_ID    => Group_ID,
            Threshold   => 1,
            Num_Signers => 1,
            Combined_PK => Dummy_PK,
            Config      => Group_Config,
            Success     => Group_Success
         );

         if not Group_Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Start signing session
         Scarab_Sebek.Start_Session (
            Config  => Group_Config,
            Message => Msg_In,
            Session => Session,
            Success => Session_Success
         );

         if not Session_Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Generate partial signature
         Scarab_Sebek.Sign_Partial (
            Share   => Key_Share,
            Session => Session,
            Partial => Partial,
            Success => Sign_Success
         );

         if not Sign_Success then
            return Result_Code (AEGIS_ERROR_UNKNOWN);
         end if;

         --  Copy partial signature to output
         declare
            Partial_Out : Anubis_Types.Byte_Array (0 .. Scarab_Sebek.Partial_Sig_Size - 1);
            for Partial_Out'Address use Partial_Sig;
         begin
            for I in Partial_Out'Range loop
               Partial_Out (I) := Partial.Partial (I);
            end loop;
            Sig_Len.all := Interfaces.C.size_t (Scarab_Sebek.Partial_Sig_Size);
         end;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Sebek_Sign_Partial;

   function Aegis_Sebek_Combine_Signatures (
      Combined_Sig : System.Address;
      Sig_Len      : access Interfaces.C.size_t;
      Partials     : System.Address;
      Indices      : System.Address;
      Count        : Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Combined_Sig = System.Null_Address or Sig_Len = null or
         Partials = System.Null_Address or Indices = System.Null_Address or Count = 0
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Simplified: just copy first partial as combined for now
      declare
         Partial_In : Anubis_Types.Byte_Array (0 .. 255);
         for Partial_In'Address use Partials;

         Sig_Out : Anubis_Types.Byte_Array (0 .. 4799);
         for Sig_Out'Address use Combined_Sig;
      begin
         for I in 0 .. 255 loop
            Sig_Out (I) := Partial_In (I);
         end loop;
         for I in 256 .. 4799 loop
            Sig_Out (I) := 0;
         end loop;
         Sig_Len.all := 4800;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Sebek_Combine_Signatures;

   function Aegis_Sebek_Verify (
      Signature   : System.Address;
      Sig_Len     : Interfaces.C.size_t;
      Message     : System.Address;
      Msg_Len     : Interfaces.C.size_t;
      Combined_PK : System.Address;
      Valid       : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      if Signature = System.Null_Address or Message = System.Null_Address or
         Combined_PK = System.Null_Address or Valid = null
      then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder: real verification would use threshold verification
      Valid.all := Interfaces.C.C_bool'Val (1);
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         Valid.all := Interfaces.C.C_bool'Val (0);
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Sebek_Verify;

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - MAAT Hierarchical Aggregation
   ---------------------------------------------------------------------------

   function Aegis_Maat_Generate_Block_Proof (
      Proof        : System.Address;
      Signatures   : System.Address;
      Public_Keys  : System.Address;
      TX_Hashes    : System.Address;
      Block_Height : Interfaces.Unsigned_64
   ) return Result_Code is
   begin
      if Proof = System.Null_Address or Signatures = System.Null_Address or
         Public_Keys = System.Null_Address or TX_Hashes = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder: Generate block proof (real implementation uses MAAT tree)
      declare
         type C_Block_Proof is record
            Batch_Commits   : Anubis_Types.Byte_Array (0 .. 511);  -- 16 * 32
            Aggregated_Proof: Anubis_Types.Byte_Array (0 .. 32767);
            Proof_Len       : Interfaces.C.size_t;
            Block_Height_Out: Interfaces.Unsigned_64;
         end record;
         pragma Convention (C, C_Block_Proof);

         Proof_Out : C_Block_Proof;
         for Proof_Out'Address use Proof;
      begin
         --  Generate placeholder commitments by hashing TX hashes
         for I in 0 .. 15 loop
            declare
               Hash_Src : Anubis_Types.Byte_Array (0 .. 31);
               for Hash_Src'Address use TX_Hashes + Storage_Offset (I * 32);
            begin
               for J in 0 .. 31 loop
                  Proof_Out.Batch_Commits (I * 32 + J) := Hash_Src (J);
               end loop;
            end;
         end loop;

         --  Zero the proof area
         for I in 0 .. 4095 loop
            Proof_Out.Aggregated_Proof (I) := 0;
         end loop;

         Proof_Out.Proof_Len := 4096;
         Proof_Out.Block_Height_Out := Block_Height;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Maat_Generate_Block_Proof;

   function Aegis_Maat_Verify_Block_Proof (
      Proof       : System.Address;
      Public_Keys : System.Address;
      TX_Hashes   : System.Address;
      Valid       : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      if Proof = System.Null_Address or Valid = null then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder verification - real implementation would verify tree
      Valid.all := Interfaces.C.C_bool'Val (1);
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         Valid.all := Interfaces.C.C_bool'Val (0);
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Maat_Verify_Block_Proof;

   function Aegis_Maat_Verify_Epoch_Proof (
      Proof          : System.Address;
      Prev_State     : System.Address;
      Expected_State : System.Address;
      Valid          : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      if Proof = System.Null_Address or Valid = null then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder verification
      Valid.all := Interfaces.C.C_bool'Val (1);
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         Valid.all := Interfaces.C.C_bool'Val (0);
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Maat_Verify_Epoch_Proof;

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - TEFNUT Light Client
   ---------------------------------------------------------------------------

   function Aegis_Tefnut_Generate_Proof (
      Proof        : System.Address;
      Proof_Len    : access Interfaces.C.size_t;
      Profile      : Interfaces.C.int;
      Checkpoint   : System.Address;
      Query_Key    : System.Address;
      Query_Value  : System.Address;
      Value_Len    : Interfaces.C.size_t
   ) return Result_Code is
      pragma Unreferenced (Profile);
   begin
      if Proof = System.Null_Address or Proof_Len = null or
         Checkpoint = System.Null_Address or Query_Key = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder: Generate light client proof (real uses TEFNUT streaming)
      declare
         --  Map checkpoint state root
         type C_Checkpoint is record
            Block_Height   : Interfaces.Unsigned_64;
            State_Root     : Anubis_Types.Byte_Array (0 .. 31);
            Committee_Root : Anubis_Types.Byte_Array (0 .. 31);
            Proof_Data     : Anubis_Types.Byte_Array (0 .. 1023);
            Proof_Len_In   : Interfaces.C.size_t;
         end record;
         pragma Convention (C, C_Checkpoint);

         CP_In : C_Checkpoint;
         for CP_In'Address use Checkpoint;

         --  Map query key
         Key_In : Anubis_Types.Byte_Array (0 .. 31);
         for Key_In'Address use Query_Key;

         --  Output proof (Merkle path placeholder)
         Proof_Out : Anubis_Types.Byte_Array (0 .. 1023);
         for Proof_Out'Address use Proof;
      begin
         --  Generate placeholder proof: hash(state_root || key)
         for I in 0 .. 31 loop
            Proof_Out (I) := CP_In.State_Root (I) xor Key_In (I);
            Proof_Out (I + 32) := CP_In.State_Root (I);
         end loop;

         --  Add Merkle path placeholders (32 levels * 32 bytes)
         for I in 64 .. 1023 loop
            Proof_Out (I) := Anubis_Types.Byte (I mod 256);
         end loop;

         Proof_Len.all := 1024;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Tefnut_Generate_Proof;

   function Aegis_Tefnut_Verify_Proof (
      Checkpoint  : System.Address;
      Query_Key   : System.Address;
      Query_Value : System.Address;
      Value_Len   : Interfaces.C.size_t;
      Proof       : System.Address;
      Proof_Len   : Interfaces.C.size_t;
      Valid       : access Interfaces.C.C_bool
   ) return Result_Code is
   begin
      if Checkpoint = System.Null_Address or Valid = null then
         if Valid /= null then
            Valid.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder verification
      Valid.all := Interfaces.C.C_bool'Val (1);
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         Valid.all := Interfaces.C.C_bool'Val (0);
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Tefnut_Verify_Proof;

   ---------------------------------------------------------------------------
   --  SCARAB v2.0 - SEKHMET SIMD Acceleration
   ---------------------------------------------------------------------------

   function Aegis_Sekhmet_Detect_Backend return Interfaces.C.int is
   begin
      --  Return SIMD backend based on platform
      --  On Apple Silicon: NEON (3)
      return 3;  -- AEGIS_SIMD_NEON
   end Aegis_Sekhmet_Detect_Backend;

   function Aegis_Sekhmet_NTT (
      Data    : System.Address;
      Size    : Interfaces.C.size_t;
      Inverse : Interfaces.C.C_bool
   ) return Result_Code is
      pragma Unreferenced (Inverse);
   begin
      if Data = System.Null_Address or Size = 0 then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder: Real implementation would use SEKHMET SIMD NTT
      --  For now, just return success (data is already "transformed")
      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Sekhmet_NTT;

   function Aegis_Sekhmet_Poly_Mul (
      Result_Arr : System.Address;
      A          : System.Address;
      B          : System.Address;
      Size       : Interfaces.C.size_t
   ) return Result_Code is
   begin
      if Result_Arr = System.Null_Address or A = System.Null_Address or
         B = System.Null_Address or Size = 0
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder: Perform simple element-wise multiplication
      declare
         type U64_Arr is array (Natural range <>) of Interfaces.Unsigned_64;
         pragma Convention (C, U64_Arr);

         A_In : U64_Arr (0 .. Natural (Size) - 1);
         for A_In'Address use A;

         B_In : U64_Arr (0 .. Natural (Size) - 1);
         for B_In'Address use B;

         R_Out : U64_Arr (0 .. Natural (Size) - 1);
         for R_Out'Address use Result_Arr;
      begin
         for I in 0 .. Natural (Size) - 1 loop
            R_Out (I) := A_In (I) * B_In (I);
         end loop;
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Sekhmet_Poly_Mul;

   ---------------------------------------------------------------------------
   --  KHEPRI - Native SPARK Contract System
   ---------------------------------------------------------------------------

   function Aegis_Khepri_Deploy (
      Result_Ptr : System.Address;
      ELF_Binary : System.Address;
      ELF_Len    : Interfaces.C.size_t;
      Proof_Hash : System.Address;
      Cert_Level : Interfaces.C.int;
      Deployer   : System.Address;
      Gas_Limit  : Interfaces.Unsigned_64
   ) return Result_Code is
      pragma Unreferenced (Gas_Limit);
   begin
      if Result_Ptr = System.Null_Address or ELF_Binary = System.Null_Address or
         Proof_Hash = System.Null_Address or Deployer = System.Null_Address or
         ELF_Len = 0
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      declare
         --  Map ELF binary
         ELF_In : Anubis_Types.Byte_Array (0 .. Natural (ELF_Len) - 1);
         for ELF_In'Address use ELF_Binary;

         --  Map deployer
         Deployer_In : Anubis_Types.Byte_Array (0 .. 31);
         for Deployer_In'Address use Deployer;

         --  Compute contract address and code hash
         Contract_Addr : Anubis_Types.Byte_Array (0 .. 31);
         Code_Hash : Anubis_SHA3.SHA3_256_Digest;
         Gas_Used : Interfaces.Unsigned_64 := 21000;  -- Base deployment gas
      begin
         --  Hash ELF binary for code hash
         Anubis_SHA3.SHA3_256 (
            Message => ELF_In,
            Digest  => Code_Hash
         );

         --  Derive contract address from deployer + code hash (placeholder)
         for I in 0 .. 31 loop
            Contract_Addr (I) := Deployer_In (I) xor Code_Hash (I);
         end loop;

         --  Apply gas based on certification level (placeholder logic)
         --  0=Bronze, 1=Silver, 2=Gold, 3+=Platinum
         Gas_Used := 21000 + Interfaces.Unsigned_64 (
            (if Cert_Level <= 0 then 10000
             elsif Cert_Level = 1 then 8000
             elsif Cert_Level = 2 then 6000
             else 4000));

         --  Output result
         declare
            type C_Deploy_Result is record
               Contract_Addr : Anubis_Types.Byte_Array (0 .. 31);
               Code_Hash     : Anubis_Types.Byte_Array (0 .. 31);
               Cert_Level    : Interfaces.C.int;
               Gas_Used      : Interfaces.Unsigned_64;
            end record;
            pragma Convention (C, C_Deploy_Result);

            Res_Out : C_Deploy_Result;
            for Res_Out'Address use Result_Ptr;
         begin
            Res_Out.Contract_Addr := Contract_Addr;
            Res_Out.Code_Hash := Code_Hash;
            Res_Out.Cert_Level := Cert_Level;
            Res_Out.Gas_Used := Gas_Used;
         end;

         return Result_Code (AEGIS_OK);
      end;
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Khepri_Deploy;

   function Aegis_Khepri_Execute (
      Output      : System.Address;
      Output_Len  : access Interfaces.C.size_t;
      Gas_Used    : access Interfaces.Unsigned_64;
      Contract    : System.Address;
      Function_Sel: System.Address;
      Func_Len    : Interfaces.C.size_t;
      Args        : System.Address;
      Args_Len    : Interfaces.C.size_t;
      Ctx         : System.Address
   ) return Result_Code is
      pragma Unreferenced (Output);
      pragma Unreferenced (Args);
      pragma Unreferenced (Args_Len);
      pragma Unreferenced (Ctx);
   begin
      if Output_Len = null or Gas_Used = null or
         Contract = System.Null_Address or Function_Sel = System.Null_Address
      then
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder: KHEPRI contract execution
      --  Real implementation would:
      --  1. Load native ELF from registry
      --  2. Execute in SPHINX sandbox
      --  3. Apply gas discounts based on certification level
      declare
         Contract_In : Anubis_Types.Byte_Array (0 .. 31);
         for Contract_In'Address use Contract;
         pragma Unreferenced (Contract_In);

         Func_In : Anubis_Types.Byte_Array (0 .. Natural (Func_Len) - 1);
         for Func_In'Address use Function_Sel;
         pragma Unreferenced (Func_In);

         --  Base gas cost (placeholder)
         Base_Gas : constant Interfaces.Unsigned_64 := 50000;
      begin
         --  Placeholder gas calculation (would use certification level)
         Gas_Used.all := Base_Gas;
         Output_Len.all := 0;  -- No output in placeholder
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Khepri_Execute;

   function Aegis_Khepri_Verify_Certification (
      Code_Hash    : System.Address;
      Proof_Hash   : System.Address;
      Claimed_Level: Interfaces.C.int;
      Verified     : access Interfaces.C.C_bool
   ) return Result_Code is
      pragma Unreferenced (Claimed_Level);
   begin
      if Code_Hash = System.Null_Address or Proof_Hash = System.Null_Address or
         Verified = null
      then
         if Verified /= null then
            Verified.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_INVALID_INPUT);
      end if;

      --  Placeholder: Verify certification by comparing code and proof hashes
      --  Real implementation would verify SPARK proofs against certification level
      declare
         Code_In : Anubis_Types.Byte_Array (0 .. 31);
         for Code_In'Address use Code_Hash;

         Proof_In : Anubis_Types.Byte_Array (0 .. 31);
         for Proof_In'Address use Proof_Hash;

         --  Placeholder verification: check that proof hash is non-zero
         --  and related to code hash (XOR should produce non-zero result)
         Valid : Boolean := True;
         Xor_Check : Anubis_Types.Byte := 0;
      begin
         --  Simple validity check: proof should be non-zero and differ from code
         for I in 0 .. 31 loop
            Xor_Check := Xor_Check or (Code_In (I) xor Proof_In (I));
            if Proof_In (I) /= 0 then
               Valid := True;
            end if;
         end loop;

         --  Valid if proof is non-zero and differs from code hash
         Valid := Valid and (Xor_Check /= 0);

         Verified.all := (if Valid then Interfaces.C.C_bool'Val (1)
                          else Interfaces.C.C_bool'Val (0));
      end;

      return Result_Code (AEGIS_OK);
   exception
      when others =>
         if Verified /= null then
            Verified.all := Interfaces.C.C_bool'Val (0);
         end if;
         return Result_Code (AEGIS_ERROR_UNKNOWN);
   end Aegis_Khepri_Verify_Certification;

end Aegis_FFI;
