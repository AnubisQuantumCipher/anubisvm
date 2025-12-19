--  SPHINX Native macOS Implementation
--
--  Uses dlopen/dlsym for loading contract dylibs on macOS.
--  Production version: provides real VM syscalls to contracts.

with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Interfaces.C.Strings;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Anubis_Types;
with Anubis_SHA3;
with Anubis_MLDSA;
with Anubis_MLDSA_Types;
with Anubis_MLKEM;
with Anubis_MLKEM_Types;

use type System.Address;
use type Interfaces.C.int;
use type Interfaces.C.size_t;
use type Interfaces.C.Strings.chars_ptr;

package body Sphinx_Native_MacOS is

   ---------------------------------------------------------------------------
   --  C Library Interface (dyld)
   ---------------------------------------------------------------------------

   RTLD_LAZY   : constant Interfaces.C.int := 1;
   RTLD_NOW    : constant Interfaces.C.int := 2;
   RTLD_LOCAL  : constant Interfaces.C.int := 4;
   RTLD_GLOBAL : constant Interfaces.C.int := 8;
   pragma Unreferenced (RTLD_NOW, RTLD_GLOBAL);

   function C_dlopen (
      Path  : Interfaces.C.char_array;
      Mode  : Interfaces.C.int
   ) return System.Address
   with Import => True, Convention => C, External_Name => "dlopen";

   function C_dlsym (
      Handle : System.Address;
      Symbol : Interfaces.C.char_array
   ) return System.Address
   with Import => True, Convention => C, External_Name => "dlsym";

   function C_dlclose (Handle : System.Address) return Interfaces.C.int
   with Import => True, Convention => C, External_Name => "dlclose";

   function C_dlerror return Interfaces.C.Strings.chars_ptr
   with Import => True, Convention => C, External_Name => "dlerror";

   ---------------------------------------------------------------------------
   --  C Types for VM Context (match anubis_syscalls.h)
   ---------------------------------------------------------------------------

   type C_Account_ID is array (0 .. 31) of Unsigned_8
   with Convention => C;

   type C_Syscall_Table;
   type C_Syscall_Table_Ptr is access all C_Syscall_Table
   with Convention => C;

   type C_Execution_Context is record
      Caller       : C_Account_ID;
      Self         : C_Account_ID;
      Call_Value   : Unsigned_64;
      Gas_Limit    : Unsigned_64;
      Gas_Used     : Unsigned_64;
      Block_Number : Unsigned_64;
      Timestamp    : Unsigned_64;
      Syscalls     : C_Syscall_Table_Ptr;
   end record
   with Convention => C;

   type C_Context_Ptr is access all C_Execution_Context
   with Convention => C;

   --  Syscall function types
   type C_SLoad_Fn is access procedure (
      Slot  : System.Address;
      Value : System.Address
   ) with Convention => C;

   type C_SStore_Fn is access procedure (
      Slot  : System.Address;
      Value : System.Address
   ) with Convention => C;

   type C_SHA3_Fn is access procedure (
      Input     : System.Address;
      Input_Len : Interfaces.C.size_t;
      Output    : System.Address
   ) with Convention => C;

   type C_MLDSA_Verify_Fn is access function (
      Message   : System.Address;
      Msg_Len   : Interfaces.C.size_t;
      Signature : System.Address;
      Sig_Len   : Interfaces.C.size_t;
      Pub_Key   : System.Address
   ) return Interfaces.C.int
   with Convention => C;

   type C_MLKEM_Decaps_Fn is access function (
      Ciphertext  : System.Address;
      CT_Len      : Interfaces.C.size_t;
      Secret_Key  : System.Address;
      SK_Len      : Interfaces.C.size_t;
      Shared_Sec  : System.Address
   ) return Interfaces.C.int
   with Convention => C;

   type C_Get_Caller_Fn is access procedure (Addr : System.Address)
   with Convention => C;

   type C_Get_Self_Fn is access procedure (Addr : System.Address)
   with Convention => C;

   type C_Get_U64_Fn is access function return Unsigned_64
   with Convention => C;

   type C_Syscall_Table is record
      SLoad         : C_SLoad_Fn;
      SStore        : C_SStore_Fn;
      SHA3          : C_SHA3_Fn;
      MLDSA_Verify  : C_MLDSA_Verify_Fn;
      MLKEM_Decaps  : C_MLKEM_Decaps_Fn;
      Get_Caller    : C_Get_Caller_Fn;
      Get_Self      : C_Get_Self_Fn;
      Get_Timestamp : C_Get_U64_Fn;
      Get_Block_Num : C_Get_U64_Fn;
      Get_Gas_Rem   : C_Get_U64_Fn;
   end record
   with Convention => C;

   ---------------------------------------------------------------------------
   --  Storage Callback State
   ---------------------------------------------------------------------------

   Current_Load_Fn  : Storage_Load_Fn := null;
   Current_Store_Fn : Storage_Store_Fn := null;

   --  Persistent storage (256 slots for testing)
   type Storage_Array is array (0 .. 255) of Hash256;
   Persistent_Storage : Storage_Array := (others => (others => 0));

   --  Current execution context
   Current_Context : aliased C_Execution_Context;

   ---------------------------------------------------------------------------
   --  VM Syscall Implementations (forward declarations for C convention)
   ---------------------------------------------------------------------------

   procedure VM_SLoad_Impl (Slot : System.Address; Value : System.Address)
      with Export, Convention => C, External_Name => "sphinx_sload";

   procedure VM_SStore_Impl (Slot : System.Address; Value : System.Address)
      with Export, Convention => C, External_Name => "sphinx_sstore";

   procedure VM_SHA3_Impl (
      Input     : System.Address;
      Input_Len : Interfaces.C.size_t;
      Output    : System.Address
   ) with Export, Convention => C, External_Name => "sphinx_sha3";

   function VM_MLDSA_Verify_Impl (
      Message   : System.Address;
      Msg_Len   : Interfaces.C.size_t;
      Signature : System.Address;
      Sig_Len   : Interfaces.C.size_t;
      Pub_Key   : System.Address
   ) return Interfaces.C.int
      with Export, Convention => C, External_Name => "sphinx_mldsa_verify";

   function VM_MLKEM_Decaps_Impl (
      Ciphertext  : System.Address;
      CT_Len      : Interfaces.C.size_t;
      Secret_Key  : System.Address;
      SK_Len      : Interfaces.C.size_t;
      Shared_Sec  : System.Address
   ) return Interfaces.C.int
      with Export, Convention => C, External_Name => "sphinx_mlkem_decaps";

   procedure VM_Get_Caller_Impl (Addr : System.Address)
      with Export, Convention => C, External_Name => "sphinx_get_caller";

   procedure VM_Get_Self_Impl (Addr : System.Address)
      with Export, Convention => C, External_Name => "sphinx_get_self";

   function VM_Get_Timestamp_Impl return Unsigned_64
      with Export, Convention => C, External_Name => "sphinx_get_timestamp";

   function VM_Get_Block_Number_Impl return Unsigned_64
      with Export, Convention => C, External_Name => "sphinx_get_block_number";

   function VM_Get_Gas_Remaining_Impl return Unsigned_64
      with Export, Convention => C, External_Name => "sphinx_get_gas_remaining";

   ---------------------------------------------------------------------------
   --  VM Syscall Implementations (bodies)
   ---------------------------------------------------------------------------

   --  THOTH: Storage Load
   procedure VM_SLoad_Impl (Slot : System.Address; Value : System.Address)
   is
      type Hash_Ptr is access all Hash256;
      function To_Hash_Ptr is new Ada.Unchecked_Conversion (
         System.Address, Hash_Ptr);

      Slot_Ptr  : constant Hash_Ptr := To_Hash_Ptr (Slot);
      Value_Ptr : constant Hash_Ptr := To_Hash_Ptr (Value);
      Idx       : constant Natural := Natural (Slot_Ptr (0)) mod 256;
   begin
      --  Use persistent storage
      Value_Ptr.all := Persistent_Storage (Idx);

      --  Also call Ada callback if set
      if Current_Load_Fn /= null then
         Current_Load_Fn (Slot_Ptr.all, Value_Ptr.all);
      end if;
   end VM_SLoad_Impl;

   --  THOTH: Storage Store
   procedure VM_SStore_Impl (Slot : System.Address; Value : System.Address)
   is
      type Hash_Ptr is access all Hash256;
      function To_Hash_Ptr is new Ada.Unchecked_Conversion (
         System.Address, Hash_Ptr);

      Slot_Ptr  : constant Hash_Ptr := To_Hash_Ptr (Slot);
      Value_Ptr : constant Hash_Ptr := To_Hash_Ptr (Value);
      Idx       : constant Natural := Natural (Slot_Ptr (0)) mod 256;
   begin
      --  Store in persistent storage
      Persistent_Storage (Idx) := Value_Ptr.all;

      --  Also call Ada callback if set
      if Current_Store_Fn /= null then
         Current_Store_Fn (Slot_Ptr.all, Value_Ptr.all);
      end if;
   end VM_SStore_Impl;

   --  ANKH: SHA3-256 (using real VM crypto)
   procedure VM_SHA3_Impl (
      Input     : System.Address;
      Input_Len : Interfaces.C.size_t;
      Output    : System.Address
   )
   is
      type U8_Ptr is access all Unsigned_8;
      type VM_Hash_Ptr is access all Hash256;  -- Use VM's Hash256

      function To_U8_Ptr is new Ada.Unchecked_Conversion (
         System.Address, U8_Ptr);
      function To_Hash_Ptr is new Ada.Unchecked_Conversion (
         System.Address, VM_Hash_Ptr);

      Len : constant Natural := Natural (Input_Len);
      Out_Ptr : constant VM_Hash_Ptr := To_Hash_Ptr (Output);
   begin
      if Len = 0 then
         --  Hash empty message
         declare
            Empty : constant Anubis_Types.Byte_Array (1 .. 0) :=
               (others => 0);  -- Empty array
            Digest : Anubis_SHA3.SHA3_256_Digest;
         begin
            Anubis_SHA3.SHA3_256 (Empty, Digest);
            --  Copy result (Aegis_VM_Types.Byte = Anubis_Types.Byte = Unsigned_8)
            for I in 0 .. 31 loop
               Out_Ptr (I) := Aegis_VM_Types.Byte (Digest (I));
            end loop;
         end;
      else
         --  Hash non-empty message
         declare
            In_Data : Anubis_Types.Byte_Array (0 .. Len - 1);
            Digest  : Anubis_SHA3.SHA3_256_Digest;
         begin
            --  Copy input data
            for I in 0 .. Len - 1 loop
               declare
                  Ptr : constant System.Address := Input + Storage_Offset (I);
                  B : constant U8_Ptr := To_U8_Ptr (Ptr);
               begin
                  In_Data (I) := Anubis_Types.Byte (B.all);
               end;
            end loop;

            --  Call real SHA3-256
            Anubis_SHA3.SHA3_256 (In_Data, Digest);

            --  Copy output
            for I in 0 .. 31 loop
               Out_Ptr (I) := Aegis_VM_Types.Byte (Digest (I));
            end loop;
         end;
      end if;
   end VM_SHA3_Impl;

   --  ANKH: ML-DSA-87 Verify (FIPS 204 Level 5)
   function VM_MLDSA_Verify_Impl (
      Message   : System.Address;
      Msg_Len   : Interfaces.C.size_t;
      Signature : System.Address;
      Sig_Len   : Interfaces.C.size_t;
      Pub_Key   : System.Address
   ) return Interfaces.C.int
   is
      use Anubis_MLDSA;

      --  ML-DSA-87 signature size per FIPS 204 (4627 bytes)
      Expected_Sig_Len : constant := Signature_Bytes;

      Msg_Len_Nat : Natural;
      Result      : Boolean;
   begin
      --  Validate signature length
      if Sig_Len /= Interfaces.C.size_t (Expected_Sig_Len) then
         Ada.Text_IO.Put_Line ("  [MLDSA] Invalid signature length: " &
            Interfaces.C.size_t'Image (Sig_Len) & " (expected" &
            Natural'Image (Expected_Sig_Len) & ")");
         return 0;
      end if;

      --  Validate message length (bounded by Max_Msg_Length)
      if Msg_Len > Interfaces.C.size_t (Max_Msg_Length) then
         Ada.Text_IO.Put_Line ("  [MLDSA] Message too long");
         return 0;
      end if;

      Msg_Len_Nat := Natural (Msg_Len);

      --  Handle null pointers
      if Message = System.Null_Address or else
         Signature = System.Null_Address or else
         Pub_Key = System.Null_Address
      then
         Ada.Text_IO.Put_Line ("  [MLDSA] Null pointer in verify");
         return 0;
      end if;

      --  Copy data from C pointers to Ada arrays
      declare
         --  Local arrays for verification
         Msg_Data : Anubis_Types.Byte_Array (0 .. Msg_Len_Nat - 1);
         Sig_Data : Anubis_MLDSA_Types.Signature;
         PK_Data  : Anubis_MLDSA_Types.Public_Key;

         --  Array access types for bulk copy
         type Msg_Array_Ptr is access all Anubis_Types.Byte_Array (0 .. Msg_Len_Nat - 1);
         type Sig_Array_Ptr is access all Anubis_MLDSA_Types.Signature;
         type PK_Array_Ptr is access all Anubis_MLDSA_Types.Public_Key;

         function To_Msg_Ptr is new Ada.Unchecked_Conversion (
            System.Address, Msg_Array_Ptr);
         function To_Sig_Ptr is new Ada.Unchecked_Conversion (
            System.Address, Sig_Array_Ptr);
         function To_PK_Ptr is new Ada.Unchecked_Conversion (
            System.Address, PK_Array_Ptr);
      begin
         --  Copy message bytes
         if Msg_Len_Nat > 0 then
            Msg_Data := To_Msg_Ptr (Message).all;
         end if;

         --  Copy signature bytes
         Sig_Data := To_Sig_Ptr (Signature).all;

         --  Copy public key bytes
         PK_Data := To_PK_Ptr (Pub_Key).all;

         --  Call real ML-DSA-87 verification
         Ada.Text_IO.Put_Line ("  [MLDSA] Verifying signature (" &
            Natural'Image (Msg_Len_Nat) & " byte message)...");

         Result := Anubis_MLDSA.Verify (
            PK  => PK_Data,
            Msg => Msg_Data,
            Sig => Sig_Data
         );

         if Result then
            Ada.Text_IO.Put_Line ("  [MLDSA] Signature VALID");
            return 1;
         else
            Ada.Text_IO.Put_Line ("  [MLDSA] Signature INVALID");
            return 0;
         end if;
      end;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("  [MLDSA] Verification exception: " &
            Ada.Exceptions.Exception_Message (E));
         return 0;
   end VM_MLDSA_Verify_Impl;

   --  ANKH: ML-KEM-1024 Decapsulation (FIPS 203 Level 5)
   function VM_MLKEM_Decaps_Impl (
      Ciphertext  : System.Address;
      CT_Len      : Interfaces.C.size_t;
      Secret_Key  : System.Address;
      SK_Len      : Interfaces.C.size_t;
      Shared_Sec  : System.Address
   ) return Interfaces.C.int
   is
      use Anubis_MLKEM;

      --  ML-KEM-1024 sizes per FIPS 203
      Expected_CT_Len : constant := Ciphertext_Bytes;   --  1568
      Expected_SK_Len : constant := Decapsulation_Key_Bytes;  --  3168
   begin
      --  Validate ciphertext length
      if CT_Len /= Interfaces.C.size_t (Expected_CT_Len) then
         Ada.Text_IO.Put_Line ("  [MLKEM] Invalid ciphertext length: " &
            Interfaces.C.size_t'Image (CT_Len) & " (expected" &
            Natural'Image (Expected_CT_Len) & ")");
         return 0;
      end if;

      --  Validate secret key length
      if SK_Len /= Interfaces.C.size_t (Expected_SK_Len) then
         Ada.Text_IO.Put_Line ("  [MLKEM] Invalid decapsulation key length: " &
            Interfaces.C.size_t'Image (SK_Len) & " (expected" &
            Natural'Image (Expected_SK_Len) & ")");
         return 0;
      end if;

      --  Handle null pointers
      if Ciphertext = System.Null_Address or else
         Secret_Key = System.Null_Address or else
         Shared_Sec = System.Null_Address
      then
         Ada.Text_IO.Put_Line ("  [MLKEM] Null pointer in decapsulation");
         return 0;
      end if;

      --  Copy data and perform decapsulation
      declare
         CT_Data : Anubis_MLKEM_Types.MLKEM_Ciphertext;
         DK_Data : Anubis_MLKEM_Types.Decapsulation_Key;
         SS_Data : Anubis_MLKEM_Types.Shared_Secret;

         type CT_Array_Ptr is access all Anubis_MLKEM_Types.MLKEM_Ciphertext;
         type DK_Array_Ptr is access all Anubis_MLKEM_Types.Decapsulation_Key;
         type SS_Array_Ptr is access all Anubis_MLKEM_Types.Shared_Secret;

         function To_CT_Ptr is new Ada.Unchecked_Conversion (
            System.Address, CT_Array_Ptr);
         function To_DK_Ptr is new Ada.Unchecked_Conversion (
            System.Address, DK_Array_Ptr);
         function To_SS_Ptr is new Ada.Unchecked_Conversion (
            System.Address, SS_Array_Ptr);
      begin
         --  Copy ciphertext
         CT_Data := To_CT_Ptr (Ciphertext).all;

         --  Copy decapsulation key
         DK_Data := To_DK_Ptr (Secret_Key).all;

         --  Perform decapsulation
         Ada.Text_IO.Put_Line ("  [MLKEM] Decapsulating...");

         Anubis_MLKEM.Decaps (
            DK => DK_Data,
            CT => CT_Data,
            SS => SS_Data
         );

         --  Copy shared secret to output
         To_SS_Ptr (Shared_Sec).all := SS_Data;

         --  Zeroize local copy of shared secret
         for I in SS_Data'Range loop
            SS_Data (I) := 0;
         end loop;

         Ada.Text_IO.Put_Line ("  [MLKEM] Decapsulation complete");
         return 1;
      end;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("  [MLKEM] Decapsulation exception: " &
            Ada.Exceptions.Exception_Message (E));
         return 0;
   end VM_MLKEM_Decaps_Impl;

   procedure VM_Get_Caller_Impl (Addr : System.Address)
   is
      type Acc_Ptr is access all C_Account_ID;
      function To_Acc_Ptr is new Ada.Unchecked_Conversion (
         System.Address, Acc_Ptr);
      Ptr : constant Acc_Ptr := To_Acc_Ptr (Addr);
   begin
      Ptr.all := Current_Context.Caller;
   end VM_Get_Caller_Impl;

   procedure VM_Get_Self_Impl (Addr : System.Address)
   is
      type Acc_Ptr is access all C_Account_ID;
      function To_Acc_Ptr is new Ada.Unchecked_Conversion (
         System.Address, Acc_Ptr);
      Ptr : constant Acc_Ptr := To_Acc_Ptr (Addr);
   begin
      Ptr.all := Current_Context.Self;
   end VM_Get_Self_Impl;

   function VM_Get_Timestamp_Impl return Unsigned_64
   is
   begin
      return Current_Context.Timestamp;
   end VM_Get_Timestamp_Impl;

   function VM_Get_Block_Number_Impl return Unsigned_64
   is
   begin
      return Current_Context.Block_Number;
   end VM_Get_Block_Number_Impl;

   function VM_Get_Gas_Remaining_Impl return Unsigned_64
   is
   begin
      return Current_Context.Gas_Limit - Current_Context.Gas_Used;
   end VM_Get_Gas_Remaining_Impl;

   --  Syscall table instance
   VM_Syscalls : aliased C_Syscall_Table := (
      SLoad         => VM_SLoad_Impl'Access,
      SStore        => VM_SStore_Impl'Access,
      SHA3          => VM_SHA3_Impl'Access,
      MLDSA_Verify  => VM_MLDSA_Verify_Impl'Access,
      MLKEM_Decaps  => VM_MLKEM_Decaps_Impl'Access,
      Get_Caller    => VM_Get_Caller_Impl'Access,
      Get_Self      => VM_Get_Self_Impl'Access,
      Get_Timestamp => VM_Get_Timestamp_Impl'Access,
      Get_Block_Num => VM_Get_Block_Number_Impl'Access,
      Get_Gas_Rem   => VM_Get_Gas_Remaining_Impl'Access
   );

   ---------------------------------------------------------------------------
   --  Helpers
   ---------------------------------------------------------------------------

   function To_C_String (S : String) return Interfaces.C.char_array is
   begin
      return Interfaces.C.To_C (S);
   end To_C_String;

   ---------------------------------------------------------------------------
   --  Public Procedures
   ---------------------------------------------------------------------------

   procedure Load_Library (
      Path    : String;
      Handle  : out Library_Handle;
      Success : out Boolean
   ) is
      C_Path : constant Interfaces.C.char_array := To_C_String (Path);
      Addr   : System.Address;
   begin
      Addr := C_dlopen (C_Path, RTLD_LAZY + RTLD_LOCAL);

      if Addr = System.Null_Address then
         Handle := Null_Handle;
         Success := False;
         declare
            Err : constant Interfaces.C.Strings.chars_ptr := C_dlerror;
         begin
            if Err /= Interfaces.C.Strings.Null_Ptr then
               Ada.Text_IO.Put_Line ("dlopen error: " &
                  Interfaces.C.Strings.Value (Err));
            end if;
         end;
      else
         Handle := Library_Handle (Addr);
         Success := True;
      end if;
   end Load_Library;

   procedure Get_Entry_Point (
      Handle  : Library_Handle;
      Fn_Ptr  : out Contract_Entry_Fn;
      Success : out Boolean
   ) is
      Symbol_Name : constant Interfaces.C.char_array := To_C_String ("contract_execute");
      Sym_Addr    : System.Address;

      function To_Fn is new Ada.Unchecked_Conversion (
         System.Address, Contract_Entry_Fn);
   begin
      if Handle = Null_Handle then
         Fn_Ptr := null;
         Success := False;
         return;
      end if;

      Sym_Addr := C_dlsym (System.Address (Handle), Symbol_Name);

      if Sym_Addr = System.Null_Address then
         Fn_Ptr := null;
         Success := False;
         declare
            Err : constant Interfaces.C.Strings.chars_ptr := C_dlerror;
         begin
            if Err /= Interfaces.C.Strings.Null_Ptr then
               Ada.Text_IO.Put_Line ("dlsym error: " &
                  Interfaces.C.Strings.Value (Err));
            end if;
         end;
      else
         Fn_Ptr := To_Fn (Sym_Addr);
         Success := True;
      end if;
   end Get_Entry_Point;

   procedure Unload_Library (Handle : in out Library_Handle) is
      Result : Interfaces.C.int;
      pragma Unreferenced (Result);
   begin
      if Handle /= Null_Handle then
         Result := C_dlclose (System.Address (Handle));
         Handle := Null_Handle;
      end if;
   end Unload_Library;

   procedure Execute_Contract (
      Handle      : Library_Handle;
      Calldata    : Byte_Array;
      Gas_Limit   : Gas_Amount;
      Return_Data : out Byte_Array;
      Return_Len  : out Natural;
      Gas_Used    : out Gas_Amount;
      Success     : out Boolean
   ) is
      Entry_Fn   : Contract_Entry_Fn;
      Entry_OK   : Boolean;

      Ret_Size_C : aliased Interfaces.C.size_t := 0;
      Gas_Used_C : aliased Unsigned_64 := 0;
      C_Result   : Interfaces.C.int;

      --  vm_context symbol in the library
      Context_Sym : constant Interfaces.C.char_array := To_C_String ("vm_context");
      Context_Addr : System.Address;

      type Context_Ptr_Ptr is access all C_Context_Ptr;
      function To_Context_Ptr_Ptr is new Ada.Unchecked_Conversion (
         System.Address, Context_Ptr_Ptr);
   begin
      Return_Data := (others => 0);
      Return_Len := 0;
      Gas_Used := 0;
      Success := False;

      Get_Entry_Point (Handle, Entry_Fn, Entry_OK);
      if not Entry_OK or Entry_Fn = null then
         Ada.Text_IO.Put_Line ("  [Native] Failed to get entry point");
         return;
      end if;

      --  Find and set vm_context in the library
      Context_Addr := C_dlsym (System.Address (Handle), Context_Sym);
      if Context_Addr /= System.Null_Address then
         --  Set up execution context
         Current_Context.Caller := (others => 16#AB#);  -- Test caller
         Current_Context.Self := (others => 16#CD#);    -- Contract address
         Current_Context.Call_Value := 0;
         Current_Context.Gas_Limit := Unsigned_64 (Gas_Limit);
         Current_Context.Gas_Used := 0;
         Current_Context.Block_Number := 12345;
         Current_Context.Timestamp := 1702400000;  -- Approx current time
         Current_Context.Syscalls := VM_Syscalls'Access;

         --  Set the library's vm_context pointer
         declare
            Ctx_Ptr : constant Context_Ptr_Ptr := To_Context_Ptr_Ptr (Context_Addr);
         begin
            Ctx_Ptr.all := Current_Context'Access;
            Ada.Text_IO.Put_Line ("  [Native] VM context set with syscalls");
         end;
      else
         Ada.Text_IO.Put_Line ("  [Native] No vm_context symbol (legacy mode)");
         --  Legacy contracts without vm_context still work but use callbacks
         if Current_Load_Fn = null or Current_Store_Fn = null then
            Ada.Text_IO.Put_Line ("  [Native] Storage callbacks not set");
            return;
         end if;
      end if;

      Ada.Text_IO.Put_Line ("  [Native] Calling contract_execute...");
      begin
         C_Result := Entry_Fn (
            Calldata     => Calldata (Calldata'First)'Address,
            Calldata_Len => Interfaces.C.size_t (Calldata'Length),
            Return_Buf   => Return_Data (Return_Data'First)'Address,
            Return_Len   => Ret_Size_C'Address,
            Gas_Limit    => Unsigned_64 (Gas_Limit),
            Gas_Used     => Gas_Used_C'Address
         );

         Gas_Used := Gas_Amount (Gas_Used_C);
         Return_Len := Natural (Ret_Size_C);

         if C_Result = 0 then
            Success := True;
            Ada.Text_IO.Put_Line ("  [Native] Contract executed successfully");
         else
            Ada.Text_IO.Put_Line ("  [Native] Contract returned error:" &
               Interfaces.C.int'Image (C_Result));
         end if;
      exception
         when E : others =>
            Ada.Text_IO.Put_Line ("  [Native] Exception during execution: " &
               Ada.Exceptions.Exception_Message (E));
            Success := False;
      end;
   end Execute_Contract;

   procedure Set_Storage_Callbacks (
      Load_Fn  : Storage_Load_Fn;
      Store_Fn : Storage_Store_Fn
   ) is
   begin
      Current_Load_Fn := Load_Fn;
      Current_Store_Fn := Store_Fn;
   end Set_Storage_Callbacks;

end Sphinx_Native_MacOS;
