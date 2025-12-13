--  SPHINX Runtime Implementation

with Ada.Text_IO;

package body Sphinx_Runtime is

   ---------------------------------------------------------------------------
   --  Context Management
   ---------------------------------------------------------------------------

   procedure Set_Context (Ctx : Context_Access) is
   begin
      Current_Context := Ctx;
   end Set_Context;

   function Get_Context return Context_Access is
   begin
      return Current_Context;
   end Get_Context;

   ---------------------------------------------------------------------------
   --  Storage Operations
   ---------------------------------------------------------------------------

   procedure VM_SLoad (Slot : Hash256; Value : out Hash256) is
      Ctx : constant Context_Access := Current_Context;
   begin
      Value := (others => 0);

      if Ctx = null or else Ctx.Syscalls = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: No syscall table");
         return;
      end if;

      if Ctx.Syscalls.SLoad = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: SLoad syscall not set");
         return;
      end if;

      Ctx.Syscalls.SLoad (Slot'Address, Value'Address);
   end VM_SLoad;

   procedure VM_SStore (Slot : Hash256; Value : Hash256) is
      Ctx : constant Context_Access := Current_Context;
   begin
      if Ctx = null or else Ctx.Syscalls = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: No syscall table");
         return;
      end if;

      if Ctx.Syscalls.SStore = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: SStore syscall not set");
         return;
      end if;

      Ctx.Syscalls.SStore (Slot'Address, Value'Address);
   end VM_SStore;

   ---------------------------------------------------------------------------
   --  Crypto Operations
   ---------------------------------------------------------------------------

   procedure VM_SHA3 (Input : Byte_Array; Output : out Hash256) is
      Ctx : constant Context_Access := Current_Context;
   begin
      Output := (others => 0);

      if Ctx = null or else Ctx.Syscalls = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: No syscall table");
         return;
      end if;

      if Ctx.Syscalls.SHA3 = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: SHA3 syscall not set");
         return;
      end if;

      Ctx.Syscalls.SHA3 (
         Input (Input'First)'Address,
         Interfaces.C.size_t (Input'Length),
         Output'Address
      );
   end VM_SHA3;

   function VM_MLDSA_Verify (
      Message   : Byte_Array;
      Signature : Byte_Array;
      Pub_Key   : Byte_Array
   ) return Boolean is
      Ctx    : constant Context_Access := Current_Context;
      Result : Interfaces.C.int;
   begin
      if Ctx = null or else Ctx.Syscalls = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: No syscall table");
         return False;
      end if;

      if Ctx.Syscalls.MLDSA_Verify = null then
         Ada.Text_IO.Put_Line ("  [SPHINX] Error: MLDSA_Verify syscall not set");
         return False;
      end if;

      Result := Ctx.Syscalls.MLDSA_Verify (
         Message (Message'First)'Address,
         Interfaces.C.size_t (Message'Length),
         Signature (Signature'First)'Address,
         Interfaces.C.size_t (Signature'Length),
         Pub_Key (Pub_Key'First)'Address
      );

      return Result = 1;
   end VM_MLDSA_Verify;

   ---------------------------------------------------------------------------
   --  Environment Operations
   ---------------------------------------------------------------------------

   procedure VM_Get_Caller (Addr : out Account_ID) is
      Ctx : constant Context_Access := Current_Context;
   begin
      Addr := (others => 0);

      if Ctx = null then
         return;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Caller /= null then
         Ctx.Syscalls.Get_Caller (Addr'Address);
      else
         --  Use context's caller directly
         Addr := Ctx.Caller;
      end if;
   end VM_Get_Caller;

   procedure VM_Get_Self (Addr : out Account_ID) is
      Ctx : constant Context_Access := Current_Context;
   begin
      Addr := (others => 0);

      if Ctx = null then
         return;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Self /= null then
         Ctx.Syscalls.Get_Self (Addr'Address);
      else
         --  Use context's self directly
         Addr := Ctx.Self;
      end if;
   end VM_Get_Self;

   function VM_Get_Timestamp return Unsigned_64 is
      Ctx : constant Context_Access := Current_Context;
   begin
      if Ctx = null then
         return 0;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Timestamp /= null then
         return Ctx.Syscalls.Get_Timestamp.all;
      else
         return Ctx.Timestamp;
      end if;
   end VM_Get_Timestamp;

   function VM_Get_Block_Number return Unsigned_64 is
      Ctx : constant Context_Access := Current_Context;
   begin
      if Ctx = null then
         return 0;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Block_Number /= null then
         return Ctx.Syscalls.Get_Block_Number.all;
      else
         return Ctx.Block_Number;
      end if;
   end VM_Get_Block_Number;

   function VM_Get_Gas_Remaining return Unsigned_64 is
      Ctx : constant Context_Access := Current_Context;
   begin
      if Ctx = null then
         return 0;
      end if;

      if Ctx.Syscalls /= null and then Ctx.Syscalls.Get_Gas_Remaining /= null then
         return Ctx.Syscalls.Get_Gas_Remaining.all;
      else
         return Unsigned_64 (Ctx.Gas_Limit - Ctx.Gas_Used);
      end if;
   end VM_Get_Gas_Remaining;

end Sphinx_Runtime;
