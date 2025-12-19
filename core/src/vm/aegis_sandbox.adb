pragma SPARK_Mode (On);

package body Aegis_Sandbox with
   SPARK_Mode => On
is

   ---------------------------------------------------------------------------
   --  Capability Checking
   ---------------------------------------------------------------------------

   function Has_Capability (
      Mask : Capability_Mask;
      Cap  : Capability_Type
   ) return Boolean is
   begin
      return Mask (Cap);
   end Has_Capability;

   function Is_Static_Safe (Syscall : Syscall_Number) return Boolean is
   begin
      --  Static calls cannot modify state
      case Syscall is
         when Sys_SStore | Sys_Call | Sys_DelegateCall |
              Sys_Create | Sys_Create2 | Sys_SelfDestruct |
              Sys_Log0 | Sys_Log1 | Sys_Log2 | Sys_Log3 | Sys_Log4 |
              Sys_Private_Store | Sys_Private_Delete |
              Sys_Confidential_Transfer =>
            return False;  -- State-modifying syscalls
         when others =>
            return True;   -- All others are static-safe
      end case;
   end Is_Static_Safe;

   function Required_Capabilities (
      Syscall : Syscall_Number
   ) return Capability_Mask is
      Result : Capability_Mask := No_Capabilities;
   begin
      case Syscall is
         when Sys_SLoad | Sys_Private_Load =>
            Result (Cap_Read_Storage) := True;
         when Sys_SStore | Sys_Private_Store | Sys_Private_Delete =>
            Result (Cap_Write_Storage) := True;
         when Sys_Call | Sys_StaticCall | Sys_DelegateCall | Sys_Private_Call =>
            Result (Cap_Call) := True;
         when Sys_Create | Sys_Create2 =>
            Result (Cap_Create) := True;
         when Sys_SelfDestruct =>
            Result (Cap_Self_Destruct) := True;
         when Sys_Log0 | Sys_Log1 | Sys_Log2 | Sys_Log3 | Sys_Log4 =>
            Result (Cap_Event) := True;
         when Sys_SHA3 | Sys_Keccak256 | Sys_MLDSA_Verify | Sys_MLKEM_Decaps =>
            Result (Cap_Crypto) := True;
         when Sys_Commit_Amount | Sys_Verify_Range | Sys_Add_Commitments |
              Sys_Verify_Balance | Sys_Verify_Execution |
              Sys_Create_Session | Sys_Close_Session |
              Sys_Create_Disclosure | Sys_Verify_Disclosure |
              Sys_Derive_View_Key | Sys_Generate_Stealth |
              Sys_Ring_Sign | Sys_Verify_Ring_Sig |
              Sys_Compute_Key_Image | Sys_Check_Spent |
              Sys_ZK_Prove_Range | Sys_ZK_Verify_Range |
              Sys_ZK_Prove_Linear | Sys_ZK_Verify_Linear |
              Sys_Confidential_Transfer | Sys_Create_Transfer_Proof |
              Sys_Verify_Transfer | Sys_Scan_Confidential_Output =>
            Result (Cap_Privacy) := True;
         when others =>
            null;  -- No special capabilities needed
      end case;
      return Result;
   end Required_Capabilities;

   ---------------------------------------------------------------------------
   --  Sandbox Validation
   ---------------------------------------------------------------------------

   function Validate_Syscall (
      Context : Sandbox_Context;
      Syscall : Syscall_Number
   ) return Syscall_Result is
      Required : constant Capability_Mask := Required_Capabilities (Syscall);
   begin
      --  Check if call is from static context (using Is_Static_Safe which
      --  mirrors the Ghost Is_State_Modifying logic)
      if Context.Current_Frame.Is_Static and then
         not Is_Static_Safe (Syscall) then
         return Syscall_Denied;
      end if;

      --  Check capabilities
      for Cap in Capability_Type loop
         if Required (Cap) and then
            not Context.Current_Frame.Capabilities (Cap) then
            return Syscall_Denied;
         end if;
      end loop;

      --  Check gas
      if Context.Gas.Gas_Used >= Context.Gas.Gas_Limit then
         return Syscall_Out_Of_Gas;
      end if;

      return Syscall_OK;
   end Validate_Syscall;

   function Validate_Memory_Access (
      Context : Sandbox_Context;
      Offset  : Natural;
      Size    : Natural;
      Mode    : Memory_Access
   ) return Boolean is
      pragma Unreferenced (Mode);
   begin
      --  Check bounds against current memory size
      if Offset + Size > Context.Memory_Size then
         return False;
      end if;

      --  All modes allowed within bounds (Execute handled at higher level)
      return True;
   end Validate_Memory_Access;

   ---------------------------------------------------------------------------
   --  Lemma Subprogram Bodies
   ---------------------------------------------------------------------------

   procedure Lemma_Static_Denies_Modifying (
      Frame   : Call_Frame;
      Syscall : Syscall_Number
   ) is
      pragma Unreferenced (Frame, Syscall);
   begin
      --  Proof follows from definition of Static_Context_Allows
      null;
   end Lemma_Static_Denies_Modifying;

   procedure Lemma_Capability_Sound (
      Frame   : Call_Frame;
      Syscall : Syscall_Number;
      Cap     : Capability_Type
   ) is
      pragma Unreferenced (Frame, Syscall, Cap);
   begin
      --  Proof follows from universal quantifier in Has_Required_Capabilities
      null;
   end Lemma_Capability_Sound;

   procedure Lemma_Valid_Sandbox_Gas_Bounds (
      Ctx : Sandbox_Context
   ) is
      pragma Unreferenced (Ctx);
   begin
      --  Proof follows from Sandbox_Valid decomposition
      null;
   end Lemma_Valid_Sandbox_Gas_Bounds;

   procedure Lemma_Memory_Access_Decidable (
      Ctx    : Sandbox_Context;
      Offset : Natural;
      Size   : Natural
   ) is
      pragma Unreferenced (Ctx, Offset, Size);
   begin
      --  Boolean is decidable by law of excluded middle
      null;
   end Lemma_Memory_Access_Decidable;

   procedure Lemma_Stack_Frame_Valid (
      Ctx : Sandbox_Context;
      Idx : Call_Frame_Index
   ) is
      pragma Unreferenced (Ctx, Idx);
   begin
      --  Proof follows from Stack_Consistent definition
      null;
   end Lemma_Stack_Frame_Valid;

end Aegis_Sandbox;
