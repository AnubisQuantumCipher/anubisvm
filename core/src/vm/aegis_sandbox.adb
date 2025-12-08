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
         when Sys_SLoad | Sys_SHA3 | Sys_Keccak256 | Sys_MLDSA_Verify |
              Sys_MLKEM_Decaps | Sys_Caller | Sys_Address | Sys_CallValue |
              Sys_CallData | Sys_CallDataSize | Sys_BlockNumber |
              Sys_Timestamp | Sys_GasPrice | Sys_GasRemaining |
              Sys_Balance | Sys_SelfBalance | Sys_StaticCall |
              Sys_Return | Sys_Revert | Sys_Stop =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Static_Safe;

   function Required_Capabilities (
      Syscall : Syscall_Number
   ) return Capability_Mask is
      Result : Capability_Mask := No_Capabilities;
   begin
      case Syscall is
         when Sys_SLoad =>
            Result (Cap_Read_Storage) := True;
         when Sys_SStore =>
            Result (Cap_Write_Storage) := True;
         when Sys_Call | Sys_StaticCall | Sys_DelegateCall =>
            Result (Cap_Call) := True;
         when Sys_Create | Sys_Create2 =>
            Result (Cap_Create) := True;
         when Sys_SelfDestruct =>
            Result (Cap_Self_Destruct) := True;
         when Sys_Log0 | Sys_Log1 | Sys_Log2 | Sys_Log3 | Sys_Log4 =>
            Result (Cap_Write_Storage) := True;  -- Logs modify state
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
      --  Check if call is from static context
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

      --  Check gas (simplified - actual cost computed elsewhere)
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
   begin
      --  Check bounds against current memory size
      if Offset + Size > Context.Memory_Size then
         return False;
      end if;

      --  Check access mode (simplified)
      case Mode is
         when Read_Only =>
            return True;  -- Reading is always allowed within bounds
         when Read_Write =>
            return True;  -- Writing is allowed within bounds
         when Execute =>
            return False;  -- Execute not directly supported
      end case;
   end Validate_Memory_Access;

end Aegis_Sandbox;
