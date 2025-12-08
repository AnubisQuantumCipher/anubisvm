pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (aegisvmmain, Spec_File_Name => "b__aegisvm.ads");
pragma Source_File_Name (aegisvmmain, Body_File_Name => "b__aegisvm.adb");
pragma Suppress (Overflow_Check);

package body aegisvmmain is

   E073 : Short_Integer; pragma Import (Ada, E073, "ada__exceptions_E");
   E078 : Short_Integer; pragma Import (Ada, E078, "system__soft_links_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "system__exception_table_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "ada__numerics_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "system__exceptions_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "system__soft_links__initialize_E");
   E118 : Short_Integer; pragma Import (Ada, E118, "ada__assertions_E");
   E120 : Short_Integer; pragma Import (Ada, E120, "interfaces__c_E");
   E122 : Short_Integer; pragma Import (Ada, E122, "interfaces__c__strings_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "aegis_storage_E");
   E002 : Short_Integer; pragma Import (Ada, E002, "aegis_contract_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "aegis_sandbox_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "aegis_execution_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "anubis_address_base32_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "anubis_cbd_E");
   E035 : Short_Integer; pragma Import (Ada, E035, "anubis_keccak_E");
   E040 : Short_Integer; pragma Import (Ada, E040, "anubis_mldsa_encoding_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "anubis_mldsa_ntt_E");
   E046 : Short_Integer; pragma Import (Ada, E046, "anubis_mldsa_poly_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "anubis_mlkem_compress_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "anubis_mlkem_encoding_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "anubis_ntt_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "anubis_mlkem_poly_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "anubis_sha3_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "aegis_ffi_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "anubis_address_checksum_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "anubis_address_derive_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "anubis_address_E");
   E048 : Short_Integer; pragma Import (Ada, E048, "anubis_mldsa_sample_E");
   E037 : Short_Integer; pragma Import (Ada, E037, "anubis_mldsa_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "anubis_mlkem_sample_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "anubis_mlkem_E");
   E004 : Short_Integer; pragma Import (Ada, E004, "aegis_crypto_api_E");
   E016 : Short_Integer; pragma Import (Ada, E016, "aegis_syscall_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure aegisvmfinal is

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      null;
   end aegisvmfinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure aegisvminit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;

      aegisvmmain'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      if E073 = 0 then
         Ada.Exceptions'Elab_Spec;
      end if;
      if E078 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E089 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E089 := E089 + 1;
      if E096 = 0 then
         Ada.Numerics'Elab_Spec;
      end if;
      E096 := E096 + 1;
      if E090 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E090 := E090 + 1;
      if E085 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E085 := E085 + 1;
      E078 := E078 + 1;
      E073 := E073 + 1;
      if E118 = 0 then
         Ada.Assertions'Elab_Spec;
      end if;
      E118 := E118 + 1;
      if E120 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      E120 := E120 + 1;
      if E122 = 0 then
         Interfaces.C.Strings'Elab_Spec;
      end if;
      E122 := E122 + 1;
      E014 := E014 + 1;
      E002 := E002 + 1;
      if E012 = 0 then
         Aegis_Sandbox'Elab_Spec;
      end if;
      E012 := E012 + 1;
      E006 := E006 + 1;
      E023 := E023 + 1;
      E030 := E030 + 1;
      E035 := E035 + 1;
      E040 := E040 + 1;
      E044 := E044 + 1;
      E046 := E046 + 1;
      E053 := E053 + 1;
      E055 := E055 + 1;
      E062 := E062 + 1;
      E057 := E057 + 1;
      E064 := E064 + 1;
      E008 := E008 + 1;
      E025 := E025 + 1;
      E027 := E027 + 1;
      E021 := E021 + 1;
      E048 := E048 + 1;
      E037 := E037 + 1;
      E059 := E059 + 1;
      E051 := E051 + 1;
      E004 := E004 + 1;
      E016 := E016 + 1;
   end aegisvminit;

--  BEGIN Object file/option list
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_vm_types.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_gas.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_u256.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_address_types.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_config.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_field.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mldsa_config.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mldsa_field.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_types.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mldsa_types.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mlkem_types.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_storage.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_contract.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_sandbox.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_execution.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_address_base32.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_cbd.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_keccak.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mldsa_encoding.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mldsa_ntt.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mldsa_poly.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mlkem_compress.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mlkem_encoding.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_ntt.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mlkem_poly.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_sha3.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_ffi.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_address_checksum.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_address_derive.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_address.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mldsa_sample.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mldsa.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mlkem_sample.o
   --   /Users/sicarii/anubisvm/core/obj/debug/anubis_mlkem.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_crypto_api.o
   --   /Users/sicarii/anubisvm/core/obj/debug/aegis_syscall.o
   --   -L/Users/sicarii/anubisvm/core/obj/debug/
   --   -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end aegisvmmain;
