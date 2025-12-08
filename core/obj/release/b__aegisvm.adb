pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (aegisvmmain, Spec_File_Name => "b__aegisvm.ads");
pragma Source_File_Name (aegisvmmain, Body_File_Name => "b__aegisvm.adb");
pragma Suppress (Overflow_Check);

package body aegisvmmain is

   E149 : Short_Integer; pragma Import (Ada, E149, "ada__exceptions_E");
   E154 : Short_Integer; pragma Import (Ada, E154, "system__soft_links_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__exception_table_E");
   E172 : Short_Integer; pragma Import (Ada, E172, "ada__numerics_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "system__exceptions_E");
   E161 : Short_Integer; pragma Import (Ada, E161, "system__soft_links__initialize_E");
   E194 : Short_Integer; pragma Import (Ada, E194, "ada__assertions_E");
   E196 : Short_Integer; pragma Import (Ada, E196, "interfaces__c_E");
   E198 : Short_Integer; pragma Import (Ada, E198, "interfaces__c__strings_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "anubis_address_base32_E");
   E030 : Short_Integer; pragma Import (Ada, E030, "anubis_bug_bounty_E");
   E032 : Short_Integer; pragma Import (Ada, E032, "anubis_bytes_E");
   E034 : Short_Integer; pragma Import (Ada, E034, "anubis_cbd_E");
   E036 : Short_Integer; pragma Import (Ada, E036, "anubis_certification_E");
   E043 : Short_Integer; pragma Import (Ada, E043, "anubis_dead_man_switch_E");
   E045 : Short_Integer; pragma Import (Ada, E045, "anubis_developer_rewards_E");
   E053 : Short_Integer; pragma Import (Ada, E053, "anubis_genesis_provers_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "anubis_genesis_validators_E");
   E057 : Short_Integer; pragma Import (Ada, E057, "anubis_governance_E");
   E059 : Short_Integer; pragma Import (Ada, E059, "anubis_keccak_E");
   E066 : Short_Integer; pragma Import (Ada, E066, "anubis_mldsa_encoding_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "anubis_mldsa_ntt_E");
   E072 : Short_Integer; pragma Import (Ada, E072, "anubis_mldsa_poly_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "anubis_mlkem_compress_E");
   E081 : Short_Integer; pragma Import (Ada, E081, "anubis_mlkem_encoding_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "anubis_ntt_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "anubis_mlkem_poly_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "anubis_proof_of_build_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "anubis_quantum_insurance_E");
   E096 : Short_Integer; pragma Import (Ada, E096, "anubis_sha3_E");
   E014 : Short_Integer; pragma Import (Ada, E014, "aegis_storage_E");
   E002 : Short_Integer; pragma Import (Ada, E002, "aegis_contract_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "aegis_sandbox_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "anubis_address_checksum_E");
   E027 : Short_Integer; pragma Import (Ada, E027, "anubis_address_derive_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "anubis_address_E");
   E061 : Short_Integer; pragma Import (Ada, E061, "anubis_lattice_zk_E");
   E074 : Short_Integer; pragma Import (Ada, E074, "anubis_mldsa_sample_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "anubis_mldsa_E");
   E047 : Short_Integer; pragma Import (Ada, E047, "anubis_eye_E");
   E085 : Short_Integer; pragma Import (Ada, E085, "anubis_mlkem_sample_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "anubis_mlkem_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "anubis_shield_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "anubis_stark_poly_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "anubis_stark_fri_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "anubis_gate_E");
   E106 : Short_Integer; pragma Import (Ada, E106, "anubis_token_E");
   E108 : Short_Integer; pragma Import (Ada, E108, "anubis_treasury_E");
   E111 : Short_Integer; pragma Import (Ada, E111, "anubis_vesting_E");
   E113 : Short_Integer; pragma Import (Ada, E113, "anubis_whisper_E");
   E119 : Short_Integer; pragma Import (Ada, E119, "khepri_mpt_types_E");
   E117 : Short_Integer; pragma Import (Ada, E117, "khepri_mpt_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "aegis_execution_E");
   E004 : Short_Integer; pragma Import (Ada, E004, "aegis_crypto_api_E");
   E016 : Short_Integer; pragma Import (Ada, E016, "aegis_syscall_E");
   E121 : Short_Integer; pragma Import (Ada, E121, "khepri_registry_E");
   E125 : Short_Integer; pragma Import (Ada, E125, "khepri_types_E");
   E123 : Short_Integer; pragma Import (Ada, E123, "khepri_storage_trie_E");
   E127 : Short_Integer; pragma Import (Ada, E127, "scarab_aadkg_E");
   E129 : Short_Integer; pragma Import (Ada, E129, "scarab_horus_E");
   E131 : Short_Integer; pragma Import (Ada, E131, "scarab_khnum_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "scarab_maat_E");
   E135 : Short_Integer; pragma Import (Ada, E135, "scarab_sebek_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "scarab_sekhmet_E");
   E139 : Short_Integer; pragma Import (Ada, E139, "scarab_tefnut_E");
   E141 : Short_Integer; pragma Import (Ada, E141, "scarab_thoth_E");
   E008 : Short_Integer; pragma Import (Ada, E008, "aegis_ffi_E");

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

      if E149 = 0 then
         Ada.Exceptions'Elab_Spec;
      end if;
      if E154 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E165 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E165 := E165 + 1;
      if E172 = 0 then
         Ada.Numerics'Elab_Spec;
      end if;
      E172 := E172 + 1;
      if E166 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E166 := E166 + 1;
      if E161 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E161 := E161 + 1;
      E154 := E154 + 1;
      E149 := E149 + 1;
      if E194 = 0 then
         Ada.Assertions'Elab_Spec;
      end if;
      E194 := E194 + 1;
      if E196 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      E196 := E196 + 1;
      if E198 = 0 then
         Interfaces.C.Strings'Elab_Spec;
      end if;
      E198 := E198 + 1;
      E023 := E023 + 1;
      E030 := E030 + 1;
      E032 := E032 + 1;
      E034 := E034 + 1;
      E036 := E036 + 1;
      E043 := E043 + 1;
      E045 := E045 + 1;
      E053 := E053 + 1;
      E055 := E055 + 1;
      E057 := E057 + 1;
      E059 := E059 + 1;
      E066 := E066 + 1;
      E070 := E070 + 1;
      E072 := E072 + 1;
      E079 := E079 + 1;
      E081 := E081 + 1;
      E088 := E088 + 1;
      E083 := E083 + 1;
      E092 := E092 + 1;
      E094 := E094 + 1;
      E096 := E096 + 1;
      E014 := E014 + 1;
      E002 := E002 + 1;
      if E012 = 0 then
         Aegis_Sandbox'Elab_Spec;
      end if;
      E012 := E012 + 1;
      E025 := E025 + 1;
      E027 := E027 + 1;
      E021 := E021 + 1;
      if E061 = 0 then
         Anubis_Lattice_Zk'Elab_Spec;
      end if;
      E061 := E061 + 1;
      E074 := E074 + 1;
      E063 := E063 + 1;
      E047 := E047 + 1;
      E085 := E085 + 1;
      E077 := E077 + 1;
      E098 := E098 + 1;
      E104 := E104 + 1;
      E102 := E102 + 1;
      E051 := E051 + 1;
      E106 := E106 + 1;
      E108 := E108 + 1;
      E111 := E111 + 1;
      E113 := E113 + 1;
      if E119 = 0 then
         Khepri_Mpt_Types'Elab_Spec;
      end if;
      E119 := E119 + 1;
      if E117 = 0 then
         Khepri_Mpt'Elab_Body;
      end if;
      E117 := E117 + 1;
      E006 := E006 + 1;
      E004 := E004 + 1;
      E016 := E016 + 1;
      if E121 = 0 then
         Khepri_Registry'Elab_Body;
      end if;
      E121 := E121 + 1;
      if E125 = 0 then
         Khepri_Types'Elab_Spec;
      end if;
      E125 := E125 + 1;
      if E123 = 0 then
         Khepri_Storage_Trie'Elab_Body;
      end if;
      E123 := E123 + 1;
      E127 := E127 + 1;
      if E129 = 0 then
         Scarab_Horus'Elab_Body;
      end if;
      E129 := E129 + 1;
      E131 := E131 + 1;
      E133 := E133 + 1;
      E135 := E135 + 1;
      if E137 = 0 then
         Scarab_Sekhmet'Elab_Body;
      end if;
      E137 := E137 + 1;
      E139 := E139 + 1;
      E141 := E141 + 1;
      E008 := E008 + 1;
   end aegisvminit;

--  BEGIN Object file/option list
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_vm_types.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_gas.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_u256.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_address_types.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_config.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_field.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mldsa_config.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mldsa_field.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_types.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_chacha20.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mldsa_types.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mlkem_types.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_poly1305.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_chacha20_poly1305.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_stark_field.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_word.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_address_base32.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_bug_bounty.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_bytes.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_cbd.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_certification.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_dead_man_switch.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_developer_rewards.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_genesis_provers.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_genesis_validators.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_governance.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_keccak.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mldsa_encoding.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mldsa_ntt.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mldsa_poly.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mlkem_compress.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mlkem_encoding.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_ntt.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mlkem_poly.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_proof_of_build.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_quantum_insurance.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_sha3.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_storage.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_contract.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_sandbox.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_address_checksum.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_address_derive.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_address.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_lattice_zk.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mldsa_sample.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mldsa.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_eye.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mlkem_sample.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_mlkem.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_shield.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_stark_poly.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_stark_fri.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_gate.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_token.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_treasury.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_vesting.o
   --   /Users/sicarii/anubisvm/core/obj/release/anubis_whisper.o
   --   /Users/sicarii/anubisvm/core/obj/release/khepri_mpt_types.o
   --   /Users/sicarii/anubisvm/core/obj/release/khepri_mpt.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_execution.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_crypto_api.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_syscall.o
   --   /Users/sicarii/anubisvm/core/obj/release/khepri_registry.o
   --   /Users/sicarii/anubisvm/core/obj/release/khepri_types.o
   --   /Users/sicarii/anubisvm/core/obj/release/khepri_storage_trie.o
   --   /Users/sicarii/anubisvm/core/obj/release/scarab_aadkg.o
   --   /Users/sicarii/anubisvm/core/obj/release/scarab_horus.o
   --   /Users/sicarii/anubisvm/core/obj/release/scarab_khnum.o
   --   /Users/sicarii/anubisvm/core/obj/release/scarab_maat.o
   --   /Users/sicarii/anubisvm/core/obj/release/scarab_sebek.o
   --   /Users/sicarii/anubisvm/core/obj/release/scarab_sekhmet.o
   --   /Users/sicarii/anubisvm/core/obj/release/scarab_tefnut.o
   --   /Users/sicarii/anubisvm/core/obj/release/scarab_thoth.o
   --   /Users/sicarii/anubisvm/core/obj/release/aegis_ffi.o
   --   -L/Users/sicarii/anubisvm/core/obj/release/
   --   -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib/
   --   -static
   --   -lgnat
--  END Object file/option list   

end aegisvmmain;
