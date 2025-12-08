pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__test_contract_e2e.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__test_contract_e2e.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E016 : Short_Integer; pragma Import (Ada, E016, "ada__exceptions_E");
   E012 : Short_Integer; pragma Import (Ada, E012, "system__soft_links_E");
   E010 : Short_Integer; pragma Import (Ada, E010, "system__exception_table_E");
   E025 : Short_Integer; pragma Import (Ada, E025, "ada__numerics_E");
   E019 : Short_Integer; pragma Import (Ada, E019, "system__exceptions_E");
   E051 : Short_Integer; pragma Import (Ada, E051, "system__soft_links__initialize_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "ada__io_exceptions_E");
   E007 : Short_Integer; pragma Import (Ada, E007, "ada__strings_E");
   E055 : Short_Integer; pragma Import (Ada, E055, "ada__strings__utf_encoding_E");
   E101 : Short_Integer; pragma Import (Ada, E101, "interfaces__c_E");
   E094 : Short_Integer; pragma Import (Ada, E094, "system__os_lib_E");
   E063 : Short_Integer; pragma Import (Ada, E063, "ada__tags_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__strings__text_buffers_E");
   E079 : Short_Integer; pragma Import (Ada, E079, "ada__streams_E");
   E104 : Short_Integer; pragma Import (Ada, E104, "system__file_control_block_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "system__finalization_root_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "ada__finalization_E");
   E089 : Short_Integer; pragma Import (Ada, E089, "system__file_io_E");
   E077 : Short_Integer; pragma Import (Ada, E077, "ada__text_io_E");

   Sec_Default_Sized_Stacks : array (1 .. 1) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      E077 := E077 - 1;
      declare
         procedure F1;
         pragma Import (Ada, F1, "ada__text_io__finalize_spec");
      begin
         if E077 = 0 then
            F1;
         end if;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "system__file_io__finalize_body");
      begin
         E089 := E089 - 1;
         if E089 = 0 then
            F2;
         end if;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);

   procedure adainit is
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

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 1;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      if E016 = 0 then
         Ada.Exceptions'Elab_Spec;
      end if;
      if E012 = 0 then
         System.Soft_Links'Elab_Spec;
      end if;
      if E010 = 0 then
         System.Exception_Table'Elab_Body;
      end if;
      E010 := E010 + 1;
      if E025 = 0 then
         Ada.Numerics'Elab_Spec;
      end if;
      E025 := E025 + 1;
      if E019 = 0 then
         System.Exceptions'Elab_Spec;
      end if;
      E019 := E019 + 1;
      if E051 = 0 then
         System.Soft_Links.Initialize'Elab_Body;
      end if;
      E051 := E051 + 1;
      E012 := E012 + 1;
      E016 := E016 + 1;
      if E080 = 0 then
         Ada.Io_Exceptions'Elab_Spec;
      end if;
      E080 := E080 + 1;
      if E007 = 0 then
         Ada.Strings'Elab_Spec;
      end if;
      E007 := E007 + 1;
      if E055 = 0 then
         Ada.Strings.Utf_Encoding'Elab_Spec;
      end if;
      E055 := E055 + 1;
      if E101 = 0 then
         Interfaces.C'Elab_Spec;
      end if;
      E101 := E101 + 1;
      if E094 = 0 then
         System.Os_Lib'Elab_Body;
      end if;
      E094 := E094 + 1;
      if E063 = 0 then
         Ada.Tags'Elab_Spec;
      end if;
      if E063 = 0 then
         Ada.Tags'Elab_Body;
      end if;
      E063 := E063 + 1;
      if E006 = 0 then
         Ada.Strings.Text_Buffers'Elab_Spec;
      end if;
      E006 := E006 + 1;
      if E079 = 0 then
         Ada.Streams'Elab_Spec;
      end if;
      E079 := E079 + 1;
      if E104 = 0 then
         System.File_Control_Block'Elab_Spec;
      end if;
      E104 := E104 + 1;
      if E092 = 0 then
         System.Finalization_Root'Elab_Spec;
      end if;
      E092 := E092 + 1;
      if E090 = 0 then
         Ada.Finalization'Elab_Spec;
      end if;
      E090 := E090 + 1;
      if E089 = 0 then
         System.File_Io'Elab_Body;
      end if;
      E089 := E089 + 1;
      if E077 = 0 then
         Ada.Text_Io'Elab_Spec;
      end if;
      if E077 = 0 then
         Ada.Text_Io'Elab_Body;
      end if;
      E077 := E077 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_test_contract_e2e");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      if gnat_argc = 0 then
         gnat_argc := argc;
         gnat_argv := argv;
      end if;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /Users/sicarii/anubisvm/core/obj/tests/test_contract_e2e.o
   --   -L/Users/sicarii/anubisvm/core/obj/tests/
   --   -L/Users/sicarii/anubisvm/core/obj/tests/
   --   -L/Users/sicarii/anubisvm/core/lib/
   --   -L/Users/sicarii/.local/share/alire/toolchains/gnat_native_14.2.1_cc5517d6/lib/gcc/aarch64-apple-darwin23.6.0/14.2.0/adalib/
   --   -shared
   --   -lgnat-14
--  END Object file/option list   

end ada_main;
