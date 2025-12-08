pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package ada_main is

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 14.2.0" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   GNAT_Version_Address : constant System.Address := GNAT_Version'Address;
   pragma Export (C, GNAT_Version_Address, "__gnat_version_address");

   Ada_Main_Program_Name : constant String := "_ada_test_address" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#64eea06c#;
   pragma Export (C, u00001, "test_addressB");
   u00002 : constant Version_32 := 16#30305195#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#6278fccd#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#76789da1#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#f64b89a4#;
   pragma Export (C, u00005, "ada__integer_text_ioB");
   u00006 : constant Version_32 := 16#b4dc53db#;
   pragma Export (C, u00006, "ada__integer_text_ioS");
   u00007 : constant Version_32 := 16#fe032bc8#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#00870947#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#0740df23#;
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   u00010 : constant Version_32 := 16#a028f72d#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   u00011 : constant Version_32 := 16#70765b54#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#fd5f5f4c#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#210214a9#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#524f7d04#;
   pragma Export (C, u00014, "system__secondary_stackB");
   u00015 : constant Version_32 := 16#debd0a58#;
   pragma Export (C, u00015, "system__secondary_stackS");
   u00016 : constant Version_32 := 16#a43efea2#;
   pragma Export (C, u00016, "system__parametersB");
   u00017 : constant Version_32 := 16#45e1a745#;
   pragma Export (C, u00017, "system__parametersS");
   u00018 : constant Version_32 := 16#bca88fbc#;
   pragma Export (C, u00018, "system__storage_elementsS");
   u00019 : constant Version_32 := 16#0286ce9f#;
   pragma Export (C, u00019, "system__soft_links__initializeB");
   u00020 : constant Version_32 := 16#2ed17187#;
   pragma Export (C, u00020, "system__soft_links__initializeS");
   u00021 : constant Version_32 := 16#8599b27b#;
   pragma Export (C, u00021, "system__stack_checkingB");
   u00022 : constant Version_32 := 16#b7294e42#;
   pragma Export (C, u00022, "system__stack_checkingS");
   u00023 : constant Version_32 := 16#c71e6c8a#;
   pragma Export (C, u00023, "system__exception_tableB");
   u00024 : constant Version_32 := 16#fd5d2d4d#;
   pragma Export (C, u00024, "system__exception_tableS");
   u00025 : constant Version_32 := 16#42d3e466#;
   pragma Export (C, u00025, "system__exceptionsS");
   u00026 : constant Version_32 := 16#69416224#;
   pragma Export (C, u00026, "system__exceptions__machineB");
   u00027 : constant Version_32 := 16#46355a4a#;
   pragma Export (C, u00027, "system__exceptions__machineS");
   u00028 : constant Version_32 := 16#7706238d#;
   pragma Export (C, u00028, "system__exceptions_debugB");
   u00029 : constant Version_32 := 16#40780307#;
   pragma Export (C, u00029, "system__exceptions_debugS");
   u00030 : constant Version_32 := 16#52e91815#;
   pragma Export (C, u00030, "system__img_intS");
   u00031 : constant Version_32 := 16#f2c63a02#;
   pragma Export (C, u00031, "ada__numericsS");
   u00032 : constant Version_32 := 16#174f5472#;
   pragma Export (C, u00032, "ada__numerics__big_numbersS");
   u00033 : constant Version_32 := 16#8a5c240d#;
   pragma Export (C, u00033, "system__unsigned_typesS");
   u00034 : constant Version_32 := 16#5c7d9c20#;
   pragma Export (C, u00034, "system__tracebackB");
   u00035 : constant Version_32 := 16#f6ecafe9#;
   pragma Export (C, u00035, "system__tracebackS");
   u00036 : constant Version_32 := 16#5f6b6486#;
   pragma Export (C, u00036, "system__traceback_entriesB");
   u00037 : constant Version_32 := 16#b86ae4d8#;
   pragma Export (C, u00037, "system__traceback_entriesS");
   u00038 : constant Version_32 := 16#a0281f47#;
   pragma Export (C, u00038, "system__traceback__symbolicB");
   u00039 : constant Version_32 := 16#140ceb78#;
   pragma Export (C, u00039, "system__traceback__symbolicS");
   u00040 : constant Version_32 := 16#701f9d88#;
   pragma Export (C, u00040, "ada__exceptions__tracebackB");
   u00041 : constant Version_32 := 16#26ed0985#;
   pragma Export (C, u00041, "ada__exceptions__tracebackS");
   u00042 : constant Version_32 := 16#a0d3d22b#;
   pragma Export (C, u00042, "system__address_imageB");
   u00043 : constant Version_32 := 16#d19ac66e#;
   pragma Export (C, u00043, "system__address_imageS");
   u00044 : constant Version_32 := 16#fd158a37#;
   pragma Export (C, u00044, "system__wch_conB");
   u00045 : constant Version_32 := 16#a9757837#;
   pragma Export (C, u00045, "system__wch_conS");
   u00046 : constant Version_32 := 16#5c289972#;
   pragma Export (C, u00046, "system__wch_stwB");
   u00047 : constant Version_32 := 16#84645436#;
   pragma Export (C, u00047, "system__wch_stwS");
   u00048 : constant Version_32 := 16#7cd63de5#;
   pragma Export (C, u00048, "system__wch_cnvB");
   u00049 : constant Version_32 := 16#afb5b247#;
   pragma Export (C, u00049, "system__wch_cnvS");
   u00050 : constant Version_32 := 16#9111f9c1#;
   pragma Export (C, u00050, "interfacesS");
   u00051 : constant Version_32 := 16#e538de43#;
   pragma Export (C, u00051, "system__wch_jisB");
   u00052 : constant Version_32 := 16#1a02d06d#;
   pragma Export (C, u00052, "system__wch_jisS");
   u00053 : constant Version_32 := 16#2170d2a2#;
   pragma Export (C, u00053, "ada__text_ioB");
   u00054 : constant Version_32 := 16#6629c04a#;
   pragma Export (C, u00054, "ada__text_ioS");
   u00055 : constant Version_32 := 16#b4f41810#;
   pragma Export (C, u00055, "ada__streamsB");
   u00056 : constant Version_32 := 16#67e31212#;
   pragma Export (C, u00056, "ada__streamsS");
   u00057 : constant Version_32 := 16#367911c4#;
   pragma Export (C, u00057, "ada__io_exceptionsS");
   u00058 : constant Version_32 := 16#a201b8c5#;
   pragma Export (C, u00058, "ada__strings__text_buffersB");
   u00059 : constant Version_32 := 16#a7cfd09b#;
   pragma Export (C, u00059, "ada__strings__text_buffersS");
   u00060 : constant Version_32 := 16#e6d4fa36#;
   pragma Export (C, u00060, "ada__stringsS");
   u00061 : constant Version_32 := 16#8b7604c4#;
   pragma Export (C, u00061, "ada__strings__utf_encodingB");
   u00062 : constant Version_32 := 16#c9e86997#;
   pragma Export (C, u00062, "ada__strings__utf_encodingS");
   u00063 : constant Version_32 := 16#bb780f45#;
   pragma Export (C, u00063, "ada__strings__utf_encoding__stringsB");
   u00064 : constant Version_32 := 16#b85ff4b6#;
   pragma Export (C, u00064, "ada__strings__utf_encoding__stringsS");
   u00065 : constant Version_32 := 16#d1d1ed0b#;
   pragma Export (C, u00065, "ada__strings__utf_encoding__wide_stringsB");
   u00066 : constant Version_32 := 16#5678478f#;
   pragma Export (C, u00066, "ada__strings__utf_encoding__wide_stringsS");
   u00067 : constant Version_32 := 16#c2b98963#;
   pragma Export (C, u00067, "ada__strings__utf_encoding__wide_wide_stringsB");
   u00068 : constant Version_32 := 16#d7af3358#;
   pragma Export (C, u00068, "ada__strings__utf_encoding__wide_wide_stringsS");
   u00069 : constant Version_32 := 16#0d5e09a4#;
   pragma Export (C, u00069, "ada__tagsB");
   u00070 : constant Version_32 := 16#2a9756e0#;
   pragma Export (C, u00070, "ada__tagsS");
   u00071 : constant Version_32 := 16#3548d972#;
   pragma Export (C, u00071, "system__htableB");
   u00072 : constant Version_32 := 16#f1af03bf#;
   pragma Export (C, u00072, "system__htableS");
   u00073 : constant Version_32 := 16#1f1abe38#;
   pragma Export (C, u00073, "system__string_hashB");
   u00074 : constant Version_32 := 16#56ea83c0#;
   pragma Export (C, u00074, "system__string_hashS");
   u00075 : constant Version_32 := 16#e7d0da5b#;
   pragma Export (C, u00075, "system__val_lluS");
   u00076 : constant Version_32 := 16#238798c9#;
   pragma Export (C, u00076, "system__sparkS");
   u00077 : constant Version_32 := 16#a571a4dc#;
   pragma Export (C, u00077, "system__spark__cut_operationsB");
   u00078 : constant Version_32 := 16#629c0fb7#;
   pragma Export (C, u00078, "system__spark__cut_operationsS");
   u00079 : constant Version_32 := 16#1bac5121#;
   pragma Export (C, u00079, "system__val_utilB");
   u00080 : constant Version_32 := 16#dc0fff4f#;
   pragma Export (C, u00080, "system__val_utilS");
   u00081 : constant Version_32 := 16#b98923bf#;
   pragma Export (C, u00081, "system__case_utilB");
   u00082 : constant Version_32 := 16#bf658c01#;
   pragma Export (C, u00082, "system__case_utilS");
   u00083 : constant Version_32 := 16#05222263#;
   pragma Export (C, u00083, "system__put_imagesB");
   u00084 : constant Version_32 := 16#6cd85c4b#;
   pragma Export (C, u00084, "system__put_imagesS");
   u00085 : constant Version_32 := 16#22b9eb9f#;
   pragma Export (C, u00085, "ada__strings__text_buffers__utilsB");
   u00086 : constant Version_32 := 16#89062ac3#;
   pragma Export (C, u00086, "ada__strings__text_buffers__utilsS");
   u00087 : constant Version_32 := 16#1cacf006#;
   pragma Export (C, u00087, "interfaces__c_streamsB");
   u00088 : constant Version_32 := 16#d07279c2#;
   pragma Export (C, u00088, "interfaces__c_streamsS");
   u00089 : constant Version_32 := 16#fb523cdb#;
   pragma Export (C, u00089, "system__crtlS");
   u00090 : constant Version_32 := 16#f74fab1c#;
   pragma Export (C, u00090, "system__file_ioB");
   u00091 : constant Version_32 := 16#16390e12#;
   pragma Export (C, u00091, "system__file_ioS");
   u00092 : constant Version_32 := 16#86c56e5a#;
   pragma Export (C, u00092, "ada__finalizationS");
   u00093 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00093, "system__finalization_rootB");
   u00094 : constant Version_32 := 16#3f8428c4#;
   pragma Export (C, u00094, "system__finalization_rootS");
   u00095 : constant Version_32 := 16#29c68ba2#;
   pragma Export (C, u00095, "system__os_libB");
   u00096 : constant Version_32 := 16#8a1a8b0b#;
   pragma Export (C, u00096, "system__os_libS");
   u00097 : constant Version_32 := 16#94d23d25#;
   pragma Export (C, u00097, "system__atomic_operations__test_and_setB");
   u00098 : constant Version_32 := 16#57acee8e#;
   pragma Export (C, u00098, "system__atomic_operations__test_and_setS");
   u00099 : constant Version_32 := 16#b7152171#;
   pragma Export (C, u00099, "system__atomic_operationsS");
   u00100 : constant Version_32 := 16#553a519e#;
   pragma Export (C, u00100, "system__atomic_primitivesB");
   u00101 : constant Version_32 := 16#3b295013#;
   pragma Export (C, u00101, "system__atomic_primitivesS");
   u00102 : constant Version_32 := 16#0390ef72#;
   pragma Export (C, u00102, "interfaces__cB");
   u00103 : constant Version_32 := 16#7e33484a#;
   pragma Export (C, u00103, "interfaces__cS");
   u00104 : constant Version_32 := 16#256dbbe5#;
   pragma Export (C, u00104, "system__stringsB");
   u00105 : constant Version_32 := 16#ebf45b4c#;
   pragma Export (C, u00105, "system__stringsS");
   u00106 : constant Version_32 := 16#fcdf3530#;
   pragma Export (C, u00106, "system__file_control_blockS");
   u00107 : constant Version_32 := 16#5e511f79#;
   pragma Export (C, u00107, "ada__text_io__generic_auxB");
   u00108 : constant Version_32 := 16#d2ac8a2d#;
   pragma Export (C, u00108, "ada__text_io__generic_auxS");
   u00109 : constant Version_32 := 16#b981d8aa#;
   pragma Export (C, u00109, "system__img_biuS");
   u00110 : constant Version_32 := 16#f4df1f74#;
   pragma Export (C, u00110, "system__img_llbS");
   u00111 : constant Version_32 := 16#3ab08e6e#;
   pragma Export (C, u00111, "system__img_lliS");
   u00112 : constant Version_32 := 16#832eea06#;
   pragma Export (C, u00112, "system__img_lllbS");
   u00113 : constant Version_32 := 16#c9d8ed88#;
   pragma Export (C, u00113, "system__img_llliS");
   u00114 : constant Version_32 := 16#895af30a#;
   pragma Export (C, u00114, "system__img_lllwS");
   u00115 : constant Version_32 := 16#a8ed6a7f#;
   pragma Export (C, u00115, "system__img_llwS");
   u00116 : constant Version_32 := 16#865b6398#;
   pragma Export (C, u00116, "system__img_wiuS");
   u00117 : constant Version_32 := 16#ce5f50f9#;
   pragma Export (C, u00117, "system__val_intS");
   u00118 : constant Version_32 := 16#39f8db91#;
   pragma Export (C, u00118, "system__val_unsS");
   u00119 : constant Version_32 := 16#111e58d8#;
   pragma Export (C, u00119, "system__val_lliS");
   u00120 : constant Version_32 := 16#c1a0d3c0#;
   pragma Export (C, u00120, "system__val_llliS");
   u00121 : constant Version_32 := 16#7a141c22#;
   pragma Export (C, u00121, "system__val_llluS");
   u00122 : constant Version_32 := 16#fb144c02#;
   pragma Export (C, u00122, "anubis_addressB");
   u00123 : constant Version_32 := 16#1f3e67aa#;
   pragma Export (C, u00123, "anubis_addressS");
   u00124 : constant Version_32 := 16#a3f1a165#;
   pragma Export (C, u00124, "anubis_address_base32B");
   u00125 : constant Version_32 := 16#14a2b86d#;
   pragma Export (C, u00125, "anubis_address_base32S");
   u00126 : constant Version_32 := 16#e259c480#;
   pragma Export (C, u00126, "system__assertionsB");
   u00127 : constant Version_32 := 16#567524cf#;
   pragma Export (C, u00127, "system__assertionsS");
   u00128 : constant Version_32 := 16#8b2c6428#;
   pragma Export (C, u00128, "ada__assertionsB");
   u00129 : constant Version_32 := 16#cc3ec2fd#;
   pragma Export (C, u00129, "ada__assertionsS");
   u00130 : constant Version_32 := 16#ac6d54ca#;
   pragma Export (C, u00130, "anubis_address_typesS");
   u00131 : constant Version_32 := 16#40bd80eb#;
   pragma Export (C, u00131, "anubis_address_checksumB");
   u00132 : constant Version_32 := 16#3cd5af54#;
   pragma Export (C, u00132, "anubis_address_checksumS");
   u00133 : constant Version_32 := 16#836bf0a8#;
   pragma Export (C, u00133, "anubis_sha3B");
   u00134 : constant Version_32 := 16#95c7bdbf#;
   pragma Export (C, u00134, "anubis_sha3S");
   u00135 : constant Version_32 := 16#68d297b3#;
   pragma Export (C, u00135, "anubis_keccakB");
   u00136 : constant Version_32 := 16#88a5c868#;
   pragma Export (C, u00136, "anubis_keccakS");
   u00137 : constant Version_32 := 16#dea2a956#;
   pragma Export (C, u00137, "anubis_typesS");
   u00138 : constant Version_32 := 16#411a8f62#;
   pragma Export (C, u00138, "anubis_address_deriveB");
   u00139 : constant Version_32 := 16#f2f66bf7#;
   pragma Export (C, u00139, "anubis_address_deriveS");
   u00140 : constant Version_32 := 16#ca878138#;
   pragma Export (C, u00140, "system__concat_2B");
   u00141 : constant Version_32 := 16#c58d28a3#;
   pragma Export (C, u00141, "system__concat_2S");
   u00142 : constant Version_32 := 16#752a67ed#;
   pragma Export (C, u00142, "system__concat_3B");
   u00143 : constant Version_32 := 16#fa0c42f6#;
   pragma Export (C, u00143, "system__concat_3S");
   u00144 : constant Version_32 := 16#8f2423cb#;
   pragma Export (C, u00144, "system__memoryB");
   u00145 : constant Version_32 := 16#68e2c74e#;
   pragma Export (C, u00145, "system__memoryS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.atomic_operations%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.spark%s
   --  system.spark.cut_operations%s
   --  system.spark.cut_operations%b
   --  system.storage_elements%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%s
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.img_biu%s
   --  system.img_llb%s
   --  system.img_lllb%s
   --  system.img_lllw%s
   --  system.img_llw%s
   --  system.img_wiu%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
   --  system.concat_2%s
   --  system.concat_2%b
   --  system.concat_3%s
   --  system.concat_3%b
   --  system.traceback%s
   --  system.traceback%b
   --  system.secondary_stack%s
   --  system.standard_library%s
   --  ada.exceptions%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.soft_links%s
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  ada.exceptions.traceback%s
   --  ada.exceptions.traceback%b
   --  system.address_image%s
   --  system.address_image%b
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.numerics%s
   --  ada.numerics.big_numbers%s
   --  system.exceptions%s
   --  system.exceptions.machine%s
   --  system.exceptions.machine%b
   --  system.img_int%s
   --  system.memory%s
   --  system.memory%b
   --  system.secondary_stack%b
   --  system.soft_links.initialize%s
   --  system.soft_links.initialize%b
   --  system.soft_links%b
   --  system.standard_library%b
   --  system.traceback.symbolic%s
   --  system.traceback.symbolic%b
   --  ada.exceptions%b
   --  ada.assertions%s
   --  ada.assertions%b
   --  ada.io_exceptions%s
   --  ada.strings%s
   --  ada.strings.utf_encoding%s
   --  ada.strings.utf_encoding%b
   --  ada.strings.utf_encoding.strings%s
   --  ada.strings.utf_encoding.strings%b
   --  ada.strings.utf_encoding.wide_strings%s
   --  ada.strings.utf_encoding.wide_strings%b
   --  ada.strings.utf_encoding.wide_wide_strings%s
   --  ada.strings.utf_encoding.wide_wide_strings%b
   --  interfaces.c%s
   --  interfaces.c%b
   --  system.atomic_primitives%s
   --  system.atomic_primitives%b
   --  system.atomic_operations.test_and_set%s
   --  system.atomic_operations.test_and_set%b
   --  system.case_util%s
   --  system.case_util%b
   --  system.os_lib%s
   --  system.os_lib%b
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_lllu%s
   --  system.val_llli%s
   --  system.val_llu%s
   --  ada.tags%s
   --  ada.tags%b
   --  ada.strings.text_buffers%s
   --  ada.strings.text_buffers%b
   --  ada.strings.text_buffers.utils%s
   --  ada.strings.text_buffers.utils%b
   --  system.put_images%s
   --  system.put_images%b
   --  ada.streams%s
   --  ada.streams%b
   --  system.file_control_block%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  system.file_io%s
   --  system.file_io%b
   --  system.val_lli%s
   --  system.val_uns%s
   --  system.val_int%s
   --  ada.text_io%s
   --  ada.text_io%b
   --  ada.text_io.generic_aux%s
   --  ada.text_io.generic_aux%b
   --  system.assertions%s
   --  system.assertions%b
   --  system.img_lli%s
   --  system.img_llli%s
   --  ada.integer_text_io%s
   --  ada.integer_text_io%b
   --  anubis_address_types%s
   --  anubis_types%s
   --  anubis_address_base32%s
   --  anubis_address_base32%b
   --  anubis_keccak%s
   --  anubis_keccak%b
   --  anubis_sha3%s
   --  anubis_sha3%b
   --  anubis_address_checksum%s
   --  anubis_address_checksum%b
   --  anubis_address_derive%s
   --  anubis_address_derive%b
   --  anubis_address%s
   --  anubis_address%b
   --  test_address%b
   --  END ELABORATION ORDER

end ada_main;
