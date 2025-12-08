pragma Warnings (Off);
pragma Ada_95;
with System;
with System.Parameters;
with System.Secondary_Stack;
package aegisvmmain is

   procedure aegisvminit;
   pragma Export (C, aegisvminit, "aegisvminit");
   pragma Linker_Constructor (aegisvminit);

   procedure aegisvmfinal;
   pragma Export (C, aegisvmfinal, "aegisvmfinal");
   pragma Linker_Destructor (aegisvmfinal);

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#1e96d54b#;
   pragma Export (C, u00001, "aegis_contractB");
   u00002 : constant Version_32 := 16#5fafd5de#;
   pragma Export (C, u00002, "aegis_contractS");
   u00003 : constant Version_32 := 16#2f41855d#;
   pragma Export (C, u00003, "aegis_crypto_apiB");
   u00004 : constant Version_32 := 16#96e8a060#;
   pragma Export (C, u00004, "aegis_crypto_apiS");
   u00005 : constant Version_32 := 16#6136b6c6#;
   pragma Export (C, u00005, "aegis_executionB");
   u00006 : constant Version_32 := 16#07e7bc30#;
   pragma Export (C, u00006, "aegis_executionS");
   u00007 : constant Version_32 := 16#ad1da994#;
   pragma Export (C, u00007, "aegis_ffiB");
   u00008 : constant Version_32 := 16#bf8993fa#;
   pragma Export (C, u00008, "aegis_ffiS");
   u00009 : constant Version_32 := 16#b10d79e2#;
   pragma Export (C, u00009, "aegis_gasB");
   u00010 : constant Version_32 := 16#5e0332b5#;
   pragma Export (C, u00010, "aegis_gasS");
   u00011 : constant Version_32 := 16#7d6bbf1a#;
   pragma Export (C, u00011, "aegis_sandboxB");
   u00012 : constant Version_32 := 16#5f12e4dc#;
   pragma Export (C, u00012, "aegis_sandboxS");
   u00013 : constant Version_32 := 16#6efd4be7#;
   pragma Export (C, u00013, "aegis_storageB");
   u00014 : constant Version_32 := 16#aba92f60#;
   pragma Export (C, u00014, "aegis_storageS");
   u00015 : constant Version_32 := 16#65cf1bfe#;
   pragma Export (C, u00015, "aegis_syscallB");
   u00016 : constant Version_32 := 16#ce3fb1fe#;
   pragma Export (C, u00016, "aegis_syscallS");
   u00017 : constant Version_32 := 16#901bfeda#;
   pragma Export (C, u00017, "aegis_u256B");
   u00018 : constant Version_32 := 16#b2ed0653#;
   pragma Export (C, u00018, "aegis_u256S");
   u00019 : constant Version_32 := 16#4dc0f818#;
   pragma Export (C, u00019, "aegis_vm_typesS");
   u00020 : constant Version_32 := 16#dbc4ba97#;
   pragma Export (C, u00020, "anubis_addressB");
   u00021 : constant Version_32 := 16#d71534e3#;
   pragma Export (C, u00021, "anubis_addressS");
   u00022 : constant Version_32 := 16#90e5681e#;
   pragma Export (C, u00022, "anubis_address_base32B");
   u00023 : constant Version_32 := 16#f824be28#;
   pragma Export (C, u00023, "anubis_address_base32S");
   u00024 : constant Version_32 := 16#eaa3575e#;
   pragma Export (C, u00024, "anubis_address_checksumB");
   u00025 : constant Version_32 := 16#8280df96#;
   pragma Export (C, u00025, "anubis_address_checksumS");
   u00026 : constant Version_32 := 16#8c23543e#;
   pragma Export (C, u00026, "anubis_address_deriveB");
   u00027 : constant Version_32 := 16#635d4dfd#;
   pragma Export (C, u00027, "anubis_address_deriveS");
   u00028 : constant Version_32 := 16#73ff9bf4#;
   pragma Export (C, u00028, "anubis_address_typesS");
   u00029 : constant Version_32 := 16#88f42651#;
   pragma Export (C, u00029, "anubis_cbdB");
   u00030 : constant Version_32 := 16#027471d7#;
   pragma Export (C, u00030, "anubis_cbdS");
   u00031 : constant Version_32 := 16#19c7b4b2#;
   pragma Export (C, u00031, "anubis_configS");
   u00032 : constant Version_32 := 16#5ca17633#;
   pragma Export (C, u00032, "anubis_fieldB");
   u00033 : constant Version_32 := 16#44178e5a#;
   pragma Export (C, u00033, "anubis_fieldS");
   u00034 : constant Version_32 := 16#68d297b3#;
   pragma Export (C, u00034, "anubis_keccakB");
   u00035 : constant Version_32 := 16#88a5c868#;
   pragma Export (C, u00035, "anubis_keccakS");
   u00036 : constant Version_32 := 16#a5eaf66f#;
   pragma Export (C, u00036, "anubis_mldsaB");
   u00037 : constant Version_32 := 16#204626f6#;
   pragma Export (C, u00037, "anubis_mldsaS");
   u00038 : constant Version_32 := 16#21404e58#;
   pragma Export (C, u00038, "anubis_mldsa_configS");
   u00039 : constant Version_32 := 16#6ab7cbb5#;
   pragma Export (C, u00039, "anubis_mldsa_encodingB");
   u00040 : constant Version_32 := 16#4d04fdb4#;
   pragma Export (C, u00040, "anubis_mldsa_encodingS");
   u00041 : constant Version_32 := 16#385b5656#;
   pragma Export (C, u00041, "anubis_mldsa_fieldB");
   u00042 : constant Version_32 := 16#057b1d52#;
   pragma Export (C, u00042, "anubis_mldsa_fieldS");
   u00043 : constant Version_32 := 16#466f55ae#;
   pragma Export (C, u00043, "anubis_mldsa_nttB");
   u00044 : constant Version_32 := 16#841d40ff#;
   pragma Export (C, u00044, "anubis_mldsa_nttS");
   u00045 : constant Version_32 := 16#2ed11c01#;
   pragma Export (C, u00045, "anubis_mldsa_polyB");
   u00046 : constant Version_32 := 16#4731d7ba#;
   pragma Export (C, u00046, "anubis_mldsa_polyS");
   u00047 : constant Version_32 := 16#3358749c#;
   pragma Export (C, u00047, "anubis_mldsa_sampleB");
   u00048 : constant Version_32 := 16#ea1029ab#;
   pragma Export (C, u00048, "anubis_mldsa_sampleS");
   u00049 : constant Version_32 := 16#6b544876#;
   pragma Export (C, u00049, "anubis_mldsa_typesS");
   u00050 : constant Version_32 := 16#6d057dff#;
   pragma Export (C, u00050, "anubis_mlkemB");
   u00051 : constant Version_32 := 16#0fe851b9#;
   pragma Export (C, u00051, "anubis_mlkemS");
   u00052 : constant Version_32 := 16#3175d113#;
   pragma Export (C, u00052, "anubis_mlkem_compressB");
   u00053 : constant Version_32 := 16#68bed3d7#;
   pragma Export (C, u00053, "anubis_mlkem_compressS");
   u00054 : constant Version_32 := 16#e2f39be5#;
   pragma Export (C, u00054, "anubis_mlkem_encodingB");
   u00055 : constant Version_32 := 16#049a9ea1#;
   pragma Export (C, u00055, "anubis_mlkem_encodingS");
   u00056 : constant Version_32 := 16#061e731f#;
   pragma Export (C, u00056, "anubis_mlkem_polyB");
   u00057 : constant Version_32 := 16#a4cdd192#;
   pragma Export (C, u00057, "anubis_mlkem_polyS");
   u00058 : constant Version_32 := 16#5d133190#;
   pragma Export (C, u00058, "anubis_mlkem_sampleB");
   u00059 : constant Version_32 := 16#32f9574c#;
   pragma Export (C, u00059, "anubis_mlkem_sampleS");
   u00060 : constant Version_32 := 16#4763c32a#;
   pragma Export (C, u00060, "anubis_mlkem_typesS");
   u00061 : constant Version_32 := 16#7c55654e#;
   pragma Export (C, u00061, "anubis_nttB");
   u00062 : constant Version_32 := 16#d34de1b7#;
   pragma Export (C, u00062, "anubis_nttS");
   u00063 : constant Version_32 := 16#1f5fce7b#;
   pragma Export (C, u00063, "anubis_sha3B");
   u00064 : constant Version_32 := 16#06c20106#;
   pragma Export (C, u00064, "anubis_sha3S");
   u00065 : constant Version_32 := 16#dea2a956#;
   pragma Export (C, u00065, "anubis_typesS");

   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  system.storage_elements%s
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  system.unsigned_types%s
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%s
   --  system.wch_cnv%b
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
   --  interfaces.c%s
   --  interfaces.c%b
   --  interfaces.c.strings%s
   --  interfaces.c.strings%b
   --  system.assertions%s
   --  system.assertions%b
   --  aegis_vm_types%s
   --  aegis_gas%s
   --  aegis_gas%b
   --  aegis_u256%s
   --  aegis_u256%b
   --  anubis_address_types%s
   --  anubis_config%s
   --  anubis_field%s
   --  anubis_field%b
   --  anubis_mldsa_config%s
   --  anubis_mldsa_field%s
   --  anubis_mldsa_field%b
   --  anubis_types%s
   --  anubis_mldsa_types%s
   --  anubis_mlkem_types%s
   --  aegis_storage%s
   --  aegis_storage%b
   --  aegis_contract%s
   --  aegis_contract%b
   --  aegis_sandbox%s
   --  aegis_sandbox%b
   --  aegis_execution%s
   --  aegis_execution%b
   --  anubis_address_base32%s
   --  anubis_address_base32%b
   --  anubis_cbd%s
   --  anubis_cbd%b
   --  anubis_keccak%s
   --  anubis_keccak%b
   --  anubis_mldsa_encoding%s
   --  anubis_mldsa_encoding%b
   --  anubis_mldsa_ntt%s
   --  anubis_mldsa_ntt%b
   --  anubis_mldsa_poly%s
   --  anubis_mldsa_poly%b
   --  anubis_mlkem_compress%s
   --  anubis_mlkem_compress%b
   --  anubis_mlkem_encoding%s
   --  anubis_mlkem_encoding%b
   --  anubis_ntt%s
   --  anubis_ntt%b
   --  anubis_mlkem_poly%s
   --  anubis_mlkem_poly%b
   --  anubis_sha3%s
   --  anubis_sha3%b
   --  aegis_ffi%s
   --  aegis_ffi%b
   --  anubis_address_checksum%s
   --  anubis_address_checksum%b
   --  anubis_address_derive%s
   --  anubis_address_derive%b
   --  anubis_address%s
   --  anubis_address%b
   --  anubis_mldsa_sample%s
   --  anubis_mldsa_sample%b
   --  anubis_mldsa%s
   --  anubis_mldsa%b
   --  anubis_mlkem_sample%s
   --  anubis_mlkem_sample%b
   --  anubis_mlkem%s
   --  anubis_mlkem%b
   --  aegis_crypto_api%s
   --  aegis_crypto_api%b
   --  aegis_syscall%s
   --  aegis_syscall%b
   --  END ELABORATION ORDER

end aegisvmmain;
