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
   u00002 : constant Version_32 := 16#fefd3fb7#;
   pragma Export (C, u00002, "aegis_contractS");
   u00003 : constant Version_32 := 16#3653b7b7#;
   pragma Export (C, u00003, "aegis_crypto_apiB");
   u00004 : constant Version_32 := 16#4fac3ec3#;
   pragma Export (C, u00004, "aegis_crypto_apiS");
   u00005 : constant Version_32 := 16#eb3e624d#;
   pragma Export (C, u00005, "aegis_executionB");
   u00006 : constant Version_32 := 16#2f3b57ff#;
   pragma Export (C, u00006, "aegis_executionS");
   u00007 : constant Version_32 := 16#8b79870b#;
   pragma Export (C, u00007, "aegis_ffiB");
   u00008 : constant Version_32 := 16#8e7d511d#;
   pragma Export (C, u00008, "aegis_ffiS");
   u00009 : constant Version_32 := 16#05f00f54#;
   pragma Export (C, u00009, "aegis_gasB");
   u00010 : constant Version_32 := 16#f0e872dc#;
   pragma Export (C, u00010, "aegis_gasS");
   u00011 : constant Version_32 := 16#834a6076#;
   pragma Export (C, u00011, "aegis_sandboxB");
   u00012 : constant Version_32 := 16#0061d1d9#;
   pragma Export (C, u00012, "aegis_sandboxS");
   u00013 : constant Version_32 := 16#940a855e#;
   pragma Export (C, u00013, "aegis_storageB");
   u00014 : constant Version_32 := 16#0afbc509#;
   pragma Export (C, u00014, "aegis_storageS");
   u00015 : constant Version_32 := 16#7c7e6d57#;
   pragma Export (C, u00015, "aegis_syscallB");
   u00016 : constant Version_32 := 16#f4af08c0#;
   pragma Export (C, u00016, "aegis_syscallS");
   u00017 : constant Version_32 := 16#cfb9f9d9#;
   pragma Export (C, u00017, "aegis_u256B");
   u00018 : constant Version_32 := 16#b2ed0653#;
   pragma Export (C, u00018, "aegis_u256S");
   u00019 : constant Version_32 := 16#4dc0f818#;
   pragma Export (C, u00019, "aegis_vm_typesS");
   u00020 : constant Version_32 := 16#09f5e292#;
   pragma Export (C, u00020, "anubis_addressB");
   u00021 : constant Version_32 := 16#4c246da6#;
   pragma Export (C, u00021, "anubis_addressS");
   u00022 : constant Version_32 := 16#0383624d#;
   pragma Export (C, u00022, "anubis_address_base32B");
   u00023 : constant Version_32 := 16#412e45bb#;
   pragma Export (C, u00023, "anubis_address_base32S");
   u00024 : constant Version_32 := 16#d3154af3#;
   pragma Export (C, u00024, "anubis_address_checksumB");
   u00025 : constant Version_32 := 16#8280df96#;
   pragma Export (C, u00025, "anubis_address_checksumS");
   u00026 : constant Version_32 := 16#fc0fedb6#;
   pragma Export (C, u00026, "anubis_address_deriveB");
   u00027 : constant Version_32 := 16#635d4dfd#;
   pragma Export (C, u00027, "anubis_address_deriveS");
   u00028 : constant Version_32 := 16#73ff9bf4#;
   pragma Export (C, u00028, "anubis_address_typesS");
   u00029 : constant Version_32 := 16#c05471ad#;
   pragma Export (C, u00029, "anubis_bug_bountyB");
   u00030 : constant Version_32 := 16#92e803a6#;
   pragma Export (C, u00030, "anubis_bug_bountyS");
   u00031 : constant Version_32 := 16#b811d9f9#;
   pragma Export (C, u00031, "anubis_bytesB");
   u00032 : constant Version_32 := 16#acbb0390#;
   pragma Export (C, u00032, "anubis_bytesS");
   u00033 : constant Version_32 := 16#88f42651#;
   pragma Export (C, u00033, "anubis_cbdB");
   u00034 : constant Version_32 := 16#027471d7#;
   pragma Export (C, u00034, "anubis_cbdS");
   u00035 : constant Version_32 := 16#a9509f79#;
   pragma Export (C, u00035, "anubis_certificationB");
   u00036 : constant Version_32 := 16#b91dcb40#;
   pragma Export (C, u00036, "anubis_certificationS");
   u00037 : constant Version_32 := 16#c9dac2bb#;
   pragma Export (C, u00037, "anubis_chacha20B");
   u00038 : constant Version_32 := 16#0fb3db30#;
   pragma Export (C, u00038, "anubis_chacha20S");
   u00039 : constant Version_32 := 16#a6a4c630#;
   pragma Export (C, u00039, "anubis_chacha20_poly1305B");
   u00040 : constant Version_32 := 16#cc228313#;
   pragma Export (C, u00040, "anubis_chacha20_poly1305S");
   u00041 : constant Version_32 := 16#19c7b4b2#;
   pragma Export (C, u00041, "anubis_configS");
   u00042 : constant Version_32 := 16#6dae4d6d#;
   pragma Export (C, u00042, "anubis_dead_man_switchB");
   u00043 : constant Version_32 := 16#45de4070#;
   pragma Export (C, u00043, "anubis_dead_man_switchS");
   u00044 : constant Version_32 := 16#bc45ef90#;
   pragma Export (C, u00044, "anubis_developer_rewardsB");
   u00045 : constant Version_32 := 16#f9966e8a#;
   pragma Export (C, u00045, "anubis_developer_rewardsS");
   u00046 : constant Version_32 := 16#09782255#;
   pragma Export (C, u00046, "anubis_eyeB");
   u00047 : constant Version_32 := 16#fe972430#;
   pragma Export (C, u00047, "anubis_eyeS");
   u00048 : constant Version_32 := 16#5ca17633#;
   pragma Export (C, u00048, "anubis_fieldB");
   u00049 : constant Version_32 := 16#44178e5a#;
   pragma Export (C, u00049, "anubis_fieldS");
   u00050 : constant Version_32 := 16#563d5644#;
   pragma Export (C, u00050, "anubis_gateB");
   u00051 : constant Version_32 := 16#ded2b630#;
   pragma Export (C, u00051, "anubis_gateS");
   u00052 : constant Version_32 := 16#2d2196d7#;
   pragma Export (C, u00052, "anubis_genesis_proversB");
   u00053 : constant Version_32 := 16#72c64f9a#;
   pragma Export (C, u00053, "anubis_genesis_proversS");
   u00054 : constant Version_32 := 16#4c38163d#;
   pragma Export (C, u00054, "anubis_genesis_validatorsB");
   u00055 : constant Version_32 := 16#c18559bd#;
   pragma Export (C, u00055, "anubis_genesis_validatorsS");
   u00056 : constant Version_32 := 16#8a7a0de8#;
   pragma Export (C, u00056, "anubis_governanceB");
   u00057 : constant Version_32 := 16#37371a4e#;
   pragma Export (C, u00057, "anubis_governanceS");
   u00058 : constant Version_32 := 16#68d297b3#;
   pragma Export (C, u00058, "anubis_keccakB");
   u00059 : constant Version_32 := 16#88a5c868#;
   pragma Export (C, u00059, "anubis_keccakS");
   u00060 : constant Version_32 := 16#3fa17c03#;
   pragma Export (C, u00060, "anubis_lattice_zkB");
   u00061 : constant Version_32 := 16#40750357#;
   pragma Export (C, u00061, "anubis_lattice_zkS");
   u00062 : constant Version_32 := 16#32e8506f#;
   pragma Export (C, u00062, "anubis_mldsaB");
   u00063 : constant Version_32 := 16#204626f6#;
   pragma Export (C, u00063, "anubis_mldsaS");
   u00064 : constant Version_32 := 16#44e9e9a1#;
   pragma Export (C, u00064, "anubis_mldsa_configS");
   u00065 : constant Version_32 := 16#6ab7cbb5#;
   pragma Export (C, u00065, "anubis_mldsa_encodingB");
   u00066 : constant Version_32 := 16#28ad5a4d#;
   pragma Export (C, u00066, "anubis_mldsa_encodingS");
   u00067 : constant Version_32 := 16#385b5656#;
   pragma Export (C, u00067, "anubis_mldsa_fieldB");
   u00068 : constant Version_32 := 16#60d2baab#;
   pragma Export (C, u00068, "anubis_mldsa_fieldS");
   u00069 : constant Version_32 := 16#23c6f257#;
   pragma Export (C, u00069, "anubis_mldsa_nttB");
   u00070 : constant Version_32 := 16#841d40ff#;
   pragma Export (C, u00070, "anubis_mldsa_nttS");
   u00071 : constant Version_32 := 16#2ed11c01#;
   pragma Export (C, u00071, "anubis_mldsa_polyB");
   u00072 : constant Version_32 := 16#22987043#;
   pragma Export (C, u00072, "anubis_mldsa_polyS");
   u00073 : constant Version_32 := 16#a75c1567#;
   pragma Export (C, u00073, "anubis_mldsa_sampleB");
   u00074 : constant Version_32 := 16#44a49206#;
   pragma Export (C, u00074, "anubis_mldsa_sampleS");
   u00075 : constant Version_32 := 16#0efdef8f#;
   pragma Export (C, u00075, "anubis_mldsa_typesS");
   u00076 : constant Version_32 := 16#da94e8b8#;
   pragma Export (C, u00076, "anubis_mlkemB");
   u00077 : constant Version_32 := 16#0fe851b9#;
   pragma Export (C, u00077, "anubis_mlkemS");
   u00078 : constant Version_32 := 16#ed97372f#;
   pragma Export (C, u00078, "anubis_mlkem_compressB");
   u00079 : constant Version_32 := 16#b3aa765d#;
   pragma Export (C, u00079, "anubis_mlkem_compressS");
   u00080 : constant Version_32 := 16#e2f39be5#;
   pragma Export (C, u00080, "anubis_mlkem_encodingB");
   u00081 : constant Version_32 := 16#049a9ea1#;
   pragma Export (C, u00081, "anubis_mlkem_encodingS");
   u00082 : constant Version_32 := 16#06718b21#;
   pragma Export (C, u00082, "anubis_mlkem_polyB");
   u00083 : constant Version_32 := 16#1754d7ec#;
   pragma Export (C, u00083, "anubis_mlkem_polyS");
   u00084 : constant Version_32 := 16#52a40800#;
   pragma Export (C, u00084, "anubis_mlkem_sampleB");
   u00085 : constant Version_32 := 16#a06e6e04#;
   pragma Export (C, u00085, "anubis_mlkem_sampleS");
   u00086 : constant Version_32 := 16#4763c32a#;
   pragma Export (C, u00086, "anubis_mlkem_typesS");
   u00087 : constant Version_32 := 16#7c55654e#;
   pragma Export (C, u00087, "anubis_nttB");
   u00088 : constant Version_32 := 16#d34de1b7#;
   pragma Export (C, u00088, "anubis_nttS");
   u00089 : constant Version_32 := 16#e7692bd5#;
   pragma Export (C, u00089, "anubis_poly1305B");
   u00090 : constant Version_32 := 16#47e519b3#;
   pragma Export (C, u00090, "anubis_poly1305S");
   u00091 : constant Version_32 := 16#aaba3ee3#;
   pragma Export (C, u00091, "anubis_proof_of_buildB");
   u00092 : constant Version_32 := 16#70808846#;
   pragma Export (C, u00092, "anubis_proof_of_buildS");
   u00093 : constant Version_32 := 16#417feaf3#;
   pragma Export (C, u00093, "anubis_quantum_insuranceB");
   u00094 : constant Version_32 := 16#8471d094#;
   pragma Export (C, u00094, "anubis_quantum_insuranceS");
   u00095 : constant Version_32 := 16#b73b8681#;
   pragma Export (C, u00095, "anubis_sha3B");
   u00096 : constant Version_32 := 16#3f741cab#;
   pragma Export (C, u00096, "anubis_sha3S");
   u00097 : constant Version_32 := 16#bea3a350#;
   pragma Export (C, u00097, "anubis_shieldB");
   u00098 : constant Version_32 := 16#f46a840f#;
   pragma Export (C, u00098, "anubis_shieldS");
   u00099 : constant Version_32 := 16#eeca3098#;
   pragma Export (C, u00099, "anubis_stark_fieldB");
   u00100 : constant Version_32 := 16#467a8327#;
   pragma Export (C, u00100, "anubis_stark_fieldS");
   u00101 : constant Version_32 := 16#ef66e608#;
   pragma Export (C, u00101, "anubis_stark_friB");
   u00102 : constant Version_32 := 16#63e48596#;
   pragma Export (C, u00102, "anubis_stark_friS");
   u00103 : constant Version_32 := 16#c57bc416#;
   pragma Export (C, u00103, "anubis_stark_polyB");
   u00104 : constant Version_32 := 16#e24c520b#;
   pragma Export (C, u00104, "anubis_stark_polyS");
   u00105 : constant Version_32 := 16#a0a1ece6#;
   pragma Export (C, u00105, "anubis_tokenB");
   u00106 : constant Version_32 := 16#ddddd698#;
   pragma Export (C, u00106, "anubis_tokenS");
   u00107 : constant Version_32 := 16#ec82b6b8#;
   pragma Export (C, u00107, "anubis_treasuryB");
   u00108 : constant Version_32 := 16#3590a3a2#;
   pragma Export (C, u00108, "anubis_treasuryS");
   u00109 : constant Version_32 := 16#dea2a956#;
   pragma Export (C, u00109, "anubis_typesS");
   u00110 : constant Version_32 := 16#a983c8a9#;
   pragma Export (C, u00110, "anubis_vestingB");
   u00111 : constant Version_32 := 16#34d16235#;
   pragma Export (C, u00111, "anubis_vestingS");
   u00112 : constant Version_32 := 16#3ad7d239#;
   pragma Export (C, u00112, "anubis_whisperB");
   u00113 : constant Version_32 := 16#646c62f6#;
   pragma Export (C, u00113, "anubis_whisperS");
   u00114 : constant Version_32 := 16#7870cd28#;
   pragma Export (C, u00114, "anubis_wordB");
   u00115 : constant Version_32 := 16#ec44f66b#;
   pragma Export (C, u00115, "anubis_wordS");
   u00116 : constant Version_32 := 16#c2da88e8#;
   pragma Export (C, u00116, "khepri_mptB");
   u00117 : constant Version_32 := 16#91e89cfd#;
   pragma Export (C, u00117, "khepri_mptS");
   u00118 : constant Version_32 := 16#2d8e4b36#;
   pragma Export (C, u00118, "khepri_mpt_typesB");
   u00119 : constant Version_32 := 16#b8015c59#;
   pragma Export (C, u00119, "khepri_mpt_typesS");
   u00120 : constant Version_32 := 16#b82c6a5e#;
   pragma Export (C, u00120, "khepri_registryB");
   u00121 : constant Version_32 := 16#a4e017a4#;
   pragma Export (C, u00121, "khepri_registryS");
   u00122 : constant Version_32 := 16#f89b3c6c#;
   pragma Export (C, u00122, "khepri_storage_trieB");
   u00123 : constant Version_32 := 16#6d14f4d7#;
   pragma Export (C, u00123, "khepri_storage_trieS");
   u00124 : constant Version_32 := 16#d0d35279#;
   pragma Export (C, u00124, "khepri_typesB");
   u00125 : constant Version_32 := 16#b41e847e#;
   pragma Export (C, u00125, "khepri_typesS");
   u00126 : constant Version_32 := 16#f203cf20#;
   pragma Export (C, u00126, "scarab_aadkgB");
   u00127 : constant Version_32 := 16#5ccd4814#;
   pragma Export (C, u00127, "scarab_aadkgS");
   u00128 : constant Version_32 := 16#ddc121d6#;
   pragma Export (C, u00128, "scarab_horusB");
   u00129 : constant Version_32 := 16#4b1c6b9d#;
   pragma Export (C, u00129, "scarab_horusS");
   u00130 : constant Version_32 := 16#2bdbebe1#;
   pragma Export (C, u00130, "scarab_khnumB");
   u00131 : constant Version_32 := 16#8af522c5#;
   pragma Export (C, u00131, "scarab_khnumS");
   u00132 : constant Version_32 := 16#2ab74abb#;
   pragma Export (C, u00132, "scarab_maatB");
   u00133 : constant Version_32 := 16#483310ff#;
   pragma Export (C, u00133, "scarab_maatS");
   u00134 : constant Version_32 := 16#817f3bdb#;
   pragma Export (C, u00134, "scarab_sebekB");
   u00135 : constant Version_32 := 16#a37a40fe#;
   pragma Export (C, u00135, "scarab_sebekS");
   u00136 : constant Version_32 := 16#088b6518#;
   pragma Export (C, u00136, "scarab_sekhmetB");
   u00137 : constant Version_32 := 16#9dd7f00e#;
   pragma Export (C, u00137, "scarab_sekhmetS");
   u00138 : constant Version_32 := 16#ca23dbdf#;
   pragma Export (C, u00138, "scarab_tefnutB");
   u00139 : constant Version_32 := 16#bef1e9fb#;
   pragma Export (C, u00139, "scarab_tefnutS");
   u00140 : constant Version_32 := 16#caf22b74#;
   pragma Export (C, u00140, "scarab_thothB");
   u00141 : constant Version_32 := 16#a5f47dd8#;
   pragma Export (C, u00141, "scarab_thothS");

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
   --  system.exp_llu%s
   --  system.exp_uns%s
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
   --  system.exp_int%s
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
   --  anubis_chacha20%s
   --  anubis_chacha20%b
   --  anubis_mldsa_types%s
   --  anubis_mlkem_types%s
   --  anubis_poly1305%s
   --  anubis_poly1305%b
   --  anubis_chacha20_poly1305%s
   --  anubis_chacha20_poly1305%b
   --  anubis_stark_field%s
   --  anubis_stark_field%b
   --  anubis_word%s
   --  anubis_word%b
   --  anubis_address_base32%s
   --  anubis_address_base32%b
   --  anubis_bug_bounty%s
   --  anubis_bug_bounty%b
   --  anubis_bytes%s
   --  anubis_bytes%b
   --  anubis_cbd%s
   --  anubis_cbd%b
   --  anubis_certification%s
   --  anubis_certification%b
   --  anubis_dead_man_switch%s
   --  anubis_dead_man_switch%b
   --  anubis_developer_rewards%s
   --  anubis_developer_rewards%b
   --  anubis_genesis_provers%s
   --  anubis_genesis_provers%b
   --  anubis_genesis_validators%s
   --  anubis_genesis_validators%b
   --  anubis_governance%s
   --  anubis_governance%b
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
   --  anubis_proof_of_build%s
   --  anubis_proof_of_build%b
   --  anubis_quantum_insurance%s
   --  anubis_quantum_insurance%b
   --  anubis_sha3%s
   --  anubis_sha3%b
   --  aegis_storage%s
   --  aegis_storage%b
   --  aegis_contract%s
   --  aegis_contract%b
   --  aegis_sandbox%s
   --  aegis_sandbox%b
   --  anubis_address_checksum%s
   --  anubis_address_checksum%b
   --  anubis_address_derive%s
   --  anubis_address_derive%b
   --  anubis_address%s
   --  anubis_address%b
   --  anubis_lattice_zk%s
   --  anubis_lattice_zk%b
   --  anubis_mldsa_sample%s
   --  anubis_mldsa_sample%b
   --  anubis_mldsa%s
   --  anubis_mldsa%b
   --  anubis_eye%s
   --  anubis_eye%b
   --  anubis_mlkem_sample%s
   --  anubis_mlkem_sample%b
   --  anubis_mlkem%s
   --  anubis_mlkem%b
   --  anubis_shield%s
   --  anubis_shield%b
   --  anubis_stark_poly%s
   --  anubis_stark_poly%b
   --  anubis_stark_fri%s
   --  anubis_stark_fri%b
   --  anubis_gate%s
   --  anubis_gate%b
   --  anubis_token%s
   --  anubis_token%b
   --  anubis_treasury%s
   --  anubis_treasury%b
   --  anubis_vesting%s
   --  anubis_vesting%b
   --  anubis_whisper%s
   --  anubis_whisper%b
   --  khepri_mpt_types%s
   --  khepri_mpt_types%b
   --  khepri_mpt%s
   --  khepri_mpt%b
   --  aegis_execution%s
   --  aegis_execution%b
   --  aegis_crypto_api%s
   --  aegis_crypto_api%b
   --  aegis_syscall%s
   --  aegis_syscall%b
   --  khepri_registry%s
   --  khepri_registry%b
   --  khepri_types%s
   --  khepri_types%b
   --  khepri_storage_trie%s
   --  khepri_storage_trie%b
   --  scarab_aadkg%s
   --  scarab_aadkg%b
   --  scarab_horus%s
   --  scarab_horus%b
   --  scarab_khnum%s
   --  scarab_khnum%b
   --  scarab_maat%s
   --  scarab_maat%b
   --  scarab_sebek%s
   --  scarab_sebek%b
   --  scarab_sekhmet%s
   --  scarab_sekhmet%b
   --  scarab_tefnut%s
   --  scarab_tefnut%b
   --  scarab_thoth%s
   --  scarab_thoth%b
   --  aegis_ffi%s
   --  aegis_ffi%b
   --  END ELABORATION ORDER

end aegisvmmain;
