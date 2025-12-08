	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_derive__Tpublic_key_bytesBIP
_anubis_address_derive__Tpublic_key_bytesBIP:
LFB2:
	ret
LFE2:
	.const
	.align	3
lC2:
	.ascii "anubis_address_derive.adb"
	.space 1
	.align	3
lC3:
	.ascii "                  "
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_derive__build_domain_separator
_anubis_address_derive__build_domain_separator:
LFB3:
	stp	x29, x30, [sp, -80]!
LCFI0:
	mov	x29, sp
LCFI1:
LEHB0:
LEHE0:
	add	x2, x29, 80
	stp	x19, x20, [sp, 16]
LCFI2:
	add	x20, x29, 48
	mov	x19, x1
	mov	x8, x20
	str	x21, [sp, 32]
LCFI3:
	mov	w21, w0
	str	x2, [x29, 72]
LEHB1:
	bl	_system__secondary_stack__ss_mark
	cmp	w21, 3
	bhi	L13
	mov	w0, w21
	bl	_anubis_address_types__entity_string
LEHE1:
	ldp	w7, w6, [x1]
	and	w1, w6, w6, asr #31
	cmp	w7, w1
	ble	L14
	adrp	x1, lC3@PAGE
	adrp	x4, _anubis_address_derive__domain_prefix@PAGE
	add	x1, x1, lC3@PAGEOFF;
	add	x4, x4, _anubis_address_derive__domain_prefix@PAGEOFF;
	ldp	x8, x9, [x1]
	mov	x2, 1
	sub	x5, x19, #1
	ldrh	w1, [x1, 16]
LEHB2:
	stp	x8, x9, [x19]
	strh	w1, [x19, 16]
LEHE2:
	.p2align 5,,15
L6:
	add	x3, x2, x4
LEHB3:
	ldrsb	w3, [x3, -1]
	strb	w3, [x5, x2]
	add	x2, x2, 1
	cmp	x2, 18
	bne	L6
	cmp	w7, w6
	bgt	L15
	ldrsb	w1, [x0]
LEHE3:
	mov	x0, x20
	strb	w1, [x19, 17]
LEHB4:
	bl	_system__secondary_stack__ss_release
	mov	w0, 18
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI4:
	ret
	.p2align 2,,3
L15:
LCFI5:
	mov	x0, x20
	bl	_system__secondary_stack__ss_release
	mov	w0, 17
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
LEHE4:
	ldp	x29, x30, [sp], 80
LCFI6:
	ret
L14:
LCFI7:
	adrp	x0, lC2@PAGE
	mov	w1, 17
	add	x0, x0, lC2@PAGEOFF;
LEHB5:
	bl	___gnat_rcheck_CE_Range_Check
L13:
	adrp	x0, lC2@PAGE
	mov	w1, 17
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE5:
L10:
	mov	x19, x0
	mov	x0, x20
LEHB6:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE6:
LFE3:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
LLSDA3:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE3-LLSDACSB3
LLSDACSB3:
	.uleb128 LEHB0-LFB3
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB3
	.uleb128 LEHE1-LEHB1
	.uleb128 L10-LFB3
	.uleb128 0
	.uleb128 LEHB2-LFB3
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB3-LFB3
	.uleb128 LEHE3-LEHB3
	.uleb128 L10-LFB3
	.uleb128 0
	.uleb128 LEHB4-LFB3
	.uleb128 LEHE4-LEHB4
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB5-LFB3
	.uleb128 LEHE5-LEHB5
	.uleb128 L10-LFB3
	.uleb128 0
	.uleb128 LEHB6-LFB3
	.uleb128 LEHE6-LEHB6
	.uleb128 0
	.uleb128 0
LLSDACSE3:
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_derive__derive_account_id
_anubis_address_derive__derive_account_id:
LFB6:
	sub	sp, sp, #2768
LCFI8:
	stp	x29, x30, [sp]
LCFI9:
	mov	x29, sp
LCFI10:
	stp	x19, x20, [sp, 16]
LCFI11:
	add	x19, x29, 144
	stp	x21, x22, [sp, 32]
LCFI12:
	mov	w21, w0
	mov	x22, x2
	mov	x0, x19
	mov	x2, 2610
	str	x23, [sp, 48]
LCFI13:
	mov	x23, x1
	mov	w1, 0
	bl	_memset
	cmp	w21, 3
	bhi	L21
	add	x20, x29, 80
	mov	w0, w21
	mov	x1, x20
	bl	_anubis_address_derive__build_domain_separator
	mov	x3, -1
	mov	w21, w0
	sub	w6, w0, #1
	.p2align 5,,15
L18:
	add	x5, x20, x3
	add	x4, x19, x3
	ldrsb	w5, [x5, 1]
	add	x3, x3, 1
	strb	w5, [x4, 1]
	cmp	x6, x3
	bne	L18
	mov	x1, x23
	mov	x2, 2592
	add	x0, x19, w21, sxtw
	bl	_memcpy
	add	x2, x29, 112
	add	w21, w21, 2591
	mov	x0, x19
	add	x1, x29, 72
	mov	x19, x2
	stp	wzr, w21, [x29, 72]
	bl	_anubis_sha3__sha3_256
	ldp	q31, q30, [x19]
	stp	q31, q30, [x22]
	ldr	x23, [sp, 48]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, 2768
LCFI14:
	ret
L21:
LCFI15:
	adrp	x0, lC2@PAGE
	mov	w1, 61
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE6:
	.globl _anubis_address_derive__domain_prefix
	.const
	.align	3
_anubis_address_derive__domain_prefix:
	.ascii "aegis-v1-mldsa87-"
	.globl _anubis_address_derive_E
	.data
	.align	1
_anubis_address_derive_E:
	.space 2
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0
	.byte	0x3
	.ascii "zPLR\0"
	.uleb128 0x1
	.sleb128 -8
	.uleb128 0x1e
	.uleb128 0x7
	.byte	0x9b
L_got_pcr0:
	.long	___gnat_personality_v0@GOT-L_got_pcr0
	.byte	0x10
	.byte	0x10
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LECIE1:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB2-.
	.set L$set$2,LFE2-LFB2
	.quad L$set$2
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$3,LEFDE3-LASFDE3
	.long L$set$3
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB3-.
	.set L$set$4,LFE3-LFB3
	.quad L$set$4
	.uleb128 0x8
	.quad	LLSDA3-.
	.byte	0x4
	.set L$set$5,LCFI0-LFB3
	.long L$set$5
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$6,LCFI1-LCFI0
	.long L$set$6
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$7,LCFI2-LCFI1
	.long L$set$7
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$8,LCFI3-LCFI2
	.long L$set$8
	.byte	0x95
	.uleb128 0x6
	.byte	0x4
	.set L$set$9,LCFI4-LCFI3
	.long L$set$9
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$10,LCFI5-LCFI4
	.long L$set$10
	.byte	0xb
	.byte	0x4
	.set L$set$11,LCFI6-LCFI5
	.long L$set$11
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$13,LEFDE5-LASFDE5
	.long L$set$13
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB6-.
	.set L$set$14,LFE6-LFB6
	.quad L$set$14
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$15,LCFI8-LFB6
	.long L$set$15
	.byte	0xe
	.uleb128 0xad0
	.byte	0x4
	.set L$set$16,LCFI9-LCFI8
	.long L$set$16
	.byte	0x9d
	.uleb128 0x15a
	.byte	0x9e
	.uleb128 0x159
	.byte	0x4
	.set L$set$17,LCFI10-LCFI9
	.long L$set$17
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0x93
	.uleb128 0x158
	.byte	0x94
	.uleb128 0x157
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0x95
	.uleb128 0x156
	.byte	0x96
	.uleb128 0x155
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0x97
	.uleb128 0x154
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0xa
	.byte	0xd7
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$22,LCFI15-LCFI14
	.long L$set$22
	.byte	0xb
	.align	3
LEFDE5:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
