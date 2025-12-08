	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC3:
	.ascii "failed precondition from anubis_address_checksum.ads:64"
	.align	3
lC4:
	.ascii "anubis_address_checksum.adb"
	.space 1
	.align	3
lC5:
	.ascii "Loop_Invariant failed at anubis_address_checksum.adb:30"
	.align	3
lC6:
	.ascii "Loop_Invariant failed at anubis_address_checksum.adb:31"
	.align	3
lC7:
	.ascii "Loop_Invariant failed at anubis_address_checksum.adb:42"
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_address_checksum.adb:43"
	.align	3
lC9:
	.ascii "Loop_Invariant failed at anubis_address_checksum.adb:62"
	.align	3
lC10:
	.ascii "Loop_Invariant failed at anubis_address_checksum.adb:63"
	.align	3
lC11:
	.ascii "failed postcondition from anubis_address_checksum.ads:65"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_checksum__build_preimage
_anubis_address_checksum__build_preimage:
LFB2:
	stp	x29, x30, [sp, -128]!
LCFI0:
	mov	x29, sp
LCFI1:
LEHB0:
LEHE0:
	stp	x19, x20, [sp, 16]
LCFI2:
	mov	w19, w0
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI3:
	mov	w27, w1
	ldpsw	x26, x0, [x4]
	add	x1, x26, 68
	cmp	x1, x0
	bge	L43
	add	x21, x29, 96
	add	x0, x29, 128
	mov	x8, x21
	mov	x20, x4
	mov	x25, x2
	mov	x24, x3
	str	x0, [x29, 120]
LEHB1:
	bl	_system__secondary_stack__ss_mark
	cmp	w19, 4
	bhi	L44
	mov	w0, w19
	bl	_anubis_address_types__network_string
	ldp	w19, w23, [x1]
	mov	x22, x0
	and	w0, w23, w23, asr #31
	cmp	w19, w0
	ble	L45
	cmp	w27, 3
	bhi	L46
	mov	w0, w27
	bl	_anubis_address_types__entity_char
	ldr	w1, [x20]
	mov	w27, w0
	tbnz	w1, #31, L47
	ldr	w0, [x20, 4]
	cmp	w1, w0
	ble	L48
	mov	x2, 0
	sxtw	x28, w1
L7:
	mov	w1, 32
	mov	x0, x24
	bl	_memset
	adrp	x3, _algo_tag.2@PAGE
	ldp	w4, w6, [x20]
	mov	x1, x28
	add	x3, x3, _algo_tag.2@PAGEOFF;
	add	x0, x28, 7
	sub	x3, x3, x28
	sub	x5, x24, x26
	b	L10
	.p2align 2,,3
L35:
	mov	x1, x2
L10:
	cmp	w4, w1
	bgt	L49
	cmp	w6, w1
	ble	L50
	ldrsb	w2, [x3, x1]
	strb	w2, [x5, x1]
	add	x2, x1, 1
	cmp	x2, x0
	bne	L35
	add	w2, w1, 1
	cmp	w4, w2
	ccmp	w6, w2, 1, le
	blt	L51
	sxtw	x0, w2
	mov	w3, 58
	sub	x0, x0, x26
	strb	w3, [x24, x0]
	mov	w0, 2147483647
	cmp	w2, w0
	beq	L52
	mov	w2, w1
	add	w1, w1, 2
	cmp	w19, w23
	bgt	L13
	sxtw	x5, w19
	sxtw	x3, w2
	add	x2, x26, x5
	sub	x0, x22, x5
	sub	x3, x3, x2
	add	x0, x0, 1
	add	x3, x3, 3
	sxtw	x23, w23
	add	x3, x24, x3
	sub	x5, x5, #1
	.p2align 5,,15
L16:
	cmp	w4, w1
	bgt	L53
	cmp	w6, w1
	ble	L54
	ldrsb	w2, [x0, x5]
	strb	w2, [x3, x5]
	add	x5, x5, 1
	add	w1, w1, 1
	cmp	x23, x5
	bne	L16
L13:
	cmp	w4, w1
	ccmp	w6, w1, 1, le
	blt	L55
	sxtw	x0, w1
	mov	w2, 58
	sub	x0, x0, x26
	strb	w2, [x24, x0]
	mov	w2, 2147483647
	cmp	w1, w2
	beq	L56
	add	w2, w1, 1
	cmp	w6, w2
	blt	L57
	sxtw	x3, w2
	sub	x3, x3, x26
	strb	w27, [x24, x3]
	mov	w3, 2147483647
	cmp	w2, w3
	beq	L58
	add	w3, w1, 2
	cmp	w6, w3
	blt	L59
	sxtw	x2, w3
	mov	w5, 58
	sub	x2, x2, x26
	strb	w5, [x24, x2]
	mov	w2, 2147483647
	cmp	w3, w2
	beq	L60
	add	x2, x0, 3
	mov	x5, 0
	add	w0, w1, 3
	add	x3, x24, x2
	.p2align 5,,15
L25:
	cmp	w4, w0
	bgt	L61
	cmp	w6, w0
	ble	L62
	ldrsb	w1, [x25, x5]
	strb	w1, [x3, x5]
LEHE1:
	add	x5, x5, 1
	add	w0, w0, 1
	cmp	x5, 52
	bne	L25
	subs	w19, w0, w4
	bvs	L27
	tbnz	w19, #31, L63
	mov	x0, x21
LEHB2:
	bl	_system__secondary_stack__ss_release
	ldp	w1, w0, [x20]
	subs	w0, w0, w1
	csinc	w0, wzr, w0, lt
	cmp	w0, w19
	blt	L64
	mov	w0, w19
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
LEHE2:
	ldp	x29, x30, [sp], 128
LCFI4:
	ret
	.p2align 2,,3
L48:
LCFI5:
	sxtw	x2, w0
	sxtw	x28, w1
	add	x2, x2, 1
	sub	x2, x2, x28
	b	L7
L61:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
LEHB3:
	bl	_system__assertions__raise_assert_failure
L62:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L54:
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L50:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L53:
	adrp	x0, lC7@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L49:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L27:
	adrp	x0, lC4@PAGE
	mov	w1, 68
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L47:
	adrp	x0, lC4@PAGE
	mov	w1, 23
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L51:
	adrp	x0, lC4@PAGE
	mov	w1, 37
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L52:
	adrp	x0, lC4@PAGE
	mov	w1, 38
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L46:
	adrp	x0, lC4@PAGE
	mov	w1, 22
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L45:
	adrp	x0, lC4@PAGE
	mov	w1, 21
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L44:
	adrp	x0, lC4@PAGE
	mov	w1, 21
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L63:
	adrp	x0, lC4@PAGE
	mov	w1, 68
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LEHE3:
L64:
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
LEHB4:
	bl	_system__assertions__raise_assert_failure
LEHE4:
L59:
	adrp	x0, lC4@PAGE
	mov	w1, 57
	add	x0, x0, lC4@PAGEOFF;
LEHB5:
	bl	___gnat_rcheck_CE_Index_Check
L60:
	adrp	x0, lC4@PAGE
	mov	w1, 58
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L55:
	adrp	x0, lC4@PAGE
	mov	w1, 49
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L56:
	adrp	x0, lC4@PAGE
	mov	w1, 50
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L57:
	adrp	x0, lC4@PAGE
	mov	w1, 53
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L58:
	adrp	x0, lC4@PAGE
	mov	w1, 54
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LEHE5:
L43:
	adrp	x0, lC3@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC3@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
LEHB6:
	bl	_system__assertions__raise_assert_failure
L36:
	mov	x19, x0
	mov	x0, x21
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE6:
LFE2:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
LLSDA2:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE2-LLSDACSB2
LLSDACSB2:
	.uleb128 LEHB0-LFB2
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB2
	.uleb128 LEHE1-LEHB1
	.uleb128 L36-LFB2
	.uleb128 0
	.uleb128 LEHB2-LFB2
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB3-LFB2
	.uleb128 LEHE3-LEHB3
	.uleb128 L36-LFB2
	.uleb128 0
	.uleb128 LEHB4-LFB2
	.uleb128 LEHE4-LEHB4
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB5-LFB2
	.uleb128 LEHE5-LEHB5
	.uleb128 L36-LFB2
	.uleb128 0
	.uleb128 LEHB6-LFB2
	.uleb128 LEHE6-LEHB6
	.uleb128 0
	.uleb128 0
LLSDACSE2:
	.text
	.const
	.align	2
lC0:
	.word	1
	.word	55
	.align	2
lC1:
	.word	1
	.word	56
	.text
	.const
	.align	3
lC12:
	.ascii "Loop_Invariant failed at anubis_address_checksum.adb:91"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_checksum__compute_checksum
_anubis_address_checksum__compute_checksum:
LFB5:
	stp	x29, x30, [sp, -240]!
LCFI6:
	mov	x29, sp
LCFI7:
	str	x19, [sp, 16]
LCFI8:
	cmp	w0, 4
	bhi	L67
	cmp	w1, 3
	bhi	L67
	add	x19, x29, 80
	adrp	x4, lC2@PAGE
	mov	x3, x19
	add	x4, x4, lC2@PAGEOFF;
	bl	_anubis_address_checksum__build_preimage
	sub	w3, w0, #1
	cbz	w0, L75
	uxtw	x5, w0
	mov	x1, 1
	add	x0, x29, 160
	b	L69
	.p2align 2,,3
L70:
	add	x1, x1, 1
	cmp	x1, 71
	beq	L76
L69:
	add	x4, x19, x1
	add	x2, x0, x1
	ldrsb	w4, [x4, -1]
	strb	w4, [x2, -1]
	cmp	x5, x1
	bne	L70
L71:
	add	x2, x29, 48
	add	x1, x29, 40
	stp	wzr, w3, [x29, 40]
	bl	_anubis_sha3__sha3_256
	ldrh	w2, [x29, 48]
	mov	x0, 0
	ldrb	w1, [x29, 50]
	bfi	x0, x2, 0, 16
	bfi	x0, x1, 16, 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 240
LCFI9:
	ret
	.p2align 2,,3
L75:
LCFI10:
	add	x0, x29, 160
	b	L71
	.p2align 2,,3
L76:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L67:
	adrp	x0, lC4@PAGE
	mov	w1, 87
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE5:
	.const
	.align	2
lC2:
	.word	1
	.word	70
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_checksum__verify_checksum
_anubis_address_checksum__verify_checksum:
LFB6:
	stp	x29, x30, [sp, -64]!
LCFI11:
	mov	x29, sp
LCFI12:
	str	x3, [x29, 24]
	cmp	w0, 4
	bhi	L79
	cmp	w1, 3
	bhi	L79
	bl	_anubis_address_checksum__compute_checksum
	sxtw	x0, w0
	ldrb	w5, [x29, 24]
	ubfx	x2, x0, 8, 8
	ubfx	x1, x0, 16, 8
	strb	w0, [x29, 40]
	strb	w2, [x29, 41]
	ldrh	w0, [x29, 40]
	ldrb	w4, [x29, 25]
	ldrb	w3, [x29, 26]
	ubfx	x2, x0, 8, 8
	eor	w0, w0, w5
	eor	w2, w2, w4
	orr	w0, w0, w2
	eor	w1, w1, w3
	orr	w0, w1, w0
	tst	w0, 255
	cset	w0, eq
	ldp	x29, x30, [sp], 64
LCFI13:
	ret
L79:
LCFI14:
	adrp	x0, lC4@PAGE
	mov	w1, 114
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE6:
	.const
	.align	3
_algo_tag.2:
	.ascii "mldsa87"
	.space 1
	.globl _anubis_address_checksum_E
	.data
	.align	1
_anubis_address_checksum_E:
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
	.quad	LLSDA2-.
	.byte	0x4
	.set L$set$3,LCFI0-LFB2
	.long L$set$3
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x97
	.uleb128 0xa
	.byte	0x98
	.uleb128 0x9
	.byte	0x99
	.uleb128 0x8
	.byte	0x9a
	.uleb128 0x7
	.byte	0x9b
	.uleb128 0x6
	.byte	0x9c
	.uleb128 0x5
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
	.byte	0xdc
	.byte	0xd9
	.byte	0xda
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$8,LCFI5-LCFI4
	.long L$set$8
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$9,LEFDE3-LASFDE3
	.long L$set$9
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB5-.
	.set L$set$10,LFE5-LFB5
	.quad L$set$10
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$11,LCFI6-LFB5
	.long L$set$11
	.byte	0xe
	.uleb128 0xf0
	.byte	0x9d
	.uleb128 0x1e
	.byte	0x9e
	.uleb128 0x1d
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0x93
	.uleb128 0x1c
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$15,LCFI10-LCFI9
	.long L$set$15
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$16,LEFDE5-LASFDE5
	.long L$set$16
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB6-.
	.set L$set$17,LFE6-LFB6
	.quad L$set$17
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$18,LCFI11-LFB6
	.long L$set$18
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0xb
	.align	3
LEFDE5:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
