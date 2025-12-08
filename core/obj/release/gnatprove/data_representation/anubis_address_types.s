	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__network_typeH
_anubis_address_types__network_typeH:
LFB1:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L3
	ldrb	w2, [x0]
	mov	w1, 60495
	movk	w1, 0x4ec4, lsl 16
	add	w0, w2, w2, lsl 1
	lsl	w2, w2, 3
	umull	x3, w2, w1
	umull	x1, w0, w1
	lsr	x3, x3, 34
	lsr	x1, x1, 34
	add	w5, w3, w3, lsl 1
	add	w4, w1, w1, lsl 1
	add	w3, w3, w5, lsl 2
	add	w1, w1, w4, lsl 2
	sub	w2, w2, w3
	sub	w0, w0, w1
	sxtw	x2, w2
	sxtw	x0, w0
L2:
	adrp	x3, _network_typeG.7@PAGE
	mov	w1, 52429
	add	x3, x3, _network_typeG.7@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, x0]
	ldrb	w2, [x3, x2]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
	.p2align 2,,3
L3:
	mov	x2, 0
	mov	x0, 0
	b	L2
LFE1:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__entity_typeH
_anubis_address_types__entity_typeH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L7
	ldrb	w0, [x0]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w3, w0, w0, lsl 1
	add	w0, w0, w0, lsl 3
	umull	x2, w3, w1
	umull	x1, w0, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	add	w5, w2, w2, lsl 2
	add	w4, w1, w1, lsl 2
	add	w2, w2, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w3, w3, w2
	adrp	x2, _entity_typeG.3@PAGE
	sub	w0, w0, w1
	add	x2, x2, _entity_typeG.3@PAGEOFF;
	sxtw	x3, w3
	sxtw	x1, w0
	ldrb	w1, [x2, x1]
	ldrb	w0, [x2, x3]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L7:
	adrp	x2, _entity_typeG.3@PAGE
	mov	x3, 0
	add	x2, x2, _entity_typeG.3@PAGEOFF;
	mov	x1, 0
	ldrb	w0, [x2, x3]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__Tbase32_alphabet_arrayBIP
_anubis_address_types__Tbase32_alphabet_arrayBIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__Taccount_idBIP
_anubis_address_types__Taccount_idBIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__Tchecksum_bytesBIP
_anubis_address_types__Tchecksum_bytesBIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__base32_charPredicate
_anubis_address_types__base32_charPredicate:
LFB6:
	cmp	w0, 57
	ble	L15
	sub	w0, w0, #97
	mov	x1, 46847
	and	w2, w0, 255
	movk	x1, 0x3ef, lsl 16
	cmp	w2, 26
	lsr	x0, x1, x0
	and	w0, w0, 1
	csel	w0, w0, wzr, cc
	ret
	.p2align 2,,3
L15:
	cmp	w0, 47
	cset	w0, gt
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__Tpayload_base32BIP
_anubis_address_types__Tpayload_base32BIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__Tchunked_payloadBIP
_anubis_address_types__Tchunked_payloadBIP:
LFB8:
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__Tchecksum_base32BIP
_anubis_address_types__Tchecksum_base32BIP:
LFB9:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__addressIP
_anubis_address_types__addressIP:
LFB10:
	ret
LFE10:
	.const
	.align	3
lC9:
	.ascii "anubis_address_types.ads"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__network_string
_anubis_address_types__network_string:
LFB11:
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	x29, sp
LCFI1:
	cmp	w0, 4
	bhi	L34
	cmp	w0, 2
	beq	L22
	bhi	L23
	cbz	w0, L35
	mov	x1, 4
	mov	x0, 12
	bl	_system__secondary_stack__ss_allocate
	adrp	x1, lC11@PAGE
	mov	w3, 25972
	movk	w3, 0x7473, lsl 16
	ldr	d31, [x1, #lC11@PAGEOFF]
L32:
	mov	x2, x0
	mov	x1, x0
	str	d31, [x0], 8
	str	w3, [x2, 8]
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
	.p2align 2,,3
L23:
LCFI3:
	cmp	w0, 3
	bne	L36
	mov	x1, 4
	mov	x0, 12
	bl	_system__secondary_stack__ss_allocate
	adrp	x1, lC12@PAGE
	mov	w4, 24940
	mov	x2, x0
	mov	w3, 98
	ldr	d31, [x1, #lC12@PAGEOFF]
L33:
	mov	x0, x2
	mov	x1, x2
	str	d31, [x0], 8
	strh	w4, [x2, 8]
	strb	w3, [x2, 10]
	ldp	x29, x30, [sp], 16
LCFI4:
	ret
	.p2align 2,,3
L36:
LCFI5:
	mov	x1, 4
	mov	x0, 16
	bl	_system__secondary_stack__ss_allocate
	adrp	x1, lC13@PAGE
	adrp	x4, lC10@PAGE
	add	x4, x4, lC10@PAGEOFF;
	mov	x2, x0
	ldr	d31, [x1, #lC13@PAGEOFF]
	mov	x3, x0
	mov	x1, x0
	ldr	w5, [x4]
	ldr	w4, [x4, 3]
	str	d31, [x2], 8
	mov	x0, x2
	str	w5, [x3, 8]
	str	w4, [x2, 3]
	ldp	x29, x30, [sp], 16
LCFI6:
	ret
	.p2align 2,,3
L35:
LCFI7:
	mov	x1, 4
	mov	x0, 12
	bl	_system__secondary_stack__ss_allocate
	adrp	x1, lC11@PAGE
	mov	w3, 24941
	movk	w3, 0x6e69, lsl 16
	ldr	d31, [x1, #lC11@PAGEOFF]
	b	L32
	.p2align 2,,3
L22:
	mov	x1, 4
	mov	x0, 12
	bl	_system__secondary_stack__ss_allocate
	adrp	x1, lC12@PAGE
	mov	w4, 25956
	mov	x2, x0
	mov	w3, 118
	ldr	d31, [x1, #lC12@PAGEOFF]
	b	L33
L34:
	adrp	x0, lC9@PAGE
	mov	w1, 98
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE11:
	.const
	.align	3
lC10:
	.ascii "staging"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__entity_char
_anubis_address_types__entity_char:
LFB13:
	cmp	w0, 3
	bhi	L44
	mov	w1, w0
	mov	w0, 115
	beq	L39
	adrp	x0, _CSWTCH.67@PAGE
	add	x0, x0, _CSWTCH.67@PAGEOFF;
	ldrsb	w0, [x0, w1, uxtw]
L39:
	ret
L44:
	adrp	x0, lC9@PAGE
	stp	x29, x30, [sp, -16]!
LCFI8:
	mov	w1, 109
	mov	x29, sp
LCFI9:
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.const
	.align	3
lC14:
	.ascii "failed postcondition from anubis_address_types.ads:125"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_types__entity_string
_anubis_address_types__entity_string:
LFB15:
	stp	x29, x30, [sp, -16]!
LCFI10:
	mov	x29, sp
LCFI11:
	cmp	w0, 3
	bhi	L55
	mov	x1, 4
	cmp	w0, 1
	beq	L47
	cmp	w0, 2
	beq	L48
	cbnz	w0, L49
	mov	x0, 12
	bl	_system__secondary_stack__ss_allocate
	movi	v31.2s, 0x1
	mov	w3, 117
L54:
	mov	x2, x0
	str	d31, [x0]
	mov	x1, x0
	strb	w3, [x0, 8]!
	ldp	w3, w2, [x2]
	and	w4, w2, w2, asr #31
	cmp	w3, w4
	ble	L56
	cmp	w3, w2
	bne	L57
	ldp	x29, x30, [sp], 16
LCFI12:
	ret
	.p2align 2,,3
L48:
LCFI13:
	mov	x0, 12
	bl	_system__secondary_stack__ss_allocate
	movi	v31.2s, 0x1
	mov	w3, 118
	b	L54
	.p2align 2,,3
L47:
	mov	x0, 12
	bl	_system__secondary_stack__ss_allocate
	movi	v31.2s, 0x1
	mov	w3, 99
	b	L54
	.p2align 2,,3
L49:
	mov	x0, 12
	bl	_system__secondary_stack__ss_allocate
	movi	v31.2s, 0x1
	mov	w3, 115
	b	L54
L56:
	adrp	x0, lC9@PAGE
	mov	w1, 118
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L57:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L55:
	adrp	x0, lC9@PAGE
	mov	w1, 119
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE15:
	.const
	.align	2
lC0:
	.word	1
	.word	54
	.text
	.const
_CSWTCH.67:
	.byte	117
	.byte	99
	.byte	118
	.align	3
_entity_typeG.3:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	3
	.space 5
	.align	3
_network_typeG.7:
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	1
	.byte	0
	.byte	1
	.byte	0
	.space 3
	.globl _anubis_address_types__base32_alphabet
	.align	3
_anubis_address_types__base32_alphabet:
	.ascii "0123456789abcdefghjkmnpqrstvwxyz"
	.globl _anubis_address_types__entity_typeN
	.align	3
_anubis_address_types__entity_typeN:
	.byte	1
	.byte	5
	.byte	13
	.byte	22
	.byte	28
	.space 3
	.globl _anubis_address_types__entity_typeS
	.align	3
_anubis_address_types__entity_typeS:
	.ascii "USERCONTRACTVALIDATORSYSTEM"
	.globl _anubis_address_types__network_typeN
	.align	3
_anubis_address_types__network_typeN:
	.byte	1
	.byte	5
	.byte	9
	.byte	12
	.byte	15
	.byte	22
	.space 2
	.globl _anubis_address_types__network_typeS
	.align	3
_anubis_address_types__network_typeS:
	.ascii "MAINTESTDEVLABSTAGING"
	.globl _anubis_address_types__algorithm_tag
	.align	3
_anubis_address_types__algorithm_tag:
	.ascii "mldsa87"
	.space 1
	.globl _anubis_address_types__separator_dash
_anubis_address_types__separator_dash:
	.byte	45
	.globl _anubis_address_types__separator_colon
_anubis_address_types__separator_colon:
	.byte	58
	.globl _anubis_address_types_E
	.data
	.align	1
_anubis_address_types_E:
	.space 2
	.const
	.align	3
lC11:
	.word	1
	.word	4
	.align	3
lC12:
	.word	1
	.word	3
	.align	3
lC13:
	.word	1
	.word	7
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0
	.byte	0x3
	.ascii "zR\0"
	.uleb128 0x1
	.sleb128 -8
	.uleb128 0x1e
	.uleb128 0x1
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
	.quad	LFB1-.
	.set L$set$2,LFE1-LFB1
	.quad L$set$2
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$3,LEFDE3-LASFDE3
	.long L$set$3
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB2-.
	.set L$set$4,LFE2-LFB2
	.quad L$set$4
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$5,LEFDE5-LASFDE5
	.long L$set$5
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB3-.
	.set L$set$6,LFE3-LFB3
	.quad L$set$6
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$7,LEFDE7-LASFDE7
	.long L$set$7
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB4-.
	.set L$set$8,LFE4-LFB4
	.quad L$set$8
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$9,LEFDE9-LASFDE9
	.long L$set$9
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB5-.
	.set L$set$10,LFE5-LFB5
	.quad L$set$10
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$11,LEFDE11-LASFDE11
	.long L$set$11
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB6-.
	.set L$set$12,LFE6-LFB6
	.quad L$set$12
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$13,LEFDE13-LASFDE13
	.long L$set$13
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB7-.
	.set L$set$14,LFE7-LFB7
	.quad L$set$14
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$15,LEFDE15-LASFDE15
	.long L$set$15
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB8-.
	.set L$set$16,LFE8-LFB8
	.quad L$set$16
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$17,LEFDE17-LASFDE17
	.long L$set$17
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB9-.
	.set L$set$18,LFE9-LFB9
	.quad L$set$18
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$19,LEFDE19-LASFDE19
	.long L$set$19
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB10-.
	.set L$set$20,LFE10-LFB10
	.quad L$set$20
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$21,LEFDE21-LASFDE21
	.long L$set$21
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB11-.
	.set L$set$22,LFE11-LFB11
	.quad L$set$22
	.uleb128 0
	.byte	0x4
	.set L$set$23,LCFI0-LFB11
	.long L$set$23
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$24,LCFI1-LCFI0
	.long L$set$24
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$25,LCFI2-LCFI1
	.long L$set$25
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$26,LCFI3-LCFI2
	.long L$set$26
	.byte	0xb
	.byte	0x4
	.set L$set$27,LCFI4-LCFI3
	.long L$set$27
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$28,LCFI5-LCFI4
	.long L$set$28
	.byte	0xb
	.byte	0x4
	.set L$set$29,LCFI6-LCFI5
	.long L$set$29
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$30,LCFI7-LCFI6
	.long L$set$30
	.byte	0xb
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$31,LEFDE23-LASFDE23
	.long L$set$31
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB13-.
	.set L$set$32,LFE13-LFB13
	.quad L$set$32
	.uleb128 0
	.byte	0x4
	.set L$set$33,LCFI8-LFB13
	.long L$set$33
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$34,LCFI9-LCFI8
	.long L$set$34
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$35,LEFDE25-LASFDE25
	.long L$set$35
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB15-.
	.set L$set$36,LFE15-LFB15
	.quad L$set$36
	.uleb128 0
	.byte	0x4
	.set L$set$37,LCFI10-LFB15
	.long L$set$37
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$38,LCFI11-LCFI10
	.long L$set$38
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$39,LCFI12-LCFI11
	.long L$set$39
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$40,LCFI13-LCFI12
	.long L$set$40
	.byte	0xb
	.align	3
LEFDE25:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
