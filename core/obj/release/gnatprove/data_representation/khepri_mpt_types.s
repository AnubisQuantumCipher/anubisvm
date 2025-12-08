	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__Tnibble_pathBIP
_khepri_mpt_types__Tnibble_pathBIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__nibble_keyIP
_khepri_mpt_types__nibble_keyIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__Thash_256BIP
_khepri_mpt_types__Thash_256BIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__node_hashIP
_khepri_mpt_types__node_hashIP:
LFB59:
	ret
LFE59:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__Tnode_valueBIP
_khepri_mpt_types__Tnode_valueBIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__value_dataIP
_khepri_mpt_types__value_dataIP:
LFB61:
	ret
LFE61:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__Tchild_arrayBIP
_khepri_mpt_types__Tchild_arrayBIP:
LFB8:
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__node_kindH
_khepri_mpt_types__node_kindH:
LFB9:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L12
	sub	w1, w1, w2
	cmp	w1, 5
	bgt	L13
L12:
	adrp	x1, _node_kindG.4@PAGE
	mov	x2, 0
	add	x1, x1, _node_kindG.4@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L13:
	ldrb	w0, [x0, 6]
	mov	w1, 36409
	movk	w1, 0x38e3, lsl 16
	add	w2, w0, w0, lsl 1
	lsl	w0, w0, 2
	lsl	w2, w2, 1
	umull	x3, w0, w1
	umull	x1, w2, w1
	lsr	x3, x3, 33
	add	w3, w3, w3, lsl 3
	lsr	x1, x1, 33
	add	w1, w1, w1, lsl 3
	sub	w0, w0, w3
	sxtw	x0, w0
	sub	w2, w2, w1
	adrp	x1, _node_kindG.4@PAGE
	sxtw	x2, w2
	add	x1, x1, _node_kindG.4@PAGEOFF;
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__mpt_nodeIP
_khepri_mpt_types__mpt_nodeIP:
LFB63:
	ret
LFE63:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__Tproof_node_dataBIP
_khepri_mpt_types__Tproof_node_dataBIP:
LFB11:
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__proof_nodeIP
_khepri_mpt_types__proof_nodeIP:
LFB65:
	ret
LFE65:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__Tproof_pathBIP
_khepri_mpt_types__Tproof_pathBIP:
LFB13:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__merkle_proofIP
_khepri_mpt_types__merkle_proofIP:
LFB67:
	ret
LFE67:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__mpt_errorH
_khepri_mpt_types__mpt_errorH:
LFB15:
	ldp	w4, w1, [x1]
	mov	w10, -1
	adrp	x9, _mpt_errorP.3@PAGE
	add	w6, w10, 1
	add	x9, x9, _mpt_errorP.3@PAGEOFF;
	adrp	x12, _mpt_errorT1.2@PAGE
	adrp	x11, _mpt_errorT2.1@PAGE
	mov	w2, 0
	mov	w3, 0
	add	x12, x12, _mpt_errorT1.2@PAGEOFF;
	add	x11, x11, _mpt_errorT2.1@PAGEOFF;
	add	w8, w4, w10
	cmp	w4, w1
	sxtw	x15, w4
	sub	w1, w1, w4
	ldr	w4, [x9, w6, sxtw 2]
	sxtw	x8, w8
	csinc	w5, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w4, sxtw
	mov	w13, 17
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w5, w4
	blt	L21
L25:
	ldrb	w7, [x0, x1]
	ldrb	w4, [x12, w6, sxtw]
	ldrb	w1, [x11, w6, sxtw]
	madd	w4, w4, w7, w3
	madd	w1, w1, w7, w2
	sdiv	w3, w4, w13
	sdiv	w2, w1, w13
	add	w3, w3, w3, lsl 4
	sub	w3, w4, w3
	add	w2, w2, w2, lsl 4
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L21
	add	w6, w10, 1
	mov	w14, 1
	ldr	w4, [x9, w6, sxtw 2]
	mov	w10, 0
	add	x1, x8, w4, sxtw
	sub	x1, x1, x15
	cmp	w5, w4
	bge	L25
L21:
	adrp	x1, _mpt_errorG.0@PAGE
	add	x1, x1, _mpt_errorG.0@PAGEOFF;
	ldrb	w0, [x1, w3, sxtw]
	ldrb	w1, [x1, w2, sxtw]
	add	w0, w0, w1
	and	w0, w0, 7
	ret
LFE15:
	.const
	.align	3
lC2:
	.ascii "khepri_mpt_types.ads"
	.space 1
	.align	3
lC3:
	.ascii "failed precondition from khepri_mpt_types.ads:183"
	.align	3
lC4:
	.ascii "failed precondition from khepri_mpt_types.ads:184"
	.align	3
lC5:
	.ascii "failed precondition from khepri_mpt_types.ads:185"
	.align	3
lC6:
	.ascii "failed precondition from khepri_mpt_types.ads:186"
	.align	3
lC7:
	.ascii "Loop_Invariant failed at khepri_mpt_types.adb:26"
	.align	3
lC8:
	.ascii "Loop_Invariant failed at khepri_mpt_types.adb:28"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__bytes_to_nibbles
_khepri_mpt_types__bytes_to_nibbles:
LFB16:
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	x29, sp
LCFI1:
	ldp	w3, w6, [x1]
	tbnz	w2, #31, L43
	cmp	w2, 32
	bgt	L44
	cbnz	w3, L45
	mov	w1, 2147483647
	cmp	w6, w1
	beq	L46
	tbnz	w6, #31, L47
	add	w6, w6, 1
	cmp	w6, w2
	blt	L32
	movi	v31.4s, 0
	str	wzr, [x8, 64]
	stp	q31, q31, [x8]
	stp	q31, q31, [x8, 32]
	cbz	w2, L26
	sub	w7, w2, #1
	lsl	w6, w6, 1
	add	x7, x7, x0
	mov	x4, x8
	b	L35
	.p2align 2,,3
L36:
	add	x0, x0, 1
	add	x4, x4, 2
	cmp	w3, w6
	beq	L48
L35:
	ldrb	w1, [x0]
	add	w3, w3, 2
	lsr	w5, w1, 4
	and	w1, w1, 15
	strb	w5, [x4]
	strb	w1, [x4, 1]
	cmp	x0, x7
	beq	L49
	cmp	w3, 64
	bne	L36
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L47:
	cbnz	w2, L32
	movi	v31.4s, 0
	str	wzr, [x8, 64]
	stp	q31, q31, [x8]
	stp	q31, q31, [x8, 32]
L26:
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
	.p2align 2,,3
L49:
LCFI3:
	lsl	w2, w2, 1
	str	w2, [x8, 64]
	ldp	x29, x30, [sp], 16
LCFI4:
	ret
L48:
LCFI5:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L43:
	adrp	x0, lC2@PAGE
	mov	w1, 183
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L46:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L32:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L45:
	adrp	x0, lC4@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L44:
	adrp	x0, lC3@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC3@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE16:
	.const
	.align	2
lC1:
	.word	1
	.word	48
	.align	2
lC0:
	.word	1
	.word	49
	.text
	.const
	.align	3
lC9:
	.ascii "failed precondition from khepri_mpt_types.ads:190"
	.align	3
lC10:
	.ascii "Loop_Invariant failed at khepri_mpt_types.adb:56"
	.align	3
lC11:
	.ascii "khepri_mpt_types.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__keys_equal
_khepri_mpt_types__keys_equal:
LFB17:
	stp	x29, x30, [sp, -16]!
LCFI6:
	mov	x29, sp
LCFI7:
	ldr	w5, [x0, 64]
	tbnz	w5, #31, L52
	ldr	w2, [x1, 64]
	tbnz	w2, #31, L52
	mov	x6, x0
	cmp	w5, 64
	mov	w0, 64
	ccmp	w2, w0, 0, le
	cset	w0, gt
	bgt	L62
	cmp	w5, w2
	bne	L54
	cbz	w5, L59
	sub	w7, w5, #1
	mov	x2, 0
	b	L55
	.p2align 2,,3
L64:
	cmp	x7, x2
	beq	L59
	add	x2, x2, 1
	cmp	w5, w2
	ble	L63
L55:
	ldrb	w4, [x6, x2]
	cmp	w4, 15
	bhi	L57
	ldrb	w3, [x1, x2]
	cmp	w3, 15
	bhi	L57
	cmp	w4, w3
	beq	L64
L54:
	ldp	x29, x30, [sp], 16
LCFI8:
	ret
	.p2align 2,,3
L59:
LCFI9:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI10:
	ret
L57:
LCFI11:
	adrp	x0, lC11@PAGE
	mov	w1, 60
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L63:
	adrp	x0, lC10@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L62:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L52:
	adrp	x0, lC2@PAGE
	mov	w1, 190
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE17:
	.const
	.align	3
lC12:
	.ascii "failed precondition from khepri_mpt_types.ads:194"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__common_prefix_length
_khepri_mpt_types__common_prefix_length:
LFB18:
	stp	x29, x30, [sp, -16]!
LCFI12:
	mov	x29, sp
LCFI13:
	ldr	w2, [x0, 64]
	tbnz	w2, #31, L67
	mov	x5, x0
	ldr	w0, [x1, 64]
	tbnz	w0, #31, L67
	cmp	w2, 64
	mov	w3, 64
	ccmp	w0, w3, 0, le
	bgt	L79
	cmp	w2, w0
	csel	w0, w2, w0, le
	cbz	w0, L65
	uxtw	x0, w0
	mov	x2, 0
	b	L73
	.p2align 2,,3
L72:
	add	x3, x2, 1
	cmp	x0, x3
	beq	L80
	mov	x2, x3
L73:
	ldrb	w4, [x5, x2]
	cmp	w4, 15
	bhi	L71
	ldrb	w3, [x1, x2]
	cmp	w3, 15
	bhi	L71
	cmp	w4, w3
	beq	L72
	mov	w0, w2
L65:
	ldp	x29, x30, [sp], 16
LCFI14:
	ret
	.p2align 2,,3
L80:
LCFI15:
	add	w0, w2, 1
	ldp	x29, x30, [sp], 16
LCFI16:
	ret
L71:
LCFI17:
	adrp	x0, lC11@PAGE
	mov	w1, 86
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L67:
	adrp	x0, lC2@PAGE
	mov	w1, 194
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L79:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE18:
	.const
	.align	3
lC13:
	.ascii "failed precondition from khepri_mpt_types.ads:198"
	.align	3
lC14:
	.ascii "Loop_Invariant failed at khepri_mpt_types.adb:110"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__is_prefix
_khepri_mpt_types__is_prefix:
LFB19:
	stp	x29, x30, [sp, -16]!
LCFI18:
	mov	x29, sp
LCFI19:
	ldr	w5, [x0, 64]
	tbnz	w5, #31, L83
	ldr	w2, [x1, 64]
	tbnz	w2, #31, L83
	mov	x6, x0
	cmp	w5, 64
	mov	w0, 64
	ccmp	w2, w0, 0, le
	cset	w0, gt
	bgt	L93
	cmp	w5, w2
	bgt	L85
	cbz	w5, L90
	sub	w7, w5, #1
	mov	x2, 0
	b	L86
	.p2align 2,,3
L95:
	cmp	x7, x2
	beq	L90
	add	x2, x2, 1
	cmp	w5, w2
	ble	L94
L86:
	ldrb	w4, [x6, x2]
	cmp	w4, 15
	bhi	L88
	ldrb	w3, [x1, x2]
	cmp	w3, 15
	bhi	L88
	cmp	w4, w3
	beq	L95
L85:
	ldp	x29, x30, [sp], 16
LCFI20:
	ret
	.p2align 2,,3
L90:
LCFI21:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI22:
	ret
L88:
LCFI23:
	adrp	x0, lC11@PAGE
	mov	w1, 114
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L94:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L83:
	adrp	x0, lC2@PAGE
	mov	w1, 198
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L93:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE19:
	.const
	.align	3
lC15:
	.ascii "failed precondition from khepri_mpt_types.ads:205"
	.align	3
lC16:
	.ascii "Loop_Invariant failed at khepri_mpt_types.adb:141"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__remove_prefix
_khepri_mpt_types__remove_prefix:
LFB20:
	stp	x29, x30, [sp, -16]!
LCFI24:
	mov	x29, sp
LCFI25:
	tbnz	w1, #31, L98
	ldr	w4, [x0, 64]
	tbnz	w4, #31, L98
	cmp	w4, 64
	ccmp	w4, w1, 1, le
	blt	L109
	movi	v31.4s, 0
	subs	w7, w4, w1
	str	wzr, [x8, 64]
	stp	q31, q31, [x8]
	stp	q31, q31, [x8, 32]
	beq	L96
	sxtw	x2, w1
	add	w3, w1, 1
	add	x1, x2, 1
	sub	w6, w7, #1
	add	x5, x0, x1
	mov	x0, -1
	.p2align 5,,15
L103:
	add	w1, w3, w0
	cmp	w4, w1
	ble	L110
	ldrb	w1, [x5, x0]
	cmp	w1, 15
	bhi	L111
	add	x2, x8, x0
	add	x0, x0, 1
	strb	w1, [x2, 1]
	cmp	x6, x0
	bne	L103
	str	w7, [x8, 64]
L96:
	ldp	x29, x30, [sp], 16
LCFI26:
	ret
L111:
LCFI27:
	adrp	x0, lC11@PAGE
	mov	w1, 144
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L110:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L109:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L98:
	adrp	x0, lC2@PAGE
	mov	w1, 205
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types__hash_equal
_khepri_mpt_types__hash_equal:
LFB21:
	mov	x2, 0
	b	L114
	.p2align 2,,3
L118:
	cmp	x2, 32
	beq	L117
L114:
	ldrb	w4, [x0, x2]
	ldrb	w3, [x1, x2]
	add	x2, x2, 1
	cmp	w4, w3
	beq	L118
	mov	w0, 0
	ret
	.p2align 2,,3
L117:
	mov	w0, 1
	ret
LFE21:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt_types___elabs
_khepri_mpt_types___elabs:
LFB1:
	adrp	x3, _khepri_mpt_types__empty_children@PAGE
	stp	x29, x30, [sp, -32]!
LCFI28:
	mov	x2, 528
	add	x3, x3, _khepri_mpt_types__empty_children@PAGEOFF;
	mov	x29, sp
LCFI29:
	mov	x0, x3
	mov	w1, 0
	str	x19, [sp, 16]
LCFI30:
	bl	_memset
	movi	v31.4s, 0
	adrp	x4, _khepri_mpt_types__empty_node@PAGE
	mov	x1, x0
	add	x19, x4, _khepri_mpt_types__empty_node@PAGEOFF;
	mov	x2, 528
	add	x0, x19, 108
	strb	wzr, [x4, #_khepri_mpt_types__empty_node@PAGEOFF]
	str	wzr, [x19, 68]
	str	q31, [x19, 4]
	str	q31, [x19, 20]
	str	q31, [x19, 36]
	str	q31, [x19, 52]
	str	q31, [x19, 72]
	str	q31, [x19, 88]
	str	wzr, [x19, 104]
	bl	_memcpy
	movi	v31.4s, 0
	add	x0, x19, 512
	strb	wzr, [x19, 668]
	str	q31, [x0, 124]
	str	q31, [x0, 140]
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI31:
	ret
LFE1:
	.const
_mpt_errorG.0:
	.byte	6
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	6
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	2
	.byte	0
	.byte	5
	.byte	0
	.align	1
_mpt_errorT2.1:
	.byte	15
	.byte	8
	.align	1
_mpt_errorT1.2:
	.byte	16
	.byte	1
	.align	3
_mpt_errorP.3:
	.word	7
	.word	15
	.align	3
_node_kindG.4:
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.space 7
	.globl _khepri_mpt_types__mpt_errorN
	.align	1
_khepri_mpt_types__mpt_errorN:
	.hword	1
	.hword	11
	.hword	30
	.hword	48
	.hword	67
	.hword	86
	.hword	101
	.hword	118
	.hword	132
	.globl _khepri_mpt_types__mpt_errorS
	.align	3
_khepri_mpt_types__mpt_errorS:
	.ascii "ERROR_NONEERROR_KEY_NOT_FOUNDERROR_INVALID_NODEERROR_INVALID_PROOFERROR_HASH_MISMATCHERROR_TRIE_FULLERROR_INVALID_RLPERROR_OVERFLOW"
	.globl _khepri_mpt_types__empty_node
	.zerofill __DATA,__common,_khepri_mpt_types__empty_node,672,4
	.globl _khepri_mpt_types__node_kindN
	.const
	.align	3
_khepri_mpt_types__node_kindN:
	.byte	1
	.byte	11
	.byte	20
	.byte	34
	.byte	45
	.space 3
	.globl _khepri_mpt_types__node_kindS
	.align	3
_khepri_mpt_types__node_kindS:
	.ascii "NODE_EMPTYNODE_LEAFNODE_EXTENSIONNODE_BRANCH"
	.globl _khepri_mpt_types__empty_children
	.zerofill __DATA,__common,_khepri_mpt_types__empty_children,528,0
	.globl _khepri_mpt_types__empty_value
	.const
	.align	2
_khepri_mpt_types__empty_value:
	.space 36
	.globl _khepri_mpt_types__null_node_hash
_khepri_mpt_types__null_node_hash:
	.space 33
	.globl _khepri_mpt_types__empty_hash
_khepri_mpt_types__empty_hash:
	.space 32
	.globl _khepri_mpt_types__empty_nibble_key
	.align	2
_khepri_mpt_types__empty_nibble_key:
	.space 68
	.globl _khepri_mpt_types_E
	.data
	.align	1
_khepri_mpt_types_E:
	.space 2
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
	.quad	LFB2-.
	.set L$set$2,LFE2-LFB2
	.quad L$set$2
	.uleb128 0
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
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$5,LEFDE5-LASFDE5
	.long L$set$5
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB4-.
	.set L$set$6,LFE4-LFB4
	.quad L$set$6
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$7,LEFDE7-LASFDE7
	.long L$set$7
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB59-.
	.set L$set$8,LFE59-LFB59
	.quad L$set$8
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$9,LEFDE9-LASFDE9
	.long L$set$9
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB6-.
	.set L$set$10,LFE6-LFB6
	.quad L$set$10
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$11,LEFDE11-LASFDE11
	.long L$set$11
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB61-.
	.set L$set$12,LFE61-LFB61
	.quad L$set$12
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$13,LEFDE13-LASFDE13
	.long L$set$13
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB8-.
	.set L$set$14,LFE8-LFB8
	.quad L$set$14
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$15,LEFDE15-LASFDE15
	.long L$set$15
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB9-.
	.set L$set$16,LFE9-LFB9
	.quad L$set$16
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$17,LEFDE17-LASFDE17
	.long L$set$17
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB63-.
	.set L$set$18,LFE63-LFB63
	.quad L$set$18
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$19,LEFDE19-LASFDE19
	.long L$set$19
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB11-.
	.set L$set$20,LFE11-LFB11
	.quad L$set$20
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$21,LEFDE21-LASFDE21
	.long L$set$21
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB65-.
	.set L$set$22,LFE65-LFB65
	.quad L$set$22
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$23,LEFDE23-LASFDE23
	.long L$set$23
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB13-.
	.set L$set$24,LFE13-LFB13
	.quad L$set$24
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$25,LEFDE25-LASFDE25
	.long L$set$25
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB67-.
	.set L$set$26,LFE67-LFB67
	.quad L$set$26
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$27,LEFDE27-LASFDE27
	.long L$set$27
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB15-.
	.set L$set$28,LFE15-LFB15
	.quad L$set$28
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$29,LEFDE29-LASFDE29
	.long L$set$29
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB16-.
	.set L$set$30,LFE16-LFB16
	.quad L$set$30
	.uleb128 0
	.byte	0x4
	.set L$set$31,LCFI0-LFB16
	.long L$set$31
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$32,LCFI1-LCFI0
	.long L$set$32
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$33,LCFI2-LCFI1
	.long L$set$33
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$34,LCFI3-LCFI2
	.long L$set$34
	.byte	0xb
	.byte	0x4
	.set L$set$35,LCFI4-LCFI3
	.long L$set$35
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI5-LCFI4
	.long L$set$36
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$37,LEFDE31-LASFDE31
	.long L$set$37
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB17-.
	.set L$set$38,LFE17-LFB17
	.quad L$set$38
	.uleb128 0
	.byte	0x4
	.set L$set$39,LCFI6-LFB17
	.long L$set$39
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$40,LCFI7-LCFI6
	.long L$set$40
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$41,LCFI8-LCFI7
	.long L$set$41
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$42,LCFI9-LCFI8
	.long L$set$42
	.byte	0xb
	.byte	0x4
	.set L$set$43,LCFI10-LCFI9
	.long L$set$43
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI11-LCFI10
	.long L$set$44
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$45,LEFDE33-LASFDE33
	.long L$set$45
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB18-.
	.set L$set$46,LFE18-LFB18
	.quad L$set$46
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI12-LFB18
	.long L$set$47
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$48,LCFI13-LCFI12
	.long L$set$48
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$49,LCFI14-LCFI13
	.long L$set$49
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI15-LCFI14
	.long L$set$50
	.byte	0xb
	.byte	0x4
	.set L$set$51,LCFI16-LCFI15
	.long L$set$51
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$52,LCFI17-LCFI16
	.long L$set$52
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$53,LEFDE35-LASFDE35
	.long L$set$53
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB19-.
	.set L$set$54,LFE19-LFB19
	.quad L$set$54
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI18-LFB19
	.long L$set$55
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$56,LCFI19-LCFI18
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$57,LCFI20-LCFI19
	.long L$set$57
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$58,LCFI21-LCFI20
	.long L$set$58
	.byte	0xb
	.byte	0x4
	.set L$set$59,LCFI22-LCFI21
	.long L$set$59
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$60,LCFI23-LCFI22
	.long L$set$60
	.byte	0xb
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$61,LEFDE37-LASFDE37
	.long L$set$61
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB20-.
	.set L$set$62,LFE20-LFB20
	.quad L$set$62
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI24-LFB20
	.long L$set$63
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$64,LCFI25-LCFI24
	.long L$set$64
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$65,LCFI26-LCFI25
	.long L$set$65
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$66,LCFI27-LCFI26
	.long L$set$66
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$67,LEFDE39-LASFDE39
	.long L$set$67
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB21-.
	.set L$set$68,LFE21-LFB21
	.quad L$set$68
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$69,LEFDE41-LASFDE41
	.long L$set$69
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB1-.
	.set L$set$70,LFE1-LFB1
	.quad L$set$70
	.uleb128 0
	.byte	0x4
	.set L$set$71,LCFI28-LFB1
	.long L$set$71
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$72,LCFI29-LCFI28
	.long L$set$72
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$73,LCFI30-LCFI29
	.long L$set$73
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$74,LCFI31-LCFI30
	.long L$set$74
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE41:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
