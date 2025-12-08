	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__hardware_tierH
_anubis_genesis_provers__hardware_tierH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L3
	ldrb	w1, [x0]
	mov	w0, 52429
	movk	w0, 0xcccc, lsl 16
	add	w3, w1, w1, lsl 1
	lsl	w2, w1, 3
	sub	w2, w2, w1
	umull	x1, w3, w0
	umull	x0, w2, w0
	lsr	x1, x1, 35
	lsr	x0, x0, 35
	add	w1, w1, w1, lsl 2
	add	w0, w0, w0, lsl 2
	sub	w1, w3, w1, lsl 1
	sub	w0, w2, w0, lsl 1
	adrp	x2, _hardware_tierG.9@PAGE
	add	x2, x2, _hardware_tierG.9@PAGEOFF;
	sxtw	x1, w1
	sxtw	x0, w0
	ldrb	w1, [x2, x1]
	ldrb	w0, [x2, x0]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L3:
	adrp	x2, _hardware_tierG.9@PAGE
	mov	x1, 0
	add	x2, x2, _hardware_tierG.9@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__prover_statusH
_anubis_genesis_provers__prover_statusH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L8
	sub	w1, w1, w2
	cmp	w1, 2
	bgt	L9
L8:
	mov	x3, 0
	mov	x1, 0
L6:
	adrp	x0, _prover_statusG.5@PAGE
	mov	w2, 7
	add	x0, x0, _prover_statusG.5@PAGEOFF;
	ldrb	w1, [x0, x1]
	ldrb	w0, [x0, x3]
	add	w1, w1, w0
	udiv	w2, w1, w2
	lsl	w0, w2, 3
	sub	w0, w0, w2
	sub	w0, w1, w0
	ret
	.p2align 2,,3
L9:
	ldrb	w0, [x0, 3]
	mov	w1, 36409
	movk	w1, 0x38e3, lsl 16
	add	w2, w0, w0, lsl 1
	lsl	w4, w0, 4
	sub	w4, w4, w0
	add	w0, w0, w2, lsl 2
	umull	x3, w4, w1
	umull	x1, w0, w1
	lsr	x3, x3, 34
	add	w3, w3, w3, lsl 3
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 3
	sub	w3, w4, w3, lsl 1
	sub	w1, w0, w1, lsl 1
	sxtw	x3, w3
	sxtw	x1, w1
	b	L6
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__prover_recordIP
_anubis_genesis_provers__prover_recordIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__Tprover_arrayBIP
_anubis_genesis_provers__Tprover_arrayBIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__prover_set_stateIP
_anubis_genesis_provers__prover_set_stateIP:
LFB63:
	ret
LFE63:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__register_resultH
_anubis_genesis_provers__register_resultH:
LFB7:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L16
	sub	w1, w1, w2
	cmp	w1, 7
	bgt	L17
L16:
	mov	x3, 0
	mov	x0, 0
L14:
	adrp	x2, _register_resultG.1@PAGE
	mov	w1, 52429
	add	x2, x2, _register_resultG.1@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x2, x0]
	ldrb	w2, [x2, x3]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
	.p2align 2,,3
L17:
	ldrb	w3, [x0, 8]
	mov	w1, 60495
	movk	w1, 0x4ec4, lsl 16
	add	w0, w3, w3, lsl 1
	lsl	w3, w3, 3
	umull	x2, w3, w1
	umull	x1, w0, w1
	lsr	x2, x2, 34
	lsr	x1, x1, 34
	add	w5, w2, w2, lsl 1
	add	w4, w1, w1, lsl 1
	add	w2, w2, w5, lsl 2
	add	w1, w1, w4, lsl 2
	sub	w3, w3, w2
	sub	w0, w0, w1
	sxtw	x3, w3
	sxtw	x0, w0
	b	L14
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__init_prover_set
_anubis_genesis_provers__init_prover_set:
LFB8:
	movi	v31.4s, 0
	mov	x2, 0
	.p2align 5,,15
L19:
	add	x3, x2, x2, lsl 2
	lsl	x3, x3, 5
	add	x4, x0, x3
	str	q31, [x0, x3]
	stp	q31, q31, [x4, 16]
	stp	q31, q31, [x4, 48]
	stp	q31, q31, [x4, 80]
	stp	q31, q31, [x4, 112]
	str	q31, [x4, 144]
	str	w2, [x0, x3]
	add	x2, x2, 1
	cmp	x2, 50
	bne	L19
	adrp	x2, lC2@PAGE
	add	x3, x0, 8192
	str	xzr, [x0, 8000]
	ldr	q31, [x2, #lC2@PAGEOFF]
	add	x2, x0, 4096
	str	q31, [x3, -184]
	strb	wzr, [x2, 3928]
	str	x1, [x0, 8032]
	str	x1, [x0, 8040]
	ret
LFE8:
	.const
	.align	3
lC3:
	.ascii "failed precondition from anubis_genesis_provers.ads:151"
	.align	3
lC4:
	.ascii "anubis_genesis_provers.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__register_prover
_anubis_genesis_provers__register_prover:
LFB10:
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	x29, sp
LCFI1:
	mov	x9, x0
	mov	x8, 0
	ldp	w10, w13, [x2]
	ldp	w4, w30, [x4]
	sxtw	x17, w10
	sxtw	x11, w13
	add	x2, x17, 31
	sxtw	x16, w4
	cmp	x2, x11
	add	x0, x16, 31
	sxtw	x12, w30
	ccmp	x0, x12, 0, eq
	bne	L67
	.p2align 5,,15
L22:
	add	x2, x8, x8, lsl 2
	add	x2, x9, x2, lsl 5
	ldrb	w0, [x2, 68]
	cmp	w0, 6
	bhi	L68
	cbnz	w0, L24
	ldr	x2, [x2, 136]
	cbz	x2, L25
L24:
	add	x8, x8, 1
	cmp	x8, 50
	bne	L22
L49:
	mov	w0, 1
L50:
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
	.p2align 2,,3
L25:
LCFI3:
	cmp	w7, 1
	bhi	L28
	cbz	w7, L27
	ldr	w2, [x9, 8004]
	tbnz	w2, #31, L28
	cmp	w2, 49
	bgt	L49
L27:
	sbfiz	x14, x8, 2, 32
	add	x2, x14, w8, sxtw
	lsl	x2, x2, 5
	sxtw	x15, w8
	str	w8, [x9, x2]
	cmp	w10, w13
	bgt	L29
	mov	w13, 160
	sub	x1, x1, x17
	sub	x2, x17, #1
	umull	x13, w15, w13
	sub	x13, x13, x17
	add	x13, x13, 4
	add	x13, x9, x13
	.p2align 5,,15
L35:
	add	x2, x2, 1
	subs	w8, w2, w10
	bvs	L31
	cmp	w8, 31
	bgt	L33
	bhi	L69
	ldrb	w8, [x1, x2]
	strb	w8, [x13, x2]
L33:
	cmp	x11, x2
	bne	L35
L29:
	cmp	w4, w30
	bgt	L36
	mov	w8, 160
	sub	x3, x3, x16
	sub	x1, x16, #1
	umull	x8, w15, w8
	sub	x8, x8, x16
	add	x8, x8, 36
	add	x8, x9, x8
	.p2align 5,,15
L42:
	add	x1, x1, 1
	subs	w2, w1, w4
	bvs	L38
	cmp	w2, 31
	bgt	L40
	bhi	L70
	ldrb	w2, [x3, x1]
	strb	w2, [x8, x1]
L40:
	cmp	x12, x1
	bne	L42
L36:
	add	x1, x14, x15
	mov	w2, 1
	add	x1, x9, x1, lsl 5
	strb	w2, [x1, 68]
	cmp	w5, 3
	bhi	L71
	ldr	x2, [x9, 8040]
	strb	w5, [x1, 69]
	strb	w7, [x1, 70]
	str	x6, [x1, 112]
	str	x2, [x1, 136]
	cbz	w7, L50
	ldr	w1, [x9, 8004]
	tbnz	w1, #31, L72
	mov	w2, 2147483647
	cmp	w1, w2
	beq	L73
	add	w1, w1, 1
	str	w1, [x9, 8004]
	ldp	x29, x30, [sp], 16
LCFI4:
	ret
L68:
LCFI5:
	adrp	x0, lC4@PAGE
	mov	w1, 72
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L38:
	adrp	x0, lC4@PAGE
	mov	w1, 104
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L31:
	adrp	x0, lC4@PAGE
	mov	w1, 97
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L67:
	adrp	x0, lC3@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC3@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L28:
	adrp	x0, lC4@PAGE
	mov	w1, 87
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L71:
	adrp	x0, lC4@PAGE
	mov	w1, 110
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L70:
	adrp	x0, lC4@PAGE
	mov	w1, 105
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L73:
	adrp	x0, lC4@PAGE
	mov	w1, 116
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L72:
	adrp	x0, lC4@PAGE
	mov	w1, 116
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L69:
	adrp	x0, lC4@PAGE
	mov	w1, 98
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE10:
	.const
	.align	2
lC1:
	.word	1
	.word	55
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__record_proof
_anubis_genesis_provers__record_proof:
LFB11:
	cmp	w1, 49
	bhi	L76
	sbfiz	x2, x1, 2, 32
	add	x1, x2, w1, sxtw
	add	x0, x0, x1, lsl 5
	ldrb	w1, [x0, 68]
	cmp	w1, 6
	bhi	L76
	sub	w1, w1, #2
	and	w1, w1, 255
	cmp	w1, 1
	bhi	L74
	ldr	x2, [x0, 72]
	mov	w1, 2
	str	x4, [x0, 152]
	ldr	w4, [x0, 88]
	strb	w1, [x0, 68]
	add	x2, x2, 1
	add	w1, w4, w3
	cmp	w4, 0
	str	x2, [x0, 72]
	lsr	w1, w1, 1
	csel	w3, w1, w3, ne
	str	w3, [x0, 88]
L74:
	ret
L76:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI6:
	mov	w1, 136
	mov	x29, sp
LCFI7:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE11:
	.const
	.align	3
lC5:
	.ascii "failed precondition from anubis_genesis_provers.ads:173"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__record_proof_failed
_anubis_genesis_provers__record_proof_failed:
LFB12:
	stp	x29, x30, [sp, -16]!
LCFI8:
	mov	x29, sp
LCFI9:
	ldpsw	x2, x3, [x3]
	add	x2, x2, 31
	cmp	x2, x3
	bne	L89
	cmp	w1, 49
	bhi	L86
	sbfiz	x2, x1, 2, 32
	add	x1, x2, w1, sxtw
	add	x0, x0, x1, lsl 5
	ldrb	w1, [x0, 68]
	cmp	w1, 6
	bhi	L86
	sub	w1, w1, #2
	and	w1, w1, 255
	cmp	w1, 1
	bhi	L83
	ldr	x1, [x0, 80]
	add	x1, x1, 1
	str	x1, [x0, 80]
L83:
	ldp	x29, x30, [sp], 16
LCFI10:
	ret
L86:
LCFI11:
	adrp	x0, lC4@PAGE
	mov	w1, 162
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L89:
	adrp	x0, lC5@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE12:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__calculate_proof_reward
_anubis_genesis_provers__calculate_proof_reward:
LFB13:
	cmp	w1, 3
	bhi	L92
	adrp	x3, _anubis_genesis_provers__tier_reward_multiplier@PAGE
	uxtw	x1, w1
	add	x3, x3, _anubis_genesis_provers__tier_reward_multiplier@PAGEOFF;
	ldr	w1, [x3, x1, lsl 2]
	tbnz	w1, #31, L92
	mov	w5, 100
	mov	x4, 22859
	ldr	x3, [x0, 8016]
	movk	x4, 0x3886, lsl 16
	lsr	w2, w2, 10
	umull	x0, w1, w5
	movk	x4, 0xc5d6, lsl 32
	movk	x4, 0x346d, lsl 48
	umulh	x0, x0, x4
	add	x0, x2, x0, lsr 11
	cmp	x0, x3
	csel	x0, x0, x3, ls
	ret
L92:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI12:
	mov	w1, 180
	mov	x29, sp
LCFI13:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__distribute_proof_reward
_anubis_genesis_provers__distribute_proof_reward:
LFB14:
	ldr	x3, [x0, 8016]
	cmp	x3, x2
	bcc	L95
	ldr	x4, [x0, 8008]
	sub	x3, x3, x2
	str	x3, [x0, 8016]
	add	x3, x4, x2
	str	x3, [x0, 8008]
	cmp	w1, 49
	bhi	L101
	sbfiz	x3, x1, 2, 32
	add	x1, x3, w1, sxtw
	add	x0, x0, x1, lsl 5
	ldr	x1, [x0, 104]
	add	x2, x2, x1
	str	x2, [x0, 104]
L95:
	ret
L101:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI14:
	mov	w1, 209
	mov	x29, sp
LCFI15:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__claim_rewards
_anubis_genesis_provers__claim_rewards:
LFB15:
	cmp	w1, 49
	bhi	L110
	sbfiz	x2, x1, 2, 32
	add	x1, x2, w1, sxtw
	add	x2, x0, x1, lsl 5
	mov	w1, 0
	ldr	x0, [x2, 104]
	cbnz	x0, L111
	and	x1, x1, 1
	ret
	.p2align 2,,3
L111:
	ldr	x3, [x2, 96]
	mov	w1, 1
	and	x1, x1, 1
	add	x3, x0, x3
	stp	x3, xzr, [x2, 96]
	ret
L110:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI16:
	mov	w1, 220
	mov	x29, sp
LCFI17:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE15:
	.const
	.align	3
lC6:
	.ascii "anubis_genesis_provers.ads"
	.space 1
	.align	3
lC7:
	.ascii "failed precondition from anubis_genesis_provers.ads:212"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__slash_prover
_anubis_genesis_provers__slash_prover:
LFB16:
	stp	x29, x30, [sp, -16]!
LCFI18:
	mov	x29, sp
LCFI19:
	ldp	w3, w4, [x4]
	tbnz	w2, #31, L125
	sxtw	x3, w3
	mov	w5, 10000
	add	x3, x3, 31
	cmp	x3, w4, sxtw
	ccmp	w2, w5, 0, eq
	bgt	L126
	cmp	w1, 49
	bhi	L127
	sbfiz	x3, x1, 2, 32
	add	x1, x3, w1, sxtw
	add	x1, x0, x1, lsl 5
	sxtw	x2, w2
	mov	x5, 22859
	ldr	x3, [x1, 112]
	movk	x5, 0x3886, lsl 16
	movk	x5, 0xc5d6, lsl 32
	movk	x5, 0x346d, lsl 48
	ldr	w4, [x1, 120]
	mul	x2, x2, x3
	umulh	x2, x2, x5
	lsr	x2, x2, 11
	subs	x3, x3, x2
	csel	x3, x3, xzr, hi
	str	x3, [x1, 112]
	tbnz	w4, #31, L128
	mov	w5, 2147483647
	cmp	w4, w5
	beq	L129
	ldr	x5, [x1, 128]
	add	w4, w4, 1
	str	w4, [x1, 120]
	add	x2, x2, x5
	str	x2, [x1, 128]
	cbnz	x3, L112
	ldr	w2, [x0, 8000]
	mov	w3, 5
	strb	w3, [x1, 68]
	cmp	w2, 0
	blt	L130
	beq	L112
	sub	w2, w2, #1
	str	w2, [x0, 8000]
L112:
	ldp	x29, x30, [sp], 16
LCFI20:
	ret
L129:
LCFI21:
	adrp	x0, lC4@PAGE
	mov	w1, 260
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L128:
	adrp	x0, lC4@PAGE
	mov	w1, 260
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L127:
	adrp	x0, lC4@PAGE
	mov	w1, 248
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L126:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L125:
	adrp	x0, lC6@PAGE
	mov	w1, 213
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L130:
	adrp	x0, lC4@PAGE
	mov	w1, 267
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__get_prover
_anubis_genesis_provers__get_prover:
LFB17:
	cmp	w1, 49
	bhi	L136
	sbfiz	x2, x1, 2, 32
	add	x1, x2, w1, sxtw
	lsl	x1, x1, 5
	add	x2, x0, x1
	ldr	q31, [x0, x1]
	ldp	q30, q29, [x2, 16]
	ldr	q28, [x2, 48]
	stp	q31, q30, [x8]
	ldp	q31, q30, [x2, 64]
	stp	q29, q28, [x8, 32]
	ldp	q29, q28, [x2, 96]
	stp	q31, q30, [x8, 64]
	ldp	q31, q30, [x2, 128]
	stp	q29, q28, [x8, 96]
	stp	q31, q30, [x8, 128]
	ret
L136:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI22:
	mov	w1, 282
	mov	x29, sp
LCFI23:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE17:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__get_active_count
_anubis_genesis_provers__get_active_count:
LFB18:
	ldr	w0, [x0, 8000]
	tbnz	w0, #31, L142
	ret
L142:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI24:
	mov	w1, 289
	mov	x29, sp
LCFI25:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE18:
	.const
	.align	3
lC8:
	.ascii "failed precondition from anubis_genesis_provers.ads:239"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__enable_open_entry
_anubis_genesis_provers__enable_open_entry:
LFB19:
	mov	x2, 6559
	movk	x2, 0x28, lsl 16
	cmp	x1, x2
	bls	L148
	add	x0, x0, 4096
	mov	w1, 1
	strb	w1, [x0, 3928]
	ret
L148:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI26:
	add	x0, x0, lC8@PAGEOFF;
	mov	x29, sp
LCFI27:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__zeroize_prover_set
_anubis_genesis_provers__zeroize_prover_set:
LFB20:
	movi	v31.4s, 0
	mov	x3, 0
	add	x1, x0, 68
	.p2align 5,,15
L150:
	add	x4, x3, x3, lsl 2
	add	x3, x3, 1
	add	x1, x1, 160
	lsl	x4, x4, 5
	add	x2, x0, x4
	add	x6, x2, 4
	add	x5, x2, 36
	str	wzr, [x0, x4]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	stp	xzr, xzr, [x5]
	stp	xzr, xzr, [x5, 16]
	strh	wzr, [x1, -160]
	strb	wzr, [x2, 70]
	str	q31, [x1, -156]
	str	wzr, [x2, 88]
	str	q31, [x1, -132]
	str	xzr, [x2, 112]
	str	wzr, [x2, 120]
	str	q31, [x1, -100]
	str	q31, [x1, -84]
	cmp	x3, 50
	bne	L150
	add	x2, x0, 8192
	add	x1, x0, 4096
	str	xzr, [x0, 8000]
	str	q31, [x2, -184]
	strb	wzr, [x1, 3928]
	str	q31, [x0, 8032]
	ret
LFE20:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_provers__zeroize_prover
_anubis_genesis_provers__zeroize_prover:
LFB21:
	movi	v31.4s, 0
	mov	x1, x0
	add	x2, x0, 36
	str	wzr, [x1], 4
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	strh	wzr, [x0, 68]
	strb	wzr, [x0, 70]
	str	q31, [x0, 72]
	str	wzr, [x0, 88]
	str	q31, [x0, 96]
	str	xzr, [x0, 112]
	str	wzr, [x0, 120]
	stp	q31, q31, [x0, 128]
	ret
LFE21:
	.const
	.align	3
_register_resultG.1:
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.space 3
_prover_statusG.5:
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	2
	.byte	0
	.byte	5
	.align	3
_hardware_tierG.9:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	3
	.byte	0
	.byte	1
	.space 6
	.globl _anubis_genesis_provers__register_resultN
	.align	3
_anubis_genesis_provers__register_resultN:
	.byte	1
	.byte	11
	.byte	23
	.byte	41
	.byte	57
	.byte	74
	.space 2
	.globl _anubis_genesis_provers__register_resultS
	.align	3
_anubis_genesis_provers__register_resultS:
	.ascii "REGISTEREDNOT_APPROVEDINSUFFICIENT_STAKEINVALID_HARDWAREINVALID_SIGNATURE"
	.globl _anubis_genesis_provers__prover_statusN
	.align	3
_anubis_genesis_provers__prover_statusN:
	.byte	1
	.byte	20
	.byte	28
	.byte	34
	.byte	38
	.byte	47
	.byte	54
	.byte	61
	.globl _anubis_genesis_provers__prover_statusS
	.align	3
_anubis_genesis_provers__prover_statusS:
	.ascii "PENDING_APPLICATIONAPPROVEDACTIVEBUSYSUSPENDEDSLASHEDREMOVED"
	.globl _anubis_genesis_provers__tier_reward_multiplier
	.align	3
_anubis_genesis_provers__tier_reward_multiplier:
	.word	5000
	.word	10000
	.word	15000
	.word	20000
	.globl _anubis_genesis_provers__tier_ram_requirements
	.align	3
_anubis_genesis_provers__tier_ram_requirements:
	.word	16
	.word	32
	.word	64
	.word	128
	.globl _anubis_genesis_provers__hardware_tierN
	.align	3
_anubis_genesis_provers__hardware_tierN:
	.byte	1
	.byte	6
	.byte	14
	.byte	19
	.byte	29
	.space 3
	.globl _anubis_genesis_provers__hardware_tierS
	.align	3
_anubis_genesis_provers__hardware_tierS:
	.ascii "LIGHTSTANDARDHEAVYENTERPRISE"
	.globl _anubis_genesis_provers_E
	.data
	.align	1
_anubis_genesis_provers_E:
	.space 2
	.literal16
	.align	4
lC2:
	.xword	0
	.xword	80000000
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
	.quad	LFB5-.
	.set L$set$8,LFE5-LFB5
	.quad L$set$8
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$9,LEFDE9-LASFDE9
	.long L$set$9
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB63-.
	.set L$set$10,LFE63-LFB63
	.quad L$set$10
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$11,LEFDE11-LASFDE11
	.long L$set$11
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB7-.
	.set L$set$12,LFE7-LFB7
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
	.quad	LFB10-.
	.set L$set$16,LFE10-LFB10
	.quad L$set$16
	.uleb128 0
	.byte	0x4
	.set L$set$17,LCFI0-LFB10
	.long L$set$17
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$18,LCFI1-LCFI0
	.long L$set$18
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$19,LCFI2-LCFI1
	.long L$set$19
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$20,LCFI3-LCFI2
	.long L$set$20
	.byte	0xb
	.byte	0x4
	.set L$set$21,LCFI4-LCFI3
	.long L$set$21
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$22,LCFI5-LCFI4
	.long L$set$22
	.byte	0xb
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$23,LEFDE17-LASFDE17
	.long L$set$23
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB11-.
	.set L$set$24,LFE11-LFB11
	.quad L$set$24
	.uleb128 0
	.byte	0x4
	.set L$set$25,LCFI6-LFB11
	.long L$set$25
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$26,LCFI7-LCFI6
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$27,LEFDE19-LASFDE19
	.long L$set$27
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB12-.
	.set L$set$28,LFE12-LFB12
	.quad L$set$28
	.uleb128 0
	.byte	0x4
	.set L$set$29,LCFI8-LFB12
	.long L$set$29
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$30,LCFI9-LCFI8
	.long L$set$30
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$31,LCFI10-LCFI9
	.long L$set$31
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$32,LCFI11-LCFI10
	.long L$set$32
	.byte	0xb
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$33,LEFDE21-LASFDE21
	.long L$set$33
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB13-.
	.set L$set$34,LFE13-LFB13
	.quad L$set$34
	.uleb128 0
	.byte	0x4
	.set L$set$35,LCFI12-LFB13
	.long L$set$35
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$36,LCFI13-LCFI12
	.long L$set$36
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$37,LEFDE23-LASFDE23
	.long L$set$37
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB14-.
	.set L$set$38,LFE14-LFB14
	.quad L$set$38
	.uleb128 0
	.byte	0x4
	.set L$set$39,LCFI14-LFB14
	.long L$set$39
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$40,LCFI15-LCFI14
	.long L$set$40
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$41,LEFDE25-LASFDE25
	.long L$set$41
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB15-.
	.set L$set$42,LFE15-LFB15
	.quad L$set$42
	.uleb128 0
	.byte	0x4
	.set L$set$43,LCFI16-LFB15
	.long L$set$43
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$44,LCFI17-LCFI16
	.long L$set$44
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$45,LEFDE27-LASFDE27
	.long L$set$45
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB16-.
	.set L$set$46,LFE16-LFB16
	.quad L$set$46
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI18-LFB16
	.long L$set$47
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$48,LCFI19-LCFI18
	.long L$set$48
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$49,LCFI20-LCFI19
	.long L$set$49
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI21-LCFI20
	.long L$set$50
	.byte	0xb
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$51,LEFDE29-LASFDE29
	.long L$set$51
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB17-.
	.set L$set$52,LFE17-LFB17
	.quad L$set$52
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI22-LFB17
	.long L$set$53
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$54,LCFI23-LCFI22
	.long L$set$54
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$55,LEFDE31-LASFDE31
	.long L$set$55
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB18-.
	.set L$set$56,LFE18-LFB18
	.quad L$set$56
	.uleb128 0
	.byte	0x4
	.set L$set$57,LCFI24-LFB18
	.long L$set$57
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$58,LCFI25-LCFI24
	.long L$set$58
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$59,LEFDE33-LASFDE33
	.long L$set$59
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB19-.
	.set L$set$60,LFE19-LFB19
	.quad L$set$60
	.uleb128 0
	.byte	0x4
	.set L$set$61,LCFI26-LFB19
	.long L$set$61
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$62,LCFI27-LCFI26
	.long L$set$62
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$63,LEFDE35-LASFDE35
	.long L$set$63
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB20-.
	.set L$set$64,LFE20-LFB20
	.quad L$set$64
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$65,LEFDE37-LASFDE37
	.long L$set$65
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB21-.
	.set L$set$66,LFE21-LFB21
	.quad L$set$66
	.uleb128 0
	.align	3
LEFDE37:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
