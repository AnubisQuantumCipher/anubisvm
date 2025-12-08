	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__reward_rangeIP
_anubis_developer_rewards__reward_rangeIP:
LFB2:
	mov	x0, 0
	mov	x1, 0
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__contribution_typeH
_anubis_developer_rewards__contribution_typeH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L6
	sub	w1, w1, w2
	cmp	w1, 5
	bgt	L7
L6:
	mov	x3, 0
	mov	x1, 0
L4:
	adrp	x0, _contribution_typeG.17@PAGE
	mov	w2, 7
	add	x0, x0, _contribution_typeG.17@PAGEOFF;
	ldrb	w1, [x0, x1]
	ldrb	w0, [x0, x3]
	add	w1, w1, w0
	udiv	w2, w1, w2
	lsl	w0, w2, 3
	sub	w0, w0, w2
	sub	w0, w1, w0
	ret
	.p2align 2,,3
L7:
	ldrb	w0, [x0, 6]
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
	b	L4
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__verification_typeH
_anubis_developer_rewards__verification_typeH:
LFB4:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L10
	ldrb	w0, [x0]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w4, w0, w0, lsl 1
	add	w0, w0, w0, lsl 3
	umull	x2, w4, w1
	umull	x1, w0, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	add	w5, w2, w2, lsl 2
	add	w3, w1, w1, lsl 2
	add	w2, w2, w5, lsl 1
	add	w1, w1, w3, lsl 1
	sub	w4, w4, w2
	sub	w0, w0, w1
	sxtw	x4, w4
	sxtw	x2, w0
L9:
	adrp	x3, _verification_typeG.13@PAGE
	mov	w1, 52429
	add	x3, x3, _verification_typeG.13@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, x4]
	ldrb	w2, [x3, x2]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
	.p2align 2,,3
L10:
	mov	x2, 0
	mov	x4, 0
	b	L9
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__contribution_statusH
_anubis_developer_rewards__contribution_statusH:
LFB5:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L13
	ldrb	w0, [x0]
	mov	w1, 14
	add	w2, w0, w0, lsl 1
	lsl	w4, w0, 3
	sub	w4, w4, w0
	add	w2, w0, w2, lsl 2
	udiv	w0, w4, w1
	udiv	w3, w2, w1
	msub	w0, w0, w1, w4
	sxtw	x0, w0
	msub	w3, w3, w1, w2
	sxtw	x3, w3
L12:
	adrp	x2, _contribution_statusG.9@PAGE
	mov	w1, 43691
	add	x2, x2, _contribution_statusG.9@PAGEOFF;
	movk	w1, 0xaaaa, lsl 16
	ldrb	w0, [x2, x0]
	ldrb	w2, [x2, x3]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 1
	sub	w0, w0, w1, lsl 1
	ret
	.p2align 2,,3
L13:
	mov	x3, 0
	mov	x0, 0
	b	L12
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__contribution_recordIP
_anubis_developer_rewards__contribution_recordIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__contribution_arrayIP
_anubis_developer_rewards__contribution_arrayIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__developer_ecosystem_stateIP
_anubis_developer_rewards__developer_ecosystem_stateIP:
LFB84:
	ret
LFE84:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__submit_resultH
_anubis_developer_rewards__submit_resultH:
LFB9:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L19
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
L18:
	adrp	x3, _submit_resultG.5@PAGE
	mov	w1, 52429
	add	x3, x3, _submit_resultG.5@PAGEOFF;
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
L19:
	mov	x2, 0
	mov	x0, 0
	b	L18
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__review_resultH
_anubis_developer_rewards__review_resultH:
LFB10:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L22
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
	adrp	x2, _review_resultG.1@PAGE
	sub	w0, w0, w1
	add	x2, x2, _review_resultG.1@PAGEOFF;
	sxtw	x3, w3
	sxtw	x1, w0
	ldrb	w1, [x2, x1]
	ldrb	w0, [x2, x3]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L22:
	adrp	x2, _review_resultG.1@PAGE
	mov	x3, 0
	add	x2, x2, _review_resultG.1@PAGEOFF;
	mov	x1, 0
	ldrb	w0, [x2, x3]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__init_developer_ecosystem
_anubis_developer_rewards__init_developer_ecosystem:
LFB11:
	adrp	x2, lC2@PAGE
	str	xzr, [x0, 16]
	str	wzr, [x0, 24]
	ldr	q31, [x2, #lC2@PAGEOFF]
	stp	x1, x1, [x0, 32]
	str	q31, [x0]
	ret
LFE11:
	.const
	.align	3
lC3:
	.ascii "failed precondition from anubis_developer_rewards.ads:181"
	.align	3
lC4:
	.ascii "anubis_developer_rewards.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__submit_contribution
_anubis_developer_rewards__submit_contribution:
LFB13:
	stp	x29, x30, [sp, -32]!
LCFI0:
	mov	x29, sp
LCFI1:
	mov	x16, x0
	stp	x19, x20, [sp, 16]
LCFI2:
	ldp	w10, w30, [x2]
	ldp	w11, w19, [x5]
	ldp	w7, w17, [x7]
	sxtw	x8, w10
	sxtw	x14, w30
	add	x0, x8, 31
	ldr	x12, [x29, 40]
	sxtw	x9, w11
	cmp	x0, x14
	add	x5, x9, 31
	sxtw	x15, w19
	sxtw	x2, w7
	ccmp	x5, x15, 0, eq
	add	x0, x2, 31
	sxtw	x13, w17
	ccmp	x0, x13, 0, eq
	bne	L59
	ldr	x0, [x16, 8]
	cbz	x0, L60
	adrp	x20, _anubis_developer_rewards__contribution_counter@PAGE
	mov	x5, x12
	ldr	x0, [x20, #_anubis_developer_rewards__contribution_counter@PAGEOFF]
	add	x0, x0, 1
	str	x0, [x5], 8
	stp	xzr, xzr, [x12, 8]
	str	x0, [x20, #_anubis_developer_rewards__contribution_counter@PAGEOFF]
	stp	xzr, xzr, [x5, 16]
	cmp	w10, w30
	bgt	L28
	sub	x0, x12, x8
	sub	x1, x1, x8
	add	x0, x0, 8
	sub	x5, x8, #1
	.p2align 5,,15
L34:
	add	x5, x5, 1
	subs	w8, w5, w10
	bvs	L30
	cmp	w8, 31
	bgt	L32
	bhi	L61
	ldrb	w8, [x1, x5]
	strb	w8, [x0, x5]
L32:
	cmp	x14, x5
	bne	L34
L28:
	cmp	w3, 6
	bhi	L62
	add	x0, x12, 42
	strb	w3, [x12, 40]
	strb	wzr, [x12, 41]
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w11, w19
	bgt	L36
	sub	x0, x12, x9
	sub	x4, x4, x9
	add	x0, x0, 42
	sub	x1, x9, #1
	.p2align 5,,15
L42:
	add	x1, x1, 1
	subs	w5, w1, w11
	bvs	L38
	cmp	w5, 31
	bgt	L40
	bhi	L63
	ldrb	w5, [x4, x1]
	strb	w5, [x0, x1]
L40:
	cmp	x15, x1
	bne	L42
L36:
	add	x0, x12, 74
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w7, w17
	bgt	L43
	sub	x0, x12, x2
	sub	x6, x6, x2
	add	x0, x0, 74
	sub	x1, x2, #1
	.p2align 5,,15
L49:
	add	x1, x1, 1
	subs	w2, w1, w7
	bvs	L45
	cmp	w2, 31
	bgt	L47
	bhi	L64
	ldrb	w2, [x6, x1]
	strb	w2, [x0, x1]
L47:
	cmp	x13, x1
	bne	L49
L43:
	ldp	w1, w6, [x29, 32]
	mov	x4, -3689348814741910324
	adrp	x0, _anubis_developer_rewards__contribution_rewards@PAGE
	movk	x4, 0xcccd, lsl 0
	ubfiz	x3, x3, 4, 8
	movi	v31.4s, 0
	add	x0, x0, _anubis_developer_rewards__contribution_rewards@PAGEOFF;
	ldr	x2, [x16, 16]
	mov	x11, -6148914691236517206
	movk	x11, 0xaaab, lsl 0
	mov	x10, 100
	ldr	x5, [x0, x3]
	mov	x9, 62915
	add	x7, x12, 106
	umulh	x1, x1, x4
	add	x4, x0, x3
	movk	x9, 0x5c28, lsl 16
	ldr	x4, [x4, 8]
	movk	x9, 0xc28f, lsl 32
	mov	w0, 0
	add	x3, x2, 1
	movk	x9, 0x28f5, lsl 48
	add	x1, x6, x1, lsr 3
	ldr	x8, [x16, 40]
	stp	xzr, xzr, [x7]
	cmp	x1, 303
	umulh	x2, x1, x11
	sub	x1, x4, x5
	stp	xzr, xzr, [x7, 16]
	lsr	x2, x2, 1
	csel	x2, x2, x10, cc
	mul	x1, x1, x2
	ldr	w2, [x29, 32]
	str	q31, [x12, 154]
	lsr	x1, x1, 2
	str	q31, [x12, 138]
	str	q31, [x12, 168]
	umulh	x1, x1, x9
	stp	w2, wzr, [x12, 184]
	ldr	w2, [x29, 36]
	add	x1, x5, x1, lsr 2
	stp	xzr, x8, [x12, 216]
	cmp	x1, x4
	csel	x1, x1, x4, ls
	stp	x1, xzr, [x12, 200]
	stp	w2, wzr, [x12, 192]
	stp	xzr, xzr, [x12, 232]
	str	x3, [x16, 16]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI3:
	ret
	.p2align 2,,3
L60:
LCFI4:
	movi	v31.4s, 0
	mov	w1, 4
	mov	w0, 4
	str	xzr, [x12, 240]
	stp	q31, q31, [x12, 32]
	stp	q31, q31, [x12]
	strb	w1, [x12, 41]
	stp	q31, q31, [x12, 64]
	stp	q31, q31, [x12, 96]
	stp	q31, q31, [x12, 128]
	stp	q31, q31, [x12, 160]
	stp	q31, q31, [x12, 192]
	str	q31, [x12, 224]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI5:
	ret
L45:
LCFI6:
	adrp	x0, lC4@PAGE
	mov	w1, 106
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L30:
	adrp	x0, lC4@PAGE
	mov	w1, 85
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L38:
	adrp	x0, lC4@PAGE
	mov	w1, 97
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L61:
	adrp	x0, lC4@PAGE
	mov	w1, 86
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L63:
	adrp	x0, lC4@PAGE
	mov	w1, 98
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L64:
	adrp	x0, lC4@PAGE
	mov	w1, 107
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L59:
	adrp	x0, lC3@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC3@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L62:
	adrp	x0, lC4@PAGE
	mov	w1, 91
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.const
	.align	2
lC1:
	.word	1
	.word	57
	.text
	.const
	.align	3
lC5:
	.ascii "failed precondition from anubis_developer_rewards.ads:198"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__review_contribution
_anubis_developer_rewards__review_contribution:
LFB14:
	stp	x29, x30, [sp, -16]!
LCFI7:
	mov	x29, sp
LCFI8:
	mov	x1, x0
	ldpsw	x5, x7, [x2]
	ldpsw	x0, x2, [x6]
	add	x5, x5, 31
	cmp	x5, x7
	add	x0, x0, 31
	ccmp	x0, x2, 0, eq
	bne	L73
	ldrb	w0, [x1, 41]
	cmp	w0, 5
	bhi	L74
	cmp	w0, 1
	bhi	L68
	cmp	w3, 1
	bhi	L75
	cbz	w3, L70
	mov	w2, 3
	mov	w0, 0
	str	x4, [x1, 208]
	strb	w2, [x1, 41]
	ldp	x29, x30, [sp], 16
LCFI9:
	ret
	.p2align 2,,3
L70:
LCFI10:
	mov	w0, 4
	str	xzr, [x1, 208]
	strb	w0, [x1, 41]
L68:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI11:
	ret
L74:
LCFI12:
	adrp	x0, lC4@PAGE
	mov	w1, 150
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L73:
	adrp	x0, lC5@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L75:
	adrp	x0, lC4@PAGE
	mov	w1, 157
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE14:
	.const
	.align	3
lC6:
	.ascii "failed precondition from anubis_developer_rewards.ads:208"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__community_vote__2
_anubis_developer_rewards__community_vote__2:
LFB15:
	stp	x29, x30, [sp, -16]!
LCFI13:
	mov	x29, sp
LCFI14:
	cmp	w4, 0
	ldpsw	x1, x2, [x2]
	add	x1, x1, 31
	ccmp	x1, x2, 0, ne
	bne	L82
	cmp	w3, 1
	bhi	L83
	cbnz	w3, L84
	ldr	w1, [x0, 220]
	add	w1, w1, w4
	str	w1, [x0, 220]
	ldp	x29, x30, [sp], 16
LCFI15:
	ret
	.p2align 2,,3
L84:
LCFI16:
	ldr	w1, [x0, 216]
	add	w1, w1, w4
	str	w1, [x0, 216]
	ldp	x29, x30, [sp], 16
LCFI17:
	ret
L83:
LCFI18:
	adrp	x0, lC4@PAGE
	mov	w1, 176
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L82:
	adrp	x0, lC6@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE15:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__calculate_reward
_anubis_developer_rewards__calculate_reward:
LFB16:
	stp	x29, x30, [sp, -16]!
LCFI19:
	mov	x29, sp
LCFI20:
	cmp	w0, 6
	bhi	L91
	mov	x5, -3689348814741910324
	uxtw	x1, w1
	movk	x5, 0xcccd, lsl 0
	adrp	x4, _anubis_developer_rewards__contribution_rewards@PAGE
	add	x4, x4, _anubis_developer_rewards__contribution_rewards@PAGEOFF;
	ubfiz	x0, x0, 4, 8
	umulh	x1, x1, x5
	add	x6, x4, x0
	ldr	x5, [x4, x0]
	uxtw	x2, w2
	ldr	x4, [x6, 8]
	lsr	x1, x1, 3
	tbnz	w3, #31, L92
	mov	x0, -6148914691236517206
	add	x3, x2, w3, sxtw
	add	x3, x3, x1
	movk	x0, 0xaaab, lsl 0
	sub	x2, x4, x5
	cmp	x3, 303
	mov	x1, 62915
	umulh	x0, x3, x0
	mov	x3, 100
	movk	x1, 0x5c28, lsl 16
	movk	x1, 0xc28f, lsl 32
	movk	x1, 0x28f5, lsl 48
	lsr	x0, x0, 1
	csel	x0, x0, x3, cc
	mul	x0, x0, x2
	lsr	x0, x0, 2
	umulh	x0, x0, x1
	add	x0, x5, x0, lsr 2
	cmp	x0, x4
	csel	x0, x0, x4, ls
	ldp	x29, x30, [sp], 16
LCFI21:
	ret
L91:
LCFI22:
	adrp	x0, lC4@PAGE
	mov	w1, 190
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L92:
	adrp	x0, lC4@PAGE
	mov	w1, 200
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE16:
	.const
	.align	3
lC7:
	.ascii "anubis_developer_rewards.ads"
	.space 1
	.align	3
lC8:
	.ascii "failed precondition from anubis_developer_rewards.ads:229"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__distribute_reward
_anubis_developer_rewards__distribute_reward:
LFB17:
	stp	x29, x30, [sp, -16]!
LCFI23:
	mov	x2, x0
	mov	x29, sp
LCFI24:
	ldrb	w0, [x1, 41]
	cmp	w0, 5
	bhi	L98
	ldr	x3, [x1, 208]
	cmp	x3, 0
	ccmp	w0, 3, 0, ne
	cset	w0, ne
	bne	L99
	ldr	x4, [x2, 8]
	cmp	x3, x4
	bls	L100
	ldp	x29, x30, [sp], 16
LCFI25:
	ret
	.p2align 2,,3
L100:
LCFI26:
	ldr	x5, [x2]
	sub	x4, x4, x3
	mov	w7, 5
	mov	w0, 1
	ldr	x6, [x2, 40]
	add	x3, x5, x3
	stp	x3, x4, [x2]
	strb	w7, [x1, 41]
	str	x6, [x1, 240]
	ldp	x29, x30, [sp], 16
LCFI27:
	ret
L99:
LCFI28:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L98:
	adrp	x0, lC7@PAGE
	mov	w1, 229
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE17:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__get_remaining_pool
_anubis_developer_rewards__get_remaining_pool:
LFB18:
	ldr	x0, [x0, 8]
	ret
LFE18:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__get_reward_range
_anubis_developer_rewards__get_reward_range:
LFB19:
	cmp	w0, 6
	bhi	L107
	adrp	x1, _anubis_developer_rewards__contribution_rewards@PAGE
	add	x1, x1, _anubis_developer_rewards__contribution_rewards@PAGEOFF;
	add	x0, x1, w0, uxtb 4
	ldp	x0, x1, [x0]
	ret
L107:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI29:
	mov	w1, 273
	mov	x29, sp
LCFI30:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__zeroize_ecosystem_state
_anubis_developer_rewards__zeroize_ecosystem_state:
LFB20:
	movi	v31.4s, 0
	str	xzr, [x0, 16]
	str	wzr, [x0, 24]
	str	q31, [x0]
	str	q31, [x0, 32]
	ret
LFE20:
	.align	2
	.p2align 5,,15
	.globl _anubis_developer_rewards__zeroize_contribution
_anubis_developer_rewards__zeroize_contribution:
LFB21:
	mov	x1, x0
	mov	w6, 1024
	movi	v31.4s, 0
	add	x5, x0, 42
	add	x4, x0, 74
	movi	v30.4s, 0
	str	xzr, [x1], 8
	add	x3, x0, 106
	stp	xzr, xzr, [x0, 8]
	stp	xzr, xzr, [x1, 16]
	strh	w6, [x0, 40]
	stp	xzr, xzr, [x5]
	stp	xzr, xzr, [x5, 16]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	str	q31, [x0, 154]
	str	q31, [x0, 138]
	str	q31, [x0, 168]
	str	xzr, [x0, 184]
	stp	wzr, wzr, [x0, 192]
	str	q30, [x0, 200]
	str	xzr, [x0, 216]
	str	q30, [x0, 224]
	str	xzr, [x0, 240]
	ret
LFE21:
	.const
	.align	3
_review_resultG.1:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.space 5
	.align	3
_submit_resultG.5:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	3
	.byte	2
	.byte	0
	.byte	1
	.space 3
	.align	3
_contribution_statusG.9:
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	5
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	1
	.space 2
	.align	3
_verification_typeG.13:
	.byte	0
	.byte	0
	.byte	4
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	2
	.space 5
_contribution_typeG.17:
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	3
	.byte	0
	.byte	2
	.byte	2
	.byte	0
	.byte	4
	.globl _anubis_developer_rewards__review_resultN
	.align	3
_anubis_developer_rewards__review_resultN:
	.byte	1
	.byte	9
	.byte	17
	.byte	31
	.byte	40
	.space 3
	.globl _anubis_developer_rewards__review_resultS
	.align	3
_anubis_developer_rewards__review_resultS:
	.ascii "APPROVEDREJECTEDNEEDS_REVISIONESCALATED"
	.globl _anubis_developer_rewards__submit_resultN
	.align	3
_anubis_developer_rewards__submit_resultN:
	.byte	1
	.byte	10
	.byte	22
	.byte	35
	.byte	51
	.byte	65
	.space 2
	.globl _anubis_developer_rewards__submit_resultS
	.align	3
_anubis_developer_rewards__submit_resultS:
	.ascii "SUBMITTEDINVALID_TYPEMISSING_PROOFDUPLICATE_COMMITPOOL_EXHAUSTED"
	.globl _anubis_developer_rewards__contribution_statusN
	.align	3
_anubis_developer_rewards__contribution_statusN:
	.byte	1
	.byte	10
	.byte	22
	.byte	42
	.byte	50
	.byte	58
	.byte	62
	.space 1
	.globl _anubis_developer_rewards__contribution_statusS
	.align	3
_anubis_developer_rewards__contribution_statusS:
	.ascii "SUBMITTEDUNDER_REVIEWVERIFICATION_PENDINGAPPROVEDREJECTEDPAID"
	.globl _anubis_developer_rewards__required_verification
	.align	3
_anubis_developer_rewards__required_verification:
	.byte	0
	.byte	1
	.byte	2
	.byte	3
	.byte	3
	.byte	3
	.byte	4
	.space 1
	.globl _anubis_developer_rewards__verification_typeN
	.align	3
_anubis_developer_rewards__verification_typeN:
	.byte	1
	.byte	17
	.byte	33
	.byte	53
	.byte	67
	.byte	85
	.space 2
	.globl _anubis_developer_rewards__verification_typeS
	.align	3
_anubis_developer_rewards__verification_typeS:
	.ascii "GNATPROVE_REVIEWFULL_SPARK_PROOFTEST_COVERAGE_REVIEWCOMMUNITY_VOTEAUDIT_VERIFICATION"
	.globl _anubis_developer_rewards__contribution_rewards
	.align	3
_anubis_developer_rewards__contribution_rewards:
	.xword	1000
	.xword	50000
	.xword	500
	.xword	10000
	.xword	200
	.xword	5000
	.xword	50
	.xword	500
	.xword	100
	.xword	1000
	.xword	100
	.xword	5000
	.xword	1000
	.xword	25000
	.globl _anubis_developer_rewards__contribution_typeN
	.align	3
_anubis_developer_rewards__contribution_typeN:
	.byte	1
	.byte	17
	.byte	32
	.byte	43
	.byte	56
	.byte	74
	.byte	84
	.byte	98
	.globl _anubis_developer_rewards__contribution_typeS
	.align	3
_anubis_developer_rewards__contribution_typeS:
	.ascii "CORE_PROTOCOL_PRKHEPRI_CONTRACTSDK_TOOLINGDOCUMENTATIONTUTORIAL_EDUCATIONBUG_REPORTSECURITY_AUDIT"
	.data
	.align	3
_anubis_developer_rewards__contribution_counter:
	.space 8
	.globl _anubis_developer_rewards_E
	.align	1
_anubis_developer_rewards_E:
	.space 2
	.literal16
	.align	4
lC2:
	.xword	0
	.xword	70000000
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
	.quad	LFB84-.
	.set L$set$14,LFE84-LFB84
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
	.quad	LFB10-.
	.set L$set$18,LFE10-LFB10
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
	.quad	LFB13-.
	.set L$set$22,LFE13-LFB13
	.quad L$set$22
	.uleb128 0
	.byte	0x4
	.set L$set$23,LCFI0-LFB13
	.long L$set$23
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$24,LCFI1-LCFI0
	.long L$set$24
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$25,LCFI2-LCFI1
	.long L$set$25
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$26,LCFI3-LCFI2
	.long L$set$26
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$27,LCFI4-LCFI3
	.long L$set$27
	.byte	0xb
	.byte	0x4
	.set L$set$28,LCFI5-LCFI4
	.long L$set$28
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$29,LCFI6-LCFI5
	.long L$set$29
	.byte	0xb
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$30,LEFDE23-LASFDE23
	.long L$set$30
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB14-.
	.set L$set$31,LFE14-LFB14
	.quad L$set$31
	.uleb128 0
	.byte	0x4
	.set L$set$32,LCFI7-LFB14
	.long L$set$32
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$33,LCFI8-LCFI7
	.long L$set$33
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$34,LCFI9-LCFI8
	.long L$set$34
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$35,LCFI10-LCFI9
	.long L$set$35
	.byte	0xb
	.byte	0x4
	.set L$set$36,LCFI11-LCFI10
	.long L$set$36
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$37,LCFI12-LCFI11
	.long L$set$37
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$38,LEFDE25-LASFDE25
	.long L$set$38
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB15-.
	.set L$set$39,LFE15-LFB15
	.quad L$set$39
	.uleb128 0
	.byte	0x4
	.set L$set$40,LCFI13-LFB15
	.long L$set$40
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$41,LCFI14-LCFI13
	.long L$set$41
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$42,LCFI15-LCFI14
	.long L$set$42
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$43,LCFI16-LCFI15
	.long L$set$43
	.byte	0xb
	.byte	0x4
	.set L$set$44,LCFI17-LCFI16
	.long L$set$44
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$45,LCFI18-LCFI17
	.long L$set$45
	.byte	0xb
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$46,LEFDE27-LASFDE27
	.long L$set$46
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB16-.
	.set L$set$47,LFE16-LFB16
	.quad L$set$47
	.uleb128 0
	.byte	0x4
	.set L$set$48,LCFI19-LFB16
	.long L$set$48
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$49,LCFI20-LCFI19
	.long L$set$49
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$50,LCFI21-LCFI20
	.long L$set$50
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$51,LCFI22-LCFI21
	.long L$set$51
	.byte	0xb
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$52,LEFDE29-LASFDE29
	.long L$set$52
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB17-.
	.set L$set$53,LFE17-LFB17
	.quad L$set$53
	.uleb128 0
	.byte	0x4
	.set L$set$54,LCFI23-LFB17
	.long L$set$54
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$55,LCFI24-LCFI23
	.long L$set$55
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$56,LCFI25-LCFI24
	.long L$set$56
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$57,LCFI26-LCFI25
	.long L$set$57
	.byte	0xb
	.byte	0x4
	.set L$set$58,LCFI27-LCFI26
	.long L$set$58
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$59,LCFI28-LCFI27
	.long L$set$59
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$60,LEFDE31-LASFDE31
	.long L$set$60
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB18-.
	.set L$set$61,LFE18-LFB18
	.quad L$set$61
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$62,LEFDE33-LASFDE33
	.long L$set$62
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB19-.
	.set L$set$63,LFE19-LFB19
	.quad L$set$63
	.uleb128 0
	.byte	0x4
	.set L$set$64,LCFI29-LFB19
	.long L$set$64
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$65,LCFI30-LCFI29
	.long L$set$65
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$66,LEFDE35-LASFDE35
	.long L$set$66
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB20-.
	.set L$set$67,LFE20-LFB20
	.quad L$set$67
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$68,LEFDE37-LASFDE37
	.long L$set$68
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB21-.
	.set L$set$69,LFE21-LFB21
	.quad L$set$69
	.uleb128 0
	.align	3
LEFDE37:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
