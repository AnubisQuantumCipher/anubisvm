	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC4:
	.ascii "anubis_vesting.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_vesting__serialize_vesting_state__write_u64.12:
LFB50:
	ldp	x7, x1, [x16]
	mov	w2, 8
	mov	w8, 2147483647
	ldp	x6, x1, [x1]
	ldp	w5, w4, [x1]
L8:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	blt	L15
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	x29, sp
LCFI1:
L9:
	cmp	w5, w1
	bgt	L16
	sxtw	x3, w1
	sub	x3, x3, x7
	strb	w0, [x6, x3]
	lsr	x0, x0, 8
	cmp	w1, w8
	beq	L17
	add	w1, w1, 1
	subs	w2, w2, #1
	str	w1, [x16, 16]
	beq	L18
L5:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	bge	L9
	subs	w2, w2, #1
	bne	L5
L18:
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
	.p2align 2,,3
L15:
	subs	w2, w2, #1
	bne	L8
	ret
L17:
LCFI3:
	adrp	x0, lC4@PAGE
	mov	w1, 574
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L16:
	adrp	x0, lC4@PAGE
	mov	w1, 572
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE50:
	.align	2
	.p2align 5,,15
_anubis_vesting__deserialize_vesting_state__read_u64.13:
LFB52:
	stp	x29, x30, [sp, -64]!
LCFI4:
	mov	x29, sp
LCFI5:
	stp	x19, x20, [sp, 16]
LCFI6:
	mov	w20, -1
	mov	x19, x16
	stp	x21, x22, [sp, 32]
LCFI7:
	mov	w21, 2147483647
	str	x23, [sp, 48]
LCFI8:
	mov	x23, 0
	.p2align 5,,15
L23:
	ldr	x3, [x19, 8]
	add	w20, w20, 1
	ldr	w0, [x19, 16]
	ldr	x1, [x3, 8]
	ldr	w2, [x1, 4]
	cmp	w2, w0
	blt	L20
	ldr	w1, [x1]
	cmp	w1, w0
	bgt	L26
	ldr	x4, [x19]
	sxtw	x2, w0
	mov	w1, w20
	mov	x0, 256
	ldr	x3, [x3]
	sub	x2, x2, x4
	ldrb	w22, [x3, x2]
	bl	_system__exp_llu__exp_long_long_unsigned
	ldr	w1, [x19, 16]
	madd	x23, x22, x0, x23
	cmp	w1, w21
	beq	L27
	add	w1, w1, 1
	str	w1, [x19, 16]
L20:
	cmp	w20, 7
	bne	L23
	mov	x0, x23
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI9:
	ret
L26:
LCFI10:
	adrp	x0, lC4@PAGE
	mov	w1, 628
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L27:
	adrp	x0, lC4@PAGE
	mov	w1, 629
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE52:
	.const
	.align	3
lC5:
	.ascii "failed postcondition from anubis_vesting.ads:241"
	.text
	.align	2
	.p2align 5,,15
_anubis_vesting__total_vested_amount___wrapped_statements.4:
LFB25:
	stp	x29, x30, [sp, -16]!
LCFI11:
	mov	x8, 1463
	mov	x29, sp
LCFI12:
	movk	x8, 0x3d16, lsl 16
	mov	x7, 6655
	movk	x8, 0x24d0, lsl 32
	mov	x0, 0
	ldr	x6, [x16]
	mov	x3, 1
	movk	x7, 0x4f, lsl 16
	movk	x8, 0x6790, lsl 48
	.p2align 5,,15
L35:
	lsl	x1, x3, 3
	ldr	x2, [x6, 2144]
	sub	x1, x1, x3
	add	x1, x3, x1, lsl 2
	add	x1, x6, x1, lsl 3
	ldrb	w5, [x1, -199]
	cmp	w5, 5
	bhi	L40
	sub	w4, w5, #3
	and	w4, w4, 255
	cmp	w4, 1
	bhi	L41
	ldr	x4, [x1, -184]
	cmp	x2, x4
	bls	L31
	sub	x2, x2, x4
	cmp	x2, x7
	bhi	L39
	ldr	x4, [x1, -192]
	mul	x1, x2, x4
	umulh	x1, x1, x8
	lsr	x1, x1, 21
	cmp	x1, x4
	csel	x1, x1, x4, ls
	cmp	x4, x1
	bcc	L34
L36:
	add	x0, x0, x1
L31:
	add	x3, x3, 1
	cmp	x3, 10
	bne	L35
	mov	x1, 41728
	movk	x1, 0x11e1, lsl 16
	cmp	x0, x1
	csel	x0, x0, x1, ls
	ldp	x29, x30, [sp], 16
LCFI13:
	ret
	.p2align 2,,3
L41:
LCFI14:
	cmp	w5, 5
	bne	L31
L39:
	ldr	x1, [x1, -192]
	b	L36
L34:
	adrp	x0, lC5@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L40:
	adrp	x0, lC4@PAGE
	mov	w1, 165
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.const
	.align	2
lC1:
	.word	1
	.word	48
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__verification_typeH
_anubis_vesting__verification_typeH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L45
	sub	w1, w1, w2
	cmp	w1, 10
	bgt	L46
L45:
	mov	x3, 0
	mov	x0, 0
L43:
	adrp	x2, _verification_typeG.34@PAGE
	mov	w1, 43691
	add	x2, x2, _verification_typeG.34@PAGEOFF;
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
L46:
	ldrb	w2, [x0, 11]
	mov	w1, 34953
	movk	w1, 0x8888, lsl 16
	add	w3, w2, w2, lsl 1
	lsl	w0, w2, 3
	sub	w0, w0, w2
	umull	x2, w3, w1
	umull	x1, w0, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	lsl	w5, w2, 4
	lsl	w4, w1, 4
	sub	w2, w5, w2
	sub	w1, w4, w1
	sub	w3, w3, w2
	sub	w0, w0, w1
	sxtw	x3, w3
	sxtw	x0, w0
	b	L43
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__milestone_statusH
_anubis_vesting__milestone_statusH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L50
	sub	w1, w1, w2
	cmp	w1, 2
	bgt	L51
L50:
	mov	x3, 0
	mov	x0, 0
L48:
	adrp	x2, _milestone_statusG.30@PAGE
	mov	w1, 43691
	add	x2, x2, _milestone_statusG.30@PAGEOFF;
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
L51:
	ldrb	w3, [x0, 3]
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
	b	L48
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__milestone_recordIP
_anubis_vesting__milestone_recordIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__Tmilestone_arrayBIP
_anubis_vesting__Tmilestone_arrayBIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__vesting_stateIP
_anubis_vesting__vesting_stateIP:
LFB129:
	ret
LFE129:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__milestone_submissionIP
_anubis_vesting__milestone_submissionIP:
LFB131:
	ret
LFE131:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__oracle_attestationIP
_anubis_vesting__oracle_attestationIP:
LFB133:
	ret
LFE133:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__submit_resultH
_anubis_vesting__submit_resultH:
LFB9:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _submit_resultP.29@PAGE
	add	w5, w10, 1
	add	x9, x9, _submit_resultP.29@PAGEOFF;
	adrp	x12, _submit_resultT1.28@PAGE
	adrp	x11, _submit_resultT2.27@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _submit_resultT1.28@PAGEOFF;
	add	x11, x11, _submit_resultT2.27@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 13
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L59
L63:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	add	w6, w2, w2, lsl 1
	add	w6, w2, w6, lsl 2
	add	w2, w5, w5, lsl 1
	sub	w6, w3, w6
	add	w2, w5, w2, lsl 2
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L59
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L63
L59:
	adrp	x3, _submit_resultG.26@PAGE
	mov	w1, 43691
	add	x3, x3, _submit_resultG.26@PAGEOFF;
	movk	w1, 0xaaaa, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 1
	sub	w0, w0, w1, lsl 1
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__verify_resultH
_anubis_vesting__verify_resultH:
LFB10:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L67
	sub	w1, w1, w2
	cmp	w1, 5
	bgt	L68
L67:
	mov	x3, 0
	mov	x0, 0
L65:
	adrp	x2, _verify_resultG.22@PAGE
	mov	w1, 43691
	add	x2, x2, _verify_resultG.22@PAGEOFF;
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
L68:
	ldrb	w1, [x0, 6]
	mov	w0, 36409
	movk	w0, 0x38e3, lsl 16
	add	w2, w1, w1, lsl 1
	lsl	w4, w1, 4
	sub	w4, w4, w1
	add	w1, w1, w2, lsl 2
	umull	x3, w4, w0
	umull	x0, w1, w0
	lsr	x3, x3, 34
	add	w3, w3, w3, lsl 3
	lsr	x0, x0, 34
	add	w0, w0, w0, lsl 3
	sub	w3, w4, w3, lsl 1
	sub	w0, w1, w0, lsl 1
	sxtw	x3, w3
	sxtw	x0, w0
	b	L65
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__release_resultH
_anubis_vesting__release_resultH:
LFB11:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L72
	sub	w1, w1, w2
	cmp	w1, 5
	bgt	L73
L72:
	mov	x3, 0
	mov	x0, 0
L70:
	adrp	x2, _release_resultG.18@PAGE
	mov	w1, 52429
	add	x2, x2, _release_resultG.18@PAGEOFF;
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
L73:
	ldrb	w0, [x0, 6]
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
	b	L70
LFE11:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__weekly_updateIP
_anubis_vesting__weekly_updateIP:
LFB135:
	ret
LFE135:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__update_statusH
_anubis_vesting__update_statusH:
LFB13:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L77
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
L76:
	adrp	x3, _update_statusG.14@PAGE
	mov	w1, 52429
	add	x3, x3, _update_statusG.14@PAGEOFF;
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
L77:
	mov	x2, 0
	mov	x0, 0
	b	L76
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__proof_of_buildIP
_anubis_vesting__proof_of_buildIP:
LFB137:
	ret
LFE137:
	.const
	.align	3
lC6:
	.ascii "failed precondition from anubis_vesting.ads:191"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__init_vesting
_anubis_vesting__init_vesting:
LFB15:
	stp	x29, x30, [sp, -16]!
LCFI15:
	mov	x29, sp
LCFI16:
	ldp	w2, w3, [x2]
	sxtw	x5, w2
	sxtw	x4, w3
	add	x6, x5, 31
	cmp	x6, x4
	bne	L94
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w2, w3
	bgt	L89
	sub	x6, x1, x5
	sub	x1, x5, #1
	sub	x5, x0, x5
	.p2align 5,,15
L88:
	add	x1, x1, 1
	subs	w3, w1, w2
	bvs	L84
	cmp	w3, 31
	bgt	L86
	bhi	L95
	ldrb	w3, [x6, x1]
	strb	w3, [x5, x1]
L86:
	cmp	x1, x4
	bne	L88
L89:
	movi	v31.4s, 0
	adrp	x5, _anubis_vesting__milestone_amounts@PAGE
	mov	x3, 1
	add	x5, x5, _anubis_vesting__milestone_amounts@PAGEOFF;
	.p2align 5,,15
L82:
	lsl	x1, x3, 3
	add	x2, x5, x3, lsl 3
	sub	x1, x1, x3
	add	x1, x3, x1, lsl 2
	ldr	x4, [x2, -8]
	add	x1, x0, x1, lsl 3
	sub	x2, x1, #200
	str	q31, [x1, -200]
	stp	q31, q31, [x2, 16]
	stp	q31, q31, [x2, 48]
	stp	q31, q31, [x2, 80]
	stp	q31, q31, [x2, 112]
	stp	q31, q31, [x2, 144]
	stp	q31, q31, [x2, 176]
	strb	w3, [x1, -200]
	add	x3, x3, 1
	str	x4, [x1, -192]
	str	q31, [x1, 8]
	str	xzr, [x1, 24]
	cmp	x3, 10
	bne	L82
	movi	v31.4s, 0
	add	x1, x0, 2048
	mov	w2, 1
	str	q31, [x1, 72]
	str	q31, [x1, 88]
	strb	w2, [x0, 2152]
	ldp	x29, x30, [sp], 16
LCFI17:
	ret
L84:
LCFI18:
	adrp	x0, lC4@PAGE
	mov	w1, 29
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L95:
	adrp	x0, lC4@PAGE
	mov	w1, 30
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L94:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE15:
	.const
	.align	2
lC0:
	.word	1
	.word	47
	.text
	.const
	.align	3
lC7:
	.ascii "failed precondition from anubis_vesting.ads:220"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__verify_milestone
_anubis_vesting__verify_milestone:
LFB19:
	stp	x29, x30, [sp, -16]!
LCFI19:
	mov	x7, x0
	mov	x29, sp
LCFI20:
	ldpsw	x2, x0, [x3]
	add	x2, x2, 62
	cmp	x2, x0
	bge	L110
	ldrsb	w3, [x1]
	sub	w0, w3, #1
	and	w0, w0, 255
	cmp	w0, 8
	bhi	L99
	sxtw	x8, w3
	sbfiz	x6, x3, 3, 32
	sub	x2, x6, x8
	add	x2, x8, x2, lsl 2
	add	x2, x7, x2, lsl 3
	ldrb	w2, [x2, -199]
	cmp	w2, 5
	bhi	L99
	sub	w2, w2, #1
	and	w2, w2, 255
	cmp	w2, 1
	bhi	L106
	ldrb	w2, [x1, 24]
	cmp	w2, 1
	bhi	L111
	mov	w0, 4
	cbz	w2, L100
	mov	w0, 232
	mov	x2, 0
	add	x5, x1, 57
	umaddl	x3, w3, w0, x7
	sub	x4, x3, #64
	b	L105
	.p2align 2,,3
L112:
	ldrb	w3, [x5, x2]
	strb	w3, [x4, x2]
	add	x2, x2, 1
L105:
	cmp	x2, 63
	bls	L112
	add	x2, x2, 1
	cmp	x2, 128
	bne	L105
	sub	x2, x6, x8
	ldr	x4, [x1, 16]
	mov	w5, 3
	add	x2, x8, x2, lsl 2
	mov	w0, 0
	ldr	x3, [x7, 2120]
	add	x1, x7, x2, lsl 3
	ldr	x2, [x1, -192]
	strb	w5, [x1, -199]
	str	x4, [x1, -184]
	add	x1, x3, x2
	str	x1, [x7, 2120]
L100:
	ldp	x29, x30, [sp], 16
LCFI21:
	ret
L106:
LCFI22:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI23:
	ret
L111:
LCFI24:
	adrp	x0, lC4@PAGE
	mov	w1, 107
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L99:
	adrp	x0, lC4@PAGE
	mov	w1, 99
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L110:
	adrp	x0, lC7@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__previous_milestones_complete
_anubis_vesting__previous_milestones_complete:
LFB21:
	and	x3, x1, 255
	stp	x29, x30, [sp, -16]!
LCFI25:
	mov	x29, sp
LCFI26:
	sub	w2, w3, #1
	and	w2, w2, 255
	cmp	w2, 8
	bhi	L123
	cmp	w1, 1
	beq	L122
	mov	x2, 1
	.p2align 5,,15
L117:
	lsl	x1, x2, 3
	sub	x1, x1, x2
	add	x1, x2, x1, lsl 2
	add	x1, x0, x1, lsl 3
	ldrb	w1, [x1, -199]
	cmp	w1, 5
	bhi	L124
	sub	w1, w1, #3
	and	w1, w1, 255
	cmp	w1, 2
	bhi	L119
	add	x2, x2, 1
	cmp	x2, x3
	bne	L117
L122:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI27:
	ret
	.p2align 2,,3
L119:
LCFI28:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI29:
	ret
L124:
LCFI30:
	adrp	x0, lC4@PAGE
	mov	w1, 142
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L123:
	adrp	x0, lC4@PAGE
	mov	w1, 137
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE21:
	.const
	.align	3
lC8:
	.ascii "anubis_vesting.ads"
	.space 1
	.align	3
lC9:
	.ascii "failed precondition from anubis_vesting.ads:208"
	.align	3
lC10:
	.ascii "failed postcondition from anubis_vesting.ads:209"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__submit_milestone
_anubis_vesting__submit_milestone:
LFB17:
	stp	x29, x30, [sp, -48]!
LCFI31:
	mov	x29, sp
LCFI32:
	stp	x19, x20, [sp, 16]
LCFI33:
	mov	x19, x1
	ldrb	w1, [x0, 2152]
	str	x21, [sp, 32]
LCFI34:
	cmp	w1, 1
	bhi	L142
	cbz	w1, L143
	ldrsb	w1, [x19]
	sub	w2, w1, #1
	and	w2, w2, 255
	cmp	w2, 8
	bhi	L129
	sxtw	x3, w1
	sbfiz	x2, x1, 3, 32
	sub	x2, x2, x3
	mov	x21, x0
	add	x2, x3, x2, lsl 2
	add	x2, x0, x2, lsl 3
	ldrb	w20, [x2, -199]
	cmp	w20, 5
	bhi	L129
	cbnz	w20, L134
	mov	x4, 1
	cmp	w1, 1
	bne	L144
L131:
	lsl	x1, x4, 3
	lsl	x2, x3, 3
	ldr	q29, [x19, 1]
	sub	x1, x1, x4
	sub	x2, x2, x3
	add	x1, x4, x1, lsl 2
	ldr	q28, [x19, 17]
	mov	w4, 1
	add	x2, x3, x2, lsl 2
	add	x1, x21, x1, lsl 3
	ldr	q31, [x19, 33]
	add	x0, x21, x2, lsl 3
	ldr	q30, [x19, 49]
	strb	w4, [x1, -199]
	ldrb	w0, [x0, -199]
	stp	q29, q28, [x1, -160]
	stp	q31, q30, [x1, -128]
	cmp	w0, 5
	bhi	L145
	cmp	w0, 1
	bne	L146
	mov	w0, w20
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI35:
	ret
	.p2align 2,,3
L144:
LCFI36:
	bl	_anubis_vesting__previous_milestones_complete
	tbz	x0, 0, L136
	ldrsb	w1, [x19]
	sub	w0, w1, #1
	and	w0, w0, 255
	cmp	w0, 8
	bhi	L147
	sxtw	x3, w1
	mov	x4, x3
	b	L131
	.p2align 2,,3
L134:
	mov	w20, 2
	mov	w0, w20
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI37:
	ret
	.p2align 2,,3
L136:
LCFI38:
	mov	w20, 4
	mov	w0, w20
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI39:
	ret
L146:
LCFI40:
	adrp	x0, lC10@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L129:
	adrp	x0, lC4@PAGE
	mov	w1, 70
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L145:
	adrp	x0, lC8@PAGE
	mov	w1, 210
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L143:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L142:
	adrp	x0, lC8@PAGE
	mov	w1, 208
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L147:
	adrp	x0, lC4@PAGE
	mov	w1, 82
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE17:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__calculate_vested
_anubis_vesting__calculate_vested:
LFB22:
	ldrb	w3, [x0, 1]
	cmp	w3, 5
	bhi	L158
	mov	x2, x0
	sub	w0, w3, #3
	and	w0, w0, 255
	cmp	w0, 1
	bhi	L159
	ldr	x3, [x2, 16]
	mov	x0, 0
	cmp	x1, x3
	bls	L148
	sub	x1, x1, x3
	mov	x3, 6655
	ldr	x0, [x2, 8]
	movk	x3, 0x4f, lsl 16
	cmp	x1, x3
	bhi	L148
	mov	x2, 1463
	mul	x1, x0, x1
	movk	x2, 0x3d16, lsl 16
	movk	x2, 0x24d0, lsl 32
	movk	x2, 0x6790, lsl 48
	umulh	x1, x1, x2
	lsr	x1, x1, 21
	cmp	x0, x1
	csel	x0, x0, x1, ls
L148:
	ret
	.p2align 2,,3
L159:
	mov	x0, 0
	cmp	w3, 5
	bne	L148
	ldr	x0, [x2, 8]
	ret
L158:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI41:
	mov	w1, 165
	mov	x29, sp
LCFI42:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__total_vested_amount
_anubis_vesting__total_vested_amount:
LFB24:
	stp	x29, x30, [sp, -32]!
LCFI43:
	mov	x29, sp
LCFI44:
	add	x1, x29, 32
	add	x16, x29, 16
	stp	x0, x1, [x29, 16]
	bl	_anubis_vesting__total_vested_amount___wrapped_statements.4
	ldp	x29, x30, [sp], 32
LCFI45:
	ret
LFE24:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__claimable_amount
_anubis_vesting__claimable_amount:
LFB26:
	stp	x29, x30, [sp, -48]!
LCFI46:
	mov	x29, sp
LCFI47:
	add	x1, x29, 48
	add	x16, x29, 32
	str	x19, [sp, 16]
LCFI48:
	mov	x19, x0
	stp	x0, x1, [x29, 32]
	bl	_anubis_vesting__total_vested_amount___wrapped_statements.4
	ldr	x1, [x19, 2136]
	cmp	x1, x0
	sub	x0, x0, x1
	csel	x0, x0, xzr, cc
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI49:
	ret
LFE26:
	.const
	.align	3
lC11:
	.ascii "failed precondition from anubis_vesting.ads:262"
	.align	3
lC12:
	.ascii "failed postcondition from anubis_vesting.ads:263"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__update_vesting
_anubis_vesting__update_vesting:
LFB27:
	stp	x29, x30, [sp, -96]!
LCFI50:
	mov	x29, sp
LCFI51:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI52:
	mov	x22, x0
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI53:
	ldrb	w0, [x0, 2152]
	cmp	w0, 1
	bhi	L177
	cbz	w0, L178
	mov	x24, 6656
	mov	x23, x1
	str	x1, [x22, 2144]
	add	x21, x22, 32
	mov	x20, 1
	movk	x24, 0x4f, lsl 16
	mov	w26, 5
	mov	w25, 4
	b	L173
	.p2align 2,,3
L170:
	cmp	w2, 4
	beq	L179
L171:
	add	x20, x20, 1
	add	x21, x21, 232
	cmp	x20, 10
	beq	L180
L173:
	lsl	x19, x20, 3
	sub	x19, x19, x20
	add	x19, x20, x19, lsl 2
	add	x19, x22, x19, lsl 3
	ldrb	w2, [x19, -199]
	cmp	w2, 5
	bhi	L181
	cmp	w2, 3
	bne	L170
	ldr	x0, [x19, -184]
	cmp	x23, x0
	bls	L171
	add	x20, x20, 1
	strb	w25, [x19, -199]
	add	x21, x21, 232
	cmp	x20, 10
	bne	L173
	.p2align 5,,15
L180:
	add	x0, x29, 96
	add	x16, x29, 80
	stp	x22, x0, [x29, 80]
	bl	_anubis_vesting__total_vested_amount___wrapped_statements.4
	ldr	x1, [x22, 2144]
	str	x0, [x22, 2128]
	cmp	x1, x23
	bne	L182
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 96
LCFI54:
	ret
	.p2align 2,,3
L179:
LCFI55:
	ldr	x0, [x19, -184]
	add	x0, x0, x24
	cmp	x23, x0
	bcc	L172
	ldr	x0, [x19, -192]
	strb	w26, [x19, -199]
	str	x0, [x19, -176]
	b	L171
	.p2align 2,,3
L172:
	mov	x1, x23
	mov	x0, x21
	bl	_anubis_vesting__calculate_vested
	str	x0, [x19, -176]
	b	L171
L181:
	adrp	x0, lC4@PAGE
	mov	w1, 240
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L178:
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L177:
	adrp	x0, lC8@PAGE
	mov	w1, 262
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L182:
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE27:
	.const
	.align	3
lC13:
	.ascii "failed precondition from anubis_vesting.ads:277"
	.align	3
lC14:
	.ascii "failed postcondition from anubis_vesting.ads:279"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__release_vested
_anubis_vesting__release_vested:
LFB29:
	stp	x29, x30, [sp, -96]!
LCFI56:
	mov	x29, sp
LCFI57:
	cmp	x1, 0
	stp	x19, x20, [sp, 16]
LCFI58:
	mov	x19, x0
	stp	x21, x22, [sp, 32]
LCFI59:
	mov	x21, x1
	stp	x23, x24, [sp, 48]
	ldpsw	x1, x0, [x3]
	str	x25, [sp, 64]
LCFI60:
	add	x1, x1, 31
	ccmp	x1, x0, 0, ne
	bne	L199
	add	x0, x29, 96
	add	x16, x29, 80
	ldr	x22, [x19, 2136]
	stp	x19, x0, [x29, 80]
	bl	_anubis_vesting__total_vested_amount___wrapped_statements.4
	ldr	x1, [x19, 2136]
	cmp	x0, x1
	bls	L195
	sub	x0, x0, x1
	cmp	x21, x0
	bhi	L195
	add	x1, x21, x1
	add	x25, x19, 32
	add	x23, x19, 1888
	mov	x20, x21
	mov	x24, 1
	str	x1, [x19, 2136]
L186:
	ldr	x1, [x19, 2144]
	mov	x0, x25
	bl	_anubis_vesting__calculate_vested
	lsl	x2, x24, 3
	sub	x2, x2, x24
	add	x2, x24, x2, lsl 2
	add	x2, x19, x2, lsl 3
	ldr	x1, [x2, -168]
	cmp	x0, x1
	bls	L189
	sub	x3, x0, x1
	cmp	x20, x3
	bhi	L190
	add	x20, x20, x1
	str	x20, [x2, -168]
L188:
	ldr	x1, [x19, 2136]
	add	x22, x21, x22
	mov	w0, 0
	cmp	x1, x22
	beq	L197
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L195:
	mov	w0, 1
L197:
	ldr	x25, [sp, 64]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 96
LCFI61:
	ret
	.p2align 2,,3
L189:
LCFI62:
	cmp	x25, x23
	beq	L188
	add	x24, x24, 1
	add	x25, x25, 232
	b	L186
	.p2align 2,,3
L190:
	sub	x20, x20, x0
	str	x0, [x2, -168]
	add	x20, x1, x20
	cmp	x25, x23
	beq	L188
	add	x24, x24, 1
	add	x25, x25, 232
	cbnz	x20, L186
	b	L188
L199:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE29:
	.const
	.align	3
lC15:
	.ascii "failed precondition from anubis_vesting.ads:290"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__release_all_vested
_anubis_vesting__release_all_vested:
LFB31:
	stp	x29, x30, [sp, -64]!
LCFI63:
	mov	x29, sp
LCFI64:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI65:
	mov	x21, x0
	ldpsw	x3, x0, [x2]
	add	x3, x3, 31
	cmp	x3, x0
	bne	L206
	add	x0, x29, 64
	add	x16, x29, 48
	mov	x22, x1
	mov	x20, x2
	stp	x21, x0, [x29, 48]
	bl	_anubis_vesting__total_vested_amount___wrapped_statements.4
	ldr	x1, [x21, 2136]
	cmp	x0, x1
	bls	L204
	sub	x19, x0, x1
	mov	x2, x22
	mov	x1, x19
	mov	x3, x20
	mov	x0, x21
	bl	_anubis_vesting__release_vested
	mov	w1, w0
	tbnz	x1, 0, L204
	mov	x0, x19
	and	x1, x1, 255
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI66:
	ret
	.p2align 2,,3
L204:
LCFI67:
	mov	x19, 0
	mov	w1, 1
	mov	x0, x19
	and	x1, x1, 255
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI68:
	ret
L206:
LCFI69:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE31:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__get_milestone_status
_anubis_vesting__get_milestone_status:
LFB32:
	sub	w2, w1, #1
	and	w2, w2, 255
	cmp	w2, 8
	bhi	L209
	sxtw	x2, w1
	sbfiz	x1, x1, 3, 32
	sub	x1, x1, x2
	add	x1, x2, x1, lsl 2
	add	x0, x0, x1, lsl 3
	ldrb	w0, [x0, -199]
	cmp	w0, 5
	bhi	L209
	ret
L209:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI70:
	mov	w1, 357
	mov	x29, sp
LCFI71:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE32:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__get_milestone_allocation
_anubis_vesting__get_milestone_allocation:
LFB33:
	sub	w1, w0, #1
	and	w1, w1, 255
	cmp	w1, 8
	bhi	L217
	adrp	x1, _anubis_vesting__milestone_amounts@PAGE
	add	x1, x1, _anubis_vesting__milestone_amounts@PAGEOFF;
	add	x0, x1, w0, sxtw 3
	ldr	x0, [x0, -8]
	ret
L217:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI72:
	mov	w1, 364
	mov	x29, sp
LCFI73:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE33:
	.const
	.align	3
lC16:
	.ascii "failed postcondition from anubis_vesting.ads:316"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__get_vesting_progress
_anubis_vesting__get_vesting_progress:
LFB35:
	stp	x29, x30, [sp, -32]!
LCFI74:
	mov	x29, sp
LCFI75:
	stp	x19, x20, [sp, 16]
LCFI76:
	mov	x19, x0
	sub	w0, w1, #1
	and	w0, w0, 255
	cmp	w0, 8
	bhi	L224
	mov	w0, 232
	sxtw	x20, w1
	umaddl	x0, w1, w0, x19
	ldr	x1, [x19, 2144]
	sub	x0, x0, #200
	bl	_anubis_vesting__calculate_vested
	lsl	x1, x20, 3
	sub	x1, x1, x20
	add	x1, x20, x1, lsl 2
	add	x1, x19, x1, lsl 3
	ldr	x2, [x1, -192]
	cbz	x2, L222
	add	x0, x0, x0, lsl 2
	mov	x3, 2147483647
	add	x0, x0, x0, lsl 2
	lsl	x1, x0, 2
	udiv	x1, x1, x2
	cmp	x1, x3
	bhi	L225
	mov	w0, w1
	cmp	w1, 100
	bgt	L226
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI77:
	ret
	.p2align 2,,3
L222:
LCFI78:
	mov	w0, 0
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI79:
	ret
L224:
LCFI80:
	adrp	x0, lC4@PAGE
	mov	w1, 372
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L226:
	adrp	x0, lC16@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L225:
	adrp	x0, lC4@PAGE
	mov	w1, 379
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE35:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__get_overall_progress
_anubis_vesting__get_overall_progress:
LFB37:
	stp	x29, x30, [sp, -32]!
LCFI81:
	mov	x29, sp
LCFI82:
	add	x1, x29, 32
	add	x16, x29, 16
	stp	x0, x1, [x29, 16]
	bl	_anubis_vesting__total_vested_amount___wrapped_statements.4
	mov	w2, 100
	mov	x1, 48807
	movk	x1, 0x5d7, lsl 16
	umull	x0, w0, w2
	movk	x1, 0x9ec2, lsl 32
	movk	x1, 0xe510, lsl 48
	umulh	x0, x0, x1
	ubfx	x0, x0, 28, 32
	ldp	x29, x30, [sp], 32
LCFI83:
	ret
LFE37:
	.const
	.align	3
lC17:
	.ascii "failed postcondition from anubis_vesting.ads:330"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__completed_milestones
_anubis_vesting__completed_milestones:
LFB39:
	stp	x29, x30, [sp, -16]!
LCFI84:
	mov	w3, 0
	mov	x29, sp
LCFI85:
	mov	x2, 1
	mov	w4, 2147483647
	b	L233
	.p2align 2,,3
L231:
	add	x2, x2, 1
	cmp	x2, 10
	beq	L237
L233:
	lsl	x1, x2, 3
	sub	x1, x1, x2
	add	x1, x2, x1, lsl 2
	add	x1, x0, x1, lsl 3
	ldrb	w1, [x1, -199]
	cmp	w1, 5
	bhi	L238
	bne	L231
	cmp	w3, w4
	beq	L239
	add	x2, x2, 1
	add	w3, w3, 1
	cmp	x2, 10
	bne	L233
L237:
	cmp	w3, 9
	bgt	L240
	mov	w0, w3
	ldp	x29, x30, [sp], 16
LCFI86:
	ret
L238:
LCFI87:
	adrp	x0, lC4@PAGE
	mov	w1, 396
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L239:
	adrp	x0, lC4@PAGE
	mov	w1, 397
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L240:
	adrp	x0, lC17@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE39:
	.const
	.align	3
lC18:
	.ascii "failed precondition from anubis_vesting.ads:342"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__verify_gnatprove_hash
_anubis_vesting__verify_gnatprove_hash:
LFB41:
	ldp	w5, w4, [x1]
	mov	x2, x0
	sxtw	x1, w5
	sxtw	x3, w4
	add	x0, x1, 31
	cmp	x0, x3
	bne	L251
	mov	w0, 0
	cmp	w5, w4
	bgt	L243
	sub	x2, x2, x1
	sub	x0, x1, #1
	add	x1, x2, 1
	b	L244
	.p2align 2,,3
L253:
	add	x0, x0, 1
	cmp	x3, x0
	beq	L252
L244:
	ldrb	w2, [x1, x0]
	cbz	w2, L253
	mov	w0, 1
L243:
	ret
	.p2align 2,,3
L252:
	mov	w0, 0
	ret
L251:
	adrp	x0, lC18@PAGE
	adrp	x1, lC0@PAGE
	stp	x29, x30, [sp, -16]!
LCFI88:
	add	x0, x0, lC18@PAGEOFF;
	mov	x29, sp
LCFI89:
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE41:
	.const
	.align	3
lC19:
	.ascii "failed precondition from anubis_vesting.ads:352"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__verify_audit_signature
_anubis_vesting__verify_audit_signature:
LFB42:
	ldpsw	x1, x3, [x4]
	ldpsw	x0, x2, [x6]
	add	x1, x1, 2591
	cmp	x1, x3
	add	x0, x0, 62
	ccmp	x0, x2, 0, eq
	bge	L259
	mov	w0, 1
	ret
L259:
	adrp	x0, lC19@PAGE
	adrp	x1, lC0@PAGE
	stp	x29, x30, [sp, -16]!
LCFI90:
	add	x0, x0, lC19@PAGEOFF;
	mov	x29, sp
LCFI91:
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE42:
	.const
	.align	3
lC20:
	.ascii "failed precondition from anubis_vesting.ads:361"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__verify_oracle_attestation
_anubis_vesting__verify_oracle_attestation:
LFB43:
	stp	x29, x30, [sp, -16]!
LCFI92:
	mov	x29, sp
LCFI93:
	ldpsw	x1, x2, [x2]
	add	x1, x1, 2591
	cmp	x1, x2
	bne	L264
	ldrb	w0, [x0, 24]
	cmp	w0, 1
	bhi	L265
	ldp	x29, x30, [sp], 16
LCFI94:
	ret
L264:
LCFI95:
	adrp	x0, lC20@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L265:
	adrp	x0, lC4@PAGE
	mov	w1, 446
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE43:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__record_weekly_update
_anubis_vesting__record_weekly_update:
LFB44:
	ldr	x1, [x1, 320]
	adrp	x2, _anubis_vesting__last_update_block@PAGE
	mov	w0, 1
	str	x1, [x2, #_anubis_vesting__last_update_block@PAGEOFF]
	ret
LFE44:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__get_update_status
_anubis_vesting__get_update_status:
LFB45:
	adrp	x2, _anubis_vesting__last_update_block@PAGE
	mov	w0, 0
	ldr	x2, [x2, #_anubis_vesting__last_update_block@PAGEOFF]
	cmp	x2, 0
	ccmp	x2, x1, 2, ne
	bcc	L274
L268:
	ret
	.p2align 2,,3
L274:
	sub	x1, x1, x2
	mov	x2, 35263
	movk	x2, 0x1, lsl 16
	cmp	x1, x2
	bls	L268
	sub	x3, x1, #98304
	mov	w0, 1
	sub	x3, x3, #2496
	cmp	x3, x2
	bls	L268
	mov	x2, 9983
	mov	w0, 2
	movk	x2, 0x6, lsl 16
	cmp	x1, x2
	bls	L268
	mov	x0, 19967
	movk	x0, 0xc, lsl 16
	cmp	x1, x0
	cset	w0, hi
	add	w0, w0, 3
	ret
LFE45:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__get_vesting_rate
_anubis_vesting__get_vesting_rate:
LFB46:
	adrp	x0, _anubis_vesting__last_update_block@PAGE
	ldr	x2, [x0, #_anubis_vesting__last_update_block@PAGEOFF]
	mov	w0, 100
	cmp	x2, 0
	ccmp	x1, x2, 0, ne
	bhi	L280
L275:
	ret
	.p2align 2,,3
L280:
	sub	x1, x1, x2
	mov	x2, 4991
	movk	x2, 0x3, lsl 16
	cmp	x1, x2
	bls	L275
	mov	x2, 9983
	mov	w0, 90
	movk	x2, 0x6, lsl 16
	cmp	x1, x2
	csel	w0, wzr, w0, hi
	ret
LFE46:
	.const
	.align	3
lC21:
	.ascii "failed precondition from anubis_vesting.ads:436"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__mint_proof_of_build
_anubis_vesting__mint_proof_of_build:
LFB48:
	stp	x29, x30, [sp, -16]!
LCFI96:
	mov	x29, sp
LCFI97:
	mov	w12, w0
	ldp	w7, w13, [x2]
	ldp	w8, w15, [x4]
	sxtw	x2, w7
	sxtw	x9, w13
	add	x10, x2, 31
	sxtw	x4, w8
	cmp	x10, x9
	add	x0, x4, 31
	sxtw	x10, w15
	ccmp	x0, x10, 0, eq
	bne	L303
	adrp	x14, _anubis_vesting__nft_counter@PAGE
	sub	w11, w12, #1
	ldr	x0, [x14, #_anubis_vesting__nft_counter@PAGEOFF]
	and	w11, w11, 255
	add	x0, x0, 1
	str	x0, [x14, #_anubis_vesting__nft_counter@PAGEOFF]
	str	x0, [x6]
	cmp	w11, 8
	bhi	L304
	add	x11, x6, 9
	strb	w12, [x6, 8]
	stp	xzr, xzr, [x11]
	stp	xzr, xzr, [x11, 16]
	cmp	w7, w13
	bgt	L284
	sub	x12, x6, x2
	sub	x11, x1, x2
	add	x12, x12, 9
	sub	x1, x2, #1
	.p2align 5,,15
L290:
	add	x1, x1, 1
	subs	w2, w1, w7
	bvs	L286
	cmp	w2, 31
	bgt	L288
	bhi	L305
	ldrb	w2, [x11, x1]
	strb	w2, [x12, x1]
L288:
	cmp	x9, x1
	bne	L290
L284:
	add	x1, x6, 41
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	cmp	w8, w15
	bgt	L291
	sub	x7, x6, x4
	sub	x3, x3, x4
	add	x7, x7, 41
	sub	x1, x4, #1
	.p2align 5,,15
L297:
	add	x1, x1, 1
	subs	w2, w1, w8
	bvs	L293
	cmp	w2, 31
	bgt	L295
	bhi	L306
	ldrb	w2, [x3, x1]
	strb	w2, [x7, x1]
L295:
	cmp	x10, x1
	bne	L297
L291:
	movi	v31.4s, 0
	str	w5, [x6, 76]
	str	xzr, [x6, 80]
	str	q31, [x6, 104]
	str	q31, [x6, 88]
	str	q31, [x6, 118]
	ldp	x29, x30, [sp], 16
LCFI98:
	ret
L286:
LCFI99:
	adrp	x0, lC4@PAGE
	mov	w1, 538
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L293:
	adrp	x0, lC4@PAGE
	mov	w1, 546
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L305:
	adrp	x0, lC4@PAGE
	mov	w1, 539
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L306:
	adrp	x0, lC4@PAGE
	mov	w1, 547
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L304:
	adrp	x0, lC4@PAGE
	mov	w1, 533
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L303:
	adrp	x0, lC21@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC21@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE48:
	.const
	.align	3
lC22:
	.ascii "failed precondition from anubis_vesting.ads:450"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__serialize_vesting_state
_anubis_vesting__serialize_vesting_state:
LFB49:
	stp	x29, x30, [sp, -112]!
LCFI100:
	mov	x29, sp
LCFI101:
	add	x3, x29, 64
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
LCFI102:
	mov	x24, x0
	add	x0, x29, 112
	stp	x1, x2, [x29, 64]
	str	x3, [x29, 88]
	ldp	w19, w3, [x2]
	str	x0, [x29, 104]
	sxtw	x23, w19
	sxtw	x2, w3
	add	x0, x23, 2046
	str	x23, [x29, 80]
	cmp	x0, x2
	bge	L339
	tbnz	w19, #31, L340
	ldr	x0, [x29, 64]
	cmp	w19, w3
	mov	w1, 0
	sub	x2, x2, x23
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldp	x6, x0, [x29, 64]
	mov	x1, x24
	add	x4, x24, 32
	mov	w7, 2147483647
	ldp	w0, w3, [x0]
	.p2align 5,,15
L314:
	cmp	w3, w19
	blt	L311
	cmp	w0, w19
	bgt	L341
	ldrb	w5, [x1]
	sxtw	x2, w19
	sub	x2, x2, x23
	strb	w5, [x6, x2]
	cmp	w19, w7
	beq	L342
	add	w19, w19, 1
L311:
	add	x1, x1, 1
	cmp	x4, x1
	bne	L314
	add	x21, x29, 80
	ldr	x0, [x24, 2120]
	mov	x16, x21
	str	w19, [x29, 96]
	bl	_anubis_vesting__serialize_vesting_state__write_u64.12
	ldr	x0, [x24, 2128]
	mov	x16, x21
	bl	_anubis_vesting__serialize_vesting_state__write_u64.12
	ldr	x0, [x24, 2136]
	mov	x16, x21
	bl	_anubis_vesting__serialize_vesting_state__write_u64.12
	ldr	x0, [x24, 2144]
	mov	x16, x21
	bl	_anubis_vesting__serialize_vesting_state__write_u64.12
	ldr	w1, [x29, 96]
	tbnz	w1, #31, L343
	ldr	x2, [x29, 72]
	ldr	w0, [x2, 4]
	cmp	w0, w1
	bge	L344
L316:
	mov	x20, 1
	mov	w22, 2147483647
	b	L320
	.p2align 2,,3
L325:
	tbnz	w1, #31, L335
	ldr	x2, [x29, 72]
L320:
	lsl	x19, x20, 3
	ldr	w0, [x2, 4]
	cmp	w0, w1
	blt	L321
	ldr	w0, [x2]
	cmp	w0, w1
	bgt	L345
	lsl	x19, x20, 3
	sub	x2, x19, x20
	add	x2, x20, x2, lsl 2
	add	x2, x24, x2, lsl 3
	ldrb	w2, [x2, -199]
	cmp	w2, 5
	bhi	L346
	ldr	x3, [x29, 64]
	sxtw	x0, w1
	sub	x0, x0, x23
	strb	w2, [x3, x0]
	cmp	w1, w22
	beq	L347
	add	w1, w1, 1
L321:
	sub	x19, x19, x20
	mov	x16, x21
	str	w1, [x29, 96]
	add	x19, x20, x19, lsl 2
	add	x20, x20, 1
	add	x19, x24, x19, lsl 3
	ldr	x0, [x19, -184]
	bl	_anubis_vesting__serialize_vesting_state__write_u64.12
	ldr	x0, [x19, -176]
	mov	x16, x21
	bl	_anubis_vesting__serialize_vesting_state__write_u64.12
	ldr	x0, [x19, -168]
	mov	x16, x21
	bl	_anubis_vesting__serialize_vesting_state__write_u64.12
	ldr	w1, [x29, 96]
	cmp	x20, 10
	bne	L325
	tbnz	w1, #31, L348
	ldr	x0, [x29, 72]
	ldr	w0, [x0]
	subs	w0, w1, w0
	bvs	L328
	tbnz	w0, #31, L349
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 112
LCFI103:
	ret
	.p2align 2,,3
L344:
LCFI104:
	ldr	w0, [x2]
	cmp	w0, w1
	bgt	L350
	ldrb	w3, [x24, 2152]
	cmp	w3, 1
	bhi	L351
	ldr	x5, [x29, 64]
	sxtw	x0, w1
	mov	w4, 2147483647
	sub	x0, x0, x23
	strb	w3, [x5, x0]
	cmp	w1, w4
	beq	L352
	add	w1, w1, 1
	b	L316
L342:
	adrp	x0, lC4@PAGE
	mov	w1, 586
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L341:
	adrp	x0, lC4@PAGE
	mov	w1, 585
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L335:
	adrp	x0, lC4@PAGE
	mov	w1, 604
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L347:
	adrp	x0, lC4@PAGE
	mov	w1, 606
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L346:
	adrp	x0, lC4@PAGE
	mov	w1, 605
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L345:
	adrp	x0, lC4@PAGE
	mov	w1, 605
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L343:
	adrp	x0, lC4@PAGE
	mov	w1, 597
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L349:
	adrp	x0, lC4@PAGE
	mov	w1, 613
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L328:
	adrp	x0, lC4@PAGE
	mov	w1, 613
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L351:
	adrp	x0, lC4@PAGE
	mov	w1, 598
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L352:
	adrp	x0, lC4@PAGE
	mov	w1, 599
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L348:
	adrp	x0, lC4@PAGE
	mov	w1, 613
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L350:
	adrp	x0, lC4@PAGE
	mov	w1, 598
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L340:
	adrp	x0, lC4@PAGE
	mov	w1, 565
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L339:
	adrp	x0, lC22@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC22@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE49:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__deserialize_vesting_state
_anubis_vesting__deserialize_vesting_state:
LFB51:
	stp	x29, x30, [sp, -112]!
LCFI105:
	mov	x29, sp
LCFI106:
	add	x3, x29, 64
	stp	x0, x1, [x29, 64]
	add	x0, x29, 112
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI107:
	ldr	w19, [x1]
	str	x23, [sp, 48]
LCFI108:
	str	x3, [x29, 88]
	str	x0, [x29, 104]
	tbnz	w19, #31, L375
	mov	x20, x2
	adrp	x1, lC2@PAGE
	adrp	x2, lC3@PAGE
	mov	x0, x20
	add	x2, x2, lC3@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_anubis_vesting__init_vesting
	ldr	x0, [x29, 72]
	ldp	w4, w2, [x0]
	sxtw	x0, w4
	add	x0, x0, 98
	cmp	x0, w2, sxtw
	bge	L370
	ldr	x7, [x29, 64]
	sxtw	x5, w19
	mov	x0, x20
	add	x3, x20, 32
	mov	w6, 2147483647
	.p2align 5,,15
L359:
	cmp	w2, w19
	blt	L356
	cmp	w4, w19
	bgt	L376
	sxtw	x1, w19
	sub	x1, x1, x5
	ldrb	w1, [x7, x1]
	strb	w1, [x0]
	cmp	w19, w6
	beq	L377
	add	w19, w19, 1
L356:
	add	x0, x0, 1
	cmp	x3, x0
	bne	L359
	add	x22, x29, 80
	str	x5, [x29, 80]
	mov	x16, x22
	str	w19, [x29, 96]
	bl	_anubis_vesting__deserialize_vesting_state__read_u64.13
	mov	x16, x22
	str	x0, [x20, 2120]
	bl	_anubis_vesting__deserialize_vesting_state__read_u64.13
	mov	x16, x22
	str	x0, [x20, 2128]
	bl	_anubis_vesting__deserialize_vesting_state__read_u64.13
	mov	x16, x22
	str	x0, [x20, 2136]
	bl	_anubis_vesting__deserialize_vesting_state__read_u64.13
	ldr	w1, [x29, 96]
	str	x0, [x20, 2144]
	ldr	x2, [x29, 80]
	tbnz	w1, #31, L378
	ldr	x0, [x29, 72]
	ldr	w3, [x0, 4]
	cmp	w3, w1
	blt	L361
	ldr	w3, [x0]
	cmp	w3, w1
	bgt	L379
	ldr	x5, [x29, 64]
	sxtw	x3, w1
	mov	w4, 2147483647
	sub	x3, x3, x2
	ldrb	w3, [x5, x3]
	cmp	w3, 0
	cset	w3, ne
	strb	w3, [x20, 2152]
	cmp	w1, w4
	beq	L380
	add	w1, w1, 1
L361:
	mov	x21, 1
	mov	w23, 2147483647
	b	L364
	.p2align 2,,3
L369:
	tbnz	w1, #31, L373
	ldr	x0, [x29, 72]
L364:
	ldr	w3, [x0, 4]
	cmp	w3, w1
	blt	L365
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L381
	ldr	x3, [x29, 64]
	sxtw	x0, w1
	sub	x0, x0, x2
	ldrb	w3, [x3, x0]
	cmp	w3, 5
	bhi	L367
	lsl	x0, x21, 3
	sub	x0, x0, x21
	add	x0, x21, x0, lsl 2
	add	x0, x20, x0, lsl 3
	strb	w3, [x0, -199]
L367:
	cmp	w1, w23
	beq	L382
	add	w1, w1, 1
L365:
	lsl	x19, x21, 3
	mov	x16, x22
	str	x2, [x29, 80]
	sub	x19, x19, x21
	str	w1, [x29, 96]
	add	x19, x21, x19, lsl 2
	add	x21, x21, 1
	bl	_anubis_vesting__deserialize_vesting_state__read_u64.13
	add	x19, x20, x19, lsl 3
	mov	x16, x22
	str	x0, [x19, -184]
	bl	_anubis_vesting__deserialize_vesting_state__read_u64.13
	mov	x16, x22
	str	x0, [x19, -176]
	bl	_anubis_vesting__deserialize_vesting_state__read_u64.13
	str	x0, [x19, -168]
	ldr	x2, [x29, 80]
	ldr	w1, [x29, 96]
	cmp	x21, 10
	bne	L369
	mov	w0, 1
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 112
LCFI109:
	ret
	.p2align 2,,3
L370:
LCFI110:
	mov	w0, 0
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 112
LCFI111:
	ret
L376:
LCFI112:
	adrp	x0, lC4@PAGE
	mov	w1, 646
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L377:
	adrp	x0, lC4@PAGE
	mov	w1, 647
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L373:
	adrp	x0, lC4@PAGE
	mov	w1, 665
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L381:
	adrp	x0, lC4@PAGE
	mov	w1, 666
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L382:
	adrp	x0, lC4@PAGE
	mov	w1, 672
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L375:
	adrp	x0, lC4@PAGE
	mov	w1, 621
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L378:
	adrp	x0, lC4@PAGE
	mov	w1, 658
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L379:
	adrp	x0, lC4@PAGE
	mov	w1, 659
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L380:
	adrp	x0, lC4@PAGE
	mov	w1, 660
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE51:
	.const
lC2:
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
	.byte	0
	.byte	0
	.align	2
lC3:
	.word	0
	.word	31
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__zeroize_vesting_state
_anubis_vesting__zeroize_vesting_state:
LFB53:
	movi	v30.4s, 0
	movi	v31.4s, 0
	mov	x2, 1
	add	x3, x0, 40
	mov	w5, w2
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	.p2align 5,,15
L384:
	lsl	x1, x2, 3
	sub	x1, x1, x2
	add	x1, x2, x1, lsl 2
	add	x2, x2, 1
	add	x1, x0, x1, lsl 3
	sub	x4, x1, #64
	strh	w5, [x1, -200]
	stp	q30, q30, [x3]
	stp	xzr, xzr, [x1, -160]
	add	x3, x3, 232
	stp	xzr, xzr, [x1, -144]
	stp	xzr, xzr, [x1, -128]
	stp	xzr, xzr, [x1, -112]
	stp	xzr, xzr, [x1, -96]
	stp	xzr, xzr, [x1, -80]
	str	q31, [x1, -64]
	stp	q31, q31, [x4, 16]
	str	q31, [x1, -16]
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	cmp	x2, 10
	bne	L384
	add	x1, x0, 2048
	str	q30, [x1, 72]
	str	q30, [x1, 88]
	strb	wzr, [x0, 2152]
	ret
LFE53:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__zeroize_milestone
_anubis_vesting__zeroize_milestone:
LFB54:
	movi	v31.4s, 0
	movi	v30.4s, 0
	mov	w2, 1
	add	x1, x0, 136
	stp	xzr, xzr, [x0, 40]
	strh	w2, [x0]
	str	q30, [x0, 8]
	str	q30, [x0, 24]
	stp	xzr, xzr, [x0, 56]
	stp	xzr, xzr, [x0, 72]
	stp	xzr, xzr, [x0, 88]
	stp	xzr, xzr, [x0, 104]
	stp	xzr, xzr, [x0, 120]
	str	q31, [x0, 136]
	stp	q31, q31, [x1, 16]
	str	q31, [x0, 184]
	stp	xzr, xzr, [x0, 200]
	stp	xzr, xzr, [x0, 216]
	ret
LFE54:
	.align	2
	.p2align 5,,15
	.globl _anubis_vesting__zeroize_submission
_anubis_vesting__zeroize_submission:
LFB55:
	movi	v31.4s, 0
	mov	x2, x0
	mov	w5, 1
	add	x4, x0, 33
	add	x1, x0, 122
	strb	w5, [x2], 1
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	str	q31, [x0, 92]
	str	xzr, [x0, 68]
	str	q31, [x0, 76]
	str	q31, [x0, 106]
	str	q31, [x0, 122]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	str	q31, [x0, 234]
	ret
LFE55:
	.const
	.align	3
_update_statusG.14:
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.space 3
	.align	3
_release_resultG.18:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	2
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.space 2
_verify_resultG.22:
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.align	3
_submit_resultG.26:
	.byte	0
	.byte	3
	.byte	0
	.byte	2
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	3
	.byte	5
	.byte	0
	.byte	0
	.byte	0
	.space 3
	.align	1
_submit_resultT2.27:
	.byte	2
	.byte	0
	.align	1
_submit_resultT1.28:
	.byte	3
	.byte	8
	.align	3
_submit_resultP.29:
	.word	2
	.word	9
	.align	3
_milestone_statusG.30:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	2
	.byte	3
	.byte	0
	.byte	3
	.byte	0
	.byte	2
	.space 3
	.align	3
_verification_typeG.34:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	2
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.space 1
	.globl _anubis_vesting__update_statusN
	.align	3
_anubis_vesting__update_statusN:
	.byte	1
	.byte	9
	.byte	16
	.byte	24
	.byte	30
	.byte	46
	.space 2
	.globl _anubis_vesting__update_statusS
	.align	3
_anubis_vesting__update_statusS:
	.ascii "ON_TRACKWARNINGSLOWDOWNPAUSEDDEAD_MAN_WARNING"
	.globl _anubis_vesting__release_resultN
	.align	3
_anubis_vesting__release_resultN:
	.byte	1
	.byte	9
	.byte	23
	.byte	35
	.byte	51
	.byte	73
	.space 2
	.globl _anubis_vesting__release_resultS
	.align	3
_anubis_vesting__release_resultS:
	.ascii "RELEASEDNOTHING_VESTEDNOT_UNLOCKEDALREADY_RELEASEDDEAD_MAN_SWITCH_ACTIVE"
	.globl _anubis_vesting__verify_resultN
	.align	3
_anubis_vesting__verify_resultN:
	.byte	1
	.byte	9
	.byte	28
	.byte	41
	.byte	55
	.byte	70
	.byte	77
	.space 1
	.globl _anubis_vesting__verify_resultS
	.align	3
_anubis_vesting__verify_resultS:
	.ascii "VERIFIEDVERIFICATION_FAILEDINVALID_PROOFAUDIT_REQUIREDORACLE_REJECTEDTIMEOUT"
	.globl _anubis_vesting__submit_resultN
	.align	3
_anubis_vesting__submit_resultN:
	.byte	1
	.byte	9
	.byte	26
	.byte	43
	.byte	60
	.byte	79
	.byte	96
	.space 1
	.globl _anubis_vesting__submit_resultS
	.align	3
_anubis_vesting__submit_resultS:
	.ascii "ACCEPTEDINVALID_MILESTONEALREADY_SUBMITTEDINVALID_SIGNATUREPREVIOUS_INCOMPLETESUBMISSION_FAILED"
	.globl _anubis_vesting__milestone_statusN
	.align	3
_anubis_vesting__milestone_statusN:
	.byte	1
	.byte	8
	.byte	17
	.byte	26
	.byte	34
	.byte	41
	.byte	53
	.space 1
	.globl _anubis_vesting__milestone_statusS
	.align	3
_anubis_vesting__milestone_statusS:
	.ascii "PENDINGSUBMITTEDVERIFYINGUNLOCKEDVESTINGFULLY_VESTED"
	.globl _anubis_vesting__milestone_verification
	.align	3
_anubis_vesting__milestone_verification:
	.byte	1
	.byte	1
	.byte	2
	.byte	3
	.byte	1
	.byte	3
	.byte	4
	.byte	5
	.byte	5
	.space 7
	.globl _anubis_vesting__verification_typeN
	.align	3
_anubis_vesting__verification_typeN:
	.byte	1
	.byte	15
	.byte	30
	.byte	46
	.byte	58
	.byte	71
	.byte	84
	.space 1
	.globl _anubis_vesting__verification_typeS
	.align	3
_anubis_vesting__verification_typeS:
	.ascii "GNATPROVE_ONLYGNATPROVE_AUDITTEST_SUITE_AUDITTESTNET_DEMOMAINNET_BLOCKUPTIME_ORACLE"
	.globl _anubis_vesting__milestone_amounts
	.align	3
_anubis_vesting__milestone_amounts:
	.xword	50000000
	.xword	50000000
	.xword	50000000
	.xword	50000000
	.xword	30000000
	.xword	30000000
	.xword	20000000
	.xword	10000000
	.xword	10000000
	.data
	.align	3
_anubis_vesting__nft_counter:
	.space 8
	.align	3
_anubis_vesting__last_update_block:
	.space 8
	.globl _anubis_vesting_E
	.align	1
_anubis_vesting_E:
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
	.quad	LFB50-.
	.set L$set$2,LFE50-LFB50
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB50
	.long L$set$3
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$7,LEFDE3-LASFDE3
	.long L$set$7
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB52-.
	.set L$set$8,LFE52-LFB52
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB52
	.long L$set$9
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$10,LCFI5-LCFI4
	.long L$set$10
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$11,LCFI6-LCFI5
	.long L$set$11
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd7
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
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
	.quad	LFB25-.
	.set L$set$17,LFE25-LFB25
	.quad L$set$17
	.uleb128 0
	.byte	0x4
	.set L$set$18,LCFI11-LFB25
	.long L$set$18
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
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
LSFDE7:
	.set L$set$22,LEFDE7-LASFDE7
	.long L$set$22
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB2-.
	.set L$set$23,LFE2-LFB2
	.quad L$set$23
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$24,LEFDE9-LASFDE9
	.long L$set$24
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB3-.
	.set L$set$25,LFE3-LFB3
	.quad L$set$25
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$26,LEFDE11-LASFDE11
	.long L$set$26
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB4-.
	.set L$set$27,LFE4-LFB4
	.quad L$set$27
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$28,LEFDE13-LASFDE13
	.long L$set$28
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB5-.
	.set L$set$29,LFE5-LFB5
	.quad L$set$29
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$30,LEFDE15-LASFDE15
	.long L$set$30
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB129-.
	.set L$set$31,LFE129-LFB129
	.quad L$set$31
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$32,LEFDE17-LASFDE17
	.long L$set$32
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB131-.
	.set L$set$33,LFE131-LFB131
	.quad L$set$33
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$34,LEFDE19-LASFDE19
	.long L$set$34
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB133-.
	.set L$set$35,LFE133-LFB133
	.quad L$set$35
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$36,LEFDE21-LASFDE21
	.long L$set$36
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB9-.
	.set L$set$37,LFE9-LFB9
	.quad L$set$37
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$38,LEFDE23-LASFDE23
	.long L$set$38
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB10-.
	.set L$set$39,LFE10-LFB10
	.quad L$set$39
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$40,LEFDE25-LASFDE25
	.long L$set$40
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB11-.
	.set L$set$41,LFE11-LFB11
	.quad L$set$41
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$42,LEFDE27-LASFDE27
	.long L$set$42
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB135-.
	.set L$set$43,LFE135-LFB135
	.quad L$set$43
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$44,LEFDE29-LASFDE29
	.long L$set$44
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB13-.
	.set L$set$45,LFE13-LFB13
	.quad L$set$45
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$46,LEFDE31-LASFDE31
	.long L$set$46
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB137-.
	.set L$set$47,LFE137-LFB137
	.quad L$set$47
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$48,LEFDE33-LASFDE33
	.long L$set$48
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB15-.
	.set L$set$49,LFE15-LFB15
	.quad L$set$49
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI15-LFB15
	.long L$set$50
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$51,LCFI16-LCFI15
	.long L$set$51
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$52,LCFI17-LCFI16
	.long L$set$52
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI18-LCFI17
	.long L$set$53
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$54,LEFDE35-LASFDE35
	.long L$set$54
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB19-.
	.set L$set$55,LFE19-LFB19
	.quad L$set$55
	.uleb128 0
	.byte	0x4
	.set L$set$56,LCFI19-LFB19
	.long L$set$56
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$57,LCFI20-LCFI19
	.long L$set$57
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$58,LCFI21-LCFI20
	.long L$set$58
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$59,LCFI22-LCFI21
	.long L$set$59
	.byte	0xb
	.byte	0x4
	.set L$set$60,LCFI23-LCFI22
	.long L$set$60
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$61,LCFI24-LCFI23
	.long L$set$61
	.byte	0xb
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$62,LEFDE37-LASFDE37
	.long L$set$62
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB21-.
	.set L$set$63,LFE21-LFB21
	.quad L$set$63
	.uleb128 0
	.byte	0x4
	.set L$set$64,LCFI25-LFB21
	.long L$set$64
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$65,LCFI26-LCFI25
	.long L$set$65
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$66,LCFI27-LCFI26
	.long L$set$66
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$67,LCFI28-LCFI27
	.long L$set$67
	.byte	0xb
	.byte	0x4
	.set L$set$68,LCFI29-LCFI28
	.long L$set$68
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$69,LCFI30-LCFI29
	.long L$set$69
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$70,LEFDE39-LASFDE39
	.long L$set$70
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB17-.
	.set L$set$71,LFE17-LFB17
	.quad L$set$71
	.uleb128 0
	.byte	0x4
	.set L$set$72,LCFI31-LFB17
	.long L$set$72
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$73,LCFI32-LCFI31
	.long L$set$73
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$74,LCFI33-LCFI32
	.long L$set$74
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$75,LCFI34-LCFI33
	.long L$set$75
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$76,LCFI35-LCFI34
	.long L$set$76
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
	.set L$set$77,LCFI36-LCFI35
	.long L$set$77
	.byte	0xb
	.byte	0x4
	.set L$set$78,LCFI37-LCFI36
	.long L$set$78
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
	.set L$set$79,LCFI38-LCFI37
	.long L$set$79
	.byte	0xb
	.byte	0x4
	.set L$set$80,LCFI39-LCFI38
	.long L$set$80
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
	.set L$set$81,LCFI40-LCFI39
	.long L$set$81
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$82,LEFDE41-LASFDE41
	.long L$set$82
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB22-.
	.set L$set$83,LFE22-LFB22
	.quad L$set$83
	.uleb128 0
	.byte	0x4
	.set L$set$84,LCFI41-LFB22
	.long L$set$84
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$85,LCFI42-LCFI41
	.long L$set$85
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$86,LEFDE43-LASFDE43
	.long L$set$86
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB24-.
	.set L$set$87,LFE24-LFB24
	.quad L$set$87
	.uleb128 0
	.byte	0x4
	.set L$set$88,LCFI43-LFB24
	.long L$set$88
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$89,LCFI44-LCFI43
	.long L$set$89
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$90,LCFI45-LCFI44
	.long L$set$90
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$91,LEFDE45-LASFDE45
	.long L$set$91
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB26-.
	.set L$set$92,LFE26-LFB26
	.quad L$set$92
	.uleb128 0
	.byte	0x4
	.set L$set$93,LCFI46-LFB26
	.long L$set$93
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$94,LCFI47-LCFI46
	.long L$set$94
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$95,LCFI48-LCFI47
	.long L$set$95
	.byte	0x93
	.uleb128 0x4
	.byte	0x4
	.set L$set$96,LCFI49-LCFI48
	.long L$set$96
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$97,LEFDE47-LASFDE47
	.long L$set$97
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB27-.
	.set L$set$98,LFE27-LFB27
	.quad L$set$98
	.uleb128 0
	.byte	0x4
	.set L$set$99,LCFI50-LFB27
	.long L$set$99
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$100,LCFI51-LCFI50
	.long L$set$100
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$101,LCFI52-LCFI51
	.long L$set$101
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$102,LCFI53-LCFI52
	.long L$set$102
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x99
	.uleb128 0x4
	.byte	0x9a
	.uleb128 0x3
	.byte	0x4
	.set L$set$103,LCFI54-LCFI53
	.long L$set$103
	.byte	0xa
	.byte	0xde
	.byte	0xdd
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
	.set L$set$104,LCFI55-LCFI54
	.long L$set$104
	.byte	0xb
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$105,LEFDE49-LASFDE49
	.long L$set$105
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB29-.
	.set L$set$106,LFE29-LFB29
	.quad L$set$106
	.uleb128 0
	.byte	0x4
	.set L$set$107,LCFI56-LFB29
	.long L$set$107
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$108,LCFI57-LCFI56
	.long L$set$108
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$109,LCFI58-LCFI57
	.long L$set$109
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$110,LCFI59-LCFI58
	.long L$set$110
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$111,LCFI60-LCFI59
	.long L$set$111
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x99
	.uleb128 0x4
	.byte	0x4
	.set L$set$112,LCFI61-LCFI60
	.long L$set$112
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd9
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
	.set L$set$113,LCFI62-LCFI61
	.long L$set$113
	.byte	0xb
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$114,LEFDE51-LASFDE51
	.long L$set$114
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB31-.
	.set L$set$115,LFE31-LFB31
	.quad L$set$115
	.uleb128 0
	.byte	0x4
	.set L$set$116,LCFI63-LFB31
	.long L$set$116
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$117,LCFI64-LCFI63
	.long L$set$117
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$118,LCFI65-LCFI64
	.long L$set$118
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$119,LCFI66-LCFI65
	.long L$set$119
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$120,LCFI67-LCFI66
	.long L$set$120
	.byte	0xb
	.byte	0x4
	.set L$set$121,LCFI68-LCFI67
	.long L$set$121
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$122,LCFI69-LCFI68
	.long L$set$122
	.byte	0xb
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$123,LEFDE53-LASFDE53
	.long L$set$123
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB32-.
	.set L$set$124,LFE32-LFB32
	.quad L$set$124
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI70-LFB32
	.long L$set$125
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$126,LCFI71-LCFI70
	.long L$set$126
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$127,LEFDE55-LASFDE55
	.long L$set$127
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB33-.
	.set L$set$128,LFE33-LFB33
	.quad L$set$128
	.uleb128 0
	.byte	0x4
	.set L$set$129,LCFI72-LFB33
	.long L$set$129
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$130,LCFI73-LCFI72
	.long L$set$130
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$131,LEFDE57-LASFDE57
	.long L$set$131
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB35-.
	.set L$set$132,LFE35-LFB35
	.quad L$set$132
	.uleb128 0
	.byte	0x4
	.set L$set$133,LCFI74-LFB35
	.long L$set$133
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$134,LCFI75-LCFI74
	.long L$set$134
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$135,LCFI76-LCFI75
	.long L$set$135
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$136,LCFI77-LCFI76
	.long L$set$136
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$137,LCFI78-LCFI77
	.long L$set$137
	.byte	0xb
	.byte	0x4
	.set L$set$138,LCFI79-LCFI78
	.long L$set$138
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$139,LCFI80-LCFI79
	.long L$set$139
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$140,LEFDE59-LASFDE59
	.long L$set$140
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB37-.
	.set L$set$141,LFE37-LFB37
	.quad L$set$141
	.uleb128 0
	.byte	0x4
	.set L$set$142,LCFI81-LFB37
	.long L$set$142
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$143,LCFI82-LCFI81
	.long L$set$143
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$144,LCFI83-LCFI82
	.long L$set$144
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$145,LEFDE61-LASFDE61
	.long L$set$145
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB39-.
	.set L$set$146,LFE39-LFB39
	.quad L$set$146
	.uleb128 0
	.byte	0x4
	.set L$set$147,LCFI84-LFB39
	.long L$set$147
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$148,LCFI85-LCFI84
	.long L$set$148
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$149,LCFI86-LCFI85
	.long L$set$149
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$150,LCFI87-LCFI86
	.long L$set$150
	.byte	0xb
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$151,LEFDE63-LASFDE63
	.long L$set$151
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB41-.
	.set L$set$152,LFE41-LFB41
	.quad L$set$152
	.uleb128 0
	.byte	0x4
	.set L$set$153,LCFI88-LFB41
	.long L$set$153
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$154,LCFI89-LCFI88
	.long L$set$154
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$155,LEFDE65-LASFDE65
	.long L$set$155
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB42-.
	.set L$set$156,LFE42-LFB42
	.quad L$set$156
	.uleb128 0
	.byte	0x4
	.set L$set$157,LCFI90-LFB42
	.long L$set$157
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$158,LCFI91-LCFI90
	.long L$set$158
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$159,LEFDE67-LASFDE67
	.long L$set$159
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB43-.
	.set L$set$160,LFE43-LFB43
	.quad L$set$160
	.uleb128 0
	.byte	0x4
	.set L$set$161,LCFI92-LFB43
	.long L$set$161
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$162,LCFI93-LCFI92
	.long L$set$162
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$163,LCFI94-LCFI93
	.long L$set$163
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$164,LCFI95-LCFI94
	.long L$set$164
	.byte	0xb
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$165,LEFDE69-LASFDE69
	.long L$set$165
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB44-.
	.set L$set$166,LFE44-LFB44
	.quad L$set$166
	.uleb128 0
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$167,LEFDE71-LASFDE71
	.long L$set$167
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB45-.
	.set L$set$168,LFE45-LFB45
	.quad L$set$168
	.uleb128 0
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$169,LEFDE73-LASFDE73
	.long L$set$169
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB46-.
	.set L$set$170,LFE46-LFB46
	.quad L$set$170
	.uleb128 0
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$171,LEFDE75-LASFDE75
	.long L$set$171
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB48-.
	.set L$set$172,LFE48-LFB48
	.quad L$set$172
	.uleb128 0
	.byte	0x4
	.set L$set$173,LCFI96-LFB48
	.long L$set$173
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$174,LCFI97-LCFI96
	.long L$set$174
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$175,LCFI98-LCFI97
	.long L$set$175
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$176,LCFI99-LCFI98
	.long L$set$176
	.byte	0xb
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$177,LEFDE77-LASFDE77
	.long L$set$177
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB49-.
	.set L$set$178,LFE49-LFB49
	.quad L$set$178
	.uleb128 0
	.byte	0x4
	.set L$set$179,LCFI100-LFB49
	.long L$set$179
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$180,LCFI101-LCFI100
	.long L$set$180
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$181,LCFI102-LCFI101
	.long L$set$181
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x96
	.uleb128 0x9
	.byte	0x97
	.uleb128 0x8
	.byte	0x98
	.uleb128 0x7
	.byte	0x4
	.set L$set$182,LCFI103-LCFI102
	.long L$set$182
	.byte	0xa
	.byte	0xde
	.byte	0xdd
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
	.set L$set$183,LCFI104-LCFI103
	.long L$set$183
	.byte	0xb
	.align	3
LEFDE77:
LSFDE79:
	.set L$set$184,LEFDE79-LASFDE79
	.long L$set$184
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB51-.
	.set L$set$185,LFE51-LFB51
	.quad L$set$185
	.uleb128 0
	.byte	0x4
	.set L$set$186,LCFI105-LFB51
	.long L$set$186
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$187,LCFI106-LCFI105
	.long L$set$187
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$188,LCFI107-LCFI106
	.long L$set$188
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x96
	.uleb128 0x9
	.byte	0x4
	.set L$set$189,LCFI108-LCFI107
	.long L$set$189
	.byte	0x97
	.uleb128 0x8
	.byte	0x4
	.set L$set$190,LCFI109-LCFI108
	.long L$set$190
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd7
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$191,LCFI110-LCFI109
	.long L$set$191
	.byte	0xb
	.byte	0x4
	.set L$set$192,LCFI111-LCFI110
	.long L$set$192
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd7
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$193,LCFI112-LCFI111
	.long L$set$193
	.byte	0xb
	.align	3
LEFDE79:
LSFDE81:
	.set L$set$194,LEFDE81-LASFDE81
	.long L$set$194
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB53-.
	.set L$set$195,LFE53-LFB53
	.quad L$set$195
	.uleb128 0
	.align	3
LEFDE81:
LSFDE83:
	.set L$set$196,LEFDE83-LASFDE83
	.long L$set$196
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB54-.
	.set L$set$197,LFE54-LFB54
	.quad L$set$197
	.uleb128 0
	.align	3
LEFDE83:
LSFDE85:
	.set L$set$198,LEFDE85-LASFDE85
	.long L$set$198
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB55-.
	.set L$set$199,LFE55-LFB55
	.quad L$set$199
	.uleb128 0
	.align	3
LEFDE85:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
