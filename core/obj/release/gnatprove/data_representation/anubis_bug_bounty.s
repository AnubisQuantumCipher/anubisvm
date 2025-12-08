	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__reward_rangeIP
_anubis_bug_bounty__reward_rangeIP:
LFB2:
	mov	x0, 0
	mov	x1, 0
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__severity_levelH
_anubis_bug_bounty__severity_levelH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L5
	ldrb	w1, [x0]
	mov	w2, 60495
	movk	w2, 0x4ec4, lsl 16
	add	w0, w1, w1, lsl 1
	lsl	w1, w1, 3
	umull	x3, w1, w2
	umull	x2, w0, w2
	lsr	x3, x3, 34
	lsr	x2, x2, 34
	add	w5, w3, w3, lsl 1
	add	w4, w2, w2, lsl 1
	add	w3, w3, w5, lsl 2
	add	w2, w2, w4, lsl 2
	sub	w1, w1, w3
	sub	w0, w0, w2
	adrp	x2, _severity_levelG.17@PAGE
	add	x2, x2, _severity_levelG.17@PAGEOFF;
	sxtw	x1, w1
	sxtw	x0, w0
	ldrb	w1, [x2, x1]
	ldrb	w0, [x2, x0]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L5:
	adrp	x2, _severity_levelG.17@PAGE
	mov	x1, 0
	add	x2, x2, _severity_levelG.17@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__bug_categoryH
_anubis_bug_bounty__bug_categoryH:
LFB4:
	ldp	w4, w1, [x1]
	mov	w11, -1
	adrp	x10, _bug_categoryP.16@PAGE
	add	w7, w11, 1
	add	x10, x10, _bug_categoryP.16@PAGEOFF;
	adrp	x13, _bug_categoryT1.15@PAGE
	adrp	x12, _bug_categoryT2.14@PAGE
	mov	w2, 0
	mov	w3, 0
	add	x13, x13, _bug_categoryT1.15@PAGEOFF;
	add	x12, x12, _bug_categoryT2.14@PAGEOFF;
	add	w9, w4, w11
	cmp	w4, w1
	sxtw	x15, w4
	sub	w1, w1, w4
	ldr	w4, [x10, w7, sxtw 2]
	sxtw	x9, w9
	csinc	w6, wzr, w1, gt
	mov	w14, 2
	add	x1, x9, w4, sxtw
	mov	w5, 23
	mov	w11, 0
	sub	x1, x1, x15
	cmp	w6, w4
	blt	L8
L12:
	ldrb	w8, [x0, x1]
	ldrb	w4, [x13, w7, sxtw]
	ldrb	w1, [x12, w7, sxtw]
	madd	w4, w4, w8, w3
	madd	w1, w1, w8, w2
	sdiv	w3, w4, w5
	sdiv	w2, w1, w5
	msub	w3, w3, w5, w4
	msub	w2, w2, w5, w1
	cmp	w14, 1
	beq	L8
	add	w7, w11, 1
	mov	w14, 1
	ldr	w4, [x10, w7, sxtw 2]
	mov	w11, 0
	add	x1, x9, w4, sxtw
	sub	x1, x1, x15
	cmp	w6, w4
	bge	L12
L8:
	adrp	x4, _bug_categoryG.13@PAGE
	mov	w1, 35747
	add	x4, x4, _bug_categoryG.13@PAGEOFF;
	movk	w1, 0xba2e, lsl 16
	ldrb	w0, [x4, w3, sxtw]
	ldrb	w2, [x4, w2, sxtw]
	add	w2, w0, w2
	umull	x1, w2, w1
	lsr	x1, x1, 35
	add	w0, w1, w1, lsl 2
	add	w0, w1, w0, lsl 1
	sub	w0, w2, w0
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__report_statusH
_anubis_bug_bounty__report_statusH:
LFB5:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _report_statusP.12@PAGE
	add	w5, w10, 1
	add	x9, x9, _report_statusP.12@PAGEOFF;
	adrp	x12, _report_statusT1.11@PAGE
	adrp	x11, _report_statusT2.10@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _report_statusT1.11@PAGEOFF;
	add	x11, x11, _report_statusT2.10@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 19
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L15
L19:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	add	w6, w2, w2, lsl 3
	add	w6, w2, w6, lsl 1
	add	w2, w5, w5, lsl 3
	sub	w6, w3, w6
	add	w2, w5, w2, lsl 1
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L15
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L19
L15:
	adrp	x1, _report_statusG.9@PAGE
	add	x1, x1, _report_statusG.9@PAGEOFF;
	ldrb	w0, [x1, w6, sxtw]
	ldrb	w1, [x1, w2, sxtw]
	add	w0, w0, w1
	and	w0, w0, 7
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__bug_reportIP
_anubis_bug_bounty__bug_reportIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__report_arrayIP
_anubis_bug_bounty__report_arrayIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__bounty_stateIP
_anubis_bug_bounty__bounty_stateIP:
LFB84:
	ret
LFE84:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__submit_resultH
_anubis_bug_bounty__submit_resultH:
LFB9:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L25
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
L24:
	adrp	x3, _submit_resultG.5@PAGE
	mov	w1, 52429
	add	x3, x3, _submit_resultG.5@PAGEOFF;
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
L25:
	mov	x2, 0
	mov	x4, 0
	b	L24
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__payout_resultH
_anubis_bug_bounty__payout_resultH:
LFB10:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L28
	ldrb	w0, [x0]
	mov	w2, 36409
	movk	w2, 0x38e3, lsl 16
	add	w1, w0, w0, lsl 1
	lsl	w0, w0, 2
	lsl	w1, w1, 1
	umull	x3, w0, w2
	umull	x2, w1, w2
	lsr	x3, x3, 33
	add	w3, w3, w3, lsl 3
	lsr	x2, x2, 33
	add	w2, w2, w2, lsl 3
	sub	w0, w0, w3
	sxtw	x0, w0
	sub	w1, w1, w2
	adrp	x2, _payout_resultG.1@PAGE
	add	x2, x2, _payout_resultG.1@PAGEOFF;
	sxtw	x1, w1
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L28:
	adrp	x2, _payout_resultG.1@PAGE
	mov	x1, 0
	add	x2, x2, _payout_resultG.1@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__init_bug_bounty
_anubis_bug_bounty__init_bug_bounty:
LFB11:
	adrp	x2, lC2@PAGE
	movi	v31.4s, 0
	stp	x1, x1, [x0, 48]
	ldr	q30, [x2, #lC2@PAGEOFF]
	str	q31, [x0, 32]
	stp	q30, q31, [x0]
	ret
LFE11:
	.const
	.align	3
lC3:
	.ascii "failed precondition from anubis_bug_bounty.ads:180"
	.align	3
lC4:
	.ascii "anubis_bug_bounty.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__submit_report
_anubis_bug_bounty__submit_report:
LFB13:
	stp	x29, x30, [sp, -80]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
LCFI2:
	mov	x19, x0
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	str	x25, [sp, 64]
LCFI3:
	ldp	w9, w21, [x2]
	ldr	x8, [x29, 88]
	ldp	w10, w23, [x5]
	sxtw	x2, w9
	sxtw	x15, w21
	add	x11, x2, 31
	ldp	w12, w20, [x8]
	cmp	x11, x15
	ldp	w11, w22, [x7]
	sxtw	x5, w10
	sxtw	x16, w23
	add	x0, x5, 31
	ccmp	x0, x16, 0, eq
	cset	w13, ne
	ldr	x0, [x29, 80]
	sxtw	x8, w12
	sxtw	x30, w20
	add	x24, x8, 31
	sxtw	x7, w11
	sxtw	x17, w22
	add	x14, x7, 31
	cmp	x14, x17
	ldr	x14, [x29, 96]
	ccmp	x24, x30, 0, eq
	cset	w24, ne
	orr	w13, w13, w24
	cbnz	w13, L73
	ldr	x13, [x19, 8]
	cbz	x13, L74
	adrp	x25, _anubis_bug_bounty__report_counter@PAGE
	mov	x24, x14
	ldr	x13, [x25, #_anubis_bug_bounty__report_counter@PAGEOFF]
	add	x13, x13, 1
	str	x13, [x24], 8
	stp	xzr, xzr, [x14, 8]
	str	x13, [x25, #_anubis_bug_bounty__report_counter@PAGEOFF]
	stp	xzr, xzr, [x24, 16]
	cmp	w9, w21
	bgt	L34
	sub	x21, x14, x2
	sub	x1, x1, x2
	add	x21, x21, 8
	sub	x2, x2, #1
	.p2align 5,,15
L40:
	add	x2, x2, 1
	subs	w13, w2, w9
	bvs	L36
	cmp	w13, 31
	bgt	L38
	bhi	L75
	ldrb	w13, [x1, x2]
	strb	w13, [x21, x2]
L38:
	cmp	x15, x2
	bne	L40
L34:
	cmp	w3, 10
	bhi	L76
	adrp	x1, _anubis_bug_bounty__category_severity@PAGE
	strb	w3, [x14, 40]
	add	x1, x1, _anubis_bug_bounty__category_severity@PAGEOFF;
	ldrb	w3, [x1, w3, uxtw]
	cmp	w3, 3
	bhi	L77
	add	x1, x14, 43
	strb	w3, [x14, 41]
	strb	wzr, [x14, 42]
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	cmp	w10, w23
	bgt	L43
	sub	x9, x14, x5
	sub	x4, x4, x5
	add	x9, x9, 43
	sub	x1, x5, #1
	.p2align 5,,15
L49:
	add	x1, x1, 1
	subs	w2, w1, w10
	bvs	L45
	cmp	w2, 31
	bgt	L47
	bhi	L78
	ldrb	w2, [x4, x1]
	strb	w2, [x9, x1]
L47:
	cmp	x16, x1
	bne	L49
L43:
	add	x1, x14, 75
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	cmp	w11, w22
	bgt	L50
	sub	x4, x14, x7
	sub	x6, x6, x7
	add	x4, x4, 75
	sub	x1, x7, #1
	.p2align 5,,15
L56:
	add	x1, x1, 1
	subs	w2, w1, w11
	bvs	L52
	cmp	w2, 31
	bgt	L54
	bhi	L79
	ldrb	w2, [x6, x1]
	strb	w2, [x4, x1]
L54:
	cmp	x17, x1
	bne	L56
L50:
	add	x1, x14, 107
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	cmp	w12, w20
	bgt	L57
	sub	x4, x14, x8
	sub	x0, x0, x8
	add	x4, x4, 107
	sub	x1, x8, #1
	.p2align 5,,15
L63:
	add	x1, x1, 1
	subs	w2, w1, w12
	bvs	L59
	cmp	w2, 31
	bgt	L61
	bhi	L80
	ldrb	w2, [x0, x1]
	strb	w2, [x4, x1]
L61:
	cmp	x30, x1
	bne	L63
L57:
	ldr	x1, [x19, 24]
	movi	v31.4s, 0
	mov	w0, 0
	ldr	x4, [x19, 56]
	strb	w3, [x14, 185]
	str	q31, [x14, 155]
	add	x1, x1, 1
	str	q31, [x14, 139]
	str	q31, [x14, 169]
	str	xzr, [x14, 192]
	stp	xzr, xzr, [x14, 200]
	stp	xzr, xzr, [x14, 216]
	stp	x4, xzr, [x14, 232]
	stp	xzr, xzr, [x14, 248]
	str	x1, [x19, 24]
	ldr	x25, [sp, 64]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 80
LCFI4:
	ret
	.p2align 2,,3
L74:
LCFI5:
	mov	x2, 264
	mov	w1, 0
	mov	x0, x14
	bl	_memset
	mov	w2, 777
	mov	x14, x0
	movk	w2, 0x5, lsl 16
	mov	w1, 3
	mov	w0, 3
	str	w2, [x14, 40]
	strb	w1, [x14, 185]
	ldr	x25, [sp, 64]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 80
LCFI6:
	ret
L36:
LCFI7:
	adrp	x0, lC4@PAGE
	mov	w1, 82
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L45:
	adrp	x0, lC4@PAGE
	mov	w1, 94
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L52:
	adrp	x0, lC4@PAGE
	mov	w1, 102
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L59:
	adrp	x0, lC4@PAGE
	mov	w1, 110
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L75:
	adrp	x0, lC4@PAGE
	mov	w1, 83
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L80:
	adrp	x0, lC4@PAGE
	mov	w1, 111
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L78:
	adrp	x0, lC4@PAGE
	mov	w1, 95
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L79:
	adrp	x0, lC4@PAGE
	mov	w1, 103
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L73:
	adrp	x0, lC3@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC3@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L77:
	adrp	x0, lC4@PAGE
	mov	w1, 88
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L76:
	adrp	x0, lC4@PAGE
	mov	w1, 87
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.const
	.align	2
lC1:
	.word	1
	.word	50
	.text
	.const
	.align	3
lC5:
	.ascii "failed precondition from anubis_bug_bounty.ads:198"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__assess_report
_anubis_bug_bounty__assess_report:
LFB14:
	stp	x29, x30, [sp, -16]!
LCFI8:
	mov	x29, sp
LCFI9:
	ldpsw	x1, x2, [x2]
	add	x1, x1, 31
	cmp	x1, x2
	bne	L91
	ldrb	w1, [x0, 42]
	cmp	w1, 7
	bhi	L92
	cmp	w1, 1
	bhi	L89
	cmp	w3, 1
	bhi	L93
	cbz	w3, L86
	mov	w1, 2
	strb	w1, [x0, 42]
	cmp	w4, 3
	bhi	L94
	strb	w4, [x0, 185]
	str	x5, [x0, 192]
L88:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI10:
	ret
	.p2align 2,,3
L86:
LCFI11:
	mov	w1, 5
	str	xzr, [x0, 192]
	strb	w1, [x0, 42]
	b	L88
	.p2align 2,,3
L89:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI12:
	ret
L92:
LCFI13:
	adrp	x0, lC4@PAGE
	mov	w1, 144
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L91:
	adrp	x0, lC5@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L93:
	adrp	x0, lC4@PAGE
	mov	w1, 155
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L94:
	adrp	x0, lC4@PAGE
	mov	w1, 157
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE14:
	.const
	.align	3
lC6:
	.ascii "anubis_bug_bounty.ads"
	.space 1
	.align	3
lC7:
	.ascii "failed precondition from anubis_bug_bounty.ads:206"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__calculate_reward
_anubis_bug_bounty__calculate_reward:
LFB15:
	stp	x29, x30, [sp, -16]!
LCFI14:
	orr	w3, w1, w2
	mov	x29, sp
LCFI15:
	tbnz	w3, #31, L101
	cmp	w1, 100
	mov	w3, 100
	ccmp	w2, w3, 0, le
	bgt	L102
	cmp	w0, 10
	bhi	L99
	adrp	x3, _anubis_bug_bounty__category_severity@PAGE
	add	x3, x3, _anubis_bug_bounty__category_severity@PAGEOFF;
	ldrb	w3, [x3, w0, uxtw]
	cmp	w3, 3
	bhi	L99
	adrp	x4, _anubis_bug_bounty__severity_rewards@PAGE
	ubfiz	x3, x3, 4, 8
	add	x4, x4, _anubis_bug_bounty__severity_rewards@PAGEOFF;
	add	w0, w1, w2
	add	x1, x4, x3
	ldr	x2, [x4, x3]
	asr	w0, w0, 1
	sxtw	x0, w0
	mov	x3, 62915
	ldr	x1, [x1, 8]
	movk	x3, 0x5c28, lsl 16
	movk	x3, 0xc28f, lsl 32
	movk	x3, 0x28f5, lsl 48
	sub	x4, x1, x2
	mul	x0, x0, x4
	lsr	x0, x0, 2
	umulh	x0, x0, x3
	add	x0, x2, x0, lsr 2
	cmp	x0, x1
	csel	x0, x0, x1, ls
	ldp	x29, x30, [sp], 16
LCFI16:
	ret
L101:
LCFI17:
	adrp	x0, lC6@PAGE
	mov	w1, 206
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L99:
	adrp	x0, lC4@PAGE
	mov	w1, 172
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L102:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE15:
	.const
	.align	3
lC8:
	.ascii "failed precondition from anubis_bug_bounty.ads:218"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__process_payout
_anubis_bug_bounty__process_payout:
LFB16:
	stp	x29, x30, [sp, -16]!
LCFI18:
	mov	x29, sp
LCFI19:
	ldrb	w2, [x1, 42]
	cmp	w2, 7
	bhi	L110
	sub	w2, w2, #2
	and	w2, w2, 255
	cmp	w2, 1
	bhi	L111
	ldr	x2, [x1, 256]
	mov	x3, x0
	mov	w0, 2
	cbnz	x2, L106
	ldr	x2, [x3, 8]
	mov	w0, 3
	ldr	x4, [x1, 192]
	cmp	x4, x2
	bhi	L106
	ldr	x6, [x3, 16]
	sub	x2, x2, x4
	mov	w8, 6
	mov	w0, 0
	ldr	x5, [x3, 32]
	ldr	x7, [x3, 56]
	add	x4, x6, x4
	stp	x2, x4, [x3, 8]
	add	x2, x5, 1
	str	x2, [x3, 32]
	strb	w8, [x1, 42]
	str	x7, [x1, 256]
L106:
	ldp	x29, x30, [sp], 16
LCFI20:
	ret
L111:
LCFI21:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L110:
	adrp	x0, lC6@PAGE
	mov	w1, 218
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__get_remaining_pool
_anubis_bug_bounty__get_remaining_pool:
LFB17:
	ldr	x0, [x0, 8]
	ret
LFE17:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__get_reward_range
_anubis_bug_bounty__get_reward_range:
LFB18:
	cmp	w0, 3
	bhi	L118
	adrp	x1, _anubis_bug_bounty__severity_rewards@PAGE
	add	x1, x1, _anubis_bug_bounty__severity_rewards@PAGEOFF;
	add	x0, x1, w0, uxtb 4
	ldp	x0, x1, [x0]
	ret
L118:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI22:
	mov	w1, 243
	mov	x29, sp
LCFI23:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE18:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__zeroize_bounty_state
_anubis_bug_bounty__zeroize_bounty_state:
LFB19:
	movi	v31.4s, 0
	stp	q31, q31, [x0]
	stp	q31, q31, [x0, 32]
	ret
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_bug_bounty__zeroize_report
_anubis_bug_bounty__zeroize_report:
LFB20:
	mov	x1, x0
	mov	w8, 777
	movi	v31.4s, 0
	mov	w7, 5
	add	x5, x0, 43
	movi	v30.4s, 0
	str	xzr, [x1], 8
	add	x4, x0, 75
	add	x3, x0, 107
	stp	xzr, xzr, [x0, 8]
	mov	w6, 3
	stp	xzr, xzr, [x1, 16]
	strh	w8, [x0, 40]
	strb	w7, [x0, 42]
	stp	xzr, xzr, [x5]
	stp	xzr, xzr, [x5, 16]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	str	q31, [x0, 155]
	str	q31, [x0, 139]
	str	q31, [x0, 169]
	strb	w6, [x0, 185]
	str	xzr, [x0, 192]
	stp	xzr, xzr, [x0, 200]
	stp	xzr, xzr, [x0, 216]
	str	q30, [x0, 232]
	str	q30, [x0, 248]
	ret
LFE20:
	.const
	.align	3
_payout_resultG.1:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	1
	.byte	0
	.byte	2
	.space 7
	.align	3
_submit_resultG.5:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	2
	.byte	0
	.byte	1
	.byte	3
	.byte	0
	.space 5
_report_statusG.9:
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	7
	.byte	0
	.byte	4
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	5
	.byte	2
	.byte	0
	.align	1
_report_statusT2.10:
	.byte	6
	.byte	10
	.align	1
_report_statusT1.11:
	.byte	8
	.byte	10
	.align	3
_report_statusP.12:
	.word	1
	.word	2
_bug_categoryG.13:
	.byte	0
	.byte	0
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	1
	.byte	0
	.byte	6
	.byte	0
	.byte	0
	.byte	3
	.byte	6
	.byte	0
	.byte	0
	.byte	8
	.byte	6
	.byte	0
	.byte	1
	.align	1
_bug_categoryT2.14:
	.byte	21
	.byte	10
	.align	1
_bug_categoryT1.15:
	.byte	18
	.byte	19
	.align	3
_bug_categoryP.16:
	.word	1
	.word	4
	.align	3
_severity_levelG.17:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	1
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.space 3
	.globl _anubis_bug_bounty__payout_resultN
	.align	3
_anubis_bug_bounty__payout_resultN:
	.byte	1
	.byte	5
	.byte	18
	.byte	30
	.byte	47
	.space 3
	.globl _anubis_bug_bounty__payout_resultS
	.align	3
_anubis_bug_bounty__payout_resultS:
	.ascii "PAIDNOT_CONFIRMEDALREADY_PAIDINSUFFICIENT_POOL"
	.globl _anubis_bug_bounty__assess_resultN
	.align	2
_anubis_bug_bounty__assess_resultN:
	.byte	1
	.byte	9
	.byte	23
	.byte	39
	.globl _anubis_bug_bounty__assess_resultS
	.align	3
_anubis_bug_bounty__assess_resultS:
	.ascii "ASSESSEDINVALID_REPORTALREADY_ASSESSED"
	.globl _anubis_bug_bounty__submit_resultN
	.align	3
_anubis_bug_bounty__submit_resultN:
	.byte	1
	.byte	10
	.byte	26
	.byte	42
	.byte	56
	.byte	71
	.space 2
	.globl _anubis_bug_bounty__submit_resultS
	.align	3
_anubis_bug_bounty__submit_resultS:
	.ascii "SUBMITTEDINVALID_CATEGORYDUPLICATE_REPORTPOOL_EXHAUSTEDREPORTER_BANNED"
	.globl _anubis_bug_bounty__report_statusN
	.align	3
_anubis_bug_bounty__report_statusN:
	.byte	1
	.byte	10
	.byte	18
	.byte	27
	.byte	32
	.byte	40
	.byte	48
	.byte	52
	.byte	61
	.space 7
	.globl _anubis_bug_bounty__report_statusS
	.align	3
_anubis_bug_bounty__report_statusS:
	.ascii "SUBMITTEDTRIAGINGCONFIRMEDFIXEDDISPUTEDREJECTEDPAIDDUPLICATE"
	.globl _anubis_bug_bounty__category_severity
	.align	3
_anubis_bug_bounty__category_severity:
	.byte	0
	.byte	0
	.byte	1
	.byte	1
	.byte	2
	.byte	2
	.byte	0
	.byte	0
	.byte	1
	.byte	3
	.byte	3
	.space 5
	.globl _anubis_bug_bounty__bug_categoryN
	.align	1
_anubis_bug_bounty__bug_categoryN:
	.hword	1
	.hword	16
	.hword	26
	.hword	42
	.hword	59
	.hword	71
	.hword	87
	.hword	103
	.hword	121
	.hword	135
	.hword	144
	.hword	152
	.globl _anubis_bug_bounty__bug_categoryS
	.align	3
_anubis_bug_bounty__bug_categoryS:
	.ascii "CONSENSUS_BREAKFUND_THEFTSTATE_CORRUPTIONDENIAL_OF_SERVICEPRIVACY_LEAKGAS_MANIPULATIONSIGNATURE_BYPASSCRYPTOGRAPHIC_FLAWACCESS_CONTROLMINOR_BUGUX_ISSUE"
	.globl _anubis_bug_bounty__severity_rewards
	.align	3
_anubis_bug_bounty__severity_rewards:
	.xword	500000
	.xword	5000000
	.xword	100000
	.xword	500000
	.xword	25000
	.xword	100000
	.xword	5000
	.xword	25000
	.globl _anubis_bug_bounty__severity_levelN
	.align	3
_anubis_bug_bounty__severity_levelN:
	.byte	1
	.byte	9
	.byte	13
	.byte	19
	.byte	22
	.space 3
	.globl _anubis_bug_bounty__severity_levelS
	.align	3
_anubis_bug_bounty__severity_levelS:
	.ascii "CRITICALHIGHMEDIUMLOW"
	.data
	.align	3
_anubis_bug_bounty__report_counter:
	.space 8
	.globl _anubis_bug_bounty_E
	.align	1
_anubis_bug_bounty_E:
	.space 2
	.literal16
	.align	4
lC2:
	.xword	50000000
	.xword	50000000
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
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$24,LCFI1-LCFI0
	.long L$set$24
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$25,LCFI2-LCFI1
	.long L$set$25
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$26,LCFI3-LCFI2
	.long L$set$26
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x99
	.uleb128 0x2
	.byte	0x4
	.set L$set$27,LCFI4-LCFI3
	.long L$set$27
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
	.set L$set$28,LCFI5-LCFI4
	.long L$set$28
	.byte	0xb
	.byte	0x4
	.set L$set$29,LCFI6-LCFI5
	.long L$set$29
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
	.quad	LFB14-.
	.set L$set$32,LFE14-LFB14
	.quad L$set$32
	.uleb128 0
	.byte	0x4
	.set L$set$33,LCFI8-LFB14
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
	.byte	0x4
	.set L$set$35,LCFI10-LCFI9
	.long L$set$35
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI11-LCFI10
	.long L$set$36
	.byte	0xb
	.byte	0x4
	.set L$set$37,LCFI12-LCFI11
	.long L$set$37
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$38,LCFI13-LCFI12
	.long L$set$38
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$39,LEFDE25-LASFDE25
	.long L$set$39
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB15-.
	.set L$set$40,LFE15-LFB15
	.quad L$set$40
	.uleb128 0
	.byte	0x4
	.set L$set$41,LCFI14-LFB15
	.long L$set$41
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$42,LCFI15-LCFI14
	.long L$set$42
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$43,LCFI16-LCFI15
	.long L$set$43
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI17-LCFI16
	.long L$set$44
	.byte	0xb
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
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$53,LEFDE31-LASFDE31
	.long L$set$53
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB18-.
	.set L$set$54,LFE18-LFB18
	.quad L$set$54
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI22-LFB18
	.long L$set$55
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$56,LCFI23-LCFI22
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$57,LEFDE33-LASFDE33
	.long L$set$57
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB19-.
	.set L$set$58,LFE19-LFB19
	.quad L$set$58
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$59,LEFDE35-LASFDE35
	.long L$set$59
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB20-.
	.set L$set$60,LFE20-LFB20
	.quad L$set$60
	.uleb128 0
	.align	3
LEFDE35:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
