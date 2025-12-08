	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__cert_levelH
_anubis_certification__cert_levelH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L3
	ldrb	w2, [x0]
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
L2:
	adrp	x2, _cert_levelG.22@PAGE
	mov	w1, 52429
	add	x2, x2, _cert_levelG.22@PAGEOFF;
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
L3:
	mov	x3, 0
	mov	x0, 0
	b	L2
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__verification_requirementH
_anubis_certification__verification_requirementH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L7
	ldrb	w2, [x0]
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
L6:
	adrp	x2, _verification_requirementG.18@PAGE
	mov	w1, 52429
	add	x2, x2, _verification_requirementG.18@PAGEOFF;
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
L7:
	mov	x3, 0
	mov	x0, 0
	b	L6
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__certification_statusH
_anubis_certification__certification_statusH:
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
	adrp	x3, _certification_statusG.14@PAGE
	mov	w1, 52429
	add	x3, x3, _certification_statusG.14@PAGEOFF;
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
	.globl _anubis_certification__contract_certificationIP
_anubis_certification__contract_certificationIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__certification_arrayIP
_anubis_certification__certification_arrayIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__Tlevel_count_arrayBIP
_anubis_certification__Tlevel_count_arrayBIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__certification_stateIP
_anubis_certification__certification_stateIP:
LFB107:
	ret
LFE107:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__apply_resultH
_anubis_certification__apply_resultH:
LFB9:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _apply_resultP.13@PAGE
	add	w5, w10, 1
	add	x9, x9, _apply_resultP.13@PAGEOFF;
	adrp	x12, _apply_resultT1.12@PAGE
	adrp	x11, _apply_resultT2.11@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _apply_resultT1.12@PAGEOFF;
	add	x11, x11, _apply_resultT2.11@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 11
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L17
L21:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	add	w6, w2, w2, lsl 2
	add	w6, w2, w6, lsl 1
	add	w2, w5, w5, lsl 2
	sub	w6, w3, w6
	add	w2, w5, w2, lsl 1
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L17
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L21
L17:
	adrp	x3, _apply_resultG.10@PAGE
	mov	w1, 52429
	add	x3, x3, _apply_resultG.10@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__verify_resultH
_anubis_certification__verify_resultH:
LFB10:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L25
	sub	w1, w1, w2
	cmp	w1, 5
	bgt	L26
L25:
	mov	x2, 0
	mov	x0, 0
L23:
	adrp	x3, _verify_resultG.6@PAGE
	mov	w1, 52429
	add	x3, x3, _verify_resultG.6@PAGEOFF;
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
L26:
	ldrb	w0, [x0, 6]
	mov	w2, 43691
	movk	w2, 0xaaaa, lsl 16
	lsl	w1, w0, 3
	sub	w3, w1, w0
	add	w1, w1, w0
	umull	x0, w3, w2
	umull	x2, w1, w2
	lsr	x0, x0, 35
	lsr	x2, x2, 35
	add	w0, w0, w0, lsl 1
	add	w2, w2, w2, lsl 1
	sub	w0, w3, w0, lsl 2
	sub	w2, w1, w2, lsl 2
	sxtw	x0, w0
	sxtw	x2, w2
	b	L23
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__upgrade_resultH
_anubis_certification__upgrade_resultH:
LFB11:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L30
	sub	w1, w1, w2
	cmp	w1, 1
	ble	L30
	ldrb	w2, [x0, 2]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w0, w2, w2, lsl 1
	add	w2, w2, w2, lsl 3
	umull	x3, w0, w1
	umull	x1, w2, w1
	lsr	x3, x3, 35
	lsr	x1, x1, 35
	add	w5, w3, w3, lsl 2
	add	w4, w1, w1, lsl 2
	add	w3, w3, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w0, w0, w3
	sub	w2, w2, w1
	adrp	x1, _upgrade_resultG.2@PAGE
	sxtw	x2, w2
	add	x1, x1, _upgrade_resultG.2@PAGEOFF;
	sxtw	x0, w0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L30:
	adrp	x1, _upgrade_resultG.2@PAGE
	mov	x2, 0
	add	x1, x1, _upgrade_resultG.2@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__init_certification_state
_anubis_certification__init_certification_state:
LFB12:
	stp	xzr, xzr, [x0]
	str	xzr, [x0, 16]
	stp	xzr, xzr, [x0, 24]
	str	wzr, [x0, 40]
	stp	x1, x1, [x0, 48]
	ret
LFE12:
	.const
	.align	3
lC2:
	.ascii "anubis_certification.ads"
	.space 1
	.align	3
lC3:
	.ascii "failed precondition from anubis_certification.ads:172"
	.align	3
lC4:
	.ascii "anubis_certification.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__apply_certification
_anubis_certification__apply_certification:
LFB13:
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	x29, sp
LCFI1:
	ldp	w8, w15, [x6]
	ldp	w6, w12, [x2]
	cmp	w3, 4
	bhi	L56
	sxtw	x13, w6
	mov	x14, x0
	sxtw	x10, w12
	add	x0, x13, 31
	sxtw	x2, w8
	cmp	x0, x10
	sxtw	x11, w15
	add	x0, x2, 31
	ccmp	x0, x11, 0, eq
	ccmp	w3, 0, 4, eq
	beq	L57
	adrp	x0, _anubis_certification__level_deposits@PAGE
	uxtw	x9, w3
	add	x0, x0, _anubis_certification__level_deposits@PAGEOFF;
	ldr	x0, [x0, x9, lsl 3]
	cmp	x0, x4
	bhi	L58
	stp	xzr, xzr, [x7]
	stp	xzr, xzr, [x7, 16]
	cmp	w6, w12
	bgt	L37
	sub	x12, x1, x13
	sub	x1, x13, #1
	sub	x13, x7, x13
	.p2align 5,,15
L43:
	add	x1, x1, 1
	subs	w9, w1, w6
	bvs	L39
	cmp	w9, 31
	bgt	L41
	bhi	L59
	ldrb	w9, [x12, x1]
	strb	w9, [x13, x1]
L41:
	cmp	x10, x1
	bne	L43
L37:
	mov	w1, 1
	add	x0, x7, 34
	strb	w3, [x7, 32]
	strb	w1, [x7, 33]
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w8, w15
	bgt	L44
	sub	x3, x7, x2
	sub	x5, x5, x2
	add	x3, x3, 34
	sub	x1, x2, #1
	.p2align 5,,15
L50:
	add	x1, x1, 1
	subs	w2, w1, w8
	bvs	L46
	cmp	w2, 31
	bgt	L48
	bhi	L60
	ldrb	w2, [x5, x1]
	strb	w2, [x3, x1]
L48:
	cmp	x11, x1
	bne	L50
L44:
	ldr	x1, [x14, 8]
	add	x5, x7, 76
	add	x3, x7, 108
	add	x2, x7, 140
	mov	w8, 1
	ldr	x6, [x14, 56]
	mov	w0, 0
	str	xzr, [x7, 68]
	stp	xzr, xzr, [x5]
	add	x1, x1, x4
	stp	xzr, xzr, [x5, 16]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	str	x4, [x7, 176]
	strb	w8, [x7, 184]
	stp	x6, xzr, [x7, 192]
	stp	xzr, xzr, [x7, 208]
	stp	xzr, xzr, [x7, 224]
	str	x1, [x14, 8]
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
	.p2align 2,,3
L58:
LCFI3:
	movi	v31.4s, 0
	mov	w0, 2
	stp	q31, q31, [x7]
	stp	q31, q31, [x7, 32]
	stp	q31, q31, [x7, 64]
	stp	q31, q31, [x7, 96]
	stp	q31, q31, [x7, 128]
	stp	q31, q31, [x7, 160]
	stp	q31, q31, [x7, 192]
	str	q31, [x7, 224]
	ldp	x29, x30, [sp], 16
LCFI4:
	ret
L39:
LCFI5:
	adrp	x0, lC4@PAGE
	mov	w1, 77
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L46:
	adrp	x0, lC4@PAGE
	mov	w1, 89
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L59:
	adrp	x0, lC4@PAGE
	mov	w1, 78
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L60:
	adrp	x0, lC4@PAGE
	mov	w1, 90
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L56:
	adrp	x0, lC2@PAGE
	mov	w1, 174
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L57:
	adrp	x0, lC3@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC3@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE13:
	.const
	.align	2
lC0:
	.word	1
	.word	53
	.text
	.const
	.align	3
lC5:
	.ascii "failed precondition from anubis_certification.ads:189"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__verify_certification
_anubis_certification__verify_certification:
LFB14:
	stp	x29, x30, [sp, -16]!
LCFI6:
	mov	x29, sp
LCFI7:
	mov	x1, x0
	ldpsw	x3, x7, [x2]
	ldpsw	x0, x2, [x4]
	add	x3, x3, 31
	cmp	x3, x7
	ldp	x8, x3, [x29, 16]
	add	x0, x0, 31
	ccmp	x0, x2, 0, eq
	bne	L101
	ldrb	w0, [x1, 33]
	cmp	w0, 4
	bhi	L102
	cmp	w0, 1
	beq	L103
	mov	w0, 1
L64:
	ldp	x29, x30, [sp], 16
LCFI8:
	ret
	.p2align 2,,3
L103:
LCFI9:
	ldrb	w2, [x1, 32]
	cmp	w2, 4
	bhi	L104
	cmp	w2, 2
	beq	L66
	bhi	L67
	cbz	w2, L64
L68:
	ldr	x2, [x1, 192]
	mov	w3, 2
	mov	w0, 0
	strb	w3, [x1, 33]
	add	x3, x2, 5255168
	str	x2, [x1, 216]
	add	x3, x3, 832
	stp	x2, x3, [x1, 200]
	ldp	x29, x30, [sp], 16
LCFI10:
	ret
	.p2align 2,,3
L67:
LCFI11:
	cmp	w2, 3
	bne	L105
	ldp	w3, w0, [x1, 68]
	ldr	w2, [x6]
	cmp	w3, w0
	beq	L106
L72:
	mov	w0, 2
L109:
	ldp	x29, x30, [sp], 16
LCFI12:
	ret
	.p2align 2,,3
L105:
LCFI13:
	ldp	w4, w0, [x1, 68]
	ldr	w2, [x3]
	cmp	w4, w0
	bne	L72
	ldr	w4, [x3, 4]
	cmp	w2, w4
	bgt	L88
	sxtw	x6, w2
	sxtw	x9, w4
	sub	x8, x8, x6
	sub	x0, x6, #1
	add	x7, x8, 1
	mov	x3, x0
	b	L81
	.p2align 2,,3
L98:
	add	x3, x3, 1
	cmp	x9, x3
	beq	L88
L81:
	ldrb	w5, [x7, x3]
	cbz	w5, L98
	sub	x5, x1, x6
	sxtw	x4, w4
	add	x5, x5, 108
	b	L80
	.p2align 2,,3
L108:
	cmp	w3, 31
	bgt	L85
	bhi	L107
	ldrb	w3, [x8, x0]
	strb	w3, [x5, x0]
L85:
	cmp	x4, x0
	beq	L68
L80:
	add	x0, x0, 1
	subs	w3, w0, w2
	bvc	L108
	adrp	x0, lC4@PAGE
	mov	w1, 191
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L66:
	ldp	w2, w0, [x1, 68]
	cmp	w2, w0
	beq	L68
	mov	w0, 2
	b	L109
	.p2align 2,,3
L106:
	ldr	w4, [x6, 4]
	cmp	w2, w4
	bgt	L68
	sxtw	x0, w2
	sxtw	x4, w4
	sub	x6, x1, x0
	sub	x5, x5, x0
	add	x6, x6, 76
	sub	x0, x0, #1
	b	L79
	.p2align 2,,3
L111:
	cmp	w3, 31
	bgt	L77
	bhi	L110
	ldrb	w3, [x5, x0]
	strb	w3, [x6, x0]
L77:
	cmp	x4, x0
	beq	L68
L79:
	add	x0, x0, 1
	subs	w3, w0, w2
	bvc	L111
	adrp	x0, lC4@PAGE
	mov	w1, 162
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L88:
	mov	w0, 3
	b	L64
L101:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L102:
	adrp	x0, lC4@PAGE
	mov	w1, 131
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L107:
	adrp	x0, lC4@PAGE
	mov	w1, 192
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L104:
	adrp	x0, lC4@PAGE
	mov	w1, 137
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L110:
	adrp	x0, lC4@PAGE
	mov	w1, 163
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__check_vc_coverage
_anubis_certification__check_vc_coverage:
LFB15:
	cmp	w0, w1
	cset	w0, eq
	ret
LFE15:
	.const
	.align	3
lC6:
	.ascii "failed precondition from anubis_certification.ads:211"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__upgrade_level
_anubis_certification__upgrade_level:
LFB17:
	stp	x29, x30, [sp, -16]!
LCFI14:
	mov	x29, sp
LCFI15:
	mov	x5, x0
	ldp	w3, w0, [x4]
	cmp	w1, 4
	bhi	L115
	ldrb	w4, [x5, 32]
	cmp	w4, 4
	bhi	L115
	sxtw	x3, w3
	add	x3, x3, 31
	cmp	x3, w0, sxtw
	ccmp	w1, w4, 0, eq
	bls	L120
	adrp	x3, _anubis_certification__level_deposits@PAGE
	uxtw	x6, w1
	add	x3, x3, _anubis_certification__level_deposits@PAGEOFF;
	mov	w0, 2
	ldr	x7, [x3, x4, lsl 3]
	ldr	x3, [x3, x6, lsl 3]
	sub	x3, x3, x7
	cmp	x3, x2
	bhi	L117
	ldr	x3, [x5, 176]
	mov	w0, 0
	strb	w1, [x5, 32]
	add	x1, x3, x2
	str	x1, [x5, 176]
L117:
	ldp	x29, x30, [sp], 16
LCFI16:
	ret
L115:
LCFI17:
	adrp	x0, lC2@PAGE
	mov	w1, 212
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L120:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE17:
	.const
	.align	3
lC7:
	.ascii "failed precondition from anubis_certification.ads:220"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__downgrade_level
_anubis_certification__downgrade_level:
LFB18:
	stp	x29, x30, [sp, -16]!
LCFI18:
	mov	x29, sp
LCFI19:
	cmp	w1, 4
	bhi	L123
	mov	x2, x0
	ldrb	w0, [x0, 32]
	cmp	w0, 4
	bhi	L123
	cmp	w1, w0
	bcs	L126
	adrp	x3, _anubis_certification__level_deposits@PAGE
	uxtw	x5, w1
	strb	w1, [x2, 32]
	add	x3, x3, _anubis_certification__level_deposits@PAGEOFF;
	ldr	x4, [x2, 176]
	ldr	x1, [x3, x0, lsl 3]
	ldr	x0, [x3, x5, lsl 3]
	sub	x3, x0, x1
	sub	x0, x1, x0
	add	x4, x4, x3
	str	x4, [x2, 176]
	ldp	x29, x30, [sp], 16
LCFI20:
	ret
L123:
LCFI21:
	adrp	x0, lC2@PAGE
	mov	w1, 220
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L126:
	adrp	x0, lC7@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE18:
	.const
	.align	3
lC8:
	.ascii "failed precondition from anubis_certification.ads:230"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__revoke_certification
_anubis_certification__revoke_certification:
LFB19:
	stp	x29, x30, [sp, -16]!
LCFI22:
	mov	x2, x0
	mov	x29, sp
LCFI23:
	ldpsw	x5, x0, [x3]
	add	x5, x5, 31
	cmp	x5, x0
	bne	L140
	cmp	w4, 1
	bhi	L141
	ldr	x3, [x1, 176]
	mov	x0, 0
	cbz	w4, L130
	lsr	x0, x3, 1
	sub	x3, x3, x0
	str	x3, [x1, 176]
L130:
	ldr	x4, [x2, 8]
	cmp	x4, x3
	bcs	L142
L131:
	ldrb	w3, [x1, 32]
	cmp	w3, 4
	bhi	L143
	ubfiz	x3, x3, 2, 8
	add	x3, x3, 16
	add	x2, x2, x3
	ldr	w3, [x2, 8]
	cbz	w3, L133
	sub	w3, w3, #1
	str	w3, [x2, 8]
L133:
	mov	w2, 4
	strb	wzr, [x1, 184]
	strb	w2, [x1, 33]
	ldp	x29, x30, [sp], 16
LCFI24:
	ret
	.p2align 2,,3
L142:
LCFI25:
	sub	x4, x4, x3
	str	x4, [x2, 8]
	b	L131
L143:
	adrp	x0, lC4@PAGE
	mov	w1, 281
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L141:
	adrp	x0, lC4@PAGE
	mov	w1, 268
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L140:
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE19:
	.const
	.align	3
lC9:
	.ascii "failed postcondition from anubis_certification.ads:240"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__get_discount_rate
_anubis_certification__get_discount_rate:
LFB20:
	stp	x29, x30, [sp, -16]!
LCFI26:
	mov	x29, sp
LCFI27:
	cmp	w0, 4
	bhi	L146
	adrp	x1, _anubis_certification__level_discounts@PAGE
	uxtw	x0, w0
	add	x1, x1, _anubis_certification__level_discounts@PAGEOFF;
	ldr	w0, [x1, x0, lsl 2]
	tbnz	w0, #31, L146
	cmp	w0, 3000
	bgt	L149
	ldp	x29, x30, [sp], 16
LCFI28:
	ret
L146:
LCFI29:
	adrp	x0, lC4@PAGE
	mov	w1, 298
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L149:
	adrp	x0, lC9@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE20:
	.const
	.align	2
lC1:
	.word	1
	.word	54
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__calculate_discounted_gas
_anubis_certification__calculate_discounted_gas:
LFB22:
	cmp	w1, 4
	bhi	L152
	adrp	x2, _anubis_certification__level_discounts@PAGE
	uxtw	x1, w1
	add	x2, x2, _anubis_certification__level_discounts@PAGEOFF;
	ldr	w1, [x2, x1, lsl 2]
	tbnz	w1, #31, L152
	sxtw	x1, w1
	mov	x2, 22859
	movk	x2, 0x3886, lsl 16
	mul	x1, x1, x0
	movk	x2, 0xc5d6, lsl 32
	movk	x2, 0x346d, lsl 48
	umulh	x1, x1, x2
	sub	x0, x0, x1, lsr 11
	ret
L152:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI30:
	mov	w1, 305
	mov	x29, sp
LCFI31:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__apply_gas_discount
_anubis_certification__apply_gas_discount:
LFB23:
	mov	x2, x0
	stp	x29, x30, [sp, -16]!
LCFI32:
	mov	x0, x1
	mov	x29, sp
LCFI33:
	ldrb	w1, [x2, 33]
	cmp	w1, 4
	bhi	L161
	cmp	w1, 2
	beq	L162
	ldp	x29, x30, [sp], 16
LCFI34:
	ret
	.p2align 2,,3
L162:
LCFI35:
	ldrb	w1, [x2, 32]
	cmp	w1, 4
	bhi	L163
	adrp	x3, _anubis_certification__level_discounts@PAGE
	add	x3, x3, _anubis_certification__level_discounts@PAGEOFF;
	ldr	w1, [x3, x1, lsl 2]
	tbnz	w1, #31, L164
	sxtw	x1, w1
	mov	x3, 22859
	ldr	q31, [x2, 224]
	movk	x3, 0x3886, lsl 16
	adrp	x4, lC10@PAGE
	mul	x1, x1, x0
	movk	x3, 0xc5d6, lsl 32
	movk	x3, 0x346d, lsl 48
	ldr	q30, [x4, #lC10@PAGEOFF]
	umulh	x1, x1, x3
	lsr	x1, x1, 11
	sub	x0, x0, x1
	ins	v30.d[0], x1
	add	v30.2d, v30.2d, v31.2d
	str	q30, [x2, 224]
	ldp	x29, x30, [sp], 16
LCFI36:
	ret
L161:
LCFI37:
	adrp	x0, lC4@PAGE
	mov	w1, 322
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L163:
	adrp	x0, lC4@PAGE
	mov	w1, 328
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L164:
	adrp	x0, lC4@PAGE
	mov	w1, 305
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.const
	.align	3
lC11:
	.ascii "failed precondition from anubis_certification.ads:266"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__renew_certification
_anubis_certification__renew_certification:
LFB24:
	stp	x29, x30, [sp, -16]!
LCFI38:
	mov	x7, x0
	mov	x29, sp
LCFI39:
	ldp	w4, w0, [x2]
	sxtw	x2, w4
	sxtw	x5, w0
	add	x6, x2, 31
	cmp	x6, x5
	bne	L180
	ldrb	w6, [x7, 33]
	cmp	w6, 4
	bhi	L181
	sub	w6, w6, #2
	and	w6, w6, 255
	cmp	w6, 1
	bhi	L176
	cmp	w4, w0
	bgt	L169
	sub	x0, x7, x2
	sub	x6, x1, x2
	add	x0, x0, 34
	sub	x1, x2, #1
	.p2align 5,,15
L175:
	add	x1, x1, 1
	subs	w2, w1, w4
	bvs	L171
	cmp	w2, 31
	bgt	L173
	bhi	L182
	ldrb	w2, [x6, x1]
	strb	w2, [x0, x1]
L173:
	cmp	x5, x1
	bne	L175
L169:
	add	x0, x3, 5255168
	mov	w1, 2
	add	x0, x0, 832
	strb	w1, [x7, 33]
	stp	x0, x3, [x7, 208]
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI40:
	ret
	.p2align 2,,3
L176:
LCFI41:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI42:
	ret
L171:
LCFI43:
	adrp	x0, lC4@PAGE
	mov	w1, 356
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L182:
	adrp	x0, lC4@PAGE
	mov	w1, 357
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L181:
	adrp	x0, lC4@PAGE
	mov	w1, 349
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L180:
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE24:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__is_expired
_anubis_certification__is_expired:
LFB25:
	ldr	x0, [x0, 208]
	cmp	x0, x1
	cset	w0, ls
	ret
LFE25:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__get_level
_anubis_certification__get_level:
LFB26:
	ldrb	w0, [x0, 32]
	cmp	w0, 4
	bhi	L189
	ret
L189:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI44:
	mov	w1, 384
	mov	x29, sp
LCFI45:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__get_deposit
_anubis_certification__get_deposit:
LFB27:
	cmp	w0, 4
	bhi	L195
	adrp	x1, _anubis_certification__level_deposits@PAGE
	uxtw	x0, w0
	add	x1, x1, _anubis_certification__level_deposits@PAGEOFF;
	ldr	x0, [x1, x0, lsl 3]
	ret
L195:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI46:
	mov	w1, 389
	mov	x29, sp
LCFI47:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE27:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__get_total_certified
_anubis_certification__get_total_certified:
LFB28:
	ldr	x0, [x0]
	ret
LFE28:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__zeroize_certification_state
_anubis_certification__zeroize_certification_state:
LFB29:
	movi	v31.4s, 0
	str	xzr, [x0, 16]
	stp	xzr, xzr, [x0, 24]
	str	q31, [x0]
	str	wzr, [x0, 40]
	str	q31, [x0, 48]
	ret
LFE29:
	.align	2
	.p2align 5,,15
	.globl _anubis_certification__zeroize_contract_cert
_anubis_certification__zeroize_contract_cert:
LFB30:
	add	x4, x0, 34
	stp	xzr, xzr, [x0]
	add	x3, x0, 76
	movi	v31.4s, 0
	add	x2, x0, 108
	add	x1, x0, 140
	stp	xzr, xzr, [x0, 16]
	strh	wzr, [x0, 32]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	str	xzr, [x0, 68]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	str	xzr, [x0, 176]
	strb	wzr, [x0, 184]
	stp	q31, q31, [x0, 192]
	str	q31, [x0, 224]
	ret
LFE30:
	.const
	.align	3
_upgrade_resultG.2:
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.space 5
	.align	3
_verify_resultG.6:
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	3
	.byte	0
	.space 4
	.align	3
_apply_resultG.10:
	.byte	3
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.space 5
	.align	1
_apply_resultT2.11:
	.byte	2
	.byte	4
	.align	1
_apply_resultT1.12:
	.byte	3
	.byte	5
	.align	3
_apply_resultP.13:
	.word	3
	.word	9
	.align	3
_certification_statusG.14:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	3
	.space 5
	.align	3
_verification_requirementG.18:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	4
	.byte	0
	.byte	0
	.space 1
	.align	3
_cert_levelG.22:
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	1
	.byte	0
	.byte	0
	.space 1
	.globl _anubis_certification__upgrade_resultN
	.align	3
_anubis_certification__upgrade_resultN:
	.byte	1
	.byte	9
	.byte	29
	.byte	49
	.byte	69
	.space 3
	.globl _anubis_certification__upgrade_resultS
	.align	3
_anubis_certification__upgrade_resultS:
	.ascii "UPGRADEDREQUIREMENTS_NOT_METINSUFFICIENT_DEPOSITINVALID_UPGRADE_PATH"
	.globl _anubis_certification__verify_resultN
	.align	3
_anubis_certification__verify_resultN:
	.byte	1
	.byte	9
	.byte	28
	.byte	42
	.byte	56
	.byte	63
	.space 2
	.globl _anubis_certification__verify_resultS
	.align	3
_anubis_certification__verify_resultS:
	.ascii "VERIFIEDVERIFICATION_FAILEDMISSING_PROOFSAUDIT_REQUIREDEXPIRED"
	.globl _anubis_certification__apply_resultN
	.align	3
_anubis_certification__apply_resultN:
	.byte	1
	.byte	8
	.byte	25
	.byte	45
	.byte	61
	.byte	74
	.space 2
	.globl _anubis_certification__apply_resultS
	.align	3
_anubis_certification__apply_resultS:
	.ascii "APPLIEDALREADY_CERTIFIEDINSUFFICIENT_DEPOSITINVALID_CONTRACTINVALID_LEVEL"
	.globl _anubis_certification__certification_statusN
	.align	3
_anubis_certification__certification_statusN:
	.byte	1
	.byte	12
	.byte	26
	.byte	32
	.byte	41
	.byte	48
	.space 2
	.globl _anubis_certification__certification_statusS
	.align	3
_anubis_certification__certification_statusS:
	.ascii "NOT_APPLIEDPENDING_REVIEWACTIVESUSPENDEDREVOKED"
	.globl _anubis_certification__level_requirements
	.align	3
_anubis_certification__level_requirements:
	.byte	0
	.byte	1
	.byte	2
	.byte	3
	.byte	4
	.space 3
	.globl _anubis_certification__verification_requirementN
	.align	3
_anubis_certification__verification_requirementN:
	.byte	1
	.byte	14
	.byte	32
	.byte	46
	.byte	68
	.byte	87
	.space 2
	.globl _anubis_certification__verification_requirementS
	.align	3
_anubis_certification__verification_requirementS:
	.ascii "NONE_REQUIREDSPARK_MODE_ENABLEDALL_VCS_PROVENCONSTANT_TIME_VERIFIEDTHIRD_PARTY_AUDITED"
	.globl _anubis_certification__level_deposits
	.align	3
_anubis_certification__level_deposits:
	.xword	0
	.xword	1000
	.xword	10000
	.xword	50000
	.xword	100000
	.globl _anubis_certification__level_discounts
	.align	2
_anubis_certification__level_discounts:
	.word	0
	.word	0
	.word	1000
	.word	2000
	.word	3000
	.globl _anubis_certification__cert_levelN
	.align	3
_anubis_certification__cert_levelN:
	.byte	1
	.byte	5
	.byte	11
	.byte	17
	.byte	21
	.byte	29
	.space 2
	.globl _anubis_certification__cert_levelS
	.align	3
_anubis_certification__cert_levelS:
	.ascii "NONEBRONZESILVERGOLDPLATINUM"
	.globl _anubis_certification_E
	.data
	.align	1
_anubis_certification_E:
	.space 2
	.literal16
	.align	4
lC10:
	.xword	1
	.xword	1
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
	.quad	LFB107-.
	.set L$set$14,LFE107-LFB107
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
	.quad	LFB12-.
	.set L$set$22,LFE12-LFB12
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
	.byte	0x4
	.set L$set$25,LCFI0-LFB13
	.long L$set$25
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$26,LCFI1-LCFI0
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$27,LCFI2-LCFI1
	.long L$set$27
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$28,LCFI3-LCFI2
	.long L$set$28
	.byte	0xb
	.byte	0x4
	.set L$set$29,LCFI4-LCFI3
	.long L$set$29
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$30,LCFI5-LCFI4
	.long L$set$30
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$31,LEFDE25-LASFDE25
	.long L$set$31
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB14-.
	.set L$set$32,LFE14-LFB14
	.quad L$set$32
	.uleb128 0
	.byte	0x4
	.set L$set$33,LCFI6-LFB14
	.long L$set$33
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$34,LCFI7-LCFI6
	.long L$set$34
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$35,LCFI8-LCFI7
	.long L$set$35
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI9-LCFI8
	.long L$set$36
	.byte	0xb
	.byte	0x4
	.set L$set$37,LCFI10-LCFI9
	.long L$set$37
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$38,LCFI11-LCFI10
	.long L$set$38
	.byte	0xb
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
LSFDE27:
	.set L$set$41,LEFDE27-LASFDE27
	.long L$set$41
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB15-.
	.set L$set$42,LFE15-LFB15
	.quad L$set$42
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$43,LEFDE29-LASFDE29
	.long L$set$43
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB17-.
	.set L$set$44,LFE17-LFB17
	.quad L$set$44
	.uleb128 0
	.byte	0x4
	.set L$set$45,LCFI14-LFB17
	.long L$set$45
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$46,LCFI15-LCFI14
	.long L$set$46
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$47,LCFI16-LCFI15
	.long L$set$47
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$48,LCFI17-LCFI16
	.long L$set$48
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$49,LEFDE31-LASFDE31
	.long L$set$49
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB18-.
	.set L$set$50,LFE18-LFB18
	.quad L$set$50
	.uleb128 0
	.byte	0x4
	.set L$set$51,LCFI18-LFB18
	.long L$set$51
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$52,LCFI19-LCFI18
	.long L$set$52
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$53,LCFI20-LCFI19
	.long L$set$53
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$54,LCFI21-LCFI20
	.long L$set$54
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$55,LEFDE33-LASFDE33
	.long L$set$55
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB19-.
	.set L$set$56,LFE19-LFB19
	.quad L$set$56
	.uleb128 0
	.byte	0x4
	.set L$set$57,LCFI22-LFB19
	.long L$set$57
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$58,LCFI23-LCFI22
	.long L$set$58
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$59,LCFI24-LCFI23
	.long L$set$59
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$60,LCFI25-LCFI24
	.long L$set$60
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$61,LEFDE35-LASFDE35
	.long L$set$61
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB20-.
	.set L$set$62,LFE20-LFB20
	.quad L$set$62
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI26-LFB20
	.long L$set$63
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$64,LCFI27-LCFI26
	.long L$set$64
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$65,LCFI28-LCFI27
	.long L$set$65
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$66,LCFI29-LCFI28
	.long L$set$66
	.byte	0xb
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$67,LEFDE37-LASFDE37
	.long L$set$67
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB22-.
	.set L$set$68,LFE22-LFB22
	.quad L$set$68
	.uleb128 0
	.byte	0x4
	.set L$set$69,LCFI30-LFB22
	.long L$set$69
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$70,LCFI31-LCFI30
	.long L$set$70
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$71,LEFDE39-LASFDE39
	.long L$set$71
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB23-.
	.set L$set$72,LFE23-LFB23
	.quad L$set$72
	.uleb128 0
	.byte	0x4
	.set L$set$73,LCFI32-LFB23
	.long L$set$73
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$74,LCFI33-LCFI32
	.long L$set$74
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$75,LCFI34-LCFI33
	.long L$set$75
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI35-LCFI34
	.long L$set$76
	.byte	0xb
	.byte	0x4
	.set L$set$77,LCFI36-LCFI35
	.long L$set$77
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$78,LCFI37-LCFI36
	.long L$set$78
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$79,LEFDE41-LASFDE41
	.long L$set$79
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB24-.
	.set L$set$80,LFE24-LFB24
	.quad L$set$80
	.uleb128 0
	.byte	0x4
	.set L$set$81,LCFI38-LFB24
	.long L$set$81
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$82,LCFI39-LCFI38
	.long L$set$82
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$83,LCFI40-LCFI39
	.long L$set$83
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$84,LCFI41-LCFI40
	.long L$set$84
	.byte	0xb
	.byte	0x4
	.set L$set$85,LCFI42-LCFI41
	.long L$set$85
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$86,LCFI43-LCFI42
	.long L$set$86
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$87,LEFDE43-LASFDE43
	.long L$set$87
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB25-.
	.set L$set$88,LFE25-LFB25
	.quad L$set$88
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$89,LEFDE45-LASFDE45
	.long L$set$89
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB26-.
	.set L$set$90,LFE26-LFB26
	.quad L$set$90
	.uleb128 0
	.byte	0x4
	.set L$set$91,LCFI44-LFB26
	.long L$set$91
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$92,LCFI45-LCFI44
	.long L$set$92
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$93,LEFDE47-LASFDE47
	.long L$set$93
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB27-.
	.set L$set$94,LFE27-LFB27
	.quad L$set$94
	.uleb128 0
	.byte	0x4
	.set L$set$95,LCFI46-LFB27
	.long L$set$95
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$96,LCFI47-LCFI46
	.long L$set$96
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$97,LEFDE49-LASFDE49
	.long L$set$97
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB28-.
	.set L$set$98,LFE28-LFB28
	.quad L$set$98
	.uleb128 0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$99,LEFDE51-LASFDE51
	.long L$set$99
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB29-.
	.set L$set$100,LFE29-LFB29
	.quad L$set$100
	.uleb128 0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$101,LEFDE53-LASFDE53
	.long L$set$101
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB30-.
	.set L$set$102,LFE30-LFB30
	.quad L$set$102
	.uleb128 0
	.align	3
LEFDE53:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
