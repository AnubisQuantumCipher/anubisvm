	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__certification_levelH
_khepri_registry__certification_levelH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L4
	sub	w1, w1, w2
	cmp	w1, 5
	bgt	L6
L4:
	mov	x3, 0
	mov	x0, 0
L2:
	adrp	x2, _certification_levelG.8@PAGE
	mov	w1, 52429
	add	x2, x2, _certification_levelG.8@PAGEOFF;
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
L6:
	ldrb	w2, [x0, 6]
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
	b	L2
LFE2:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__auditor_attestationIP
_khepri_registry__auditor_attestationIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__Tauditor_arrayBIP
_khepri_registry__Tauditor_arrayBIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__registry_entryIP
_khepri_registry__registry_entryIP:
LFB81:
	ret
LFE81:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__registry_errorH
_khepri_registry__registry_errorH:
LFB6:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _registry_errorP.7@PAGE
	add	w5, w10, 1
	add	x9, x9, _registry_errorP.7@PAGEOFF;
	adrp	x12, _registry_errorT1.6@PAGE
	adrp	x11, _registry_errorT2.5@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _registry_errorT1.6@PAGEOFF;
	add	x11, x11, _registry_errorT2.5@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 21
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L12
L16:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	add	w6, w2, w2, lsl 2
	add	w6, w2, w6, lsl 2
	add	w2, w5, w5, lsl 2
	sub	w6, w3, w6
	add	w2, w5, w2, lsl 2
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L12
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L16
L12:
	adrp	x3, _registry_errorG.4@PAGE
	mov	w1, 52429
	add	x3, x3, _registry_errorG.4@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 35
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1, lsl 1
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__registration_eventIP
_khepri_registry__registration_eventIP:
LFB83:
	ret
LFE83:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__certification_eventIP
_khepri_registry__certification_eventIP:
LFB85:
	ret
LFE85:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__revocation_eventIP
_khepri_registry__revocation_eventIP:
LFB87:
	ret
LFE87:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__audit_eventIP
_khepri_registry__audit_eventIP:
LFB89:
	ret
LFE89:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__Tentry_arrayBIP
_khepri_registry__Tentry_arrayBIP:
LFB11:
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__Tcertifier_arrayBIP
_khepri_registry__Tcertifier_arrayBIP:
LFB12:
	ret
LFE12:
	.const
	.align	3
lC2:
	.ascii "khepri_registry.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__register
_khepri_registry__register:
LFB19:
	adrp	x7, _khepri_registry__entries@PAGE
	ldr	q30, [x0]
	mov	x4, 0
	add	x3, x7, _khepri_registry__entries@PAGEOFF;
	mov	x6, x0
	mov	x11, x1
	mov	x10, x2
	mov	x9, x3
	mov	x8, 856
	mov	x5, 10000
	b	L28
	.p2align 2,,3
L26:
	add	x4, x4, 1
	add	x3, x3, 856
	cmp	x4, x5
	beq	L43
L28:
	ldr	q31, [x3]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L26
	ldr	q31, [x3, 16]
	ldr	q29, [x6, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L26
	madd	x0, x4, x8, x9
	ldr	x0, [x0, 736]
	cbz	x0, L26
	mov	w2, 0
	mov	x0, 0
	bfi	x0, x2, 0, 8
	mov	w1, 1
	bfi	x0, x1, 8, 8
	ret
L43:
	sub	sp, sp, #896
LCFI0:
	add	x4, x7, _khepri_registry__entries@PAGEOFF;
	mov	x3, 0
	mov	x5, 856
	stp	x29, x30, [sp]
LCFI1:
	mov	x29, sp
LCFI2:
	mov	x7, 10000
	stp	x19, x20, [sp, 16]
LCFI3:
	b	L32
	.p2align 2,,3
L29:
	add	x3, x3, 1
	cmp	x3, x7
	beq	L44
L32:
	madd	x19, x3, x5, x4
	ldr	x0, [x19, 736]
	cbnz	x0, L29
	add	x3, x29, 1
	add	x20, x29, 32
	mov	x2, 560
	mov	w1, 0
	stp	xzr, xzr, [x3, 128]
	add	x0, x29, 200
	stp	xzr, xzr, [x3, 144]
	ldp	q31, q30, [x6]
	stp	q31, q30, [x20]
	ldp	q31, q30, [x11]
	str	q31, [x29, 64]
	ldp	q29, q31, [x10]
	stp	q30, q29, [x29, 80]
	str	q31, [x29, 112]
	bl	_memset
	adrp	x0, lC3@PAGE
	add	x3, x29, 1025
	movi	v31.4s, 0
	strb	wzr, [x29, 128]
	mov	x1, x20
	mov	x2, 856
	ldr	q30, [x0, #lC3@PAGEOFF]
	mov	x0, x19
	stp	xzr, xzr, [x3, -240]
	stp	xzr, xzr, [x3, -224]
	add	x3, x29, 1024
	sub	x4, x3, #200
	strb	wzr, [x29, 784]
	stp	q31, q31, [x4]
	stp	q31, q31, [x4, 32]
	str	q31, [x29, 168]
	str	q31, [x29, 184]
	str	wzr, [x29, 760]
	str	q30, [x29, 768]
	bl	_memcpy
	adrp	x1, _khepri_registry__entry_count@PAGE
	ldr	w0, [x1, #_khepri_registry__entry_count@PAGEOFF]
	tbnz	w0, #31, L45
	mov	w2, 2147483647
	cmp	w0, w2
	beq	L34
	add	w0, w0, 1
	str	w0, [x1, #_khepri_registry__entry_count@PAGEOFF]
L34:
	mov	w1, 0
	mov	w2, 1
L27:
	mov	x0, 0
	bfi	x0, x2, 0, 8
	bfi	x0, x1, 8, 8
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 896
LCFI4:
	ret
L44:
LCFI5:
	mov	w1, 9
	mov	w2, 0
	b	L27
L45:
	adrp	x0, lC2@PAGE
	mov	w1, 125
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE19:
	.const
	.align	3
lC4:
	.ascii "khepri_registry.ads"
	.space 1
	.align	3
lC5:
	.ascii "failed precondition from khepri_registry.ads:163"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__set_certification
_khepri_registry__set_certification:
LFB20:
	stp	x29, x30, [sp, -16]!
LCFI6:
	mov	x29, sp
LCFI7:
	cmp	w1, 4
	bhi	L76
	cbz	w1, L77
	adrp	x5, _khepri_registry__admin_addr@PAGE
	ldr	q30, [x4]
	add	x5, x5, _khepri_registry__admin_addr@PAGEOFF;
	ldr	q31, [x5]
	eor	v31.16b, v30.16b, v31.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x6, d31
	cbz	x6, L78
L49:
	adrp	x5, _khepri_registry__certifiers@PAGE
	add	x5, x5, _khepri_registry__certifiers@PAGEOFF;
	add	x6, x5, 3200
	.p2align 5,,15
L54:
	ldr	q31, [x5]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbnz	x7, L70
	ldr	q29, [x4, 16]
	ldr	q31, [x5, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbnz	x7, L70
L51:
	adrp	x11, _khepri_registry__entries@PAGE
	ldr	q30, [x0]
	mov	x5, 0
	add	x4, x11, _khepri_registry__entries@PAGEOFF;
	mov	x10, 856
	mov	x9, x4
	mov	x6, 10000
	ldr	q31, [x4]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbz	x7, L79
	.p2align 5,,15
L58:
	add	x5, x5, 1
	add	x4, x4, 856
	cmp	x5, x6
	beq	L80
	ldr	q31, [x4]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbnz	x7, L58
L79:
	ldr	q29, [x0, 16]
	ldr	q31, [x4, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbnz	x7, L58
	madd	x7, x5, x10, x9
	ldr	x8, [x7, 736]
	cbz	x8, L58
	ldrb	w4, [x7, 752]
	cmp	w4, 1
	bhi	L81
	cbnz	w4, L64
	cmp	w1, 4
	beq	L82
L62:
	mov	x4, 856
	add	x11, x11, _khepri_registry__entries@PAGEOFF;
	mov	x0, 1
	mov	w6, 0
	madd	x5, x5, x4, x11
	mov	w4, w0
	strb	w1, [x5, 96]
	add	x1, x5, 136
	ld1	{v28.16b - v29.16b}, [x3]
	ldp	q30, q31, [x2]
	str	x0, [x5, 744]
	st1	{v28.16b - v29.16b}, [x1]
	str	q30, [x5, 97]
	str	q31, [x5, 113]
L55:
	mov	x0, 0
	bfi	x0, x4, 0, 8
	bfi	x0, x6, 8, 8
	ldp	x29, x30, [sp], 16
LCFI8:
	ret
	.p2align 2,,3
L70:
LCFI9:
	add	x5, x5, 32
	cmp	x6, x5
	bne	L54
	mov	w6, 3
	mov	w4, 0
	b	L55
	.p2align 2,,3
L78:
	ldr	q29, [x5, 16]
	ldr	q31, [x4, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbz	x5, L51
	b	L49
	.p2align 2,,3
L64:
	mov	w4, 0
	mov	x0, 0
	bfi	x0, x4, 0, 8
	mov	w6, 6
	bfi	x0, x6, 8, 8
	ldp	x29, x30, [sp], 16
LCFI10:
	ret
L80:
LCFI11:
	mov	w6, 2
	mov	w4, 0
	b	L55
L82:
	ldr	w0, [x7, 728]
	cmp	w0, 0
	blt	L83
	mov	w6, 7
	beq	L55
	b	L62
L76:
	adrp	x0, lC4@PAGE
	mov	w1, 163
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L83:
	adrp	x0, lC2@PAGE
	mov	w1, 168
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L77:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L81:
	adrp	x0, lC2@PAGE
	mov	w1, 161
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.const
	.align	2
lC0:
	.word	1
	.word	48
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__add_auditor
_khepri_registry__add_auditor:
LFB21:
	adrp	x3, _khepri_registry__entries@PAGE
	ldr	q30, [x0]
	mov	x4, 0
	add	x3, x3, _khepri_registry__entries@PAGEOFF;
	mov	x10, 856
	mov	x9, x3
	mov	x5, 10000
	b	L90
	.p2align 2,,3
L87:
	add	x4, x4, 1
	add	x3, x3, 856
	cmp	x4, x5
	beq	L104
L90:
	ldr	q31, [x3]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x6, d31
	cbnz	x6, L87
	ldr	q29, [x0, 16]
	ldr	q31, [x3, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x6, d31
	cbnz	x6, L87
	mul	x7, x4, x10
	add	x6, x9, x7
	ldr	x8, [x6, 736]
	cbz	x8, L87
	stp	x29, x30, [sp, -112]!
LCFI12:
	mov	x29, sp
LCFI13:
	ldrb	w3, [x6, 752]
	cmp	w3, 1
	bhi	L105
	cbz	w3, L106
	mov	w3, 0
	mov	w4, 6
L91:
	mov	x0, 0
	bfi	x0, x3, 0, 8
	bfi	x0, x4, 8, 8
	ldp	x29, x30, [sp], 112
LCFI14:
	ret
	.p2align 2,,3
L106:
LCFI15:
	ldr	w0, [x6, 728]
	tbnz	w0, #31, L107
	mov	w4, 9
	cmp	w0, 4
	bgt	L91
	ldp	q26, q28, [x2]
	sbfiz	x5, x0, 3, 32
	sub	x5, x5, w0, sxtw
	add	x5, x7, x5, lsl 4
	add	w0, w0, 1
	mov	x7, 1
	ldp	q27, q31, [x2, 32]
	mov	w3, w7
	mov	w4, 0
	add	x9, x9, x5
	ldp	q29, q30, [x1]
	add	x1, x9, 200
	str	q26, [x9, 200]
	stp	q28, q27, [x1, 16]
	str	w0, [x6, 728]
	mov	x0, 0
	bfi	x0, x3, 0, 8
	str	q29, [x9, 168]
	str	q30, [x9, 184]
	bfi	x0, x4, 8, 8
	str	q31, [x9, 248]
	str	x7, [x9, 264]
	strb	w7, [x9, 272]
	str	x7, [x6, 744]
	ldp	x29, x30, [sp], 112
LCFI16:
	ret
L104:
	mov	w3, 0
	mov	x0, 0
	bfi	x0, x3, 0, 8
	mov	w4, 2
	bfi	x0, x4, 8, 8
	ret
L107:
LCFI17:
	adrp	x0, lC2@PAGE
	mov	w1, 211
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L105:
	adrp	x0, lC2@PAGE
	mov	w1, 205
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE21:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__revoke
_khepri_registry__revoke:
LFB22:
	adrp	x3, _khepri_registry__admin_addr@PAGE
	ldr	q30, [x2]
	add	x3, x3, _khepri_registry__admin_addr@PAGEOFF;
	ldr	q31, [x3]
	eor	v31.16b, v30.16b, v31.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L109
	ldr	q29, [x3, 16]
	ldr	q31, [x2, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x3, d31
	cbz	x3, L111
L109:
	adrp	x3, _khepri_registry__certifiers@PAGE
	add	x3, x3, _khepri_registry__certifiers@PAGEOFF;
	add	x4, x3, 3200
	.p2align 5,,15
L114:
	ldr	q31, [x3]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbnz	x5, L127
	ldr	q29, [x2, 16]
	ldr	q31, [x3, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbnz	x5, L127
L111:
	adrp	x2, _khepri_registry__entries@PAGE
	ldr	q30, [x0]
	mov	x3, 0
	add	x2, x2, _khepri_registry__entries@PAGEOFF;
	mov	x8, 856
	mov	x7, x2
	mov	x4, 10000
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbz	x5, L135
	.p2align 5,,15
L118:
	add	x3, x3, 1
	add	x2, x2, 856
	cmp	x3, x4
	beq	L136
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbnz	x5, L118
L135:
	ldr	q29, [x0, 16]
	ldr	q31, [x2, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbnz	x5, L118
	madd	x5, x3, x8, x7
	ldr	x6, [x5, 736]
	cbz	x6, L118
	ldrb	w0, [x5, 752]
	cmp	w0, 1
	bhi	L137
	cbnz	w0, L122
	mov	w3, 1
	mov	x0, 1
	add	x4, x5, 753
	mov	w2, 0
	strb	w3, [x5, 752]
	mov	w3, w0
	ldp	q31, q30, [x1]
	strb	wzr, [x5, 96]
	str	x0, [x5, 744]
	stp	q31, q30, [x4]
L115:
	mov	x0, 0
	bfi	x0, x3, 0, 8
	bfi	x0, x2, 8, 8
	ret
	.p2align 2,,3
L127:
	add	x3, x3, 32
	cmp	x3, x4
	bne	L114
	mov	w2, 3
	mov	w3, 0
	b	L115
	.p2align 2,,3
L122:
	mov	w3, 0
	mov	x0, 0
	bfi	x0, x3, 0, 8
	mov	w2, 6
	bfi	x0, x2, 8, 8
	ret
L136:
	mov	w2, 2
	mov	w3, 0
	b	L115
L137:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI18:
	mov	w1, 258
	mov	x29, sp
LCFI19:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__is_registered
_khepri_registry__is_registered:
LFB23:
	adrp	x1, _khepri_registry__entries@PAGE
	ldr	q30, [x0]
	mov	x2, 0
	add	x1, x1, _khepri_registry__entries@PAGEOFF;
	mov	x5, 856
	mov	x6, x1
	mov	x3, 10000
	b	L143
	.p2align 2,,3
L141:
	add	x2, x2, 1
	add	x1, x1, 856
	cmp	x2, x3
	beq	L146
L143:
	ldr	q31, [x1]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L141
	ldr	q29, [x0, 16]
	madd	x4, x2, x5, x6
	ldr	q31, [x1, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbnz	x7, L141
	ldr	x4, [x4, 736]
	cbz	x4, L141
	mov	w0, 1
	ret
L146:
	mov	w0, 0
	ret
LFE23:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__get_level
_khepri_registry__get_level:
LFB24:
	adrp	x1, _khepri_registry__entries@PAGE
	ldr	q30, [x0]
	mov	x2, 0
	add	x1, x1, _khepri_registry__entries@PAGEOFF;
	mov	x6, 856
	mov	x7, x1
	mov	x3, 10000
	b	L153
	.p2align 2,,3
L150:
	add	x2, x2, 1
	add	x1, x1, 856
	cmp	x2, x3
	beq	L165
L153:
	ldr	q31, [x1]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L150
	ldr	q29, [x0, 16]
	ldr	q31, [x1, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L150
	madd	x4, x2, x6, x7
	ldr	x5, [x4, 736]
	cbz	x5, L150
	stp	x29, x30, [sp, -16]!
LCFI20:
	mov	x29, sp
LCFI21:
	ldrb	w0, [x4, 752]
	cmp	w0, 1
	bhi	L166
	cbnz	w0, L155
	ldrb	w0, [x4, 96]
	cmp	w0, 4
	bhi	L167
	ldp	x29, x30, [sp], 16
LCFI22:
	ret
	.p2align 2,,3
L155:
LCFI23:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI24:
	ret
L165:
	mov	w0, 0
	ret
L166:
LCFI25:
	adrp	x0, lC2@PAGE
	mov	w1, 287
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L167:
	adrp	x0, lC2@PAGE
	mov	w1, 290
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__get_registry_entry
_khepri_registry__get_registry_entry:
LFB25:
	mov	x5, x0
	adrp	x2, _khepri_registry__entries@PAGE
	stp	x29, x30, [sp, -16]!
LCFI26:
	add	x2, x2, _khepri_registry__entries@PAGEOFF;
	mov	x29, sp
LCFI27:
	mov	x3, 0
	mov	x0, x1
	ldr	q30, [x5]
	mov	x7, x2
	mov	x6, 856
	mov	x4, 10000
	b	L173
	.p2align 2,,3
L171:
	add	x3, x3, 1
	add	x2, x2, 856
	cmp	x3, x4
	beq	L179
L173:
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbnz	x1, L171
	ldr	q31, [x2, 16]
	madd	x1, x3, x6, x7
	ldr	q29, [x5, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x8, d31
	cbnz	x8, L171
	ldr	x8, [x1, 736]
	cbz	x8, L171
	mov	x2, 856
	bl	_memcpy
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI28:
	ret
L179:
LCFI29:
	mov	x2, 856
	mov	w1, 0
	bl	_memset
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI30:
	ret
LFE25:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__is_revoked
_khepri_registry__is_revoked:
LFB26:
	adrp	x1, _khepri_registry__entries@PAGE
	ldr	q30, [x0]
	mov	x2, 0
	add	x1, x1, _khepri_registry__entries@PAGEOFF;
	mov	x6, 856
	mov	x7, x1
	mov	x3, 10000
	b	L186
	.p2align 2,,3
L183:
	add	x2, x2, 1
	add	x1, x1, 856
	cmp	x2, x3
	beq	L194
L186:
	ldr	q31, [x1]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L183
	ldr	q29, [x0, 16]
	ldr	q31, [x1, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L183
	madd	x4, x2, x6, x7
	ldr	x5, [x4, 736]
	cbz	x5, L183
	ldrb	w0, [x4, 752]
	cmp	w0, 1
	bhi	L195
	ret
L194:
	mov	w0, 0
	ret
L195:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI31:
	mov	w1, 315
	mov	x29, sp
LCFI32:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__get_wcet_bound
_khepri_registry__get_wcet_bound:
LFB27:
	adrp	x1, _khepri_registry__entries@PAGE
	ldr	q30, [x0]
	mov	x2, 0
	add	x1, x1, _khepri_registry__entries@PAGEOFF;
	mov	x5, 856
	mov	x6, x1
	mov	x3, 10000
	b	L201
	.p2align 2,,3
L199:
	add	x2, x2, 1
	add	x1, x1, 856
	cmp	x2, x3
	beq	L206
L201:
	ldr	q31, [x1]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L199
	ldr	q29, [x0, 16]
	madd	x4, x2, x5, x6
	ldr	q31, [x1, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbnz	x7, L199
	ldr	x7, [x4, 736]
	cbz	x7, L199
	add	x4, x4, 136
	ld1	{v30.16b - v31.16b}, [x4]
	st1	{v30.16b - v31.16b}, [x8]
	ret
L206:
	movi	v31.4s, 0
	stp	q31, q31, [x8]
	ret
LFE27:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__get_gas_discount
_khepri_registry__get_gas_discount:
LFB28:
	stp	x29, x30, [sp, -16]!
LCFI33:
	mov	x29, sp
LCFI34:
	bl	_khepri_registry__get_level
	cmp	w0, 4
	beq	L209
	adrp	x1, _CSWTCH.74@PAGE
	add	x1, x1, _CSWTCH.74@PAGEOFF;
	ldr	w0, [x1, w0, uxtw 2]
	ldp	x29, x30, [sp], 16
LCFI35:
	ret
	.p2align 2,,3
L209:
LCFI36:
	mov	w0, 35
	ldp	x29, x30, [sp], 16
LCFI37:
	ret
LFE28:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__apply_discount
_khepri_registry__apply_discount:
LFB30:
	stp	x29, x30, [sp, -192]!
LCFI38:
	mov	x29, sp
LCFI39:
	stp	x19, x20, [sp, 16]
LCFI40:
	mov	x19, x0
	mov	x0, x1
	mov	x20, x8
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI41:
	bl	_khepri_registry__get_level
	cmp	w0, 4
	beq	L215
	adrp	x1, _CSWTCH.74@PAGE
	add	x1, x1, _CSWTCH.74@PAGEOFF;
	ldr	w0, [x1, w0, uxtw 2]
	cbnz	w0, L217
	ld1	{v30.16b - v31.16b}, [x19]
	st1	{v30.16b - v31.16b}, [x20]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 192
LCFI42:
	ret
	.p2align 2,,3
L215:
LCFI43:
	mov	x0, 35
L212:
	add	x22, x29, 160
	add	x21, x29, 96
	mov	x8, x22
	add	x23, x29, 64
	bl	_aegis_u256__from_word64
	add	x2, x29, 128
	mov	x3, x21
	mov	x1, x22
	mov	x0, x19
	bl	_aegis_u256__mul
	mov	x8, x22
	mov	x0, 100
	bl	_aegis_u256__from_word64
	mov	x1, x22
	mov	x8, x23
	mov	x0, x21
	bl	_aegis_u256__div
	ld1	{v30.16b - v31.16b}, [x23]
	mov	x0, x19
	mov	x1, x21
	mov	x8, x23
	st1	{v30.16b - v31.16b}, [x21]
	bl	_aegis_u256__sub_mod
	ld1	{v30.16b - v31.16b}, [x23]
	st1	{v30.16b - v31.16b}, [x20]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 192
LCFI44:
	ret
	.p2align 2,,3
L217:
LCFI45:
	sxtw	x0, w0
	b	L212
LFE30:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__record_gas_usage
_khepri_registry__record_gas_usage:
LFB31:
	stp	x29, x30, [sp, -144]!
LCFI46:
	mov	x29, sp
LCFI47:
	mov	x4, 40320
	mov	x1, 856
	movk	x4, 0x82, lsl 16
	stp	x23, x24, [sp, 48]
LCFI48:
	adrp	x24, _khepri_registry__entries@PAGE
	add	x3, x24, _khepri_registry__entries@PAGEOFF;
	mov	x23, x3
	stp	x19, x20, [sp, 16]
LCFI49:
	mov	x19, 0
	mov	x20, 0
	stp	x21, x22, [sp, 32]
LCFI50:
	mov	x22, x2
	stp	x25, x26, [sp, 64]
LCFI51:
	ldr	q30, [x0]
	b	L224
	.p2align 2,,3
L221:
	add	x19, x19, 856
	add	x20, x20, 1
	add	x3, x3, 856
	cmp	x19, x4
	beq	L218
L224:
	ldr	q31, [x3]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x2, d31
	cbnz	x2, L221
	ldr	q29, [x0, 16]
	ldr	q31, [x3, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x2, d31
	cbnz	x2, L221
	madd	x21, x20, x1, x23
	ldr	x5, [x21, 736]
	cbz	x5, L221
	add	x25, x29, 112
	mov	x0, 1
	mov	x8, x25
	add	x26, x29, 80
	bl	_aegis_u256__from_word64
	add	x0, x19, 792
	mov	x1, x25
	add	x0, x23, x0
	mov	x2, x26
	bl	_aegis_u256__add
	cmp	w0, 1
	bhi	L235
	cbz	w0, L236
L226:
	add	x0, x19, 824
	add	x24, x24, _khepri_registry__entries@PAGEOFF;
	add	x0, x24, x0
	mov	x1, x22
	mov	x2, x26
	bl	_aegis_u256__add
	cmp	w0, 1
	bhi	L237
	cbnz	w0, L218
	ld1	{v30.16b - v31.16b}, [x26]
	mov	x0, 856
	madd	x0, x20, x0, x24
	add	x0, x0, 824
	st1	{v30.16b - v31.16b}, [x0]
L218:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 144
LCFI52:
	ret
	.p2align 2,,3
L236:
LCFI53:
	ld1	{v30.16b - v31.16b}, [x26]
	add	x21, x21, 792
	st1	{v30.16b - v31.16b}, [x21]
	b	L226
L237:
	adrp	x0, lC2@PAGE
	mov	w1, 381
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L235:
	adrp	x0, lC2@PAGE
	mov	w1, 375
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE31:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__verify_proof
_khepri_registry__verify_proof:
LFB32:
	adrp	x2, _khepri_registry__entries@PAGE
	mov	x5, 40320
	ldr	q30, [x0]
	add	x2, x2, _khepri_registry__entries@PAGEOFF;
	mov	x3, 0
	mov	x4, 0
	mov	x7, x2
	mov	x8, 856
	movk	x5, 0x82, lsl 16
	b	L245
	.p2align 2,,3
L241:
	add	x3, x3, 856
	add	x4, x4, 1
	add	x2, x2, 856
	cmp	x3, x5
	beq	L250
L245:
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x6, d31
	cbnz	x6, L241
	ldr	q29, [x0, 16]
	madd	x6, x4, x8, x7
	ldr	q31, [x2, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x9, d31
	cbnz	x9, L241
	ldr	x6, [x6, 736]
	cbz	x6, L241
	add	x3, x3, 97
	ldr	q30, [x1]
	add	x0, x7, x3
	ldr	q31, [x7, x3]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x2, d31
	cbz	x2, L251
	mov	w0, 1
L243:
	eor	w0, w0, 1
	ret
	.p2align 2,,3
L251:
	ldr	q31, [x0, 16]
	ldr	q30, [x1, 16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbz	x0, L243
	mov	w0, 1
	b	L243
L250:
	mov	w0, 0
	ret
LFE32:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__verify_auditor
_khepri_registry__verify_auditor:
LFB33:
	adrp	x10, _khepri_registry__entries@PAGE
	mov	x6, 40320
	ldr	q30, [x0]
	add	x2, x10, _khepri_registry__entries@PAGEOFF;
	mov	x3, 0
	mov	x4, 0
	mov	x5, x2
	mov	x9, 856
	movk	x6, 0x82, lsl 16
	b	L258
	.p2align 2,,3
L255:
	add	x3, x3, 856
	add	x4, x4, 1
	add	x2, x2, 856
	cmp	x3, x6
	beq	L278
L258:
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbnz	x7, L255
	ldr	q29, [x0, 16]
	ldr	q31, [x2, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x7, d31
	cbnz	x7, L255
	madd	x7, x4, x9, x5
	ldr	x8, [x7, 736]
	cbz	x8, L255
	stp	x29, x30, [sp, -16]!
LCFI54:
	mov	x29, sp
LCFI55:
	ldr	w6, [x7, 728]
	cmp	w6, 0
	blt	L279
	ccmp	w6, 5, 0, ne
	cset	w0, gt
	ble	L280
	mov	w0, 0
L259:
	ldp	x29, x30, [sp], 16
LCFI56:
	ret
	.p2align 2,,3
L280:
LCFI57:
	mov	x8, 856
	add	x3, x3, 168
	ldr	q30, [x1]
	add	x2, x5, x3
	sub	w6, w6, #1
	mul	x4, x4, x8
	mov	x3, 0
	add	x7, x10, _khepri_registry__entries@PAGEOFF;
	b	L260
	.p2align 2,,3
L263:
	cmp	w6, w3
	beq	L259
	add	x3, x3, 1
	add	x2, x2, 112
	cmp	x3, 5
	beq	L281
L260:
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbnz	x5, L263
	ldr	q29, [x1, 16]
	ldr	q31, [x2, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbnz	x5, L263
	lsl	x5, x3, 3
	sub	x5, x5, x3
	add	x5, x4, x5, lsl 4
	add	x5, x7, x5
	ldrb	w5, [x5, 272]
	cmp	w5, 1
	bhi	L282
	cbz	w5, L263
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI58:
	ret
L278:
	mov	w0, 0
	ret
L282:
LCFI59:
	adrp	x0, lC2@PAGE
	mov	w1, 423
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L281:
	adrp	x0, lC2@PAGE
	mov	w1, 422
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L279:
	adrp	x0, lC2@PAGE
	mov	w1, 413
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE33:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__meets_level
_khepri_registry__meets_level:
LFB34:
	adrp	x2, _khepri_registry__entries@PAGE
	stp	x29, x30, [sp, -16]!
LCFI60:
	mov	x3, 0
	add	x2, x2, _khepri_registry__entries@PAGEOFF;
	mov	x29, sp
LCFI61:
	mov	x8, x2
	mov	x7, 856
	ldr	q30, [x0]
	mov	x4, 10000
	b	L289
	.p2align 2,,3
L286:
	add	x3, x3, 1
	add	x2, x2, 856
	cmp	x3, x4
	beq	L292
L289:
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbnz	x5, L286
	ldr	q29, [x0, 16]
	ldr	q31, [x2, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x5, d31
	cbnz	x5, L286
	madd	x5, x3, x7, x8
	ldr	x6, [x5, 736]
	cbz	x6, L286
	ldrb	w0, [x5, 752]
	cmp	w0, 1
	bhi	L299
	cbnz	w0, L292
	ldrb	w0, [x5, 96]
	cmp	w0, 4
	bhi	L300
L290:
	cmp	w1, 4
	bhi	L301
	cmp	w1, w0
	cset	w0, ls
	ldp	x29, x30, [sp], 16
LCFI62:
	ret
	.p2align 2,,3
L292:
LCFI63:
	mov	w0, 0
	b	L290
L299:
	adrp	x0, lC2@PAGE
	mov	w1, 287
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L301:
	adrp	x0, lC2@PAGE
	mov	w1, 438
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L300:
	adrp	x0, lC2@PAGE
	mov	w1, 290
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE34:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__total_registered
_khepri_registry__total_registered:
LFB35:
	adrp	x0, _khepri_registry__entry_count@PAGE
	ldr	w0, [x0, #_khepri_registry__entry_count@PAGEOFF]
	tbnz	w0, #31, L307
	ret
L307:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI64:
	mov	w1, 447
	mov	x29, sp
LCFI65:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE35:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__count_by_level
_khepri_registry__count_by_level:
LFB36:
	adrp	x4, _khepri_registry__entries@PAGE
	mov	x1, 0
	mov	w7, 0
	add	x4, x4, _khepri_registry__entries@PAGEOFF;
	mov	x5, 856
	mov	w8, 9999
	mov	x6, 10000
	.p2align 5,,15
L319:
	madd	x2, x1, x5, x4
	ldr	x3, [x2, 736]
	cbz	x3, L325
	stp	x29, x30, [sp, -16]!
LCFI66:
	mov	x29, sp
LCFI67:
L320:
	ldrb	w3, [x2, 752]
	cmp	w3, 1
	bhi	L326
	cbnz	w3, L309
	ldrb	w2, [x2, 96]
	cmp	w2, 4
	bhi	L312
	cmp	w0, 4
	bhi	L312
	cmp	w7, w8
	ccmp	w2, w0, 0, le
	cinc	w7, w7, eq
L309:
	add	x1, x1, 1
	cmp	x1, x6
	beq	L327
L313:
	madd	x2, x1, x5, x4
	ldr	x3, [x2, 736]
	cbnz	x3, L320
	add	x1, x1, 1
	cmp	x1, x6
	bne	L313
L327:
	mov	w0, w7
	ldp	x29, x30, [sp], 16
LCFI68:
	ret
	.p2align 2,,3
L325:
	add	x1, x1, 1
	cmp	x1, x6
	bne	L319
	mov	w0, w7
	ret
L312:
LCFI69:
	adrp	x0, lC2@PAGE
	mov	w1, 456
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L326:
	adrp	x0, lC2@PAGE
	mov	w1, 455
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE36:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__total_gas_saved
_khepri_registry__total_gas_saved:
LFB37:
	movi	v31.4s, 0
	stp	x29, x30, [sp, -144]!
LCFI70:
	mov	x29, sp
LCFI71:
	stp	x19, x20, [sp, 16]
LCFI72:
	adrp	x20, _khepri_registry__entries@PAGE+824
	mov	x19, 0
	add	x20, x20, _khepri_registry__entries@PAGEOFF+824;
	stp	x21, x22, [sp, 32]
LCFI73:
	adrp	x21, _khepri_registry__entries@PAGE
	mov	x22, x8
	add	x21, x21, _khepri_registry__entries@PAGEOFF;
	stp	x23, x24, [sp, 48]
LCFI74:
	add	x24, x29, 80
	mov	x23, 856
	stp	x25, x26, [sp, 64]
LCFI75:
	add	x26, x29, 112
	mov	x25, 10000
	stp	q31, q31, [x29, 80]
	b	L333
	.p2align 2,,3
L330:
	add	x19, x19, 1
	add	x20, x20, 856
	cmp	x19, x25
	beq	L336
L333:
	madd	x0, x19, x23, x21
	ldr	x0, [x0, 736]
	cbz	x0, L330
	mov	x2, x26
	mov	x1, x20
	mov	x0, x24
	bl	_aegis_u256__add
	cmp	w0, 1
	bhi	L337
	cbnz	w0, L330
	ld1	{v30.16b - v31.16b}, [x26]
	add	x19, x19, 1
	add	x20, x20, 856
	st1	{v30.16b - v31.16b}, [x24]
	cmp	x19, x25
	bne	L333
L336:
	ld1	{v30.16b - v31.16b}, [x24]
	st1	{v30.16b - v31.16b}, [x22]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 144
LCFI76:
	ret
L337:
LCFI77:
	adrp	x0, lC2@PAGE
	mov	w1, 475
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE37:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__initialize
_khepri_registry__initialize:
LFB38:
	adrp	x0, _khepri_registry__entries@PAGE
	mov	x2, 40320
	stp	x29, x30, [sp, -16]!
LCFI78:
	movk	x2, 0x82, lsl 16
	mov	x29, sp
LCFI79:
	mov	w1, 0
	add	x0, x0, _khepri_registry__entries@PAGEOFF;
	bl	_memset
	adrp	x3, _khepri_registry__entry_count@PAGE
	adrp	x0, _khepri_registry__certifiers@PAGE
	mov	x2, 3200
	mov	w1, 0
	add	x0, x0, _khepri_registry__certifiers@PAGEOFF;
	str	wzr, [x3, #_khepri_registry__entry_count@PAGEOFF]
	bl	_memset
	adrp	x0, _khepri_registry__admin_addr@PAGE
	adrp	x1, _khepri_registry__certifier_count@PAGE
	add	x0, x0, _khepri_registry__admin_addr@PAGEOFF;
	str	wzr, [x1, #_khepri_registry__certifier_count@PAGEOFF]
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	ldp	x29, x30, [sp], 16
LCFI80:
	ret
LFE38:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__is_certifier
_khepri_registry__is_certifier:
LFB39:
	adrp	x1, _khepri_registry__admin_addr@PAGE
	ldr	q30, [x0]
	add	x1, x1, _khepri_registry__admin_addr@PAGEOFF;
	ldr	q31, [x1]
	eor	v31.16b, v30.16b, v31.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x2, d31
	cbnz	x2, L341
	ldr	q29, [x1, 16]
	ldr	q31, [x0, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbz	x1, L351
L341:
	adrp	x1, _khepri_registry__certifiers@PAGE
	add	x1, x1, _khepri_registry__certifiers@PAGEOFF;
	add	x2, x1, 3200
	b	L346
	.p2align 2,,3
L349:
	add	x1, x1, 32
	cmp	x1, x2
	beq	L352
L346:
	ldr	q31, [x1]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x3, d31
	cbnz	x3, L349
	ldr	q29, [x0, 16]
	ldr	q31, [x1, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x3, d31
	cbnz	x3, L349
L351:
	mov	w0, 1
	ret
L352:
	mov	w0, 0
	ret
LFE39:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__add_certifier
_khepri_registry__add_certifier:
LFB40:
	adrp	x2, _khepri_registry__admin_addr@PAGE
	ldr	q31, [x1]
	mov	x3, x0
	add	x2, x2, _khepri_registry__admin_addr@PAGEOFF;
	ldr	q30, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L354
	ldr	q31, [x1, 16]
	ldr	q30, [x2, 16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L354
	adrp	x4, _khepri_registry__certifier_count@PAGE
	ldr	w1, [x4, #_khepri_registry__certifier_count@PAGEOFF]
	tbnz	w1, #31, L364
	mov	w0, 0
	cmp	w1, 99
	bgt	L356
	ldp	q30, q31, [x3]
	adrp	x2, _khepri_registry__certifiers@PAGE
	sbfiz	x5, x1, 5, 32
	add	x2, x2, _khepri_registry__certifiers@PAGEOFF;
	add	w1, w1, 1
	add	x3, x2, x5
	mov	w0, 1
	str	w1, [x4, #_khepri_registry__certifier_count@PAGEOFF]
	str	q30, [x2, x5]
	str	q31, [x3, 16]
L356:
	ret
	.p2align 2,,3
L354:
	mov	w0, 0
	ret
L364:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI81:
	mov	w1, 523
	mov	x29, sp
LCFI82:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE40:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry__remove_certifier
_khepri_registry__remove_certifier:
LFB41:
	adrp	x2, _khepri_registry__admin_addr@PAGE
	ldr	q31, [x1]
	add	x2, x2, _khepri_registry__admin_addr@PAGEOFF;
	ldr	q30, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x3, d31
	cbnz	x3, L376
	ldr	q31, [x1, 16]
	ldr	q30, [x2, 16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbnz	x1, L376
	ldr	q30, [x0]
	adrp	x3, _khepri_registry__certifiers@PAGE
	add	x2, x3, _khepri_registry__certifiers@PAGEOFF;
	.p2align 5,,15
L372:
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L374
	ldr	q29, [x0, 16]
	ldr	q31, [x2, 16]
	eor	v31.16b, v31.16b, v29.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L374
	add	x2, x3, _khepri_registry__certifiers@PAGEOFF;
	mov	w0, 1
	add	x1, x2, x1, lsl 5
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	ret
	.p2align 2,,3
L374:
	add	x1, x1, 1
	add	x2, x2, 32
	cmp	x1, 100
	bne	L372
L376:
	mov	w0, 0
	ret
LFE41:
	.align	2
	.p2align 5,,15
	.globl _khepri_registry___elabb
_khepri_registry___elabb:
LFB0:
	adrp	x0, _khepri_registry__entries@PAGE
	mov	x2, 40320
	add	x0, x0, _khepri_registry__entries@PAGEOFF;
	movk	x2, 0x82, lsl 16
	mov	w1, 0
	b	_memset
LFE0:
	.const
	.align	2
_CSWTCH.74:
	.word	0
	.word	5
	.word	15
	.word	25
_registry_errorG.4:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	8
	.byte	0
	.byte	2
	.byte	0
	.byte	7
	.byte	2
	.byte	0
	.byte	8
	.byte	0
	.byte	0
	.byte	1
	.align	1
_registry_errorT2.5:
	.byte	4
	.byte	9
	.align	1
_registry_errorT1.6:
	.byte	6
	.byte	4
	.align	3
_registry_errorP.7:
	.word	9
	.word	17
	.align	3
_certification_levelG.8:
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
	.data
_khepri_registry__admin_addr:
	.space 32
_khepri_registry__certifiers:
	.space 3200
	.zerofill __DATA,__bss,_khepri_registry__entries,8560000,4
	.globl _khepri_registry__registry_errorN
	.const
	.align	1
_khepri_registry__registry_errorN:
	.hword	1
	.hword	11
	.hword	35
	.hword	55
	.hword	73
	.hword	92
	.hword	111
	.hword	132
	.hword	154
	.hword	177
	.hword	196
	.globl _khepri_registry__registry_errorS
	.align	3
_khepri_registry__registry_errorS:
	.ascii "ERROR_NONEERROR_ALREADY_REGISTEREDERROR_NOT_REGISTEREDERROR_UNAUTHORIZEDERROR_INVALID_PROOFERROR_INVALID_LEVELERROR_ALREADY_REVOKEDERROR_AUDITOR_REQUIREDERROR_INVALID_SIGNATUREERROR_REGISTRY_FULL"
	.globl _khepri_registry__null_entry
	.align	3
_khepri_registry__null_entry:
	.space 856
	.globl _khepri_registry__certification_levelN
	.align	3
_khepri_registry__certification_levelN:
	.byte	1
	.byte	11
	.byte	23
	.byte	35
	.byte	45
	.byte	59
	.space 2
	.globl _khepri_registry__certification_levelS
	.align	3
_khepri_registry__certification_levelS:
	.ascii "LEVEL_NONELEVEL_BRONZELEVEL_SILVERLEVEL_GOLDLEVEL_PLATINUM"
	.data
	.align	2
_khepri_registry__certifier_count:
	.space 4
	.align	2
_khepri_registry__entry_count:
	.space 4
	.globl _khepri_registry_E
	.align	1
_khepri_registry_E:
	.space 2
	.literal16
	.align	4
lC3:
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
	.quad	LFB81-.
	.set L$set$8,LFE81-LFB81
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
	.quad	LFB83-.
	.set L$set$12,LFE83-LFB83
	.quad L$set$12
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$13,LEFDE13-LASFDE13
	.long L$set$13
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB85-.
	.set L$set$14,LFE85-LFB85
	.quad L$set$14
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$15,LEFDE15-LASFDE15
	.long L$set$15
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB87-.
	.set L$set$16,LFE87-LFB87
	.quad L$set$16
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$17,LEFDE17-LASFDE17
	.long L$set$17
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB89-.
	.set L$set$18,LFE89-LFB89
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
	.quad	LFB19-.
	.set L$set$24,LFE19-LFB19
	.quad L$set$24
	.uleb128 0
	.byte	0x4
	.set L$set$25,LCFI0-LFB19
	.long L$set$25
	.byte	0xe
	.uleb128 0x380
	.byte	0x4
	.set L$set$26,LCFI1-LCFI0
	.long L$set$26
	.byte	0x9d
	.uleb128 0x70
	.byte	0x9e
	.uleb128 0x6f
	.byte	0x4
	.set L$set$27,LCFI2-LCFI1
	.long L$set$27
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$28,LCFI3-LCFI2
	.long L$set$28
	.byte	0x93
	.uleb128 0x6e
	.byte	0x94
	.uleb128 0x6d
	.byte	0x4
	.set L$set$29,LCFI4-LCFI3
	.long L$set$29
	.byte	0xa
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
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
	.quad	LFB20-.
	.set L$set$32,LFE20-LFB20
	.quad L$set$32
	.uleb128 0
	.byte	0x4
	.set L$set$33,LCFI6-LFB20
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
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$39,LEFDE27-LASFDE27
	.long L$set$39
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB21-.
	.set L$set$40,LFE21-LFB21
	.quad L$set$40
	.uleb128 0
	.byte	0x4
	.set L$set$41,LCFI12-LFB21
	.long L$set$41
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$42,LCFI13-LCFI12
	.long L$set$42
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$43,LCFI14-LCFI13
	.long L$set$43
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI15-LCFI14
	.long L$set$44
	.byte	0xb
	.byte	0x4
	.set L$set$45,LCFI16-LCFI15
	.long L$set$45
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$46,LCFI17-LCFI16
	.long L$set$46
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$47,LEFDE29-LASFDE29
	.long L$set$47
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB22-.
	.set L$set$48,LFE22-LFB22
	.quad L$set$48
	.uleb128 0
	.byte	0x4
	.set L$set$49,LCFI18-LFB22
	.long L$set$49
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$50,LCFI19-LCFI18
	.long L$set$50
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$51,LEFDE31-LASFDE31
	.long L$set$51
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB23-.
	.set L$set$52,LFE23-LFB23
	.quad L$set$52
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$53,LEFDE33-LASFDE33
	.long L$set$53
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB24-.
	.set L$set$54,LFE24-LFB24
	.quad L$set$54
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI20-LFB24
	.long L$set$55
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$56,LCFI21-LCFI20
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$57,LCFI22-LCFI21
	.long L$set$57
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$58,LCFI23-LCFI22
	.long L$set$58
	.byte	0xb
	.byte	0x4
	.set L$set$59,LCFI24-LCFI23
	.long L$set$59
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$60,LCFI25-LCFI24
	.long L$set$60
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$61,LEFDE35-LASFDE35
	.long L$set$61
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB25-.
	.set L$set$62,LFE25-LFB25
	.quad L$set$62
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI26-LFB25
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
	.byte	0x4
	.set L$set$67,LCFI30-LCFI29
	.long L$set$67
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$68,LEFDE37-LASFDE37
	.long L$set$68
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB26-.
	.set L$set$69,LFE26-LFB26
	.quad L$set$69
	.uleb128 0
	.byte	0x4
	.set L$set$70,LCFI31-LFB26
	.long L$set$70
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$71,LCFI32-LCFI31
	.long L$set$71
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$72,LEFDE39-LASFDE39
	.long L$set$72
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB27-.
	.set L$set$73,LFE27-LFB27
	.quad L$set$73
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$74,LEFDE41-LASFDE41
	.long L$set$74
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB28-.
	.set L$set$75,LFE28-LFB28
	.quad L$set$75
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI33-LFB28
	.long L$set$76
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$77,LCFI34-LCFI33
	.long L$set$77
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$78,LCFI35-LCFI34
	.long L$set$78
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$79,LCFI36-LCFI35
	.long L$set$79
	.byte	0xb
	.byte	0x4
	.set L$set$80,LCFI37-LCFI36
	.long L$set$80
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$81,LEFDE43-LASFDE43
	.long L$set$81
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB30-.
	.set L$set$82,LFE30-LFB30
	.quad L$set$82
	.uleb128 0
	.byte	0x4
	.set L$set$83,LCFI38-LFB30
	.long L$set$83
	.byte	0xe
	.uleb128 0xc0
	.byte	0x9d
	.uleb128 0x18
	.byte	0x9e
	.uleb128 0x17
	.byte	0x4
	.set L$set$84,LCFI39-LCFI38
	.long L$set$84
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$85,LCFI40-LCFI39
	.long L$set$85
	.byte	0x93
	.uleb128 0x16
	.byte	0x94
	.uleb128 0x15
	.byte	0x4
	.set L$set$86,LCFI41-LCFI40
	.long L$set$86
	.byte	0x95
	.uleb128 0x14
	.byte	0x96
	.uleb128 0x13
	.byte	0x97
	.uleb128 0x12
	.byte	0x4
	.set L$set$87,LCFI42-LCFI41
	.long L$set$87
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
	.set L$set$88,LCFI43-LCFI42
	.long L$set$88
	.byte	0xb
	.byte	0x4
	.set L$set$89,LCFI44-LCFI43
	.long L$set$89
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
	.set L$set$90,LCFI45-LCFI44
	.long L$set$90
	.byte	0xb
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$91,LEFDE45-LASFDE45
	.long L$set$91
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB31-.
	.set L$set$92,LFE31-LFB31
	.quad L$set$92
	.uleb128 0
	.byte	0x4
	.set L$set$93,LCFI46-LFB31
	.long L$set$93
	.byte	0xe
	.uleb128 0x90
	.byte	0x9d
	.uleb128 0x12
	.byte	0x9e
	.uleb128 0x11
	.byte	0x4
	.set L$set$94,LCFI47-LCFI46
	.long L$set$94
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$95,LCFI48-LCFI47
	.long L$set$95
	.byte	0x97
	.uleb128 0xc
	.byte	0x98
	.uleb128 0xb
	.byte	0x4
	.set L$set$96,LCFI49-LCFI48
	.long L$set$96
	.byte	0x93
	.uleb128 0x10
	.byte	0x94
	.uleb128 0xf
	.byte	0x4
	.set L$set$97,LCFI50-LCFI49
	.long L$set$97
	.byte	0x95
	.uleb128 0xe
	.byte	0x96
	.uleb128 0xd
	.byte	0x4
	.set L$set$98,LCFI51-LCFI50
	.long L$set$98
	.byte	0x99
	.uleb128 0xa
	.byte	0x9a
	.uleb128 0x9
	.byte	0x4
	.set L$set$99,LCFI52-LCFI51
	.long L$set$99
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
	.set L$set$100,LCFI53-LCFI52
	.long L$set$100
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$101,LEFDE47-LASFDE47
	.long L$set$101
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB32-.
	.set L$set$102,LFE32-LFB32
	.quad L$set$102
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$103,LEFDE49-LASFDE49
	.long L$set$103
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB33-.
	.set L$set$104,LFE33-LFB33
	.quad L$set$104
	.uleb128 0
	.byte	0x4
	.set L$set$105,LCFI54-LFB33
	.long L$set$105
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$106,LCFI55-LCFI54
	.long L$set$106
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$107,LCFI56-LCFI55
	.long L$set$107
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$108,LCFI57-LCFI56
	.long L$set$108
	.byte	0xb
	.byte	0x4
	.set L$set$109,LCFI58-LCFI57
	.long L$set$109
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$110,LCFI59-LCFI58
	.long L$set$110
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$111,LEFDE51-LASFDE51
	.long L$set$111
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB34-.
	.set L$set$112,LFE34-LFB34
	.quad L$set$112
	.uleb128 0
	.byte	0x4
	.set L$set$113,LCFI60-LFB34
	.long L$set$113
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$114,LCFI61-LCFI60
	.long L$set$114
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$115,LCFI62-LCFI61
	.long L$set$115
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$116,LCFI63-LCFI62
	.long L$set$116
	.byte	0xb
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$117,LEFDE53-LASFDE53
	.long L$set$117
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB35-.
	.set L$set$118,LFE35-LFB35
	.quad L$set$118
	.uleb128 0
	.byte	0x4
	.set L$set$119,LCFI64-LFB35
	.long L$set$119
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$120,LCFI65-LCFI64
	.long L$set$120
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$121,LEFDE55-LASFDE55
	.long L$set$121
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB36-.
	.set L$set$122,LFE36-LFB36
	.quad L$set$122
	.uleb128 0
	.byte	0x4
	.set L$set$123,LCFI66-LFB36
	.long L$set$123
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$124,LCFI67-LCFI66
	.long L$set$124
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$125,LCFI68-LCFI67
	.long L$set$125
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$126,LCFI69-LCFI68
	.long L$set$126
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$127,LEFDE57-LASFDE57
	.long L$set$127
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB37-.
	.set L$set$128,LFE37-LFB37
	.quad L$set$128
	.uleb128 0
	.byte	0x4
	.set L$set$129,LCFI70-LFB37
	.long L$set$129
	.byte	0xe
	.uleb128 0x90
	.byte	0x9d
	.uleb128 0x12
	.byte	0x9e
	.uleb128 0x11
	.byte	0x4
	.set L$set$130,LCFI71-LCFI70
	.long L$set$130
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$131,LCFI72-LCFI71
	.long L$set$131
	.byte	0x93
	.uleb128 0x10
	.byte	0x94
	.uleb128 0xf
	.byte	0x4
	.set L$set$132,LCFI73-LCFI72
	.long L$set$132
	.byte	0x95
	.uleb128 0xe
	.byte	0x96
	.uleb128 0xd
	.byte	0x4
	.set L$set$133,LCFI74-LCFI73
	.long L$set$133
	.byte	0x97
	.uleb128 0xc
	.byte	0x98
	.uleb128 0xb
	.byte	0x4
	.set L$set$134,LCFI75-LCFI74
	.long L$set$134
	.byte	0x99
	.uleb128 0xa
	.byte	0x9a
	.uleb128 0x9
	.byte	0x4
	.set L$set$135,LCFI76-LCFI75
	.long L$set$135
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
	.set L$set$136,LCFI77-LCFI76
	.long L$set$136
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$137,LEFDE59-LASFDE59
	.long L$set$137
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB38-.
	.set L$set$138,LFE38-LFB38
	.quad L$set$138
	.uleb128 0
	.byte	0x4
	.set L$set$139,LCFI78-LFB38
	.long L$set$139
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$140,LCFI79-LCFI78
	.long L$set$140
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$141,LCFI80-LCFI79
	.long L$set$141
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$142,LEFDE61-LASFDE61
	.long L$set$142
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB39-.
	.set L$set$143,LFE39-LFB39
	.quad L$set$143
	.uleb128 0
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$144,LEFDE63-LASFDE63
	.long L$set$144
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB40-.
	.set L$set$145,LFE40-LFB40
	.quad L$set$145
	.uleb128 0
	.byte	0x4
	.set L$set$146,LCFI81-LFB40
	.long L$set$146
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$147,LCFI82-LCFI81
	.long L$set$147
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$148,LEFDE65-LASFDE65
	.long L$set$148
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB41-.
	.set L$set$149,LFE41-LFB41
	.quad L$set$149
	.uleb128 0
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$150,LEFDE67-LASFDE67
	.long L$set$150
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB0-.
	.set L$set$151,LFE0-LFB0
	.quad L$set$151
	.uleb128 0
	.align	3
LEFDE67:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
