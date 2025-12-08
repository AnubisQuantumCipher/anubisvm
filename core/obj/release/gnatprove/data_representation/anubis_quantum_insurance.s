	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
_anubis_quantum_insurance__init_quantum_insurance___wrapped_statements.0:
LFB10:
	adrp	x0, lC2@PAGE
	movi	v30.4s, 0
	movi	v31.4s, 0
	sub	sp, sp, #256
LCFI0:
	mov	x4, 256
	ldr	q29, [x0, #lC2@PAGEOFF]
	movk	x4, 0x644, lsl 16
	ldr	x0, [x16, 8]
	add	x3, x0, 49
	add	x2, x0, 81
	str	q29, [x0]
	str	xzr, [x0, 16]
	add	x1, x0, 113
	strb	wzr, [x0, 24]
	str	q30, [x0, 32]
	strb	wzr, [x0, 48]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	str	q31, [x0, 113]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	str	q31, [x0, 225]
	ldr	x0, [x16, 8]
	add	x6, x0, 241
	str	q31, [x0, 241]
	add	x1, x0, 32
	add	x2, x0, 336
	add	x3, x0, 512
	str	q31, [x6, 16]
	add	x5, x0, 744
	str	q31, [x6, 30]
	ldr	q6, [x0, 32]
	strb	wzr, [x0, 288]
	ldp	q16, q7, [x1, 16]
	ldp	q18, q17, [x1, 48]
	ldp	q20, q19, [x1, 80]
	ldp	q22, q21, [x1, 112]
	ldp	q24, q23, [x1, 144]
	ldp	q26, q25, [x1, 176]
	ldp	q28, q27, [x1, 208]
	ldr	q29, [x1, 240]
	str	xzr, [x0, 296]
	stp	q31, q31, [x0, 304]
	str	q6, [x0, 336]
	stp	q16, q7, [x2, 16]
	stp	q18, q17, [x2, 48]
	stp	q20, q19, [x2, 80]
	stp	q22, q21, [x2, 112]
	stp	q24, q23, [x2, 144]
	stp	q26, q25, [x2, 176]
	stp	q28, q27, [x2, 208]
	ldr	x2, [x16]
	str	q29, [x0, 576]
	str	xzr, [x0, 592]
	str	q31, [x3, 88]
	str	q31, [x3, 104]
	add	x1, x2, x4
	str	q31, [x3, 120]
	str	q31, [x3, 136]
	str	q30, [x3, 152]
	str	wzr, [x0, 680]
	str	q30, [x0, 688]
	str	xzr, [x0, 704]
	strh	wzr, [x0, 712]
	strb	wzr, [x0, 714]
	str	x2, [x0, 720]
	str	x2, [x0, 728]
	str	x1, [x0, 736]
	stp	xzr, xzr, [x5]
	stp	xzr, xzr, [x5, 16]
	add	sp, sp, 256
LCFI1:
	ret
LFE10:
	.const
	.align	3
lC3:
	.ascii "anubis_quantum_insurance.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_quantum_insurance__serialize_insurance_state__write_u64.7:
LFB34:
	ldp	x7, x1, [x16]
	mov	w2, 8
	mov	w8, 2147483647
	ldp	x6, x1, [x1]
	ldp	w5, w4, [x1]
L11:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	blt	L17
	stp	x29, x30, [sp, -16]!
LCFI2:
	mov	x29, sp
LCFI3:
L12:
	cmp	w5, w1
	bgt	L18
	sxtw	x3, w1
	sub	x3, x3, x7
	strb	w0, [x6, x3]
	lsr	x0, x0, 8
	cmp	w1, w8
	beq	L19
	add	w1, w1, 1
	subs	w2, w2, #1
	str	w1, [x16, 16]
	beq	L20
L8:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	bge	L12
	subs	w2, w2, #1
	bne	L8
L20:
	ldp	x29, x30, [sp], 16
LCFI4:
	ret
	.p2align 2,,3
L17:
	subs	w2, w2, #1
	bne	L11
	ret
L19:
LCFI5:
	adrp	x0, lC3@PAGE
	mov	w1, 444
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L18:
	adrp	x0, lC3@PAGE
	mov	w1, 442
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE34:
	.align	2
	.p2align 5,,15
_anubis_quantum_insurance__deserialize_insurance_state__read_u64.8:
LFB36:
	stp	x29, x30, [sp, -64]!
LCFI6:
	mov	x29, sp
LCFI7:
	stp	x19, x20, [sp, 16]
LCFI8:
	mov	w20, -1
	mov	x19, x16
	stp	x21, x22, [sp, 32]
LCFI9:
	mov	w21, 2147483647
	str	x23, [sp, 48]
LCFI10:
	mov	x23, 0
	.p2align 5,,15
L25:
	ldr	x3, [x19, 8]
	add	w20, w20, 1
	ldr	w0, [x19, 16]
	ldr	x1, [x3, 8]
	ldr	w2, [x1, 4]
	cmp	w2, w0
	blt	L22
	ldr	w1, [x1]
	cmp	w1, w0
	bgt	L28
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
	beq	L29
	add	w1, w1, 1
	str	w1, [x19, 16]
L22:
	cmp	w20, 7
	bne	L25
	mov	x0, x23
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI11:
	ret
L28:
LCFI12:
	adrp	x0, lC3@PAGE
	mov	w1, 495
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L29:
	adrp	x0, lC3@PAGE
	mov	w1, 496
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE36:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__quantum_threat_levelH
_anubis_quantum_insurance__quantum_threat_levelH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L32
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
L31:
	adrp	x3, _quantum_threat_levelG.21@PAGE
	mov	w1, 52429
	add	x3, x3, _quantum_threat_levelG.21@PAGEOFF;
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
L32:
	mov	x2, 0
	mov	x4, 0
	b	L31
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__oracle_attestationIP
_anubis_quantum_insurance__oracle_attestationIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__release_proposalIP
_anubis_quantum_insurance__release_proposalIP:
LFB86:
	ret
LFE86:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__insurance_stateIP
_anubis_quantum_insurance__insurance_stateIP:
LFB88:
	ret
LFE88:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__attest_resultH
_anubis_quantum_insurance__attest_resultH:
LFB6:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L39
	sub	w1, w1, w2
	cmp	w1, 7
	bgt	L40
L39:
	mov	x3, 0
	mov	x0, 0
L37:
	adrp	x2, _attest_resultG.17@PAGE
	mov	w1, 52429
	add	x2, x2, _attest_resultG.17@PAGEOFF;
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
L40:
	ldrb	w3, [x0, 8]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w0, w3, w3, lsl 1
	add	w3, w3, w3, lsl 3
	umull	x2, w0, w1
	umull	x1, w3, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	add	w5, w2, w2, lsl 2
	add	w4, w1, w1, lsl 2
	add	w2, w2, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w0, w0, w2
	sub	w3, w3, w1
	sxtw	x0, w0
	sxtw	x3, w3
	b	L37
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__propose_resultH
_anubis_quantum_insurance__propose_resultH:
LFB7:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L44
	sub	w1, w1, w2
	cmp	w1, 1
	ble	L44
	ldrb	w2, [x0, 2]
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
	b	L42
	.p2align 2,,3
L44:
	mov	x3, 0
	mov	x0, 0
L42:
	adrp	x2, _propose_resultG.13@PAGE
	mov	w1, 43691
	add	x2, x2, _propose_resultG.13@PAGEOFF;
	movk	w1, 0xaaaa, lsl 16
	ldrb	w0, [x2, x0]
	ldrb	w2, [x2, x3]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 1
	sub	w0, w0, w1, lsl 1
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__release_resultH
_anubis_quantum_insurance__release_resultH:
LFB8:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L47
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
L46:
	adrp	x2, _release_resultG.9@PAGE
	mov	w1, 52429
	add	x2, x2, _release_resultG.9@PAGEOFF;
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
L47:
	mov	x3, 0
	mov	x0, 0
	b	L46
LFE8:
	.const
	.align	3
lC4:
	.ascii "anubis_quantum_insurance.ads"
	.space 1
	.align	3
lC5:
	.ascii "failed postcondition from anubis_quantum_insurance.ads:161"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__init_quantum_insurance
_anubis_quantum_insurance__init_quantum_insurance:
LFB9:
	stp	x29, x30, [sp, -48]!
LCFI13:
	mov	x29, sp
LCFI14:
	add	x16, x29, 24
	mov	x7, x0
	mov	x8, x1
	stp	x1, x0, [x29, 24]
	bl	_anubis_quantum_insurance__init_quantum_insurance___wrapped_statements.0
	ldrb	w0, [x7, 24]
	cmp	w0, 4
	bhi	L52
	ldr	x4, [x7, 736]
	mov	x1, 256
	cmp	w0, 0
	ldp	x3, x2, [x7]
	movk	x1, 0x644, lsl 16
	add	x8, x8, x1
	mov	x1, 61568
	movk	x1, 0x2fa, lsl 16
	ccmp	x4, x8, 0, eq
	cset	w0, ne
	cmp	x3, x1
	ccmp	x2, x1, 0, eq
	cset	w1, ne
	orr	w0, w0, w1
	cbnz	w0, L53
	ldp	x29, x30, [sp], 48
LCFI15:
	ret
L52:
LCFI16:
	adrp	x0, lC4@PAGE
	mov	w1, 163
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L53:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE9:
	.const
	.align	2
lC0:
	.word	1
	.word	58
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__record_attestation
_anubis_quantum_insurance__record_attestation:
LFB11:
	stp	x29, x30, [sp, -16]!
LCFI17:
	mov	x3, x0
	mov	x29, sp
LCFI18:
	ldrb	w0, [x1, 16]
	cmp	w0, 4
	bhi	L56
	ldrb	w2, [x3, 24]
	cmp	w2, 4
	bhi	L56
	cmp	w0, w2
	bcc	L59
	ldp	q26, q28, [x1]
	add	x2, x3, 32
	ldp	q27, q30, [x1, 32]
	ldp	q29, q31, [x1, 64]
	str	q26, [x3, 32]
	stp	q28, q27, [x2, 16]
	stp	q30, q29, [x2, 48]
	str	q31, [x3, 112]
	ldp	q29, q28, [x1, 128]
	ldp	q31, q30, [x1, 160]
	ldp	q27, q26, [x1, 96]
	stp	q29, q28, [x2, 128]
	stp	q31, q30, [x2, 160]
	stp	q27, q26, [x2, 96]
	ldp	q29, q28, [x1, 192]
	ldp	q31, q30, [x1, 224]
	stp	q29, q28, [x2, 192]
	stp	q31, q30, [x2, 224]
	ldrb	w1, [x1, 16]
	cmp	w1, 4
	bhi	L61
	mov	w0, 0
	strb	w1, [x3, 24]
	ldp	x29, x30, [sp], 16
LCFI19:
	ret
	.p2align 2,,3
L59:
LCFI20:
	mov	w0, 3
	ldp	x29, x30, [sp], 16
LCFI21:
	ret
L56:
LCFI22:
	adrp	x0, lC3@PAGE
	mov	w1, 78
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L61:
	adrp	x0, lC3@PAGE
	mov	w1, 85
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE11:
	.const
	.align	3
lC6:
	.ascii "failed precondition from anubis_quantum_insurance.ads:184"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__verify_oracle_signature
_anubis_quantum_insurance__verify_oracle_signature:
LFB12:
	ldpsw	x1, x2, [x2]
	add	x1, x1, 2591
	cmp	x1, x2
	bne	L71
	add	x1, x0, 17
	add	x2, x0, 49
	b	L65
	.p2align 2,,3
L72:
	add	x1, x1, 1
	cmp	x1, x2
	beq	L64
L65:
	ldrb	w0, [x1]
	cbz	w0, L72
	mov	w0, 1
L64:
	ret
L71:
	adrp	x0, lC6@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI23:
	add	x0, x0, lC6@PAGEOFF;
	mov	x29, sp
LCFI24:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE12:
	.const
	.align	2
lC1:
	.word	1
	.word	57
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__get_threat_level
_anubis_quantum_insurance__get_threat_level:
LFB13:
	ldrb	w0, [x0, 24]
	cmp	w0, 4
	bhi	L78
	ret
L78:
	adrp	x0, lC3@PAGE
	stp	x29, x30, [sp, -16]!
LCFI25:
	mov	w1, 111
	mov	x29, sp
LCFI26:
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.const
	.align	3
lC7:
	.ascii "failed precondition from anubis_quantum_insurance.ads:208"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__propose_release
_anubis_quantum_insurance__propose_release:
LFB15:
	stp	x29, x30, [sp, -16]!
LCFI27:
	mov	x29, sp
LCFI28:
	mov	x11, x0
	ldp	w8, w30, [x2]
	ldp	w9, w12, [x5]
	ldp	w10, w14, [x7]
	sxtw	x2, w8
	sxtw	x17, w30
	add	x13, x2, 31
	sxtw	x5, w9
	cmp	x13, x17
	add	x0, x5, 31
	sxtw	x13, w12
	sxtw	x7, w10
	ccmp	x0, x13, 0, eq
	add	x16, x7, 31
	cset	w0, ne
	sxtw	x15, w14
	cmp	x3, 0
	ccmp	x16, x15, 0, ne
	cset	w16, ne
	orr	w0, w0, w16
	cbnz	w0, L116
	ldrb	w0, [x11, 24]
	cmp	w0, 4
	bhi	L117
	cmp	w0, 2
	bls	L105
	ldrb	w0, [x11, 288]
	cmp	w0, 1
	bhi	L118
	cbnz	w0, L106
	ldr	x0, [x11, 8]
	cmp	x0, x3
	bcc	L107
	ldr	x16, [x29, 16]
	ldr	x0, [x11, 736]
	cmp	x0, x16
	bls	L108
	ldr	x0, [x11, 296]
	add	x0, x0, 1
	str	x0, [x11, 296]
	cmp	w8, w30
	bgt	L84
	sub	x16, x11, x2
	sub	x1, x1, x2
	add	x16, x16, 304
	sub	x2, x2, #1
	.p2align 5,,15
L90:
	add	x2, x2, 1
	subs	w0, w2, w8
	bvs	L86
	cmp	w0, 31
	bgt	L88
	bhi	L119
	ldrb	w0, [x1, x2]
	strb	w0, [x16, x2]
L88:
	cmp	x17, x2
	bne	L90
L84:
	add	x0, x11, 32
	ldr	q29, [x11, 32]
	add	x1, x11, 336
	ldp	q28, q31, [x0, 16]
	ldp	q30, q27, [x0, 48]
	ldr	q26, [x0, 80]
	str	q29, [x11, 336]
	ldr	q29, [x0, 96]
	str	q28, [x11, 352]
	ldr	q28, [x0, 112]
	str	q31, [x11, 368]
	ldr	q31, [x0, 128]
	str	q30, [x11, 384]
	ldr	q30, [x0, 144]
	str	q27, [x11, 400]
	ldr	q27, [x0, 160]
	str	q26, [x11, 416]
	ldr	q26, [x0, 176]
	str	q29, [x11, 432]
	ldr	q29, [x0, 192]
	str	q28, [x11, 448]
	ldr	q28, [x0, 208]
	str	q31, [x11, 464]
	ldr	q31, [x0, 224]
	str	q30, [x11, 480]
	ldr	q30, [x0, 240]
	stp	q27, q26, [x1, 160]
	stp	q29, q28, [x1, 192]
	stp	q31, q30, [x1, 224]
	str	x3, [x11, 592]
	cmp	w9, w12
	bgt	L91
	sub	x2, x11, x5
	sub	x4, x4, x5
	add	x2, x2, 600
	sub	x1, x5, #1
	.p2align 5,,15
L97:
	add	x1, x1, 1
	subs	w0, w1, w9
	bvs	L93
	cmp	w0, 31
	bgt	L95
	bhi	L120
	ldrb	w0, [x4, x1]
	strb	w0, [x2, x1]
L95:
	cmp	x13, x1
	bne	L97
L91:
	cmp	w10, w14
	bgt	L98
	sub	x2, x11, x7
	sub	x6, x6, x7
	add	x2, x2, 632
	sub	x1, x7, #1
	.p2align 5,,15
L104:
	add	x1, x1, 1
	subs	w0, w1, w10
	bvs	L100
	cmp	w0, 31
	bgt	L102
	bhi	L121
	ldrb	w0, [x6, x1]
	strb	w0, [x2, x1]
L102:
	cmp	x15, x1
	bne	L104
L98:
	ldr	x0, [x29, 16]
	add	x4, x11, 512
	mov	w3, 1
	ldr	x1, [x29, 16]
	stp	xzr, xzr, [x4, 152]
	add	x2, x0, 602112
	mov	w0, 0
	strb	w3, [x11, 288]
	add	x2, x2, 2688
	ldr	x3, [x29, 16]
	add	x1, x1, 802816
	add	x1, x1, 3584
	str	wzr, [x11, 680]
	str	x2, [x11, 696]
	str	x1, [x11, 704]
	strh	wzr, [x11, 712]
	str	x3, [x11, 688]
	strb	wzr, [x11, 714]
	str	x3, [x11, 728]
	ldp	x29, x30, [sp], 16
LCFI29:
	ret
	.p2align 2,,3
L105:
LCFI30:
	mov	w0, 1
L82:
	ldp	x29, x30, [sp], 16
LCFI31:
	ret
	.p2align 2,,3
L108:
LCFI32:
	mov	w0, 5
	b	L82
	.p2align 2,,3
L106:
	mov	w0, 2
	ldp	x29, x30, [sp], 16
LCFI33:
	ret
	.p2align 2,,3
L107:
LCFI34:
	mov	w0, 3
	ldp	x29, x30, [sp], 16
LCFI35:
	ret
L86:
LCFI36:
	adrp	x0, lC3@PAGE
	mov	w1, 157
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L100:
	adrp	x0, lC3@PAGE
	mov	w1, 176
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L93:
	adrp	x0, lC3@PAGE
	mov	w1, 168
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L120:
	adrp	x0, lC3@PAGE
	mov	w1, 169
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L119:
	adrp	x0, lC3@PAGE
	mov	w1, 158
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L117:
	adrp	x0, lC3@PAGE
	mov	w1, 129
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L116:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L121:
	adrp	x0, lC3@PAGE
	mov	w1, 178
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L118:
	adrp	x0, lC3@PAGE
	mov	w1, 135
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE15:
	.const
	.align	3
lC8:
	.ascii "failed precondition from anubis_quantum_insurance.ads:223"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__vote_release
_anubis_quantum_insurance__vote_release:
LFB16:
	stp	x29, x30, [sp, -16]!
LCFI37:
	mov	x1, x0
	mov	x29, sp
LCFI38:
	ldrb	w0, [x0, 288]
	ldp	w2, w6, [x2]
	cmp	w0, 1
	bhi	L130
	sxtw	x2, w2
	sxtw	x6, w6
	add	x2, x2, 31
	cmp	x3, 0
	ccmp	x2, x6, 0, ne
	eor	w2, w0, 1
	cset	w0, ne
	orr	w0, w0, w2
	tbnz	x0, 0, L131
	ldr	x2, [x1, 696]
	cmp	x2, x5
	bcc	L125
	cmp	w4, 1
	bhi	L132
	cbz	w4, L127
	ldr	x0, [x1, 664]
	add	x0, x0, x3
	str	x0, [x1, 664]
L128:
	ldr	w2, [x1, 680]
	mov	w0, 1
	add	w2, w2, w0
	str	w2, [x1, 680]
L125:
	ldp	x29, x30, [sp], 16
LCFI39:
	ret
	.p2align 2,,3
L127:
LCFI40:
	ldr	x0, [x1, 672]
	add	x0, x0, x3
	str	x0, [x1, 672]
	b	L128
L131:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L130:
	adrp	x0, lC4@PAGE
	mov	w1, 225
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L132:
	adrp	x0, lC3@PAGE
	mov	w1, 217
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE16:
	.const
	.align	3
lC9:
	.ascii "failed precondition from anubis_quantum_insurance.ads:235"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__finalize_vote
_anubis_quantum_insurance__finalize_vote:
LFB17:
	stp	x29, x30, [sp, -16]!
LCFI41:
	mov	x3, x0
	mov	x29, sp
LCFI42:
	ldrb	w0, [x0, 288]
	cmp	w0, 1
	bhi	L141
	cbz	w0, L142
	ldr	x0, [x3, 696]
	cmp	x0, x2
	bls	L143
L136:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI43:
	ret
	.p2align 2,,3
L143:
LCFI44:
	mov	x5, 2500
	mov	x4, 22859
	ldr	x2, [x3, 664]
	movk	x4, 0x3886, lsl 16
	ldr	x0, [x3, 672]
	mul	x1, x1, x5
	movk	x4, 0xc5d6, lsl 32
	movk	x4, 0x346d, lsl 48
	umulh	x1, x1, x4
	add	x0, x2, x0
	cmp	x0, x1, lsr 11
	bcc	L139
	mov	x1, 6700
	mul	x0, x0, x1
	umulh	x0, x0, x4
	cmp	x2, x0, lsr 11
	bcc	L139
	mov	w1, 1
	mov	w0, 1
	strb	w1, [x3, 712]
	ldp	x29, x30, [sp], 16
LCFI45:
	ret
	.p2align 2,,3
L139:
LCFI46:
	strb	wzr, [x3, 288]
	strb	wzr, [x3, 712]
	b	L136
L141:
	adrp	x0, lC4@PAGE
	mov	w1, 235
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L142:
	adrp	x0, lC9@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE17:
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_quantum_insurance.ads:244"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__execute_release
_anubis_quantum_insurance__execute_release:
LFB18:
	stp	x29, x30, [sp, -16]!
LCFI47:
	mov	x2, x0
	mov	x29, sp
LCFI48:
	ldrb	w0, [x0, 288]
	cmp	w0, 1
	bhi	L154
	ldrb	w3, [x2, 712]
	cmp	w3, 1
	bhi	L155
	eor	w0, w0, 1
	eor	w3, w3, 1
	orr	w0, w0, w3
	tbnz	x0, 0, L156
	ldr	x0, [x2, 704]
	cmp	x0, x1
	bhi	L150
	ldrb	w1, [x2, 713]
	cmp	w1, 1
	bhi	L157
	mov	w0, 3
	cbnz	w1, L148
	ldr	x1, [x2, 8]
	mov	w0, 4
	ldr	x3, [x2, 592]
	cmp	x3, x1
	bhi	L148
	ldr	x4, [x2, 16]
	sub	x1, x1, x3
	mov	w5, 1
	mov	w0, 0
	strb	wzr, [x2, 288]
	strb	w5, [x2, 713]
	add	x3, x4, x3
	stp	x1, x3, [x2, 8]
L148:
	ldp	x29, x30, [sp], 16
LCFI49:
	ret
	.p2align 2,,3
L150:
LCFI50:
	mov	w0, 2
	ldp	x29, x30, [sp], 16
LCFI51:
	ret
L156:
LCFI52:
	adrp	x0, lC10@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L155:
	adrp	x0, lC4@PAGE
	mov	w1, 245
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L154:
	adrp	x0, lC4@PAGE
	mov	w1, 244
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L157:
	adrp	x0, lC3@PAGE
	mov	w1, 293
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE18:
	.const
	.align	3
lC11:
	.ascii "failed precondition from anubis_quantum_insurance.ads:260"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__is_builder
_anubis_quantum_insurance__is_builder:
LFB19:
	stp	x29, x30, [sp, -16]!
LCFI53:
	mov	x29, sp
LCFI54:
	mov	x8, x0
	ldp	w4, w7, [x1]
	ldp	w9, w3, [x3]
	sxtw	x1, w4
	sxtw	x5, w7
	add	x0, x1, 31
	sxtw	x6, w9
	cmp	x0, x5
	add	x10, x6, 31
	sxtw	x0, w3
	ccmp	x10, x0, 0, eq
	bne	L171
	cset	w0, ne
	cmp	w4, w7
	bgt	L167
	add	x6, x6, x1
	sub	x8, x8, x1
	sub	x7, x2, x6
	sub	x1, x1, #1
	.p2align 5,,15
L166:
	add	x1, x1, 1
	subs	w2, w1, w4
	bvs	L162
	cmp	w3, w2
	blt	L164
	cmp	w9, w2
	bgt	L172
	ldrb	w6, [x8, x1]
	ldrb	w2, [x7, x1]
	cmp	w6, w2
	bne	L160
L164:
	cmp	x5, x1
	bne	L166
L167:
	mov	w0, 1
L160:
	ldp	x29, x30, [sp], 16
LCFI55:
	ret
L162:
LCFI56:
	adrp	x0, lC3@PAGE
	mov	w1, 326
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L172:
	adrp	x0, lC3@PAGE
	mov	w1, 327
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L171:
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE19:
	.const
	.align	3
lC12:
	.ascii "failed precondition from anubis_quantum_insurance.ads:269"
	.align	3
lC13:
	.ascii "failed postcondition from anubis_quantum_insurance.ads:271"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__is_valid_recipient
_anubis_quantum_insurance__is_valid_recipient:
LFB20:
	stp	x29, x30, [sp, -48]!
LCFI57:
	mov	x29, sp
LCFI58:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI59:
	ldpsw	x5, x7, [x1]
	ldpsw	x4, x6, [x3]
	add	x5, x5, 31
	cmp	x5, x7
	add	x4, x4, 31
	ccmp	x4, x6, 0, eq
	bne	L177
	mov	x19, x1
	mov	x20, x3
	mov	x21, x0
	mov	x22, x2
	bl	_anubis_quantum_insurance__is_builder
	eor	w4, w0, 1
	mov	x1, x19
	mov	x2, x22
	mov	x3, x20
	mov	x0, x21
	and	w19, w4, 255
	bl	_anubis_quantum_insurance__is_builder
	eor	w1, w0, 1
	and	w0, w1, 255
	cmp	w19, w1, uxtb
	bne	L178
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI60:
	ret
L177:
LCFI61:
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L178:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE20:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__verify_lock_integrity
_anubis_quantum_insurance__verify_lock_integrity:
LFB22:
	ldr	x2, [x0, 16]
	ldp	x0, x1, [x0]
	sub	x0, x0, x2
	cmp	x0, x1
	cset	w0, eq
	ret
LFE22:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__is_sunset
_anubis_quantum_insurance__is_sunset:
LFB23:
	ldr	x0, [x0, 736]
	cmp	x1, x0
	cset	w0, cs
	ret
LFE23:
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_quantum_insurance.ads:300"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__handle_sunset
_anubis_quantum_insurance__handle_sunset:
LFB25:
	mov	x1, x0
	ldpsw	x0, x4, [x2]
	ldr	x2, [x1, 736]
	add	x0, x0, 31
	cmp	x0, x4
	ccmp	x2, x3, 2, eq
	bhi	L186
	ldp	x3, x2, [x1, 8]
	mov	w0, 1
	add	x2, x2, x3
	stp	xzr, x2, [x1, 8]
	ret
L186:
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI62:
	add	x0, x0, lC14@PAGEOFF;
	mov	x29, sp
LCFI63:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE25:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__get_balance
_anubis_quantum_insurance__get_balance:
LFB26:
	ldr	x0, [x0, 8]
	ret
LFE26:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__get_total_released
_anubis_quantum_insurance__get_total_released:
LFB28:
	ldr	x0, [x0, 16]
	ret
LFE28:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__blocks_until_sunset
_anubis_quantum_insurance__blocks_until_sunset:
LFB30:
	ldr	x0, [x0, 736]
	subs	x0, x0, x1
	csel	x0, x0, xzr, hi
	ret
LFE30:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__can_release
_anubis_quantum_insurance__can_release:
LFB31:
	ldrb	w0, [x0, 24]
	cmp	w0, 4
	bhi	L197
	cmp	w0, 2
	cset	w0, hi
	ret
L197:
	adrp	x0, lC3@PAGE
	stp	x29, x30, [sp, -16]!
LCFI64:
	mov	w1, 423
	mov	x29, sp
LCFI65:
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE31:
	.const
	.align	3
lC15:
	.ascii "failed precondition from anubis_quantum_insurance.ads:346"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__serialize_insurance_state
_anubis_quantum_insurance__serialize_insurance_state:
LFB33:
	stp	x29, x30, [sp, -96]!
LCFI66:
	mov	x29, sp
LCFI67:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
LCFI68:
	mov	x19, x0
	add	x0, x29, 96
	stp	x21, x22, [sp, 32]
LCFI69:
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w22, w3, [x2]
	str	x0, [x29, 88]
	sxtw	x20, w22
	sxtw	x2, w3
	add	x0, x20, 1022
	str	x20, [x29, 64]
	cmp	x0, x2
	bge	L225
	tbnz	w22, #31, L226
	ldr	x0, [x29, 48]
	mov	w1, 0
	cmp	w22, w3
	add	x21, x29, 64
	sub	x2, x2, x20
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldr	x0, [x19]
	mov	x16, x21
	str	w22, [x29, 80]
	bl	_anubis_quantum_insurance__serialize_insurance_state__write_u64.7
	ldr	x0, [x19, 8]
	mov	x16, x21
	bl	_anubis_quantum_insurance__serialize_insurance_state__write_u64.7
	ldr	x0, [x19, 16]
	mov	x16, x21
	bl	_anubis_quantum_insurance__serialize_insurance_state__write_u64.7
	ldr	w1, [x29, 80]
	tbnz	w1, #31, L227
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L203
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L228
	ldrb	w2, [x19, 24]
	cmp	w2, 4
	bhi	L229
	ldr	x4, [x29, 48]
	sxtw	x0, w1
	mov	w3, 2147483647
	sub	x0, x0, x20
	strb	w2, [x4, x0]
	cmp	w1, w3
	beq	L230
	add	w1, w1, 1
L203:
	ldr	x0, [x19, 720]
	mov	x16, x21
	str	w1, [x29, 80]
	bl	_anubis_quantum_insurance__serialize_insurance_state__write_u64.7
	ldr	x0, [x19, 728]
	mov	x16, x21
	bl	_anubis_quantum_insurance__serialize_insurance_state__write_u64.7
	ldr	x0, [x19, 736]
	mov	x16, x21
	bl	_anubis_quantum_insurance__serialize_insurance_state__write_u64.7
	ldr	w1, [x29, 80]
	tbnz	w1, #31, L231
	ldr	x0, [x29, 56]
	ldp	w6, w4, [x0]
	cmp	w4, w1
	bge	L208
	ldr	x7, [x29, 48]
L209:
	add	x2, x19, 744
	add	x0, x19, 776
	mov	w8, 2147483647
	.p2align 5,,15
L216:
	cmp	w4, w1
	blt	L213
	cmp	w1, w6
	blt	L232
	ldrb	w5, [x2]
	sxtw	x3, w1
	sub	x3, x3, x20
	strb	w5, [x7, x3]
	cmp	w1, w8
	beq	L233
	add	w1, w1, 1
L213:
	add	x2, x2, 1
	cmp	x0, x2
	bne	L216
	subs	w0, w1, w6
	bvs	L218
	tbnz	w0, #31, L234
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI70:
	ret
	.p2align 2,,3
L208:
LCFI71:
	cmp	w6, w1
	bgt	L235
	ldrb	w2, [x19, 288]
	cmp	w2, 1
	bhi	L236
	ldr	x7, [x29, 48]
	sxtw	x0, w1
	mov	w3, 2147483647
	sub	x0, x0, x20
	strb	w2, [x7, x0]
	cmp	w1, w3
	beq	L237
	add	w1, w1, 1
	b	L209
L232:
	adrp	x0, lC3@PAGE
	mov	w1, 475
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L233:
	adrp	x0, lC3@PAGE
	mov	w1, 476
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L237:
	adrp	x0, lC3@PAGE
	mov	w1, 469
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L231:
	adrp	x0, lC3@PAGE
	mov	w1, 467
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L230:
	adrp	x0, lC3@PAGE
	mov	w1, 459
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L234:
	adrp	x0, lC3@PAGE
	mov	w1, 480
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L236:
	adrp	x0, lC3@PAGE
	mov	w1, 468
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L226:
	adrp	x0, lC3@PAGE
	mov	w1, 435
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L228:
	adrp	x0, lC3@PAGE
	mov	w1, 458
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L227:
	adrp	x0, lC3@PAGE
	mov	w1, 457
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L235:
	adrp	x0, lC3@PAGE
	mov	w1, 468
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L218:
	adrp	x0, lC3@PAGE
	mov	w1, 480
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L225:
	adrp	x0, lC15@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L229:
	adrp	x0, lC3@PAGE
	mov	w1, 458
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE33:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__deserialize_insurance_state
_anubis_quantum_insurance__deserialize_insurance_state:
LFB35:
	stp	x29, x30, [sp, -96]!
LCFI72:
	mov	x29, sp
LCFI73:
	add	x3, x29, 48
	stp	x0, x1, [x29, 48]
	add	x0, x29, 96
	stp	x19, x20, [sp, 16]
LCFI74:
	ldr	w20, [x1]
	str	x21, [sp, 32]
LCFI75:
	str	x3, [x29, 72]
	str	x0, [x29, 88]
	tbnz	w20, #31, L258
	mov	x0, x2
	mov	x1, 0
	mov	x19, x2
	bl	_anubis_quantum_insurance__init_quantum_insurance
	ldr	x2, [x29, 56]
	mov	w0, 0
	ldpsw	x1, x2, [x2]
	add	x1, x1, 56
	cmp	x1, x2
	blt	L259
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI76:
	ret
	.p2align 2,,3
L259:
LCFI77:
	add	x21, x29, 64
	sxtw	x0, w20
	str	w20, [x29, 80]
	mov	x16, x21
	str	x0, [x29, 64]
	bl	_anubis_quantum_insurance__deserialize_insurance_state__read_u64.8
	mov	x16, x21
	str	x0, [x19]
	bl	_anubis_quantum_insurance__deserialize_insurance_state__read_u64.8
	mov	x16, x21
	str	x0, [x19, 8]
	bl	_anubis_quantum_insurance__deserialize_insurance_state__read_u64.8
	ldr	w1, [x29, 80]
	str	x0, [x19, 16]
	ldr	x3, [x29, 64]
	tbnz	w1, #31, L260
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L242
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L261
	ldr	x2, [x29, 48]
	sxtw	x0, w1
	sub	x0, x0, x3
	ldrb	w0, [x2, x0]
	cmp	w0, 4
	bls	L262
L244:
	mov	w0, 2147483647
	cmp	w1, w0
	beq	L263
	add	w1, w1, 1
L242:
	mov	x16, x21
	str	w1, [x29, 80]
	bl	_anubis_quantum_insurance__deserialize_insurance_state__read_u64.8
	mov	x16, x21
	str	x0, [x19, 720]
	bl	_anubis_quantum_insurance__deserialize_insurance_state__read_u64.8
	mov	x16, x21
	str	x0, [x19, 728]
	bl	_anubis_quantum_insurance__deserialize_insurance_state__read_u64.8
	ldr	w1, [x29, 80]
	str	x0, [x19, 736]
	ldr	x7, [x29, 64]
	tbnz	w1, #31, L264
	ldr	x0, [x29, 56]
	ldp	w6, w4, [x0]
	cmp	w4, w1
	bge	L247
	ldr	x5, [x29, 48]
L248:
	add	x3, x19, 744
	add	x2, x19, 776
	mov	w8, 2147483647
	.p2align 5,,15
L254:
	cmp	w4, w1
	blt	L251
	cmp	w1, w6
	blt	L265
	sxtw	x0, w1
	sub	x0, x0, x7
	ldrb	w0, [x5, x0]
	strb	w0, [x3]
	cmp	w1, w8
	beq	L266
	add	w1, w1, 1
L251:
	add	x3, x3, 1
	cmp	x2, x3
	bne	L254
	mov	w0, 1
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI78:
	ret
	.p2align 2,,3
L247:
LCFI79:
	cmp	w6, w1
	bgt	L267
	ldr	x5, [x29, 48]
	sxtw	x0, w1
	mov	w2, 2147483647
	sub	x0, x0, x7
	ldrb	w0, [x5, x0]
	cmp	w0, 0
	cset	w0, ne
	strb	w0, [x19, 288]
	cmp	w1, w2
	beq	L268
	add	w1, w1, 1
	b	L248
	.p2align 2,,3
L262:
	strb	w0, [x19, 24]
	b	L244
L265:
	adrp	x0, lC3@PAGE
	mov	w1, 537
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L266:
	adrp	x0, lC3@PAGE
	mov	w1, 538
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L267:
	adrp	x0, lC3@PAGE
	mov	w1, 530
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L261:
	adrp	x0, lC3@PAGE
	mov	w1, 515
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L268:
	adrp	x0, lC3@PAGE
	mov	w1, 531
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L258:
	adrp	x0, lC3@PAGE
	mov	w1, 488
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L260:
	adrp	x0, lC3@PAGE
	mov	w1, 514
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L263:
	adrp	x0, lC3@PAGE
	mov	w1, 521
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L264:
	adrp	x0, lC3@PAGE
	mov	w1, 529
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE35:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__zeroize_insurance_state
_anubis_quantum_insurance__zeroize_insurance_state:
LFB37:
	movi	v30.4s, 0
	mov	x4, x0
	movi	v31.4s, 0
	add	x12, x0, 49
	str	xzr, [x0, 16]
	add	x11, x0, 81
	strb	wzr, [x0, 24]
	add	x3, x0, 113
	add	x10, x0, 241
	add	x9, x0, 353
	add	x8, x0, 385
	str	q30, [x0]
	add	x2, x0, 417
	add	x7, x0, 545
	str	q30, [x4, 32]!
	add	x6, x0, 600
	add	x5, x0, 632
	strb	wzr, [x4, 16]
	add	x13, x0, 512
	add	x4, x0, 744
	stp	xzr, xzr, [x12]
	stp	xzr, xzr, [x12, 16]
	stp	xzr, xzr, [x11]
	stp	xzr, xzr, [x11, 16]
	str	q31, [x0, 113]
	stp	q31, q31, [x3, 16]
	stp	q31, q31, [x3, 48]
	stp	q31, q31, [x3, 80]
	str	q31, [x0, 225]
	str	q31, [x0, 241]
	str	q31, [x10, 16]
	str	q31, [x10, 30]
	strb	wzr, [x0, 288]
	str	xzr, [x0, 296]
	stp	xzr, xzr, [x0, 304]
	stp	xzr, xzr, [x0, 320]
	str	q30, [x0, 336]
	strb	wzr, [x0, 352]
	stp	xzr, xzr, [x9]
	stp	xzr, xzr, [x9, 16]
	stp	xzr, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
	stp	q31, q31, [x2]
	stp	q31, q31, [x2, 32]
	stp	q31, q31, [x2, 64]
	stp	q31, q31, [x2, 96]
	stp	q31, q31, [x7]
	str	q31, [x7, 30]
	str	xzr, [x0, 592]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	stp	xzr, xzr, [x5]
	stp	xzr, xzr, [x5, 16]
	str	q30, [x13, 152]
	str	wzr, [x0, 680]
	str	q30, [x0, 688]
	str	xzr, [x0, 704]
	strh	wzr, [x0, 712]
	strb	wzr, [x0, 714]
	str	q30, [x0, 720]
	str	xzr, [x0, 736]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	ret
LFE37:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__zeroize_attestation
_anubis_quantum_insurance__zeroize_attestation:
LFB38:
	movi	v31.4s, 0
	add	x4, x0, 17
	add	x3, x0, 49
	stp	xzr, xzr, [x0]
	add	x1, x0, 81
	strb	wzr, [x0, 16]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	str	q31, [x0, 81]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	str	q31, [x0, 225]
	str	q31, [x0, 193]
	str	q31, [x0, 209]
	str	q31, [x0, 239]
	ret
LFE38:
	.align	2
	.p2align 5,,15
	.globl _anubis_quantum_insurance__zeroize_proposal
_anubis_quantum_insurance__zeroize_proposal:
LFB39:
	mov	x3, x0
	movi	v30.4s, 0
	mov	x2, x0
	movi	v31.4s, 0
	add	x6, x0, 57
	add	x5, x0, 89
	str	xzr, [x3], 8
	add	x1, x0, 121
	add	x4, x0, 249
	stp	xzr, xzr, [x0, 8]
	add	x7, x0, 512
	stp	xzr, xzr, [x3, 16]
	str	q30, [x2, 40]!
	strb	wzr, [x2, 16]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	stp	xzr, xzr, [x5]
	stp	xzr, xzr, [x5, 16]
	str	q31, [x0, 121]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	str	q31, [x0, 233]
	str	q31, [x0, 249]
	str	q31, [x4, 16]
	str	q31, [x4, 30]
	str	xzr, [x0, 296]
	stp	xzr, xzr, [x0, 304]
	stp	xzr, xzr, [x0, 320]
	stp	xzr, xzr, [x0, 336]
	stp	xzr, xzr, [x0, 352]
	str	q30, [x0, 368]
	str	wzr, [x0, 384]
	str	q30, [x7, -120]
	str	xzr, [x0, 408]
	strh	wzr, [x0, 416]
	strb	wzr, [x0, 418]
	ret
LFE39:
	.const
	.align	3
_release_resultG.9:
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.space 1
	.align	3
_propose_resultG.13:
	.byte	0
	.byte	0
	.byte	2
	.byte	2
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	0
	.space 1
	.align	3
_attest_resultG.17:
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	2
	.byte	0
	.byte	4
	.byte	0
	.space 5
	.align	3
_quantum_threat_levelG.21:
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	3
	.space 5
	.globl _anubis_quantum_insurance__release_resultN
	.align	3
_anubis_quantum_insurance__release_resultN:
	.byte	1
	.byte	9
	.byte	24
	.byte	39
	.byte	55
	.byte	75
	.space 2
	.globl _anubis_quantum_insurance__release_resultS
	.align	3
_anubis_quantum_insurance__release_resultS:
	.ascii "RELEASEDVOTE_NOT_PASSEDTIMELOCK_ACTIVEALREADY_EXECUTEDINSUFFICIENT_BALANCE"
	.globl _anubis_quantum_insurance__propose_resultN
	.align	3
_anubis_quantum_insurance__propose_resultN:
	.byte	1
	.byte	9
	.byte	34
	.byte	56
	.byte	75
	.byte	92
	.byte	106
	.space 1
	.globl _anubis_quantum_insurance__propose_resultS
	.align	3
_anubis_quantum_insurance__propose_resultS:
	.ascii "PROPOSEDTHREAT_LEVEL_INSUFFICIENTACTIVE_PROPOSAL_EXISTSINSURANCE_EXHAUSTEDINVALID_RECIPIENTSUNSET_REACHED"
	.globl _anubis_quantum_insurance__attest_resultN
	.align	3
_anubis_quantum_insurance__attest_resultN:
	.byte	1
	.byte	21
	.byte	35
	.byte	52
	.byte	70
	.byte	80
	.space 2
	.globl _anubis_quantum_insurance__attest_resultS
	.align	3
_anubis_quantum_insurance__attest_resultS:
	.ascii "ATTESTATION_RECORDEDINVALID_ORACLEINVALID_SIGNATURELOWER_THAN_CURRENTTOO_RECENT"
	.globl _anubis_quantum_insurance__quantum_threat_levelN
	.align	3
_anubis_quantum_insurance__quantum_threat_levelN:
	.byte	1
	.byte	5
	.byte	13
	.byte	24
	.byte	32
	.byte	38
	.space 2
	.globl _anubis_quantum_insurance__quantum_threat_levelS
	.align	3
_anubis_quantum_insurance__quantum_threat_levelS:
	.ascii "NONERESEARCHDEVELOPMENTIMMINENTACTIVE"
	.globl _anubis_quantum_insurance__min_release_threat_level
_anubis_quantum_insurance__min_release_threat_level:
	.byte	3
	.globl _anubis_quantum_insurance__builder_access
_anubis_quantum_insurance__builder_access:
	.space 1
	.globl _anubis_quantum_insurance_E
	.data
	.align	1
_anubis_quantum_insurance_E:
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
	.quad	LFB10-.
	.set L$set$2,LFE10-LFB10
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB10
	.long L$set$3
	.byte	0xe
	.uleb128 0x100
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$5,LEFDE3-LASFDE3
	.long L$set$5
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB34-.
	.set L$set$6,LFE34-LFB34
	.quad L$set$6
	.uleb128 0
	.byte	0x4
	.set L$set$7,LCFI2-LFB34
	.long L$set$7
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$8,LCFI3-LCFI2
	.long L$set$8
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$9,LCFI4-LCFI3
	.long L$set$9
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$10,LCFI5-LCFI4
	.long L$set$10
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$11,LEFDE5-LASFDE5
	.long L$set$11
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB36-.
	.set L$set$12,LFE36-LFB36
	.quad L$set$12
	.uleb128 0
	.byte	0x4
	.set L$set$13,LCFI6-LFB36
	.long L$set$13
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$14,LCFI7-LCFI6
	.long L$set$14
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$15,LCFI8-LCFI7
	.long L$set$15
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$16,LCFI9-LCFI8
	.long L$set$16
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$17,LCFI10-LCFI9
	.long L$set$17
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
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
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$20,LEFDE7-LASFDE7
	.long L$set$20
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB2-.
	.set L$set$21,LFE2-LFB2
	.quad L$set$21
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$22,LEFDE9-LASFDE9
	.long L$set$22
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB3-.
	.set L$set$23,LFE3-LFB3
	.quad L$set$23
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$24,LEFDE11-LASFDE11
	.long L$set$24
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB86-.
	.set L$set$25,LFE86-LFB86
	.quad L$set$25
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$26,LEFDE13-LASFDE13
	.long L$set$26
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB88-.
	.set L$set$27,LFE88-LFB88
	.quad L$set$27
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$28,LEFDE15-LASFDE15
	.long L$set$28
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB6-.
	.set L$set$29,LFE6-LFB6
	.quad L$set$29
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$30,LEFDE17-LASFDE17
	.long L$set$30
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB7-.
	.set L$set$31,LFE7-LFB7
	.quad L$set$31
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$32,LEFDE19-LASFDE19
	.long L$set$32
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB8-.
	.set L$set$33,LFE8-LFB8
	.quad L$set$33
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$34,LEFDE21-LASFDE21
	.long L$set$34
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB9-.
	.set L$set$35,LFE9-LFB9
	.quad L$set$35
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI13-LFB9
	.long L$set$36
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$37,LCFI14-LCFI13
	.long L$set$37
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$38,LCFI15-LCFI14
	.long L$set$38
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$39,LCFI16-LCFI15
	.long L$set$39
	.byte	0xb
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$40,LEFDE23-LASFDE23
	.long L$set$40
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB11-.
	.set L$set$41,LFE11-LFB11
	.quad L$set$41
	.uleb128 0
	.byte	0x4
	.set L$set$42,LCFI17-LFB11
	.long L$set$42
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$43,LCFI18-LCFI17
	.long L$set$43
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$44,LCFI19-LCFI18
	.long L$set$44
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$45,LCFI20-LCFI19
	.long L$set$45
	.byte	0xb
	.byte	0x4
	.set L$set$46,LCFI21-LCFI20
	.long L$set$46
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI22-LCFI21
	.long L$set$47
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$48,LEFDE25-LASFDE25
	.long L$set$48
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB12-.
	.set L$set$49,LFE12-LFB12
	.quad L$set$49
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI23-LFB12
	.long L$set$50
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$51,LCFI24-LCFI23
	.long L$set$51
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$52,LEFDE27-LASFDE27
	.long L$set$52
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB13-.
	.set L$set$53,LFE13-LFB13
	.quad L$set$53
	.uleb128 0
	.byte	0x4
	.set L$set$54,LCFI25-LFB13
	.long L$set$54
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$55,LCFI26-LCFI25
	.long L$set$55
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$56,LEFDE29-LASFDE29
	.long L$set$56
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB15-.
	.set L$set$57,LFE15-LFB15
	.quad L$set$57
	.uleb128 0
	.byte	0x4
	.set L$set$58,LCFI27-LFB15
	.long L$set$58
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$59,LCFI28-LCFI27
	.long L$set$59
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$60,LCFI29-LCFI28
	.long L$set$60
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$61,LCFI30-LCFI29
	.long L$set$61
	.byte	0xb
	.byte	0x4
	.set L$set$62,LCFI31-LCFI30
	.long L$set$62
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI32-LCFI31
	.long L$set$63
	.byte	0xb
	.byte	0x4
	.set L$set$64,LCFI33-LCFI32
	.long L$set$64
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI34-LCFI33
	.long L$set$65
	.byte	0xb
	.byte	0x4
	.set L$set$66,LCFI35-LCFI34
	.long L$set$66
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$67,LCFI36-LCFI35
	.long L$set$67
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$68,LEFDE31-LASFDE31
	.long L$set$68
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB16-.
	.set L$set$69,LFE16-LFB16
	.quad L$set$69
	.uleb128 0
	.byte	0x4
	.set L$set$70,LCFI37-LFB16
	.long L$set$70
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$71,LCFI38-LCFI37
	.long L$set$71
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$72,LCFI39-LCFI38
	.long L$set$72
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$73,LCFI40-LCFI39
	.long L$set$73
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$74,LEFDE33-LASFDE33
	.long L$set$74
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB17-.
	.set L$set$75,LFE17-LFB17
	.quad L$set$75
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI41-LFB17
	.long L$set$76
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$77,LCFI42-LCFI41
	.long L$set$77
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$78,LCFI43-LCFI42
	.long L$set$78
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$79,LCFI44-LCFI43
	.long L$set$79
	.byte	0xb
	.byte	0x4
	.set L$set$80,LCFI45-LCFI44
	.long L$set$80
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$81,LCFI46-LCFI45
	.long L$set$81
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$82,LEFDE35-LASFDE35
	.long L$set$82
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB18-.
	.set L$set$83,LFE18-LFB18
	.quad L$set$83
	.uleb128 0
	.byte	0x4
	.set L$set$84,LCFI47-LFB18
	.long L$set$84
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$85,LCFI48-LCFI47
	.long L$set$85
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$86,LCFI49-LCFI48
	.long L$set$86
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$87,LCFI50-LCFI49
	.long L$set$87
	.byte	0xb
	.byte	0x4
	.set L$set$88,LCFI51-LCFI50
	.long L$set$88
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$89,LCFI52-LCFI51
	.long L$set$89
	.byte	0xb
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$90,LEFDE37-LASFDE37
	.long L$set$90
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB19-.
	.set L$set$91,LFE19-LFB19
	.quad L$set$91
	.uleb128 0
	.byte	0x4
	.set L$set$92,LCFI53-LFB19
	.long L$set$92
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$93,LCFI54-LCFI53
	.long L$set$93
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$94,LCFI55-LCFI54
	.long L$set$94
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$95,LCFI56-LCFI55
	.long L$set$95
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$96,LEFDE39-LASFDE39
	.long L$set$96
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB20-.
	.set L$set$97,LFE20-LFB20
	.quad L$set$97
	.uleb128 0
	.byte	0x4
	.set L$set$98,LCFI57-LFB20
	.long L$set$98
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$99,LCFI58-LCFI57
	.long L$set$99
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$100,LCFI59-LCFI58
	.long L$set$100
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$101,LCFI60-LCFI59
	.long L$set$101
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
	.set L$set$102,LCFI61-LCFI60
	.long L$set$102
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$103,LEFDE41-LASFDE41
	.long L$set$103
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB22-.
	.set L$set$104,LFE22-LFB22
	.quad L$set$104
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$105,LEFDE43-LASFDE43
	.long L$set$105
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB23-.
	.set L$set$106,LFE23-LFB23
	.quad L$set$106
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$107,LEFDE45-LASFDE45
	.long L$set$107
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB25-.
	.set L$set$108,LFE25-LFB25
	.quad L$set$108
	.uleb128 0
	.byte	0x4
	.set L$set$109,LCFI62-LFB25
	.long L$set$109
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$110,LCFI63-LCFI62
	.long L$set$110
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$111,LEFDE47-LASFDE47
	.long L$set$111
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB26-.
	.set L$set$112,LFE26-LFB26
	.quad L$set$112
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$113,LEFDE49-LASFDE49
	.long L$set$113
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB28-.
	.set L$set$114,LFE28-LFB28
	.quad L$set$114
	.uleb128 0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$115,LEFDE51-LASFDE51
	.long L$set$115
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB30-.
	.set L$set$116,LFE30-LFB30
	.quad L$set$116
	.uleb128 0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$117,LEFDE53-LASFDE53
	.long L$set$117
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB31-.
	.set L$set$118,LFE31-LFB31
	.quad L$set$118
	.uleb128 0
	.byte	0x4
	.set L$set$119,LCFI64-LFB31
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
	.quad	LFB33-.
	.set L$set$122,LFE33-LFB33
	.quad L$set$122
	.uleb128 0
	.byte	0x4
	.set L$set$123,LCFI66-LFB33
	.long L$set$123
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$124,LCFI67-LCFI66
	.long L$set$124
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$125,LCFI68-LCFI67
	.long L$set$125
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$126,LCFI69-LCFI68
	.long L$set$126
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$127,LCFI70-LCFI69
	.long L$set$127
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
	.set L$set$128,LCFI71-LCFI70
	.long L$set$128
	.byte	0xb
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$129,LEFDE57-LASFDE57
	.long L$set$129
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB35-.
	.set L$set$130,LFE35-LFB35
	.quad L$set$130
	.uleb128 0
	.byte	0x4
	.set L$set$131,LCFI72-LFB35
	.long L$set$131
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$132,LCFI73-LCFI72
	.long L$set$132
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$133,LCFI74-LCFI73
	.long L$set$133
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$134,LCFI75-LCFI74
	.long L$set$134
	.byte	0x95
	.uleb128 0x8
	.byte	0x4
	.set L$set$135,LCFI76-LCFI75
	.long L$set$135
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
	.set L$set$136,LCFI77-LCFI76
	.long L$set$136
	.byte	0xb
	.byte	0x4
	.set L$set$137,LCFI78-LCFI77
	.long L$set$137
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
	.set L$set$138,LCFI79-LCFI78
	.long L$set$138
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$139,LEFDE59-LASFDE59
	.long L$set$139
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB37-.
	.set L$set$140,LFE37-LFB37
	.quad L$set$140
	.uleb128 0
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$141,LEFDE61-LASFDE61
	.long L$set$141
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB38-.
	.set L$set$142,LFE38-LFB38
	.quad L$set$142
	.uleb128 0
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$143,LEFDE63-LASFDE63
	.long L$set$143
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB39-.
	.set L$set$144,LFE39-LFB39
	.quad L$set$144
	.uleb128 0
	.align	3
LEFDE63:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
