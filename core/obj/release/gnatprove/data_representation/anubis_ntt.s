	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC4:
	.ascii "Loop_Invariant failed at anubis_field.adb:136"
	.align	3
lC5:
	.ascii "anubis_field.adb"
	.space 1
	.align	3
lC6:
	.ascii "Loop_Invariant failed at anubis_field.adb:62"
	.text
	.align	2
	.p2align 5,,15
_anubis_ntt__basemul:
LFB10:
	mul	w6, w1, w3
	mov	w5, 3327
	mov	w9, 3329
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	w7, 22
	mov	x29, sp
LCFI1:
	mul	w8, w6, w5
	and	w8, w8, 65535
	madd	w8, w8, w9, w6
	lsr	w8, w8, 16
	mov	w5, w8
	b	L2
	.p2align 2,,3
L3:
	cmp	w8, w5
	bcc	L30
L2:
	sub	w6, w5, #3329
	subs	w7, w7, #1
	eor	w5, w6, w5
	and	w5, w5, w6, asr 31
	eor	w5, w5, w6
	bne	L3
	mov	w6, 65535
	cmp	w5, w6
	bhi	L7
	mul	w4, w4, w5
	mov	w7, 3327
	mov	w5, 3329
	mov	w6, 22
	mul	w7, w4, w7
	and	w7, w7, 65535
	madd	w7, w7, w5, w4
	lsr	w7, w7, 16
	mov	w5, w7
	b	L5
	.p2align 2,,3
L6:
	cmp	w7, w5
	bcc	L30
L5:
	sub	w4, w5, #3329
	subs	w6, w6, #1
	eor	w5, w4, w5
	and	w5, w5, w4, asr 31
	eor	w5, w5, w4
	bne	L6
	mov	w4, 65535
	cmp	w5, w4
	bhi	L7
	mul	w4, w0, w2
	mov	w8, 3327
	mov	w6, 3329
	mov	w7, 22
	mul	w8, w4, w8
	and	w8, w8, 65535
	madd	w8, w8, w6, w4
	lsr	w8, w8, 16
	mov	w4, w8
	b	L8
	.p2align 2,,3
L9:
	cmp	w8, w4
	bcc	L30
L8:
	sub	w6, w4, #3329
	subs	w7, w7, #1
	eor	w4, w6, w4
	and	w4, w4, w6, asr 31
	eor	w4, w4, w6
	bne	L9
	mov	w6, 65535
	cmp	w4, w6
	bhi	L7
	add	w6, w4, w5
	mov	w8, 20158
	add	w4, w4, w5
	mov	w7, 3329
	umull	x5, w6, w8
	mov	w6, 15
	lsr	x5, x5, 26
	umsubl	x4, w5, w7, x4
	mov	w7, w4
	b	L10
	.p2align 2,,3
L11:
	cmp	w7, w4
	bcc	L31
L10:
	sub	w5, w4, #3329
	subs	w6, w6, #1
	eor	w4, w5, w4
	and	w4, w4, w5, asr 31
	eor	w4, w4, w5
	bne	L11
	cmp	w4, 3328
	bhi	L19
	mul	w0, w0, w3
	mov	w6, 3327
	mov	w3, 3329
	and	w4, w4, 65535
	mov	w5, 22
	mul	w6, w0, w6
	and	w6, w6, 65535
	madd	w6, w6, w3, w0
	lsr	w6, w6, 16
	mov	w3, w6
	b	L13
	.p2align 2,,3
L14:
	cmp	w6, w3
	bcc	L30
L13:
	sub	w0, w3, #3329
	subs	w5, w5, #1
	eor	w3, w0, w3
	and	w3, w3, w0, asr 31
	eor	w3, w3, w0
	bne	L14
	mov	w0, 65535
	cmp	w3, w0
	bhi	L7
	mul	w0, w2, w1
	mov	w2, 3327
	mov	w1, 3329
	mov	w5, 22
	mul	w2, w0, w2
	and	w2, w2, 65535
	madd	w2, w2, w1, w0
	lsr	w2, w2, 16
	mov	w0, w2
	b	L15
	.p2align 2,,3
L16:
	cmp	w2, w0
	bcc	L30
L15:
	sub	w1, w0, #3329
	subs	w5, w5, #1
	eor	w0, w1, w0
	and	w0, w0, w1, asr 31
	eor	w0, w0, w1
	bne	L16
	mov	w1, 65535
	cmp	w0, w1
	bhi	L7
	add	w1, w0, w3
	mov	w2, 20158
	add	w0, w0, w3
	mov	w3, 3329
	umull	x1, w1, w2
	mov	w2, 15
	lsr	x1, x1, 26
	umsubl	x1, w1, w3, x0
	mov	w3, w1
	b	L17
	.p2align 2,,3
L18:
	cmp	w3, w1
	bcc	L31
L17:
	sub	w0, w1, #3329
	subs	w2, w2, #1
	eor	w1, w0, w1
	and	w1, w1, w0, asr 31
	eor	w1, w1, w0
	bne	L18
	cmp	w1, 3328
	bhi	L19
	mov	x0, 0
	bfi	x0, x4, 0, 16
	bfi	x0, x1, 16, 16
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
L30:
LCFI3:
	adrp	x0, lC4@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L31:
	adrp	x0, lC6@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L7:
	adrp	x0, lC5@PAGE
	mov	w1, 141
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L19:
	adrp	x0, lC5@PAGE
	mov	w1, 66
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE10:
	.const
	.align	2
lC2:
	.word	1
	.word	45
	.align	2
lC3:
	.word	1
	.word	44
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_ntt__Ttwiddle_arrayBIP
_anubis_ntt__Ttwiddle_arrayBIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_ntt__TpolynomialBIP
_anubis_ntt__TpolynomialBIP:
LFB3:
	ret
LFE3:
	.const
	.align	3
lC7:
	.ascii "anubis_ntt.adb"
	.space 1
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:17"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_ntt__bit_reverse
_anubis_ntt__bit_reverse:
LFB4:
	stp	x29, x30, [sp, -16]!
LCFI4:
	mov	x29, sp
LCFI5:
	cmp	w0, 255
	bhi	L42
	mov	w1, w0
	mov	w2, 8
	mov	w0, 0
	b	L36
	.p2align 2,,3
L37:
	cmp	w0, 255
	bgt	L43
L36:
	and	w3, w1, 1
	subs	w2, w2, #1
	add	w0, w3, w0, lsl 1
	asr	w1, w1, 1
	bne	L37
	cmp	w0, 255
	bgt	L44
	ldp	x29, x30, [sp], 16
LCFI6:
	ret
L43:
LCFI7:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L42:
	adrp	x0, lC7@PAGE
	mov	w1, 12
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L44:
	adrp	x0, lC7@PAGE
	mov	w1, 27
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE4:
	.const
	.align	2
lC1:
	.word	1
	.word	42
	.text
	.const
	.align	3
lC9:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:44"
	.align	3
lC10:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:45"
	.align	3
lC11:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:46"
	.align	3
lC12:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:52"
	.align	3
lC13:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:53"
	.align	3
lC14:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:60"
	.align	3
lC15:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:63"
	.align	3
lC16:
	.ascii "failed precondition from anubis_field.ads:55"
	.align	3
lC17:
	.ascii "failed postcondition from anubis_ntt.ads:58"
	.align	3
lC18:
	.ascii "failed precondition from anubis_ntt.ads:57"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_ntt__ntt
_anubis_ntt__ntt:
LFB6:
	stp	x29, x30, [sp, -80]!
LCFI8:
	mov	x29, sp
LCFI9:
	mov	x17, x0
	add	x15, x0, 512
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI10:
	b	L47
	.p2align 2,,3
L84:
	add	x0, x0, 2
	cmp	x15, x0
	beq	L83
L47:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L84
	adrp	x0, lC18@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L83:
	adrp	x21, _anubis_ntt__zetas@PAGE
	mov	w7, 128
	mov	w30, 0
	add	x21, x21, _anubis_ntt__zetas@PAGEOFF;
	mov	w16, 1
	mov	w13, 3327
	mov	w4, 3329
	mov	w12, 65535
	mov	w11, 6658
	mov	w10, 16777215
	mov	w5, 20158
L48:
	lsl	w22, w7, 1
	ubfiz	x23, x7, 2, 31
	add	x20, x21, w16, uxtw 1
	mov	x19, x17
	mov	w8, 0
	sxtw	x6, w7
L52:
	ldrh	w9, [x20]
	mov	x2, x19
	mov	w3, w8
	add	w14, w8, w7
	.p2align 5,,15
L54:
	add	w0, w3, w7
	cmp	w0, 255
	bhi	L85
	ldrh	w0, [x2, x6, lsl 1]
	mov	w24, 22
	mul	w0, w9, w0
	mul	w25, w0, w13
	and	w25, w25, 65535
	madd	w25, w25, w4, w0
	lsr	w25, w25, 16
	mov	w0, w25
	b	L56
	.p2align 2,,3
L57:
	cmp	w25, w0
	bcc	L86
L56:
	sub	w1, w0, #3329
	subs	w24, w24, #1
	eor	w0, w1, w0
	and	w0, w0, w1, asr 31
	eor	w0, w0, w1
	bne	L57
	cmp	w0, w12
	bhi	L87
	ldrh	w24, [x2]
	add	w24, w24, w11
	sub	w24, w24, w0
	cmp	w24, w10
	bhi	L88
	umull	x1, w24, w5
	uxtw	x25, w24
	mov	w24, 15
	lsr	x1, x1, 26
	umsubl	x1, w1, w4, x25
	mov	w25, w1
	b	L60
	.p2align 2,,3
L61:
	cmp	w25, w1
	bcc	L82
L60:
	sub	w26, w1, #3329
	subs	w24, w24, #1
	eor	w1, w26, w1
	and	w1, w1, w26, asr 31
	eor	w1, w1, w26
	bne	L61
	cmp	w1, 3328
	bhi	L65
	strh	w1, [x2, x6, lsl 1]
	mov	w1, 15
	ldrh	w25, [x2]
	add	w24, w25, w0
	add	w25, w25, w0
	umull	x0, w24, w5
	lsr	x0, x0, 26
	umsubl	x0, w0, w4, x25
	mov	w25, w0
	b	L63
	.p2align 2,,3
L64:
	cmp	w25, w0
	bcc	L82
L63:
	sub	w24, w0, #3329
	subs	w1, w1, #1
	eor	w0, w24, w0
	and	w0, w0, w24, asr 31
	eor	w0, w0, w24
	bne	L64
	cmp	w0, 3328
	bhi	L65
	add	w3, w3, 1
	strh	w0, [x2]
	cmp	w3, w14
	bge	L89
	add	x2, x2, 2
	cmp	w3, w8
	bge	L54
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L89:
	add	w8, w8, w22
	add	w0, w16, 1
	cmp	w8, 255
	bgt	L90
	udiv	w1, w8, w22
	msub	w1, w1, w22, w8
	cbnz	w1, L91
	add	x20, x20, 2
	add	x19, x19, x23
	cmp	w0, 256
	beq	L92
	mov	w16, w0
	b	L52
L82:
	adrp	x0, lC6@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L86:
	adrp	x0, lC4@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L92:
	adrp	x0, lC13@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L91:
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L90:
	cmp	w7, 1
	beq	L93
	cmp	w30, 6
	beq	L94
	add	w30, w30, 1
	cmp	w16, 127
	bgt	L95
	mov	w16, 1
	lsl	w16, w16, w30
	cmp	w16, w0
	bne	L96
	mov	w0, 128
	asr	w7, w7, 1
	udiv	w0, w0, w16
	cmp	w0, w7
	beq	L48
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L96:
	adrp	x0, lC10@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L95:
	adrp	x0, lC9@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L94:
	mov	w0, 6658
	b	L71
	.p2align 2,,3
L98:
	add	x17, x17, 2
	cmp	x15, x17
	beq	L97
L71:
	ldrh	w1, [x17]
	cmp	w1, w0
	bls	L98
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L93:
	adrp	x0, lC7@PAGE
	mov	w1, 78
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L97:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 80
LCFI11:
	ret
L65:
LCFI12:
	adrp	x0, lC5@PAGE
	mov	w1, 66
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L88:
	adrp	x0, lC16@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L87:
	adrp	x0, lC5@PAGE
	mov	w1, 141
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L85:
	adrp	x0, lC15@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE6:
	.const
	.align	2
lC0:
	.word	1
	.word	43
	.text
	.const
	.align	3
lC19:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:96"
	.align	3
lC20:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:98"
	.align	3
lC21:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:104"
	.align	3
lC22:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:113"
	.align	3
lC23:
	.ascii "Loop_Invariant failed at anubis_ntt.adb:116"
	.align	3
lC24:
	.ascii "failed postcondition from anubis_ntt.ads:65"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_ntt__intt
_anubis_ntt__intt:
LFB8:
	stp	x29, x30, [sp, -80]!
LCFI13:
	mov	x29, sp
LCFI14:
	mov	w17, 2
	mov	x14, x0
	mov	w7, w17
	mov	w15, 127
	stp	x21, x22, [sp, 32]
LCFI15:
	adrp	x21, _anubis_ntt__zetas@PAGE
	mov	w5, 20158
	add	x21, x21, _anubis_ntt__zetas@PAGEOFF;
	mov	w4, 3329
	mov	w9, 6658
	mov	w8, 16777215
	mov	w13, 3327
	mov	w12, 65535
	stp	x19, x20, [sp, 16]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI16:
L100:
	lsl	w16, w7, 1
	ubfiz	x22, x7, 2, 31
	add	x20, x21, w15, uxtw 1
	mov	x30, x14
	mov	x19, x14
	mov	w6, 0
	sxtw	x3, w7
L103:
	sub	w15, w15, #1
	ldrh	w11, [x20]
	cmn	w15, #1
	beq	L146
	mov	x1, x19
	mov	w2, w6
	add	w10, w6, w7
	.p2align 5,,15
L104:
	add	w0, w2, w7
	cmp	w0, 255
	bhi	L147
	ldrh	w0, [x1, x3, lsl 1]
	mov	w23, 15
	ldrh	w24, [x1]
	add	w25, w0, w24
	umull	x0, w25, w5
	lsr	x0, x0, 26
	umsubl	x0, w0, w4, x25
	mov	w25, w0
	b	L106
	.p2align 2,,3
L107:
	cmp	w25, w0
	bcc	L145
L106:
	sub	w26, w0, #3329
	subs	w23, w23, #1
	eor	w0, w26, w0
	and	w0, w0, w26, asr 31
	eor	w0, w0, w26
	bne	L107
	cmp	w0, 3328
	bhi	L112
	strh	w0, [x1]
	ldrh	w23, [x1, x3, lsl 1]
	add	w23, w23, w9
	sub	w23, w23, w24
	cmp	w23, w8
	bhi	L148
	umull	x0, w23, w5
	uxtw	x24, w23
	mov	w23, 15
	lsr	x0, x0, 26
	umsubl	x0, w0, w4, x24
	mov	w25, w0
	b	L110
	.p2align 2,,3
L111:
	cmp	w25, w0
	bcc	L145
L110:
	sub	w24, w0, #3329
	subs	w23, w23, #1
	eor	w0, w24, w0
	and	w0, w0, w24, asr 31
	eor	w0, w0, w24
	bne	L111
	cmp	w0, 3328
	bhi	L112
	mul	w23, w11, w0
	mov	w24, 22
	strh	w0, [x1, x3, lsl 1]
	mul	w25, w23, w13
	and	w25, w25, 65535
	madd	w25, w25, w4, w23
	lsr	w25, w25, 16
	mov	w0, w25
	b	L113
	.p2align 2,,3
L114:
	cmp	w25, w0
	bcc	L149
L113:
	sub	w23, w0, #3329
	subs	w24, w24, #1
	eor	w0, w23, w0
	and	w0, w0, w23, asr 31
	eor	w0, w0, w23
	bne	L114
	cmp	w0, w12
	bhi	L150
	add	w2, w2, 1
	strh	w0, [x1, x3, lsl 1]
	cmp	w2, w10
	bge	L151
	add	x1, x1, 2
	cmp	w2, w6
	bge	L104
	adrp	x0, lC22@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC22@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L151:
	add	w6, w6, w16
	cmp	w6, 255
	bgt	L152
	udiv	w0, w6, w16
	sub	x20, x20, #2
	add	x19, x19, x22
	msub	w0, w0, w16, w6
	cbz	w0, L103
	adrp	x0, lC21@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC21@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L145:
	adrp	x0, lC6@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L149:
	adrp	x0, lC4@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L147:
	adrp	x0, lC23@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC23@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L150:
	adrp	x0, lC5@PAGE
	mov	w1, 141
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L148:
	adrp	x0, lC16@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L112:
	adrp	x0, lC5@PAGE
	mov	w1, 66
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L146:
	adrp	x0, lC7@PAGE
	mov	w1, 109
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L152:
	cmp	w17, 8
	beq	L153
	sub	w0, w16, #2
	cmp	w0, 126
	bhi	L154
	mov	w0, 1
	lsl	w0, w0, w17
	add	w17, w17, 1
	cmp	w0, w16
	bne	L102
	mov	w7, w16
	b	L100
	.p2align 2,,3
L154:
	adrp	x0, lC19@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L153:
	add	x5, x14, 512
	mov	x3, x14
	mov	w8, 3303
	mov	w7, 20158
	mov	w6, 3329
L122:
	ldrh	w1, [x3]
	cmp	w1, 3328
	bhi	L155
	mul	w1, w1, w8
	mov	w2, 15
	umull	x0, w1, w7
	lsr	x0, x0, 26
	umsubl	x0, w0, w6, x1
	mov	w4, w0
	b	L120
	.p2align 2,,3
L121:
	cmp	w4, w0
	bcc	L145
L120:
	sub	w1, w0, #3329
	subs	w2, w2, #1
	eor	w0, w1, w0
	and	w0, w0, w1, asr 31
	eor	w0, w0, w1
	bne	L121
	cmp	w0, 3328
	bhi	L112
	strh	w0, [x3], 2
	cmp	x5, x3
	bne	L122
	mov	w6, 20158
	mov	w4, 3329
L125:
	ldrh	w1, [x14]
	mov	w2, 15
	umull	x0, w1, w6
	lsr	x0, x0, 26
	umsubl	x0, w0, w4, x1
	mov	w3, w0
	b	L123
	.p2align 2,,3
L124:
	cmp	w3, w0
	bcc	L145
L123:
	sub	w1, w0, #3329
	subs	w2, w2, #1
	eor	w0, w1, w0
	and	w0, w0, w1, asr 31
	eor	w0, w0, w1
	bne	L124
	cmp	w0, 3328
	bhi	L112
	strh	w0, [x14], 2
	cmp	x5, x14
	bne	L125
	b	L127
	.p2align 2,,3
L157:
	add	x30, x30, 2
	cmp	x5, x30
	beq	L156
L127:
	ldrh	w0, [x30]
	cmp	w0, 3328
	bls	L157
	adrp	x0, lC24@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC24@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L155:
	adrp	x0, lC7@PAGE
	mov	w1, 140
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L156:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 80
LCFI17:
	ret
L102:
LCFI18:
	adrp	x0, lC20@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE8:
	.const
	.align	3
lC25:
	.ascii "failed postcondition from anubis_ntt.ads:76"
	.align	3
lC26:
	.ascii "failed precondition from anubis_ntt.ads:75"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_ntt__ntt_mul
_anubis_ntt__ntt_mul:
LFB11:
	stp	x29, x30, [sp, -96]!
LCFI19:
	mov	x29, sp
LCFI20:
	mov	x3, 0
	stp	x19, x20, [sp, 16]
LCFI21:
	mov	x20, x1
	stp	x21, x22, [sp, 32]
LCFI22:
	mov	x21, x0
	mov	w0, 6657
	stp	x23, x24, [sp, 48]
LCFI23:
	mov	x24, x2
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
LCFI24:
	b	L160
	.p2align 2,,3
L181:
	add	x3, x3, 2
	cmp	x3, 512
	beq	L180
L160:
	ldrh	w2, [x21, x3]
	ldrh	w1, [x20, x3]
	cmp	w2, w0
	ccmp	w1, w0, 2, ls
	bls	L181
	adrp	x0, lC26@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC26@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L180:
	adrp	x25, _anubis_ntt__zetas@PAGE
	adrp	x27, _anubis_ntt__zetas@PAGE+128
	add	x25, x25, _anubis_ntt__zetas@PAGEOFF;
	add	x27, x27, _anubis_ntt__zetas@PAGEOFF+128;
	add	x25, x25, 256
	mov	x19, x24
	mov	x22, x24
	mov	w26, 3329
	.p2align 5,,15
L163:
	ldrh	w4, [x27], 2
	add	x21, x21, 8
	add	x20, x20, 8
	add	x22, x22, 8
	ldrh	w1, [x21, -6]
	ldrh	w3, [x20, -6]
	cmp	w4, 0
	sub	w23, w26, w4
	ldrh	w2, [x20, -8]
	and	w23, w23, 65535
	csel	w23, w23, wzr, ne
	ldrh	w0, [x21, -8]
	bl	_anubis_ntt__basemul
	lsr	w1, w0, 16
	strh	w0, [x22, -8]
	mov	w4, w23
	strh	w1, [x22, -6]
	ldrh	w1, [x21, -2]
	ldrh	w3, [x20, -2]
	ldrh	w2, [x20, -4]
	ldrh	w0, [x21, -4]
	bl	_anubis_ntt__basemul
	lsr	w1, w0, 16
	strh	w0, [x22, -4]
	strh	w1, [x22, -2]
	cmp	x27, x25
	bne	L163
	mov	w8, 44983
	add	x4, x24, 512
	movk	w8, 0x44, lsl 16
	mov	w7, 3329
	mov	w6, 1353
	mov	w5, 65535
	.p2align 5,,15
L167:
	ldrh	w0, [x24]
	mov	w2, 22
	mul	w3, w0, w8
	mul	w0, w0, w6
	and	w3, w3, 65535
	madd	w3, w3, w7, w0
	lsr	w3, w3, 16
	mov	w0, w3
	b	L164
	.p2align 2,,3
L165:
	cmp	w3, w0
	bcc	L182
L164:
	sub	w1, w0, #3329
	subs	w2, w2, #1
	eor	w0, w0, w1
	and	w0, w0, w1, asr 31
	eor	w0, w0, w1
	bne	L165
	cmp	w0, w5
	bhi	L183
	strh	w0, [x24], 2
	cmp	x24, x4
	bne	L167
	mov	w1, 6657
	b	L169
L185:
	add	x19, x19, 2
	cmp	x4, x19
	beq	L184
L169:
	ldrh	w0, [x19]
	cmp	w0, w1
	bls	L185
	adrp	x0, lC25@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC25@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L184:
	ldr	x27, [sp, 80]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 96
LCFI25:
	ret
L182:
LCFI26:
	adrp	x0, lC4@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L183:
	adrp	x0, lC5@PAGE
	mov	w1, 141
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE11:
	.const
	.align	3
lC27:
	.ascii "failed postcondition from anubis_ntt.ads:81"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_ntt__normalize
_anubis_ntt__normalize:
LFB13:
	mov	w7, 20158
	mov	x8, x0
	stp	x29, x30, [sp, -16]!
LCFI27:
	mov	w6, 3329
	add	x5, x0, 512
	mov	x4, x0
	mov	x29, sp
LCFI28:
	.p2align 5,,15
L190:
	ldrh	w1, [x4]
	mov	w2, 15
	umull	x0, w1, w7
	lsr	x0, x0, 26
	umsubl	x0, w0, w6, x1
	mov	w3, w0
	b	L187
	.p2align 2,,3
L188:
	cmp	w3, w0
	bcc	L198
L187:
	sub	w1, w0, #3329
	subs	w2, w2, #1
	eor	w0, w0, w1
	and	w0, w0, w1, asr 31
	eor	w0, w0, w1
	bne	L188
	cmp	w0, 3328
	bhi	L199
	strh	w0, [x4], 2
	cmp	x5, x4
	bne	L190
	ldrh	w0, [x8]
	cmp	w0, 3328
	bhi	L191
L201:
	add	x8, x8, 2
	cmp	x5, x8
	beq	L200
	ldrh	w0, [x8]
	cmp	w0, 3328
	bls	L201
L191:
	adrp	x0, lC27@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC27@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L200:
	ldp	x29, x30, [sp], 16
LCFI29:
	ret
L198:
LCFI30:
	adrp	x0, lC6@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L199:
	adrp	x0, lC5@PAGE
	mov	w1, 66
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE13:
	.globl _anubis_ntt__zetas_inv
	.const
	.align	1
_anubis_ntt__zetas_inv:
	.hword	2360
	.hword	1019
	.hword	993
	.hword	2849
	.hword	555
	.hword	184
	.hword	1602
	.hword	872
	.hword	2226
	.hword	153
	.hword	3225
	.hword	691
	.hword	1177
	.hword	761
	.hword	255
	.hword	2134
	.hword	733
	.hword	588
	.hword	1054
	.hword	927
	.hword	1541
	.hword	2222
	.hword	2127
	.hword	842
	.hword	2428
	.hword	1103
	.hword	1919
	.hword	1801
	.hword	1762
	.hword	557
	.hword	3099
	.hword	2648
	.hword	780
	.hword	721
	.hword	3220
	.hword	792
	.hword	1524
	.hword	293
	.hword	2291
	.hword	1947
	.hword	1690
	.hword	229
	.hword	3035
	.hword	940
	.hword	1726
	.hword	156
	.hword	956
	.hword	396
	.hword	555
	.hword	2445
	.hword	669
	.hword	1126
	.hword	2231
	.hword	859
	.hword	1727
	.hword	1698
	.hword	2077
	.hword	612
	.hword	635
	.hword	1990
	.hword	946
	.hword	305
	.hword	1930
	.hword	2960
	.hword	1943
	.hword	3337
	.hword	732
	.hword	603
	.hword	807
	.hword	502
	.hword	477
	.hword	2176
	.hword	1714
	.hword	293
	.hword	1127
	.hword	788
	.hword	2797
	.hword	869
	.hword	637
	.hword	2962
	.hword	308
	.hword	1567
	.hword	758
	.hword	1711
	.hword	2663
	.hword	3009
	.hword	3321
	.hword	516
	.hword	1785
	.hword	3047
	.hword	1491
	.hword	2036
	.hword	1015
	.hword	2777
	.hword	652
	.hword	1223
	.hword	1758
	.hword	3124
	.hword	411
	.hword	1787
	.hword	608
	.hword	732
	.hword	1017
	.hword	2648
	.hword	3199
	.hword	1727
	.hword	1458
	.hword	2500
	.hword	383
	.hword	264
	.hword	2004
	.hword	573
	.hword	1468
	.hword	1855
	.hword	2127
	.hword	962
	.hword	182
	.hword	1577
	.hword	622
	.hword	3158
	.hword	202
	.hword	287
	.hword	1422
	.hword	1493
	.hword	1812
	.hword	2970
	.hword	2571
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.globl _anubis_ntt__zetas
	.align	1
_anubis_ntt__zetas:
	.hword	2285
	.hword	2571
	.hword	2970
	.hword	1812
	.hword	1493
	.hword	1422
	.hword	287
	.hword	202
	.hword	3158
	.hword	622
	.hword	1577
	.hword	182
	.hword	962
	.hword	2127
	.hword	1855
	.hword	1468
	.hword	573
	.hword	2004
	.hword	264
	.hword	383
	.hword	2500
	.hword	1458
	.hword	1727
	.hword	3199
	.hword	2648
	.hword	1017
	.hword	732
	.hword	608
	.hword	1787
	.hword	411
	.hword	3124
	.hword	1758
	.hword	1223
	.hword	652
	.hword	2777
	.hword	1015
	.hword	2036
	.hword	1491
	.hword	3047
	.hword	1785
	.hword	516
	.hword	3321
	.hword	3009
	.hword	2663
	.hword	1711
	.hword	2167
	.hword	126
	.hword	1469
	.hword	2476
	.hword	3239
	.hword	3058
	.hword	830
	.hword	107
	.hword	1908
	.hword	3082
	.hword	2378
	.hword	2931
	.hword	961
	.hword	1821
	.hword	2604
	.hword	448
	.hword	2264
	.hword	677
	.hword	2054
	.hword	2226
	.hword	430
	.hword	555
	.hword	843
	.hword	2078
	.hword	871
	.hword	1550
	.hword	105
	.hword	422
	.hword	587
	.hword	177
	.hword	3094
	.hword	3038
	.hword	2869
	.hword	1574
	.hword	1653
	.hword	3083
	.hword	778
	.hword	1159
	.hword	3182
	.hword	2552
	.hword	1483
	.hword	2727
	.hword	1119
	.hword	1739
	.hword	644
	.hword	2457
	.hword	349
	.hword	418
	.hword	329
	.hword	3173
	.hword	3254
	.hword	817
	.hword	1097
	.hword	603
	.hword	610
	.hword	1322
	.hword	2044
	.hword	1864
	.hword	384
	.hword	2114
	.hword	3193
	.hword	1218
	.hword	1994
	.hword	2455
	.hword	220
	.hword	2142
	.hword	1670
	.hword	2144
	.hword	1799
	.hword	2051
	.hword	794
	.hword	1819
	.hword	2475
	.hword	2459
	.hword	478
	.hword	3221
	.hword	3021
	.hword	996
	.hword	991
	.hword	958
	.hword	1869
	.hword	1522
	.hword	1628
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.hword	0
	.globl _anubis_ntt__n_inv
	.align	1
_anubis_ntt__n_inv:
	.hword	3303
	.globl _anubis_ntt_E
	.data
	.align	1
_anubis_ntt_E:
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
	.quad	LFB10-.
	.set L$set$2,LFE10-LFB10
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB10
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
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$7,LEFDE3-LASFDE3
	.long L$set$7
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB2-.
	.set L$set$8,LFE2-LFB2
	.quad L$set$8
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$9,LEFDE5-LASFDE5
	.long L$set$9
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB3-.
	.set L$set$10,LFE3-LFB3
	.quad L$set$10
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$11,LEFDE7-LASFDE7
	.long L$set$11
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB4-.
	.set L$set$12,LFE4-LFB4
	.quad L$set$12
	.uleb128 0
	.byte	0x4
	.set L$set$13,LCFI4-LFB4
	.long L$set$13
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$14,LCFI5-LCFI4
	.long L$set$14
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$15,LCFI6-LCFI5
	.long L$set$15
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$16,LCFI7-LCFI6
	.long L$set$16
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$17,LEFDE9-LASFDE9
	.long L$set$17
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB6-.
	.set L$set$18,LFE6-LFB6
	.quad L$set$18
	.uleb128 0
	.byte	0x4
	.set L$set$19,LCFI8-LFB6
	.long L$set$19
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$20,LCFI9-LCFI8
	.long L$set$20
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$21,LCFI10-LCFI9
	.long L$set$21
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
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
	.byte	0x9a
	.uleb128 0x1
	.byte	0x4
	.set L$set$22,LCFI11-LCFI10
	.long L$set$22
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
	.set L$set$23,LCFI12-LCFI11
	.long L$set$23
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$24,LEFDE11-LASFDE11
	.long L$set$24
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB8-.
	.set L$set$25,LFE8-LFB8
	.quad L$set$25
	.uleb128 0
	.byte	0x4
	.set L$set$26,LCFI13-LFB8
	.long L$set$26
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$27,LCFI14-LCFI13
	.long L$set$27
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$28,LCFI15-LCFI14
	.long L$set$28
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x4
	.set L$set$29,LCFI16-LCFI15
	.long L$set$29
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x99
	.uleb128 0x2
	.byte	0x9a
	.uleb128 0x1
	.byte	0x4
	.set L$set$30,LCFI17-LCFI16
	.long L$set$30
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
	.set L$set$31,LCFI18-LCFI17
	.long L$set$31
	.byte	0xb
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$32,LEFDE13-LASFDE13
	.long L$set$32
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB11-.
	.set L$set$33,LFE11-LFB11
	.quad L$set$33
	.uleb128 0
	.byte	0x4
	.set L$set$34,LCFI19-LFB11
	.long L$set$34
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$35,LCFI20-LCFI19
	.long L$set$35
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$36,LCFI21-LCFI20
	.long L$set$36
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$37,LCFI22-LCFI21
	.long L$set$37
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$38,LCFI23-LCFI22
	.long L$set$38
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x4
	.set L$set$39,LCFI24-LCFI23
	.long L$set$39
	.byte	0x99
	.uleb128 0x4
	.byte	0x9a
	.uleb128 0x3
	.byte	0x9b
	.uleb128 0x2
	.byte	0x4
	.set L$set$40,LCFI25-LCFI24
	.long L$set$40
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
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
	.set L$set$41,LCFI26-LCFI25
	.long L$set$41
	.byte	0xb
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$42,LEFDE15-LASFDE15
	.long L$set$42
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB13-.
	.set L$set$43,LFE13-LFB13
	.quad L$set$43
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI27-LFB13
	.long L$set$44
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$45,LCFI28-LCFI27
	.long L$set$45
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$46,LCFI29-LCFI28
	.long L$set$46
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI30-LCFI29
	.long L$set$47
	.byte	0xb
	.align	3
LEFDE15:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
