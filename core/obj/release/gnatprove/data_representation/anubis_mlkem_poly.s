	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
_anubis_mlkem_poly__poly_intt___wrapped_statements.5:
LFB8:
	sub	sp, sp, #544
LCFI0:
	stp	x29, x30, [sp]
LCFI1:
	mov	x29, sp
LCFI2:
	stp	x19, x20, [sp, 16]
LCFI3:
	add	x19, x29, 32
	mov	x20, x16
	mov	x0, x19
	ldr	x1, [x16]
	.p2align 5,,15
L2:
	add	x2, x29, 544
	ldr	q31, [x1], 16
	str	q31, [x0], 16
	cmp	x0, x2
	bne	L2
	mov	x0, x19
	bl	_anubis_ntt__intt
	ldr	x1, [x20]
	mov	x0, x19
	add	x2, x29, 544
	.p2align 5,,15
L3:
	ldr	q31, [x0], 16
	str	q31, [x1], 16
	cmp	x2, x0
	bne	L3
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 544
LCFI4:
	ret
LFE8:
	.align	2
	.p2align 5,,15
_anubis_mlkem_poly__poly_reduce___wrapped_statements.6:
LFB11:
	sub	sp, sp, #544
LCFI5:
	stp	x29, x30, [sp]
LCFI6:
	mov	x29, sp
LCFI7:
	stp	x19, x20, [sp, 16]
LCFI8:
	add	x19, x29, 32
	mov	x20, x16
	mov	x0, x19
	ldr	x1, [x16]
	.p2align 5,,15
L9:
	add	x2, x29, 544
	ldr	q31, [x1], 16
	str	q31, [x0], 16
	cmp	x0, x2
	bne	L9
	mov	x0, x19
	bl	_anubis_ntt__normalize
	ldr	x1, [x20]
	mov	x0, x19
	add	x2, x29, 544
	.p2align 5,,15
L10:
	ldr	q31, [x0], 16
	str	q31, [x1], 16
	cmp	x2, x0
	bne	L10
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 544
LCFI9:
	ret
LFE11:
	.const
	.align	3
lC5:
	.ascii "Loop_Invariant failed at anubis_mlkem_poly.adb:48"
	.align	3
lC6:
	.ascii "anubis_mlkem_poly.adb:52"
	.text
	.align	2
	.p2align 5,,15
_anubis_mlkem_poly__poly_ntt.part.0:
LFB42:
	sub	sp, sp, #560
LCFI10:
	mov	x5, -1
	add	x7, x0, 2
	stp	x29, x30, [sp]
LCFI11:
	mov	x29, sp
LCFI12:
	add	x1, x29, 48
	add	x8, x29, 50
	str	x19, [sp, 16]
LCFI13:
	mov	x19, x0
	.p2align 5,,15
L21:
	cmn	w5, #1
	bne	L31
	ldrh	w0, [x7, -2]
	mov	x5, 0
	strh	w0, [x29, 48]
L31:
	add	x4, x8, x5, lsl 1
	mov	x0, x1
	lsl	x6, x5, 1
	mov	x2, x1
	b	L17
	.p2align 2,,3
L34:
	add	x2, x2, 2
	cmp	x2, x4
	beq	L33
L17:
	ldrh	w3, [x2]
	cmp	w3, 3328
	bls	L34
	adrp	x0, lC5@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L33:
	ldrh	w2, [x7, x5, lsl 1]
	add	x6, x1, x6
	add	x5, x5, 1
	strh	w2, [x6, 2]
	cmp	x5, 255
	bne	L21
	adrp	x2, lC7@PAGE
	adrp	x3, lC8@PAGE
	mvni	v24.4s, 0x7
	movi	v25.4s, 0x8
	movi	v26.8h, 0xd, lsl 8
	ldr	q29, [x2, #lC7@PAGEOFF]
	add	x2, x29, 560
	ldr	q30, [x3, #lC8@PAGEOFF]
	b	L24
	.p2align 2,,3
L22:
	add	x0, x0, 16
	cmp	x0, x2
	beq	L25
L24:
	ldr	q31, [x0]
	mov	v27.16b, v29.16b
	mov	v28.16b, v30.16b
	add	v29.4s, v29.4s, v24.4s
	add	v30.4s, v30.4s, v25.4s
	cmhi	v31.8h, v31.8h, v26.8h
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x3, d31
	cbz	x3, L22
	fmov	w0, s28
	fmov	w2, s27
	sxtw	x0, w0
	sub	w2, w2, #1
	add	x3, x0, 1
	add	x3, x3, x2
	b	L23
	.p2align 2,,3
L35:
	add	x0, x0, 1
	cmp	x0, x3
	beq	L25
L23:
	add	x2, x1, x0, lsl 1
	ldrh	w2, [x2, 2]
	cmp	w2, 3328
	bls	L35
	adrp	x0, lC6@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L25:
	mov	x0, x1
	str	x1, [x29, 40]
	bl	_anubis_ntt__ntt
	ldr	x1, [x29, 40]
	mov	x0, x19
	mov	x2, 512
	bl	_memcpy
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp]
	add	sp, sp, 560
LCFI14:
	ret
LFE42:
	.const
	.align	2
lC1:
	.word	1
	.word	49
	.align	2
lC4:
	.word	1
	.word	24
	.text
	.align	2
	.p2align 5,,15
_anubis_mlkem_poly__poly_mul_ntt.part.0:
LFB44:
	sub	sp, sp, #1584
LCFI15:
	stp	x29, x30, [sp]
LCFI16:
	mov	x29, sp
LCFI17:
	stp	x19, x20, [sp, 16]
LCFI18:
	add	x20, x29, 48
	mov	x19, x1
	mov	x1, x0
	mov	x0, x20
	str	x21, [sp, 32]
LCFI19:
	mov	x21, x2
	mov	x2, 512
	bl	_memcpy
	add	x3, x29, 560
	mov	x1, x19
	mov	x0, x3
	mov	x2, 512
	bl	_memcpy
	add	x19, x29, 1072
	mov	x1, x0
	mov	x2, x19
	mov	x0, x20
	bl	_anubis_ntt__ntt_mul
	mov	x2, 512
	mov	x1, x19
	mov	x0, x21
	bl	_memcpy
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 1584
LCFI20:
	ret
LFE44:
	.const
	.align	3
lC9:
	.ascii "anubis_field.adb"
	.space 1
	.align	3
lC10:
	.ascii "failed postcondition from anubis_mlkem_poly.ads:32"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__poly_add
_anubis_mlkem_poly__poly_add:
LFB2:
	mov	x5, 0
	mov	w7, 3329
	stp	x29, x30, [sp, -16]!
LCFI21:
	mov	x29, sp
LCFI22:
	.p2align 5,,15
L40:
	ldrh	w6, [x1, x5]
	ldrh	w8, [x0, x5]
	udiv	w4, w6, w7
	udiv	w3, w8, w7
	msub	w4, w4, w7, w6
	msub	w3, w3, w7, w8
	add	w3, w3, w4
	and	w3, w3, 65535
	sub	w4, w3, #3329
	and	w6, w4, 65535
	sbfx	x4, x4, 15, 1
	eor	w3, w3, w6
	and	w4, w4, w3
	eor	w6, w6, w4
	cmp	w6, 3328
	bhi	L48
	strh	w6, [x2, x5]
	add	x5, x5, 2
	cmp	x5, 512
	bne	L40
	add	x1, x2, 512
	b	L42
	.p2align 2,,3
L50:
	add	x2, x2, 2
	cmp	x1, x2
	beq	L49
L42:
	ldrh	w0, [x2]
	cmp	w0, 3328
	bls	L50
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L49:
	ldp	x29, x30, [sp], 16
LCFI23:
	ret
L48:
LCFI24:
	adrp	x0, lC9@PAGE
	mov	w1, 24
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE2:
	.const
	.align	2
lC0:
	.word	1
	.word	50
	.text
	.const
	.align	3
lC11:
	.ascii "failed postcondition from anubis_mlkem_poly.ads:40"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__poly_sub
_anubis_mlkem_poly__poly_sub:
LFB4:
	mov	x4, 0
	mov	w6, 3329
	stp	x29, x30, [sp, -16]!
LCFI25:
	mov	x29, sp
LCFI26:
	.p2align 5,,15
L53:
	ldrh	w7, [x1, x4]
	ldrh	w8, [x0, x4]
	udiv	w5, w7, w6
	udiv	w3, w8, w6
	msub	w5, w5, w6, w7
	msub	w3, w3, w6, w8
	sub	w3, w3, w5
	and	w3, w3, 65535
	add	w5, w3, 3329
	sbfx	x7, x3, 15, 1
	eor	w5, w3, w5
	and	w5, w5, w7
	eor	w3, w3, w5
	and	w3, w3, 65535
	cmp	w3, 3328
	bhi	L61
	strh	w3, [x2, x4]
	add	x4, x4, 2
	cmp	x4, 512
	bne	L53
	add	x1, x2, 512
	b	L55
	.p2align 2,,3
L63:
	add	x2, x2, 2
	cmp	x1, x2
	beq	L62
L55:
	ldrh	w0, [x2]
	cmp	w0, 3328
	bls	L63
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L62:
	ldp	x29, x30, [sp], 16
LCFI27:
	ret
L61:
LCFI28:
	adrp	x0, lC9@PAGE
	mov	w1, 24
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE4:
	.const
	.align	3
lC12:
	.ascii "failed precondition from anubis_mlkem_poly.ads:47"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__poly_ntt
_anubis_mlkem_poly__poly_ntt:
LFB6:
	mov	x1, x0
	add	x3, x0, 512
	b	L66
	.p2align 2,,3
L71:
	add	x1, x1, 2
	cmp	x3, x1
	beq	L70
L66:
	ldrh	w2, [x1]
	cmp	w2, 3328
	bls	L71
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI29:
	add	x0, x0, lC12@PAGEOFF;
	mov	x29, sp
LCFI30:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L70:
LCFI31:
	b	_anubis_mlkem_poly__poly_ntt.part.0
LFE6:
	.const
	.align	3
lC13:
	.ascii "failed postcondition from anubis_mlkem_poly.ads:54"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__poly_intt
_anubis_mlkem_poly__poly_intt:
LFB7:
	stp	x29, x30, [sp, -48]!
LCFI32:
	mov	x29, sp
LCFI33:
	add	x1, x29, 48
	add	x16, x29, 32
	str	x19, [sp, 16]
LCFI34:
	mov	x19, x0
	stp	x0, x1, [x29, 32]
	bl	_anubis_mlkem_poly__poly_intt___wrapped_statements.5
	mov	x0, x19
	add	x2, x19, 512
	b	L74
	.p2align 2,,3
L79:
	add	x0, x0, 2
	cmp	x2, x0
	beq	L78
L74:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L79
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L78:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI35:
	ret
LFE7:
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_mlkem_poly.ads:63"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__poly_mul_ntt
_anubis_mlkem_poly__poly_mul_ntt:
LFB9:
	mov	x3, 0
	mov	w4, 6657
	b	L82
	.p2align 2,,3
L87:
	add	x3, x3, 2
	cmp	x3, 512
	beq	L86
L82:
	ldrh	w6, [x0, x3]
	ldrh	w5, [x1, x3]
	cmp	w6, w4
	ccmp	w5, w4, 2, ls
	bls	L87
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI36:
	add	x0, x0, lC14@PAGEOFF;
	mov	x29, sp
LCFI37:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L86:
LCFI38:
	b	_anubis_mlkem_poly__poly_mul_ntt.part.0
LFE9:
	.const
	.align	3
lC15:
	.ascii "failed postcondition from anubis_mlkem_poly.ads:70"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__poly_reduce
_anubis_mlkem_poly__poly_reduce:
LFB10:
	stp	x29, x30, [sp, -48]!
LCFI39:
	mov	x29, sp
LCFI40:
	add	x1, x29, 48
	add	x16, x29, 32
	str	x19, [sp, 16]
LCFI41:
	mov	x19, x0
	stp	x0, x1, [x29, 32]
	bl	_anubis_mlkem_poly__poly_reduce___wrapped_statements.6
	mov	x0, x19
	add	x2, x19, 512
	b	L90
	.p2align 2,,3
L95:
	add	x0, x0, 2
	cmp	x2, x0
	beq	L94
L90:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L95
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L94:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI42:
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__msg_to_poly
_anubis_mlkem_poly__msg_to_poly:
LFB12:
	mov	x5, 0
	mov	x6, x0
	mov	x2, x1
	mov	w4, 1665
	.p2align 5,,15
L98:
	ldrb	w3, [x6, x5]
	mov	x1, 0
	.p2align 5,,15
L97:
	lsr	w0, w3, w1
	sbfx	x0, x0, 0, 1
	and	w0, w0, w4
	strh	w0, [x2, x1, lsl 1]
	add	x1, x1, 1
	cmp	x1, 8
	bne	L97
	add	x5, x5, 1
	add	x2, x2, 16
	cmp	x5, 32
	bne	L98
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__vec_add
_anubis_mlkem_poly__vec_add:
LFB13:
	stp	x29, x30, [sp, -48]!
LCFI43:
	mov	x29, sp
LCFI44:
	stp	x19, x20, [sp, 16]
LCFI45:
	mov	x19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI46:
	mov	x22, x0
	mov	x21, x1
L102:
	add	x2, x20, x19
	add	x1, x21, x19
	add	x0, x22, x19
	add	x19, x19, 512
	bl	_anubis_mlkem_poly__poly_add
	cmp	x19, 2048
	bne	L102
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI47:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__vec_sub
_anubis_mlkem_poly__vec_sub:
LFB14:
	stp	x29, x30, [sp, -48]!
LCFI48:
	mov	x29, sp
LCFI49:
	stp	x19, x20, [sp, 16]
LCFI50:
	mov	x19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI51:
	mov	x22, x0
	mov	x21, x1
L106:
	add	x2, x20, x19
	add	x1, x21, x19
	add	x0, x22, x19
	add	x19, x19, 512
	bl	_anubis_mlkem_poly__poly_sub
	cmp	x19, 2048
	bne	L106
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI52:
	ret
LFE14:
	.const
	.align	3
lC16:
	.ascii "Loop_Invariant failed at anubis_mlkem_poly.adb:179"
	.align	3
lC17:
	.ascii "failed precondition from anubis_mlkem_poly.ads:100"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__vec_ntt
_anubis_mlkem_poly__vec_ntt:
LFB15:
	add	x4, x0, 512
	add	x3, x0, 2560
	stp	x29, x30, [sp, -32]!
LCFI53:
	mov	x2, x4
	mov	x29, sp
LCFI54:
	stp	x19, x20, [sp, 16]
LCFI55:
L112:
	sub	x0, x2, #512
	b	L111
	.p2align 2,,3
L132:
	add	x0, x0, 2
	cmp	x0, x2
	beq	L131
L111:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L132
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L131:
	add	x2, x0, 512
	cmp	x2, x3
	bne	L112
	mov	w20, -1
L120:
	mov	w3, w20
	mov	x2, x4
	add	w20, w20, 1
	.p2align 5,,15
L115:
	add	w3, w3, 1
	sub	x0, x2, #512
	b	L114
	.p2align 2,,3
L134:
	add	x0, x0, 2
	cmp	x0, x2
	beq	L133
L114:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L134
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L133:
	add	x2, x0, 512
	cmp	w3, 3
	bne	L115
	sub	x19, x4, #512
	mov	x0, x19
	b	L118
	.p2align 2,,3
L136:
	add	x19, x19, 2
	cmp	x19, x4
	beq	L135
L118:
	ldrh	w1, [x19]
	cmp	w1, 3328
	bls	L136
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L135:
	bl	_anubis_mlkem_poly__poly_ntt.part.0
	add	x4, x19, 512
	cmp	w20, 3
	bne	L120
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI56:
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__vec_intt
_anubis_mlkem_poly__vec_intt:
LFB16:
	stp	x29, x30, [sp, -64]!
LCFI57:
	mov	x29, sp
LCFI58:
	stp	x19, x20, [sp, 16]
LCFI59:
	add	x20, x0, 512
	stp	x21, x22, [sp, 32]
LCFI60:
	add	x22, x29, 48
	add	x21, x0, 2560
L141:
	sub	x19, x20, #512
	add	x0, x29, 64
	mov	x16, x22
	stp	x19, x0, [x29, 48]
	bl	_anubis_mlkem_poly__poly_intt___wrapped_statements.5
	b	L139
	.p2align 2,,3
L147:
	add	x19, x19, 2
	cmp	x19, x20
	beq	L146
L139:
	ldrh	w0, [x19]
	cmp	w0, 3328
	bls	L147
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L146:
	add	x20, x19, 512
	cmp	x20, x21
	bne	L141
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI61:
	ret
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__vec_dot_product_ntt
_anubis_mlkem_poly__vec_dot_product_ntt:
LFB17:
	sub	sp, sp, #1088
LCFI62:
	stp	x29, x30, [sp]
LCFI63:
	mov	x29, sp
LCFI64:
	stp	x19, x20, [sp, 16]
LCFI65:
	mov	x20, x2
	mov	x19, 0
	stp	x21, x22, [sp, 32]
LCFI66:
	add	x22, x29, 64
	add	x21, x29, 576
	stp	x23, x24, [sp, 48]
LCFI67:
	mov	x24, x0
	mov	x23, x1
	mov	x0, x2
	mov	w1, 0
	mov	x2, 512
	bl	_memset
L149:
	add	x1, x23, x19
	add	x0, x24, x19
	mov	x2, x22
	add	x19, x19, 512
	bl	_anubis_mlkem_poly__poly_mul_ntt
	mov	x2, x21
	mov	x1, x22
	mov	x0, x20
	bl	_anubis_mlkem_poly__poly_add
	mov	x1, x21
	mov	x0, x20
	mov	x2, 512
	bl	_memcpy
	cmp	x19, 2048
	bne	L149
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 1088
LCFI68:
	ret
LFE17:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__matrix_vec_mul_ntt
_anubis_mlkem_poly__matrix_vec_mul_ntt:
LFB18:
	sub	sp, sp, #1120
LCFI69:
	stp	x29, x30, [sp]
LCFI70:
	mov	x29, sp
LCFI71:
	stp	x21, x22, [sp, 32]
LCFI72:
	mov	x21, x2
	add	x22, x29, 608
	stp	x23, x24, [sp, 48]
LCFI73:
	add	x23, x29, 96
	add	x24, x1, 2048
	stp	x25, x26, [sp, 64]
LCFI74:
	mov	x26, 0
	mov	x25, x0
	str	x27, [sp, 80]
LCFI75:
	mov	x27, x1
	stp	x19, x20, [sp, 16]
LCFI76:
L154:
	mov	x2, 512
	mov	w1, 0
	mov	x0, x21
	mov	x19, x27
	mov	x20, x25
	bl	_memset
L153:
	mov	x1, x19
	mov	x0, x20
	mov	x2, x23
	add	x19, x19, 512
	bl	_anubis_mlkem_poly__poly_mul_ntt
	mov	x2, x22
	mov	x1, x23
	mov	x0, x21
	add	x20, x20, 512
	bl	_anubis_mlkem_poly__poly_add
	mov	x1, x22
	mov	x0, x21
	mov	x2, 512
	bl	_memcpy
	cmp	x19, x24
	bne	L153
	add	x26, x26, 4
	add	x21, x21, 512
	add	x25, x25, 2048
	cmp	x26, 16
	bne	L154
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, 1120
LCFI77:
	ret
LFE18:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_poly__matrix_t_vec_mul_ntt
_anubis_mlkem_poly__matrix_t_vec_mul_ntt:
LFB19:
	sub	sp, sp, #1120
LCFI78:
	stp	x29, x30, [sp]
LCFI79:
	mov	x29, sp
LCFI80:
	stp	x21, x22, [sp, 32]
LCFI81:
	mov	x21, x2
	add	x22, x29, 608
	stp	x23, x24, [sp, 48]
LCFI82:
	add	x23, x29, 96
	add	x24, x1, 2048
	stp	x25, x26, [sp, 64]
LCFI83:
	mov	x26, 0
	mov	x25, x0
	str	x27, [sp, 80]
LCFI84:
	mov	x27, x1
	stp	x19, x20, [sp, 16]
LCFI85:
L160:
	mov	x2, 512
	mov	w1, 0
	mov	x0, x21
	mov	x19, x27
	mov	x20, x25
	bl	_memset
L159:
	mov	x1, x19
	mov	x0, x20
	mov	x2, x23
	add	x19, x19, 512
	bl	_anubis_mlkem_poly__poly_mul_ntt
	mov	x2, x22
	mov	x1, x23
	mov	x0, x21
	add	x20, x20, 2048
	bl	_anubis_mlkem_poly__poly_add
	mov	x1, x22
	mov	x0, x21
	mov	x2, 512
	bl	_memcpy
	cmp	x19, x24
	bne	L159
	add	x26, x26, 1
	add	x21, x21, 512
	add	x25, x25, 512
	cmp	x26, 4
	bne	L160
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, 1120
LCFI86:
	ret
LFE19:
	.globl _anubis_mlkem_poly_E
	.data
	.align	1
_anubis_mlkem_poly_E:
	.space 2
	.literal16
	.align	4
lC7:
	.word	256
	.word	255
	.word	254
	.word	253
	.align	4
lC8:
	.word	-1
	.word	0
	.word	1
	.word	2
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
	.quad	LFB8-.
	.set L$set$2,LFE8-LFB8
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB8
	.long L$set$3
	.byte	0xe
	.uleb128 0x220
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0x9d
	.uleb128 0x44
	.byte	0x9e
	.uleb128 0x43
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0x93
	.uleb128 0x42
	.byte	0x94
	.uleb128 0x41
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$8,LEFDE3-LASFDE3
	.long L$set$8
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB11-.
	.set L$set$9,LFE11-LFB11
	.quad L$set$9
	.uleb128 0
	.byte	0x4
	.set L$set$10,LCFI5-LFB11
	.long L$set$10
	.byte	0xe
	.uleb128 0x220
	.byte	0x4
	.set L$set$11,LCFI6-LCFI5
	.long L$set$11
	.byte	0x9d
	.uleb128 0x44
	.byte	0x9e
	.uleb128 0x43
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0x93
	.uleb128 0x42
	.byte	0x94
	.uleb128 0x41
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$15,LEFDE5-LASFDE5
	.long L$set$15
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB42-.
	.set L$set$16,LFE42-LFB42
	.quad L$set$16
	.uleb128 0
	.byte	0x4
	.set L$set$17,LCFI10-LFB42
	.long L$set$17
	.byte	0xe
	.uleb128 0x230
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0x9d
	.uleb128 0x46
	.byte	0x9e
	.uleb128 0x45
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0x93
	.uleb128 0x44
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0xd3
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$22,LEFDE7-LASFDE7
	.long L$set$22
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB44-.
	.set L$set$23,LFE44-LFB44
	.quad L$set$23
	.uleb128 0
	.byte	0x4
	.set L$set$24,LCFI15-LFB44
	.long L$set$24
	.byte	0xe
	.uleb128 0x630
	.byte	0x4
	.set L$set$25,LCFI16-LCFI15
	.long L$set$25
	.byte	0x9d
	.uleb128 0xc6
	.byte	0x9e
	.uleb128 0xc5
	.byte	0x4
	.set L$set$26,LCFI17-LCFI16
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$27,LCFI18-LCFI17
	.long L$set$27
	.byte	0x93
	.uleb128 0xc4
	.byte	0x94
	.uleb128 0xc3
	.byte	0x4
	.set L$set$28,LCFI19-LCFI18
	.long L$set$28
	.byte	0x95
	.uleb128 0xc2
	.byte	0x4
	.set L$set$29,LCFI20-LCFI19
	.long L$set$29
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$30,LEFDE9-LASFDE9
	.long L$set$30
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB2-.
	.set L$set$31,LFE2-LFB2
	.quad L$set$31
	.uleb128 0
	.byte	0x4
	.set L$set$32,LCFI21-LFB2
	.long L$set$32
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$33,LCFI22-LCFI21
	.long L$set$33
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$34,LCFI23-LCFI22
	.long L$set$34
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$35,LCFI24-LCFI23
	.long L$set$35
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$36,LEFDE11-LASFDE11
	.long L$set$36
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB4-.
	.set L$set$37,LFE4-LFB4
	.quad L$set$37
	.uleb128 0
	.byte	0x4
	.set L$set$38,LCFI25-LFB4
	.long L$set$38
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$39,LCFI26-LCFI25
	.long L$set$39
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$40,LCFI27-LCFI26
	.long L$set$40
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$41,LCFI28-LCFI27
	.long L$set$41
	.byte	0xb
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$42,LEFDE13-LASFDE13
	.long L$set$42
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB6-.
	.set L$set$43,LFE6-LFB6
	.quad L$set$43
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI29-LFB6
	.long L$set$44
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$45,LCFI30-LCFI29
	.long L$set$45
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$46,LCFI31-LCFI30
	.long L$set$46
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xdd
	.byte	0xde
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$47,LEFDE15-LASFDE15
	.long L$set$47
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB7-.
	.set L$set$48,LFE7-LFB7
	.quad L$set$48
	.uleb128 0
	.byte	0x4
	.set L$set$49,LCFI32-LFB7
	.long L$set$49
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$50,LCFI33-LCFI32
	.long L$set$50
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$51,LCFI34-LCFI33
	.long L$set$51
	.byte	0x93
	.uleb128 0x4
	.byte	0x4
	.set L$set$52,LCFI35-LCFI34
	.long L$set$52
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$53,LEFDE17-LASFDE17
	.long L$set$53
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB9-.
	.set L$set$54,LFE9-LFB9
	.quad L$set$54
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI36-LFB9
	.long L$set$55
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$56,LCFI37-LCFI36
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$57,LCFI38-LCFI37
	.long L$set$57
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xdd
	.byte	0xde
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$58,LEFDE19-LASFDE19
	.long L$set$58
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB10-.
	.set L$set$59,LFE10-LFB10
	.quad L$set$59
	.uleb128 0
	.byte	0x4
	.set L$set$60,LCFI39-LFB10
	.long L$set$60
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$61,LCFI40-LCFI39
	.long L$set$61
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$62,LCFI41-LCFI40
	.long L$set$62
	.byte	0x93
	.uleb128 0x4
	.byte	0x4
	.set L$set$63,LCFI42-LCFI41
	.long L$set$63
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$64,LEFDE21-LASFDE21
	.long L$set$64
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB12-.
	.set L$set$65,LFE12-LFB12
	.quad L$set$65
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$66,LEFDE23-LASFDE23
	.long L$set$66
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB13-.
	.set L$set$67,LFE13-LFB13
	.quad L$set$67
	.uleb128 0
	.byte	0x4
	.set L$set$68,LCFI43-LFB13
	.long L$set$68
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$69,LCFI44-LCFI43
	.long L$set$69
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$70,LCFI45-LCFI44
	.long L$set$70
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$71,LCFI46-LCFI45
	.long L$set$71
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$72,LCFI47-LCFI46
	.long L$set$72
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$73,LEFDE25-LASFDE25
	.long L$set$73
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB14-.
	.set L$set$74,LFE14-LFB14
	.quad L$set$74
	.uleb128 0
	.byte	0x4
	.set L$set$75,LCFI48-LFB14
	.long L$set$75
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$76,LCFI49-LCFI48
	.long L$set$76
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$77,LCFI50-LCFI49
	.long L$set$77
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$78,LCFI51-LCFI50
	.long L$set$78
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$79,LCFI52-LCFI51
	.long L$set$79
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$80,LEFDE27-LASFDE27
	.long L$set$80
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB15-.
	.set L$set$81,LFE15-LFB15
	.quad L$set$81
	.uleb128 0
	.byte	0x4
	.set L$set$82,LCFI53-LFB15
	.long L$set$82
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$83,LCFI54-LCFI53
	.long L$set$83
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$84,LCFI55-LCFI54
	.long L$set$84
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$85,LCFI56-LCFI55
	.long L$set$85
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$86,LEFDE29-LASFDE29
	.long L$set$86
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB16-.
	.set L$set$87,LFE16-LFB16
	.quad L$set$87
	.uleb128 0
	.byte	0x4
	.set L$set$88,LCFI57-LFB16
	.long L$set$88
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$89,LCFI58-LCFI57
	.long L$set$89
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$90,LCFI59-LCFI58
	.long L$set$90
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$91,LCFI60-LCFI59
	.long L$set$91
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$92,LCFI61-LCFI60
	.long L$set$92
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$93,LEFDE31-LASFDE31
	.long L$set$93
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB17-.
	.set L$set$94,LFE17-LFB17
	.quad L$set$94
	.uleb128 0
	.byte	0x4
	.set L$set$95,LCFI62-LFB17
	.long L$set$95
	.byte	0xe
	.uleb128 0x440
	.byte	0x4
	.set L$set$96,LCFI63-LCFI62
	.long L$set$96
	.byte	0x9d
	.uleb128 0x88
	.byte	0x9e
	.uleb128 0x87
	.byte	0x4
	.set L$set$97,LCFI64-LCFI63
	.long L$set$97
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$98,LCFI65-LCFI64
	.long L$set$98
	.byte	0x93
	.uleb128 0x86
	.byte	0x94
	.uleb128 0x85
	.byte	0x4
	.set L$set$99,LCFI66-LCFI65
	.long L$set$99
	.byte	0x95
	.uleb128 0x84
	.byte	0x96
	.uleb128 0x83
	.byte	0x4
	.set L$set$100,LCFI67-LCFI66
	.long L$set$100
	.byte	0x97
	.uleb128 0x82
	.byte	0x98
	.uleb128 0x81
	.byte	0x4
	.set L$set$101,LCFI68-LCFI67
	.long L$set$101
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$102,LEFDE33-LASFDE33
	.long L$set$102
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB18-.
	.set L$set$103,LFE18-LFB18
	.quad L$set$103
	.uleb128 0
	.byte	0x4
	.set L$set$104,LCFI69-LFB18
	.long L$set$104
	.byte	0xe
	.uleb128 0x460
	.byte	0x4
	.set L$set$105,LCFI70-LCFI69
	.long L$set$105
	.byte	0x9d
	.uleb128 0x8c
	.byte	0x9e
	.uleb128 0x8b
	.byte	0x4
	.set L$set$106,LCFI71-LCFI70
	.long L$set$106
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$107,LCFI72-LCFI71
	.long L$set$107
	.byte	0x95
	.uleb128 0x88
	.byte	0x96
	.uleb128 0x87
	.byte	0x4
	.set L$set$108,LCFI73-LCFI72
	.long L$set$108
	.byte	0x97
	.uleb128 0x86
	.byte	0x98
	.uleb128 0x85
	.byte	0x4
	.set L$set$109,LCFI74-LCFI73
	.long L$set$109
	.byte	0x99
	.uleb128 0x84
	.byte	0x9a
	.uleb128 0x83
	.byte	0x4
	.set L$set$110,LCFI75-LCFI74
	.long L$set$110
	.byte	0x9b
	.uleb128 0x82
	.byte	0x4
	.set L$set$111,LCFI76-LCFI75
	.long L$set$111
	.byte	0x93
	.uleb128 0x8a
	.byte	0x94
	.uleb128 0x89
	.byte	0x4
	.set L$set$112,LCFI77-LCFI76
	.long L$set$112
	.byte	0xdb
	.byte	0xd9
	.byte	0xda
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$113,LEFDE35-LASFDE35
	.long L$set$113
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB19-.
	.set L$set$114,LFE19-LFB19
	.quad L$set$114
	.uleb128 0
	.byte	0x4
	.set L$set$115,LCFI78-LFB19
	.long L$set$115
	.byte	0xe
	.uleb128 0x460
	.byte	0x4
	.set L$set$116,LCFI79-LCFI78
	.long L$set$116
	.byte	0x9d
	.uleb128 0x8c
	.byte	0x9e
	.uleb128 0x8b
	.byte	0x4
	.set L$set$117,LCFI80-LCFI79
	.long L$set$117
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$118,LCFI81-LCFI80
	.long L$set$118
	.byte	0x95
	.uleb128 0x88
	.byte	0x96
	.uleb128 0x87
	.byte	0x4
	.set L$set$119,LCFI82-LCFI81
	.long L$set$119
	.byte	0x97
	.uleb128 0x86
	.byte	0x98
	.uleb128 0x85
	.byte	0x4
	.set L$set$120,LCFI83-LCFI82
	.long L$set$120
	.byte	0x99
	.uleb128 0x84
	.byte	0x9a
	.uleb128 0x83
	.byte	0x4
	.set L$set$121,LCFI84-LCFI83
	.long L$set$121
	.byte	0x9b
	.uleb128 0x82
	.byte	0x4
	.set L$set$122,LCFI85-LCFI84
	.long L$set$122
	.byte	0x93
	.uleb128 0x8a
	.byte	0x94
	.uleb128 0x89
	.byte	0x4
	.set L$set$123,LCFI86-LCFI85
	.long L$set$123
	.byte	0xdb
	.byte	0xd9
	.byte	0xda
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE35:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
