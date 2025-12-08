	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
_anubis_mlkem_encoding__byteencode_12___wrapped_statements.0:
LFB3:
	ldp	x5, x2, [x16]
	mov	x0, 0
	add	x7, x5, 1
	add	x6, x5, 2
	b	L2
	.p2align 2,,3
L3:
	add	x0, x0, 3
L2:
	ldrh	w4, [x2]
	add	x2, x2, 4
	ldrh	w1, [x2, -2]
	ubfx	x3, x4, 8, 4
	strb	w4, [x5, x0]
	ubfiz	w4, w1, 4, 4
	ubfx	x1, x1, 4, 8
	orr	w3, w3, w4
	strb	w3, [x7, x0]
	strb	w1, [x6, x0]
	cmp	x0, 381
	bne	L3
	ret
LFE3:
	.align	2
	.p2align 5,,15
_anubis_mlkem_encoding__encode_vector_12___wrapped_statements.9:
LFB20:
	stp	x29, x30, [sp, -432]!
LCFI0:
	mov	x29, sp
LCFI1:
	mov	x10, 0
	mov	x9, -384
	mov	x11, x16
	add	x8, x29, 48
	add	x12, x29, 24
L7:
	ldr	x0, [x11, 8]
	add	x1, x29, 432
	mov	x16, x12
	str	x1, [x29, 40]
	add	x0, x0, x10
	stp	x8, x0, [x29, 24]
	bl	_anubis_mlkem_encoding__byteencode_12___wrapped_statements.0
	ldr	x2, [x11]
	add	x1, x9, 385
	mov	x0, -1
	add	x2, x2, x1
	.p2align 5,,15
L6:
	add	x1, x8, x0
	ldrb	w1, [x1, 1]
	strb	w1, [x2, x0]
	add	x0, x0, 1
	cmp	x0, 383
	bne	L6
	add	x9, x9, 384
	add	x10, x10, 512
	cmp	x9, 1152
	bne	L7
	ldp	x29, x30, [sp], 432
LCFI2:
	ret
LFE20:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__byteencode_12
_anubis_mlkem_encoding__byteencode_12:
LFB2:
	stp	x29, x30, [sp, -48]!
LCFI3:
	mov	x29, sp
LCFI4:
	add	x16, x29, 16
	stp	x1, x0, [x29, 16]
	bl	_anubis_mlkem_encoding__byteencode_12___wrapped_statements.0
	ldp	x29, x30, [sp], 48
LCFI5:
	ret
LFE2:
	.const
	.align	3
lC4:
	.ascii "Loop_Invariant failed at anubis_mlkem_encoding.adb:59"
	.align	3
lC5:
	.ascii "failed postcondition from anubis_mlkem_encoding.ads:48"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__bytedecode_12
_anubis_mlkem_encoding__bytedecode_12:
LFB4:
	stp	x29, x30, [sp, -16]!
LCFI6:
	mov	x2, 0
	mov	x29, sp
LCFI7:
	mov	x9, x0
	mov	x8, x1
	mov	x6, x1
	add	x12, x1, 2
	add	x11, x0, 1
	add	x10, x0, 2
	mov	w7, -1
	mov	w4, 3329
	b	L14
	.p2align 2,,3
L15:
	ldrb	w0, [x10, x2]
	ldrb	w3, [x11, x2]
	ldrb	w1, [x9, x2]
	lsl	w0, w0, 4
	orr	w0, w0, w3, lsr 4
	ubfiz	w3, w3, 8, 4
	orr	w1, w1, w3
	udiv	w5, w0, w4
	udiv	w3, w1, w4
	msub	w0, w5, w4, w0
	strh	w0, [x6, 2]
	msub	w0, w3, w4, w1
	strh	w0, [x6]
	cmp	x2, 381
	beq	L29
	add	x2, x2, 3
	add	w7, w7, 2
	add	x6, x6, 4
L14:
	cmn	w7, #1
	beq	L15
	mov	x0, x8
	add	x3, x12, w7, uxtw 1
	b	L17
	.p2align 2,,3
L30:
	add	x0, x0, 2
	cmp	x3, x0
	beq	L15
L17:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L30
	adrp	x0, lC4@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L29:
	add	x1, x8, 512
	b	L20
	.p2align 2,,3
L32:
	add	x8, x8, 2
	cmp	x1, x8
	beq	L31
L20:
	ldrh	w0, [x8]
	cmp	w0, 3328
	bls	L32
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L31:
	ldp	x29, x30, [sp], 16
LCFI8:
	ret
LFE4:
	.const
	.align	2
lC1:
	.word	1
	.word	53
	.align	2
lC0:
	.word	1
	.word	54
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__byteencode_11
_anubis_mlkem_encoding__byteencode_11:
LFB6:
	mov	w9, 0
	.p2align 5,,15
L34:
	ldr	q31, [x0]
	add	w9, w9, 11
	add	x0, x0, 16
	add	x1, x1, 11
	bic	v31.8h, #248, lsl #8
	umov	w3, v31.h[0]
	umov	w6, v31.h[1]
	fmov	x2, d31
	umov	w5, v31.h[4]
	umov	w4, v31.h[5]
	umov	w7, v31.h[6]
	fmov	s30, w3
	ubfiz	w10, w6, 3, 13
	ubfx	x6, x6, 5, 11
	ubfx	x8, x2, 8, 8
	umov	w2, v31.h[2]
	orr	w8, w8, w10
	umov	w3, v31.h[3]
	ins	v30.b[1], w8
	ubfiz	w10, w2, 6, 10
	ubfx	x11, x2, 2, 14
	orr	w8, w6, w10
	ubfx	x2, x2, 10, 6
	ubfiz	w10, w3, 1, 15
	ubfx	x6, x3, 7, 9
	orr	w3, w2, w10
	ubfiz	w2, w5, 4, 12
	orr	w6, w6, w2
	ubfx	x5, x5, 4, 12
	umov	w2, v31.h[7]
	ubfiz	w10, w7, 2, 14
	ubfx	x7, x7, 6, 10
	ins	v30.b[2], w8
	ubfiz	w8, w4, 7, 9
	orr	w5, w5, w8
	ubfx	x8, x4, 1, 15
	ubfx	x4, x4, 9, 7
	orr	w4, w4, w10
	strb	w4, [x1, -3]
	ubfiz	w4, w2, 5, 11
	ubfx	x2, x2, 3, 13
	orr	w7, w7, w4
	strb	w2, [x1, -1]
	strb	w7, [x1, -2]
	ins	v30.b[3], w11
	ins	v30.b[4], w3
	ins	v30.b[5], w6
	ins	v30.b[6], w5
	ins	v30.b[7], w8
	str	d30, [x1, -11]
	cmp	w9, 352
	bne	L34
	ret
LFE6:
	.const
	.align	3
lC6:
	.ascii "Loop_Invariant failed at anubis_mlkem_encoding.adb:126"
	.align	3
lC7:
	.ascii "failed postcondition from anubis_mlkem_encoding.ads:68"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__bytedecode_11
_anubis_mlkem_encoding__bytedecode_11:
LFB9:
	stp	x29, x30, [sp, -48]!
LCFI9:
	mov	w7, -1
	mov	x29, sp
LCFI10:
	add	x3, x0, 1
	mov	w8, 0
	mov	x10, x1
	mov	x9, x1
	add	x11, x1, 2
	add	x4, x29, 16
	mov	x0, -1
	cmn	w7, #1
	bne	L53
	.p2align 5,,15
L41:
	ldrb	w2, [x3, x0]
	add	x1, x4, x0, lsl 1
	add	x0, x0, 1
	strh	w2, [x1, 2]
	cmp	x0, 10
	bne	L41
	ldrh	w1, [x29, 20]
	add	w8, w8, 11
	ldrh	w15, [x29, 16]
	ldrh	w14, [x29, 18]
	ubfiz	w5, w1, 5, 11
	ldrh	w0, [x29, 22]
	ldrh	w12, [x29, 26]
	orr	w5, w5, w14, lsr 3
	orr	w14, w15, w14, lsl 8
	ldrh	w2, [x29, 30]
	ubfiz	w0, w0, 2, 14
	fmov	d30, x5
	orr	w0, w0, w1, lsr 6
	fmov	d31, x14
	ubfiz	w6, w12, 7, 9
	ldrh	w1, [x29, 24]
	ubfiz	w2, w2, 1, 15
	ldrh	w13, [x29, 28]
	ldrh	w15, [x29, 36]
	orr	w0, w0, w1, lsl 10
	orr	w1, w6, w1, lsr 1
	ldrh	w6, [x29, 32]
	ins	v31.h[1], w0
	ins	v30.h[1], w1
	orr	w1, w2, w13, lsr 7
	ubfiz	w13, w13, 4, 12
	orr	w12, w13, w12, lsr 4
	ldrh	w2, [x29, 34]
	orr	w0, w1, w6, lsl 9
	ubfiz	w1, w15, 3, 13
	ins	v31.h[2], w12
	ins	v30.h[2], w0
	ubfiz	w0, w2, 6, 10
	orr	w2, w1, w2, lsr 5
	orr	w6, w0, w6, lsr 2
	ins	v31.h[3], w6
	ins	v30.h[3], w2
	zip1	v31.8h, v31.8h, v30.8h
	bic	v31.8h, #248, lsl #8
	str	q31, [x9]
	cmp	w8, 352
	beq	L54
	add	w7, w7, 8
	add	x9, x9, 16
	add	x3, x3, 11
	mov	x0, -1
	cmn	w7, #1
	beq	L41
L53:
	mov	x0, x10
	add	x2, x11, w7, uxtw 1
	b	L40
	.p2align 2,,3
L56:
	add	x0, x0, 2
	cmp	x0, x2
	beq	L55
L40:
	ldrh	w1, [x0]
	cmp	w1, 2047
	bls	L56
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L55:
	mov	x0, -1
	b	L41
L54:
	add	x1, x10, 512
	b	L44
	.p2align 2,,3
L58:
	add	x10, x10, 2
	cmp	x1, x10
	beq	L57
L44:
	ldrh	w0, [x10]
	cmp	w0, 2047
	bls	L58
	adrp	x0, lC7@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L57:
	ldp	x29, x30, [sp], 48
LCFI11:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__byteencode_5
_anubis_mlkem_encoding__byteencode_5:
LFB11:
	movi	v31.8b, 0x1f
	mov	w11, 0
	.p2align 5,,15
L60:
	ldr	q30, [x0]
	add	w11, w11, 5
	add	x0, x0, 16
	add	x1, x1, 5
	xtn	v30.8b, v30.8h
	and	v30.8b, v30.8b, v31.8b
	umov	w2, v30.b[1]
	umov	w6, v30.b[4]
	umov	w9, v30.b[5]
	umov	w10, v30.b[2]
	umov	w4, v30.b[3]
	umov	w3, v30.b[6]
	umov	w7, v30.b[7]
	ubfiz	w8, w2, 5, 3
	ubfx	x5, x6, 4, 4
	ubfx	x2, x2, 3, 5
	fmov	s29, w8
	ubfiz	w8, w9, 1, 7
	ubfiz	w6, w6, 4, 4
	ubfiz	w10, w10, 2, 6
	orr	w5, w5, w8
	ubfiz	w9, w4, 7, 1
	orr	w2, w2, w10
	ubfiz	w8, w3, 6, 2
	ubfx	x4, x4, 1, 7
	ubfx	x3, x3, 2, 6
	ubfiz	w7, w7, 3, 5
	orr	w2, w2, w9
	orr	w4, w4, w6
	orr	w5, w5, w8
	orr	w3, w3, w7
	strb	w2, [x1, -4]
	strb	w4, [x1, -3]
	orr	v30.8b, v29.8b, v30.8b
	strb	w5, [x1, -2]
	strb	w3, [x1, -1]
	str	b30, [x1, -5]
	cmp	w11, 160
	bne	L60
	ret
LFE11:
	.const
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_mlkem_encoding.adb:191"
	.align	3
lC9:
	.ascii "failed postcondition from anubis_mlkem_encoding.ads:88"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__bytedecode_5
_anubis_mlkem_encoding__bytedecode_5:
LFB14:
	stp	x29, x30, [sp, -32]!
LCFI12:
	movi	v30.4s, 0
	mov	x29, sp
LCFI13:
	mov	w6, 0
	mov	x8, x1
	mov	x7, x1
	add	x14, x1, 2
	mov	w5, -1
	b	L63
	.p2align 2,,3
L64:
	ldr	w2, [x0]
	add	w6, w6, 5
	ldrb	w3, [x0, 4]
	and	w4, w2, 31
	ubfx	x12, x2, 10, 5
	str	w2, [x29, 24]
	ubfx	x11, x2, 15, 5
	ubfx	x10, x2, 20, 5
	fmov	s31, w4
	ubfx	x4, x2, 5, 5
	ubfx	x9, x2, 25, 5
	ubfiz	w13, w3, 2, 6
	strb	w3, [x29, 28]
	orr	w2, w13, w2, lsr 30
	and	w2, w2, 31
	ins	v31.b[1], w4
	ubfx	x4, x3, 3, 5
	ins	v31.b[2], w12
	ins	v31.b[3], w11
	ins	v31.b[4], w10
	ins	v31.b[5], w9
	ins	v31.b[6], w2
	ins	v31.b[7], w4
	zip1	v31.16b, v31.16b, v30.16b
	str	q31, [x7]
	cmp	w6, 160
	beq	L78
	add	w5, w5, 8
	add	x0, x0, 5
	add	x7, x7, 16
L63:
	cmn	w5, #1
	beq	L64
	mov	x2, x1
	add	x4, x14, w5, uxtw 1
	b	L66
	.p2align 2,,3
L79:
	add	x2, x2, 2
	cmp	x2, x4
	beq	L64
L66:
	ldrh	w3, [x2]
	cmp	w3, 31
	bls	L79
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L78:
	add	x1, x1, 512
	b	L69
	.p2align 2,,3
L81:
	add	x8, x8, 2
	cmp	x8, x1
	beq	L80
L69:
	ldrh	w0, [x8]
	cmp	w0, 31
	bls	L81
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L80:
	ldp	x29, x30, [sp], 32
LCFI14:
	ret
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__byteencode_1
_anubis_mlkem_encoding__byteencode_1:
LFB16:
	movi	v31.16b, 0x1
	mov	x2, 0
L83:
	ldp	q30, q29, [x0]
	ldp	q1, q27, [x0, 32]
	ldp	q24, q0, [x0, 96]
	ldp	q26, q25, [x0, 64]
	ldp	q6, q5, [x0, 128]
	uzp1	v22.8h, v30.8h, v29.8h
	uzp2	v29.8h, v30.8h, v29.8h
	uzp1	v21.8h, v1.8h, v27.8h
	ldp	q4, q30, [x0, 160]
	uzp1	v2.8h, v24.8h, v0.8h
	uzp2	v27.8h, v1.8h, v27.8h
	uzp1	v18.8h, v22.8h, v21.8h
	ldp	q28, q1, [x0, 192]
	uzp2	v21.8h, v22.8h, v21.8h
	uzp2	v0.8h, v24.8h, v0.8h
	ldp	q24, q22, [x0, 224]
	add	x0, x0, 256
	uzp1	v3.8h, v26.8h, v25.8h
	uzp1	v16.8h, v29.8h, v27.8h
	uzp2	v25.8h, v26.8h, v25.8h
	uzp2	v27.8h, v29.8h, v27.8h
	uzp1	v17.8h, v3.8h, v2.8h
	uzp1	v29.8h, v6.8h, v5.8h
	uzp2	v2.8h, v3.8h, v2.8h
	uzp2	v5.8h, v6.8h, v5.8h
	uzp1	v3.8h, v28.8h, v1.8h
	uzp1	v6.8h, v24.8h, v22.8h
	uzp2	v1.8h, v28.8h, v1.8h
	uzp1	v20.8h, v4.8h, v30.8h
	uzp2	v22.8h, v24.8h, v22.8h
	uzp2	v30.8h, v4.8h, v30.8h
	uzp1	v7.8h, v25.8h, v0.8h
	uzp1	v4.8h, v29.8h, v20.8h
	uzp2	v20.8h, v29.8h, v20.8h
	uzp1	v24.8h, v5.8h, v30.8h
	uzp1	v29.8h, v1.8h, v22.8h
	uzp1	v28.8h, v3.8h, v6.8h
	uzp2	v0.8h, v25.8h, v0.8h
	uzp2	v30.8h, v5.8h, v30.8h
	uzp2	v22.8h, v1.8h, v22.8h
	uzp1	v5.8h, v16.8h, v7.8h
	uzp2	v6.8h, v3.8h, v6.8h
	uzp1	v25.8h, v24.8h, v29.8h
	uzp1	v26.8h, v21.8h, v2.8h
	uzp2	v1.8h, v27.8h, v0.8h
	uzp1	v3.8h, v20.8h, v6.8h
	uzp1	v25.16b, v5.16b, v25.16b
	uzp2	v19.8h, v30.8h, v22.8h
	uzp2	v5.8h, v18.8h, v17.8h
	uzp2	v23.8h, v4.8h, v28.8h
	uzp1	v0.8h, v27.8h, v0.8h
	uzp1	v22.8h, v30.8h, v22.8h
	uzp2	v7.8h, v16.8h, v7.8h
	uzp2	v29.8h, v24.8h, v29.8h
	uzp2	v2.8h, v21.8h, v2.8h
	uzp1	v3.16b, v26.16b, v3.16b
	uzp2	v6.8h, v20.8h, v6.8h
	uzp1	v22.16b, v0.16b, v22.16b
	uzp1	v23.16b, v5.16b, v23.16b
	and	v25.16b, v25.16b, v31.16b
	and	v3.16b, v3.16b, v31.16b
	uzp1	v29.16b, v7.16b, v29.16b
	uzp1	v6.16b, v2.16b, v6.16b
	and	v22.16b, v22.16b, v31.16b
	and	v23.16b, v23.16b, v31.16b
	shl	v25.16b, v25.16b, 1
	and	v29.16b, v29.16b, v31.16b
	shl	v3.16b, v3.16b, 2
	and	v6.16b, v6.16b, v31.16b
	uzp1	v19.16b, v1.16b, v19.16b
	shl	v22.16b, v22.16b, 3
	uzp1	v17.8h, v18.8h, v17.8h
	shl	v23.16b, v23.16b, 4
	orr	v3.16b, v25.16b, v3.16b
	uzp1	v28.8h, v4.8h, v28.8h
	shl	v29.16b, v29.16b, 5
	and	v19.16b, v19.16b, v31.16b
	shl	v6.16b, v6.16b, 6
	orr	v23.16b, v22.16b, v23.16b
	uzp1	v28.16b, v17.16b, v28.16b
	shl	v19.16b, v19.16b, 7
	orr	v6.16b, v3.16b, v6.16b
	orr	v29.16b, v23.16b, v29.16b
	and	v28.16b, v28.16b, v31.16b
	orr	v19.16b, v6.16b, v19.16b
	orr	v28.16b, v29.16b, v28.16b
	orr	v28.16b, v19.16b, v28.16b
	str	q28, [x1, x2]
	add	x2, x2, 16
	cmp	x2, 32
	bne	L83
	ret
LFE16:
	.const
	.align	3
lC10:
	.ascii "Loop_Invariant failed at anubis_mlkem_encoding.adb:244"
	.align	3
lC11:
	.ascii "Loop_Invariant failed at anubis_mlkem_encoding.adb:249"
	.align	3
lC12:
	.ascii "failed postcondition from anubis_mlkem_encoding.ads:105"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__bytedecode_1
_anubis_mlkem_encoding__bytedecode_1:
LFB17:
	mov	x8, x1
	mov	x11, x0
	stp	x29, x30, [sp, -16]!
LCFI15:
	mov	x6, x1
	add	x12, x1, 16
	add	x10, x1, 2
	mov	w9, -1
	mov	x29, sp
LCFI16:
	.p2align 5,,15
L93:
	cmn	w9, #1
	beq	L86
	sub	w2, w9, #7
	mov	x0, x8
	add	x2, x12, w2, uxtw 1
	b	L88
	.p2align 2,,3
L109:
	add	x0, x0, 2
	cmp	x0, x2
	beq	L86
L88:
	ldrh	w1, [x0]
	cmp	w1, 1
	bls	L109
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L86:
	ldrb	w7, [x11]
	mov	x4, 0
	mov	w5, w9
	.p2align 5,,15
L92:
	mov	w2, w4
	cmn	w5, #1
	beq	L89
	mov	x0, x8
	add	x3, x10, w5, uxtw 1
	b	L91
	.p2align 2,,3
L110:
	add	x0, x0, 2
	cmp	x3, x0
	beq	L89
L91:
	ldrh	w1, [x0]
	cmp	w1, 1
	bls	L110
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L89:
	lsr	w0, w7, w2
	add	w5, w5, 1
	and	x0, x0, 1
	strh	w0, [x6, x4, lsl 1]
	add	x4, x4, 1
	cmp	x4, 8
	bne	L92
	add	w9, w9, 8
	add	x6, x6, 16
	add	x11, x11, 1
	cmp	w9, 255
	bne	L93
	add	x1, x8, 512
	b	L95
	.p2align 2,,3
L112:
	add	x8, x8, 2
	cmp	x8, x1
	beq	L111
L95:
	ldrh	w0, [x8]
	cmp	w0, 1
	bls	L112
	adrp	x0, lC12@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L111:
	ldp	x29, x30, [sp], 16
LCFI17:
	ret
LFE17:
	.const
	.align	2
lC3:
	.word	1
	.word	55
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__encode_vector_12
_anubis_mlkem_encoding__encode_vector_12:
LFB19:
	stp	x29, x30, [sp, -48]!
LCFI18:
	mov	x29, sp
LCFI19:
	add	x16, x29, 16
	stp	x1, x0, [x29, 16]
	bl	_anubis_mlkem_encoding__encode_vector_12___wrapped_statements.9
	ldp	x29, x30, [sp], 48
LCFI20:
	ret
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_encoding__decode_vector_12
_anubis_mlkem_encoding__decode_vector_12:
LFB22:
	stp	x29, x30, [sp, -432]!
LCFI21:
	mov	x29, sp
LCFI22:
	stp	x19, x20, [sp, 16]
LCFI23:
	mov	x19, x0
	mov	x20, x1
	stp	x21, x22, [sp, 32]
LCFI24:
	add	x22, x0, 1536
	add	x21, x29, 48
L116:
	mov	x1, x19
	mov	x2, 384
	mov	x0, x21
	add	x19, x19, 384
	bl	_memcpy
	mov	x1, x20
	mov	x0, x21
	bl	_anubis_mlkem_encoding__bytedecode_12
	add	x20, x20, 512
	cmp	x22, x19
	bne	L116
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 432
LCFI25:
	ret
LFE22:
	.globl _anubis_mlkem_encoding_E
	.data
	.align	1
_anubis_mlkem_encoding_E:
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
	.quad	LFB3-.
	.set L$set$2,LFE3-LFB3
	.quad L$set$2
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$3,LEFDE3-LASFDE3
	.long L$set$3
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB20-.
	.set L$set$4,LFE20-LFB20
	.quad L$set$4
	.uleb128 0
	.byte	0x4
	.set L$set$5,LCFI0-LFB20
	.long L$set$5
	.byte	0xe
	.uleb128 0x1b0
	.byte	0x9d
	.uleb128 0x36
	.byte	0x9e
	.uleb128 0x35
	.byte	0x4
	.set L$set$6,LCFI1-LCFI0
	.long L$set$6
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$7,LCFI2-LCFI1
	.long L$set$7
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$8,LEFDE5-LASFDE5
	.long L$set$8
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB2-.
	.set L$set$9,LFE2-LFB2
	.quad L$set$9
	.uleb128 0
	.byte	0x4
	.set L$set$10,LCFI3-LFB2
	.long L$set$10
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$11,LCFI4-LCFI3
	.long L$set$11
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$13,LEFDE7-LASFDE7
	.long L$set$13
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB4-.
	.set L$set$14,LFE4-LFB4
	.quad L$set$14
	.uleb128 0
	.byte	0x4
	.set L$set$15,LCFI6-LFB4
	.long L$set$15
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$16,LCFI7-LCFI6
	.long L$set$16
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$17,LCFI8-LCFI7
	.long L$set$17
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$18,LEFDE9-LASFDE9
	.long L$set$18
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB6-.
	.set L$set$19,LFE6-LFB6
	.quad L$set$19
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$20,LEFDE11-LASFDE11
	.long L$set$20
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB9-.
	.set L$set$21,LFE9-LFB9
	.quad L$set$21
	.uleb128 0
	.byte	0x4
	.set L$set$22,LCFI9-LFB9
	.long L$set$22
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$23,LCFI10-LCFI9
	.long L$set$23
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$24,LCFI11-LCFI10
	.long L$set$24
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$25,LEFDE13-LASFDE13
	.long L$set$25
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB11-.
	.set L$set$26,LFE11-LFB11
	.quad L$set$26
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$27,LEFDE15-LASFDE15
	.long L$set$27
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB14-.
	.set L$set$28,LFE14-LFB14
	.quad L$set$28
	.uleb128 0
	.byte	0x4
	.set L$set$29,LCFI12-LFB14
	.long L$set$29
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$30,LCFI13-LCFI12
	.long L$set$30
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$31,LCFI14-LCFI13
	.long L$set$31
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$32,LEFDE17-LASFDE17
	.long L$set$32
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB16-.
	.set L$set$33,LFE16-LFB16
	.quad L$set$33
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$34,LEFDE19-LASFDE19
	.long L$set$34
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB17-.
	.set L$set$35,LFE17-LFB17
	.quad L$set$35
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI15-LFB17
	.long L$set$36
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$37,LCFI16-LCFI15
	.long L$set$37
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$38,LCFI17-LCFI16
	.long L$set$38
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$39,LEFDE21-LASFDE21
	.long L$set$39
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB19-.
	.set L$set$40,LFE19-LFB19
	.quad L$set$40
	.uleb128 0
	.byte	0x4
	.set L$set$41,LCFI18-LFB19
	.long L$set$41
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$42,LCFI19-LCFI18
	.long L$set$42
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$43,LCFI20-LCFI19
	.long L$set$43
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$44,LEFDE23-LASFDE23
	.long L$set$44
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB22-.
	.set L$set$45,LFE22-LFB22
	.quad L$set$45
	.uleb128 0
	.byte	0x4
	.set L$set$46,LCFI21-LFB22
	.long L$set$46
	.byte	0xe
	.uleb128 0x1b0
	.byte	0x9d
	.uleb128 0x36
	.byte	0x9e
	.uleb128 0x35
	.byte	0x4
	.set L$set$47,LCFI22-LCFI21
	.long L$set$47
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$48,LCFI23-LCFI22
	.long L$set$48
	.byte	0x93
	.uleb128 0x34
	.byte	0x94
	.uleb128 0x33
	.byte	0x4
	.set L$set$49,LCFI24-LCFI23
	.long L$set$49
	.byte	0x95
	.uleb128 0x32
	.byte	0x96
	.uleb128 0x31
	.byte	0x4
	.set L$set$50,LCFI25-LCFI24
	.long L$set$50
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
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
