	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
_anubis_mlkem__keygen___wrapped_statements.0:
LFB6:
	mov	x12, 23744
	mov	x0, 15552
	sub	sp, sp, x12
LCFI0:
	adrp	x1, lC1@PAGE
	mov	x3, 5312
	add	x1, x1, lC1@PAGEOFF;
	stp	x29, x30, [sp]
LCFI1:
	mov	x29, sp
LCFI2:
	stp	x23, x24, [sp, 48]
LCFI3:
	add	x23, x29, x0
	add	x24, x29, x3
	mov	x2, x23
	stp	x19, x20, [sp, 16]
LCFI4:
	add	x20, x29, 96
	add	x19, x29, 3264
	stp	x21, x22, [sp, 32]
LCFI5:
	mov	x21, x16
	add	x22, x29, 64
	ldr	x0, [x16, 24]
	bl	_anubis_sha3__sha3_512
	ldr	q28, [x29, 15552]
	mov	x1, x23
	mov	x0, x22
	ldr	q29, [x29, 15568]
	ldr	q30, [x29, 15584]
	ldr	q31, [x29, 15600]
	stp	q28, q29, [x29, 64]
	stp	q30, q31, [x29, 96]
	bl	_anubis_mlkem_sample__generate_matrix
	mov	x0, x20
	mov	x2, x19
	mov	w1, 0
	bl	_anubis_mlkem_sample__sample_vector_cbd
	mov	x0, x20
	mov	x2, x24
	mov	w1, 4
	bl	_anubis_mlkem_sample__sample_vector_cbd
	mov	x4, 7360
	mov	x1, x19
	add	x20, x29, x4
	mov	x2, 2048
	mov	x0, x20
	bl	_memcpy
	mov	x0, x20
	bl	_anubis_mlkem_poly__vec_ntt
	mov	x5, 9408
	mov	x1, x24
	add	x19, x29, x5
	mov	x2, 2048
	mov	x0, x19
	bl	_memcpy
	mov	x0, x19
	bl	_anubis_mlkem_poly__vec_ntt
	mov	x6, 13504
	mov	x0, x23
	add	x24, x29, x6
	mov	x1, x20
	mov	x2, x24
	bl	_anubis_mlkem_poly__matrix_vec_mul_ntt
	mov	x7, 11456
	mov	x1, x19
	add	x23, x29, x7
	mov	x0, x24
	mov	x2, x23
	add	x19, x29, 192
	bl	_anubis_mlkem_poly__vec_add
	mov	x0, x23
	bl	_anubis_mlkem_poly__poly_reduce
	add	x0, x23, 512
	bl	_anubis_mlkem_poly__poly_reduce
	add	x0, x23, 1024
	bl	_anubis_mlkem_poly__poly_reduce
	add	x0, x23, 1536
	bl	_anubis_mlkem_poly__poly_reduce
	mov	x0, x23
	mov	x1, x19
	bl	_anubis_mlkem_encoding__encode_vector_12
	ldr	x3, [x21, 16]
	mov	x0, -1
	add	x1, x3, 1
	.p2align 5,,15
L2:
	add	x2, x19, x0
	ldrb	w2, [x2, 1]
	strb	w2, [x1, x0]
	add	x0, x0, 1
	cmp	x0, 1535
	bne	L2
	add	x2, x3, 1537
	mov	x0, -1
	.p2align 5,,15
L3:
	add	x1, x22, x0
	ldrb	w1, [x1, 1]
	strb	w1, [x2, x0]
	add	x0, x0, 1
	cmp	x0, 31
	bne	L3
	add	x19, x29, 1728
	mov	x0, x20
	mov	x1, x19
	bl	_anubis_mlkem_encoding__encode_vector_12
	ldr	x3, [x21, 8]
	mov	x0, -1
	add	x1, x3, 1
	.p2align 5,,15
L4:
	add	x2, x19, x0
	ldrb	w2, [x2, 1]
	strb	w2, [x1, x0]
	add	x0, x0, 1
	cmp	x0, 1535
	bne	L4
	ldr	x0, [x21, 16]
	mov	x1, 0
	add	x3, x3, 1536
	.p2align 5,,15
L5:
	ldrb	w2, [x0, x1]
	strb	w2, [x3, x1]
	add	x1, x1, 1
	cmp	x1, 1568
	bne	L5
	adrp	x1, lC3@PAGE
	add	x2, x29, 160
	add	x1, x1, lC3@PAGEOFF;
	bl	_anubis_sha3__sha3_256
	ldp	q30, q31, [x29, 160]
	mov	x0, 0
	ldr	x1, [x21, 8]
	add	x3, x1, 3136
	str	q30, [x1, 3104]
	str	q31, [x1, 3120]
	ldr	x2, [x21]
	stp	q30, q31, [x29, 128]
	.p2align 5,,15
L6:
	ldrb	w1, [x2, x0]
	strb	w1, [x3, x0]
	add	x0, x0, 1
	cmp	x0, 32
	bne	L6
	mov	x12, 23744
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, x12
LCFI6:
	ret
LFE6:
	.const
	.align	2
lC1:
	.word	0
	.word	31
	.align	2
lC3:
	.word	0
	.word	1567
	.text
	.align	2
	.p2align 5,,15
_anubis_mlkem__k_pke_encrypt___wrapped_statements.3:
LFB10:
	mov	x12, 28496
	mov	x9, 4432
	sub	sp, sp, x12
LCFI7:
	mov	x1, -1
	stp	x29, x30, [sp]
LCFI8:
	mov	x29, sp
LCFI9:
	add	x0, x29, x9
	stp	x19, x20, [sp, 16]
LCFI10:
	mov	x20, x16
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	ldr	x4, [x16, 24]
	str	x25, [sp, 64]
LCFI11:
	add	x4, x4, 1
	.p2align 5,,15
L15:
	ldrb	w3, [x4, x1]
	add	x2, x0, x1
	add	x1, x1, 1
	strb	w3, [x2, 1]
	cmp	x1, 1535
	bne	L15
	mov	x8, 5968
	add	x21, x29, x8
	mov	x1, x21
	bl	_anubis_mlkem_encoding__decode_vector_12
	ldr	x4, [x20, 24]
	add	x0, x29, 80
	mov	x1, -1
	add	x4, x4, 1537
	.p2align 5,,15
L16:
	ldrb	w3, [x4, x1]
	add	x2, x0, x1
	add	x1, x1, 1
	strb	w3, [x2, 1]
	cmp	x1, 31
	bne	L16
	mov	x1, 20304
	add	x24, x29, 464
	add	x23, x29, x1
	mov	x1, x23
	bl	_anubis_mlkem_sample__generate_matrix_transpose
	mov	x2, 8016
	ldr	x0, [x20, 16]
	mov	x3, 10064
	add	x19, x29, x2
	mov	x4, 12112
	mov	x2, x19
	mov	w1, 0
	add	x22, x29, x4
	add	x25, x29, x3
	bl	_anubis_mlkem_sample__sample_vector_cbd
	ldr	x0, [x20, 16]
	mov	x2, x25
	mov	w1, 4
	bl	_anubis_mlkem_sample__sample_vector_cbd
	ldr	x0, [x20, 16]
	mov	x2, x24
	mov	w1, 8
	bl	_anubis_mlkem_sample__sample_poly_cbd
	mov	x1, x19
	mov	x2, 2048
	mov	x0, x22
	bl	_memcpy
	mov	x0, x22
	bl	_anubis_mlkem_poly__vec_ntt
	mov	x5, 16208
	mov	x1, x22
	add	x19, x29, x5
	mov	x0, x23
	mov	x2, x19
	bl	_anubis_mlkem_poly__matrix_vec_mul_ntt
	mov	x0, x19
	bl	_anubis_mlkem_poly__vec_intt
	mov	x6, 14160
	mov	x1, x25
	add	x23, x29, x6
	mov	x0, x19
	mov	x2, x23
	add	x19, x29, 1488
	bl	_anubis_mlkem_poly__vec_add
	mov	x1, x22
	mov	x2, x19
	mov	x0, x21
	add	x22, x29, 976
	bl	_anubis_mlkem_poly__vec_dot_product_ntt
	mov	x0, x19
	add	x21, x29, 2512
	bl	_anubis_mlkem_poly__poly_intt
	mov	x2, x22
	mov	x1, x24
	mov	x0, x19
	add	x24, x29, 3024
	bl	_anubis_mlkem_poly__poly_add
	ldr	x0, [x20, 8]
	mov	x1, x21
	add	x25, x29, 2000
	bl	_anubis_mlkem_poly__msg_to_poly
	mov	x2, x19
	mov	x1, x21
	mov	x0, x22
	bl	_anubis_mlkem_poly__poly_add
	mov	x1, x19
	mov	x2, 512
	mov	x0, x22
	add	x19, x29, 112
	bl	_memcpy
	mov	x7, 18256
	mov	x0, x23
	add	x21, x29, x7
	add	x23, x24, 1408
	mov	x1, x21
	bl	_anubis_mlkem_compress__compress_vector_du
	mov	x0, x22
	mov	x1, x25
	mov	x22, x24
	bl	_anubis_mlkem_compress__compress_dv
L17:
	mov	x0, x21
	mov	x1, x19
	bl	_anubis_mlkem_encoding__byteencode_11
	mov	x0, x22
	mov	x1, x19
	mov	x2, 352
	add	x22, x22, 352
	bl	_memcpy
	add	x21, x21, 512
	cmp	x23, x22
	bne	L17
	mov	x0, x25
	mov	x1, x19
	bl	_anubis_mlkem_encoding__byteencode_5
	ldr	x3, [x20]
	mov	x2, 1408
	mov	x1, x24
	mov	x0, x3
	bl	_memcpy
	add	x2, x0, 1409
	mov	x0, -1
	.p2align 5,,15
L18:
	add	x1, x19, x0
	ldrb	w1, [x1, 1]
	strb	w1, [x2, x0]
	add	x0, x0, 1
	cmp	x0, 159
	bne	L18
	mov	x12, 28496
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, x12
LCFI12:
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem__keygen
_anubis_mlkem__keygen:
LFB5:
	stp	x29, x30, [sp, -64]!
LCFI13:
	mov	x29, sp
LCFI14:
	add	x4, x29, 64
	add	x16, x29, 16
	stp	x1, x3, [x29, 16]
	stp	x2, x0, [x29, 32]
	str	x4, [x29, 48]
	bl	_anubis_mlkem__keygen___wrapped_statements.0
	ldp	x29, x30, [sp], 64
LCFI15:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem__k_pke_encrypt
_anubis_mlkem__k_pke_encrypt:
LFB9:
	stp	x29, x30, [sp, -64]!
LCFI16:
	mov	x29, sp
LCFI17:
	add	x4, x29, 64
	add	x16, x29, 16
	stp	x3, x1, [x29, 16]
	stp	x2, x0, [x29, 32]
	str	x4, [x29, 48]
	bl	_anubis_mlkem__k_pke_encrypt___wrapped_statements.3
	ldp	x29, x30, [sp], 64
LCFI18:
	ret
LFE9:
	.const
	.align	3
lC7:
	.ascii "failed precondition from anubis_mlkem.ads:131"
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_mlkem.adb:284"
	.align	3
lC9:
	.ascii "Loop_Invariant failed at anubis_mlkem.adb:297"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem__k_pke_decrypt
_anubis_mlkem__k_pke_decrypt:
LFB12:
	mov	x12, 9808
	mov	x4, x0
	sub	sp, sp, x12
LCFI19:
	stp	x29, x30, [sp]
LCFI20:
	mov	x29, sp
LCFI21:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI22:
	ldp	w5, w7, [x1]
	sxtw	x0, w5
	add	x0, x0, 1535
	cmp	x0, w7, sxtw
	bne	L54
	dup	v25.4s, w5
	adrp	x0, lC10@PAGE
	adrp	x1, lC11@PAGE
	mov	x19, x2
	mov	x20, x3
	dup	v26.4s, w7
	mov	x2, x4
	ldr	q23, [x0, #lC10@PAGEOFF]
	add	x0, x29, 2128
	mvni	v16.4s, 0xf
	movi	v17.4s, 0x10
	add	x3, x0, 1536
	movi	v18.4s, 0x1
	movi	v19.4s, 0xd
	ldr	q24, [x1, #lC11@PAGEOFF]
	movi	v20.4s, 0x5
	movi	v21.4s, 0x9
	mov	x1, x0
	.p2align 5,,15
L32:
	mov	v29.16b, v24.16b
	mov	v22.16b, v23.16b
	add	v24.4s, v24.4s, v17.4s
	add	v23.4s, v23.4s, v16.4s
	add	v31.4s, v29.4s, v18.4s
	add	v27.4s, v29.4s, v19.4s
	add	v30.4s, v29.4s, v20.4s
	add	v28.4s, v29.4s, v21.4s
	add	v31.4s, v31.4s, v25.4s
	add	v27.4s, v27.4s, v25.4s
	add	v30.4s, v30.4s, v25.4s
	add	v28.4s, v28.4s, v25.4s
	cmgt	v31.4s, v31.4s, v26.4s
	cmgt	v27.4s, v27.4s, v26.4s
	cmgt	v30.4s, v30.4s, v26.4s
	cmgt	v28.4s, v28.4s, v26.4s
	orr	v31.16b, v31.16b, v27.16b
	orr	v30.16b, v30.16b, v28.16b
	orr	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x6, d31
	cbnz	x6, L55
	ldr	q31, [x2], 16
	str	q31, [x1], 16
	cmp	x1, x3
	bne	L32
L33:
	add	x23, x29, 3664
	mov	x26, x19
	mov	x1, x23
	mov	x25, -1
	bl	_anubis_mlkem_encoding__decode_vector_12
	mov	x0, 5712
	mov	w1, 0
	add	x24, x29, x0
	mov	x2, 2048
	mov	x0, x24
	mov	x22, x24
	bl	_memset
	mov	x1, 7760
	add	x21, x29, x1
L41:
	mov	w4, w25
	cmn	w25, #1
	beq	L35
L58:
	mov	x2, x24
	mov	w3, -1
	.p2align 5,,15
L38:
	mov	x0, 0
	add	w3, w3, 1
	b	L37
	.p2align 2,,3
L57:
	add	x0, x0, 1
	cmp	x0, 256
	beq	L56
L37:
	ldrh	w1, [x2, x0, lsl 1]
	cmp	w1, 2047
	bls	L57
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L56:
	add	x2, x2, 512
	cmp	w3, w4
	bne	L38
	mov	x2, 352
	mov	x1, x26
	mov	x0, x21
	add	x25, x25, 1
	bl	_memcpy
	mov	x1, x22
	mov	x0, x21
	bl	_anubis_mlkem_encoding__bytedecode_11
	cmp	x25, 3
	beq	L40
	add	x26, x26, 352
	add	x22, x22, 512
	mov	w4, w25
	cmn	w25, #1
	bne	L58
L35:
	mov	x1, x26
	mov	x2, 352
	mov	x0, x21
	add	x26, x26, 352
	bl	_memcpy
	mov	x1, x22
	mov	x0, x21
	add	x22, x22, 512
	mov	x25, 0
	bl	_anubis_mlkem_encoding__bytedecode_11
	b	L41
L55:
	fmov	w1, s29
	add	w5, w5, 1
	fmov	w2, s22
	add	x4, x4, 1
	sxtw	x1, w1
	sub	w2, w2, #1
	add	x6, x1, 1
	add	x6, x6, x2
	.p2align 5,,15
L31:
	add	w2, w5, w1
	cmp	w7, w2
	blt	L34
	ldrb	w3, [x4, x1]
	add	x2, x0, x1
	add	x1, x1, 1
	strb	w3, [x2, 1]
	cmp	x6, x1
	bne	L31
	b	L33
L40:
	add	x2, x19, 1408
	ldr	q30, [x19, 1408]
	add	x19, x29, 592
	mov	x1, x19
	mov	x0, x21
	ldp	q31, q28, [x2, 16]
	add	x22, x29, 1104
	str	q30, [x29, 7760]
	ldp	q30, q29, [x2, 48]
	stp	q31, q28, [x21, 16]
	ldp	q31, q28, [x2, 80]
	stp	q30, q29, [x21, 48]
	ldp	q30, q29, [x2, 112]
	stp	q31, q28, [x21, 80]
	ldr	q31, [x2, 144]
	stp	q30, q29, [x21, 112]
	str	q31, [x21, 144]
	bl	_anubis_mlkem_encoding__bytedecode_5
	mov	x1, x21
	mov	x0, x24
	bl	_anubis_mlkem_compress__decompress_vector_du
	mov	x0, x19
	mov	x1, x22
	bl	_anubis_mlkem_compress__decompress_dv
	mov	x0, x21
	add	x19, x29, 1616
	bl	_anubis_mlkem_poly__vec_ntt
	mov	x2, x19
	mov	x1, x21
	mov	x0, x23
	bl	_anubis_mlkem_poly__vec_dot_product_ntt
	mov	x0, x19
	bl	_anubis_mlkem_poly__poly_intt
	add	x4, x29, 80
	mov	x1, x19
	mov	x2, x4
	mov	x0, x22
	mov	x19, x4
	bl	_anubis_mlkem_poly__poly_sub
	mov	w5, 30337
	mov	x6, 0
	mov	x4, x19
	movk	w5, 0x3afb, lsl 16
	.p2align 5,,15
L43:
	mov	x1, 0
	mov	w3, 0
	.p2align 5,,15
L42:
	ldrh	w0, [x4, x1, lsl 1]
	add	w0, w0, 832
	lsl	w0, w0, 1
	umull	x2, w0, w5
	lsr	x2, x2, 32
	sub	w0, w0, w2
	add	w0, w2, w0, lsr 1
	ubfx	x0, x0, 11, 1
	lsl	w0, w0, w1
	add	x1, x1, 1
	orr	w3, w3, w0
	and	w3, w3, 255
	cmp	x1, 8
	bne	L42
	strb	w3, [x20, x6]
	add	x6, x6, 1
	add	x4, x4, 16
	cmp	x6, 32
	bne	L43
	mov	x12, 9808
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, x12
LCFI23:
	ret
L34:
LCFI24:
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L54:
	adrp	x0, lC7@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE12:
	.const
	.align	2
lC0:
	.word	1
	.word	45
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem__encaps
_anubis_mlkem__encaps:
LFB13:
	movi	v31.4s, 0
	stp	x29, x30, [sp, -480]!
LCFI25:
	mov	x29, sp
LCFI26:
	stp	x21, x22, [sp, 32]
LCFI27:
	add	x22, x29, 288
	add	x21, x29, 96
	stp	x19, x20, [sp, 16]
LCFI28:
	add	x20, x29, 352
	add	x19, x29, 416
	stp	x23, x24, [sp, 48]
LCFI29:
	mov	x23, x2
	mov	x2, x19
	mov	x24, x3
	stp	x25, x26, [sp, 64]
LCFI30:
	mov	x25, x0
	mov	x0, x1
	adrp	x26, lC5@PAGE
	add	x26, x26, lC5@PAGEOFF;
	stp	x27, x28, [sp, 80]
LCFI31:
	adrp	x27, lC1@PAGE
	adrp	x28, lC3@PAGE
	add	x27, x27, lC1@PAGEOFF;
	add	x28, x28, lC3@PAGEOFF;
	mov	x1, x27
	stp	q31, q31, [x22]
	stp	q31, q31, [x20]
	stp	q31, q31, [x22, 32]
	stp	q31, q31, [x20, 32]
	bl	_anubis_sha3__sha3_256
	ldp	q31, q30, [x29, 416]
	mov	x2, x19
	mov	x0, x25
	mov	x1, x28
	stp	q31, q30, [x21]
	bl	_anubis_sha3__sha3_256
	ldp	q28, q31, [x29, 416]
	mov	x0, x22
	mov	x1, x26
	mov	x2, x19
	ldp	q30, q29, [x21]
	str	q28, [x29, 320]
	stp	q30, q29, [x22]
	str	q31, [x29, 336]
	bl	_anubis_sha3__sha3_512
	ldp	q29, q28, [x29, 416]
	add	x2, x29, 480
	add	x1, x29, 160
	add	x0, x29, 192
	stp	x24, x21, [x29, 416]
	mov	x16, x19
	mov	x21, x1
	add	x19, x29, 256
	ldp	q31, q30, [x29, 448]
	str	x2, [x29, 448]
	stp	q29, q28, [x1]
	stp	q31, q30, [x0]
	stp	x0, x25, [x29, 432]
	bl	_anubis_mlkem__k_pke_encrypt___wrapped_statements.3
	mov	x0, x24
	mov	x1, x28
	mov	x2, x19
	bl	_anubis_sha3__sha3_256
	ldp	q28, q31, [x19]
	mov	x2, x23
	mov	x3, x27
	mov	x1, x26
	mov	x0, x20
	ldp	q30, q29, [x21]
	mov	w4, 32
	str	q28, [x29, 384]
	stp	q30, q29, [x20]
	str	q31, [x29, 400]
	bl	_anubis_sha3__shake256
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 480
LCFI32:
	ret
LFE13:
	.const
	.align	2
lC5:
	.word	0
	.word	63
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem__decaps
_anubis_mlkem__decaps:
LFB14:
	mov	x12, 5152
	movi	v31.4s, 0
	sub	sp, sp, x12
LCFI33:
	stp	x29, x30, [sp]
LCFI34:
	mov	x29, sp
LCFI35:
	stp	x19, x20, [sp, 16]
LCFI36:
	add	x20, x29, 480
	mov	x19, x0
	stp	x21, x22, [sp, 32]
LCFI37:
	mov	x21, x1
	mov	x22, x2
	mov	x1, x0
	mov	x2, 1536
	mov	x0, x20
	stp	x23, x24, [sp, 48]
LCFI38:
	add	x24, x29, 3584
	adrp	x23, lC5@PAGE
	stp	x25, x26, [sp, 64]
LCFI39:
	add	x25, x29, 176
	add	x26, x29, 416
	stp	q31, q31, [x29, 352]
	stp	q31, q31, [x29, 384]
	stp	q31, q31, [x29, 416]
	stp	q31, q31, [x29, 448]
	bl	_memcpy
	add	x1, x19, 1536
	mov	x2, 1568
	mov	x0, x24
	bl	_memcpy
	ldr	q28, [x19, 3104]
	adrp	x1, lC6@PAGE
	mov	x0, x20
	mov	x3, x25
	mov	x2, x21
	ldr	q29, [x19, 3120]
	add	x1, x1, lC6@PAGEOFF;
	add	x20, x29, 2016
	ldr	q30, [x19, 3136]
	ldr	q31, [x19, 3152]
	mov	w19, 1
	stp	q28, q29, [x29, 144]
	stp	q30, q31, [x29, 80]
	bl	_anubis_mlkem__k_pke_decrypt
	ldp	q28, q29, [x29, 144]
	mov	x0, x26
	mov	x2, x20
	add	x1, x23, lC5@PAGEOFF;
	ldp	q30, q31, [x29, 176]
	stp	q28, q29, [x29, 448]
	stp	q30, q31, [x29, 416]
	bl	_anubis_sha3__sha3_512
	ldr	q31, [x20, 16]
	add	x0, x29, 208
	add	x16, x29, 304
	stp	x20, x25, [x29, 304]
	ldr	q28, [x29, 2016]
	stp	x0, x24, [x29, 320]
	mov	x0, 5152
	add	x0, x29, x0
	ldr	q29, [x29, 2048]
	str	x0, [x29, 336]
	ldr	q30, [x29, 2064]
	stp	q28, q31, [x29, 112]
	stp	q29, q30, [x29, 208]
	bl	_anubis_mlkem__k_pke_encrypt___wrapped_statements.3
	add	x5, x21, 1
	mov	x0, -1
	.p2align 5,,15
L62:
	add	x3, x20, x0
	ldrb	w4, [x5, x0]
	cmp	w19, 0
	add	x0, x0, 1
	ldrb	w3, [x3, 1]
	ccmp	w4, w3, 0, ne
	cset	w19, eq
	cmp	x0, 1567
	bne	L62
	adrp	x1, lC3@PAGE
	mov	x0, x21
	add	x2, x29, 272
	add	x1, x1, lC3@PAGEOFF;
	bl	_anubis_sha3__sha3_256
	fmov	s31, w19
	adrp	x3, lC1@PAGE
	mov	x2, x22
	ldp	q0, q29, [x29, 80]
	add	x1, x23, lC5@PAGEOFF;
	add	x0, x29, 352
	add	x3, x3, lC1@PAGEOFF;
	mov	w4, 32
	ldp	q1, q30, [x29, 112]
	ldp	q27, q28, [x29, 272]
	neg	v31.2s, v31.2s
	dup	v31.16b, v31.b[0]
	stp	q27, q28, [x29, 384]
	bit	v0.16b, v1.16b, v31.16b
	bit	v29.16b, v30.16b, v31.16b
	stp	q0, q29, [x29, 352]
	bl	_anubis_sha3__shake256
	mov	x12, 5152
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, x12
LCFI40:
	ret
LFE14:
	.const
	.align	2
lC6:
	.word	0
	.word	1535
	.text
	.globl _anubis_mlkem_E
	.data
	.align	1
_anubis_mlkem_E:
	.space 2
	.literal16
	.align	4
lC10:
	.word	1536
	.word	1535
	.word	1534
	.word	1533
	.align	4
lC11:
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
	.quad	LFB6-.
	.set L$set$2,LFE6-LFB6
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB6
	.long L$set$3
	.byte	0xe
	.uleb128 0x5cc0
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0x9d
	.uleb128 0xb98
	.byte	0x9e
	.uleb128 0xb97
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0x97
	.uleb128 0xb92
	.byte	0x98
	.uleb128 0xb91
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0x93
	.uleb128 0xb96
	.byte	0x94
	.uleb128 0xb95
	.byte	0x4
	.set L$set$8,LCFI5-LCFI4
	.long L$set$8
	.byte	0x95
	.uleb128 0xb94
	.byte	0x96
	.uleb128 0xb93
	.byte	0x4
	.set L$set$9,LCFI6-LCFI5
	.long L$set$9
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
LEFDE1:
LSFDE3:
	.set L$set$10,LEFDE3-LASFDE3
	.long L$set$10
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB10-.
	.set L$set$11,LFE10-LFB10
	.quad L$set$11
	.uleb128 0
	.byte	0x4
	.set L$set$12,LCFI7-LFB10
	.long L$set$12
	.byte	0xe
	.uleb128 0x6f50
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0x9d
	.uleb128 0xdea
	.byte	0x9e
	.uleb128 0xde9
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$15,LCFI10-LCFI9
	.long L$set$15
	.byte	0x93
	.uleb128 0xde8
	.byte	0x94
	.uleb128 0xde7
	.byte	0x4
	.set L$set$16,LCFI11-LCFI10
	.long L$set$16
	.byte	0x95
	.uleb128 0xde6
	.byte	0x96
	.uleb128 0xde5
	.byte	0x97
	.uleb128 0xde4
	.byte	0x98
	.uleb128 0xde3
	.byte	0x99
	.uleb128 0xde2
	.byte	0x4
	.set L$set$17,LCFI12-LCFI11
	.long L$set$17
	.byte	0xd9
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
LEFDE3:
LSFDE5:
	.set L$set$18,LEFDE5-LASFDE5
	.long L$set$18
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB5-.
	.set L$set$19,LFE5-LFB5
	.quad L$set$19
	.uleb128 0
	.byte	0x4
	.set L$set$20,LCFI13-LFB5
	.long L$set$20
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$22,LCFI15-LCFI14
	.long L$set$22
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$23,LEFDE7-LASFDE7
	.long L$set$23
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB9-.
	.set L$set$24,LFE9-LFB9
	.quad L$set$24
	.uleb128 0
	.byte	0x4
	.set L$set$25,LCFI16-LFB9
	.long L$set$25
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$26,LCFI17-LCFI16
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$27,LCFI18-LCFI17
	.long L$set$27
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$28,LEFDE9-LASFDE9
	.long L$set$28
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB12-.
	.set L$set$29,LFE12-LFB12
	.quad L$set$29
	.uleb128 0
	.byte	0x4
	.set L$set$30,LCFI19-LFB12
	.long L$set$30
	.byte	0xe
	.uleb128 0x2650
	.byte	0x4
	.set L$set$31,LCFI20-LCFI19
	.long L$set$31
	.byte	0x9d
	.uleb128 0x4ca
	.byte	0x9e
	.uleb128 0x4c9
	.byte	0x4
	.set L$set$32,LCFI21-LCFI20
	.long L$set$32
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$33,LCFI22-LCFI21
	.long L$set$33
	.byte	0x93
	.uleb128 0x4c8
	.byte	0x94
	.uleb128 0x4c7
	.byte	0x95
	.uleb128 0x4c6
	.byte	0x96
	.uleb128 0x4c5
	.byte	0x97
	.uleb128 0x4c4
	.byte	0x98
	.uleb128 0x4c3
	.byte	0x99
	.uleb128 0x4c2
	.byte	0x9a
	.uleb128 0x4c1
	.byte	0x4
	.set L$set$34,LCFI23-LCFI22
	.long L$set$34
	.byte	0xa
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
	.quad	LFB13-.
	.set L$set$37,LFE13-LFB13
	.quad L$set$37
	.uleb128 0
	.byte	0x4
	.set L$set$38,LCFI25-LFB13
	.long L$set$38
	.byte	0xe
	.uleb128 0x1e0
	.byte	0x9d
	.uleb128 0x3c
	.byte	0x9e
	.uleb128 0x3b
	.byte	0x4
	.set L$set$39,LCFI26-LCFI25
	.long L$set$39
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$40,LCFI27-LCFI26
	.long L$set$40
	.byte	0x95
	.uleb128 0x38
	.byte	0x96
	.uleb128 0x37
	.byte	0x4
	.set L$set$41,LCFI28-LCFI27
	.long L$set$41
	.byte	0x93
	.uleb128 0x3a
	.byte	0x94
	.uleb128 0x39
	.byte	0x4
	.set L$set$42,LCFI29-LCFI28
	.long L$set$42
	.byte	0x97
	.uleb128 0x36
	.byte	0x98
	.uleb128 0x35
	.byte	0x4
	.set L$set$43,LCFI30-LCFI29
	.long L$set$43
	.byte	0x99
	.uleb128 0x34
	.byte	0x9a
	.uleb128 0x33
	.byte	0x4
	.set L$set$44,LCFI31-LCFI30
	.long L$set$44
	.byte	0x9b
	.uleb128 0x32
	.byte	0x9c
	.uleb128 0x31
	.byte	0x4
	.set L$set$45,LCFI32-LCFI31
	.long L$set$45
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
	.byte	0xdc
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
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$46,LEFDE13-LASFDE13
	.long L$set$46
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB14-.
	.set L$set$47,LFE14-LFB14
	.quad L$set$47
	.uleb128 0
	.byte	0x4
	.set L$set$48,LCFI33-LFB14
	.long L$set$48
	.byte	0xe
	.uleb128 0x1420
	.byte	0x4
	.set L$set$49,LCFI34-LCFI33
	.long L$set$49
	.byte	0x9d
	.uleb128 0x284
	.byte	0x9e
	.uleb128 0x283
	.byte	0x4
	.set L$set$50,LCFI35-LCFI34
	.long L$set$50
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$51,LCFI36-LCFI35
	.long L$set$51
	.byte	0x93
	.uleb128 0x282
	.byte	0x94
	.uleb128 0x281
	.byte	0x4
	.set L$set$52,LCFI37-LCFI36
	.long L$set$52
	.byte	0x95
	.uleb128 0x280
	.byte	0x96
	.uleb128 0x27f
	.byte	0x4
	.set L$set$53,LCFI38-LCFI37
	.long L$set$53
	.byte	0x97
	.uleb128 0x27e
	.byte	0x98
	.uleb128 0x27d
	.byte	0x4
	.set L$set$54,LCFI39-LCFI38
	.long L$set$54
	.byte	0x99
	.uleb128 0x27c
	.byte	0x9a
	.uleb128 0x27b
	.byte	0x4
	.set L$set$55,LCFI40-LCFI39
	.long L$set$55
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
LEFDE13:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
