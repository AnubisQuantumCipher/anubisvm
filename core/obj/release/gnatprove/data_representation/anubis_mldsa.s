	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa__keygen
_anubis_mldsa__keygen:
LFB2:
	sub	sp, sp, #2432
LCFI0:
	adrp	x4, lC0@PAGE
	sub	sp, sp, #110592
LCFI1:
	adrp	x3, lC1@PAGE
	mov	x5, 14720
	mov	x6, 22912
	stp	x29, x30, [sp]
LCFI2:
	mov	x29, sp
LCFI3:
	add	x3, x3, lC1@PAGEOFF;
	stp	x23, x24, [sp, 48]
LCFI4:
	mov	x24, x1
	add	x1, x4, lC0@PAGEOFF;
	mov	w4, 128
	add	x23, x29, x6
	stp	x25, x26, [sp, 64]
LCFI5:
	mov	x26, x2
	add	x2, x29, 256
	add	x25, x29, 160
	stp	x19, x20, [sp, 16]
LCFI6:
	add	x19, x29, 96
	add	x20, x29, 384
	stp	x21, x22, [sp, 32]
LCFI7:
	add	x21, x29, x5
	add	x22, x29, 128
	str	x27, [sp, 80]
LCFI8:
	bl	_anubis_sha3__shake256
	ldp	q30, q31, [x29, 256]
	mov	x7, 55680
	mov	x0, x19
	add	x27, x29, x7
	ldp	q28, q29, [x29, 288]
	mov	x1, x27
	stp	q30, q31, [x29, 96]
	ldp	q30, q31, [x29, 320]
	stp	q28, q29, [x29, 128]
	stp	q30, q31, [x29, 160]
	bl	_anubis_mldsa_sample__expanda
	mov	x0, x22
	mov	x2, x21
	mov	x1, x20
	bl	_anubis_mldsa_sample__expands
	mov	x8, 7552
	mov	x1, x20
	add	x22, x29, x8
	mov	x2, 7168
	mov	x0, x22
	bl	_memcpy
	mov	x0, x22
	bl	_anubis_mldsa_ntt__vec_ntt_l
	mov	x1, x22
	mov	x2, x23
	mov	x0, x27
	bl	_anubis_mldsa_ntt__matrix_vec_mul
	mov	x0, x23
	bl	_anubis_mldsa_ntt__vec_intt_k
	mov	x9, 31104
	mov	x1, x21
	add	x22, x29, x9
	mov	x0, x23
	mov	x2, x22
	bl	_anubis_mldsa_poly__vec_add_k
	mov	x0, x22
	bl	_anubis_mldsa_poly__vec_reduce_k
	mov	x10, 47488
	mov	x11, 39296
	add	x23, x29, x10
	add	x1, x29, x11
	mov	x0, x22
	mov	x2, x23
	mov	x22, x1
	bl	_anubis_mldsa_poly__vec_power2round_k
	mov	x2, x24
	mov	x1, x22
	mov	x0, x19
	add	x22, x29, 192
	bl	_anubis_mldsa_encoding__pack_public_key
	adrp	x1, lC2@PAGE
	adrp	x3, lC3@PAGE
	mov	x0, x24
	mov	x2, x22
	add	x1, x1, lC2@PAGEOFF;
	add	x3, x3, lC3@PAGEOFF;
	mov	w4, 64
	bl	_anubis_sha3__shake256
	mov	x6, x26
	mov	x5, x23
	mov	x4, x21
	mov	x3, x20
	mov	x2, x22
	mov	x1, x25
	mov	x0, x19
	bl	_anubis_mldsa_encoding__pack_secret_key
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
LCFI9:
	add	sp, sp, 2432
LCFI10:
	add	sp, sp, 110592
LCFI11:
	ret
LFE2:
	.const
	.align	2
lC0:
	.word	0
	.word	31
	.align	2
lC1:
	.word	0
	.word	127
	.align	2
lC2:
	.word	0
	.word	2591
	.align	2
lC3:
	.word	0
	.word	63
	.text
	.const
	.align	3
lC7:
	.ascii "failed precondition from anubis_mldsa.ads:59"
	.align	3
lC8:
	.ascii "anubis_mldsa.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa__sign
_anubis_mldsa__sign:
LFB3:
	sub	sp, sp, #1520
LCFI12:
	sub	sp, sp, #192512
LCFI13:
	stp	x29, x30, [sp]
LCFI14:
	mov	x29, sp
LCFI15:
	stp	x23, x24, [sp, 48]
LCFI16:
	mov	x23, x2
	mov	x2, 1073741823
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI17:
	ldp	w6, w5, [x23]
	add	x2, x2, w6, sxtw
	cmp	w6, 0
	sxtw	x21, w5
	ccmp	x2, x21, 1, eq
	blt	L54
	movi	v31.4s, 0
	mov	x24, x0
	mov	x22, x1
	mov	x20, x3
	mov	x26, x4
	cset	w27, lt
	stp	q31, q31, [x29, 432]
	stp	q31, q31, [x29, 464]
	tbnz	w5, #31, L33
	add	x21, x21, 1
	mov	x0, 2147483648
	cmp	x21, x0
	beq	L55
	mov	x0, 2147483584
	cmp	x21, x0
	bcs	L56
	add	w2, w21, 64
	add	w21, w21, 63
	sxtw	x2, w2
L6:
	add	x0, x2, 15
	mov	x10, 54768
	and	x0, x0, -16
	mov	w1, 0
	sub	sp, sp, x0
	add	x25, x29, x10
	mov	x0, sp
	add	x28, x29, 2544
	str	x0, [x29, 216]
	bl	_memset
	mov	x2, 4627
	mov	w1, 0
	mov	x0, x26
	bl	_memset
	mov	x11, 62960
	add	x2, x29, 240
	add	x1, x29, x11
	mov	x4, x28
	mov	x6, x1
	mov	x5, x25
	add	x3, x29, 368
	mov	x0, x24
	mov	x19, x2
	mov	x24, x1
	mov	x1, x2
	add	x2, x29, 272
	bl	_anubis_mldsa_encoding__unpack_secret_key
	add	x0, x29, 135168
	add	x0, x0, 1520
	mov	x1, x0
	mov	x0, x19
	str	x1, [x29, 176]
	bl	_anubis_mldsa_sample__expanda
	mov	x12, 9712
	mov	x1, x28
	add	x0, x29, x12
	mov	x2, 7168
	mov	x19, x0
	str	x0, [x29, 128]
	bl	_memcpy
	mov	x0, x19
	bl	_anubis_mldsa_ntt__vec_ntt_l
	add	x0, x29, 69632
	mov	x1, x25
	add	x0, x0, 1520
	mov	x2, 8192
	mov	x19, x0
	str	x0, [x29, 120]
	bl	_memcpy
	mov	x0, x19
	bl	_anubis_mldsa_ntt__vec_ntt_k
	add	x0, x29, 77824
	mov	x1, x24
	add	x0, x0, 1520
	mov	x2, 8192
	mov	x19, x0
	str	x0, [x29, 200]
	bl	_memcpy
	mov	x0, x19
	bl	_anubis_mldsa_ntt__vec_ntt_k
	ldp	q29, q28, [x29, 368]
	ldp	q31, q30, [x29, 400]
	stp	q29, q28, [sp]
	ldp	w2, w4, [x23]
	stp	q31, q30, [sp, 32]
	sxtw	x1, w2
	sub	x0, x1, #1
	cmp	w2, w4
	bgt	L17
	sub	x1, sp, x1
	sxtw	x4, w4
	add	x3, x1, 64
	mov	w5, 2147483584
	.p2align 5,,15
L16:
	add	x0, x0, 1
	cmp	w0, w5
	bge	L14
	add	w1, w0, 64
	subs	w1, w1, w2
	bvs	L14
	cmp	w21, w1
	bcc	L57
	ldrb	w1, [x22, x0]
	strb	w1, [x3, x0]
	cmp	x4, x0
	bne	L16
L17:
	add	x0, x29, 432
	adrp	x3, lC3@PAGE
	stp	wzr, w21, [x29, 232]
	mov	x2, x0
	mov	x0, 38384
	add	x3, x3, lC3@PAGEOFF;
	mov	w4, 64
	add	x1, x29, 232
	add	x25, x29, x0
	ldr	x0, [x29, 216]
	str	x2, [x29, 136]
	add	x19, x29, 126976
	adrp	x22, lC1@PAGE
	bl	_anubis_sha3__shake256
	ldp	q28, q29, [x29, 272]
	add	x5, x29, 114688
	add	x2, x29, 304
	add	x5, x5, 128
	add	x19, x19, 1520
	ldp	q30, q31, [x29, 432]
	add	x22, x22, lC1@PAGEOFF;
	adrp	x3, lC0@PAGE
	str	x2, [x29, 152]
	mov	x0, x19
	mov	x1, x22
	mov	x6, 16880
	add	x3, x3, lC0@PAGEOFF;
	mov	w4, 32
	add	x21, x29, x6
	add	x24, x29, 86016
	add	x23, x29, 102400
	add	x24, x24, 1520
	add	x23, x23, 1520
	str	q28, [x5, 13680]
	str	q29, [x5, 13696]
	str	q30, [x5, 13744]
	str	q31, [x5, 13760]
	ldp	q30, q31, [x20]
	add	x20, x29, 1520
	ldp	q28, q29, [x29, 464]
	str	q30, [x5, 13712]
	str	q28, [x5, 13776]
	str	q29, [x5, 13792]
	str	q31, [x19, 48]
	bl	_anubis_sha3__shake256
	mov	x7, 24048
	mov	x9, 46576
	str	x26, [x29, 104]
	add	x0, x29, x7
	mov	x8, 31216
	stp	w27, wzr, [x29, 208]
	add	x1, x29, x8
	str	x0, [x29, 168]
	add	x0, x29, 94208
	add	x0, x0, 1520
	str	x1, [x29, 192]
	str	x0, [x29, 160]
	add	x0, x29, x9
	str	x0, [x29, 216]
	add	x0, x29, 126976
	add	x0, x0, 1584
	str	x0, [x29, 144]
	add	x0, x29, 336
	str	x0, [x29, 184]
	add	x0, x29, 496
	str	x0, [x29, 112]
L10:
	ldr	x0, [x29, 152]
	mov	x2, x21
	ldr	w1, [x29, 212]
	bl	_anubis_mldsa_sample__expandmask
	ldr	x26, [x29, 168]
	mov	x2, 7168
	mov	x1, x21
	mov	x0, x26
	bl	_memcpy
	mov	x0, x26
	bl	_anubis_mldsa_ntt__vec_ntt_l
	ldr	x0, [x29, 176]
	mov	x2, x24
	mov	x1, x26
	bl	_anubis_mldsa_ntt__matrix_vec_mul
	mov	x0, x24
	bl	_anubis_mldsa_poly__vec_reduce_k
	mov	x0, x24
	bl	_anubis_mldsa_ntt__vec_intt_k
	mov	x0, x24
	bl	_anubis_mldsa_poly__vec_reduce_k
	ldr	x27, [x29, 160]
	mov	x0, x24
	mov	x1, x27
	bl	_anubis_mldsa_poly__vec_highbits_k
	ldr	x26, [x29, 216]
	mov	x2, 1024
	mov	w1, 0
	mov	x0, x26
	bl	_memset
	mov	x0, x19
	mov	x2, 1088
	mov	w1, 0
	bl	_memset
	.p2align 5,,15
L19:
	mov	x0, x27
	mov	x1, x25
	mov	x2, x22
	add	x27, x27, 1024
	bl	_anubis_mldsa_encoding__pack_w1
	ldp	q25, q24, [x25]
	add	x26, x26, 128
	ldp	q27, q26, [x25, 32]
	ldp	q29, q28, [x25, 64]
	ldp	q31, q30, [x25, 96]
	stp	q25, q24, [x26, -128]
	stp	q27, q26, [x26, -96]
	stp	q29, q28, [x26, -64]
	stp	q31, q30, [x26, -32]
	cmp	x27, x23
	bne	L19
	ldp	x3, x0, [x29, 136]
	mov	x2, 1024
	ldr	x1, [x29, 216]
	ldp	q29, q28, [x3], 32
	ldp	q31, q30, [x3]
	stp	q29, q28, [x19]
	stp	q31, q30, [x19, 32]
	bl	_memcpy
	ldr	x28, [x29, 184]
	adrp	x0, lC0@PAGE
	adrp	x1, lC6@PAGE
	add	x26, x0, lC0@PAGEOFF;
	mov	w4, 32
	mov	x3, x26
	mov	x0, x19
	add	x1, x1, lC6@PAGEOFF;
	mov	x2, x28
	bl	_anubis_sha3__shake256
	ldr	x27, [x29, 112]
	mov	x0, x28
	mov	x1, x26
	mov	x2, x27
	bl	_anubis_mldsa_sample__sampleinball
	mov	x1, x27
	mov	x2, 1024
	mov	x0, x20
	mov	x27, x21
	bl	_memcpy
	mov	x0, x20
	bl	_anubis_mldsa_ntt__ntt
	ldr	x28, [x29, 128]
	ldr	x26, [x29, 192]
L20:
	mov	x1, x28
	mov	x2, x19
	mov	x0, x20
	add	x28, x28, 1024
	bl	_anubis_mldsa_ntt__ntt_mul
	mov	x0, x19
	bl	_anubis_mldsa_ntt__intt
	mov	x2, x26
	mov	x0, x27
	mov	x1, x19
	add	x27, x27, 1024
	bl	_anubis_mldsa_poly__poly_add
	add	x26, x26, 1024
	cmp	x21, x28
	bne	L20
	ldr	x26, [x29, 192]
	mov	x0, x26
	bl	_anubis_mldsa_poly__vec_reduce_l
	mov	w1, 65416
	mov	x0, x26
	movk	w1, 0x7, lsl 16
	bl	_anubis_mldsa_poly__vec_chk_norm_l
	cmp	w0, 1
	bhi	L58
	cbnz	w0, L59
L32:
	ldr	w1, [x29, 212]
	mov	w0, 6993
	cmp	w1, w0
	beq	L52
	add	w0, w1, 7
	str	w0, [x29, 212]
	b	L10
L33:
	mov	x2, 64
	mov	w21, 63
	b	L6
L59:
	ldr	x3, [x29, 120]
	add	x28, x29, 110592
	mov	x0, x23
	add	x28, x28, 1520
	mov	x23, x21
	str	x22, [x29, 96]
	mov	x26, x24
	mov	x22, x20
	mov	x21, x28
	mov	x20, x19
	mov	x27, x0
	mov	x19, x3
L23:
	mov	x1, x19
	mov	x2, x20
	mov	x0, x22
	add	x19, x19, 1024
	bl	_anubis_mldsa_ntt__ntt_mul
	mov	x0, x20
	bl	_anubis_mldsa_ntt__intt
	mov	x0, x26
	mov	x2, x21
	mov	x1, x20
	add	x26, x26, 1024
	bl	_anubis_mldsa_poly__poly_sub
	ldr	x0, [x29, 200]
	add	x21, x21, 1024
	cmp	x19, x0
	bne	L23
	mov	x0, x28
	mov	x19, x20
	mov	x20, x22
	ldr	x22, [x29, 96]
	mov	x21, x23
	bl	_anubis_mldsa_poly__vec_reduce_k
	mov	x1, x27
	mov	x0, x28
	bl	_anubis_mldsa_poly__vec_lowbits_k
	mov	w1, 65160
	mov	x0, x27
	movk	w1, 0x3, lsl 16
	mov	x23, x27
	bl	_anubis_mldsa_poly__vec_chk_norm_k
	cmp	w0, 1
	bhi	L60
	cbz	w0, L32
	ldr	x3, [x29, 200]
	mov	x27, x25
	mov	x26, x25
	str	x23, [x29, 96]
	mov	x23, x21
	mov	x21, x20
	mov	x20, x19
	mov	x19, x3
L25:
	mov	x1, x19
	mov	x2, x20
	mov	x0, x21
	add	x19, x19, 1024
	bl	_anubis_mldsa_ntt__ntt_mul
	mov	x0, x20
	bl	_anubis_mldsa_ntt__intt
	mov	x0, x26
	mov	x1, x20
	mov	x2, 1024
	add	x26, x26, 1024
	bl	_memcpy
	cmp	x24, x19
	bne	L25
	mov	x0, x25
	mov	x19, x20
	mov	x20, x21
	mov	x21, x23
	ldr	x23, [x29, 96]
	bl	_anubis_mldsa_poly__vec_reduce_k
	mov	x0, x25
	mov	w1, 261888
	bl	_anubis_mldsa_poly__vec_chk_norm_k
	cmp	w0, 1
	bhi	L61
	cbz	w0, L32
	ldr	x26, [x29, 216]
	mov	x2, 8192
	mov	w1, 0
	mov	x0, x26
	bl	_memset
	mov	x1, x25
	mov	x2, x19
	mov	x0, x28
	bl	_anubis_mldsa_poly__vec_add_k
	mov	x0, x19
	bl	_anubis_mldsa_poly__vec_reduce_k
	adrp	x0, lC9@PAGE
	mov	x1, x26
	ldr	q31, [x0, #lC9@PAGEOFF]
L29:
	mov	x0, 0
L28:
	ldr	q30, [x27, x0]
	sub	v29.4s, v31.4s, v30.4s
	cmeq	v30.4s, v30.4s, #0
	bic	v30.16b, v29.16b, v30.16b
	str	q30, [x1, x0]
	add	x0, x0, 16
	cmp	x0, 1024
	bne	L28
	ldr	x0, [x29, 216]
	add	x27, x27, 1024
	add	x1, x1, 1024
	cmp	x0, x27
	bne	L29
	add	x28, x29, 118784
	mov	x1, x19
	add	x28, x28, 1520
	mov	x2, x28
	bl	_anubis_mldsa_poly__vec_makehint_k
	tbnz	w0, #31, L62
	cmp	w0, 75
	bgt	L32
	ldr	x26, [x29, 104]
	mov	x2, x28
	mov	w27, 1
	ldp	x0, x1, [x29, 184]
	mov	x3, x26
	bl	_anubis_mldsa_encoding__pack_signature
	b	L18
L61:
	adrp	x0, lC8@PAGE
	mov	w1, 297
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L58:
	adrp	x0, lC8@PAGE
	mov	w1, 255
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L54:
	adrp	x0, lC7@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L52:
	ldr	w27, [x29, 208]
L18:
	mov	w0, w27
	mov	sp, x29
LCFI18:
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
LCFI19:
	add	sp, sp, 1520
LCFI20:
	add	sp, sp, 192512
LCFI21:
	ret
L57:
LCFI22:
	adrp	x0, lC8@PAGE
	mov	w1, 161
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L14:
	adrp	x0, lC8@PAGE
	mov	w1, 161
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L56:
	adrp	x0, lC8@PAGE
	mov	w1, 113
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L55:
	adrp	x0, lC8@PAGE
	mov	w1, 113
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L60:
	adrp	x0, lC8@PAGE
	mov	w1, 276
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L62:
	adrp	x0, lC8@PAGE
	mov	w1, 327
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE3:
	.const
	.align	2
lC6:
	.word	0
	.word	1087
	.align	2
lC4:
	.word	1
	.word	44
	.text
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_mldsa.ads:74"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa__verify
_anubis_mldsa__verify:
LFB4:
	sub	sp, sp, #2608
LCFI23:
	sub	sp, sp, #122880
LCFI24:
	stp	x29, x30, [sp]
LCFI25:
	mov	x29, sp
LCFI26:
	stp	x25, x26, [sp, 64]
LCFI27:
	mov	x26, x2
	mov	x2, 1073741823
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x27, x28, [sp, 80]
LCFI28:
	mov	x27, x0
	ldp	w5, w4, [x26]
	add	x2, x2, w5, sxtw
	cmp	w5, 0
	sxtw	x5, w4
	ccmp	x2, x5, 1, eq
	cset	w0, lt
	str	w0, [x29, 124]
	blt	L101
	movi	v31.4s, 0
	mov	x25, x1
	mov	x24, x3
	stp	q31, q31, [x29, 304]
	stp	q31, q31, [x29, 336]
	tbnz	w4, #31, L87
	add	x5, x5, 1
	mov	x0, 2147483648
	cmp	x5, x0
	beq	L102
	mov	x0, 2147483584
	cmp	x5, x0
	bcs	L103
	add	w2, w5, 64
	add	w22, w5, 63
	sxtw	x2, w2
L65:
	add	x0, x2, 15
	mov	x6, 27184
	and	x0, x0, -16
	mov	x7, 35376
	sub	sp, sp, x0
	mov	x8, 4656
	mov	x0, sp
	mov	w1, 0
	add	x21, x29, x6
	add	x19, x29, x7
	add	x23, x29, x8
	add	x28, x29, 208
	str	x0, [x29, 96]
	bl	_memset
	mov	x2, x21
	mov	x1, x28
	mov	x0, x27
	add	x20, x29, 144
	bl	_anubis_mldsa_encoding__unpack_public_key
	mov	x0, x24
	mov	x3, x19
	mov	x2, x23
	mov	x1, x20
	bl	_anubis_mldsa_encoding__unpack_signature
	cmp	w0, 1
	bhi	L104
	cbnz	w0, L105
L70:
	ldr	w0, [x29, 124]
	mov	sp, x29
LCFI29:
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
LCFI30:
	add	sp, sp, 2608
LCFI31:
	add	sp, sp, 122880
LCFI32:
	ret
L105:
LCFI33:
	mov	w1, 65416
	mov	x0, x23
	movk	w1, 0x7, lsl 16
	bl	_anubis_mldsa_poly__vec_chk_norm_l
	cmp	w0, 1
	bhi	L106
	cbz	w0, L70
	add	x24, x29, 65536
	mov	x0, x28
	add	x24, x24, 2608
	adrp	x28, lC3@PAGE
	mov	x1, x24
	bl	_anubis_mldsa_sample__expanda
	adrp	x1, lC2@PAGE
	mov	x0, x27
	add	x1, x1, lC2@PAGEOFF;
	add	x3, x28, lC3@PAGEOFF;
	mov	w4, 64
	add	x2, x29, 240
	bl	_anubis_sha3__shake256
	ldp	q29, q28, [x29, 240]
	ldp	q31, q30, [x29, 272]
	stp	q29, q28, [sp]
	ldp	w3, w4, [x26]
	stp	q31, q30, [sp, 32]
	sxtw	x1, w3
	sub	x0, x1, #1
	cmp	w3, w4
	bgt	L81
	sub	x1, sp, x1
	sxtw	x4, w4
	add	x1, x1, 64
	mov	w7, 2147483584
	.p2align 5,,15
L80:
	add	x0, x0, 1
	cmp	w0, w7
	bge	L78
	add	w2, w0, 64
	subs	w2, w2, w3
	bvs	L78
	cmp	w2, w22
	bhi	L107
	ldrb	w2, [x25, x0]
	strb	w2, [x1, x0]
	cmp	x4, x0
	bne	L80
L81:
	ldr	x0, [x29, 96]
	mov	w4, 64
	add	x3, x28, lC3@PAGEOFF;
	add	x2, x29, 304
	add	x1, x29, 136
	stp	wzr, w22, [x29, 136]
	bl	_anubis_sha3__shake256
	add	x27, x29, 496
	adrp	x1, lC0@PAGE
	mov	x2, x27
	mov	x0, x20
	add	x1, x1, lC0@PAGEOFF;
	bl	_anubis_mldsa_sample__sampleinball
	mov	x3, 11824
	mov	x1, x23
	add	x0, x29, x3
	mov	x2, 7168
	mov	x23, x0
	bl	_memcpy
	mov	x0, x23
	bl	_anubis_mldsa_ntt__vec_ntt_l
	mov	x4, 18992
	mov	x5, 43568
	add	x0, x29, x4
	mov	x1, x23
	mov	x2, x0
	add	x23, x29, x5
	str	x0, [x29, 96]
	mov	x0, x24
	mov	x28, x23
	bl	_anubis_mldsa_ntt__matrix_vec_mul
L74:
	mov	x2, x28
	mov	x0, x21
	mov	w1, 13
	add	x21, x21, 1024
	bl	_anubis_mldsa_poly__poly_shiftl
	add	x28, x28, 1024
	cmp	x19, x21
	bne	L74
	mov	x0, 51760
	mov	x1, x23
	mov	x2, 8192
	add	x0, x29, x0
	mov	x22, x0
	add	x21, x29, 1520
	bl	_memcpy
	mov	x0, x22
	mov	x26, 33889
	bl	_anubis_mldsa_ntt__vec_ntt_k
	mov	x1, x27
	mov	x2, 1024
	mov	x0, x21
	movk	x26, 0x6014, lsl 16
	bl	_memcpy
	mov	x0, x21
	mov	x25, 57345
	bl	_anubis_mldsa_ntt__ntt
	ldr	x0, [x29, 96]
	mov	x1, 59952
	movk	x26, 0x1c0, lsl 32
	mov	x2, 8196
	add	x28, x29, x1
	movk	x25, 0x7f, lsl 16
	movk	x26, 0x2008, lsl 48
	add	x27, x0, 4
	add	x23, x0, x2
L83:
	mov	x2, x28
	mov	x1, x22
	mov	x0, x21
	bl	_anubis_mldsa_ntt__ntt_mul
	mov	x2, -1
	.p2align 5,,15
L82:
	add	x3, x28, x2, lsl 2
	ldr	w0, [x27, x2, lsl 2]
	ldr	w3, [x3, 4]
	add	x0, x0, x25
	sub	x0, x0, x3
	umulh	x4, x0, x26
	lsr	x4, x4, 20
	lsl	x3, x4, 10
	sub	x3, x3, x4
	add	x3, x4, x3, lsl 13
	sub	x0, x0, x3
	str	w0, [x27, x2, lsl 2]
	add	x2, x2, 1
	cmp	x2, 255
	bne	L82
	add	x27, x27, 1024
	add	x22, x22, 1024
	cmp	x23, x27
	bne	L83
	ldr	x21, [x29, 96]
	add	x25, x29, 2544
	add	x23, x29, 3568
	adrp	x27, lC1@PAGE
	add	x27, x27, lC1@PAGEOFF;
	mov	x0, x21
	bl	_anubis_mldsa_poly__vec_reduce_k
	mov	x0, x21
	bl	_anubis_mldsa_ntt__vec_intt_k
	mov	x0, x21
	bl	_anubis_mldsa_poly__vec_reduce_k
	mov	x1, x21
	mov	x0, x19
	mov	x2, x28
	mov	x21, x25
	bl	_anubis_mldsa_poly__vec_usehint_k
	mov	x2, 1024
	mov	w1, 0
	mov	x0, x25
	add	x19, x29, 368
	bl	_memset
	mov	x2, 1088
	mov	x0, x23
	mov	w1, 0
	bl	_memset
L84:
	mov	x0, x28
	mov	x1, x19
	mov	x2, x27
	add	x28, x28, 1024
	bl	_anubis_mldsa_encoding__pack_w1
	ldp	q25, q24, [x19]
	add	x21, x21, 128
	ldp	q27, q26, [x19, 32]
	ldp	q29, q28, [x19, 64]
	ldp	q31, q30, [x19, 96]
	stp	q25, q24, [x21, -128]
	stp	q27, q26, [x21, -96]
	stp	q29, q28, [x21, -64]
	stp	q31, q30, [x21, -32]
	cmp	x28, x24
	bne	L84
	ldp	q30, q31, [x29, 336]
	mov	x1, x25
	mov	x2, 1024
	add	x0, x29, 3632
	add	x19, x29, 176
	ldp	q28, q29, [x29, 304]
	str	q30, [x29, 3600]
	str	q31, [x29, 3616]
	str	q28, [x29, 3568]
	str	q29, [x29, 3584]
	bl	_memcpy
	adrp	x0, lC0@PAGE
	adrp	x1, lC6@PAGE
	add	x3, x0, lC0@PAGEOFF;
	mov	x2, x19
	mov	x0, x23
	add	x1, x1, lC6@PAGEOFF;
	mov	w4, 32
	bl	_anubis_sha3__shake256
	ldr	q31, [x29, 144]
	ldr	q30, [x29, 176]
	cmeq	v31.16b, v31.16b, v30.16b
	not	v31.16b, v31.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L88
	ldr	q31, [x29, 160]
	ldr	q30, [x29, 192]
	cmeq	v31.16b, v31.16b, v30.16b
	not	v31.16b, v31.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L108
L89:
	mov	w0, 1
	str	w0, [x29, 124]
	b	L70
L87:
	mov	x2, 64
	mov	w22, 63
	b	L65
L78:
	adrp	x0, lC8@PAGE
	mov	w1, 400
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L101:
	adrp	x0, lC10@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L103:
	adrp	x0, lC8@PAGE
	mov	w1, 365
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L102:
	adrp	x0, lC8@PAGE
	mov	w1, 365
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L106:
	adrp	x0, lC8@PAGE
	mov	w1, 385
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L104:
	adrp	x0, lC8@PAGE
	mov	w1, 380
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L108:
	adrp	x0, lC11@PAGE
	ldr	q31, [x0, #lC11@PAGEOFF]
	adrp	x0, lC12@PAGE
	str	q31, [x29, 96]
	ldr	q31, [x0, #lC12@PAGEOFF]
	umov	x1, v31.d[0]
L85:
	ldrsw	x0, [x29, 96]
	sub	w1, w1, #1
	add	x2, x0, 1
	add	x4, x1, x2
	b	L86
	.p2align 2,,3
L109:
	add	x0, x0, 1
	cmp	x0, x4
	beq	L89
L86:
	add	x3, x20, x0
	add	x1, x19, x0
	ldrb	w3, [x3, 1]
	ldrb	w1, [x1, 1]
	cmp	w3, w1
	beq	L109
	b	L70
L88:
	adrp	x0, lC13@PAGE
	ldr	q31, [x0, #lC13@PAGEOFF]
	adrp	x0, lC14@PAGE
	str	q31, [x29, 96]
	ldr	q31, [x0, #lC14@PAGEOFF]
	umov	x1, v31.d[0]
	b	L85
L107:
	adrp	x0, lC8@PAGE
	mov	w1, 400
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE4:
	.globl _anubis_mldsa_E
	.data
	.align	1
_anubis_mldsa_E:
	.space 2
	.literal16
	.align	4
lC9:
	.word	8380417
	.word	8380417
	.word	8380417
	.word	8380417
	.align	4
lC11:
	.word	15
	.word	16
	.word	17
	.word	18
	.align	4
lC12:
	.word	16
	.word	15
	.word	14
	.word	13
	.align	4
lC13:
	.word	-1
	.word	0
	.word	1
	.word	2
	.align	4
lC14:
	.word	32
	.word	31
	.word	30
	.word	29
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
	.byte	0x4
	.set L$set$3,LCFI0-LFB2
	.long L$set$3
	.byte	0xe
	.uleb128 0x980
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xe
	.uleb128 0x1b980
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0x9d
	.uleb128 0x3730
	.byte	0x9e
	.uleb128 0x372f
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0x97
	.uleb128 0x372a
	.byte	0x98
	.uleb128 0x3729
	.byte	0x4
	.set L$set$8,LCFI5-LCFI4
	.long L$set$8
	.byte	0x99
	.uleb128 0x3728
	.byte	0x9a
	.uleb128 0x3727
	.byte	0x4
	.set L$set$9,LCFI6-LCFI5
	.long L$set$9
	.byte	0x93
	.uleb128 0x372e
	.byte	0x94
	.uleb128 0x372d
	.byte	0x4
	.set L$set$10,LCFI7-LCFI6
	.long L$set$10
	.byte	0x95
	.uleb128 0x372c
	.byte	0x96
	.uleb128 0x372b
	.byte	0x4
	.set L$set$11,LCFI8-LCFI7
	.long L$set$11
	.byte	0x9b
	.uleb128 0x3726
	.byte	0x4
	.set L$set$12,LCFI9-LCFI8
	.long L$set$12
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
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$13,LCFI10-LCFI9
	.long L$set$13
	.byte	0xe
	.uleb128 0x1b000
	.byte	0x4
	.set L$set$14,LCFI11-LCFI10
	.long L$set$14
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$15,LEFDE3-LASFDE3
	.long L$set$15
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB3-.
	.set L$set$16,LFE3-LFB3
	.quad L$set$16
	.uleb128 0
	.byte	0x4
	.set L$set$17,LCFI12-LFB3
	.long L$set$17
	.byte	0xe
	.uleb128 0x5f0
	.byte	0x4
	.set L$set$18,LCFI13-LCFI12
	.long L$set$18
	.byte	0xe
	.uleb128 0x2f5f0
	.byte	0x4
	.set L$set$19,LCFI14-LCFI13
	.long L$set$19
	.byte	0x9d
	.uleb128 0x5ebe
	.byte	0x9e
	.uleb128 0x5ebd
	.byte	0x4
	.set L$set$20,LCFI15-LCFI14
	.long L$set$20
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$21,LCFI16-LCFI15
	.long L$set$21
	.byte	0x97
	.uleb128 0x5eb8
	.byte	0x98
	.uleb128 0x5eb7
	.byte	0x4
	.set L$set$22,LCFI17-LCFI16
	.long L$set$22
	.byte	0x93
	.uleb128 0x5ebc
	.byte	0x94
	.uleb128 0x5ebb
	.byte	0x95
	.uleb128 0x5eba
	.byte	0x96
	.uleb128 0x5eb9
	.byte	0x99
	.uleb128 0x5eb6
	.byte	0x9a
	.uleb128 0x5eb5
	.byte	0x9b
	.uleb128 0x5eb4
	.byte	0x9c
	.uleb128 0x5eb3
	.byte	0x4
	.set L$set$23,LCFI18-LCFI17
	.long L$set$23
	.byte	0xa
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$24,LCFI19-LCFI18
	.long L$set$24
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
	.byte	0xdd
	.byte	0xde
	.byte	0x4
	.set L$set$25,LCFI20-LCFI19
	.long L$set$25
	.byte	0xe
	.uleb128 0x2f000
	.byte	0x4
	.set L$set$26,LCFI21-LCFI20
	.long L$set$26
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$27,LCFI22-LCFI21
	.long L$set$27
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$28,LEFDE5-LASFDE5
	.long L$set$28
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB4-.
	.set L$set$29,LFE4-LFB4
	.quad L$set$29
	.uleb128 0
	.byte	0x4
	.set L$set$30,LCFI23-LFB4
	.long L$set$30
	.byte	0xe
	.uleb128 0xa30
	.byte	0x4
	.set L$set$31,LCFI24-LCFI23
	.long L$set$31
	.byte	0xe
	.uleb128 0x1ea30
	.byte	0x4
	.set L$set$32,LCFI25-LCFI24
	.long L$set$32
	.byte	0x9d
	.uleb128 0x3d46
	.byte	0x9e
	.uleb128 0x3d45
	.byte	0x4
	.set L$set$33,LCFI26-LCFI25
	.long L$set$33
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$34,LCFI27-LCFI26
	.long L$set$34
	.byte	0x99
	.uleb128 0x3d3e
	.byte	0x9a
	.uleb128 0x3d3d
	.byte	0x4
	.set L$set$35,LCFI28-LCFI27
	.long L$set$35
	.byte	0x93
	.uleb128 0x3d44
	.byte	0x94
	.uleb128 0x3d43
	.byte	0x95
	.uleb128 0x3d42
	.byte	0x96
	.uleb128 0x3d41
	.byte	0x97
	.uleb128 0x3d40
	.byte	0x98
	.uleb128 0x3d3f
	.byte	0x9b
	.uleb128 0x3d3c
	.byte	0x9c
	.uleb128 0x3d3b
	.byte	0x4
	.set L$set$36,LCFI29-LCFI28
	.long L$set$36
	.byte	0xa
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$37,LCFI30-LCFI29
	.long L$set$37
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
	.byte	0xdd
	.byte	0xde
	.byte	0x4
	.set L$set$38,LCFI31-LCFI30
	.long L$set$38
	.byte	0xe
	.uleb128 0x1e000
	.byte	0x4
	.set L$set$39,LCFI32-LCFI31
	.long L$set$39
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$40,LCFI33-LCFI32
	.long L$set$40
	.byte	0xb
	.align	3
LEFDE5:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
