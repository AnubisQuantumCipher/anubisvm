	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC7:
	.ascii "anubis_mlkem_sample.adb:35"
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_mlkem_sample.adb:44"
	.text
	.align	2
	.p2align 5,,15
_anubis_mlkem_sample__sample_ntt___wrapped_statements.0:
LFB3:
	sub	sp, sp, #560
LCFI0:
	mov	x2, 512
	mov	w1, 0
	stp	x29, x30, [sp]
LCFI1:
	mov	x29, sp
LCFI2:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI3:
	mov	x22, x16
	ldr	x0, [x16, 8]
	bl	_memset
	ldr	x0, [x22]
	mov	w1, 2147483647
	ldr	x2, [x0, 8]
	ldr	w2, [x2, 4]
	cmp	w2, w1
	beq	L21
	adrp	x20, lC3@PAGE
	add	x21, x29, 56
	ldp	x0, x1, [x0]
	add	x20, x20, lC3@PAGEOFF;
	mov	x2, x21
	mov	w4, 504
	mov	x3, x20
	bl	_anubis_sha3__shake128
	ldr	x2, [x22, 8]
	mov	w4, 0
	mov	w19, 0
	.p2align 5,,15
L3:
	mov	x0, x2
	add	x3, x2, 512
	b	L6
	.p2align 2,,3
L23:
	add	x0, x0, 2
	cmp	x0, x3
	beq	L22
L6:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L23
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L22:
	cmp	w4, 501
	bgt	L8
	add	w0, w4, 1
	add	w1, w4, 2
	sxtw	x5, w4
	sxtw	x0, w0
	sxtw	x1, w1
	add	w4, w4, 3
L9:
	ldrb	w0, [x21, x0]
	ldrb	w3, [x21, x1]
	ldrb	w1, [x21, x5]
	ubfiz	w5, w0, 8, 4
	lsl	w3, w3, 4
	orr	w0, w3, w0, lsr 4
	orr	w1, w1, w5
	cmp	w1, 3328
	bhi	L10
	cmp	w0, 3328
	mov	w3, 256
	strh	w1, [x2, w19, sxtw 1]
	add	w19, w19, 1
	ccmp	w19, w3, 4, ls
	beq	L11
L12:
	strh	w0, [x2, w19, sxtw 1]
	add	w19, w19, 1
L11:
	cmp	w19, 256
	bne	L3
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, 560
LCFI4:
	ret
	.p2align 2,,3
L10:
LCFI5:
	cmp	w0, 3328
	bhi	L3
	b	L12
	.p2align 2,,3
L8:
	ldr	x0, [x22]
	mov	w4, 504
	mov	x2, x21
	mov	x3, x20
	ldp	x0, x1, [x0]
	bl	_anubis_sha3__shake128
	ldr	x2, [x22, 8]
	mov	x5, 0
	mov	w4, 3
	mov	x1, 2
	mov	x0, 1
	b	L9
L21:
	adrp	x0, lC7@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE3:
	.const
	.align	2
lC3:
	.word	0
	.word	503
	.align	2
lC0:
	.word	1
	.word	51
	.align	2
lC2:
	.word	1
	.word	26
	.text
	.const
	.align	3
lC9:
	.ascii "Loop_Invariant failed at anubis_mlkem_sample.adb:164"
	.align	3
lC10:
	.ascii "Loop_Invariant failed at anubis_mlkem_sample.adb:166"
	.text
	.align	2
	.p2align 5,,15
_anubis_mlkem_sample__sample_poly_cbd___wrapped_statements.1:
LFB7:
	sub	sp, sp, #736
LCFI6:
	movi	v31.4s, 0
	mov	w1, 0
	mov	x2, 512
	stp	x29, x30, [sp]
LCFI7:
	mov	x29, sp
LCFI8:
	str	x21, [sp, 32]
LCFI9:
	add	x21, x29, 96
	stp	x19, x20, [sp, 16]
LCFI10:
	add	x19, x29, 224
	mov	x20, x16
	mov	x0, x19
	stp	xzr, xzr, [x29, 56]
	stp	xzr, xzr, [x29, 72]
	strb	wzr, [x29, 88]
	stp	q31, q31, [x21]
	stp	q31, q31, [x21, 32]
	stp	q31, q31, [x21, 64]
	stp	q31, q31, [x21, 96]
	bl	_memset
	ldr	x4, [x20, 8]
	add	x0, x29, 56
	mov	x1, -1
	add	x4, x4, 1
	.p2align 5,,15
L25:
	ldrb	w3, [x4, x1]
	add	x2, x0, x1
	add	x1, x1, 1
	strb	w3, [x2, 1]
	cmp	x1, 31
	bne	L25
	ldrb	w5, [x20, 16]
	adrp	x1, lC5@PAGE
	adrp	x3, lC6@PAGE
	mov	w4, 128
	add	x3, x3, lC6@PAGEOFF;
	mov	x2, x21
	add	x1, x1, lC5@PAGEOFF;
	strb	w5, [x29, 88]
	bl	_anubis_sha3__shake256
	mov	x1, x19
	mov	x0, x21
	bl	_anubis_cbd__cbd_2
	ldr	x0, [x20]
	mov	x2, 512
	mov	w1, 0
	bl	_memset
	ldr	x6, [x20]
	mvni	v24.4s, 0x7
	movi	v25.4s, 0x8
	movi	v26.8h, 0xd, lsl 8
	add	x2, x29, 736
	mov	x4, -1
	add	x5, x6, 2
	.p2align 5,,15
L34:
	cmn	w4, #1
	beq	L26
	mov	x0, x6
	add	x3, x5, w4, uxtw 1
	b	L28
	.p2align 2,,3
L41:
	add	x0, x0, 2
	cmp	x0, x3
	beq	L26
L28:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L41
	adrp	x0, lC9@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L26:
	adrp	x0, lC11@PAGE
	adrp	x1, lC12@PAGE
	ldr	q29, [x0, #lC11@PAGEOFF]
	mov	x0, x19
	ldr	q30, [x1, #lC12@PAGEOFF]
	b	L31
	.p2align 2,,3
L29:
	add	x0, x0, 16
	cmp	x0, x2
	beq	L32
L31:
	ldr	q31, [x0]
	mov	v27.16b, v29.16b
	mov	v28.16b, v30.16b
	add	v29.4s, v29.4s, v24.4s
	add	v30.4s, v30.4s, v25.4s
	cmhi	v31.8h, v31.8h, v26.8h
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbz	x1, L29
	fmov	w0, s28
	fmov	w1, s27
	sxtw	x0, w0
	sub	w1, w1, #1
	add	x3, x0, 1
	add	x3, x3, x1
	b	L30
	.p2align 2,,3
L42:
	add	x0, x0, 1
	cmp	x0, x3
	beq	L32
L30:
	add	x1, x19, x0, lsl 1
	ldrh	w1, [x1, 2]
	cmp	w1, 3328
	bls	L42
	adrp	x0, lC10@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L32:
	add	x0, x19, x4, lsl 1
	ldrh	w0, [x0, 2]
	strh	w0, [x5, x4, lsl 1]
	add	x4, x4, 1
	cmp	x4, 255
	bne	L34
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 736
LCFI11:
	ret
LFE7:
	.const
	.align	2
lC5:
	.word	0
	.word	32
	.align	2
lC6:
	.word	0
	.word	127
	.align	2
lC1:
	.word	1
	.word	52
	.text
	.const
	.align	3
lC13:
	.ascii "failed precondition from anubis_mlkem_sample.ads:34"
	.align	3
lC14:
	.ascii "failed postcondition from anubis_mlkem_sample.ads:35"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_sample__sample_ntt
_anubis_mlkem_sample__sample_ntt:
LFB2:
	stp	x29, x30, [sp, -80]!
LCFI12:
	mov	x29, sp
LCFI13:
	stp	x0, x1, [x29, 32]
	ldrsw	x0, [x1]
	ldr	w1, [x1, 4]
	str	x19, [sp, 16]
LCFI14:
	mov	x19, x2
	add	x2, x29, 32
	stp	x2, x19, [x29, 56]
	add	x2, x29, 80
	add	x0, x0, 33
	str	x2, [x29, 72]
	cmp	x0, w1, sxtw
	bne	L45
	mov	w0, 2147483647
	cmp	w1, w0
	beq	L45
	add	x16, x29, 56
	bl	_anubis_mlkem_sample__sample_ntt___wrapped_statements.0
	mov	x2, x19
	add	x1, x19, 512
	b	L47
	.p2align 2,,3
L53:
	add	x2, x2, 2
	cmp	x1, x2
	beq	L52
L47:
	ldrh	w0, [x2]
	cmp	w0, 3328
	bls	L53
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L52:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI15:
	ret
L45:
LCFI16:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_sample__generate_matrix
_anubis_mlkem_sample__generate_matrix:
LFB4:
	stp	x29, x30, [sp, -128]!
LCFI17:
	mov	x29, sp
LCFI18:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI19:
	adrp	x22, lC4@PAGE
	add	x22, x22, lC4@PAGEOFF;
	stp	x23, x24, [sp, 48]
LCFI20:
	add	x23, x29, 88
	mov	x24, 0
	str	x25, [sp, 64]
LCFI21:
	mov	x25, x1
	ldp	q31, q30, [x0]
	stp	q31, q30, [x23]
L56:
	and	w21, w24, 255
	mov	x20, x25
	mov	w19, -1
L55:
	add	w19, w19, 1
	mov	x2, x20
	strb	w21, [x29, 121]
	mov	x0, x23
	mov	x1, x22
	strb	w19, [x29, 120]
	add	x20, x20, 512
	bl	_anubis_mlkem_sample__sample_ntt
	cmp	w19, 3
	bne	L55
	add	x24, x24, 1
	add	x25, x25, 2048
	cmp	x24, 4
	bne	L56
	ldr	x25, [sp, 64]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 128
LCFI22:
	ret
LFE4:
	.const
	.align	2
lC4:
	.word	0
	.word	33
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_sample__generate_matrix_transpose
_anubis_mlkem_sample__generate_matrix_transpose:
LFB5:
	stp	x29, x30, [sp, -128]!
LCFI23:
	mov	x29, sp
LCFI24:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI25:
	adrp	x22, lC4@PAGE
	add	x22, x22, lC4@PAGEOFF;
	stp	x23, x24, [sp, 48]
LCFI26:
	add	x23, x29, 88
	mov	x24, 0
	str	x25, [sp, 64]
LCFI27:
	mov	x25, x1
	ldp	q31, q30, [x0]
	stp	q31, q30, [x23]
L62:
	and	w21, w24, 255
	mov	x20, x25
	mov	w19, -1
L61:
	add	w19, w19, 1
	mov	x2, x20
	strb	w21, [x29, 120]
	mov	x0, x23
	mov	x1, x22
	strb	w19, [x29, 121]
	add	x20, x20, 512
	bl	_anubis_mlkem_sample__sample_ntt
	cmp	w19, 3
	bne	L61
	add	x24, x24, 1
	add	x25, x25, 2048
	cmp	x24, 4
	bne	L62
	ldr	x25, [sp, 64]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 128
LCFI28:
	ret
LFE5:
	.const
	.align	3
lC15:
	.ascii "failed postcondition from anubis_mlkem_sample.ads:61"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_sample__sample_poly_cbd
_anubis_mlkem_sample__sample_poly_cbd:
LFB6:
	stp	x29, x30, [sp, -64]!
LCFI29:
	mov	x29, sp
LCFI30:
	add	x3, x29, 64
	add	x16, x29, 32
	str	x19, [sp, 16]
LCFI31:
	mov	x19, x2
	stp	x2, x0, [x29, 32]
	strb	w1, [x29, 48]
	str	x3, [x29, 56]
	bl	_anubis_mlkem_sample__sample_poly_cbd___wrapped_statements.1
	mov	x2, x19
	add	x1, x19, 512
	b	L68
	.p2align 2,,3
L73:
	add	x2, x2, 2
	cmp	x1, x2
	beq	L72
L68:
	ldrh	w0, [x2]
	cmp	w0, 3328
	bls	L73
	adrp	x0, lC15@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L72:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI32:
	ret
LFE6:
	.const
	.align	3
lC16:
	.ascii "anubis_mlkem_sample.ads"
	.space 1
	.align	3
lC17:
	.ascii "failed precondition from anubis_mlkem_sample.ads:70"
	.align	3
lC18:
	.ascii "Loop_Invariant failed at anubis_mlkem_sample.adb:183"
	.align	3
lC19:
	.ascii "failed postcondition from anubis_mlkem_sample.ads:71"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_sample__sample_vector_cbd
_anubis_mlkem_sample__sample_vector_cbd:
LFB8:
	stp	x29, x30, [sp, -128]!
LCFI33:
	mov	x29, sp
LCFI34:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI35:
	tbnz	w1, #31, L101
	mov	w21, w1
	cmp	w1, 252
	bgt	L102
	mov	x20, x2
	mov	x22, x0
	add	x26, x20, 512
	mov	x2, 2048
	mov	w1, 0
	mov	x0, x20
	add	w21, w21, 1
	mov	x27, x26
	add	x25, x20, 1024
	add	x23, x29, 96
	mov	w28, -1
	mov	w24, 512
	bl	_memset
L84:
	cmn	w28, #1
	beq	L77
	umaddl	x3, w28, w24, x25
	mov	x2, x26
	.p2align 5,,15
L80:
	sub	x0, x2, #512
	b	L79
	.p2align 2,,3
L104:
	add	x0, x0, 2
	cmp	x0, x2
	beq	L103
L79:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L104
	adrp	x0, lC18@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L103:
	add	x2, x2, 512
	cmp	x2, x3
	bne	L80
L77:
	sub	x19, x27, #512
	add	w0, w21, w28
	add	x1, x29, 128
	mov	x16, x23
	stp	x19, x22, [x29, 96]
	strb	w0, [x29, 112]
	str	x1, [x29, 120]
	bl	_anubis_mlkem_sample__sample_poly_cbd___wrapped_statements.1
	b	L82
	.p2align 2,,3
L106:
	add	x19, x19, 2
	cmp	x19, x27
	beq	L105
L82:
	ldrh	w0, [x19]
	cmp	w0, 3328
	bls	L106
	adrp	x0, lC15@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L105:
	add	w28, w28, 1
	add	x27, x19, 512
	cmp	w28, 3
	bne	L84
	add	x20, x20, 2560
L87:
	sub	x0, x26, #512
	b	L86
	.p2align 2,,3
L108:
	add	x0, x0, 2
	cmp	x26, x0
	beq	L107
L86:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L108
	adrp	x0, lC19@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L107:
	add	x26, x26, 512
	cmp	x26, x20
	bne	L87
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 128
LCFI36:
	ret
L102:
LCFI37:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L101:
	adrp	x0, lC16@PAGE
	mov	w1, 70
	add	x0, x0, lC16@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE8:
	.globl _anubis_mlkem_sample_E
	.data
	.align	1
_anubis_mlkem_sample_E:
	.space 2
	.literal16
	.align	4
lC11:
	.word	256
	.word	255
	.word	254
	.word	253
	.align	4
lC12:
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
	.quad	LFB3-.
	.set L$set$2,LFE3-LFB3
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB3
	.long L$set$3
	.byte	0xe
	.uleb128 0x230
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0x9d
	.uleb128 0x46
	.byte	0x9e
	.uleb128 0x45
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0x93
	.uleb128 0x44
	.byte	0x94
	.uleb128 0x43
	.byte	0x95
	.uleb128 0x42
	.byte	0x96
	.uleb128 0x41
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0xa
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
	.set L$set$8,LCFI5-LCFI4
	.long L$set$8
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$9,LEFDE3-LASFDE3
	.long L$set$9
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB7-.
	.set L$set$10,LFE7-LFB7
	.quad L$set$10
	.uleb128 0
	.byte	0x4
	.set L$set$11,LCFI6-LFB7
	.long L$set$11
	.byte	0xe
	.uleb128 0x2e0
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0x9d
	.uleb128 0x5c
	.byte	0x9e
	.uleb128 0x5b
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0x95
	.uleb128 0x58
	.byte	0x4
	.set L$set$15,LCFI10-LCFI9
	.long L$set$15
	.byte	0x93
	.uleb128 0x5a
	.byte	0x94
	.uleb128 0x59
	.byte	0x4
	.set L$set$16,LCFI11-LCFI10
	.long L$set$16
	.byte	0xd5
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
	.set L$set$17,LEFDE5-LASFDE5
	.long L$set$17
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB2-.
	.set L$set$18,LFE2-LFB2
	.quad L$set$18
	.uleb128 0
	.byte	0x4
	.set L$set$19,LCFI12-LFB2
	.long L$set$19
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0x93
	.uleb128 0x8
	.byte	0x4
	.set L$set$22,LCFI15-LCFI14
	.long L$set$22
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$23,LCFI16-LCFI15
	.long L$set$23
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$24,LEFDE7-LASFDE7
	.long L$set$24
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB4-.
	.set L$set$25,LFE4-LFB4
	.quad L$set$25
	.uleb128 0
	.byte	0x4
	.set L$set$26,LCFI17-LFB4
	.long L$set$26
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$27,LCFI18-LCFI17
	.long L$set$27
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$28,LCFI19-LCFI18
	.long L$set$28
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x4
	.set L$set$29,LCFI20-LCFI19
	.long L$set$29
	.byte	0x97
	.uleb128 0xa
	.byte	0x98
	.uleb128 0x9
	.byte	0x4
	.set L$set$30,LCFI21-LCFI20
	.long L$set$30
	.byte	0x99
	.uleb128 0x8
	.byte	0x4
	.set L$set$31,LCFI22-LCFI21
	.long L$set$31
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
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$32,LEFDE9-LASFDE9
	.long L$set$32
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB5-.
	.set L$set$33,LFE5-LFB5
	.quad L$set$33
	.uleb128 0
	.byte	0x4
	.set L$set$34,LCFI23-LFB5
	.long L$set$34
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$35,LCFI24-LCFI23
	.long L$set$35
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$36,LCFI25-LCFI24
	.long L$set$36
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x4
	.set L$set$37,LCFI26-LCFI25
	.long L$set$37
	.byte	0x97
	.uleb128 0xa
	.byte	0x98
	.uleb128 0x9
	.byte	0x4
	.set L$set$38,LCFI27-LCFI26
	.long L$set$38
	.byte	0x99
	.uleb128 0x8
	.byte	0x4
	.set L$set$39,LCFI28-LCFI27
	.long L$set$39
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
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$40,LEFDE11-LASFDE11
	.long L$set$40
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB6-.
	.set L$set$41,LFE6-LFB6
	.quad L$set$41
	.uleb128 0
	.byte	0x4
	.set L$set$42,LCFI29-LFB6
	.long L$set$42
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$43,LCFI30-LCFI29
	.long L$set$43
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$44,LCFI31-LCFI30
	.long L$set$44
	.byte	0x93
	.uleb128 0x6
	.byte	0x4
	.set L$set$45,LCFI32-LCFI31
	.long L$set$45
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
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
	.quad	LFB8-.
	.set L$set$47,LFE8-LFB8
	.quad L$set$47
	.uleb128 0
	.byte	0x4
	.set L$set$48,LCFI33-LFB8
	.long L$set$48
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$49,LCFI34-LCFI33
	.long L$set$49
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$50,LCFI35-LCFI34
	.long L$set$50
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x97
	.uleb128 0xa
	.byte	0x98
	.uleb128 0x9
	.byte	0x99
	.uleb128 0x8
	.byte	0x9a
	.uleb128 0x7
	.byte	0x9b
	.uleb128 0x6
	.byte	0x9c
	.uleb128 0x5
	.byte	0x4
	.set L$set$51,LCFI36-LCFI35
	.long L$set$51
	.byte	0xa
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
	.byte	0x4
	.set L$set$52,LCFI37-LCFI36
	.long L$set$52
	.byte	0xb
	.align	3
LEFDE13:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
