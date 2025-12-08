	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_mldsa_sample.ads:67"
	.align	3
lC11:
	.ascii "anubis_mldsa_sample.adb:29"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_sample__rejnttpoly
_anubis_mldsa_sample__rejnttpoly:
LFB2:
	sub	sp, sp, #912
LCFI0:
	stp	x29, x30, [sp]
LCFI1:
	mov	x29, sp
LCFI2:
	stp	x19, x20, [sp, 16]
LCFI3:
	mov	w19, 2147483647
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
LCFI4:
	mov	x24, x1
	ldr	w1, [x1, 4]
	cmp	w1, w19
	beq	L10
	mov	x20, x2
	mov	x21, x0
	mov	x2, 1024
	mov	w1, 0
	mov	x0, x20
	bl	_memset
	ldr	w0, [x24, 4]
	cmp	w0, w19
	beq	L11
	adrp	x22, lC2@PAGE
	add	x19, x29, 72
	add	x22, x22, lC2@PAGEOFF;
	mov	w4, 840
	mov	x2, x19
	mov	x3, x22
	mov	x0, x21
	mov	x1, x24
	bl	_anubis_sha3__shake128
	mov	w4, 0
	mov	w23, 0
	.p2align 5,,15
L4:
	cmp	w4, 837
	bgt	L5
	add	w2, w4, 1
	add	w0, w4, 2
	sxtw	x3, w4
	sxtw	x2, w2
	sxtw	x0, w0
	add	w4, w4, 3
L6:
	ldrb	w6, [x19, x3]
	ldrb	w3, [x19, x2]
	ldrb	w5, [x19, x0]
	orr	w3, w6, w3, lsl 8
	ubfiz	w5, w5, 16, 7
	orr	w3, w3, w5
	cmp	w3, 8380416
	bhi	L4
	str	w3, [x20, w23, sxtw 2]
	add	w23, w23, 1
	cmp	w23, 256
	bne	L4
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 912
LCFI5:
	ret
	.p2align 2,,3
L5:
LCFI6:
	mov	w4, 840
	mov	x2, x19
	mov	x3, x22
	mov	x0, x21
	mov	x1, x24
	bl	_anubis_sha3__shake128
	mov	x3, 0
	mov	w4, 3
	mov	x0, 2
	mov	x2, 1
	b	L6
L10:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L11:
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE2:
	.const
	.align	2
lC2:
	.word	0
	.word	839
	.align	2
lC0:
	.word	1
	.word	51
	.align	2
lC1:
	.word	1
	.word	26
	.text
	.const
	.align	3
lC12:
	.ascii "failed precondition from anubis_mldsa_sample.ads:77"
	.align	3
lC13:
	.ascii "anubis_mldsa_sample.adb:80"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_sample__rejboundedpoly
_anubis_mldsa_sample__rejboundedpoly:
LFB3:
	stp	x29, x30, [sp, -336]!
LCFI7:
	mov	x29, sp
LCFI8:
	stp	x19, x20, [sp, 16]
LCFI9:
	mov	w19, 2147483647
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI10:
	mov	x26, x1
	ldr	w1, [x1, 4]
	cmp	w1, w19
	beq	L29
	mov	x23, x2
	mov	x21, x0
	mov	x2, 1024
	mov	w1, 0
	mov	x0, x23
	bl	_memset
	ldr	w0, [x26, 4]
	cmp	w0, w19
	beq	L30
	adrp	x22, lC3@PAGE
	add	x24, x29, 80
	add	x22, x22, lC3@PAGEOFF;
	mov	w4, 256
	mov	x2, x24
	mov	x3, x22
	mov	x0, x21
	mov	x1, x26
	mov	w25, 57347
	mov	w19, 0
	bl	_anubis_sha3__shake256
	mov	w4, 0
	mov	w20, 2
	movk	w25, 0x7f, lsl 16
	.p2align 5,,15
L15:
	cmp	w4, 256
	beq	L17
L31:
	sxtw	x0, w4
	add	w4, w4, 1
L18:
	ldrb	w3, [x24, x0]
	and	w5, w3, 15
	lsr	w3, w3, 4
	cmp	w5, 4
	bhi	L19
	sub	w0, w25, w5
	subs	w5, w20, w5
	csel	w5, w0, w5, mi
	str	w5, [x23, w19, sxtw 2]
	add	w19, w19, 1
	cmp	w19, 256
	ccmp	w3, 4, 2, ne
	bhi	L21
L23:
	sub	w0, w25, w3
	subs	w3, w20, w3
	csel	w3, w0, w3, mi
	str	w3, [x23, w19, sxtw 2]
	add	w19, w19, 1
L21:
	cmp	w19, 256
	bne	L15
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 336
LCFI11:
	ret
	.p2align 2,,3
L19:
LCFI12:
	cmp	w3, 4
	bls	L23
	cmp	w4, 256
	bne	L31
L17:
	mov	x0, x21
	mov	x2, x24
	mov	x3, x22
	mov	x1, x26
	bl	_anubis_sha3__shake256
	mov	x0, 0
	mov	w4, 1
	b	L18
L29:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L30:
	adrp	x0, lC13@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE3:
	.const
	.align	2
lC3:
	.word	0
	.word	255
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_sample__expanda
_anubis_mldsa_sample__expanda:
LFB4:
	stp	x29, x30, [sp, -128]!
LCFI13:
	mov	x29, sp
LCFI14:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI15:
	adrp	x22, lC4@PAGE
	add	x22, x22, lC4@PAGEOFF;
	stp	x23, x24, [sp, 48]
LCFI16:
	add	x23, x29, 88
	mov	w24, -1
	stp	x25, x26, [sp, 64]
LCFI17:
	mov	x25, x1
	mov	x26, 7168
	ldp	q31, q30, [x0]
	stp	q31, q30, [x23]
	.p2align 5,,15
L34:
	add	w24, w24, 1
	mov	x20, x25
	and	w21, w24, 255
	mov	w19, -1
	.p2align 5,,15
L33:
	add	w19, w19, 1
	mov	x2, x20
	strb	w21, [x29, 121]
	mov	x0, x23
	mov	x1, x22
	strb	w19, [x29, 120]
	add	x20, x20, 1024
	bl	_anubis_mldsa_sample__rejnttpoly
	cmp	w19, 6
	bne	L33
	add	x25, x25, x26
	cmp	w24, 7
	bne	L34
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 128
LCFI18:
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
	.globl _anubis_mldsa_sample__expands
_anubis_mldsa_sample__expands:
LFB5:
	stp	x29, x30, [sp, -144]!
LCFI19:
	mov	x29, sp
LCFI20:
	stp	x19, x20, [sp, 16]
LCFI21:
	mov	w19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI22:
	adrp	x22, lC5@PAGE
	mov	x21, x1
	add	x22, x22, lC5@PAGEOFF;
	str	x23, [sp, 48]
LCFI23:
	add	x23, x29, 72
	stp	xzr, xzr, [x29, 104]
	stp	xzr, xzr, [x29, 120]
	ldp	q31, q30, [x0]
	stp	q31, q30, [x23]
	.p2align 5,,15
L39:
	mov	x2, x21
	mov	x0, x23
	strb	w19, [x29, 136]
	mov	x1, x22
	add	w19, w19, 1
	strb	wzr, [x29, 137]
	bl	_anubis_mldsa_sample__rejboundedpoly
	add	x21, x21, 1024
	cmp	w19, 7
	bne	L39
	.p2align 5,,15
L40:
	mov	x2, x20
	mov	x0, x23
	strb	w19, [x29, 136]
	mov	x1, x22
	add	w19, w19, 1
	strb	wzr, [x29, 137]
	bl	_anubis_mldsa_sample__rejboundedpoly
	add	x20, x20, 1024
	cmp	w19, 15
	bne	L40
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 144
LCFI24:
	ret
LFE5:
	.const
	.align	2
lC5:
	.word	0
	.word	65
	.text
	.const
	.align	3
lC14:
	.ascii "anubis_mldsa_sample.ads"
	.space 1
	.align	3
lC15:
	.ascii "failed precondition from anubis_mldsa_sample.ads:48"
	.align	3
lC16:
	.ascii "Loop_Invariant failed at anubis_mldsa_sample.adb:218"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_sample__expandmask
_anubis_mldsa_sample__expandmask:
LFB6:
	sub	sp, sp, #816
LCFI25:
	stp	x29, x30, [sp]
LCFI26:
	mov	x29, sp
LCFI27:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
LCFI28:
	tbnz	w1, #31, L59
	mov	w27, w1
	cmp	w1, 1048576
	bgt	L60
	mov	x26, x2
	mov	x22, x0
	stp	xzr, xzr, [x29, 136]
	mov	w1, 0
	mov	x2, 7168
	mov	x0, x26
	adrp	x20, lC5@PAGE
	stp	xzr, xzr, [x29, 152]
	mov	w25, 57345
	add	x21, x29, 104
	add	x23, x29, 176
	add	x20, x20, lC5@PAGEOFF;
	mov	w24, 524288
	movk	w25, 0x87, lsl 16
	bl	_memset
	ldp	q31, q30, [x22]
	mov	x0, 7168
	adrp	x22, lC6@PAGE
	add	x19, x26, x0
	stp	q31, q30, [x21]
	.p2align 5,,15
L54:
	mov	x2, x23
	add	x3, x22, lC6@PAGEOFF;
	strh	w27, [x29, 168]
	mov	x0, x21
	mov	w4, 640
	mov	x1, x20
	bl	_anubis_sha3__shake256
	mov	x2, x26
	mov	w3, 0
	mov	w0, 0
	b	L47
	.p2align 2,,3
L62:
	add	w1, w0, 1
	ldrb	w5, [x23, w0, sxtw]
	ldrb	w1, [x23, w1, sxtw]
	ldrb	w4, [x23, w4, sxtw]
	orr	w1, w5, w1, lsl 8
	ubfiz	w4, w4, 16, 4
	orr	w1, w1, w4
	subs	w4, w24, w1
	bmi	L49
	str	w4, [x2]
L53:
	add	w3, w3, 1
	add	x2, x2, 4
	asr	w1, w3, 1
	add	w1, w1, w1, lsl 2
	cmp	w1, w0
	bne	L61
L47:
	add	w4, w0, 2
	tbz	x3, 0, L62
	add	w5, w0, 3
	ldrb	w4, [x23, w4, sxtw]
	add	w1, w0, 4
	add	w0, w0, 5
	ldrb	w5, [x23, w5, sxtw]
	ldrb	w1, [x23, w1, sxtw]
	lsl	w5, w5, 4
	orr	w4, w5, w4, lsr 4
	orr	w1, w4, w1, lsl 12
	sub	w4, w25, w1
	subs	w1, w24, w1
	csel	w1, w4, w1, mi
	str	w1, [x2]
	cmp	w3, 255
	bne	L53
	add	x26, x26, 1024
	add	w27, w27, 1
	cmp	x19, x26
	bne	L54
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, 816
LCFI29:
	ret
	.p2align 2,,3
L49:
LCFI30:
	sub	w1, w25, w1
	add	w3, w3, 1
	add	x2, x2, 4
	str	w1, [x2, -4]
	asr	w1, w3, 1
	add	w1, w1, w1, lsl 2
	cmp	w1, w0
	beq	L47
L61:
	adrp	x0, lC16@PAGE
	adrp	x1, lC7@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC7@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L60:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L59:
	adrp	x0, lC14@PAGE
	mov	w1, 48
	add	x0, x0, lC14@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE6:
	.const
	.align	2
lC6:
	.word	0
	.word	639
	.align	2
lC7:
	.word	1
	.word	52
	.text
	.const
	.align	3
lC17:
	.ascii "failed precondition from anubis_mldsa_sample.ads:57"
	.align	3
lC18:
	.ascii "anubis_mldsa_sample.adb:269"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_sample__sampleinball
_anubis_mldsa_sample__sampleinball:
LFB7:
	stp	x29, x30, [sp, -448]!
LCFI31:
	mov	x29, sp
LCFI32:
	stp	x19, x20, [sp, 16]
LCFI33:
	mov	x20, x1
	stp	x21, x22, [sp, 32]
LCFI34:
	mov	w22, 2147483647
	ldr	w1, [x1, 4]
	cmp	w1, w22
	beq	L74
	mov	x19, x2
	mov	x21, x0
	mov	x2, 1024
	mov	w1, 0
	mov	x0, x19
	bl	_memset
	ldr	w0, [x20, 4]
	cmp	w0, w22
	beq	L75
	add	x22, x29, 48
	adrp	x3, lC9@PAGE
	mov	x0, x21
	mov	w4, 400
	mov	x1, x20
	mov	x2, x22
	add	x3, x3, lC9@PAGEOFF;
	bl	_anubis_sha3__shake256
	ldr	x5, [x29, 48]
	mov	x4, 196
	mov	w0, 8
	mov	w7, 8380416
	.p2align 5,,15
L70:
	add	x3, x22, w0, sxtw
	b	L68
	.p2align 2,,3
L76:
	ldrb	w1, [x3], 1
	add	w0, w0, 1
	cmp	w1, w4
	ble	L67
L68:
	cmp	w0, 399
	ble	L76
	mov	w1, w4
L67:
	ldr	w6, [x19, w1, sxtw 2]
	tst	x5, 1
	csinc	w3, w7, wzr, ne
	lsr	x5, x5, 1
	str	w6, [x19, x4, lsl 2]
	add	x4, x4, 1
	str	w3, [x19, w1, sxtw 2]
	cmp	x4, 256
	bne	L70
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 448
LCFI35:
	ret
L74:
LCFI36:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L75:
	adrp	x0, lC18@PAGE
	adrp	x1, lC8@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC8@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE7:
	.const
	.align	2
lC9:
	.word	0
	.word	399
	.align	2
lC8:
	.word	1
	.word	27
	.text
	.globl _anubis_mldsa_sample_E
	.data
	.align	1
_anubis_mldsa_sample_E:
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
	.quad	LFB2-.
	.set L$set$2,LFE2-LFB2
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB2
	.long L$set$3
	.byte	0xe
	.uleb128 0x390
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0x9d
	.uleb128 0x72
	.byte	0x9e
	.uleb128 0x71
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0x93
	.uleb128 0x70
	.byte	0x94
	.uleb128 0x6f
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0x95
	.uleb128 0x6e
	.byte	0x96
	.uleb128 0x6d
	.byte	0x97
	.uleb128 0x6c
	.byte	0x98
	.uleb128 0x6b
	.byte	0x4
	.set L$set$8,LCFI5-LCFI4
	.long L$set$8
	.byte	0xa
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
	.set L$set$9,LCFI6-LCFI5
	.long L$set$9
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$10,LEFDE3-LASFDE3
	.long L$set$10
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB3-.
	.set L$set$11,LFE3-LFB3
	.quad L$set$11
	.uleb128 0
	.byte	0x4
	.set L$set$12,LCFI7-LFB3
	.long L$set$12
	.byte	0xe
	.uleb128 0x150
	.byte	0x9d
	.uleb128 0x2a
	.byte	0x9e
	.uleb128 0x29
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0x93
	.uleb128 0x28
	.byte	0x94
	.uleb128 0x27
	.byte	0x4
	.set L$set$15,LCFI10-LCFI9
	.long L$set$15
	.byte	0x95
	.uleb128 0x26
	.byte	0x96
	.uleb128 0x25
	.byte	0x97
	.uleb128 0x24
	.byte	0x98
	.uleb128 0x23
	.byte	0x99
	.uleb128 0x22
	.byte	0x9a
	.uleb128 0x21
	.byte	0x4
	.set L$set$16,LCFI11-LCFI10
	.long L$set$16
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
	.set L$set$17,LCFI12-LCFI11
	.long L$set$17
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$18,LEFDE5-LASFDE5
	.long L$set$18
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB4-.
	.set L$set$19,LFE4-LFB4
	.quad L$set$19
	.uleb128 0
	.byte	0x4
	.set L$set$20,LCFI13-LFB4
	.long L$set$20
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$22,LCFI15-LCFI14
	.long L$set$22
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x4
	.set L$set$23,LCFI16-LCFI15
	.long L$set$23
	.byte	0x97
	.uleb128 0xa
	.byte	0x98
	.uleb128 0x9
	.byte	0x4
	.set L$set$24,LCFI17-LCFI16
	.long L$set$24
	.byte	0x99
	.uleb128 0x8
	.byte	0x9a
	.uleb128 0x7
	.byte	0x4
	.set L$set$25,LCFI18-LCFI17
	.long L$set$25
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
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$26,LEFDE7-LASFDE7
	.long L$set$26
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB5-.
	.set L$set$27,LFE5-LFB5
	.quad L$set$27
	.uleb128 0
	.byte	0x4
	.set L$set$28,LCFI19-LFB5
	.long L$set$28
	.byte	0xe
	.uleb128 0x90
	.byte	0x9d
	.uleb128 0x12
	.byte	0x9e
	.uleb128 0x11
	.byte	0x4
	.set L$set$29,LCFI20-LCFI19
	.long L$set$29
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$30,LCFI21-LCFI20
	.long L$set$30
	.byte	0x93
	.uleb128 0x10
	.byte	0x94
	.uleb128 0xf
	.byte	0x4
	.set L$set$31,LCFI22-LCFI21
	.long L$set$31
	.byte	0x95
	.uleb128 0xe
	.byte	0x96
	.uleb128 0xd
	.byte	0x4
	.set L$set$32,LCFI23-LCFI22
	.long L$set$32
	.byte	0x97
	.uleb128 0xc
	.byte	0x4
	.set L$set$33,LCFI24-LCFI23
	.long L$set$33
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
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$34,LEFDE9-LASFDE9
	.long L$set$34
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB6-.
	.set L$set$35,LFE6-LFB6
	.quad L$set$35
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI25-LFB6
	.long L$set$36
	.byte	0xe
	.uleb128 0x330
	.byte	0x4
	.set L$set$37,LCFI26-LCFI25
	.long L$set$37
	.byte	0x9d
	.uleb128 0x66
	.byte	0x9e
	.uleb128 0x65
	.byte	0x4
	.set L$set$38,LCFI27-LCFI26
	.long L$set$38
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$39,LCFI28-LCFI27
	.long L$set$39
	.byte	0x93
	.uleb128 0x64
	.byte	0x94
	.uleb128 0x63
	.byte	0x95
	.uleb128 0x62
	.byte	0x96
	.uleb128 0x61
	.byte	0x97
	.uleb128 0x60
	.byte	0x98
	.uleb128 0x5f
	.byte	0x99
	.uleb128 0x5e
	.byte	0x9a
	.uleb128 0x5d
	.byte	0x9b
	.uleb128 0x5c
	.byte	0x4
	.set L$set$40,LCFI29-LCFI28
	.long L$set$40
	.byte	0xa
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
	.byte	0x4
	.set L$set$41,LCFI30-LCFI29
	.long L$set$41
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$42,LEFDE11-LASFDE11
	.long L$set$42
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB7-.
	.set L$set$43,LFE7-LFB7
	.quad L$set$43
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI31-LFB7
	.long L$set$44
	.byte	0xe
	.uleb128 0x1c0
	.byte	0x9d
	.uleb128 0x38
	.byte	0x9e
	.uleb128 0x37
	.byte	0x4
	.set L$set$45,LCFI32-LCFI31
	.long L$set$45
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$46,LCFI33-LCFI32
	.long L$set$46
	.byte	0x93
	.uleb128 0x36
	.byte	0x94
	.uleb128 0x35
	.byte	0x4
	.set L$set$47,LCFI34-LCFI33
	.long L$set$47
	.byte	0x95
	.uleb128 0x34
	.byte	0x96
	.uleb128 0x33
	.byte	0x4
	.set L$set$48,LCFI35-LCFI34
	.long L$set$48
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
	.set L$set$49,LCFI36-LCFI35
	.long L$set$49
	.byte	0xb
	.align	3
LEFDE11:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
