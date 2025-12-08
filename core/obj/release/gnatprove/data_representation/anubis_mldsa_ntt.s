	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__Ttwiddle_arrayBIP
_anubis_mldsa_ntt__Ttwiddle_arrayBIP:
LFB2:
	ret
LFE2:
	.const
	.align	3
lC5:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:86"
	.align	3
lC6:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:98"
	.align	3
lC7:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:110"
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:122"
	.align	3
lC9:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:134"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__ntt
_anubis_mldsa_ntt__ntt:
LFB5:
	mov	x8, 33889
	mov	w12, 57343
	stp	x29, x30, [sp, -48]!
LCFI0:
	movk	x8, 0x6014, lsl 16
	mov	w10, 57345
	movk	x8, 0x1c0, lsl 32
	mov	x9, 57345
	mov	w13, 25847
	movk	w12, 0xfc7f, lsl 16
	mov	x3, x0
	add	x11, x0, 512
	mov	x4, x0
	movk	w10, 0x7f, lsl 16
	movk	x8, 0x2008, lsl 48
	movk	x9, 0x7f, lsl 16
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI2:
	.p2align 5,,15
L4:
	ldr	w1, [x4, 512]
	add	x4, x4, 4
	ldr	w2, [x4, -4]
	umull	x1, w1, w13
	add	x5, x2, x9
	umull	x6, w1, w12
	umaddl	x6, w6, w10, x1
	lsr	x6, x6, 32
	umulh	x1, x6, x8
	lsr	x1, x1, 20
	lsl	x7, x1, 10
	sub	x7, x7, x1
	add	x1, x1, x7, lsl 13
	sub	x1, x6, x1
	add	x2, x1, x2
	sub	x1, x5, x1
	umulh	x14, x2, x8
	umulh	x7, x1, x8
	lsr	x14, x14, 20
	lsr	x7, x7, 20
	lsl	x6, x14, 10
	lsl	x5, x7, 10
	sub	x6, x6, x14
	sub	x5, x5, x7
	add	x6, x14, x6, lsl 13
	add	x5, x7, x5, lsl 13
	sub	x2, x2, x6
	sub	x1, x1, x5
	str	w2, [x4, -4]
	str	w1, [x4, 508]
	cmp	x4, x11
	bne	L4
	mov	x8, 33889
	adrp	x4, _anubis_mldsa_ntt__zetas@PAGE
	movk	x8, 0x6014, lsl 16
	mov	w13, 57343
	mov	w12, 57345
	movk	x8, 0x1c0, lsl 32
	mov	x11, 57345
	mov	x14, 2
	add	x16, x4, _anubis_mldsa_ntt__zetas@PAGEOFF;
	mov	w1, -1
	add	x15, x0, 768
	movk	w13, 0xfc7f, lsl 16
	movk	w12, 0x7f, lsl 16
	movk	x8, 0x2008, lsl 48
	movk	x11, 0x7f, lsl 16
L6:
	add	w5, w1, 1
	ldr	w10, [x16, x14, lsl 2]
	ubfiz	x5, x5, 9, 32
	sub	x9, x5, #512
	add	x5, x0, x5
	add	x9, x15, x9
	.p2align 5,,15
L5:
	ldr	w1, [x5, 256]
	add	x5, x5, 4
	ldr	w2, [x5, -4]
	umull	x1, w10, w1
	add	x6, x2, x11
	umull	x7, w1, w13
	umaddl	x7, w7, w12, x1
	lsr	x7, x7, 32
	umulh	x1, x7, x8
	lsr	x1, x1, 20
	lsl	x17, x1, 10
	sub	x17, x17, x1
	add	x1, x1, x17, lsl 13
	sub	x1, x7, x1
	add	x2, x1, x2
	sub	x1, x6, x1
	umulh	x30, x2, x8
	umulh	x17, x1, x8
	lsr	x30, x30, 20
	lsr	x17, x17, 20
	lsl	x7, x30, 10
	lsl	x6, x17, 10
	sub	x7, x7, x30
	sub	x6, x6, x17
	add	x7, x30, x7, lsl 13
	add	x6, x17, x6, lsl 13
	sub	x2, x2, x7
	sub	x1, x1, x6
	str	w2, [x5, -4]
	str	w1, [x5, 252]
	cmp	x5, x9
	bne	L5
	add	x9, x14, 1
	mov	w1, 0
	mov	x14, 3
	cmp	x9, 4
	bne	L6
	mov	x8, 33889
	mov	w14, 57343
	movk	x8, 0x6014, lsl 16
	mov	w13, 57345
	movk	x8, 0x1c0, lsl 32
	mov	x12, 57345
	add	x10, x0, 128
	mov	x15, 0
	add	x16, x4, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	w14, 0xfc7f, lsl 16
	movk	w13, 0x7f, lsl 16
	movk	x8, 0x2008, lsl 48
	movk	x12, 0x7f, lsl 16
L8:
	ldr	w11, [x16, x9, lsl 2]
	sub	x5, x10, #128
	.p2align 5,,15
L7:
	add	x5, x5, 4
	ldr	w1, [x5, 124]
	ldr	w2, [x5, -4]
	umull	x1, w11, w1
	add	x6, x2, x12
	umull	x7, w1, w14
	umaddl	x7, w7, w13, x1
	lsr	x7, x7, 32
	umulh	x1, x7, x8
	lsr	x1, x1, 20
	lsl	x17, x1, 10
	sub	x17, x17, x1
	add	x1, x1, x17, lsl 13
	sub	x1, x7, x1
	add	x2, x1, x2
	sub	x1, x6, x1
	umulh	x30, x2, x8
	umulh	x17, x1, x8
	lsr	x30, x30, 20
	lsr	x17, x17, 20
	lsl	x7, x30, 10
	lsl	x6, x17, 10
	sub	x7, x7, x30
	sub	x6, x6, x17
	add	x7, x30, x7, lsl 13
	add	x6, x17, x6, lsl 13
	sub	x2, x2, x7
	sub	x1, x1, x6
	str	w2, [x5, -4]
	str	w1, [x5, 124]
	cmp	x10, x5
	bne	L7
	add	x15, x15, 64
	add	x10, x10, 256
	add	x1, x9, 1
	cmp	x15, 256
	beq	L38
	mov	x9, x1
	b	L8
L38:
	mov	x8, 33889
	add	w17, w9, 1
	movk	x8, 0x6014, lsl 16
	mov	w14, 57343
	add	x17, x16, x17, lsl 2
	mov	w13, 57345
	movk	x8, 0x1c0, lsl 32
	mov	x12, 57345
	mov	x15, 0
	sub	x10, x0, #64
	movk	w14, 0xfc7f, lsl 16
	movk	w13, 0x7f, lsl 16
	movk	x8, 0x2008, lsl 48
	movk	x12, 0x7f, lsl 16
	.p2align 5,,15
L11:
	sub	w1, w9, #7
	cmp	w1, 7
	bhi	L39
	ldr	w11, [x17, x15, lsl 2]
	add	w16, w9, 1
	sub	x5, x10, #64
	.p2align 5,,15
L10:
	add	x5, x5, 4
	ldr	w1, [x5, 188]
	ldr	w2, [x5, 124]
	umull	x1, w11, w1
	add	x6, x2, x12
	umull	x7, w1, w14
	umaddl	x7, w7, w13, x1
	lsr	x7, x7, 32
	umulh	x1, x7, x8
	lsr	x1, x1, 20
	lsl	x19, x1, 10
	sub	x19, x19, x1
	add	x1, x1, x19, lsl 13
	sub	x1, x7, x1
	add	x2, x1, x2
	sub	x1, x6, x1
	umulh	x19, x2, x8
	umulh	x30, x1, x8
	lsr	x19, x19, 20
	lsr	x30, x30, 20
	lsl	x7, x19, 10
	lsl	x6, x30, 10
	sub	x7, x7, x19
	sub	x6, x6, x30
	add	x7, x19, x7, lsl 13
	add	x6, x30, x6, lsl 13
	sub	x2, x2, x7
	sub	x1, x1, x6
	str	w2, [x5, 124]
	str	w1, [x5, 188]
	cmp	x10, x5
	bne	L10
	add	x15, x15, 1
	add	x10, x10, 128
	cmp	x15, 8
	beq	L40
	mov	w9, w16
	b	L11
L40:
	add	w9, w9, 2
	mov	w14, 57343
	sxtw	x8, w9
	mov	x9, 33889
	movk	x9, 0x6014, lsl 16
	mov	w13, 57345
	movk	x9, 0x1c0, lsl 32
	mov	x12, 57345
	sub	x10, x0, #32
	add	x16, x0, 992
	add	x15, x4, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	w14, 0xfc7f, lsl 16
	movk	w13, 0x7f, lsl 16
	movk	x9, 0x2008, lsl 48
	movk	x12, 0x7f, lsl 16
	.p2align 5,,15
L14:
	sub	w1, w8, #16
	cmp	w1, 15
	bhi	L41
	ldr	w11, [x15, x8, lsl 2]
	sub	x5, x10, #32
	.p2align 5,,15
L13:
	add	x5, x5, 4
	ldr	w1, [x5, 92]
	ldr	w2, [x5, 60]
	umull	x1, w11, w1
	add	x6, x2, x12
	umull	x7, w1, w14
	umaddl	x7, w7, w13, x1
	lsr	x7, x7, 32
	umulh	x1, x7, x9
	lsr	x1, x1, 20
	lsl	x17, x1, 10
	sub	x17, x17, x1
	add	x1, x1, x17, lsl 13
	sub	x1, x7, x1
	add	x2, x1, x2
	sub	x1, x6, x1
	umulh	x19, x2, x9
	umulh	x17, x1, x9
	lsr	x19, x19, 20
	lsr	x17, x17, 20
	lsl	x7, x19, 10
	lsl	x6, x17, 10
	sub	x7, x7, x19
	sub	x6, x6, x17
	add	x7, x19, x7, lsl 13
	add	x6, x17, x6, lsl 13
	sub	x2, x2, x7
	sub	x1, x1, x6
	str	w2, [x5, 60]
	str	w1, [x5, 92]
	cmp	x10, x5
	bne	L13
	add	x10, x10, 64
	add	x1, x8, 1
	cmp	x16, x10
	beq	L42
	mov	x8, x1
	b	L14
L42:
	mov	x9, 33889
	add	w8, w8, 1
	movk	x9, 0x6014, lsl 16
	mov	w14, 57343
	mov	w13, 57345
	movk	x9, 0x1c0, lsl 32
	mov	x12, 57345
	sub	x10, x0, #16
	mov	x15, 0
	sxtw	x8, w8
	add	x16, x4, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	w14, 0xfc7f, lsl 16
	movk	w13, 0x7f, lsl 16
	movk	x9, 0x2008, lsl 48
	movk	x12, 0x7f, lsl 16
	.p2align 5,,15
L17:
	sub	w1, w8, #32
	cmp	w1, 31
	bhi	L43
	ldr	w11, [x16, x8, lsl 2]
	sub	x5, x10, #16
L16:
	add	x5, x5, 4
	ldr	w1, [x5, 44]
	ldr	w2, [x5, 28]
	umull	x1, w11, w1
	add	x6, x2, x12
	umull	x7, w1, w14
	umaddl	x7, w7, w13, x1
	lsr	x7, x7, 32
	umulh	x1, x7, x9
	lsr	x1, x1, 20
	lsl	x17, x1, 10
	sub	x17, x17, x1
	add	x1, x1, x17, lsl 13
	sub	x1, x7, x1
	add	x2, x1, x2
	sub	x1, x6, x1
	umulh	x19, x2, x9
	umulh	x17, x1, x9
	lsr	x19, x19, 20
	lsr	x17, x17, 20
	lsl	x7, x19, 10
	lsl	x6, x17, 10
	sub	x7, x7, x19
	sub	x6, x6, x17
	add	x7, x19, x7, lsl 13
	add	x6, x17, x6, lsl 13
	sub	x2, x2, x7
	sub	x1, x1, x6
	str	w2, [x5, 28]
	str	w1, [x5, 44]
	cmp	x10, x5
	bne	L16
	add	x15, x15, 8
	add	x10, x10, 32
	add	x1, x8, 1
	cmp	x15, 256
	beq	L44
	mov	x8, x1
	b	L17
L44:
	mov	x10, 33889
	add	w30, w8, 1
	movk	x10, 0x6014, lsl 16
	add	x30, x16, x30, lsl 2
	mov	w17, 57343
	mov	w16, 57345
	movk	x10, 0x1c0, lsl 32
	mov	x15, 57345
	mov	x11, 0
	movk	w17, 0xfc7f, lsl 16
	movk	w16, 0x7f, lsl 16
	movk	x10, 0x2008, lsl 48
	movk	x15, 0x7f, lsl 16
	b	L20
	.p2align 2,,3
L47:
	add	x11, x11, 4
	cmp	x11, 256
	beq	L45
	mov	w8, w19
L20:
	sub	w1, w8, #63
	cmp	w1, 63
	bhi	L46
	ldr	w14, [x30, x11]
	add	w19, w8, 1
	mov	w13, 2
	mov	w7, -1
L19:
	add	w7, w7, 1
	add	x9, x11, w7, sxtw
	add	x12, x9, 2
	ldr	w1, [x0, x12, lsl 2]
	mov	w7, 0
	ldr	w2, [x0, x9, lsl 2]
	umull	x1, w14, w1
	add	x5, x2, x15
	umull	x6, w1, w17
	umaddl	x6, w6, w16, x1
	lsr	x6, x6, 32
	umulh	x1, x6, x10
	lsr	x1, x1, 20
	lsl	x20, x1, 10
	sub	x20, x20, x1
	add	x1, x1, x20, lsl 13
	sub	x1, x6, x1
	add	x2, x1, x2
	sub	x1, x5, x1
	umulh	x21, x2, x10
	umulh	x20, x1, x10
	lsr	x21, x21, 20
	lsr	x20, x20, 20
	lsl	x6, x21, 10
	sub	x6, x6, x21
	lsl	x5, x20, 10
	add	x6, x21, x6, lsl 13
	sub	x5, x5, x20
	add	x5, x20, x5, lsl 13
	sub	x2, x2, x6
	sub	x1, x1, x5
	str	w2, [x0, x9, lsl 2]
	str	w1, [x0, x12, lsl 2]
	cmp	w13, 1
	beq	L47
	mov	w13, 1
	b	L19
L45:
	mov	x9, 33889
	add	w10, w8, 2
	add	x4, x4, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	x9, 0x6014, lsl 16
	mov	w13, 57343
	mov	w12, 57345
	add	x10, x4, x10, lsl 2
	movk	x9, 0x1c0, lsl 32
	mov	x11, 57345
	mov	x7, 0
	sub	w8, w8, #126
	movk	w13, 0xfc7f, lsl 16
	movk	w12, 0x7f, lsl 16
	movk	x9, 0x2008, lsl 48
	movk	x11, 0x7f, lsl 16
	.p2align 5,,15
L22:
	add	w0, w8, w7
	cmp	w0, 127
	bhi	L48
	ldp	w0, w2, [x3]
	ldr	w1, [x10, x7, lsl 2]
	add	x7, x7, 1
	umull	x1, w1, w2
	add	x2, x0, x11
	umull	x4, w1, w13
	umaddl	x4, w4, w12, x1
	lsr	x4, x4, 32
	umulh	x1, x4, x9
	lsr	x1, x1, 20
	lsl	x5, x1, 10
	sub	x5, x5, x1
	add	x1, x1, x5, lsl 13
	sub	x1, x4, x1
	add	x0, x1, x0
	sub	x1, x2, x1
	umulh	x6, x0, x9
	umulh	x5, x1, x9
	lsr	x6, x6, 20
	lsl	x4, x6, 10
	lsr	x5, x5, 20
	sub	x4, x4, x6
	lsl	x2, x5, 10
	add	x4, x6, x4, lsl 13
	sub	x2, x2, x5
	add	x2, x5, x2, lsl 13
	sub	x0, x0, x4
	sub	x1, x1, x2
	fmov	d31, x0
	ins	v31.d[1], x1
	xtn	v31.2s, v31.2d
	str	d31, [x3], 8
	cmp	x7, 128
	bne	L22
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI3:
	ret
L48:
LCFI4:
	adrp	x0, lC9@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L46:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L43:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L41:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L39:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE5:
	.const
	.align	2
lC1:
	.word	1
	.word	49
	.align	2
lC0:
	.word	1
	.word	48
	.text
	.const
	.align	3
lC10:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:178"
	.align	3
lC11:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:195"
	.align	3
lC12:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:212"
	.align	3
lC13:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:229"
	.align	3
lC14:
	.ascii "Loop_Invariant failed at anubis_mldsa_ntt.adb:246"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__intt
_anubis_mldsa_ntt__intt:
LFB6:
	mov	x9, 33889
	adrp	x3, _anubis_mldsa_ntt__zetas@PAGE
	stp	x29, x30, [sp, -48]!
LCFI5:
	movk	x9, 0x6014, lsl 16
	add	x4, x3, _anubis_mldsa_ntt__zetas@PAGEOFF;
	adrp	x8, _anubis_mldsa_ntt__zetas@PAGE+1020
	mov	w12, 57345
	mov	x10, 57345
	movk	x9, 0x1c0, lsl 32
	mov	w11, 57343
	mov	x2, x0
	mov	x7, x0
	add	x13, x4, 508
	add	x8, x8, _anubis_mldsa_ntt__zetas@PAGEOFF+1020;
	movk	w12, 0x7f, lsl 16
	movk	x10, 0x7f, lsl 16
	movk	x9, 0x2008, lsl 48
	movk	w11, 0xfc7f, lsl 16
	mov	x29, sp
LCFI6:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI7:
	.p2align 5,,15
L51:
	ldp	w1, w14, [x7]
	ldr	w5, [x8], -4
	add	x6, x1, x10
	add	x1, x1, x14
	sub	x6, x6, x14
	cmp	w5, 0
	sub	w5, w12, w5
	csel	x5, x5, xzr, ne
	umulh	x17, x6, x9
	umulh	x16, x1, x9
	lsr	x17, x17, 20
	lsl	x15, x17, 10
	lsr	x16, x16, 20
	sub	x15, x15, x17
	lsl	x14, x16, 10
	add	x15, x17, x15, lsl 13
	sub	x14, x14, x16
	add	x14, x16, x14, lsl 13
	sub	x6, x6, x15
	sub	x1, x1, x14
	umull	x6, w6, w5
	fmov	d31, x1
	umull	x5, w6, w11
	umull	x5, w5, w10
	add	x5, x5, x6
	lsr	x5, x5, 32
	umulh	x14, x5, x9
	lsr	x14, x14, 20
	lsl	x6, x14, 10
	sub	x6, x6, x14
	add	x6, x14, x6, lsl 13
	sub	x5, x5, x6
	ins	v31.d[1], x5
	xtn	v31.2s, v31.2d
	str	d31, [x7], 8
	cmp	x8, x13
	bne	L51
	mov	x8, 33889
	mov	w15, 57345
	movk	x8, 0x6014, lsl 16
	mov	x11, 57345
	movk	x8, 0x1c0, lsl 32
	mov	w14, 57343
	mov	x12, 0
	mov	x7, 127
	add	x16, x3, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	w15, 0x7f, lsl 16
	movk	x11, 0x7f, lsl 16
	movk	x8, 0x2008, lsl 48
	movk	w14, 0xfc7f, lsl 16
	b	L54
	.p2align 2,,3
L103:
	sub	x7, x7, #1
	add	x12, x12, 4
	cmp	x7, 63
	beq	L102
L54:
	ldr	w6, [x16, x7, lsl 2]
	mov	w13, 2
	mov	w10, -1
	cmp	w6, 0
	sub	w6, w15, w6
	csel	x6, x6, xzr, ne
L53:
	add	w17, w10, 1
	add	x17, x12, w17, sxtw
	ldr	w1, [x0, x17, lsl 2]
	add	x9, x17, 2
	mov	w10, 0
	ldr	w19, [x0, x9, lsl 2]
	add	x5, x1, x11
	sub	x5, x5, x19
	add	x1, x1, x19
	umulh	x21, x5, x8
	umulh	x20, x1, x8
	lsr	x21, x21, 20
	lsl	x19, x21, 10
	lsr	x20, x20, 20
	sub	x19, x19, x21
	lsl	x30, x20, 10
	add	x19, x21, x19, lsl 13
	sub	x30, x30, x20
	add	x30, x20, x30, lsl 13
	sub	x5, x5, x19
	sub	x1, x1, x30
	umull	x5, w5, w6
	str	w1, [x0, x17, lsl 2]
	umull	x1, w5, w14
	umull	x1, w1, w11
	add	x1, x1, x5
	lsr	x1, x1, 32
	umulh	x17, x1, x8
	lsr	x17, x17, 20
	lsl	x5, x17, 10
	sub	x5, x5, x17
	add	x5, x17, x5, lsl 13
	sub	x1, x1, x5
	str	w1, [x0, x9, lsl 2]
	cmp	w13, 1
	beq	L103
	mov	w13, 1
	b	L53
L102:
	mov	x8, 33889
	mov	w14, 57345
	movk	x8, 0x6014, lsl 16
	mov	x10, 57345
	movk	x8, 0x1c0, lsl 32
	mov	w12, 57343
	sub	x11, x0, #16
	mov	x13, 0
	add	x15, x3, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	w14, 0x7f, lsl 16
	movk	x10, 0x7f, lsl 16
	movk	x8, 0x2008, lsl 48
	movk	w12, 0xfc7f, lsl 16
	.p2align 5,,15
L58:
	sub	w1, w7, #32
	cmp	w1, 31
	bhi	L104
	ldr	w9, [x15, x7, lsl 2]
	sub	x6, x11, #16
	cmp	w9, 0
	sub	w9, w14, w9
	csel	x9, x9, xzr, ne
L57:
	ldr	w1, [x6, 32]
	add	x6, x6, 4
	ldr	w16, [x6, 44]
	add	x5, x1, x10
	sub	x5, x5, x16
	add	x1, x1, x16
	umulh	x20, x5, x8
	umulh	x19, x1, x8
	lsr	x20, x20, 20
	lsl	x17, x20, 10
	lsr	x19, x19, 20
	sub	x17, x17, x20
	lsl	x16, x19, 10
	add	x17, x20, x17, lsl 13
	sub	x16, x16, x19
	add	x16, x19, x16, lsl 13
	sub	x5, x5, x17
	sub	x1, x1, x16
	umull	x5, w5, w9
	str	w1, [x6, 28]
	umull	x1, w5, w12
	umull	x1, w1, w10
	add	x1, x1, x5
	lsr	x1, x1, 32
	umulh	x16, x1, x8
	lsr	x16, x16, 20
	lsl	x5, x16, 10
	sub	x5, x5, x16
	add	x5, x16, x5, lsl 13
	sub	x1, x1, x5
	str	w1, [x6, 44]
	cmp	x6, x11
	bne	L57
	add	x13, x13, 8
	add	x11, x6, 32
	sub	x1, x7, #1
	cmp	x13, 256
	beq	L105
	mov	x7, x1
	b	L58
L105:
	mov	x8, 33889
	sub	w7, w7, #1
	movk	x8, 0x6014, lsl 16
	mov	w14, 57345
	mov	x9, 57345
	movk	x8, 0x1c0, lsl 32
	mov	w12, 57343
	sub	x11, x0, #32
	mov	x13, 0
	sxtw	x7, w7
	add	x15, x3, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	w14, 0x7f, lsl 16
	movk	x9, 0x7f, lsl 16
	movk	x8, 0x2008, lsl 48
	movk	w12, 0xfc7f, lsl 16
	.p2align 5,,15
L62:
	sub	w1, w7, #16
	cmp	w1, 15
	bhi	L106
	ldr	w10, [x15, x7, lsl 2]
	sub	x6, x11, #32
	cmp	w10, 0
	sub	w10, w14, w10
	csel	x10, x10, xzr, ne
	.p2align 5,,15
L61:
	ldr	w1, [x6, 64]
	add	x6, x6, 4
	ldr	w16, [x6, 92]
	add	x5, x1, x9
	sub	x5, x5, x16
	add	x1, x1, x16
	umulh	x20, x5, x8
	umulh	x19, x1, x8
	lsr	x20, x20, 20
	lsl	x17, x20, 10
	lsr	x19, x19, 20
	sub	x17, x17, x20
	lsl	x16, x19, 10
	add	x17, x20, x17, lsl 13
	sub	x16, x16, x19
	add	x16, x19, x16, lsl 13
	sub	x5, x5, x17
	sub	x1, x1, x16
	umull	x5, w5, w10
	str	w1, [x6, 60]
	umull	x1, w5, w12
	umull	x1, w1, w9
	add	x1, x1, x5
	lsr	x1, x1, 32
	umulh	x16, x1, x8
	lsr	x16, x16, 20
	lsl	x5, x16, 10
	sub	x5, x5, x16
	add	x5, x16, x5, lsl 13
	sub	x1, x1, x5
	str	w1, [x6, 92]
	cmp	x11, x6
	bne	L61
	add	x13, x13, 16
	add	x11, x11, 64
	sub	x1, x7, #1
	cmp	x13, 256
	beq	L107
	mov	x7, x1
	b	L62
L107:
	sub	w7, w7, #1
	mov	w13, 57345
	sxtw	x11, w7
	mov	x7, 33889
	movk	x7, 0x6014, lsl 16
	mov	x8, 57345
	movk	x7, 0x1c0, lsl 32
	mov	w12, 57343
	sub	x10, x0, #64
	add	x15, x0, 960
	add	x14, x3, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	w13, 0x7f, lsl 16
	movk	x8, 0x7f, lsl 16
	movk	x7, 0x2008, lsl 48
	movk	w12, 0xfc7f, lsl 16
	.p2align 5,,15
L66:
	sub	w1, w11, #8
	cmp	w1, 7
	bhi	L108
	ldr	w9, [x14, x11, lsl 2]
	sub	x6, x10, #64
	cmp	w9, 0
	sub	w9, w13, w9
	csel	x9, x9, xzr, ne
	.p2align 5,,15
L65:
	ldr	w1, [x6, 128]
	add	x6, x6, 4
	ldr	w16, [x6, 188]
	add	x5, x1, x8
	sub	x5, x5, x16
	add	x1, x1, x16
	umulh	x20, x5, x7
	umulh	x19, x1, x7
	lsr	x20, x20, 20
	lsl	x17, x20, 10
	lsr	x19, x19, 20
	sub	x17, x17, x20
	lsl	x16, x19, 10
	add	x17, x20, x17, lsl 13
	sub	x16, x16, x19
	add	x16, x19, x16, lsl 13
	sub	x5, x5, x17
	sub	x1, x1, x16
	umull	x5, w5, w9
	str	w1, [x6, 124]
	umull	x1, w5, w12
	umull	x1, w1, w8
	add	x1, x1, x5
	lsr	x1, x1, 32
	umulh	x16, x1, x7
	lsr	x16, x16, 20
	lsl	x5, x16, 10
	sub	x5, x5, x16
	add	x5, x16, x5, lsl 13
	sub	x1, x1, x5
	str	w1, [x6, 188]
	cmp	x10, x6
	bne	L65
	add	x10, x10, 128
	sub	x1, x11, #1
	cmp	x15, x10
	beq	L109
	mov	x11, x1
	b	L66
L109:
	mov	x7, 33889
	sub	w12, w11, #1
	movk	x7, 0x6014, lsl 16
	mov	w15, 57345
	mov	x8, 57345
	movk	x7, 0x1c0, lsl 32
	mov	w11, 57343
	add	x10, x0, 128
	mov	x14, 0
	sxtw	x12, w12
	add	x16, x3, _anubis_mldsa_ntt__zetas@PAGEOFF;
	movk	w15, 0x7f, lsl 16
	movk	x8, 0x7f, lsl 16
	movk	x7, 0x2008, lsl 48
	movk	w11, 0xfc7f, lsl 16
L70:
	sub	w1, w12, #4
	mov	w13, w12
	cmp	w1, 3
	bhi	L110
	ldr	w1, [x16, x12, lsl 2]
	sub	x6, x10, #128
	cmp	w1, 0
	sub	w1, w15, w1
	csel	x9, x1, xzr, ne
	.p2align 5,,15
L69:
	ldr	w1, [x6]
	add	x6, x6, 4
	ldr	w17, [x6, 124]
	add	x5, x1, x8
	sub	x5, x5, x17
	add	x1, x1, x17
	umulh	x21, x5, x7
	umulh	x20, x1, x7
	lsr	x21, x21, 20
	lsl	x19, x21, 10
	lsr	x20, x20, 20
	sub	x19, x19, x21
	lsl	x17, x20, 10
	add	x19, x21, x19, lsl 13
	sub	x17, x17, x20
	add	x17, x20, x17, lsl 13
	sub	x5, x5, x19
	sub	x1, x1, x17
	umull	x5, w5, w9
	str	w1, [x6, -4]
	umull	x1, w5, w11
	umull	x1, w1, w8
	add	x1, x1, x5
	lsr	x1, x1, 32
	umulh	x17, x1, x7
	lsr	x17, x17, 20
	lsl	x5, x17, 10
	sub	x5, x5, x17
	add	x5, x17, x5, lsl 13
	sub	x1, x1, x5
	str	w1, [x6, 124]
	cmp	x10, x6
	bne	L69
	add	x14, x14, 64
	add	x10, x10, 256
	sub	x1, x12, #1
	cmp	x14, 256
	beq	L111
	mov	x12, x1
	b	L70
L111:
	mov	x7, 33889
	mov	w16, 57345
	add	x4, x4, w12, uxtw 2
	movk	x7, 0x6014, lsl 16
	mov	x8, 57345
	movk	x7, 0x1c0, lsl 32
	mov	w11, 57343
	sub	w13, w13, #2
	mov	w1, -1
	movk	w16, 0x7f, lsl 16
	add	x15, x0, 768
	movk	x8, 0x7f, lsl 16
	movk	x7, 0x2008, lsl 48
	movk	w11, 0xfc7f, lsl 16
L74:
	sub	w5, w12, #3
	add	w6, w1, 1
	cmp	w5, 1
	bhi	L112
	ldr	w1, [x4, -4]
	ubfiz	x6, x6, 9, 32
	sub	w14, w12, #1
	sub	x10, x6, #512
	add	x6, x0, x6
	add	x10, x15, x10
	cmp	w1, 0
	sub	w1, w16, w1
	csel	x9, x1, xzr, ne
	.p2align 5,,15
L73:
	ldr	w1, [x6]
	add	x6, x6, 4
	ldr	w17, [x6, 252]
	add	x5, x1, x8
	sub	x5, x5, x17
	add	x1, x1, x17
	umulh	x21, x5, x7
	umulh	x20, x1, x7
	lsr	x21, x21, 20
	lsl	x19, x21, 10
	lsr	x20, x20, 20
	sub	x19, x19, x21
	lsl	x17, x20, 10
	add	x19, x21, x19, lsl 13
	sub	x17, x17, x20
	add	x17, x20, x17, lsl 13
	sub	x5, x5, x19
	sub	x1, x1, x17
	umull	x5, w5, w9
	str	w1, [x6, -4]
	umull	x1, w5, w11
	umull	x1, w1, w8
	add	x1, x1, x5
	lsr	x1, x1, 32
	umulh	x17, x1, x7
	lsr	x17, x17, 20
	lsl	x5, x17, 10
	sub	x5, x5, x17
	add	x5, x17, x5, lsl 13
	sub	x1, x1, x5
	str	w1, [x6, 252]
	cmp	x10, x6
	bne	L73
	sub	x4, x4, #4
	mov	w1, 0
	cmp	w14, w13
	bne	L90
	add	x3, x3, _anubis_mldsa_ntt__zetas@PAGEOFF;
	sub	w12, w12, #2
	ldr	w1, [x3, w12, sxtw 2]
	mov	x7, 33889
	mov	x9, 57345
	movk	x7, 0x6014, lsl 16
	mov	w10, 57343
	movk	x7, 0x1c0, lsl 32
	add	x11, x0, 512
	mov	x4, x0
	movk	x9, 0x7f, lsl 16
	movk	x7, 0x2008, lsl 48
	movk	w10, 0xfc7f, lsl 16
	sub	w8, w8, w1
	cmp	w1, 0
	csel	x8, x8, xzr, ne
	.p2align 5,,15
L76:
	ldr	w1, [x4]
	add	x4, x4, 4
	ldr	w5, [x4, 508]
	add	x3, x1, x9
	sub	x3, x3, x5
	add	x1, x1, x5
	umulh	x13, x3, x7
	umulh	x12, x1, x7
	lsr	x13, x13, 20
	lsl	x6, x13, 10
	lsr	x12, x12, 20
	sub	x6, x6, x13
	lsl	x5, x12, 10
	add	x6, x13, x6, lsl 13
	sub	x5, x5, x12
	add	x5, x12, x5, lsl 13
	sub	x3, x3, x6
	sub	x1, x1, x5
	umull	x3, w3, w8
	str	w1, [x4, -4]
	umull	x1, w3, w10
	umull	x1, w1, w9
	add	x1, x1, x3
	lsr	x1, x1, 32
	umulh	x5, x1, x7
	lsr	x5, x5, 20
	lsl	x3, x5, 10
	sub	x3, x3, x5
	add	x3, x5, x3, lsl 13
	sub	x1, x1, x3
	str	w1, [x4, 508]
	cmp	x11, x4
	bne	L76
	mov	x5, 33889
	mov	w7, 57343
	movk	x5, 0x6014, lsl 16
	mov	w6, 57345
	movk	x5, 0x1c0, lsl 32
	add	x0, x0, 1024
	mov	w8, 16382
	movk	w7, 0xfc7f, lsl 16
	movk	w6, 0x7f, lsl 16
	movk	x5, 0x2008, lsl 48
	.p2align 5,,15
L77:
	ldr	w3, [x2]
	umull	x3, w3, w8
	umull	x1, w3, w7
	umull	x1, w1, w6
	add	x1, x1, x3
	lsr	x1, x1, 32
	umulh	x4, x1, x5
	lsr	x4, x4, 20
	lsl	x3, x4, 10
	sub	x3, x3, x4
	add	x3, x4, x3, lsl 13
	sub	x1, x1, x3
	str	w1, [x2], 4
	cmp	x0, x2
	bne	L77
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI8:
	ret
L90:
LCFI9:
	mov	w12, w14
	b	L74
L104:
	adrp	x0, lC10@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L112:
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L110:
	adrp	x0, lC13@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L108:
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L106:
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__ntt_mul
_anubis_mldsa_ntt__ntt_mul:
LFB7:
	mov	x10, 33889
	mov	x4, 0
	movk	x10, 0x6014, lsl 16
	movk	x10, 0x1c0, lsl 32
	movk	x10, 0x2008, lsl 48
	.p2align 5,,15
L114:
	ldr	w3, [x0, x4]
	ldr	w9, [x1, x4]
	umulh	x8, x3, x10
	umulh	x7, x9, x10
	lsr	x8, x8, 20
	lsl	x6, x8, 10
	lsr	x7, x7, 20
	sub	x6, x6, x8
	lsl	x5, x7, 10
	add	x6, x8, x6, lsl 13
	sub	x5, x5, x7
	add	x5, x7, x5, lsl 13
	sub	x3, x3, x6
	sub	x9, x9, x5
	umull	x3, w3, w9
	umulh	x6, x3, x10
	lsr	x6, x6, 20
	lsl	x5, x6, 10
	sub	x5, x5, x6
	add	x5, x6, x5, lsl 13
	sub	x3, x3, x5
	str	w3, [x2, x4]
	add	x4, x4, 4
	cmp	x4, 1024
	bne	L114
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__vec_ntt_l
_anubis_mldsa_ntt__vec_ntt_l:
LFB8:
	stp	x29, x30, [sp, -32]!
LCFI10:
	mov	x29, sp
LCFI11:
	stp	x19, x20, [sp, 16]
LCFI12:
	mov	x19, x0
	mov	x0, 7168
	add	x20, x19, x0
	.p2align 5,,15
L117:
	mov	x0, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_ntt__ntt
	cmp	x19, x20
	bne	L117
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI13:
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__vec_intt_l
_anubis_mldsa_ntt__vec_intt_l:
LFB9:
	stp	x29, x30, [sp, -32]!
LCFI14:
	mov	x29, sp
LCFI15:
	stp	x19, x20, [sp, 16]
LCFI16:
	mov	x19, x0
	mov	x0, 7168
	add	x20, x19, x0
	.p2align 5,,15
L121:
	mov	x0, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_ntt__intt
	cmp	x19, x20
	bne	L121
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI17:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__vec_ntt_k
_anubis_mldsa_ntt__vec_ntt_k:
LFB10:
	stp	x29, x30, [sp, -32]!
LCFI18:
	mov	x29, sp
LCFI19:
	stp	x19, x20, [sp, 16]
LCFI20:
	mov	x19, x0
	add	x20, x0, 8192
	.p2align 5,,15
L125:
	mov	x0, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_ntt__ntt
	cmp	x19, x20
	bne	L125
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI21:
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__vec_intt_k
_anubis_mldsa_ntt__vec_intt_k:
LFB11:
	stp	x29, x30, [sp, -32]!
LCFI22:
	mov	x29, sp
LCFI23:
	stp	x19, x20, [sp, 16]
LCFI24:
	mov	x19, x0
	add	x20, x0, 8192
	.p2align 5,,15
L129:
	mov	x0, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_ntt__intt
	cmp	x19, x20
	bne	L129
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI25:
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__matrix_ntt
_anubis_mldsa_ntt__matrix_ntt:
LFB12:
	stp	x29, x30, [sp, -64]!
LCFI26:
	mov	x29, sp
LCFI27:
	mov	x1, 64512
	stp	x21, x22, [sp, 32]
LCFI28:
	mov	x22, 7168
	add	x21, x0, x1
	stp	x19, x20, [sp, 16]
LCFI29:
	add	x20, x0, x22
	str	x23, [sp, 48]
LCFI30:
	mov	x23, -7168
	.p2align 5,,15
L134:
	add	x19, x20, x23
	.p2align 5,,15
L133:
	mov	x0, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_ntt__ntt
	cmp	x19, x20
	bne	L133
	add	x20, x19, x22
	cmp	x20, x21
	bne	L134
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI31:
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__matrix_vec_mul
_anubis_mldsa_ntt__matrix_vec_mul:
LFB13:
	sub	sp, sp, #1104
LCFI32:
	stp	x29, x30, [sp]
LCFI33:
	mov	x29, sp
LCFI34:
	stp	x19, x20, [sp, 16]
LCFI35:
	mov	x20, 33889
	add	x19, x1, 4
	movk	x20, 0x6014, lsl 16
	movk	x20, 0x1c0, lsl 32
	stp	x23, x24, [sp, 48]
LCFI36:
	mov	x24, x0
	mov	x23, x1
	mov	x1, 7172
	movk	x20, 0x2008, lsl 48
	add	x23, x23, x1
	stp	x25, x26, [sp, 64]
LCFI37:
	add	x25, x0, 4
	mov	x0, 57348
	add	x24, x24, x0
	mov	x26, 7168
	stp	x21, x22, [sp, 32]
LCFI38:
	add	x21, x2, 4
	add	x22, x29, 80
L142:
	mov	x2, 1024
	mov	w1, 0
	sub	x0, x21, #4
	bl	_memset
	mov	x4, x19
	mov	x5, x25
L141:
	mov	x1, -1
	.p2align 5,,15
L139:
	ldr	w0, [x5, x1, lsl 2]
	add	x7, x22, x1, lsl 2
	ldr	w9, [x4, x1, lsl 2]
	add	x1, x1, 1
	umulh	x8, x0, x20
	umulh	x6, x9, x20
	lsr	x8, x8, 20
	lsl	x3, x8, 10
	lsr	x6, x6, 20
	sub	x3, x3, x8
	lsl	x2, x6, 10
	add	x3, x8, x3, lsl 13
	sub	x2, x2, x6
	add	x2, x6, x2, lsl 13
	sub	x0, x0, x3
	sub	x9, x9, x2
	umull	x0, w0, w9
	umulh	x3, x0, x20
	lsr	x3, x3, 20
	lsl	x2, x3, 10
	sub	x2, x2, x3
	add	x2, x3, x2, lsl 13
	sub	x0, x0, x2
	str	w0, [x7, 4]
	cmp	x1, 255
	bne	L139
	mov	x1, -1
	.p2align 5,,15
L140:
	add	x2, x22, x1, lsl 2
	ldr	w0, [x21, x1, lsl 2]
	ldr	w2, [x2, 4]
	add	x0, x0, x2
	umulh	x3, x0, x20
	lsr	x3, x3, 20
	lsl	x2, x3, 10
	sub	x2, x2, x3
	add	x2, x3, x2, lsl 13
	sub	x0, x0, x2
	str	w0, [x21, x1, lsl 2]
	add	x1, x1, 1
	cmp	x1, 255
	bne	L140
	add	x4, x4, 1024
	add	x5, x5, 1024
	cmp	x4, x23
	bne	L141
	add	x25, x25, x26
	add	x21, x21, 1024
	cmp	x25, x24
	bne	L142
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, 1104
LCFI39:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__vec_dot_product_l
_anubis_mldsa_ntt__vec_dot_product_l:
LFB14:
	sub	sp, sp, #1056
LCFI40:
	stp	x29, x30, [sp]
LCFI41:
	mov	x29, sp
LCFI42:
	stp	x19, x20, [sp, 16]
LCFI43:
	mov	x20, x0
	mov	x19, x1
	mov	x0, x2
	mov	w1, 0
	mov	x2, 1024
	bl	_memset
	mov	x4, 33889
	add	x8, x29, 32
	movk	x4, 0x6014, lsl 16
	add	x5, x0, 4
	movk	x4, 0x1c0, lsl 32
	mov	x7, x8
	mov	x6, 1024
	movk	x4, 0x2008, lsl 48
L151:
	sub	x1, x6, #1024
	.p2align 5,,15
L149:
	ldr	w0, [x20, x1]
	ldr	w11, [x19, x1]
	umulh	x10, x0, x4
	umulh	x9, x11, x4
	lsr	x10, x10, 20
	lsl	x3, x10, 10
	lsr	x9, x9, 20
	sub	x3, x3, x10
	lsl	x2, x9, 10
	add	x3, x10, x3, lsl 13
	sub	x2, x2, x9
	add	x2, x9, x2, lsl 13
	sub	x0, x0, x3
	sub	x11, x11, x2
	umull	x0, w0, w11
	umulh	x3, x0, x4
	lsr	x3, x3, 20
	lsl	x2, x3, 10
	sub	x2, x2, x3
	add	x2, x3, x2, lsl 13
	sub	x0, x0, x2
	str	w0, [x7, x1]
	add	x1, x1, 4
	cmp	x1, x6
	bne	L149
	mov	x1, -1
	.p2align 5,,15
L150:
	add	x2, x8, x1, lsl 2
	ldr	w0, [x5, x1, lsl 2]
	ldr	w2, [x2, 4]
	add	x0, x0, x2
	umulh	x3, x0, x4
	lsr	x3, x3, 20
	lsl	x2, x3, 10
	sub	x2, x2, x3
	add	x2, x3, x2, lsl 13
	sub	x0, x0, x2
	str	w0, [x5, x1, lsl 2]
	add	x1, x1, 1
	cmp	x1, 255
	bne	L150
	add	x6, x6, 1024
	sub	x7, x7, #1024
	cmp	x6, 8192
	bne	L151
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 1056
LCFI44:
	ret
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_ntt__vec_dot_product_k
_anubis_mldsa_ntt__vec_dot_product_k:
LFB15:
	sub	sp, sp, #1056
LCFI45:
	stp	x29, x30, [sp]
LCFI46:
	mov	x29, sp
LCFI47:
	stp	x19, x20, [sp, 16]
LCFI48:
	mov	x20, x0
	mov	x19, x1
	mov	x0, x2
	mov	w1, 0
	mov	x2, 1024
	bl	_memset
	mov	x4, 33889
	add	x8, x29, 32
	movk	x4, 0x6014, lsl 16
	add	x5, x0, 4
	movk	x4, 0x1c0, lsl 32
	mov	x7, x8
	mov	x6, 1024
	movk	x4, 0x2008, lsl 48
	mov	x9, 9216
L159:
	sub	x1, x6, #1024
	.p2align 5,,15
L157:
	ldr	w0, [x20, x1]
	ldr	w12, [x19, x1]
	umulh	x11, x0, x4
	umulh	x10, x12, x4
	lsr	x11, x11, 20
	lsl	x3, x11, 10
	lsr	x10, x10, 20
	sub	x3, x3, x11
	lsl	x2, x10, 10
	add	x3, x11, x3, lsl 13
	sub	x2, x2, x10
	add	x2, x10, x2, lsl 13
	sub	x0, x0, x3
	sub	x12, x12, x2
	umull	x0, w0, w12
	umulh	x3, x0, x4
	lsr	x3, x3, 20
	lsl	x2, x3, 10
	sub	x2, x2, x3
	add	x2, x3, x2, lsl 13
	sub	x0, x0, x2
	str	w0, [x7, x1]
	add	x1, x1, 4
	cmp	x1, x6
	bne	L157
	mov	x1, -1
	.p2align 5,,15
L158:
	add	x2, x8, x1, lsl 2
	ldr	w0, [x5, x1, lsl 2]
	ldr	w2, [x2, 4]
	add	x0, x0, x2
	umulh	x3, x0, x4
	lsr	x3, x3, 20
	lsl	x2, x3, 10
	sub	x2, x2, x3
	add	x2, x3, x2, lsl 13
	sub	x0, x0, x2
	str	w0, [x5, x1, lsl 2]
	add	x1, x1, 1
	cmp	x1, 255
	bne	L158
	add	x6, x6, 1024
	sub	x7, x7, #1024
	cmp	x6, x9
	bne	L159
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 1056
LCFI49:
	ret
LFE15:
	.globl _anubis_mldsa_ntt__zetas
	.const
	.align	2
_anubis_mldsa_ntt__zetas:
	.word	0
	.word	25847
	.word	5771523
	.word	7861508
	.word	237124
	.word	7602457
	.word	7504169
	.word	466468
	.word	1826347
	.word	2353451
	.word	8021166
	.word	6288512
	.word	3119733
	.word	5495562
	.word	3111497
	.word	2680103
	.word	2725464
	.word	1024112
	.word	7300517
	.word	3585928
	.word	7830929
	.word	7260833
	.word	2619752
	.word	6271868
	.word	6262231
	.word	4520680
	.word	6980856
	.word	5102745
	.word	1757237
	.word	8360995
	.word	4010497
	.word	280005
	.word	2706023
	.word	95776
	.word	3077325
	.word	3530437
	.word	6718724
	.word	4788269
	.word	5842901
	.word	3915439
	.word	4519302
	.word	5336701
	.word	3574422
	.word	5512770
	.word	3539968
	.word	8079950
	.word	2348700
	.word	7841118
	.word	6681150
	.word	6736599
	.word	3505694
	.word	4558682
	.word	3507263
	.word	6239768
	.word	6779997
	.word	3699596
	.word	811944
	.word	531354
	.word	954230
	.word	3881043
	.word	3900724
	.word	5823537
	.word	2071892
	.word	5582638
	.word	4450022
	.word	6851714
	.word	4702672
	.word	5339162
	.word	6927966
	.word	3475950
	.word	2176455
	.word	6795196
	.word	7122806
	.word	1939314
	.word	4296819
	.word	7380215
	.word	5190273
	.word	5223087
	.word	4747489
	.word	126922
	.word	3412210
	.word	7396998
	.word	2147896
	.word	2715295
	.word	5412772
	.word	4686924
	.word	7969390
	.word	5903370
	.word	7709315
	.word	7151892
	.word	8357436
	.word	7072248
	.word	7998430
	.word	1349076
	.word	1852771
	.word	6949987
	.word	5037034
	.word	264944
	.word	508951
	.word	3097992
	.word	44288
	.word	7280319
	.word	904516
	.word	3958618
	.word	4656075
	.word	8371839
	.word	1653064
	.word	5130689
	.word	2389356
	.word	8169440
	.word	759969
	.word	7063561
	.word	189548
	.word	4827145
	.word	3159746
	.word	6529015
	.word	5971092
	.word	8202977
	.word	1315589
	.word	1341330
	.word	1285669
	.word	6795489
	.word	7567685
	.word	6940675
	.word	5361315
	.word	4499357
	.word	4751448
	.word	3839961
	.word	2091667
	.word	3407706
	.word	2316500
	.word	3817976
	.word	5037939
	.word	2244091
	.word	5933984
	.word	4817955
	.word	266997
	.word	2434439
	.word	7144689
	.word	3513181
	.word	4860065
	.word	4621053
	.word	7183191
	.word	5187039
	.word	900702
	.word	1859098
	.word	909542
	.word	819034
	.word	495491
	.word	6767243
	.word	8337157
	.word	7857917
	.word	7725090
	.word	5257975
	.word	2031748
	.word	3207046
	.word	4823422
	.word	7855319
	.word	7611795
	.word	4784579
	.word	342297
	.word	286988
	.word	5942594
	.word	4108315
	.word	3437287
	.word	5038140
	.word	1735879
	.word	203044
	.word	2842341
	.word	2691481
	.word	5790267
	.word	1265009
	.word	4055324
	.word	1247620
	.word	2486353
	.word	1595974
	.word	4613401
	.word	1250494
	.word	2635921
	.word	4832145
	.word	5386378
	.word	1869119
	.word	1903435
	.word	7329447
	.word	7047359
	.word	1237275
	.word	5062207
	.word	6950192
	.word	7929317
	.word	1312455
	.word	3306115
	.word	6417775
	.word	7100756
	.word	1917081
	.word	5834105
	.word	7005614
	.word	1500165
	.word	777191
	.word	2235880
	.word	3406031
	.word	7838005
	.word	5548557
	.word	6709241
	.word	6533464
	.word	5796124
	.word	4656147
	.word	594136
	.word	4603424
	.word	6366809
	.word	2432395
	.word	2454455
	.word	8215696
	.word	1957272
	.word	3369112
	.word	185531
	.word	7173032
	.word	5196991
	.word	162844
	.word	1616392
	.word	3014001
	.word	810149
	.word	1652634
	.word	4686184
	.word	6581310
	.word	5341501
	.word	3523897
	.word	3866901
	.word	269760
	.word	2213111
	.word	7404533
	.word	1717735
	.word	472078
	.word	7953734
	.word	1723600
	.word	6577327
	.word	1910376
	.word	6712985
	.word	7276084
	.word	8119771
	.word	4546524
	.word	5441381
	.word	6144432
	.word	7959518
	.word	6094090
	.word	183443
	.word	7403526
	.word	1612842
	.word	4834730
	.word	7826001
	.word	3919660
	.word	8332111
	.word	7018208
	.word	3937738
	.word	1400424
	.word	7534263
	.word	1976782
	.globl _anubis_mldsa_ntt__f_const
	.align	2
_anubis_mldsa_ntt__f_const:
	.word	16382
	.globl _anubis_mldsa_ntt_E
	.data
	.align	1
_anubis_mldsa_ntt_E:
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
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$3,LEFDE3-LASFDE3
	.long L$set$3
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB5-.
	.set L$set$4,LFE5-LFB5
	.quad L$set$4
	.uleb128 0
	.byte	0x4
	.set L$set$5,LCFI0-LFB5
	.long L$set$5
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$6,LCFI1-LCFI0
	.long L$set$6
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$7,LCFI2-LCFI1
	.long L$set$7
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$8,LCFI3-LCFI2
	.long L$set$8
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
	.set L$set$9,LCFI4-LCFI3
	.long L$set$9
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$10,LEFDE5-LASFDE5
	.long L$set$10
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB6-.
	.set L$set$11,LFE6-LFB6
	.quad L$set$11
	.uleb128 0
	.byte	0x4
	.set L$set$12,LCFI5-LFB6
	.long L$set$12
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$13,LCFI6-LCFI5
	.long L$set$13
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$14,LCFI7-LCFI6
	.long L$set$14
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$15,LCFI8-LCFI7
	.long L$set$15
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
	.set L$set$16,LCFI9-LCFI8
	.long L$set$16
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$17,LEFDE7-LASFDE7
	.long L$set$17
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB7-.
	.set L$set$18,LFE7-LFB7
	.quad L$set$18
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$19,LEFDE9-LASFDE9
	.long L$set$19
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB8-.
	.set L$set$20,LFE8-LFB8
	.quad L$set$20
	.uleb128 0
	.byte	0x4
	.set L$set$21,LCFI10-LFB8
	.long L$set$21
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$22,LCFI11-LCFI10
	.long L$set$22
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$23,LCFI12-LCFI11
	.long L$set$23
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$24,LCFI13-LCFI12
	.long L$set$24
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$25,LEFDE11-LASFDE11
	.long L$set$25
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB9-.
	.set L$set$26,LFE9-LFB9
	.quad L$set$26
	.uleb128 0
	.byte	0x4
	.set L$set$27,LCFI14-LFB9
	.long L$set$27
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$28,LCFI15-LCFI14
	.long L$set$28
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$29,LCFI16-LCFI15
	.long L$set$29
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$30,LCFI17-LCFI16
	.long L$set$30
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$31,LEFDE13-LASFDE13
	.long L$set$31
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB10-.
	.set L$set$32,LFE10-LFB10
	.quad L$set$32
	.uleb128 0
	.byte	0x4
	.set L$set$33,LCFI18-LFB10
	.long L$set$33
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$34,LCFI19-LCFI18
	.long L$set$34
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$35,LCFI20-LCFI19
	.long L$set$35
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$36,LCFI21-LCFI20
	.long L$set$36
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$37,LEFDE15-LASFDE15
	.long L$set$37
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB11-.
	.set L$set$38,LFE11-LFB11
	.quad L$set$38
	.uleb128 0
	.byte	0x4
	.set L$set$39,LCFI22-LFB11
	.long L$set$39
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$40,LCFI23-LCFI22
	.long L$set$40
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$41,LCFI24-LCFI23
	.long L$set$41
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$42,LCFI25-LCFI24
	.long L$set$42
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$43,LEFDE17-LASFDE17
	.long L$set$43
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB12-.
	.set L$set$44,LFE12-LFB12
	.quad L$set$44
	.uleb128 0
	.byte	0x4
	.set L$set$45,LCFI26-LFB12
	.long L$set$45
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$46,LCFI27-LCFI26
	.long L$set$46
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$47,LCFI28-LCFI27
	.long L$set$47
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$48,LCFI29-LCFI28
	.long L$set$48
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$49,LCFI30-LCFI29
	.long L$set$49
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$50,LCFI31-LCFI30
	.long L$set$50
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
LEFDE17:
LSFDE19:
	.set L$set$51,LEFDE19-LASFDE19
	.long L$set$51
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB13-.
	.set L$set$52,LFE13-LFB13
	.quad L$set$52
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI32-LFB13
	.long L$set$53
	.byte	0xe
	.uleb128 0x450
	.byte	0x4
	.set L$set$54,LCFI33-LCFI32
	.long L$set$54
	.byte	0x9d
	.uleb128 0x8a
	.byte	0x9e
	.uleb128 0x89
	.byte	0x4
	.set L$set$55,LCFI34-LCFI33
	.long L$set$55
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$56,LCFI35-LCFI34
	.long L$set$56
	.byte	0x93
	.uleb128 0x88
	.byte	0x94
	.uleb128 0x87
	.byte	0x4
	.set L$set$57,LCFI36-LCFI35
	.long L$set$57
	.byte	0x97
	.uleb128 0x84
	.byte	0x98
	.uleb128 0x83
	.byte	0x4
	.set L$set$58,LCFI37-LCFI36
	.long L$set$58
	.byte	0x99
	.uleb128 0x82
	.byte	0x9a
	.uleb128 0x81
	.byte	0x4
	.set L$set$59,LCFI38-LCFI37
	.long L$set$59
	.byte	0x95
	.uleb128 0x86
	.byte	0x96
	.uleb128 0x85
	.byte	0x4
	.set L$set$60,LCFI39-LCFI38
	.long L$set$60
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
LEFDE19:
LSFDE21:
	.set L$set$61,LEFDE21-LASFDE21
	.long L$set$61
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB14-.
	.set L$set$62,LFE14-LFB14
	.quad L$set$62
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI40-LFB14
	.long L$set$63
	.byte	0xe
	.uleb128 0x420
	.byte	0x4
	.set L$set$64,LCFI41-LCFI40
	.long L$set$64
	.byte	0x9d
	.uleb128 0x84
	.byte	0x9e
	.uleb128 0x83
	.byte	0x4
	.set L$set$65,LCFI42-LCFI41
	.long L$set$65
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$66,LCFI43-LCFI42
	.long L$set$66
	.byte	0x93
	.uleb128 0x82
	.byte	0x94
	.uleb128 0x81
	.byte	0x4
	.set L$set$67,LCFI44-LCFI43
	.long L$set$67
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$68,LEFDE23-LASFDE23
	.long L$set$68
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB15-.
	.set L$set$69,LFE15-LFB15
	.quad L$set$69
	.uleb128 0
	.byte	0x4
	.set L$set$70,LCFI45-LFB15
	.long L$set$70
	.byte	0xe
	.uleb128 0x420
	.byte	0x4
	.set L$set$71,LCFI46-LCFI45
	.long L$set$71
	.byte	0x9d
	.uleb128 0x84
	.byte	0x9e
	.uleb128 0x83
	.byte	0x4
	.set L$set$72,LCFI47-LCFI46
	.long L$set$72
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$73,LCFI48-LCFI47
	.long L$set$73
	.byte	0x93
	.uleb128 0x82
	.byte	0x94
	.uleb128 0x81
	.byte	0x4
	.set L$set$74,LCFI49-LCFI48
	.long L$set$74
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE23:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
