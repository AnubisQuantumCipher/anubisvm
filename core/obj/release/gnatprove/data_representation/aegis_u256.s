	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC2:
	.ascii "Loop_Invariant failed at aegis_u256.adb:515"
	.text
	.align	2
	.p2align 5,,15
_aegis_u256__clz___wrapped_statements.9:
LFB47:
	ldr	x0, [x16]
	ldr	x1, [x0, 24]
	cbnz	x1, L14
	ldr	x1, [x0, 16]
	cbnz	x1, L15
	ldr	x1, [x0, 8]
	cbnz	x1, L16
	ldr	x1, [x0]
	mov	w5, 192
	mov	w0, 256
	cbnz	x1, L2
L1:
	ret
	.p2align 2,,3
L14:
	mov	w5, 0
L2:
	dup	v30.2d, x1
	adrp	x0, lC3@PAGE
	adrp	x1, lC4@PAGE
	dup	v20.4s, w5
	ldr	q31, [x0, #lC3@PAGEOFF]
	mov	w0, 0
	mvni	v18.4s, 0x3
	movi	v22.4s, 0x4
	ldr	q26, [x1, #lC4@PAGEOFF]
	adrp	x1, lC5@PAGE
	movi	v19.4s, 0x1
	ldr	q24, [x1, #lC5@PAGEOFF]
	adrp	x1, lC6@PAGE
	sshl	v30.2d, v30.2d, v31.2d
	ldr	q25, [x1, #lC6@PAGEOFF]
	add	v26.4s, v20.4s, v26.4s
	b	L8
	.p2align 2,,3
L6:
	add	w0, w0, 1
	cmp	w0, 16
	beq	L27
L8:
	mov	v23.16b, v25.16b
	mov	v29.16b, v26.16b
	mov	v27.16b, v30.16b
	mov	v21.16b, v24.16b
	add	v25.4s, v25.4s, v22.4s
	add	v26.4s, v26.4s, v22.4s
	shl	v30.2d, v30.2d, 4
	add	v24.4s, v24.4s, v18.4s
	add	v31.4s, v23.4s, v19.4s
	shl	v28.2d, v27.2d, 2
	add	v31.4s, v31.4s, v20.4s
	cmgt	v31.4s, v29.4s, v31.4s
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbnz	x1, L4
	cmlt	v28.2d, v28.2d, #0
	cmlt	v31.2d, v27.2d, #0
	orr	v31.16b, v31.16b, v28.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbz	x1, L6
L4:
	fmov	w0, s29
	add	w5, w5, 1
	fmov	w1, s23
	sub	w2, w1, w0
	fmov	w1, s21
	add	w3, w0, w1
	fmov	x1, d27
	b	L7
	.p2align 2,,3
L13:
	add	w0, w0, 1
	lsl	x1, x1, 1
	cmp	w3, w0
	beq	L1
L7:
	add	w4, w5, w0
	add	w4, w4, w2
	cmp	w0, w4
	bgt	L28
	tbz	x1, #63, L13
	ret
L15:
	mov	w5, 64
	b	L2
L16:
	mov	w5, 128
	b	L2
L27:
	add	v29.4s, v29.4s, v19.4s
	umov	w0, v29.s[3]
	ret
L28:
	adrp	x0, lC2@PAGE
	adrp	x1, lC0@PAGE
	stp	x29, x30, [sp, -16]!
LCFI0:
	add	x0, x0, lC2@PAGEOFF;
	mov	x29, sp
LCFI1:
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE47:
	.const
	.align	2
lC0:
	.word	1
	.word	43
	.text
	.const
	.align	3
lC7:
	.ascii "failed postcondition from aegis_u256.ads:57"
	.text
	.align	2
	.p2align 5,,15
_aegis_u256__is_zero.part.0:
LFB69:
	adrp	x0, lC7@PAGE
	adrp	x1, lC0@PAGE
	stp	x29, x30, [sp, -16]!
LCFI2:
	add	x0, x0, lC7@PAGEOFF;
	mov	x29, sp
LCFI3:
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE69:
	.const
	.align	3
lC8:
	.ascii "aegis_u256.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__add64_with_carry
_aegis_u256__add64_with_carry:
LFB2:
	cmp	w2, 1
	bhi	L45
	adds	x0, x0, x1
	cset	w1, cs
	cbz	w2, L35
	adds	x0, x0, 1
	csinc	w1, w1, wzr, cc
	csel	x0, xzr, x0, cs
L35:
	and	x1, x1, 1
	ret
L45:
	adrp	x0, lC8@PAGE
	stp	x29, x30, [sp, -16]!
LCFI4:
	mov	w1, 18
	mov	x29, sp
LCFI5:
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__sub64_with_borrow
_aegis_u256__sub64_with_borrow:
LFB3:
	cmp	w2, 1
	bhi	L56
	subs	x0, x0, x1
	cset	w1, cc
	cbz	w2, L48
	cbnz	x0, L57
	mov	x0, -1
	mov	w1, 1
L48:
	and	x1, x1, 1
	ret
L56:
	adrp	x0, lC8@PAGE
	stp	x29, x30, [sp, -16]!
LCFI6:
	mov	w1, 35
	mov	x29, sp
LCFI7:
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L57:
LCFI8:
	sub	x0, x0, #1
	b	L48
LFE3:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__mul64
_aegis_u256__mul64:
LFB4:
	lsr	x5, x1, 32
	lsr	x3, x0, 32
	umull	x2, w0, w1
	umull	x1, w3, w1
	umull	x0, w0, w5
	adds	x0, x0, x1
	cset	x4, cs
	lsr	x6, x0, 32
	lsl	x0, x0, 32
	adds	x1, x2, x0
	umaddl	x3, w3, w5, x6
	cset	x0, cs
	add	x0, x0, x4, lsl 32
	add	x0, x0, x3
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__equal
_aegis_u256__equal:
LFB5:
	ldr	x4, [x0]
	mov	w2, 1
	ldr	x3, [x1]
	cmp	x4, x3
	beq	L66
L65:
	mov	w2, 0
L64:
	and	w0, w2, 1
	ret
	.p2align 2,,3
L66:
	ldr	x4, [x0, 8]
	ldr	x3, [x1, 8]
	cmp	x4, x3
	bne	L65
	ldr	x4, [x0, 16]
	ldr	x3, [x1, 16]
	cmp	x4, x3
	bne	L65
	ldr	x3, [x0, 24]
	ldr	x0, [x1, 24]
	cmp	x3, x0
	bne	L65
	b	L64
LFE5:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__less_than
_aegis_u256__less_than:
LFB7:
	mov	x5, x0
	mov	x2, 3
L69:
	ldr	x4, [x5, x2, lsl 3]
	ldr	x3, [x1, x2, lsl 3]
	cmp	x4, x3
	bcc	L70
	cmp	w2, 0
	sub	x2, x2, #1
	ccmp	x4, x3, 0, ne
	cset	w0, eq
	beq	L69
	ret
	.p2align 2,,3
L70:
	mov	w0, 1
	ret
LFE7:
	.const
	.align	3
lC9:
	.ascii "failed postcondition from aegis_u256.ads:42"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__less_than_or_equal
_aegis_u256__less_than_or_equal:
LFB8:
	mov	x3, x0
	mov	x2, 3
L74:
	ldr	x4, [x3, x2, lsl 3]
	ldr	x0, [x1, x2, lsl 3]
	cmp	x4, x0
	bcc	L82
	cmp	w2, 0
	sub	x2, x2, #1
	ccmp	x4, x0, 0, ne
	cset	w0, eq
	beq	L74
	ldp	x7, x9, [x3]
	mov	w2, 1
	ldp	x6, x8, [x1]
	ldp	x11, x13, [x3, 16]
	ldp	x10, x12, [x1, 16]
	cmp	x7, x6
	beq	L89
L76:
	mov	w2, 0
L75:
	orr	w0, w0, w2
	mov	x2, 3
	and	w0, w0, 255
L78:
	ldr	x5, [x3, x2, lsl 3]
	ldr	x4, [x1, x2, lsl 3]
	cmp	x5, x4
	bcc	L83
	cmp	w2, 0
	sub	x2, x2, #1
	ccmp	x5, x4, 0, ne
	cset	w4, eq
	beq	L78
	mov	w1, 1
	cmp	x7, x6
	beq	L90
L81:
	mov	w1, 0
L80:
	orr	w4, w4, w1
	and	w4, w4, 255
	cmp	w4, w0
	bne	L91
	ret
	.p2align 2,,3
L90:
	cmp	x9, x8
	bne	L81
	cmp	x11, x10
	bne	L81
	cmp	x13, x12
	bne	L81
	b	L80
	.p2align 2,,3
L89:
	cmp	x9, x8
	bne	L76
	cmp	x11, x10
	bne	L76
	cmp	x13, x12
	bne	L76
	b	L75
	.p2align 2,,3
L82:
	ldp	x7, x9, [x3]
	mov	w0, 1
	mov	w2, 1
	ldp	x6, x8, [x1]
	ldp	x11, x13, [x3, 16]
	ldp	x10, x12, [x1, 16]
	cmp	x7, x6
	bne	L76
	b	L89
	.p2align 2,,3
L83:
	mov	w4, 1
	mov	w1, 1
	cmp	x7, x6
	bne	L81
	b	L90
L91:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	stp	x29, x30, [sp, -16]!
LCFI9:
	add	x0, x0, lC9@PAGEOFF;
	mov	x29, sp
LCFI10:
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE8:
	.const
	.align	3
lC10:
	.ascii "failed postcondition from aegis_u256.ads:47"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__greater_than
_aegis_u256__greater_than:
LFB10:
	mov	x5, x0
	mov	x2, 3
L94:
	ldr	x4, [x1, x2, lsl 3]
	ldr	x3, [x5, x2, lsl 3]
	cmp	x4, x3
	bcc	L98
	cmp	w2, 0
	sub	x2, x2, #1
	ccmp	x4, x3, 0, ne
	cset	w6, eq
	beq	L94
L93:
	mov	x2, 3
L96:
	ldr	x4, [x1, x2, lsl 3]
	ldr	x3, [x5, x2, lsl 3]
	cmp	x4, x3
	bcc	L99
	cmp	w2, 0
	sub	x2, x2, #1
	ccmp	x4, x3, 0, ne
	cset	w0, eq
	beq	L96
L95:
	cmp	w0, w6
	bne	L105
	ret
	.p2align 2,,3
L98:
	mov	w6, 1
	b	L93
	.p2align 2,,3
L99:
	mov	w0, 1
	b	L95
L105:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	stp	x29, x30, [sp, -16]!
LCFI11:
	add	x0, x0, lC10@PAGEOFF;
	mov	x29, sp
LCFI12:
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE10:
	.const
	.align	3
lC11:
	.ascii "failed postcondition from aegis_u256.ads:52"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__greater_than_or_equal
_aegis_u256__greater_than_or_equal:
LFB12:
	stp	x29, x30, [sp, -48]!
LCFI13:
	mov	x29, sp
LCFI14:
	stp	x19, x20, [sp, 16]
LCFI15:
	mov	x20, x0
	mov	x19, x1
	str	x21, [sp, 32]
LCFI16:
	bl	_aegis_u256__greater_than
	ldp	x3, x5, [x20]
	ldp	x2, x4, [x19]
	ldp	x7, x9, [x20, 16]
	ldp	x6, x8, [x19, 16]
	cmp	x3, x2
	beq	L113
L108:
	mov	w1, 0
L107:
	orr	w21, w0, w1
	mov	x1, x19
	mov	x0, x20
	and	w21, w21, 255
	bl	_aegis_u256__greater_than
	ldp	x3, x5, [x20]
	mov	w1, 1
	ldp	x2, x4, [x19]
	ldp	x7, x9, [x20, 16]
	ldp	x6, x8, [x19, 16]
	cmp	x3, x2
	beq	L114
L111:
	mov	w1, 0
L110:
	orr	w0, w0, w1
	cmp	w21, w0, uxtb
	bne	L115
	mov	w0, w21
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI17:
	ret
	.p2align 2,,3
L114:
LCFI18:
	cmp	x5, x4
	bne	L111
	cmp	x7, x6
	bne	L111
	cmp	x9, x8
	bne	L111
	b	L110
	.p2align 2,,3
L113:
	cmp	x5, x4
	bne	L108
	cmp	x7, x6
	bne	L108
	mov	w1, 1
	cmp	x9, x8
	bne	L108
	b	L107
L115:
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE12:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__is_zero
_aegis_u256__is_zero:
LFB14:
	stp	x29, x30, [sp, -16]!
LCFI19:
	mov	x29, sp
	ldp	x1, x4, [x0]
	ldp	x3, x2, [x0, 16]
	orr	x0, x1, x4
	orr	x0, x0, x3
	orr	x0, x0, x2
	cmp	x0, 0
	cset	w0, eq
	ldp	x29, x30, [sp], 16
LCFI20:
	ret
LFE14:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__add
_aegis_u256__add:
LFB16:
	mov	x4, 0
	mov	w6, 0
L131:
	ldr	x5, [x1, x4]
	ldr	x3, [x0, x4]
	adds	x3, x3, x5
	cset	w5, cs
	cbz	w6, L132
	adds	x3, x3, 1
	csel	w6, w5, w6, cc
	csel	x3, x3, xzr, cc
L128:
	str	x3, [x2, x4]
	add	x4, x4, 8
	cmp	x4, 32
	bne	L131
	mov	w0, w6
	ret
	.p2align 2,,3
L132:
	mov	w6, w5
	b	L128
LFE16:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__add_mod
_aegis_u256__add_mod:
LFB17:
	stp	x29, x30, [sp, -48]!
LCFI21:
	mov	x29, sp
LCFI22:
	add	x2, x29, 16
	bl	_aegis_u256__add
	ld1	{v30.16b - v31.16b}, [x2]
	st1	{v30.16b - v31.16b}, [x8]
	ldp	x29, x30, [sp], 48
LCFI23:
	ret
LFE17:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__sub
_aegis_u256__sub:
LFB18:
	mov	x3, 0
	mov	w6, 0
L139:
	ldr	x5, [x1, x3]
	ldr	x4, [x0, x3]
	subs	x4, x4, x5
	cset	w5, cc
	cbz	w6, L140
	cbnz	x4, L143
	mov	x4, -1
L138:
	str	x4, [x2, x3]
	add	x3, x3, 8
	cmp	x3, 32
	bne	L139
	mov	w0, w6
	ret
	.p2align 2,,3
L140:
	mov	w6, w5
	b	L138
L143:
	sub	x4, x4, #1
	mov	w6, w5
	b	L138
LFE18:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__sub_mod
_aegis_u256__sub_mod:
LFB19:
	stp	x29, x30, [sp, -48]!
LCFI24:
	mov	x29, sp
LCFI25:
	add	x2, x29, 16
	bl	_aegis_u256__sub
	ld1	{v30.16b - v31.16b}, [x2]
	st1	{v30.16b - v31.16b}, [x8]
	ldp	x29, x30, [sp], 48
LCFI26:
	ret
LFE19:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__mul
_aegis_u256__mul:
LFB20:
	movi	v31.4s, 0
	sub	sp, sp, #64
LCFI27:
	mov	x15, 0
	sub	x14, sp, #8
	stp	q31, q31, [sp]
	stp	q31, q31, [sp, 32]
L152:
	ldr	x12, [x0, x15, lsl 3]
	mov	x11, 0
	mov	x16, 0
	mov	x7, x14
	lsr	x13, x12, 32
L151:
	ldr	x4, [x1, x11]
	add	x11, x11, 8
	ldp	x9, x8, [x7, 8]
	lsr	x10, x4, 32
	umull	x6, w13, w4
	umull	x4, w12, w4
	umull	x5, w12, w10
	adds	x5, x5, x6
	cset	x6, cs
	lsr	x17, x5, 32
	lsl	x5, x5, 32
	adds	x4, x4, x5
	umaddl	x10, w13, w10, x17
	cset	x5, cs
	cmp	x9, x4
	add	x5, x5, x6, lsl 32
	add	x6, x9, x4
	csel	x9, x9, x4, cs
	add	x6, x6, x16
	add	x4, x5, x10
	cmp	x6, x9
	add	x5, x8, x4
	cinc	x5, x5, cc
	cmp	x8, x4
	stp	x6, x5, [x7, 8]!
	csel	x8, x8, x4, cs
	cmp	x5, x8
	cset	x16, cc
	cmp	x11, 32
	bne	L151
	add	x15, x15, 1
	add	x14, x14, 8
	ldr	x4, [x14, 32]
	add	x16, x16, x4
	str	x16, [x14, 32]
	cmp	x15, 4
	bne	L152
	ldp	q29, q28, [sp]
	ldp	q31, q30, [sp, 32]
	stp	q29, q28, [x3]
	stp	q31, q30, [x2]
	add	sp, sp, 64
LCFI28:
	ret
LFE20:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__mul_mod
_aegis_u256__mul_mod:
LFB21:
	stp	x29, x30, [sp, -96]!
LCFI29:
	mov	x29, sp
LCFI30:
	add	x3, x29, 64
	add	x2, x29, 32
	str	x19, [sp, 16]
LCFI31:
	mov	x19, x8
	bl	_aegis_u256__mul
	ld1	{v30.16b - v31.16b}, [x3]
	st1	{v30.16b - v31.16b}, [x19]
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI32:
	ret
LFE21:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__bit_and
_aegis_u256__bit_and:
LFB25:
	ldp	q0, q29, [x0]
	ldp	q30, q31, [x1]
	and	v30.16b, v30.16b, v0.16b
	and	v31.16b, v29.16b, v31.16b
	st1	{v30.16b - v31.16b}, [x8]
	ret
LFE25:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__bit_or
_aegis_u256__bit_or:
LFB26:
	ldp	q0, q29, [x0]
	ldp	q30, q31, [x1]
	orr	v30.16b, v30.16b, v0.16b
	orr	v31.16b, v29.16b, v31.16b
	st1	{v30.16b - v31.16b}, [x8]
	ret
LFE26:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__bit_xor
_aegis_u256__bit_xor:
LFB27:
	ldp	q0, q29, [x0]
	ldp	q30, q31, [x1]
	eor	v30.16b, v30.16b, v0.16b
	eor	v31.16b, v29.16b, v31.16b
	st1	{v30.16b - v31.16b}, [x8]
	ret
LFE27:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__bit_not
_aegis_u256__bit_not:
LFB28:
	ldp	q30, q31, [x0]
	not	v30.16b, v30.16b
	not	v31.16b, v31.16b
	st1	{v30.16b - v31.16b}, [x8]
	ret
LFE28:
	.const
	.align	3
lC12:
	.ascii "aegis_u256.ads"
	.space 1
	.align	3
lC13:
	.ascii "failed precondition from aegis_u256.ads:143"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__shift_left
_aegis_u256__shift_left:
LFB29:
	stp	x29, x30, [sp, -64]!
LCFI33:
	mov	x29, sp
LCFI34:
	stp	x19, x20, [sp, 16]
LCFI35:
	tbnz	w1, #31, L174
	cmp	w1, 255
	bgt	L175
	movi	v31.4s, 0
	mov	x19, x8
	stp	q31, q31, [x29, 32]
	cbz	w1, L176
	ands	w7, w1, 63
	asr	w6, w1, 6
	beq	L177
	mov	w9, 64
	sub	x3, x0, #8
	sub	w9, w9, w7
	sub	w1, w6, #1
	add	x20, x29, 32
	add	x4, x20, w6, uxtw 3
	.p2align 5,,15
L170:
	ldr	x2, [x3, 8]
	add	w1, w1, 1
	lsl	x2, x2, x7
	str	x2, [x4]
	cmp	w6, w1
	bge	L168
	sub	w0, w1, w6
	sub	w0, w0, #1
	cmp	w0, 3
	bhi	L178
	ldr	x5, [x3]
	lsr	x5, x5, x9
	orr	x2, x5, x2
	str	x2, [x4]
L168:
	add	x4, x4, 8
	add	x3, x3, 8
	cmp	w1, 3
	bne	L170
	b	L171
	.p2align 2,,3
L177:
	mov	w2, 4
	mov	x1, x0
	sub	w2, w2, w6
	add	x20, x29, 32
	add	x0, x20, w6, uxtw 3
	lsl	x2, x2, 3
	bl	_memcpy
L171:
	ld1	{v30.16b - v31.16b}, [x20]
	st1	{v30.16b - v31.16b}, [x19]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI36:
	ret
	.p2align 2,,3
L176:
LCFI37:
	ld1	{v30.16b - v31.16b}, [x0]
	st1	{v30.16b - v31.16b}, [x8]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI38:
	ret
L178:
LCFI39:
	adrp	x0, lC8@PAGE
	mov	w1, 340
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L175:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L174:
	adrp	x0, lC12@PAGE
	mov	w1, 143
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE29:
	.const
	.align	3
lC14:
	.ascii "failed precondition from aegis_u256.ads:108"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__div_mod
_aegis_u256__div_mod:
LFB22:
	stp	x29, x30, [sp, -272]!
LCFI40:
	mov	x29, sp
LCFI41:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI42:
	mov	x22, x1
	stp	x23, x24, [sp, 48]
LCFI43:
	mov	x23, x0
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
	stp	d13, d14, [sp, 96]
	str	d15, [sp, 112]
LCFI44:
	stp	x3, x2, [x29, 128]
	ldp	x0, x3, [x1]
	ldp	x2, x1, [x1, 16]
	orr	x0, x0, x3
	orr	x0, x0, x2
	orr	x0, x0, x1
	cbz	x0, L193
	movi	v31.4s, 0
	mov	w28, 256
	add	x27, x29, 176
	add	x20, x29, 144
	add	x24, x29, 208
	add	x25, x29, 240
	mov	v30.16b, v31.16b
	stp	q31, q31, [x29, 208]
	.p2align 5,,15
L186:
	mov	w1, 1
	mov	x0, x27
	mov	x8, x20
	sub	w28, w28, #1
	stp	q31, q30, [x29, 176]
	bl	_aegis_u256__shift_left
	ldp	x7, x6, [x20]
	asr	w26, w28, 6
	mov	x1, x22
	mov	x0, x27
	and	w21, w28, 63
	ldr	x19, [x23, w26, sxtw 3]
	str	x7, [x27]
	ldp	x5, x2, [x20, 16]
	fmov	d13, x6
	lsr	x19, x19, x28
	and	x19, x19, 1
	fmov	d14, x5
	orr	x19, x19, x7
	fmov	d15, x2
	stp	x19, x6, [x27]
	stp	x5, x2, [x27, 16]
	bl	_aegis_u256__greater_than_or_equal
	tbnz	x0, 0, L184
	fmov	d31, x19
	fmov	d30, d14
	ins	v30.d[1], v15.d[0]
	ins	v31.d[1], v13.d[0]
	cbnz	w28, L186
L194:
	ld1	{v28.16b - v29.16b}, [x24]
	ldr	x0, [x29, 136]
	st1	{v28.16b - v29.16b}, [x0]
	ldr	x0, [x29, 128]
	stp	q31, q30, [x0]
	ldr	d15, [sp, 112]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	d13, d14, [sp, 96]
	ldp	x29, x30, [sp], 272
LCFI45:
	ret
	.p2align 2,,3
L184:
LCFI46:
	mov	x2, x25
	mov	x1, x22
	mov	x0, x27
	bl	_aegis_u256__sub
	ld1	{v28.16b - v29.16b}, [x25]
	mov	x0, 1
	lsl	x21, x0, x21
	ldr	x0, [x24, w26, sxtw 3]
	orr	x21, x21, x0
	st1	{v28.16b - v29.16b}, [x27]
	str	x21, [x24, w26, sxtw 3]
	ldp	q31, q30, [x29, 240]
	cbnz	w28, L186
	b	L194
L193:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE22:
	.const
	.align	3
lC15:
	.ascii "failed precondition from aegis_u256.ads:113"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__div
_aegis_u256__div:
LFB23:
	stp	x29, x30, [sp, -96]!
LCFI47:
	mov	x29, sp
LCFI48:
	stp	x19, x20, [sp, 16]
LCFI49:
	ldp	x2, x5, [x1]
	ldp	x4, x3, [x1, 16]
	orr	x2, x2, x5
	orr	x2, x2, x4
	orr	x2, x2, x3
	cbz	x2, L205
	add	x19, x29, 32
	add	x3, x29, 64
	mov	x2, x19
	mov	x20, x8
	bl	_aegis_u256__div_mod
	ld1	{v30.16b - v31.16b}, [x19]
	st1	{v30.16b - v31.16b}, [x20]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI50:
	ret
L205:
LCFI51:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE23:
	.const
	.align	3
lC16:
	.ascii "failed precondition from aegis_u256.ads:118"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__mod_op
_aegis_u256__mod_op:
LFB24:
	stp	x29, x30, [sp, -96]!
LCFI52:
	mov	x29, sp
LCFI53:
	stp	x19, x20, [sp, 16]
LCFI54:
	ldp	x2, x5, [x1]
	ldp	x4, x3, [x1, 16]
	orr	x2, x2, x5
	orr	x2, x2, x4
	orr	x2, x2, x3
	cbz	x2, L216
	add	x19, x29, 64
	add	x2, x29, 32
	mov	x3, x19
	mov	x20, x8
	bl	_aegis_u256__div_mod
	ld1	{v30.16b - v31.16b}, [x19]
	st1	{v30.16b - v31.16b}, [x20]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI55:
	ret
L216:
LCFI56:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE24:
	.const
	.align	3
lC17:
	.ascii "failed precondition from aegis_u256.ads:148"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__shift_right
_aegis_u256__shift_right:
LFB30:
	stp	x29, x30, [sp, -48]!
LCFI57:
	mov	x29, sp
LCFI58:
	tbnz	w1, #31, L235
	cmp	w1, 255
	bgt	L236
	movi	v31.4s, 0
	stp	q31, q31, [x29, 16]
	cbz	w1, L237
	asr	w7, w1, 6
	mov	w5, 3
	ands	w6, w1, 63
	sub	w5, w5, w7
	beq	L238
	add	x9, x29, 16
	mov	w10, 4
	add	x2, x0, w7, uxtw 3
	mov	w11, 64
	mov	x4, x9
	sub	w10, w10, w7
	sub	w11, w11, w6
	mov	w0, -1
	.p2align 5,,15
L227:
	add	w0, w0, 1
	cmp	w0, w10
	beq	L239
	ldr	x1, [x2]
	add	w3, w0, w7
	lsr	x1, x1, x6
	str	x1, [x4]
	cmp	w3, 3
	beq	L226
	ldr	x3, [x2, 8]
	lsl	x3, x3, x11
	orr	x1, x3, x1
	str	x1, [x4]
L226:
	add	x2, x2, 8
	add	x4, x4, 8
	cmp	w5, w0
	bne	L227
L228:
	ld1	{v30.16b - v31.16b}, [x9]
	st1	{v30.16b - v31.16b}, [x8]
	ldp	x29, x30, [sp], 48
LCFI59:
	ret
	.p2align 2,,3
L238:
LCFI60:
	mov	w3, 4
	add	w4, w7, 1
	add	x4, x0, x4, lsl 3
	sub	w3, w3, w7
	mov	x0, -1
	add	x3, x3, x0
	add	x9, x29, 16
	b	L223
	.p2align 2,,3
L224:
	cmp	x3, x0
	beq	L240
L223:
	ldr	x2, [x4, x0, lsl 3]
	add	x1, x9, x0, lsl 3
	add	x0, x0, 1
	str	x2, [x1, 8]
	cmp	w5, w0
	bne	L224
	b	L228
	.p2align 2,,3
L237:
	ld1	{v30.16b - v31.16b}, [x0]
	st1	{v30.16b - v31.16b}, [x8]
	ldp	x29, x30, [sp], 48
LCFI61:
	ret
L239:
LCFI62:
	adrp	x0, lC8@PAGE
	mov	w1, 369
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L240:
	adrp	x0, lC8@PAGE
	mov	w1, 364
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L236:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L235:
	adrp	x0, lC12@PAGE
	mov	w1, 148
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE30:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__from_word64
_aegis_u256__from_word64:
LFB31:
	stp	x0, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
	ret
LFE31:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__to_word64
_aegis_u256__to_word64:
LFB33:
	ldr	x0, [x0]
	ret
LFE33:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__from_bytes_be
_aegis_u256__from_bytes_be:
LFB35:
	sub	sp, sp, #32
LCFI63:
	add	x0, x0, 24
	mov	x6, sp
	mov	x5, sp
	mov	x4, 24
L245:
	mov	x1, 0
	mov	x2, 0
	.p2align 5,,15
L244:
	ldrb	w3, [x0, x1]
	add	x1, x1, 1
	orr	x2, x3, x2, lsl 8
	cmp	x1, 8
	bne	L244
	sub	x4, x4, #8
	str	x2, [x5], 8
	sub	x0, x0, #8
	cmn	x4, #8
	bne	L245
	ld1	{v30.16b - v31.16b}, [x6]
	st1	{v30.16b - v31.16b}, [x8]
	add	sp, sp, 32
LCFI64:
	ret
LFE35:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__to_bytes_be
_aegis_u256__to_bytes_be:
LFB36:
	add	x0, x0, 24
	add	x3, x8, 23
	stp	xzr, xzr, [x8]
	mov	x4, 24
	stp	xzr, xzr, [x8, 16]
L251:
	neg	x2, x4
	mov	x1, 8
	ldr	x2, [x0, x2]
	.p2align 5,,15
L250:
	strb	w2, [x3, x1]
	subs	x1, x1, #1
	lsr	x2, x2, 8
	bne	L250
	sub	x4, x4, #8
	sub	x3, x3, #8
	cmn	x4, #8
	bne	L251
	ret
LFE36:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__from_bytes_le
_aegis_u256__from_bytes_le:
LFB37:
	sub	sp, sp, #32
LCFI65:
	mov	x5, 0
	mov	x7, sp
	sub	x0, x0, #1
	mov	x6, sp
L256:
	mov	x4, x0
	add	x0, x0, 8
	mov	x2, 0
	mov	x1, x0
	.p2align 5,,15
L255:
	ldrb	w3, [x1], -1
	orr	x2, x3, x2, lsl 8
	cmp	x1, x4
	bne	L255
	add	x5, x5, 8
	str	x2, [x6], 8
	cmp	x5, 32
	bne	L256
	ld1	{v30.16b - v31.16b}, [x7]
	st1	{v30.16b - v31.16b}, [x8]
	add	sp, sp, 32
LCFI66:
	ret
LFE37:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__to_bytes_le
_aegis_u256__to_bytes_le:
LFB38:
	mov	x4, 0
	mov	x3, x8
	stp	xzr, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
L262:
	mov	x1, 0
	ldr	x2, [x0, x4]
	.p2align 5,,15
L261:
	strb	w2, [x3, x1]
	add	x1, x1, 1
	lsr	x2, x2, 8
	cmp	x1, 8
	bne	L261
	add	x4, x4, 8
	add	x3, x3, 8
	cmp	x4, 32
	bne	L262
	ret
LFE38:
	.const
	.align	3
lC18:
	.ascii "failed postcondition from aegis_u256.ads:186"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__u64_to_u256
_aegis_u256__u64_to_u256:
LFB39:
	stp	x0, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
	ret
LFE39:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__u256_to_u64
_aegis_u256__u256_to_u64:
LFB74:
	ldr	x0, [x0]
	ret
LFE74:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__gas_to_u256
_aegis_u256__gas_to_u256:
LFB43:
	tbnz	x0, #63, L275
	stp	x0, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
	ret
L275:
	adrp	x0, lC8@PAGE
	stp	x29, x30, [sp, -16]!
LCFI67:
	mov	w1, 477
	mov	x29, sp
LCFI68:
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE43:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__address_to_u256
_aegis_u256__address_to_u256:
LFB44:
	mov	x2, x0
	stp	x29, x30, [sp, -48]!
LCFI69:
	mov	x29, sp
LCFI70:
	add	x1, x29, 16
	mov	x0, x1
	ldp	q31, q30, [x2]
	stp	q31, q30, [x1]
	bl	_aegis_u256__from_bytes_be
	ldp	x29, x30, [sp], 48
LCFI71:
	ret
LFE44:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__u256_to_address
_aegis_u256__u256_to_address:
LFB45:
	stp	x29, x30, [sp, -48]!
LCFI72:
	mov	x29, sp
LCFI73:
	mov	x5, x8
	add	x1, x29, 16
	mov	x8, x1
	bl	_aegis_u256__to_bytes_be
	ldp	q31, q30, [x8]
	stp	q31, q30, [x5]
	ldp	x29, x30, [sp], 48
LCFI74:
	ret
LFE45:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__clz
_aegis_u256__clz:
LFB46:
	stp	x29, x30, [sp, -32]!
LCFI75:
	mov	x29, sp
LCFI76:
	add	x1, x29, 32
	add	x16, x29, 16
	stp	x0, x1, [x29, 16]
	bl	_aegis_u256__clz___wrapped_statements.9
	ldp	x29, x30, [sp], 32
LCFI77:
	ret
LFE46:
	.const
	.align	3
lC19:
	.ascii "Loop_Invariant failed at aegis_u256.adb:540"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__ctz
_aegis_u256__ctz:
LFB48:
	ldr	x1, [x0]
	cbnz	x1, L295
	ldr	x1, [x0, 8]
	cbnz	x1, L296
	ldr	x1, [x0, 16]
	cbnz	x1, L297
	ldr	x1, [x0, 24]
	mov	w5, 192
	mov	w0, 256
	cbnz	x1, L283
L282:
	ret
	.p2align 2,,3
L295:
	mov	w5, 0
L283:
	dup	v30.2d, x1
	adrp	x0, lC20@PAGE
	adrp	x1, lC4@PAGE
	dup	v19.4s, w5
	ldr	q31, [x0, #lC20@PAGEOFF]
	mov	w0, 0
	mvni	v17.4s, 0x3
	movi	v21.4s, 0x4
	ldr	q26, [x1, #lC4@PAGEOFF]
	adrp	x1, lC5@PAGE
	movi	v18.4s, 0x1
	ldr	q24, [x1, #lC5@PAGEOFF]
	adrp	x1, lC6@PAGE
	ldr	q25, [x1, #lC6@PAGEOFF]
	adrp	x1, lC21@PAGE
	ushl	v30.2d, v30.2d, v31.2d
	ldr	q22, [x1, #lC21@PAGEOFF]
	add	v26.4s, v19.4s, v26.4s
	b	L289
	.p2align 2,,3
L287:
	add	w0, w0, 1
	cmp	w0, 16
	beq	L307
L289:
	mov	v23.16b, v25.16b
	mov	v29.16b, v26.16b
	mov	v27.16b, v30.16b
	mov	v20.16b, v24.16b
	add	v25.4s, v25.4s, v21.4s
	add	v26.4s, v26.4s, v21.4s
	ushr	v30.2d, v30.2d, 4
	add	v24.4s, v24.4s, v17.4s
	add	v31.4s, v23.4s, v18.4s
	ushr	v28.2d, v27.2d, 2
	add	v31.4s, v31.4s, v19.4s
	cmgt	v31.4s, v29.4s, v31.4s
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbnz	x1, L285
	cmtst	v28.2d, v28.2d, v22.2d
	cmtst	v31.2d, v27.2d, v22.2d
	orr	v31.16b, v31.16b, v28.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbz	x1, L287
L285:
	fmov	w0, s29
	add	w5, w5, 1
	fmov	w1, s23
	sub	w2, w1, w0
	fmov	w1, s20
	add	w3, w0, w1
	fmov	x1, d27
	b	L288
	.p2align 2,,3
L294:
	add	w0, w0, 1
	lsr	x1, x1, 1
	cmp	w3, w0
	beq	L282
L288:
	add	w4, w5, w0
	add	w4, w4, w2
	cmp	w0, w4
	bgt	L308
	tbz	x1, 0, L294
	ret
L296:
	mov	w5, 64
	b	L283
L297:
	mov	w5, 128
	b	L283
L307:
	add	v29.4s, v29.4s, v18.4s
	umov	w0, v29.s[3]
	ret
L308:
	adrp	x0, lC19@PAGE
	adrp	x1, lC0@PAGE
	stp	x29, x30, [sp, -16]!
LCFI78:
	add	x0, x0, lC19@PAGEOFF;
	mov	x29, sp
LCFI79:
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE48:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__bit_length
_aegis_u256__bit_length:
LFB50:
	stp	x29, x30, [sp, -32]!
LCFI80:
	mov	x29, sp
LCFI81:
	add	x1, x29, 32
	add	x16, x29, 16
	stp	x0, x1, [x29, 16]
	bl	_aegis_u256__clz___wrapped_statements.9
	mov	w1, 256
	sub	w0, w1, w0
	ldp	x29, x30, [sp], 32
LCFI82:
	ret
LFE50:
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__byte_length
_aegis_u256__byte_length:
LFB52:
	stp	x29, x30, [sp, -32]!
LCFI83:
	mov	x29, sp
LCFI84:
	add	x1, x29, 32
	add	x16, x29, 16
	stp	x0, x1, [x29, 16]
	bl	_aegis_u256__clz___wrapped_statements.9
	mov	w1, 263
	sub	w0, w1, w0
	asr	w0, w0, 3
	ldp	x29, x30, [sp], 32
LCFI85:
	ret
LFE52:
	.const
	.align	3
lC22:
	.ascii "failed precondition from aegis_u256.ads:241"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_u256__mod_exp
_aegis_u256__mod_exp:
LFB54:
	stp	x29, x30, [sp, -368]!
LCFI86:
	mov	x29, sp
LCFI87:
	stp	x19, x20, [sp, 16]
LCFI88:
	mov	x19, x1
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
LCFI89:
	mov	x23, x2
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI90:
	str	x8, [x29, 104]
	ldp	x2, x4, [x2]
	ldp	x3, x1, [x23, 16]
	orr	x2, x2, x4
	orr	x2, x2, x3
	orr	x2, x2, x1
	cbz	x2, L333
	add	x20, x29, 208
	mov	x1, x23
	mov	x8, x20
	add	x28, x29, 240
	bl	_aegis_u256__mod_op
	ld1	{v30.16b - v31.16b}, [x19]
	adrp	x0, lC23@PAGE
	add	x27, x29, 336
	add	x24, x29, 304
	add	x25, x29, 176
	stp	xzr, xzr, [x29, 112]
	add	x22, x29, 272
	add	x26, x29, 144
	mov	w21, 256
	ldr	q29, [x0, #lC23@PAGEOFF]
	ldr	x19, [x19]
	st1	{v30.16b - v31.16b}, [x28]
	str	q29, [x29, 128]
	b	L321
	.p2align 2,,3
L334:
	tbz	x19, 0, L320
	ldr	q31, [x29, 128]
	mov	x3, x27
	mov	x2, x24
	mov	x1, x20
	mov	x0, x25
	str	q31, [x29, 176]
	ldr	q31, [x29, 112]
	str	q31, [x29, 192]
	bl	_aegis_u256__mul
	ld1	{v30.16b - v31.16b}, [x27]
	mov	x8, x25
	mov	x1, x23
	mov	x0, x22
	st1	{v30.16b - v31.16b}, [x22]
	bl	_aegis_u256__mod_op
	ldr	q31, [x29, 176]
	str	q31, [x29, 128]
	ldr	q31, [x29, 192]
	str	q31, [x29, 112]
L320:
	mov	x2, x24
	mov	x3, x27
	mov	x1, x20
	mov	x0, x20
	bl	_aegis_u256__mul
	ld1	{v30.16b - v31.16b}, [x27]
	mov	x8, x20
	mov	x1, x23
	mov	x0, x22
	st1	{v30.16b - v31.16b}, [x22]
	bl	_aegis_u256__mod_op
	mov	x0, x28
	mov	w1, 1
	str	x19, [x29, 240]
	mov	x8, x26
	bl	_aegis_u256__shift_right
	ldp	x19, x1, [x26]
	subs	w21, w21, #1
	ldr	x2, [x26, 16]
	str	x19, [x28]
	ldr	x0, [x26, 24]
	stp	x1, x2, [x28, 8]
	str	x0, [x28, 24]
	beq	L318
L321:
	str	x19, [x29, 240]
	ldp	x0, x2, [x28, 8]
	ldr	x1, [x28, 24]
	orr	x0, x19, x0
	orr	x0, x0, x2
	orr	x0, x0, x1
	cbnz	x0, L334
L318:
	ldp	q30, q31, [x29, 112]
	ldr	x0, [x29, 104]
	stp	q31, q30, [x0]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 368
LCFI91:
	ret
L333:
LCFI92:
	adrp	x0, lC22@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC22@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE54:
	.const
	.align	2
lC1:
	.word	1
	.word	44
	.globl _aegis_u256_E
	.data
	.align	1
_aegis_u256_E:
	.space 2
	.literal16
	.align	4
lC3:
	.xword	0
	.xword	1
	.align	4
lC4:
	.word	0
	.word	1
	.word	2
	.word	3
	.align	4
lC5:
	.word	64
	.word	63
	.word	62
	.word	61
	.align	4
lC6:
	.word	-1
	.word	0
	.word	1
	.word	2
	.align	4
lC20:
	.xword	0
	.xword	-1
	.align	4
lC21:
	.xword	1
	.xword	1
	.align	4
lC23:
	.xword	1
	.xword	0
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
	.quad	LFB47-.
	.set L$set$2,LFE47-LFB47
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB47
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
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$5,LEFDE3-LASFDE3
	.long L$set$5
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB69-.
	.set L$set$6,LFE69-LFB69
	.quad L$set$6
	.uleb128 0
	.byte	0x4
	.set L$set$7,LCFI2-LFB69
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
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$9,LEFDE5-LASFDE5
	.long L$set$9
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB2-.
	.set L$set$10,LFE2-LFB2
	.quad L$set$10
	.uleb128 0
	.byte	0x4
	.set L$set$11,LCFI4-LFB2
	.long L$set$11
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$13,LEFDE7-LASFDE7
	.long L$set$13
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB3-.
	.set L$set$14,LFE3-LFB3
	.quad L$set$14
	.uleb128 0
	.byte	0x4
	.set L$set$15,LCFI6-LFB3
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
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xdd
	.byte	0xde
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$18,LEFDE9-LASFDE9
	.long L$set$18
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB4-.
	.set L$set$19,LFE4-LFB4
	.quad L$set$19
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$20,LEFDE11-LASFDE11
	.long L$set$20
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB5-.
	.set L$set$21,LFE5-LFB5
	.quad L$set$21
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$22,LEFDE13-LASFDE13
	.long L$set$22
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB7-.
	.set L$set$23,LFE7-LFB7
	.quad L$set$23
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$24,LEFDE15-LASFDE15
	.long L$set$24
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB8-.
	.set L$set$25,LFE8-LFB8
	.quad L$set$25
	.uleb128 0
	.byte	0x4
	.set L$set$26,LCFI9-LFB8
	.long L$set$26
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$27,LCFI10-LCFI9
	.long L$set$27
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$28,LEFDE17-LASFDE17
	.long L$set$28
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB10-.
	.set L$set$29,LFE10-LFB10
	.quad L$set$29
	.uleb128 0
	.byte	0x4
	.set L$set$30,LCFI11-LFB10
	.long L$set$30
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$31,LCFI12-LCFI11
	.long L$set$31
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$32,LEFDE19-LASFDE19
	.long L$set$32
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB12-.
	.set L$set$33,LFE12-LFB12
	.quad L$set$33
	.uleb128 0
	.byte	0x4
	.set L$set$34,LCFI13-LFB12
	.long L$set$34
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$35,LCFI14-LCFI13
	.long L$set$35
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$36,LCFI15-LCFI14
	.long L$set$36
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$37,LCFI16-LCFI15
	.long L$set$37
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$38,LCFI17-LCFI16
	.long L$set$38
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
	.set L$set$39,LCFI18-LCFI17
	.long L$set$39
	.byte	0xb
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$40,LEFDE21-LASFDE21
	.long L$set$40
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB14-.
	.set L$set$41,LFE14-LFB14
	.quad L$set$41
	.uleb128 0
	.byte	0x4
	.set L$set$42,LCFI19-LFB14
	.long L$set$42
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$43,LCFI20-LCFI19
	.long L$set$43
	.byte	0xde
	.byte	0xdd
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$44,LEFDE23-LASFDE23
	.long L$set$44
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB16-.
	.set L$set$45,LFE16-LFB16
	.quad L$set$45
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$46,LEFDE25-LASFDE25
	.long L$set$46
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB17-.
	.set L$set$47,LFE17-LFB17
	.quad L$set$47
	.uleb128 0
	.byte	0x4
	.set L$set$48,LCFI21-LFB17
	.long L$set$48
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$49,LCFI22-LCFI21
	.long L$set$49
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$50,LCFI23-LCFI22
	.long L$set$50
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$51,LEFDE27-LASFDE27
	.long L$set$51
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB18-.
	.set L$set$52,LFE18-LFB18
	.quad L$set$52
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$53,LEFDE29-LASFDE29
	.long L$set$53
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB19-.
	.set L$set$54,LFE19-LFB19
	.quad L$set$54
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI24-LFB19
	.long L$set$55
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$56,LCFI25-LCFI24
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$57,LCFI26-LCFI25
	.long L$set$57
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$58,LEFDE31-LASFDE31
	.long L$set$58
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB20-.
	.set L$set$59,LFE20-LFB20
	.quad L$set$59
	.uleb128 0
	.byte	0x4
	.set L$set$60,LCFI27-LFB20
	.long L$set$60
	.byte	0xe
	.uleb128 0x40
	.byte	0x4
	.set L$set$61,LCFI28-LCFI27
	.long L$set$61
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$62,LEFDE33-LASFDE33
	.long L$set$62
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB21-.
	.set L$set$63,LFE21-LFB21
	.quad L$set$63
	.uleb128 0
	.byte	0x4
	.set L$set$64,LCFI29-LFB21
	.long L$set$64
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$65,LCFI30-LCFI29
	.long L$set$65
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$66,LCFI31-LCFI30
	.long L$set$66
	.byte	0x93
	.uleb128 0xa
	.byte	0x4
	.set L$set$67,LCFI32-LCFI31
	.long L$set$67
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$68,LEFDE35-LASFDE35
	.long L$set$68
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB25-.
	.set L$set$69,LFE25-LFB25
	.quad L$set$69
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$70,LEFDE37-LASFDE37
	.long L$set$70
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB26-.
	.set L$set$71,LFE26-LFB26
	.quad L$set$71
	.uleb128 0
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
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$76,LEFDE43-LASFDE43
	.long L$set$76
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB29-.
	.set L$set$77,LFE29-LFB29
	.quad L$set$77
	.uleb128 0
	.byte	0x4
	.set L$set$78,LCFI33-LFB29
	.long L$set$78
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$79,LCFI34-LCFI33
	.long L$set$79
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$80,LCFI35-LCFI34
	.long L$set$80
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$81,LCFI36-LCFI35
	.long L$set$81
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$82,LCFI37-LCFI36
	.long L$set$82
	.byte	0xb
	.byte	0x4
	.set L$set$83,LCFI38-LCFI37
	.long L$set$83
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$84,LCFI39-LCFI38
	.long L$set$84
	.byte	0xb
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$85,LEFDE45-LASFDE45
	.long L$set$85
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB22-.
	.set L$set$86,LFE22-LFB22
	.quad L$set$86
	.uleb128 0
	.byte	0x4
	.set L$set$87,LCFI40-LFB22
	.long L$set$87
	.byte	0xe
	.uleb128 0x110
	.byte	0x9d
	.uleb128 0x22
	.byte	0x9e
	.uleb128 0x21
	.byte	0x4
	.set L$set$88,LCFI41-LCFI40
	.long L$set$88
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$89,LCFI42-LCFI41
	.long L$set$89
	.byte	0x93
	.uleb128 0x20
	.byte	0x94
	.uleb128 0x1f
	.byte	0x95
	.uleb128 0x1e
	.byte	0x96
	.uleb128 0x1d
	.byte	0x4
	.set L$set$90,LCFI43-LCFI42
	.long L$set$90
	.byte	0x97
	.uleb128 0x1c
	.byte	0x98
	.uleb128 0x1b
	.byte	0x4
	.set L$set$91,LCFI44-LCFI43
	.long L$set$91
	.byte	0x99
	.uleb128 0x1a
	.byte	0x9a
	.uleb128 0x19
	.byte	0x9b
	.uleb128 0x18
	.byte	0x9c
	.uleb128 0x17
	.byte	0x5
	.uleb128 0x4d
	.uleb128 0x16
	.byte	0x5
	.uleb128 0x4e
	.uleb128 0x15
	.byte	0x5
	.uleb128 0x4f
	.uleb128 0x14
	.byte	0x4
	.set L$set$92,LCFI45-LCFI44
	.long L$set$92
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
	.byte	0x6
	.uleb128 0x4f
	.byte	0x6
	.uleb128 0x4d
	.byte	0x6
	.uleb128 0x4e
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$93,LCFI46-LCFI45
	.long L$set$93
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$94,LEFDE47-LASFDE47
	.long L$set$94
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB23-.
	.set L$set$95,LFE23-LFB23
	.quad L$set$95
	.uleb128 0
	.byte	0x4
	.set L$set$96,LCFI47-LFB23
	.long L$set$96
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$97,LCFI48-LCFI47
	.long L$set$97
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$98,LCFI49-LCFI48
	.long L$set$98
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$99,LCFI50-LCFI49
	.long L$set$99
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$100,LCFI51-LCFI50
	.long L$set$100
	.byte	0xb
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$101,LEFDE49-LASFDE49
	.long L$set$101
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB24-.
	.set L$set$102,LFE24-LFB24
	.quad L$set$102
	.uleb128 0
	.byte	0x4
	.set L$set$103,LCFI52-LFB24
	.long L$set$103
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$104,LCFI53-LCFI52
	.long L$set$104
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$105,LCFI54-LCFI53
	.long L$set$105
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$106,LCFI55-LCFI54
	.long L$set$106
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$107,LCFI56-LCFI55
	.long L$set$107
	.byte	0xb
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$108,LEFDE51-LASFDE51
	.long L$set$108
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB30-.
	.set L$set$109,LFE30-LFB30
	.quad L$set$109
	.uleb128 0
	.byte	0x4
	.set L$set$110,LCFI57-LFB30
	.long L$set$110
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$111,LCFI58-LCFI57
	.long L$set$111
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$112,LCFI59-LCFI58
	.long L$set$112
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$113,LCFI60-LCFI59
	.long L$set$113
	.byte	0xb
	.byte	0x4
	.set L$set$114,LCFI61-LCFI60
	.long L$set$114
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$115,LCFI62-LCFI61
	.long L$set$115
	.byte	0xb
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$116,LEFDE53-LASFDE53
	.long L$set$116
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB31-.
	.set L$set$117,LFE31-LFB31
	.quad L$set$117
	.uleb128 0
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$118,LEFDE55-LASFDE55
	.long L$set$118
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB33-.
	.set L$set$119,LFE33-LFB33
	.quad L$set$119
	.uleb128 0
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$120,LEFDE57-LASFDE57
	.long L$set$120
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB35-.
	.set L$set$121,LFE35-LFB35
	.quad L$set$121
	.uleb128 0
	.byte	0x4
	.set L$set$122,LCFI63-LFB35
	.long L$set$122
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$123,LCFI64-LCFI63
	.long L$set$123
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$124,LEFDE59-LASFDE59
	.long L$set$124
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB36-.
	.set L$set$125,LFE36-LFB36
	.quad L$set$125
	.uleb128 0
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$126,LEFDE61-LASFDE61
	.long L$set$126
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB37-.
	.set L$set$127,LFE37-LFB37
	.quad L$set$127
	.uleb128 0
	.byte	0x4
	.set L$set$128,LCFI65-LFB37
	.long L$set$128
	.byte	0xe
	.uleb128 0x20
	.byte	0x4
	.set L$set$129,LCFI66-LCFI65
	.long L$set$129
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$130,LEFDE63-LASFDE63
	.long L$set$130
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB38-.
	.set L$set$131,LFE38-LFB38
	.quad L$set$131
	.uleb128 0
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$132,LEFDE65-LASFDE65
	.long L$set$132
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB39-.
	.set L$set$133,LFE39-LFB39
	.quad L$set$133
	.uleb128 0
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$134,LEFDE67-LASFDE67
	.long L$set$134
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB74-.
	.set L$set$135,LFE74-LFB74
	.quad L$set$135
	.uleb128 0
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$136,LEFDE69-LASFDE69
	.long L$set$136
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB43-.
	.set L$set$137,LFE43-LFB43
	.quad L$set$137
	.uleb128 0
	.byte	0x4
	.set L$set$138,LCFI67-LFB43
	.long L$set$138
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$139,LCFI68-LCFI67
	.long L$set$139
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$140,LEFDE71-LASFDE71
	.long L$set$140
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB44-.
	.set L$set$141,LFE44-LFB44
	.quad L$set$141
	.uleb128 0
	.byte	0x4
	.set L$set$142,LCFI69-LFB44
	.long L$set$142
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$143,LCFI70-LCFI69
	.long L$set$143
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$144,LCFI71-LCFI70
	.long L$set$144
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$145,LEFDE73-LASFDE73
	.long L$set$145
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB45-.
	.set L$set$146,LFE45-LFB45
	.quad L$set$146
	.uleb128 0
	.byte	0x4
	.set L$set$147,LCFI72-LFB45
	.long L$set$147
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$148,LCFI73-LCFI72
	.long L$set$148
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$149,LCFI74-LCFI73
	.long L$set$149
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$150,LEFDE75-LASFDE75
	.long L$set$150
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB46-.
	.set L$set$151,LFE46-LFB46
	.quad L$set$151
	.uleb128 0
	.byte	0x4
	.set L$set$152,LCFI75-LFB46
	.long L$set$152
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$153,LCFI76-LCFI75
	.long L$set$153
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$154,LCFI77-LCFI76
	.long L$set$154
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$155,LEFDE77-LASFDE77
	.long L$set$155
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB48-.
	.set L$set$156,LFE48-LFB48
	.quad L$set$156
	.uleb128 0
	.byte	0x4
	.set L$set$157,LCFI78-LFB48
	.long L$set$157
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$158,LCFI79-LCFI78
	.long L$set$158
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE77:
LSFDE79:
	.set L$set$159,LEFDE79-LASFDE79
	.long L$set$159
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB50-.
	.set L$set$160,LFE50-LFB50
	.quad L$set$160
	.uleb128 0
	.byte	0x4
	.set L$set$161,LCFI80-LFB50
	.long L$set$161
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$162,LCFI81-LCFI80
	.long L$set$162
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$163,LCFI82-LCFI81
	.long L$set$163
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE79:
LSFDE81:
	.set L$set$164,LEFDE81-LASFDE81
	.long L$set$164
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB52-.
	.set L$set$165,LFE52-LFB52
	.quad L$set$165
	.uleb128 0
	.byte	0x4
	.set L$set$166,LCFI83-LFB52
	.long L$set$166
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$167,LCFI84-LCFI83
	.long L$set$167
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$168,LCFI85-LCFI84
	.long L$set$168
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE81:
LSFDE83:
	.set L$set$169,LEFDE83-LASFDE83
	.long L$set$169
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB54-.
	.set L$set$170,LFE54-LFB54
	.quad L$set$170
	.uleb128 0
	.byte	0x4
	.set L$set$171,LCFI86-LFB54
	.long L$set$171
	.byte	0xe
	.uleb128 0x170
	.byte	0x9d
	.uleb128 0x2e
	.byte	0x9e
	.uleb128 0x2d
	.byte	0x4
	.set L$set$172,LCFI87-LCFI86
	.long L$set$172
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$173,LCFI88-LCFI87
	.long L$set$173
	.byte	0x93
	.uleb128 0x2c
	.byte	0x94
	.uleb128 0x2b
	.byte	0x4
	.set L$set$174,LCFI89-LCFI88
	.long L$set$174
	.byte	0x95
	.uleb128 0x2a
	.byte	0x96
	.uleb128 0x29
	.byte	0x97
	.uleb128 0x28
	.byte	0x98
	.uleb128 0x27
	.byte	0x4
	.set L$set$175,LCFI90-LCFI89
	.long L$set$175
	.byte	0x99
	.uleb128 0x26
	.byte	0x9a
	.uleb128 0x25
	.byte	0x9b
	.uleb128 0x24
	.byte	0x9c
	.uleb128 0x23
	.byte	0x4
	.set L$set$176,LCFI91-LCFI90
	.long L$set$176
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
	.set L$set$177,LCFI92-LCFI91
	.long L$set$177
	.byte	0xb
	.align	3
LEFDE83:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
