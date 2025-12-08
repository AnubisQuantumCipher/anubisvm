	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC11:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:21"
	.align	3
lC12:
	.ascii "anubis_mldsa_encoding.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__pack_t1
_anubis_mldsa_encoding__pack_t1:
LFB2:
	stp	x29, x30, [sp, -32]!
LCFI0:
	mov	x29, sp
LCFI1:
	mov	x4, x1
	stp	x19, x20, [sp, 16]
LCFI2:
	mov	x19, x0
	mov	x20, x2
	ldp	w1, w3, [x2]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 319
	sxtw	x2, w3
	ccmp	x0, x2, 0, eq
	bne	L15
	cmp	w3, 0
	mov	w1, 0
	csinc	x2, xzr, x2, lt
	mov	x0, x4
	bl	_memset
	ldp	w10, w5, [x20]
	mov	x4, x0
	mov	w3, 0
	mov	x0, x19
	mov	x1, x4
	.p2align 5,,15
L10:
	ldp	w8, w4, [x0]
	ldp	w2, w6, [x0, 8]
	ubfiz	w9, w4, 2, 10
	ubfx	x7, x8, 8, 2
	ubfx	x4, x4, 6, 4
	orr	w7, w7, w9
	ubfiz	w9, w2, 4, 10
	ubfx	x2, x2, 4, 6
	orr	w4, w4, w9
	ubfiz	w9, w6, 6, 10
	orr	w2, w2, w9
	ubfx	x6, x6, 2, 8
	cmp	w10, w3
	bgt	L5
	cmp	w5, w3
	blt	L5
	strb	w8, [x1]
	cmp	w3, w5
	beq	L16
	add	w8, w3, 1
	strb	w7, [x1, 1]
	cmp	w5, w8
	ble	L17
	add	w7, w3, 2
	strb	w4, [x1, 2]
	cmp	w5, w7
	ble	L18
	add	w4, w3, 3
	strb	w2, [x1, 3]
	cmp	w5, w4
	ble	L19
	add	w3, w3, 5
	strb	w6, [x1, 4]
	add	x0, x0, 16
	add	x1, x1, 5
	cmp	w3, 320
	bne	L10
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI3:
	ret
L5:
LCFI4:
	adrp	x0, lC12@PAGE
	mov	w1, 41
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L19:
	adrp	x0, lC12@PAGE
	mov	w1, 45
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L18:
	adrp	x0, lC12@PAGE
	mov	w1, 44
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L17:
	adrp	x0, lC12@PAGE
	mov	w1, 43
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L16:
	adrp	x0, lC12@PAGE
	mov	w1, 42
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L15:
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE2:
	.const
	.align	2
lC0:
	.word	1
	.word	53
	.text
	.const
	.align	3
lC13:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:29"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__unpack_t1
_anubis_mldsa_encoding__unpack_t1:
LFB3:
	stp	x29, x30, [sp, -32]!
LCFI5:
	mov	x29, sp
LCFI6:
	mov	x4, x2
	stp	x19, x20, [sp, 16]
LCFI7:
	mov	x20, x1
	mov	x19, x0
	ldrsw	x2, [x1, 4]
	ldr	w1, [x1]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 319
	ccmp	x0, x2, 0, eq
	bne	L31
	mov	x2, 1024
	mov	w1, 0
	mov	x0, x4
	bl	_memset
	ldp	w11, w1, [x20]
	mov	x4, x0
	mov	w3, 0
	mov	x0, x19
	mov	x2, x4
	.p2align 5,,15
L28:
	cmp	w11, w3
	bgt	L23
	cmp	w1, w3
	blt	L23
	ldrb	w9, [x0]
	cmp	w3, w1
	beq	L32
	add	w4, w3, 1
	ldrb	w6, [x0, 1]
	cmp	w1, w4
	ble	L33
	add	w5, w3, 2
	ldrb	w4, [x0, 2]
	cmp	w1, w5
	ble	L34
	add	w7, w3, 3
	ldrb	w5, [x0, 3]
	cmp	w1, w7
	ble	L35
	ubfiz	w8, w5, 4, 6
	ubfiz	w10, w4, 6, 4
	ldrb	w7, [x0, 4]
	orr	w4, w8, w4, lsr 4
	ubfiz	w8, w6, 8, 2
	orr	w6, w10, w6, lsr 2
	orr	w8, w8, w9
	add	w3, w3, 5
	add	x0, x0, 5
	add	x2, x2, 16
	stp	w8, w6, [x2, -16]
	lsl	w6, w7, 2
	orr	w5, w6, w5, lsr 6
	stp	w4, w5, [x2, -8]
	cmp	w3, 320
	bne	L28
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI8:
	ret
L23:
LCFI9:
	adrp	x0, lC12@PAGE
	mov	w1, 64
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L35:
	adrp	x0, lC12@PAGE
	mov	w1, 68
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L34:
	adrp	x0, lC12@PAGE
	mov	w1, 67
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L33:
	adrp	x0, lC12@PAGE
	mov	w1, 66
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L32:
	adrp	x0, lC12@PAGE
	mov	w1, 65
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L31:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE3:
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:37"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__pack_t0
_anubis_mldsa_encoding__pack_t0:
LFB4:
	stp	x29, x30, [sp, -64]!
LCFI10:
	mov	x29, sp
LCFI11:
	mov	x5, x1
	stp	x19, x20, [sp, 16]
LCFI12:
	mov	x20, x0
	mov	x19, x2
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
LCFI13:
	ldp	w1, w3, [x2]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 415
	sxtw	x2, w3
	ccmp	x0, x2, 0, eq
	bne	L86
	mov	x0, x5
	cmp	w3, 0
	csinc	x2, xzr, x2, lt
	mov	w1, 0
	mov	w23, 2
	bl	_memset
	mov	x2, 33889
	ldr	w6, [x19]
	mov	x5, x0
	movk	x2, 0x6014, lsl 16
	mov	x8, 0
	movk	x2, 0x1c0, lsl 32
	mov	w16, 0
	mov	w14, 3
	mov	w15, 1
	mov	w22, 10
	mov	w13, 9
	mov	w21, 7
	mov	w12, 8
	mov	w11, 4
	mov	w30, 5
	mov	w10, 6
	mov	w9, 11
	mov	w17, 12
	movk	x2, 0x2008, lsl 48
	mov	w7, 4096
	mov	w0, -8380417
	.p2align 5,,15
L80:
	mov	w4, -1
	.p2align 5,,15
L39:
	add	w4, w4, 1
	add	x1, x8, w4, sxtw
	ldr	w24, [x20, x1, lsl 2]
	umulh	x1, x24, x2
	lsr	x1, x1, 20
	lsl	x3, x1, 10
	sub	x3, x3, x1
	add	x1, x1, x3, lsl 13
	sub	x1, x24, x1
	add	w3, w1, w0
	cmp	x1, 4190208
	sub	w3, w7, w3
	sub	w1, w7, w1
	and	w3, w3, 8191
	and	w1, w1, 8191
	csel	w1, w1, w3, ls
	cmp	w4, 3
	beq	L42
	bgt	L43
	cmp	w4, 1
	beq	L44
	cmp	w4, 2
	beq	L87
	cmp	w6, w16
	bgt	L51
	ldr	w3, [x19, 4]
	cmp	w3, w16
	blt	L51
	strb	w1, [x5]
	cmp	w3, w15
	blt	L88
	lsr	w1, w1, 8
	mov	w4, 0
	strb	w1, [x5, 1]
	b	L39
	.p2align 2,,3
L43:
	cmp	w4, 5
	beq	L47
	cmp	w4, 6
	beq	L48
	cmp	w4, 4
	bne	L89
	cmp	w6, w10
	bgt	L66
	ldr	w24, [x19, 4]
	cmp	w24, w10
	blt	L66
	ldrb	w3, [x5, 6]
	cmp	w24, w21
	ccmp	w6, w21, 0, ge
	orr	w3, w3, w1, lsl 4
	strb	w3, [x5, 6]
	bgt	L90
	lsr	w3, w1, 4
	cmp	w24, w12
	ccmp	w6, w12, 0, ge
	strb	w3, [x5, 7]
	bgt	L91
	lsr	w1, w1, 12
	strb	w1, [x5, 8]
	b	L39
	.p2align 2,,3
L42:
	cmp	w6, w11
	bgt	L62
	ldr	w24, [x19, 4]
	cmp	w24, w11
	blt	L62
	ldrb	w3, [x5, 4]
	cmp	w6, w30
	ccmp	w24, w30, 1, le
	orr	w3, w3, w1, lsl 7
	strb	w3, [x5, 4]
	blt	L92
	lsr	w3, w1, 1
	cmp	w24, w10
	ccmp	w6, w10, 0, ge
	strb	w3, [x5, 5]
	bgt	L93
	lsr	w1, w1, 9
	strb	w1, [x5, 6]
	b	L39
	.p2align 2,,3
L89:
	cmp	w6, w9
	bgt	L77
	ldr	w4, [x19, 4]
	cmp	w4, w9
	blt	L77
	ldrb	w3, [x5, 11]
	cmp	w4, w17
	ccmp	w6, w17, 0, ge
	orr	w3, w3, w1, lsl 3
	strb	w3, [x5, 11]
	bgt	L94
	lsr	w1, w1, 5
	add	w9, w9, 13
	add	w17, w17, 13
	add	w16, w16, 13
	strb	w1, [x5, 12]
	add	x8, x8, 8
	add	x5, x5, 13
	add	w10, w10, 13
	add	w30, w30, 13
	add	w11, w11, 13
	add	w12, w12, 13
	add	w21, w21, 13
	add	w13, w13, 13
	add	w22, w22, 13
	add	w15, w15, 13
	add	w14, w14, 13
	add	w23, w23, 13
	cmp	w9, 427
	bne	L80
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 64
LCFI14:
	ret
	.p2align 2,,3
L48:
LCFI15:
	cmp	w6, w13
	bgt	L73
	ldr	w24, [x19, 4]
	cmp	w24, w13
	blt	L73
	ldrb	w3, [x5, 9]
	cmp	w24, w22
	ccmp	w6, w22, 0, ge
	orr	w3, w3, w1, lsl 6
	strb	w3, [x5, 9]
	bgt	L95
	lsr	w3, w1, 2
	cmp	w24, w9
	ccmp	w6, w9, 0, ge
	strb	w3, [x5, 10]
	bgt	L96
	lsr	w1, w1, 10
	strb	w1, [x5, 11]
	b	L39
	.p2align 2,,3
L47:
	cmp	w6, w12
	bgt	L70
	ldr	w24, [x19, 4]
	cmp	w24, w12
	blt	L70
	ldrb	w3, [x5, 8]
	cmp	w24, w13
	ccmp	w6, w13, 0, ge
	orr	w3, w3, w1, lsl 1
	strb	w3, [x5, 8]
	bgt	L97
	lsr	w1, w1, 7
	strb	w1, [x5, 9]
	b	L39
	.p2align 2,,3
L87:
	cmp	w6, w14
	bgt	L59
	ldr	w24, [x19, 4]
	cmp	w24, w14
	blt	L59
	ldrb	w3, [x5, 3]
	cmp	w24, w11
	ccmp	w6, w11, 0, ge
	orr	w3, w3, w1, lsl 2
	strb	w3, [x5, 3]
	bgt	L98
	lsr	w1, w1, 6
	strb	w1, [x5, 4]
	b	L39
	.p2align 2,,3
L44:
	cmp	w6, w15
	bgt	L55
	ldr	w24, [x19, 4]
	cmp	w24, w15
	blt	L55
	ldrb	w3, [x5, 1]
	cmp	w6, w23
	ccmp	w24, w23, 1, le
	orr	w3, w3, w1, lsl 5
	strb	w3, [x5, 1]
	blt	L99
	lsr	w3, w1, 3
	cmp	w6, w14
	ccmp	w24, w14, 1, le
	strb	w3, [x5, 2]
	blt	L100
	lsr	w1, w1, 11
	strb	w1, [x5, 3]
	b	L39
L51:
	adrp	x0, lC12@PAGE
	mov	w1, 110
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L100:
	adrp	x0, lC12@PAGE
	mov	w1, 115
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L99:
	adrp	x0, lC12@PAGE
	mov	w1, 114
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L55:
	adrp	x0, lC12@PAGE
	mov	w1, 113
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L98:
	adrp	x0, lC12@PAGE
	mov	w1, 118
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L59:
	adrp	x0, lC12@PAGE
	mov	w1, 117
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L97:
	adrp	x0, lC12@PAGE
	mov	w1, 129
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L70:
	adrp	x0, lC12@PAGE
	mov	w1, 128
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L96:
	adrp	x0, lC12@PAGE
	mov	w1, 133
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L95:
	adrp	x0, lC12@PAGE
	mov	w1, 132
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L73:
	adrp	x0, lC12@PAGE
	mov	w1, 131
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L94:
	adrp	x0, lC12@PAGE
	mov	w1, 136
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L77:
	adrp	x0, lC12@PAGE
	mov	w1, 135
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L93:
	adrp	x0, lC12@PAGE
	mov	w1, 122
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L92:
	adrp	x0, lC12@PAGE
	mov	w1, 121
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L62:
	adrp	x0, lC12@PAGE
	mov	w1, 120
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L91:
	adrp	x0, lC12@PAGE
	mov	w1, 126
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L90:
	adrp	x0, lC12@PAGE
	mov	w1, 125
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L66:
	adrp	x0, lC12@PAGE
	mov	w1, 124
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L88:
	adrp	x0, lC12@PAGE
	mov	w1, 111
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L86:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE4:
	.const
	.align	3
lC15:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:45"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__unpack_t0
_anubis_mldsa_encoding__unpack_t0:
LFB5:
	stp	x29, x30, [sp, -48]!
LCFI16:
	mov	x29, sp
LCFI17:
	mov	x6, x2
	stp	x19, x20, [sp, 16]
LCFI18:
	mov	x19, x1
	mov	x20, x0
	stp	x21, x22, [sp, 32]
LCFI19:
	ldrsw	x2, [x1, 4]
	ldr	w1, [x1]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 415
	ccmp	x0, x2, 0, eq
	bne	L152
	mov	x2, 1024
	mov	x0, x6
	mov	w1, 0
	bl	_memset
	mov	w7, 61441
	ldp	w4, w3, [x19]
	mov	x2, x20
	mov	x6, x0
	mov	w16, 0
	mov	w14, 1
	mov	w20, 2
	mov	w13, 3
	mov	w12, 4
	mov	w19, 5
	mov	w11, 6
	mov	w30, 7
	mov	w10, 8
	mov	w9, 9
	mov	w17, 10
	mov	w8, 11
	mov	w15, 12
	mov	w5, 4096
	movk	w7, 0x7f, lsl 16
	.p2align 5,,15
L146:
	mov	x1, 0
	b	L103
	.p2align 2,,3
L155:
	cmp	w1, 1
	beq	L106
	cmp	w1, 2
	beq	L153
	cmp	w4, w16
	bgt	L113
	cmp	w3, w16
	blt	L113
	cmp	w3, w14
	blt	L154
	ldrb	w0, [x2, 1]
	ldrb	w21, [x2]
	ubfiz	w0, w0, 8, 5
	orr	w0, w0, w21
	.p2align 5,,15
L116:
	sub	w21, w7, w0
	subs	w0, w5, w0
	csel	w0, w21, w0, mi
	str	w0, [x6, x1, lsl 2]
	add	x1, x1, 1
L103:
	cmp	w1, 3
	beq	L104
	ble	L155
	cmp	w1, 5
	beq	L109
	cmp	w1, 6
	beq	L110
	cmp	w1, 4
	bne	L156
	cmp	w4, w11
	bgt	L128
	cmp	w3, w11
	blt	L128
	cmp	w4, w30
	ccmp	w3, w30, 1, le
	blt	L157
	cmp	w4, w10
	ccmp	w3, w10, 1, le
	blt	L158
	ldrb	w22, [x2, 7]
	ldrb	w0, [x2, 6]
	ldrb	w21, [x2, 8]
	lsl	w22, w22, 4
	orr	w0, w22, w0, lsr 4
	ubfiz	w21, w21, 12, 1
	orr	w0, w0, w21
	b	L116
	.p2align 2,,3
L104:
	cmp	w4, w12
	bgt	L124
	cmp	w3, w12
	blt	L124
	cmp	w4, w19
	ccmp	w3, w19, 1, le
	blt	L159
	cmp	w4, w11
	ccmp	w3, w11, 1, le
	blt	L160
	ldrb	w22, [x2, 5]
	ldrb	w0, [x2, 4]
	ldrb	w21, [x2, 6]
	lsl	w22, w22, 1
	orr	w0, w22, w0, lsr 7
	ubfiz	w21, w21, 9, 4
	orr	w0, w0, w21
	b	L116
	.p2align 2,,3
L156:
	cmp	w4, w8
	bgt	L139
	cmp	w3, w8
	blt	L139
	cmp	w3, w15
	ccmp	w4, w15, 0, ge
	bgt	L161
	ldrb	w1, [x2, 12]
	add	w15, w15, 13
	add	w16, w16, 13
	add	w8, w8, 13
	add	w17, w17, 13
	ldrb	w0, [x2, 11]
	add	w9, w9, 13
	add	w10, w10, 13
	add	w30, w30, 13
	add	w11, w11, 13
	add	w19, w19, 13
	add	w12, w12, 13
	lsl	w1, w1, 5
	add	w13, w13, 13
	add	w20, w20, 13
	add	w14, w14, 13
	orr	w0, w1, w0, lsr 3
	add	x2, x2, 13
	add	x6, x6, 32
	sub	w1, w7, w0
	subs	w0, w5, w0
	csel	w0, w1, w0, mi
	str	w0, [x6, -4]
	cmp	w15, 428
	bne	L146
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI20:
	ret
	.p2align 2,,3
L110:
LCFI21:
	cmp	w4, w9
	bgt	L135
	cmp	w3, w9
	blt	L135
	cmp	w3, w17
	ccmp	w4, w17, 0, ge
	bgt	L162
	cmp	w3, w8
	ccmp	w4, w8, 0, ge
	bgt	L163
	ldrb	w22, [x2, 10]
	ldrb	w0, [x2, 9]
	ldrb	w21, [x2, 11]
	lsl	w22, w22, 2
	orr	w0, w22, w0, lsr 6
	ubfiz	w21, w21, 10, 3
	orr	w0, w0, w21
	b	L116
	.p2align 2,,3
L109:
	cmp	w4, w10
	bgt	L132
	cmp	w3, w10
	blt	L132
	cmp	w3, w9
	ccmp	w4, w9, 0, ge
	bgt	L164
	ldrb	w21, [x2, 9]
	ldrb	w0, [x2, 8]
	ubfiz	w21, w21, 7, 6
	orr	w0, w21, w0, lsr 1
	b	L116
	.p2align 2,,3
L153:
	cmp	w4, w13
	bgt	L121
	cmp	w3, w13
	blt	L121
	cmp	w4, w12
	ccmp	w3, w12, 1, le
	blt	L165
	ldrb	w21, [x2, 4]
	ldrb	w0, [x2, 3]
	ubfiz	w21, w21, 6, 7
	orr	w0, w21, w0, lsr 2
	b	L116
	.p2align 2,,3
L106:
	cmp	w4, w14
	bgt	L117
	cmp	w3, w14
	blt	L117
	cmp	w3, w20
	ccmp	w4, w20, 0, ge
	bgt	L166
	cmp	w4, w13
	ccmp	w3, w13, 1, le
	blt	L167
	ldrb	w22, [x2, 2]
	ldrb	w0, [x2, 1]
	ldrb	w21, [x2, 3]
	lsl	w22, w22, 3
	orr	w0, w22, w0, lsr 5
	ubfiz	w21, w21, 11, 2
	orr	w0, w0, w21
	b	L116
L113:
	adrp	x0, lC12@PAGE
	mov	w1, 164
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L167:
	adrp	x0, lC12@PAGE
	mov	w1, 169
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L166:
	adrp	x0, lC12@PAGE
	mov	w1, 168
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L117:
	adrp	x0, lC12@PAGE
	mov	w1, 167
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L165:
	adrp	x0, lC12@PAGE
	mov	w1, 172
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L121:
	adrp	x0, lC12@PAGE
	mov	w1, 171
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L164:
	adrp	x0, lC12@PAGE
	mov	w1, 183
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L132:
	adrp	x0, lC12@PAGE
	mov	w1, 182
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L163:
	adrp	x0, lC12@PAGE
	mov	w1, 187
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L162:
	adrp	x0, lC12@PAGE
	mov	w1, 186
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L135:
	adrp	x0, lC12@PAGE
	mov	w1, 185
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L161:
	adrp	x0, lC12@PAGE
	mov	w1, 190
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L139:
	adrp	x0, lC12@PAGE
	mov	w1, 189
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L160:
	adrp	x0, lC12@PAGE
	mov	w1, 176
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L159:
	adrp	x0, lC12@PAGE
	mov	w1, 175
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L124:
	adrp	x0, lC12@PAGE
	mov	w1, 174
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L158:
	adrp	x0, lC12@PAGE
	mov	w1, 180
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L157:
	adrp	x0, lC12@PAGE
	mov	w1, 179
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L128:
	adrp	x0, lC12@PAGE
	mov	w1, 178
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L154:
	adrp	x0, lC12@PAGE
	mov	w1, 165
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L152:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE5:
	.const
	.align	3
lC16:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:53"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__pack_eta
_anubis_mldsa_encoding__pack_eta:
LFB6:
	stp	x29, x30, [sp, -48]!
LCFI22:
	mov	x29, sp
LCFI23:
	mov	x11, x1
	stp	x19, x20, [sp, 16]
LCFI24:
	mov	x20, x0
	mov	x19, x2
	ldp	w1, w3, [x2]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 95
	sxtw	x2, w3
	ccmp	x0, x2, 0, eq
	bne	L183
	mov	x0, x11
	cmp	w3, 0
	csinc	x2, xzr, x2, lt
	mov	w1, 0
	bl	_memset
	mov	x9, 33889
	mov	x11, x0
	movk	x9, 0x6014, lsl 16
	mov	w2, 0
	movk	x9, 0x1c0, lsl 32
	add	x6, x20, 4
	add	x10, x29, 40
	movk	x9, 0x2008, lsl 48
	mov	w8, -8380417
	mov	w7, 2
	.p2align 5,,15
L178:
	mov	x1, -1
	.p2align 5,,15
L173:
	ldr	w4, [x6, x1, lsl 2]
	add	x5, x1, x10
	add	x1, x1, 1
	umulh	x0, x4, x9
	lsr	x0, x0, 20
	lsl	x3, x0, 10
	sub	x3, x3, x0
	add	x0, x0, x3, lsl 13
	sub	x0, x4, x0
	cmp	x0, 4190208
	add	w3, w8, w0
	csel	w0, w0, w3, ls
	sub	w0, w7, w0
	and	w0, w0, 7
	strb	w0, [x5, 1]
	cmp	x1, 7
	bne	L173
	ldr	w0, [x19]
	cmp	w0, w2
	bgt	L174
	ldr	w5, [x19, 4]
	cmp	w5, w2
	blt	L174
	ldrb	w0, [x29, 41]
	add	w1, w2, 1
	ldrb	w3, [x29, 42]
	ldrb	w4, [x29, 40]
	ubfiz	w0, w0, 3, 5
	orr	w0, w0, w3, lsl 6
	orr	w0, w0, w4
	strb	w0, [x11]
	cmp	w5, w1
	blt	L184
	ldrb	w12, [x29, 43]
	add	w4, w2, 2
	ldrb	w0, [x29, 44]
	ldrb	w1, [x29, 45]
	ubfiz	w12, w12, 1, 7
	orr	w3, w12, w3, lsr 2
	ubfiz	w0, w0, 4, 4
	orr	w0, w0, w1, lsl 7
	orr	w0, w0, w3
	strb	w0, [x11, 1]
	cmp	w5, w4
	blt	L185
	ldrb	w3, [x29, 46]
	add	w2, w2, 3
	add	x6, x6, 32
	add	x11, x11, 3
	ldrb	w0, [x29, 47]
	ubfiz	w3, w3, 2, 6
	orr	w1, w3, w1, lsr 1
	ubfiz	w0, w0, 5, 3
	orr	w1, w1, w0
	strb	w1, [x11, -1]
	cmp	w2, 96
	bne	L178
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI25:
	ret
L174:
LCFI26:
	adrp	x0, lC12@PAGE
	mov	w1, 238
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L183:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L185:
	adrp	x0, lC12@PAGE
	mov	w1, 241
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L184:
	adrp	x0, lC12@PAGE
	mov	w1, 239
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE6:
	.const
	.align	3
lC17:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:61"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__unpack_eta
_anubis_mldsa_encoding__unpack_eta:
LFB7:
	stp	x29, x30, [sp, -32]!
LCFI27:
	mov	x29, sp
LCFI28:
	mov	x3, x2
	stp	x19, x20, [sp, 16]
LCFI29:
	mov	x19, x1
	mov	x20, x0
	ldrsw	x2, [x1, 4]
	ldr	w1, [x1]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 95
	ccmp	x0, x2, 0, eq
	bne	L203
	mov	x2, 1024
	mov	x0, x3
	mov	w1, 0
	bl	_memset
	ldp	w11, w10, [x19]
	mov	w6, 57347
	mov	x2, x0
	mov	w7, 0
	mov	x0, x20
	mov	w3, 2
	movk	w6, 0x7f, lsl 16
	.p2align 5,,15
L200:
	cmp	w11, w7
	bgt	L189
	cmp	w10, w7
	blt	L189
	add	w1, w7, 1
	ldrb	w5, [x0]
	cmp	w10, w1
	blt	L204
	add	w1, w7, 2
	ldrb	w4, [x0, 1]
	cmp	w10, w1
	blt	L205
	ldrb	w1, [x0, 2]
	and	w9, w5, 7
	ubfiz	w13, w4, 2, 1
	ubfx	x8, x5, 3, 3
	subs	w12, w3, w9
	orr	w5, w13, w5, lsr 6
	sub	w9, w6, w9
	csel	w12, w9, w12, mi
	subs	w15, w3, w8
	sub	w9, w6, w8
	csel	w15, w9, w15, mi
	subs	w13, w3, w5
	sub	w5, w6, w5
	ubfx	x8, x4, 1, 3
	csel	w13, w5, w13, mi
	ubfiz	w5, w1, 1, 2
	subs	w14, w3, w8
	stp	w12, w15, [x2]
	orr	w5, w5, w4, lsr 7
	ubfx	x4, x4, 4, 3
	sub	w8, w6, w8
	csel	w14, w8, w14, mi
	subs	w9, w3, w4
	ubfx	x8, x1, 2, 3
	sub	w4, w6, w4
	csel	w9, w4, w9, mi
	subs	w12, w3, w5
	lsr	w1, w1, 5
	sub	w5, w6, w5
	csel	w12, w5, w12, mi
	subs	w4, w3, w8
	sub	w5, w6, w1
	stp	w13, w14, [x2, 8]
	sub	w8, w6, w8
	add	w7, w7, 3
	csel	w4, w8, w4, mi
	subs	w1, w3, w1
	csel	w1, w5, w1, mi
	stp	w9, w12, [x2, 16]
	stp	w4, w1, [x2, 24]
	add	x0, x0, 3
	add	x2, x2, 32
	cmp	w7, 96
	bne	L200
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI30:
	ret
L189:
LCFI31:
	adrp	x0, lC12@PAGE
	mov	w1, 264
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L205:
	adrp	x0, lC12@PAGE
	mov	w1, 266
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L204:
	adrp	x0, lC12@PAGE
	mov	w1, 265
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L203:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE7:
	.const
	.align	3
lC18:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:69"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__pack_z
_anubis_mldsa_encoding__pack_z:
LFB8:
	stp	x29, x30, [sp, -32]!
LCFI32:
	mov	x29, sp
LCFI33:
	mov	x3, x1
	stp	x19, x20, [sp, 16]
LCFI34:
	mov	x20, x0
	mov	x19, x2
	ldp	w1, w4, [x2]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 639
	sxtw	x2, w4
	ccmp	x0, x2, 0, eq
	bne	L221
	cmp	w4, 0
	mov	w1, 0
	csinc	x2, xzr, x2, lt
	mov	x0, x3
	bl	_memset
	mov	x10, 33889
	ldr	w11, [x19]
	mov	w4, 0
	movk	x10, 0x6014, lsl 16
	mov	x6, x20
	movk	x10, 0x1c0, lsl 32
	mov	w9, -8380417
	movk	x10, 0x2008, lsl 48
	mov	w8, 524288
	.p2align 5,,15
L217:
	ldp	w12, w2, [x6]
	umulh	x1, x12, x10
	umulh	x3, x2, x10
	lsr	x1, x1, 20
	lsr	x3, x3, 20
	lsl	x7, x1, 10
	lsl	x5, x3, 10
	sub	x7, x7, x1
	sub	x5, x5, x3
	add	x1, x1, x7, lsl 13
	add	x3, x3, x5, lsl 13
	sub	x1, x12, x1
	sub	x3, x2, x3
	cmp	x1, 4190208
	add	w5, w9, w1
	csel	w1, w1, w5, ls
	cmp	x3, 4190208
	sub	w1, w8, w1
	add	w2, w9, w3
	csel	w3, w3, w2, ls
	and	w1, w1, 1048575
	sub	w3, w8, w3
	cmp	w11, w4
	bgt	L211
	ldr	w5, [x19, 4]
	cmp	w5, w4
	blt	L211
	add	w2, w4, 1
	strb	w1, [x0]
	cmp	w5, w2
	blt	L222
	lsr	w7, w1, 8
	add	w2, w4, 2
	strb	w7, [x0, 1]
	cmp	w5, w2
	blt	L223
	ubfiz	w7, w3, 4, 4
	add	w2, w4, 3
	orr	w1, w7, w1, lsr 16
	and	w3, w3, 1048575
	strb	w1, [x0, 2]
	cmp	w5, w2
	blt	L224
	lsr	w2, w3, 4
	add	w1, w4, 4
	strb	w2, [x0, 3]
	cmp	w5, w1
	blt	L225
	lsr	w3, w3, 12
	add	w4, w4, 5
	add	x6, x6, 8
	add	x0, x0, 5
	strb	w3, [x0, -1]
	cmp	w4, 640
	bne	L217
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI35:
	ret
L211:
LCFI36:
	adrp	x0, lC12@PAGE
	mov	w1, 339
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L222:
	adrp	x0, lC12@PAGE
	mov	w1, 340
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L225:
	adrp	x0, lC12@PAGE
	mov	w1, 346
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L224:
	adrp	x0, lC12@PAGE
	mov	w1, 345
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L223:
	adrp	x0, lC12@PAGE
	mov	w1, 341
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L221:
	adrp	x0, lC18@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE8:
	.const
	.align	3
lC19:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:77"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__unpack_z
_anubis_mldsa_encoding__unpack_z:
LFB9:
	stp	x29, x30, [sp, -32]!
LCFI37:
	mov	x29, sp
LCFI38:
	mov	x4, x2
	stp	x19, x20, [sp, 16]
LCFI39:
	mov	x20, x1
	mov	x19, x0
	ldrsw	x2, [x1, 4]
	ldr	w1, [x1]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 639
	ccmp	x0, x2, 0, eq
	bne	L239
	mov	x2, 1024
	mov	x0, x4
	mov	w1, 0
	bl	_memset
	ldp	w10, w4, [x20]
	mov	w8, 57345
	mov	w3, 0
	mov	x2, x19
	mov	w6, 524288
	movk	w8, 0x87, lsl 16
	.p2align 5,,15
L236:
	cmp	w10, w3
	bgt	L228
	cmp	w4, w3
	blt	L228
	add	w1, w3, 1
	cmp	w4, w1
	blt	L240
	add	w1, w3, 2
	cmp	w4, w1
	blt	L241
	ldrb	w9, [x2]
	add	w7, w3, 3
	ldrb	w1, [x2, 1]
	ldrb	w5, [x2, 2]
	orr	w1, w9, w1, lsl 8
	ubfiz	w9, w5, 16, 4
	orr	w1, w1, w9
	sub	w9, w8, w1
	subs	w1, w6, w1
	csel	w1, w9, w1, mi
	str	w1, [x0]
	cmp	w4, w7
	blt	L242
	add	w1, w3, 4
	cmp	w4, w1
	blt	L243
	ldrb	w7, [x2, 3]
	add	w3, w3, 5
	add	x2, x2, 5
	add	x0, x0, 8
	ldrb	w1, [x2, -1]
	lsl	w7, w7, 4
	orr	w5, w7, w5, lsr 4
	orr	w1, w5, w1, lsl 12
	sub	w5, w8, w1
	subs	w1, w6, w1
	csel	w1, w5, w1, mi
	str	w1, [x0, -4]
	cmp	w3, 640
	bne	L236
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI40:
	ret
L228:
LCFI41:
	adrp	x0, lC12@PAGE
	mov	w1, 367
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L240:
	adrp	x0, lC12@PAGE
	mov	w1, 368
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L243:
	adrp	x0, lC12@PAGE
	mov	w1, 379
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L242:
	adrp	x0, lC12@PAGE
	mov	w1, 378
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L241:
	adrp	x0, lC12@PAGE
	mov	w1, 369
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L239:
	adrp	x0, lC19@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE9:
	.const
	.align	3
lC20:
	.ascii "failed precondition from anubis_mldsa_encoding.ads:85"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__pack_w1
_anubis_mldsa_encoding__pack_w1:
LFB10:
	stp	x29, x30, [sp, -32]!
LCFI42:
	mov	x29, sp
LCFI43:
	mov	x5, x1
	stp	x19, x20, [sp, 16]
LCFI44:
	mov	x19, x0
	mov	x20, x2
	ldp	w1, w3, [x2]
	sxtw	x0, w1
	cmp	w1, 0
	add	x0, x0, 127
	sxtw	x2, w3
	ccmp	x0, x2, 0, eq
	bne	L253
	cmp	w3, 0
	mov	w1, 0
	csinc	x2, xzr, x2, lt
	mov	x0, x5
	bl	_memset
	ldp	w1, w6, [x20]
	mov	x5, x0
	mov	x3, 0
	mov	x4, x19
	.p2align 5,,15
L249:
	ldp	w2, w0, [x4]
	and	w2, w2, 15
	cmp	w1, w3
	bgt	L247
	cmp	w6, w3
	blt	L247
	ubfiz	w0, w0, 4, 4
	add	x4, x4, 8
	orr	w0, w0, w2
	strb	w0, [x5, x3]
	add	x3, x3, 1
	cmp	x3, 128
	bne	L249
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI45:
	ret
L247:
LCFI46:
	adrp	x0, lC12@PAGE
	mov	w1, 406
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L253:
	adrp	x0, lC20@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__pack_public_key
_anubis_mldsa_encoding__pack_public_key:
LFB11:
	stp	x29, x30, [sp, -384]!
LCFI47:
	mov	x29, sp
LCFI48:
	stp	x19, x20, [sp, 16]
LCFI49:
	mov	x19, x0
	mov	x0, x2
	mov	x2, 2592
	stp	x21, x22, [sp, 32]
LCFI50:
	mov	x21, x1
	mov	w1, 0
	str	x23, [sp, 48]
LCFI51:
	bl	_memset
	mov	x3, x0
	mov	x0, 0
	.p2align 5,,15
L255:
	ldrb	w1, [x19, x0]
	strb	w1, [x3, x0]
	add	x0, x0, 1
	cmp	x0, 32
	bne	L255
	adrp	x23, lC7@PAGE
	add	x20, x3, 32
	add	x23, x23, lC7@PAGEOFF;
	add	x19, x3, 2592
	add	x22, x29, 64
	.p2align 5,,15
L256:
	mov	x0, x21
	mov	x1, x22
	mov	x2, x23
	add	x21, x21, 1024
	bl	_anubis_mldsa_encoding__pack_t1
	mov	x0, x20
	mov	x1, x22
	mov	x2, 320
	add	x20, x20, 320
	bl	_memcpy
	cmp	x19, x20
	bne	L256
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 384
LCFI52:
	ret
LFE11:
	.const
	.align	2
lC7:
	.word	0
	.word	319
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__unpack_public_key
_anubis_mldsa_encoding__unpack_public_key:
LFB12:
	mov	x3, 0
	mov	x4, x0
	stp	x29, x30, [sp, -384]!
LCFI53:
	mov	x29, sp
LCFI54:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI55:
	.p2align 5,,15
L261:
	ldrb	w0, [x4, x3]
	strb	w0, [x1, x3]
	add	x3, x3, 1
	cmp	x3, 32
	bne	L261
	adrp	x22, lC7@PAGE
	mov	x20, x2
	add	x19, x4, 32
	add	x23, x2, 8192
	add	x22, x22, lC7@PAGEOFF;
	add	x21, x29, 64
	.p2align 5,,15
L262:
	mov	x1, x19
	mov	x2, 320
	mov	x0, x21
	add	x19, x19, 320
	bl	_memcpy
	mov	x2, x20
	mov	x0, x21
	mov	x1, x22
	add	x20, x20, 1024
	bl	_anubis_mldsa_encoding__unpack_t1
	cmp	x23, x20
	bne	L262
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 384
LCFI56:
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__pack_secret_key
_anubis_mldsa_encoding__pack_secret_key:
LFB13:
	sub	sp, sp, #592
LCFI57:
	stp	x29, x30, [sp]
LCFI58:
	mov	x29, sp
LCFI59:
	stp	x19, x20, [sp, 16]
LCFI60:
	mov	x20, x2
	mov	x2, 4896
	mov	x19, x6
	stp	x23, x24, [sp, 48]
LCFI61:
	mov	x24, x0
	mov	x0, x6
	mov	x23, x4
	stp	x25, x26, [sp, 64]
LCFI62:
	mov	x25, x1
	mov	w1, 0
	stp	x21, x22, [sp, 32]
LCFI63:
	mov	x21, x3
	mov	x22, x5
	bl	_memset
	mov	x0, 0
	.p2align 5,,15
L267:
	ldrb	w1, [x24, x0]
	strb	w1, [x19, x0]
	add	x0, x0, 1
	cmp	x0, 32
	bne	L267
	mov	x0, 0
	add	x2, x19, 32
	.p2align 5,,15
L268:
	ldrb	w1, [x25, x0]
	strb	w1, [x2, x0]
	add	x0, x0, 1
	cmp	x0, 32
	bne	L268
	mov	x0, 0
	add	x2, x19, 64
	.p2align 5,,15
L269:
	ldrb	w1, [x20, x0]
	strb	w1, [x2, x0]
	add	x0, x0, 1
	cmp	x0, 64
	bne	L269
	adrp	x24, lC8@PAGE
	add	x20, x19, 128
	add	x24, x24, lC8@PAGEOFF;
	add	x26, x19, 800
	add	x25, x29, 80
	.p2align 5,,15
L270:
	mov	x0, x21
	mov	x1, x25
	mov	x2, x24
	add	x20, x20, 96
	bl	_anubis_mldsa_encoding__pack_eta
	ldp	q31, q30, [x25]
	add	x21, x21, 1024
	ldp	q29, q28, [x25, 32]
	stp	q31, q30, [x20, -96]
	ldp	q31, q30, [x25, 64]
	stp	q29, q28, [x20, -64]
	stp	q31, q30, [x20, -32]
	cmp	x20, x26
	bne	L270
	mov	w20, 800
	mov	w21, 4895
L273:
	mov	x2, x24
	mov	x1, x25
	mov	x0, x23
	bl	_anubis_mldsa_encoding__pack_eta
	mov	x4, x25
	mov	w2, -1
	.p2align 5,,15
L272:
	add	w2, w2, 1
	add	w3, w20, w2
	cmp	w3, w21
	bgt	L286
	ldrb	w5, [x4], 1
	strb	w5, [x19, w3, sxtw]
	cmp	w2, 95
	bne	L272
	add	w20, w20, 96
	add	x23, x23, 1024
	cmp	w20, 1568
	bne	L273
	adrp	x23, lC9@PAGE
	add	x25, x29, 176
	add	x23, x23, lC9@PAGEOFF;
	mov	w21, 4895
	mov	w24, 4896
L276:
	mov	x2, x23
	mov	x1, x25
	mov	x0, x22
	bl	_anubis_mldsa_encoding__pack_t0
	mov	x4, x25
	mov	w2, -1
	.p2align 5,,15
L275:
	add	w2, w2, 1
	add	w3, w20, w2
	cmp	w3, w21
	bgt	L287
	ldrb	w5, [x4], 1
	strb	w5, [x19, w3, sxtw]
	cmp	w2, 415
	bne	L275
	add	w20, w20, 416
	add	x22, x22, 1024
	cmp	w20, w24
	bne	L276
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, 592
LCFI64:
	ret
L286:
LCFI65:
	adrp	x0, lC12@PAGE
	mov	w1, 509
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L287:
	adrp	x0, lC12@PAGE
	mov	w1, 518
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE13:
	.const
	.align	2
lC8:
	.word	0
	.word	95
	.align	2
lC9:
	.word	0
	.word	415
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__unpack_secret_key
_anubis_mldsa_encoding__unpack_secret_key:
LFB14:
	sub	sp, sp, #592
LCFI66:
	mov	x8, x2
	mov	x7, 0
	mov	x2, x3
	stp	x29, x30, [sp]
LCFI67:
	mov	x29, sp
LCFI68:
	stp	x19, x20, [sp, 16]
LCFI69:
	mov	x19, x0
	mov	x20, x5
	stp	x21, x22, [sp, 32]
LCFI70:
	mov	x22, x6
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI71:
	.p2align 5,,15
L289:
	ldrb	w0, [x19, x7]
	strb	w0, [x1, x7]
	add	x7, x7, 1
	cmp	x7, 32
	bne	L289
	mov	x0, 0
	add	x3, x19, 32
	.p2align 5,,15
L290:
	ldrb	w1, [x3, x0]
	strb	w1, [x8, x0]
	add	x0, x0, 1
	cmp	x0, 32
	bne	L290
	mov	x0, 0
	add	x3, x19, 64
	.p2align 5,,15
L291:
	ldrb	w1, [x3, x0]
	strb	w1, [x2, x0]
	add	x0, x0, 1
	cmp	x0, 64
	bne	L291
	adrp	x23, lC8@PAGE
	mov	x0, 7168
	mov	x25, x4
	add	x26, x4, x0
	add	x23, x23, lC8@PAGEOFF;
	add	x21, x19, 128
	add	x24, x29, 80
	.p2align 5,,15
L292:
	ldp	q31, q30, [x21]
	mov	x2, x25
	mov	x0, x24
	mov	x1, x23
	add	x25, x25, 1024
	ldp	q29, q28, [x21, 32]
	stp	q31, q30, [x24]
	ldp	q31, q30, [x21, 64]
	add	x21, x21, 96
	stp	q29, q28, [x24, 32]
	stp	q31, q30, [x24, 64]
	bl	_anubis_mldsa_encoding__unpack_eta
	cmp	x25, x26
	bne	L292
	mov	x25, x20
	mov	w21, 4895
	mov	w20, 800
L295:
	mov	x4, x24
	mov	w1, -1
	.p2align 5,,15
L294:
	add	w1, w1, 1
	add	w3, w20, w1
	cmp	w3, w21
	bgt	L308
	ldrb	w3, [x19, w3, sxtw]
	strb	w3, [x4], 1
	cmp	w1, 95
	bne	L294
	mov	x2, x25
	mov	x0, x24
	mov	x1, x23
	add	w20, w20, 96
	bl	_anubis_mldsa_encoding__unpack_eta
	add	x25, x25, 1024
	cmp	w20, 1568
	bne	L295
	adrp	x23, lC9@PAGE
	add	x25, x29, 176
	add	x23, x23, lC9@PAGEOFF;
	mov	w21, 4895
	mov	w24, 4896
L298:
	mov	x4, x25
	mov	w1, -1
	.p2align 5,,15
L297:
	add	w1, w1, 1
	add	w3, w20, w1
	cmp	w3, w21
	bgt	L309
	ldrb	w3, [x19, w3, sxtw]
	strb	w3, [x4], 1
	cmp	w1, 415
	bne	L297
	mov	x2, x22
	mov	x0, x25
	mov	x1, x23
	add	w20, w20, 416
	bl	_anubis_mldsa_encoding__unpack_t0
	add	x22, x22, 1024
	cmp	w20, w24
	bne	L298
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, 592
LCFI72:
	ret
L308:
LCFI73:
	adrp	x0, lC12@PAGE
	mov	w1, 569
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L309:
	adrp	x0, lC12@PAGE
	mov	w1, 578
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__pack_signature
_anubis_mldsa_encoding__pack_signature:
LFB15:
	sub	sp, sp, #720
LCFI74:
	stp	x29, x30, [sp]
LCFI75:
	mov	x29, sp
LCFI76:
	stp	x19, x20, [sp, 16]
LCFI77:
	mov	x19, x2
	mov	x2, 4627
	mov	x20, x3
	stp	x21, x22, [sp, 32]
LCFI78:
	mov	x21, x0
	mov	x22, x1
	mov	x0, x3
	mov	w1, 0
	stp	x23, x24, [sp, 48]
	str	x25, [sp, 64]
LCFI79:
	bl	_memset
	mov	x0, 0
	.p2align 5,,15
L311:
	ldrb	w1, [x21, x0]
	strb	w1, [x20, x0]
	add	x0, x0, 1
	cmp	x0, 32
	bne	L311
	adrp	x24, lC10@PAGE
	mov	x2, 4512
	add	x24, x24, lC10@PAGEOFF;
	add	x21, x20, 32
	add	x25, x20, x2
	add	x23, x29, 80
	.p2align 5,,15
L312:
	mov	x0, x22
	mov	x1, x23
	mov	x2, x24
	add	x22, x22, 1024
	bl	_anubis_mldsa_encoding__pack_z
	mov	x0, x21
	mov	x1, x23
	mov	x2, 640
	add	x21, x21, 640
	bl	_memcpy
	cmp	x21, x25
	bne	L312
	movi	v31.4s, 0
	mov	x0, 4587
	mov	x1, 4595
	mov	w2, 0
	mov	x3, x19
	add	x5, x20, x0
	add	x6, x20, x1
	mov	w4, 4512
	str	q31, [x20, 4512]
	stp	q31, q31, [x21, 48]
	stp	q31, q31, [x21, 16]
	str	wzr, [x21, 79]
L315:
	mov	x0, 0
	b	L314
	.p2align 2,,3
L313:
	add	x0, x0, 1
	cmp	x0, 256
	beq	L324
L314:
	ldr	w1, [x3, x0, lsl 2]
	cmp	w1, 1
	bne	L313
	add	w1, w2, w4
	cmp	w2, 75
	beq	L313
	strb	w0, [x20, w1, sxtw]
	add	x0, x0, 1
	add	w2, w2, 1
	cmp	x0, 256
	bne	L314
L324:
	add	x3, x3, 1024
	strb	w2, [x5], 1
	cmp	x6, x5
	bne	L315
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 720
LCFI80:
	ret
LFE15:
	.const
	.align	2
lC10:
	.word	0
	.word	639
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_encoding__unpack_signature
_anubis_mldsa_encoding__unpack_signature:
LFB16:
	sub	sp, sp, #720
LCFI81:
	mov	x4, 0
	stp	x29, x30, [sp]
LCFI82:
	mov	x29, sp
LCFI83:
	stp	x19, x20, [sp, 16]
LCFI84:
	mov	x19, x0
	mov	x20, x3
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	str	x25, [sp, 64]
LCFI85:
	.p2align 5,,15
L326:
	ldrb	w0, [x19, x4]
	strb	w0, [x1, x4]
	add	x4, x4, 1
	cmp	x4, 32
	bne	L326
	adrp	x24, lC10@PAGE
	mov	x1, 7168
	mov	x21, x2
	add	x25, x2, x1
	add	x24, x24, lC10@PAGEOFF;
	add	x22, x19, 32
	add	x23, x29, 80
	.p2align 5,,15
L327:
	mov	x1, x22
	mov	x2, 640
	mov	x0, x23
	add	x22, x22, 640
	bl	_memcpy
	mov	x2, x21
	mov	x0, x23
	mov	x1, x24
	add	x21, x21, 1024
	bl	_anubis_mldsa_encoding__unpack_z
	cmp	x21, x25
	bne	L327
	mov	x3, x20
	add	x21, x20, 8192
	.p2align 5,,15
L328:
	mov	x0, x3
	mov	x2, 1024
	mov	w1, 0
	bl	_memset
	add	x3, x0, 1024
	cmp	x21, x3
	bne	L328
	mov	x0, 4587
	mov	x8, 0
	mov	w7, 0
	add	x9, x19, x0
	mov	w2, 4512
	mov	w6, 4626
	mov	w5, 1
	b	L331
L337:
	add	x8, x8, 1
	cmp	x8, 8
	beq	L347
L331:
	mov	w0, w7
	ldrb	w7, [x9, x8]
	cmp	w7, 75
	ccmp	w0, w7, 0, le
	bgt	L338
	cmp	w0, w7
	bge	L337
	sub	w0, w0, #1
	sub	w4, w7, #1
	lsl	x3, x8, 8
	b	L336
	.p2align 2,,3
L349:
	cmp	w1, w0
	bcc	L333
	cmp	w1, w6
	bhi	L348
	ldrb	w1, [x19, w1, sxtw]
	add	x1, x3, x1
	str	w5, [x20, x1, lsl 2]
	cmp	w4, w0
	beq	L337
L336:
	add	w0, w0, 1
	adds	w1, w0, w2
	bpl	L349
L333:
	adrp	x0, lC12@PAGE
	mov	w1, 688
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L347:
	mov	w0, 1
L329:
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 720
LCFI86:
	ret
L338:
LCFI87:
	mov	w0, 0
	b	L329
L348:
	adrp	x0, lC12@PAGE
	mov	w1, 688
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE16:
	.globl _anubis_mldsa_encoding_E
	.data
	.align	1
_anubis_mldsa_encoding_E:
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
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$8,LEFDE3-LASFDE3
	.long L$set$8
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB3-.
	.set L$set$9,LFE3-LFB3
	.quad L$set$9
	.uleb128 0
	.byte	0x4
	.set L$set$10,LCFI5-LFB3
	.long L$set$10
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$11,LCFI6-LCFI5
	.long L$set$11
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$15,LEFDE5-LASFDE5
	.long L$set$15
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB4-.
	.set L$set$16,LFE4-LFB4
	.quad L$set$16
	.uleb128 0
	.byte	0x4
	.set L$set$17,LCFI10-LFB4
	.long L$set$17
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x97
	.uleb128 0x2
	.byte	0x98
	.uleb128 0x1
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0xa
	.byte	0xde
	.byte	0xdd
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
	.set L$set$22,LCFI15-LCFI14
	.long L$set$22
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$23,LEFDE7-LASFDE7
	.long L$set$23
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB5-.
	.set L$set$24,LFE5-LFB5
	.quad L$set$24
	.uleb128 0
	.byte	0x4
	.set L$set$25,LCFI16-LFB5
	.long L$set$25
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$26,LCFI17-LCFI16
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$27,LCFI18-LCFI17
	.long L$set$27
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$28,LCFI19-LCFI18
	.long L$set$28
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$29,LCFI20-LCFI19
	.long L$set$29
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
	.set L$set$30,LCFI21-LCFI20
	.long L$set$30
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$31,LEFDE9-LASFDE9
	.long L$set$31
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB6-.
	.set L$set$32,LFE6-LFB6
	.quad L$set$32
	.uleb128 0
	.byte	0x4
	.set L$set$33,LCFI22-LFB6
	.long L$set$33
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$34,LCFI23-LCFI22
	.long L$set$34
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$35,LCFI24-LCFI23
	.long L$set$35
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$36,LCFI25-LCFI24
	.long L$set$36
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$37,LCFI26-LCFI25
	.long L$set$37
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$38,LEFDE11-LASFDE11
	.long L$set$38
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB7-.
	.set L$set$39,LFE7-LFB7
	.quad L$set$39
	.uleb128 0
	.byte	0x4
	.set L$set$40,LCFI27-LFB7
	.long L$set$40
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$41,LCFI28-LCFI27
	.long L$set$41
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$42,LCFI29-LCFI28
	.long L$set$42
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$43,LCFI30-LCFI29
	.long L$set$43
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI31-LCFI30
	.long L$set$44
	.byte	0xb
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$45,LEFDE13-LASFDE13
	.long L$set$45
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB8-.
	.set L$set$46,LFE8-LFB8
	.quad L$set$46
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI32-LFB8
	.long L$set$47
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$48,LCFI33-LCFI32
	.long L$set$48
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$49,LCFI34-LCFI33
	.long L$set$49
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$50,LCFI35-LCFI34
	.long L$set$50
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$51,LCFI36-LCFI35
	.long L$set$51
	.byte	0xb
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$52,LEFDE15-LASFDE15
	.long L$set$52
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB9-.
	.set L$set$53,LFE9-LFB9
	.quad L$set$53
	.uleb128 0
	.byte	0x4
	.set L$set$54,LCFI37-LFB9
	.long L$set$54
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$55,LCFI38-LCFI37
	.long L$set$55
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$56,LCFI39-LCFI38
	.long L$set$56
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$57,LCFI40-LCFI39
	.long L$set$57
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$58,LCFI41-LCFI40
	.long L$set$58
	.byte	0xb
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$59,LEFDE17-LASFDE17
	.long L$set$59
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB10-.
	.set L$set$60,LFE10-LFB10
	.quad L$set$60
	.uleb128 0
	.byte	0x4
	.set L$set$61,LCFI42-LFB10
	.long L$set$61
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$62,LCFI43-LCFI42
	.long L$set$62
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$63,LCFI44-LCFI43
	.long L$set$63
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$64,LCFI45-LCFI44
	.long L$set$64
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI46-LCFI45
	.long L$set$65
	.byte	0xb
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$66,LEFDE19-LASFDE19
	.long L$set$66
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB11-.
	.set L$set$67,LFE11-LFB11
	.quad L$set$67
	.uleb128 0
	.byte	0x4
	.set L$set$68,LCFI47-LFB11
	.long L$set$68
	.byte	0xe
	.uleb128 0x180
	.byte	0x9d
	.uleb128 0x30
	.byte	0x9e
	.uleb128 0x2f
	.byte	0x4
	.set L$set$69,LCFI48-LCFI47
	.long L$set$69
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$70,LCFI49-LCFI48
	.long L$set$70
	.byte	0x93
	.uleb128 0x2e
	.byte	0x94
	.uleb128 0x2d
	.byte	0x4
	.set L$set$71,LCFI50-LCFI49
	.long L$set$71
	.byte	0x95
	.uleb128 0x2c
	.byte	0x96
	.uleb128 0x2b
	.byte	0x4
	.set L$set$72,LCFI51-LCFI50
	.long L$set$72
	.byte	0x97
	.uleb128 0x2a
	.byte	0x4
	.set L$set$73,LCFI52-LCFI51
	.long L$set$73
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
LEFDE19:
LSFDE21:
	.set L$set$74,LEFDE21-LASFDE21
	.long L$set$74
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB12-.
	.set L$set$75,LFE12-LFB12
	.quad L$set$75
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI53-LFB12
	.long L$set$76
	.byte	0xe
	.uleb128 0x180
	.byte	0x9d
	.uleb128 0x30
	.byte	0x9e
	.uleb128 0x2f
	.byte	0x4
	.set L$set$77,LCFI54-LCFI53
	.long L$set$77
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$78,LCFI55-LCFI54
	.long L$set$78
	.byte	0x93
	.uleb128 0x2e
	.byte	0x94
	.uleb128 0x2d
	.byte	0x95
	.uleb128 0x2c
	.byte	0x96
	.uleb128 0x2b
	.byte	0x97
	.uleb128 0x2a
	.byte	0x4
	.set L$set$79,LCFI56-LCFI55
	.long L$set$79
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
LEFDE21:
LSFDE23:
	.set L$set$80,LEFDE23-LASFDE23
	.long L$set$80
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB13-.
	.set L$set$81,LFE13-LFB13
	.quad L$set$81
	.uleb128 0
	.byte	0x4
	.set L$set$82,LCFI57-LFB13
	.long L$set$82
	.byte	0xe
	.uleb128 0x250
	.byte	0x4
	.set L$set$83,LCFI58-LCFI57
	.long L$set$83
	.byte	0x9d
	.uleb128 0x4a
	.byte	0x9e
	.uleb128 0x49
	.byte	0x4
	.set L$set$84,LCFI59-LCFI58
	.long L$set$84
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$85,LCFI60-LCFI59
	.long L$set$85
	.byte	0x93
	.uleb128 0x48
	.byte	0x94
	.uleb128 0x47
	.byte	0x4
	.set L$set$86,LCFI61-LCFI60
	.long L$set$86
	.byte	0x97
	.uleb128 0x44
	.byte	0x98
	.uleb128 0x43
	.byte	0x4
	.set L$set$87,LCFI62-LCFI61
	.long L$set$87
	.byte	0x99
	.uleb128 0x42
	.byte	0x9a
	.uleb128 0x41
	.byte	0x4
	.set L$set$88,LCFI63-LCFI62
	.long L$set$88
	.byte	0x95
	.uleb128 0x46
	.byte	0x96
	.uleb128 0x45
	.byte	0x4
	.set L$set$89,LCFI64-LCFI63
	.long L$set$89
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
	.set L$set$90,LCFI65-LCFI64
	.long L$set$90
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$91,LEFDE25-LASFDE25
	.long L$set$91
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB14-.
	.set L$set$92,LFE14-LFB14
	.quad L$set$92
	.uleb128 0
	.byte	0x4
	.set L$set$93,LCFI66-LFB14
	.long L$set$93
	.byte	0xe
	.uleb128 0x250
	.byte	0x4
	.set L$set$94,LCFI67-LCFI66
	.long L$set$94
	.byte	0x9d
	.uleb128 0x4a
	.byte	0x9e
	.uleb128 0x49
	.byte	0x4
	.set L$set$95,LCFI68-LCFI67
	.long L$set$95
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$96,LCFI69-LCFI68
	.long L$set$96
	.byte	0x93
	.uleb128 0x48
	.byte	0x94
	.uleb128 0x47
	.byte	0x4
	.set L$set$97,LCFI70-LCFI69
	.long L$set$97
	.byte	0x95
	.uleb128 0x46
	.byte	0x96
	.uleb128 0x45
	.byte	0x4
	.set L$set$98,LCFI71-LCFI70
	.long L$set$98
	.byte	0x97
	.uleb128 0x44
	.byte	0x98
	.uleb128 0x43
	.byte	0x99
	.uleb128 0x42
	.byte	0x9a
	.uleb128 0x41
	.byte	0x4
	.set L$set$99,LCFI72-LCFI71
	.long L$set$99
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
	.set L$set$100,LCFI73-LCFI72
	.long L$set$100
	.byte	0xb
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$101,LEFDE27-LASFDE27
	.long L$set$101
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB15-.
	.set L$set$102,LFE15-LFB15
	.quad L$set$102
	.uleb128 0
	.byte	0x4
	.set L$set$103,LCFI74-LFB15
	.long L$set$103
	.byte	0xe
	.uleb128 0x2d0
	.byte	0x4
	.set L$set$104,LCFI75-LCFI74
	.long L$set$104
	.byte	0x9d
	.uleb128 0x5a
	.byte	0x9e
	.uleb128 0x59
	.byte	0x4
	.set L$set$105,LCFI76-LCFI75
	.long L$set$105
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$106,LCFI77-LCFI76
	.long L$set$106
	.byte	0x93
	.uleb128 0x58
	.byte	0x94
	.uleb128 0x57
	.byte	0x4
	.set L$set$107,LCFI78-LCFI77
	.long L$set$107
	.byte	0x95
	.uleb128 0x56
	.byte	0x96
	.uleb128 0x55
	.byte	0x4
	.set L$set$108,LCFI79-LCFI78
	.long L$set$108
	.byte	0x97
	.uleb128 0x54
	.byte	0x98
	.uleb128 0x53
	.byte	0x99
	.uleb128 0x52
	.byte	0x4
	.set L$set$109,LCFI80-LCFI79
	.long L$set$109
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
LEFDE27:
LSFDE29:
	.set L$set$110,LEFDE29-LASFDE29
	.long L$set$110
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB16-.
	.set L$set$111,LFE16-LFB16
	.quad L$set$111
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI81-LFB16
	.long L$set$112
	.byte	0xe
	.uleb128 0x2d0
	.byte	0x4
	.set L$set$113,LCFI82-LCFI81
	.long L$set$113
	.byte	0x9d
	.uleb128 0x5a
	.byte	0x9e
	.uleb128 0x59
	.byte	0x4
	.set L$set$114,LCFI83-LCFI82
	.long L$set$114
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$115,LCFI84-LCFI83
	.long L$set$115
	.byte	0x93
	.uleb128 0x58
	.byte	0x94
	.uleb128 0x57
	.byte	0x4
	.set L$set$116,LCFI85-LCFI84
	.long L$set$116
	.byte	0x95
	.uleb128 0x56
	.byte	0x96
	.uleb128 0x55
	.byte	0x97
	.uleb128 0x54
	.byte	0x98
	.uleb128 0x53
	.byte	0x99
	.uleb128 0x52
	.byte	0x4
	.set L$set$117,LCFI86-LCFI85
	.long L$set$117
	.byte	0xa
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
	.byte	0x4
	.set L$set$118,LCFI87-LCFI86
	.long L$set$118
	.byte	0xb
	.align	3
LEFDE29:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
