	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC7:
	.ascii "failed postcondition from anubis_mldsa_field.ads:55"
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_mldsa_poly.adb:22"
	.align	3
lC9:
	.ascii "failed postcondition from anubis_mldsa_poly.ads:22"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__poly_add
_anubis_mldsa_poly__poly_add:
LFB2:
	stp	x29, x30, [sp, -32]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
LCFI2:
	mov	x20, x0
	mov	x19, x1
	mov	x0, x2
	mov	w1, 0
	mov	x2, 1024
	bl	_memset
	mov	x9, 33889
	mov	x6, x0
	movk	x9, 0x6014, lsl 16
	mov	x3, 0
	movk	x9, 0x1c0, lsl 32
	add	x2, x0, 1024
	movk	x9, 0x2008, lsl 48
	mov	x10, -8380417
	.p2align 5,,15
L7:
	ldr	w8, [x20, x3]
	ldr	w7, [x19, x3]
	umulh	x0, x8, x9
	umulh	x1, x7, x9
	lsr	x0, x0, 20
	lsl	x5, x0, 10
	lsr	x1, x1, 20
	sub	x5, x5, x0
	lsl	x4, x1, 10
	add	x0, x0, x5, lsl 13
	sub	x4, x4, x1
	add	x1, x1, x4, lsl 13
	sub	x0, x8, x0
	sub	x1, x7, x1
	add	x4, x0, x1
	cmp	x4, 8380416
	bls	L2
	add	w1, w4, w10
L3:
	mov	x4, x6
	mov	x0, x6
	str	w1, [x6, x3]
	b	L5
	.p2align 2,,3
L19:
	add	x0, x0, 4
	cmp	x0, x2
	beq	L18
L5:
	ldr	w1, [x0]
	cmp	w1, 8380416
	bls	L19
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L18:
	add	x3, x3, 4
	cmp	x3, 1024
	bne	L7
	b	L9
	.p2align 2,,3
L21:
	add	x4, x4, 4
	cmp	x2, x4
	beq	L20
L9:
	ldr	w0, [x4]
	cmp	w0, 8380416
	bls	L21
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L2:
	add	w1, w0, w1
	cmp	w1, 8380416
	bls	L3
	adrp	x0, lC7@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L20:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI3:
	ret
LFE2:
	.const
	.align	2
lC1:
	.word	1
	.word	49
	.align	2
lC0:
	.word	1
	.word	50
	.align	2
lC2:
	.word	1
	.word	51
	.text
	.const
	.align	3
lC10:
	.ascii "anubis_mldsa_field.adb"
	.space 1
	.align	3
lC11:
	.ascii "Loop_Invariant failed at anubis_mldsa_poly.adb:39"
	.align	3
lC12:
	.ascii "failed postcondition from anubis_mldsa_poly.ads:31"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__poly_sub
_anubis_mldsa_poly__poly_sub:
LFB4:
	stp	x29, x30, [sp, -32]!
LCFI4:
	mov	x29, sp
LCFI5:
	stp	x19, x20, [sp, 16]
LCFI6:
	mov	x20, x0
	mov	x19, x1
	mov	x0, x2
	mov	w1, 0
	mov	x2, 1024
	bl	_memset
	mov	x9, 33889
	mov	w10, 57345
	movk	x9, 0x6014, lsl 16
	mov	x8, x0
	movk	x9, 0x1c0, lsl 32
	mov	x3, 0
	add	x2, x0, 1024
	movk	x9, 0x2008, lsl 48
	movk	w10, 0x7f, lsl 16
	.p2align 5,,15
L28:
	ldr	w0, [x20, x3]
	ldr	w1, [x19, x3]
	umulh	x7, x0, x9
	umulh	x6, x1, x9
	lsr	x7, x7, 20
	lsl	x5, x7, 10
	lsr	x6, x6, 20
	sub	x5, x5, x7
	lsl	x4, x6, 10
	add	x5, x7, x5, lsl 13
	sub	x4, x4, x6
	add	x4, x6, x4, lsl 13
	sub	x0, x0, x5
	sub	x1, x1, x4
	cmp	w0, w1
	bcc	L23
	sub	w1, w0, w1
	cmp	w1, 8380416
	bhi	L38
L24:
	mov	x4, x8
	mov	x0, x8
	str	w1, [x8, x3]
	b	L26
	.p2align 2,,3
L40:
	add	x0, x0, 4
	cmp	x0, x2
	beq	L39
L26:
	ldr	w1, [x0]
	cmp	w1, 8380416
	bls	L40
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L39:
	add	x3, x3, 4
	cmp	x3, 1024
	bne	L28
	b	L30
	.p2align 2,,3
L42:
	add	x4, x4, 4
	cmp	x4, x2
	beq	L41
L30:
	ldr	w0, [x4]
	cmp	w0, 8380416
	bls	L42
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L23:
	add	w0, w10, w0
	sub	w1, w0, w1
	cmp	w1, 8380416
	bls	L24
	adrp	x0, lC10@PAGE
	mov	w1, 48
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L41:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI7:
	ret
L38:
LCFI8:
	adrp	x0, lC10@PAGE
	mov	w1, 46
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE4:
	.const
	.align	3
lC13:
	.ascii "Loop_Invariant failed at anubis_mldsa_poly.adb:50"
	.align	3
lC14:
	.ascii "failed postcondition from anubis_mldsa_poly.ads:37"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__poly_reduce
_anubis_mldsa_poly__poly_reduce:
LFB6:
	mov	x6, 33889
	mov	x5, x0
	stp	x29, x30, [sp, -16]!
LCFI9:
	movk	x6, 0x6014, lsl 16
	add	x2, x0, 4
	movk	x6, 0x1c0, lsl 32
	add	x7, x0, 1028
	movk	x6, 0x2008, lsl 48
	mov	x29, sp
LCFI10:
	.p2align 5,,15
L47:
	ldr	w4, [x2, -4]
	mov	x0, x5
	umulh	x3, x4, x6
	lsr	x3, x3, 20
	lsl	x1, x3, 10
	sub	x1, x1, x3
	add	x1, x3, x1, lsl 13
	sub	x4, x4, x1
	str	w4, [x2, -4]
	b	L45
	.p2align 2,,3
L58:
	add	x0, x0, 4
	cmp	x0, x2
	beq	L57
L45:
	ldr	w1, [x0]
	cmp	w1, 8380416
	bls	L58
	adrp	x0, lC13@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L57:
	add	x2, x0, 4
	cmp	x2, x7
	bne	L47
	add	x1, x5, 1024
	b	L49
	.p2align 2,,3
L60:
	add	x5, x5, 4
	cmp	x1, x5
	beq	L59
L49:
	ldr	w0, [x5]
	cmp	w0, 8380416
	bls	L60
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L59:
	ldp	x29, x30, [sp], 16
LCFI11:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__poly_from_mont
_anubis_mldsa_poly__poly_from_mont:
LFB8:
	mov	x4, 33889
	mov	w7, 57343
	movk	x4, 0x6014, lsl 16
	mov	w6, 57345
	movk	x4, 0x1c0, lsl 32
	movk	w7, 0xfc7f, lsl 16
	movk	w6, 0x7f, lsl 16
	add	x5, x0, 1024
	movk	x4, 0x2008, lsl 48
	.p2align 5,,15
L62:
	ldr	w2, [x0]
	umull	x1, w2, w7
	umaddl	x1, w1, w6, x2
	lsr	x1, x1, 32
	umulh	x3, x1, x4
	lsr	x3, x3, 20
	lsl	x2, x3, 10
	sub	x2, x2, x3
	add	x2, x3, x2, lsl 13
	sub	x1, x1, x2
	str	w1, [x0], 4
	cmp	x5, x0
	bne	L62
	ret
LFE8:
	.const
	.align	3
lC15:
	.ascii "anubis_mldsa_poly.ads"
	.space 1
	.align	3
lC16:
	.ascii "failed precondition from anubis_mldsa_poly.ads:51"
	.align	3
lC17:
	.ascii "Loop_Invariant failed at anubis_mldsa_poly.adb:81"
	.align	3
lC18:
	.ascii "failed postcondition from anubis_mldsa_poly.ads:52"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__poly_shiftl
_anubis_mldsa_poly__poly_shiftl:
LFB9:
	stp	x29, x30, [sp, -32]!
LCFI12:
	mov	x29, sp
LCFI13:
	stp	x19, x20, [sp, 16]
LCFI14:
	tbnz	w1, #31, L80
	mov	w19, w1
	cmp	w1, 63
	bgt	L81
	mov	x6, x2
	mov	x20, x0
	mov	x2, 1024
	mov	x0, x6
	mov	w1, 0
	bl	_memset
	mov	x8, 33889
	mov	x6, x0
	movk	x8, 0x6014, lsl 16
	mov	x5, 0
	movk	x8, 0x1c0, lsl 32
	add	x2, x0, 1024
	movk	x8, 0x2008, lsl 48
	.p2align 5,,15
L70:
	ldr	w1, [x20, x5]
	mov	x7, x6
	mov	x0, x6
	lsl	x1, x1, x19
	umulh	x4, x1, x8
	lsr	x4, x4, 20
	lsl	x3, x4, 10
	sub	x3, x3, x4
	add	x3, x4, x3, lsl 13
	sub	x1, x1, x3
	str	w1, [x6, x5]
	b	L68
	.p2align 2,,3
L83:
	add	x0, x0, 4
	cmp	x0, x2
	beq	L82
L68:
	ldr	w1, [x0]
	cmp	w1, 8380416
	bls	L83
	adrp	x0, lC17@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L82:
	add	x5, x5, 4
	cmp	x5, 1024
	bne	L70
	ldr	w0, [x7]
	cmp	w0, 8380416
	bhi	L71
L85:
	add	x7, x7, 4
	cmp	x7, x2
	beq	L84
	ldr	w0, [x7]
	cmp	w0, 8380416
	bls	L85
L71:
	adrp	x0, lC18@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L84:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI15:
	ret
L81:
LCFI16:
	adrp	x0, lC16@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L80:
	adrp	x0, lC15@PAGE
	mov	w1, 51
	add	x0, x0, lC15@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE9:
	.const
	.align	3
lC19:
	.ascii "anubis_mldsa_poly.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__poly_chk_norm
_anubis_mldsa_poly__poly_chk_norm:
LFB11:
	ldr	w4, [x0]
	mov	x6, 33889
	mov	w5, -8380417
	movk	x6, 0x6014, lsl 16
	add	x7, x0, 1024
	movk	x6, 0x1c0, lsl 32
	movk	x6, 0x2008, lsl 48
	umulh	x3, x4, x6
	lsr	x3, x3, 20
	lsl	x2, x3, 10
	sub	x2, x2, x3
	add	x3, x3, x2, lsl 13
	sub	x3, x4, x3
	cmp	x3, 4190208
	add	w2, w5, w3
	csel	w3, w3, w2, ls
	tbz	w1, #31, L89
	b	L99
	.p2align 2,,3
L100:
	add	x0, x0, 4
	cmp	x0, x7
	beq	L93
	ldr	w4, [x0]
	umulh	x2, x4, x6
	lsr	x2, x2, 20
	lsl	x3, x2, 10
	sub	x3, x3, x2
	add	x2, x2, x3, lsl 13
	sub	x2, x4, x2
	cmp	x2, 4190208
	mov	w3, w2
	add	w2, w5, w2
	csel	w3, w2, w3, hi
L89:
	cmp	w3, 0
	csneg	w3, w3, w3, ge
	cmp	w3, w1
	blt	L100
	mov	w0, 0
	ret
L93:
	mov	w0, 1
	ret
L99:
	adrp	x0, lC19@PAGE
	stp	x29, x30, [sp, -16]!
LCFI17:
	mov	w1, 91
	mov	x29, sp
LCFI18:
	add	x0, x0, lC19@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE11:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_add_l
_anubis_mldsa_poly__vec_add_l:
LFB12:
	stp	x29, x30, [sp, -64]!
LCFI19:
	mov	x29, sp
LCFI20:
	stp	x19, x20, [sp, 16]
LCFI21:
	mov	x19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI22:
	mov	x22, x0
	mov	x21, x1
	str	x23, [sp, 48]
LCFI23:
	mov	x23, 7168
	.p2align 5,,15
L102:
	add	x2, x20, x19
	add	x1, x21, x19
	add	x0, x22, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_add
	cmp	x19, x23
	bne	L102
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI24:
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_sub_l
_anubis_mldsa_poly__vec_sub_l:
LFB13:
	stp	x29, x30, [sp, -64]!
LCFI25:
	mov	x29, sp
LCFI26:
	stp	x19, x20, [sp, 16]
LCFI27:
	mov	x19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI28:
	mov	x22, x0
	mov	x21, x1
	str	x23, [sp, 48]
LCFI29:
	mov	x23, 7168
	.p2align 5,,15
L106:
	add	x2, x20, x19
	add	x1, x21, x19
	add	x0, x22, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_sub
	cmp	x19, x23
	bne	L106
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI30:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_add_k
_anubis_mldsa_poly__vec_add_k:
LFB14:
	stp	x29, x30, [sp, -48]!
LCFI31:
	mov	x29, sp
LCFI32:
	stp	x19, x20, [sp, 16]
LCFI33:
	mov	x19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI34:
	mov	x22, x0
	mov	x21, x1
	.p2align 5,,15
L110:
	add	x2, x20, x19
	add	x1, x21, x19
	add	x0, x22, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_add
	cmp	x19, 8192
	bne	L110
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI35:
	ret
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_sub_k
_anubis_mldsa_poly__vec_sub_k:
LFB15:
	stp	x29, x30, [sp, -48]!
LCFI36:
	mov	x29, sp
LCFI37:
	stp	x19, x20, [sp, 16]
LCFI38:
	mov	x19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI39:
	mov	x22, x0
	mov	x21, x1
	.p2align 5,,15
L114:
	add	x2, x20, x19
	add	x1, x21, x19
	add	x0, x22, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_sub
	cmp	x19, 8192
	bne	L114
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI40:
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_reduce_k
_anubis_mldsa_poly__vec_reduce_k:
LFB16:
	stp	x29, x30, [sp, -32]!
LCFI41:
	mov	x29, sp
LCFI42:
	stp	x19, x20, [sp, 16]
LCFI43:
	mov	x19, x0
	add	x20, x0, 8192
	.p2align 5,,15
L118:
	mov	x0, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_reduce
	cmp	x19, x20
	bne	L118
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI44:
	ret
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_reduce_l
_anubis_mldsa_poly__vec_reduce_l:
LFB17:
	stp	x29, x30, [sp, -32]!
LCFI45:
	mov	x29, sp
LCFI46:
	stp	x19, x20, [sp, 16]
LCFI47:
	mov	x19, x0
	mov	x0, 7168
	add	x20, x19, x0
	.p2align 5,,15
L122:
	mov	x0, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_reduce
	cmp	x19, x20
	bne	L122
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI48:
	ret
LFE17:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_from_mont_k
_anubis_mldsa_poly__vec_from_mont_k:
LFB18:
	mov	x5, 33889
	mov	x8, x0
	movk	x5, 0x6014, lsl 16
	mov	w7, 57343
	mov	w6, 57345
	add	x4, x0, 1024
	movk	x5, 0x1c0, lsl 32
	mov	x0, 9216
	movk	w7, 0xfc7f, lsl 16
	movk	w6, 0x7f, lsl 16
	add	x8, x8, x0
	movk	x5, 0x2008, lsl 48
L127:
	sub	x3, x4, #1024
	.p2align 5,,15
L126:
	ldr	w1, [x3]
	umull	x0, w1, w7
	umaddl	x0, w0, w6, x1
	lsr	x0, x0, 32
	umulh	x2, x0, x5
	lsr	x2, x2, 20
	lsl	x1, x2, 10
	sub	x1, x1, x2
	add	x1, x2, x1, lsl 13
	sub	x0, x0, x1
	str	w0, [x3], 4
	cmp	x4, x3
	bne	L126
	add	x4, x4, 1024
	cmp	x8, x4
	bne	L127
	ret
LFE18:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_from_mont_l
_anubis_mldsa_poly__vec_from_mont_l:
LFB19:
	mov	x5, 33889
	mov	w7, 57343
	movk	x5, 0x6014, lsl 16
	mov	w6, 57345
	movk	x5, 0x1c0, lsl 32
	movk	w7, 0xfc7f, lsl 16
	movk	w6, 0x7f, lsl 16
	add	x4, x0, 1024
	add	x8, x0, 8192
	movk	x5, 0x2008, lsl 48
L132:
	sub	x3, x4, #1024
	.p2align 5,,15
L131:
	ldr	w1, [x3]
	umull	x0, w1, w7
	umaddl	x0, w0, w6, x1
	lsr	x0, x0, 32
	umulh	x2, x0, x5
	lsr	x2, x2, 20
	lsl	x1, x2, 10
	sub	x1, x1, x2
	add	x1, x2, x1, lsl 13
	sub	x0, x0, x1
	str	w0, [x3], 4
	cmp	x4, x3
	bne	L131
	add	x4, x4, 1024
	cmp	x8, x4
	bne	L132
	ret
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_center_k
_anubis_mldsa_poly__vec_center_k:
LFB21:
	mov	x5, x0
	add	x3, x0, 1024
	mov	x0, 9216
	mov	w4, -8380417
	add	x5, x5, x0
L138:
	sub	x0, x3, #1024
	.p2align 5,,15
L137:
	ldr	w1, [x0]
	add	w2, w1, w4
	cmp	w1, 4190208
	bls	L136
	str	w2, [x0]
L136:
	add	x0, x0, 4
	cmp	x0, x3
	bne	L137
	add	x3, x0, 1024
	cmp	x3, x5
	bne	L138
	ret
LFE21:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_center_l
_anubis_mldsa_poly__vec_center_l:
LFB22:
	mov	w4, -8380417
	add	x3, x0, 1024
	add	x5, x0, 8192
L144:
	sub	x0, x3, #1024
	.p2align 5,,15
L143:
	ldr	w1, [x0]
	add	w2, w1, w4
	cmp	w1, 4190208
	bls	L142
	str	w2, [x0]
L142:
	add	x0, x0, 4
	cmp	x0, x3
	bne	L143
	add	x3, x0, 1024
	cmp	x3, x5
	bne	L144
	ret
LFE22:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_chk_norm_l
_anubis_mldsa_poly__vec_chk_norm_l:
LFB23:
	tbnz	w1, #31, L148
	mov	x7, 33889
	add	x5, x0, 1024
	movk	x7, 0x6014, lsl 16
	add	x8, x0, 8192
	movk	x7, 0x1c0, lsl 32
	mov	w6, -8380417
	movk	x7, 0x2008, lsl 48
	.p2align 5,,15
L149:
	sub	x3, x5, #1024
	b	L152
	.p2align 2,,3
L159:
	add	x3, x3, 4
	cmp	x3, x5
	beq	L158
L152:
	ldr	w4, [x3]
	umulh	x0, x4, x7
	lsr	x0, x0, 20
	lsl	x2, x0, 10
	sub	x2, x2, x0
	add	x0, x0, x2, lsl 13
	sub	x0, x4, x0
	cmp	x0, 4190208
	add	w2, w6, w0
	csel	w0, w0, w2, ls
	cmp	w0, 0
	csneg	w0, w0, w0, ge
	cmp	w1, w0
	bgt	L159
	mov	w0, 0
	ret
L158:
	add	x5, x3, 1024
	cmp	x5, x8
	bne	L149
	mov	w0, 1
	ret
L148:
	adrp	x0, lC19@PAGE
	stp	x29, x30, [sp, -16]!
LCFI49:
	mov	w1, 196
	mov	x29, sp
LCFI50:
	add	x0, x0, lC19@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_chk_norm_k
_anubis_mldsa_poly__vec_chk_norm_k:
LFB24:
	tbnz	w1, #31, L161
	mov	x7, 33889
	mov	x2, 9216
	movk	x7, 0x6014, lsl 16
	add	x5, x0, 1024
	movk	x7, 0x1c0, lsl 32
	add	x8, x0, x2
	movk	x7, 0x2008, lsl 48
	mov	w6, -8380417
	.p2align 5,,15
L162:
	sub	x3, x5, #1024
	b	L165
	.p2align 2,,3
L172:
	add	x3, x3, 4
	cmp	x3, x5
	beq	L171
L165:
	ldr	w4, [x3]
	umulh	x0, x4, x7
	lsr	x0, x0, 20
	lsl	x2, x0, 10
	sub	x2, x2, x0
	add	x0, x0, x2, lsl 13
	sub	x0, x4, x0
	cmp	x0, 4190208
	add	w2, w6, w0
	csel	w0, w0, w2, ls
	cmp	w0, 0
	csneg	w0, w0, w0, ge
	cmp	w1, w0
	bgt	L172
	mov	w0, 0
	ret
L171:
	add	x5, x3, 1024
	cmp	x5, x8
	bne	L162
	mov	w0, 1
	ret
L161:
	adrp	x0, lC19@PAGE
	stp	x29, x30, [sp, -16]!
LCFI51:
	mov	w1, 206
	mov	x29, sp
LCFI52:
	add	x0, x0, lC19@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__poly_power2round
_anubis_mldsa_poly__poly_power2round:
LFB25:
	stp	x29, x30, [sp, -64]!
LCFI53:
	mov	x29, sp
LCFI54:
	stp	x23, x24, [sp, 48]
LCFI55:
	mov	x23, 33889
	mov	w24, 57345
	movk	x23, 0x6014, lsl 16
	movk	w24, 0x7f, lsl 16
	movk	x23, 0x1c0, lsl 32
	movk	x23, 0x2008, lsl 48
	stp	x19, x20, [sp, 16]
LCFI56:
	mov	x19, 0
	mov	x20, x0
	stp	x21, x22, [sp, 32]
LCFI57:
	mov	x22, x1
	mov	x21, x2
	b	L178
	.p2align 2,,3
L176:
	str	w3, [x21, x19]
	add	x19, x19, 4
	cmp	x19, 1024
	beq	L181
L178:
	ldr	w0, [x20, x19]
	umulh	x1, x0, x23
	lsr	x1, x1, 20
	lsl	x3, x1, 10
	sub	x3, x3, x1
	add	x3, x1, x3, lsl 13
	sub	w0, w0, w3
	bl	_anubis_mldsa_field__power2round
	asr	x3, x0, 32
	cmp	w0, 8380416
	bhi	L182
	str	w0, [x22, x19]
	tbz	w3, #31, L176
	adds	w3, w24, w3
	bpl	L176
	adrp	x0, lC19@PAGE
	mov	w1, 229
	add	x0, x0, lC19@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L181:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 64
LCFI58:
	ret
L182:
LCFI59:
	adrp	x0, lC19@PAGE
	mov	w1, 224
	add	x0, x0, lC19@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_power2round_k
_anubis_mldsa_poly__vec_power2round_k:
LFB26:
	stp	x29, x30, [sp, -48]!
LCFI60:
	mov	x29, sp
LCFI61:
	stp	x19, x20, [sp, 16]
LCFI62:
	mov	x19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI63:
	mov	x22, x0
	mov	x21, x1
	.p2align 5,,15
L184:
	add	x2, x20, x19
	add	x1, x21, x19
	add	x0, x22, x19
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_power2round
	cmp	x19, 8192
	bne	L184
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI64:
	ret
LFE26:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__poly_decompose
_anubis_mldsa_poly__poly_decompose:
LFB27:
	stp	x29, x30, [sp, -64]!
LCFI65:
	mov	x29, sp
LCFI66:
	stp	x23, x24, [sp, 48]
LCFI67:
	mov	x23, 33889
	mov	w24, 57345
	movk	x23, 0x6014, lsl 16
	movk	w24, 0x7f, lsl 16
	movk	x23, 0x1c0, lsl 32
	movk	x23, 0x2008, lsl 48
	stp	x19, x20, [sp, 16]
LCFI68:
	mov	x19, 0
	mov	x20, x0
	stp	x21, x22, [sp, 32]
LCFI69:
	mov	x22, x1
	mov	x21, x2
	b	L192
	.p2align 2,,3
L190:
	str	w3, [x21, x19]
	add	x19, x19, 4
	cmp	x19, 1024
	beq	L195
L192:
	ldr	w0, [x20, x19]
	umulh	x1, x0, x23
	lsr	x1, x1, 20
	lsl	x3, x1, 10
	sub	x3, x3, x1
	add	x3, x1, x3, lsl 13
	sub	w0, w0, w3
	bl	_anubis_mldsa_field__decompose
	asr	x3, x0, 32
	cmp	w0, 8380416
	bhi	L196
	str	w0, [x22, x19]
	tbz	w3, #31, L190
	adds	w3, w24, w3
	bpl	L190
	adrp	x0, lC19@PAGE
	mov	w1, 261
	add	x0, x0, lC19@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L195:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 64
LCFI70:
	ret
L196:
LCFI71:
	adrp	x0, lC19@PAGE
	mov	w1, 257
	add	x0, x0, lC19@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE27:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_highbits_k
_anubis_mldsa_poly__vec_highbits_k:
LFB28:
	sub	sp, sp, #1072
LCFI72:
	stp	x29, x30, [sp]
LCFI73:
	mov	x29, sp
LCFI74:
	stp	x19, x20, [sp, 16]
LCFI75:
	mov	x19, x0
	mov	x20, x1
	stp	x21, x22, [sp, 32]
LCFI76:
	add	x22, x0, 8192
	add	x21, x29, 48
	.p2align 5,,15
L198:
	mov	x1, x20
	mov	x0, x19
	mov	x2, x21
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_decompose
	add	x20, x20, 1024
	cmp	x19, x22
	bne	L198
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, 1072
LCFI77:
	ret
LFE28:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_lowbits_k
_anubis_mldsa_poly__vec_lowbits_k:
LFB29:
	sub	sp, sp, #1072
LCFI78:
	stp	x29, x30, [sp]
LCFI79:
	mov	x29, sp
LCFI80:
	stp	x19, x20, [sp, 16]
LCFI81:
	mov	x19, x0
	mov	x20, x1
	stp	x21, x22, [sp, 32]
LCFI82:
	add	x22, x0, 8192
	add	x21, x29, 48
	.p2align 5,,15
L202:
	mov	x2, x20
	mov	x0, x19
	mov	x1, x21
	add	x19, x19, 1024
	bl	_anubis_mldsa_poly__poly_decompose
	add	x20, x20, 1024
	cmp	x19, x22
	bne	L202
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, 1072
LCFI83:
	ret
LFE29:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_makehint_k
_anubis_mldsa_poly__vec_makehint_k:
LFB30:
	stp	x29, x30, [sp, -112]!
LCFI84:
	mov	x29, sp
LCFI85:
	stp	x21, x22, [sp, 32]
LCFI86:
	mov	x21, 33889
	mov	w22, 0
	movk	x21, 0x6014, lsl 16
	movk	x21, 0x1c0, lsl 32
	stp	x23, x24, [sp, 48]
LCFI87:
	mov	x23, x0
	mov	x24, x1
	mov	x0, x2
	mov	w1, 0
	movk	x21, 0x2008, lsl 48
	stp	x25, x26, [sp, 64]
LCFI88:
	mov	x25, x2
	mov	x2, 8192
	stp	x19, x20, [sp, 16]
	stp	x27, x28, [sp, 80]
LCFI89:
	bl	_memset
	add	x0, x23, 8192
	str	x0, [x29, 104]
L214:
	mov	x28, 0
	b	L213
	.p2align 2,,3
L219:
	mov	w0, -8380417
	add	w0, w19, w0
L208:
	bl	_anubis_mldsa_field__decompose
	cmp	w0, 8380416
	bhi	L209
	cmp	w27, w0
	beq	L212
	mov	w0, 1
	cmp	w22, 2048
	cinc	w22, w22, lt
	str	w0, [x25, x28]
L212:
	add	x28, x28, 4
	cmp	x28, 1024
	beq	L218
L213:
	ldr	w4, [x24, x28]
	ldr	w3, [x23, x28]
	umulh	x19, x4, x21
	umulh	x1, x3, x21
	lsr	x19, x19, 20
	lsl	x0, x19, 10
	lsr	x26, x1, 20
	sub	x0, x0, x19
	add	x19, x19, x0, lsl 13
	lsl	x0, x26, 10
	sub	x0, x0, x26
	add	x1, x26, x0, lsl 13
	sub	x19, x4, x19
	mov	w0, w19
	mov	w20, w19
	sub	x26, x3, x1
	bl	_anubis_mldsa_field__decompose
	mov	x27, x0
	cmp	w0, 8380416
	bhi	L209
	add	x19, x26, x19
	cmp	x19, 8380416
	bhi	L219
	add	w0, w20, w26
	cmp	w0, 8380416
	bls	L208
	adrp	x0, lC10@PAGE
	mov	w1, 215
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L218:
	ldr	x0, [x29, 104]
	add	x23, x23, 1024
	add	x25, x25, 1024
	add	x24, x24, 1024
	cmp	x23, x0
	bne	L214
	mov	w0, w22
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 112
LCFI90:
	ret
L209:
LCFI91:
	adrp	x0, lC10@PAGE
	mov	w1, 192
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE30:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_poly__vec_usehint_k
_anubis_mldsa_poly__vec_usehint_k:
LFB31:
	stp	x29, x30, [sp, -96]!
LCFI92:
	mov	x29, sp
LCFI93:
	stp	x23, x24, [sp, 48]
LCFI94:
	mov	x23, 33889
	mov	w24, 15
	movk	x23, 0x6014, lsl 16
	movk	x23, 0x1c0, lsl 32
	stp	x19, x20, [sp, 16]
LCFI95:
	mov	x20, x1
	mov	w1, 0
	mov	x19, 1024
	movk	x23, 0x2008, lsl 48
	stp	x21, x22, [sp, 32]
LCFI96:
	mov	x21, x0
	mov	x22, x2
	mov	x0, x2
	mov	x2, 8192
	stp	x25, x26, [sp, 64]
LCFI97:
	mov	x25, 9216
	str	x27, [sp, 80]
LCFI98:
	bl	_memset
	.p2align 5,,15
L229:
	sub	x26, x19, #1024
	b	L228
	.p2align 2,,3
L236:
	cmp	w0, 8380416
	bhi	L234
L222:
	str	w1, [x22, x26]
	add	x26, x26, 4
	cmp	x26, x19
	beq	L235
L228:
	ldr	w2, [x20, x26]
	ldr	w27, [x21, x26]
	umulh	x1, x2, x23
	lsr	x1, x1, 20
	lsl	x0, x1, 10
	sub	x0, x0, x1
	add	x0, x1, x0, lsl 13
	sub	w0, w2, w0
	bl	_anubis_mldsa_field__decompose
	mov	w1, w0
	asr	x2, x0, 32
	cmp	w27, 1
	bne	L236
	cmp	w2, 0
	ble	L223
	cmp	w0, 8380416
	bhi	L237
	cmp	w0, 15
	beq	L226
	add	w1, w0, 1
	cmp	w0, 8380416
	bne	L222
L226:
	mov	w1, 0
	str	w1, [x22, x26]
	add	x26, x26, 4
	cmp	x26, x19
	bne	L228
L235:
	add	x19, x26, 1024
	cmp	x19, x25
	bne	L229
	ldr	x27, [sp, 80]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 96
LCFI99:
	ret
	.p2align 2,,3
L223:
LCFI100:
	cmp	w0, 8380416
	bhi	L238
	cmp	w0, 0
	sub	w0, w0, #1
	csel	w1, w0, w24, ne
	b	L222
L234:
	adrp	x0, lC10@PAGE
	mov	w1, 232
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L237:
	adrp	x0, lC10@PAGE
	mov	w1, 239
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L238:
	adrp	x0, lC10@PAGE
	mov	w1, 247
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE31:
	.globl _anubis_mldsa_poly_E
	.data
	.align	1
_anubis_mldsa_poly_E:
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
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$7,LEFDE3-LASFDE3
	.long L$set$7
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB4-.
	.set L$set$8,LFE4-LFB4
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB4
	.long L$set$9
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$10,LCFI5-LCFI4
	.long L$set$10
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$11,LCFI6-LCFI5
	.long L$set$11
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$14,LEFDE5-LASFDE5
	.long L$set$14
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB6-.
	.set L$set$15,LFE6-LFB6
	.quad L$set$15
	.uleb128 0
	.byte	0x4
	.set L$set$16,LCFI9-LFB6
	.long L$set$16
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$17,LCFI10-LCFI9
	.long L$set$17
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$19,LEFDE7-LASFDE7
	.long L$set$19
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB8-.
	.set L$set$20,LFE8-LFB8
	.quad L$set$20
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$21,LEFDE9-LASFDE9
	.long L$set$21
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB9-.
	.set L$set$22,LFE9-LFB9
	.quad L$set$22
	.uleb128 0
	.byte	0x4
	.set L$set$23,LCFI12-LFB9
	.long L$set$23
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$24,LCFI13-LCFI12
	.long L$set$24
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$25,LCFI14-LCFI13
	.long L$set$25
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$26,LCFI15-LCFI14
	.long L$set$26
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$27,LCFI16-LCFI15
	.long L$set$27
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$28,LEFDE11-LASFDE11
	.long L$set$28
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB11-.
	.set L$set$29,LFE11-LFB11
	.quad L$set$29
	.uleb128 0
	.byte	0x4
	.set L$set$30,LCFI17-LFB11
	.long L$set$30
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$31,LCFI18-LCFI17
	.long L$set$31
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$32,LEFDE13-LASFDE13
	.long L$set$32
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB12-.
	.set L$set$33,LFE12-LFB12
	.quad L$set$33
	.uleb128 0
	.byte	0x4
	.set L$set$34,LCFI19-LFB12
	.long L$set$34
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$35,LCFI20-LCFI19
	.long L$set$35
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$36,LCFI21-LCFI20
	.long L$set$36
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$37,LCFI22-LCFI21
	.long L$set$37
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$38,LCFI23-LCFI22
	.long L$set$38
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$39,LCFI24-LCFI23
	.long L$set$39
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
LEFDE13:
LSFDE15:
	.set L$set$40,LEFDE15-LASFDE15
	.long L$set$40
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB13-.
	.set L$set$41,LFE13-LFB13
	.quad L$set$41
	.uleb128 0
	.byte	0x4
	.set L$set$42,LCFI25-LFB13
	.long L$set$42
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$43,LCFI26-LCFI25
	.long L$set$43
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$44,LCFI27-LCFI26
	.long L$set$44
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$45,LCFI28-LCFI27
	.long L$set$45
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$46,LCFI29-LCFI28
	.long L$set$46
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$47,LCFI30-LCFI29
	.long L$set$47
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
LEFDE15:
LSFDE17:
	.set L$set$48,LEFDE17-LASFDE17
	.long L$set$48
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB14-.
	.set L$set$49,LFE14-LFB14
	.quad L$set$49
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI31-LFB14
	.long L$set$50
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$51,LCFI32-LCFI31
	.long L$set$51
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$52,LCFI33-LCFI32
	.long L$set$52
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$53,LCFI34-LCFI33
	.long L$set$53
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$54,LCFI35-LCFI34
	.long L$set$54
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
LEFDE17:
LSFDE19:
	.set L$set$55,LEFDE19-LASFDE19
	.long L$set$55
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB15-.
	.set L$set$56,LFE15-LFB15
	.quad L$set$56
	.uleb128 0
	.byte	0x4
	.set L$set$57,LCFI36-LFB15
	.long L$set$57
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$58,LCFI37-LCFI36
	.long L$set$58
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$59,LCFI38-LCFI37
	.long L$set$59
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$60,LCFI39-LCFI38
	.long L$set$60
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$61,LCFI40-LCFI39
	.long L$set$61
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
LEFDE19:
LSFDE21:
	.set L$set$62,LEFDE21-LASFDE21
	.long L$set$62
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB16-.
	.set L$set$63,LFE16-LFB16
	.quad L$set$63
	.uleb128 0
	.byte	0x4
	.set L$set$64,LCFI41-LFB16
	.long L$set$64
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$65,LCFI42-LCFI41
	.long L$set$65
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$66,LCFI43-LCFI42
	.long L$set$66
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$67,LCFI44-LCFI43
	.long L$set$67
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
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
	.quad	LFB17-.
	.set L$set$69,LFE17-LFB17
	.quad L$set$69
	.uleb128 0
	.byte	0x4
	.set L$set$70,LCFI45-LFB17
	.long L$set$70
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$71,LCFI46-LCFI45
	.long L$set$71
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$72,LCFI47-LCFI46
	.long L$set$72
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$73,LCFI48-LCFI47
	.long L$set$73
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$74,LEFDE25-LASFDE25
	.long L$set$74
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB18-.
	.set L$set$75,LFE18-LFB18
	.quad L$set$75
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$76,LEFDE27-LASFDE27
	.long L$set$76
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB19-.
	.set L$set$77,LFE19-LFB19
	.quad L$set$77
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$78,LEFDE29-LASFDE29
	.long L$set$78
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB21-.
	.set L$set$79,LFE21-LFB21
	.quad L$set$79
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$80,LEFDE31-LASFDE31
	.long L$set$80
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB22-.
	.set L$set$81,LFE22-LFB22
	.quad L$set$81
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$82,LEFDE33-LASFDE33
	.long L$set$82
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB23-.
	.set L$set$83,LFE23-LFB23
	.quad L$set$83
	.uleb128 0
	.byte	0x4
	.set L$set$84,LCFI49-LFB23
	.long L$set$84
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$85,LCFI50-LCFI49
	.long L$set$85
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$86,LEFDE35-LASFDE35
	.long L$set$86
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB24-.
	.set L$set$87,LFE24-LFB24
	.quad L$set$87
	.uleb128 0
	.byte	0x4
	.set L$set$88,LCFI51-LFB24
	.long L$set$88
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$89,LCFI52-LCFI51
	.long L$set$89
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$90,LEFDE37-LASFDE37
	.long L$set$90
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB25-.
	.set L$set$91,LFE25-LFB25
	.quad L$set$91
	.uleb128 0
	.byte	0x4
	.set L$set$92,LCFI53-LFB25
	.long L$set$92
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$93,LCFI54-LCFI53
	.long L$set$93
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$94,LCFI55-LCFI54
	.long L$set$94
	.byte	0x97
	.uleb128 0x2
	.byte	0x98
	.uleb128 0x1
	.byte	0x4
	.set L$set$95,LCFI56-LCFI55
	.long L$set$95
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$96,LCFI57-LCFI56
	.long L$set$96
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$97,LCFI58-LCFI57
	.long L$set$97
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
	.set L$set$98,LCFI59-LCFI58
	.long L$set$98
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$99,LEFDE39-LASFDE39
	.long L$set$99
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB26-.
	.set L$set$100,LFE26-LFB26
	.quad L$set$100
	.uleb128 0
	.byte	0x4
	.set L$set$101,LCFI60-LFB26
	.long L$set$101
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$102,LCFI61-LCFI60
	.long L$set$102
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$103,LCFI62-LCFI61
	.long L$set$103
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$104,LCFI63-LCFI62
	.long L$set$104
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$105,LCFI64-LCFI63
	.long L$set$105
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
LEFDE39:
LSFDE41:
	.set L$set$106,LEFDE41-LASFDE41
	.long L$set$106
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB27-.
	.set L$set$107,LFE27-LFB27
	.quad L$set$107
	.uleb128 0
	.byte	0x4
	.set L$set$108,LCFI65-LFB27
	.long L$set$108
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$109,LCFI66-LCFI65
	.long L$set$109
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$110,LCFI67-LCFI66
	.long L$set$110
	.byte	0x97
	.uleb128 0x2
	.byte	0x98
	.uleb128 0x1
	.byte	0x4
	.set L$set$111,LCFI68-LCFI67
	.long L$set$111
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$112,LCFI69-LCFI68
	.long L$set$112
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$113,LCFI70-LCFI69
	.long L$set$113
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
	.set L$set$114,LCFI71-LCFI70
	.long L$set$114
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$115,LEFDE43-LASFDE43
	.long L$set$115
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB28-.
	.set L$set$116,LFE28-LFB28
	.quad L$set$116
	.uleb128 0
	.byte	0x4
	.set L$set$117,LCFI72-LFB28
	.long L$set$117
	.byte	0xe
	.uleb128 0x430
	.byte	0x4
	.set L$set$118,LCFI73-LCFI72
	.long L$set$118
	.byte	0x9d
	.uleb128 0x86
	.byte	0x9e
	.uleb128 0x85
	.byte	0x4
	.set L$set$119,LCFI74-LCFI73
	.long L$set$119
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$120,LCFI75-LCFI74
	.long L$set$120
	.byte	0x93
	.uleb128 0x84
	.byte	0x94
	.uleb128 0x83
	.byte	0x4
	.set L$set$121,LCFI76-LCFI75
	.long L$set$121
	.byte	0x95
	.uleb128 0x82
	.byte	0x96
	.uleb128 0x81
	.byte	0x4
	.set L$set$122,LCFI77-LCFI76
	.long L$set$122
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
LEFDE43:
LSFDE45:
	.set L$set$123,LEFDE45-LASFDE45
	.long L$set$123
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB29-.
	.set L$set$124,LFE29-LFB29
	.quad L$set$124
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI78-LFB29
	.long L$set$125
	.byte	0xe
	.uleb128 0x430
	.byte	0x4
	.set L$set$126,LCFI79-LCFI78
	.long L$set$126
	.byte	0x9d
	.uleb128 0x86
	.byte	0x9e
	.uleb128 0x85
	.byte	0x4
	.set L$set$127,LCFI80-LCFI79
	.long L$set$127
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$128,LCFI81-LCFI80
	.long L$set$128
	.byte	0x93
	.uleb128 0x84
	.byte	0x94
	.uleb128 0x83
	.byte	0x4
	.set L$set$129,LCFI82-LCFI81
	.long L$set$129
	.byte	0x95
	.uleb128 0x82
	.byte	0x96
	.uleb128 0x81
	.byte	0x4
	.set L$set$130,LCFI83-LCFI82
	.long L$set$130
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
LEFDE45:
LSFDE47:
	.set L$set$131,LEFDE47-LASFDE47
	.long L$set$131
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB30-.
	.set L$set$132,LFE30-LFB30
	.quad L$set$132
	.uleb128 0
	.byte	0x4
	.set L$set$133,LCFI84-LFB30
	.long L$set$133
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$134,LCFI85-LCFI84
	.long L$set$134
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$135,LCFI86-LCFI85
	.long L$set$135
	.byte	0x95
	.uleb128 0xa
	.byte	0x96
	.uleb128 0x9
	.byte	0x4
	.set L$set$136,LCFI87-LCFI86
	.long L$set$136
	.byte	0x97
	.uleb128 0x8
	.byte	0x98
	.uleb128 0x7
	.byte	0x4
	.set L$set$137,LCFI88-LCFI87
	.long L$set$137
	.byte	0x99
	.uleb128 0x6
	.byte	0x9a
	.uleb128 0x5
	.byte	0x4
	.set L$set$138,LCFI89-LCFI88
	.long L$set$138
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x9b
	.uleb128 0x4
	.byte	0x9c
	.uleb128 0x3
	.byte	0x4
	.set L$set$139,LCFI90-LCFI89
	.long L$set$139
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
	.set L$set$140,LCFI91-LCFI90
	.long L$set$140
	.byte	0xb
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$141,LEFDE49-LASFDE49
	.long L$set$141
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB31-.
	.set L$set$142,LFE31-LFB31
	.quad L$set$142
	.uleb128 0
	.byte	0x4
	.set L$set$143,LCFI92-LFB31
	.long L$set$143
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$144,LCFI93-LCFI92
	.long L$set$144
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$145,LCFI94-LCFI93
	.long L$set$145
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x4
	.set L$set$146,LCFI95-LCFI94
	.long L$set$146
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$147,LCFI96-LCFI95
	.long L$set$147
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$148,LCFI97-LCFI96
	.long L$set$148
	.byte	0x99
	.uleb128 0x4
	.byte	0x9a
	.uleb128 0x3
	.byte	0x4
	.set L$set$149,LCFI98-LCFI97
	.long L$set$149
	.byte	0x9b
	.uleb128 0x2
	.byte	0x4
	.set L$set$150,LCFI99-LCFI98
	.long L$set$150
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
	.set L$set$151,LCFI100-LCFI99
	.long L$set$151
	.byte	0xb
	.align	3
LEFDE49:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
