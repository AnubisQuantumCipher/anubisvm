	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
_anubis_keccak__theta___wrapped_statements.0:
LFB6:
	ldr	x2, [x16]
	sub	sp, sp, #48
LCFI0:
	add	x6, sp, 8
	mov	x4, x6
	add	x8, x2, 200
	mov	x0, x2
L2:
	ldp	x1, x5, [x0]
	ldp	x3, x7, [x0, 16]
	add	x0, x0, 40
	eor	x1, x1, x5
	ldr	x5, [x0, -8]
	eor	x3, x3, x7
	eor	x1, x1, x5
	eor	x1, x1, x3
	str	x1, [x4], 8
	cmp	x0, x8
	bne	L2
	add	x2, x2, 40
	mov	x0, 1
	mov	w4, 0
	mov	x1, 4
L3:
	mov	x5, x0
L7:
	ldr	x3, [x6, x0, lsl 3]
	sub	x0, x2, #40
	ldr	x1, [x6, x1, lsl 3]
	eor	x3, x1, x3, ror 63
L4:
	ldr	x1, [x0]
	eor	x1, x3, x1
	str	x1, [x0], 8
	cmp	x2, x0
	bne	L4
	add	x2, x2, 40
	add	x3, x5, 1
	cmp	w4, 4
	beq	L1
	add	w4, w4, 1
	mov	x0, 0
	sxtb	w4, w4
	mov	x1, 3
	cmp	w4, 4
	beq	L12
	sub	x1, x5, #1
	mov	x0, x3
	b	L3
	.p2align 2,,3
L12:
	mov	x5, x3
	b	L7
	.p2align 2,,3
L1:
	add	sp, sp, 48
LCFI1:
	ret
LFE6:
	.align	2
	.p2align 5,,15
_anubis_keccak__chi___wrapped_statements.3:
LFB12:
	ldr	x11, [x16]
	sub	sp, sp, #48
LCFI2:
	adrp	x12, _CSWTCH.52@PAGE
	mov	x8, sp
	add	x3, x11, 160
	add	x11, x11, 200
	.p2align 5,,15
L15:
	mov	x2, x3
	ldr	x7, [x3, -160]
	add	x4, x12, _CSWTCH.52@PAGEOFF;
	mov	x1, x8
	ldr	x9, [x2], -160
	ldr	x10, [x3, -120]
	ldr	x5, [x3, -80]
	str	x9, [sp, 32]
	ldr	x0, [x3, -40]
	stp	x7, x10, [sp]
	stp	x5, x0, [sp, 16]
L14:
	ldrsb	x6, [x4], 1
	ldp	x5, x0, [x1], 8
	ldr	x6, [x8, x6, lsl 3]
	bic	x0, x6, x0
	eor	x0, x0, x5
	str	x0, [x2], 40
	cmp	x2, x3
	bne	L14
	bic	x7, x10, x7
	mov	x3, x2
	eor	x7, x7, x9
	str	x7, [x3], 8
	cmp	x11, x3
	bne	L15
	add	sp, sp, 48
LCFI3:
	ret
LFE12:
	.const
	.align	3
lC2:
	.ascii "anubis_keccak.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_keccak__rho___wrapped_statements.1:
LFB8:
	ldr	x3, [x16]
	adrp	x4, _anubis_keccak__rho_offsets@PAGE
	add	x4, x4, _anubis_keccak__rho_offsets@PAGEOFF;
	add	x5, x3, 200
L22:
	mov	x0, 0
L21:
	ldr	w1, [x4, x0, lsl 2]
	ldr	x2, [x3, x0, lsl 3]
	tbnz	w1, #31, L28
	neg	w1, w1
	ror	x2, x2, x1
	str	x2, [x3, x0, lsl 3]
	add	x0, x0, 1
	cmp	x0, 5
	bne	L21
	add	x3, x3, 40
	add	x4, x4, 20
	cmp	x5, x3
	bne	L22
	ret
L28:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI4:
	mov	w1, 54
	mov	x29, sp
LCFI5:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE8:
	.align	2
	.p2align 5,,15
_anubis_keccak__pi___wrapped_statements.2:
LFB10:
	ldr	x0, [x16]
	sub	sp, sp, #208
LCFI6:
	mov	w7, 52429
	add	x6, sp, 8
	mov	x5, 0
	mov	w8, 0
	mov	w4, 15
	movk	w7, 0xcccc, lsl 16
	ldp	q29, q28, [x0]
	mov	x9, x0
	ldp	q31, q30, [x0, 32]
	stp	q29, q28, [x6]
	ldp	q29, q28, [x0, 64]
	stp	q31, q30, [x6, 32]
	ldp	q31, q30, [x0, 96]
	stp	q29, q28, [x6, 64]
	ldp	q29, q28, [x0, 128]
	stp	q31, q30, [x6, 96]
	ldp	q31, q30, [x0, 160]
	stp	q29, q28, [x6, 128]
	ldr	x0, [x0, 192]
	stp	q31, q30, [x6, 160]
	str	x0, [x6, 192]
L31:
	mov	x2, x9
	mov	w1, w8
L30:
	umull	x0, w1, w7
	lsr	x0, x0, 34
	add	w0, w0, w0, lsl 2
	sub	w0, w1, w0
	add	w1, w1, 3
	sbfiz	x3, x0, 2, 32
	add	x0, x3, w0, sxtw
	add	x0, x0, x5
	ldr	x0, [x6, x0, lsl 3]
	str	x0, [x2], 8
	cmp	w4, w1
	bne	L30
	add	w8, w8, 1
	add	x5, x5, 1
	add	x9, x9, 40
	add	w4, w4, 1
	cmp	w8, 5
	bne	L31
	add	sp, sp, 208
LCFI7:
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__Tround_constants_arrayBIP
_anubis_keccak__Tround_constants_arrayBIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__Trho_offsets_arrayBIP
_anubis_keccak__Trho_offsets_arrayBIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__Tstate_arrayBIP
_anubis_keccak__Tstate_arrayBIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__theta
_anubis_keccak__theta:
LFB5:
	stp	x29, x30, [sp, -32]!
LCFI8:
	mov	x29, sp
LCFI9:
	add	x16, x29, 16
	str	x0, [x29, 16]
	bl	_anubis_keccak__theta___wrapped_statements.0
	ldp	x29, x30, [sp], 32
LCFI10:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__rho
_anubis_keccak__rho:
LFB7:
	stp	x29, x30, [sp, -32]!
LCFI11:
	mov	x29, sp
LCFI12:
	add	x16, x29, 16
	str	x0, [x29, 16]
	bl	_anubis_keccak__rho___wrapped_statements.1
	ldp	x29, x30, [sp], 32
LCFI13:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__pi
_anubis_keccak__pi:
LFB9:
	stp	x29, x30, [sp, -32]!
LCFI14:
	mov	x29, sp
LCFI15:
	add	x16, x29, 16
	str	x0, [x29, 16]
	bl	_anubis_keccak__pi___wrapped_statements.2
	ldp	x29, x30, [sp], 32
LCFI16:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__chi
_anubis_keccak__chi:
LFB11:
	stp	x29, x30, [sp, -32]!
LCFI17:
	mov	x29, sp
LCFI18:
	add	x16, x29, 16
	str	x0, [x29, 16]
	bl	_anubis_keccak__chi___wrapped_statements.3
	ldp	x29, x30, [sp], 32
LCFI19:
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__iota
_anubis_keccak__iota:
LFB13:
	and	w2, w1, 255
	cmp	w2, 23
	bhi	L51
	adrp	x3, _anubis_keccak__round_constants@PAGE
	ldr	x2, [x0]
	add	x3, x3, _anubis_keccak__round_constants@PAGEOFF;
	ldr	x1, [x3, w1, sxtw 3]
	eor	x1, x2, x1
	str	x1, [x0]
	ret
L51:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI20:
	mov	w1, 133
	mov	x29, sp
LCFI21:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_keccak__keccak_f
_anubis_keccak__keccak_f:
LFB15:
	stp	x29, x30, [sp, -144]!
LCFI22:
	mov	x29, sp
LCFI23:
	stp	x19, x20, [sp, 16]
LCFI24:
	adrp	x20, _anubis_keccak__round_constants@PAGE
	mov	x19, x0
	add	x20, x20, _anubis_keccak__round_constants@PAGEOFF;
	stp	x21, x22, [sp, 32]
LCFI25:
	add	x22, x29, 112
	add	x21, x29, 128
	stp	x23, x24, [sp, 48]
LCFI26:
	add	x24, x29, 80
	add	x23, x29, 96
	str	x25, [sp, 64]
LCFI27:
	add	x25, x20, 192
	.p2align 5,,15
L53:
	mov	x16, x24
	str	x19, [x29, 80]
	bl	_anubis_keccak__theta___wrapped_statements.0
	mov	x16, x23
	str	x19, [x29, 96]
	bl	_anubis_keccak__rho___wrapped_statements.1
	mov	x16, x22
	str	x19, [x29, 112]
	bl	_anubis_keccak__pi___wrapped_statements.2
	mov	x16, x21
	str	x19, [x29, 128]
	bl	_anubis_keccak__chi___wrapped_statements.3
	ldr	x1, [x20], 8
	ldr	x0, [x19]
	eor	x0, x0, x1
	str	x0, [x19]
	cmp	x25, x20
	bne	L53
	ldr	x25, [sp, 64]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 144
LCFI28:
	ret
LFE15:
	.const
	.align	4
_CSWTCH.52:
	.byte	2
	.byte	3
	.byte	4
	.byte	0
	.globl _anubis_keccak__rho_offsets
	.align	2
_anubis_keccak__rho_offsets:
	.word	0
	.word	36
	.word	3
	.word	41
	.word	18
	.word	1
	.word	44
	.word	10
	.word	45
	.word	2
	.word	62
	.word	6
	.word	43
	.word	15
	.word	61
	.word	28
	.word	55
	.word	25
	.word	21
	.word	56
	.word	27
	.word	20
	.word	39
	.word	8
	.word	14
	.globl _anubis_keccak__round_constants
	.align	3
_anubis_keccak__round_constants:
	.xword	1
	.xword	32898
	.xword	-9223372036854742902
	.xword	-9223372034707259392
	.xword	32907
	.xword	2147483649
	.xword	-9223372034707259263
	.xword	-9223372036854743031
	.xword	138
	.xword	136
	.xword	2147516425
	.xword	2147483658
	.xword	2147516555
	.xword	-9223372036854775669
	.xword	-9223372036854742903
	.xword	-9223372036854743037
	.xword	-9223372036854743038
	.xword	-9223372036854775680
	.xword	32778
	.xword	-9223372034707292150
	.xword	-9223372034707259263
	.xword	-9223372036854742912
	.xword	2147483649
	.xword	-9223372034707259384
	.globl _anubis_keccak_E
	.data
	.align	1
_anubis_keccak_E:
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
	.quad	LFB6-.
	.set L$set$2,LFE6-LFB6
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB6
	.long L$set$3
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$5,LEFDE3-LASFDE3
	.long L$set$5
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB12-.
	.set L$set$6,LFE12-LFB12
	.quad L$set$6
	.uleb128 0
	.byte	0x4
	.set L$set$7,LCFI2-LFB12
	.long L$set$7
	.byte	0xe
	.uleb128 0x30
	.byte	0x4
	.set L$set$8,LCFI3-LCFI2
	.long L$set$8
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$9,LEFDE5-LASFDE5
	.long L$set$9
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB8-.
	.set L$set$10,LFE8-LFB8
	.quad L$set$10
	.uleb128 0
	.byte	0x4
	.set L$set$11,LCFI4-LFB8
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
	.quad	LFB10-.
	.set L$set$14,LFE10-LFB10
	.quad L$set$14
	.uleb128 0
	.byte	0x4
	.set L$set$15,LCFI6-LFB10
	.long L$set$15
	.byte	0xe
	.uleb128 0xd0
	.byte	0x4
	.set L$set$16,LCFI7-LCFI6
	.long L$set$16
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$17,LEFDE9-LASFDE9
	.long L$set$17
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB2-.
	.set L$set$18,LFE2-LFB2
	.quad L$set$18
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$19,LEFDE11-LASFDE11
	.long L$set$19
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB3-.
	.set L$set$20,LFE3-LFB3
	.quad L$set$20
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$21,LEFDE13-LASFDE13
	.long L$set$21
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB4-.
	.set L$set$22,LFE4-LFB4
	.quad L$set$22
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$23,LEFDE15-LASFDE15
	.long L$set$23
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB5-.
	.set L$set$24,LFE5-LFB5
	.quad L$set$24
	.uleb128 0
	.byte	0x4
	.set L$set$25,LCFI8-LFB5
	.long L$set$25
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$26,LCFI9-LCFI8
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$27,LCFI10-LCFI9
	.long L$set$27
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$28,LEFDE17-LASFDE17
	.long L$set$28
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB7-.
	.set L$set$29,LFE7-LFB7
	.quad L$set$29
	.uleb128 0
	.byte	0x4
	.set L$set$30,LCFI11-LFB7
	.long L$set$30
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$31,LCFI12-LCFI11
	.long L$set$31
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$32,LCFI13-LCFI12
	.long L$set$32
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$33,LEFDE19-LASFDE19
	.long L$set$33
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB9-.
	.set L$set$34,LFE9-LFB9
	.quad L$set$34
	.uleb128 0
	.byte	0x4
	.set L$set$35,LCFI14-LFB9
	.long L$set$35
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$36,LCFI15-LCFI14
	.long L$set$36
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$37,LCFI16-LCFI15
	.long L$set$37
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$38,LEFDE21-LASFDE21
	.long L$set$38
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB11-.
	.set L$set$39,LFE11-LFB11
	.quad L$set$39
	.uleb128 0
	.byte	0x4
	.set L$set$40,LCFI17-LFB11
	.long L$set$40
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$41,LCFI18-LCFI17
	.long L$set$41
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$42,LCFI19-LCFI18
	.long L$set$42
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$43,LEFDE23-LASFDE23
	.long L$set$43
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB13-.
	.set L$set$44,LFE13-LFB13
	.quad L$set$44
	.uleb128 0
	.byte	0x4
	.set L$set$45,LCFI20-LFB13
	.long L$set$45
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$46,LCFI21-LCFI20
	.long L$set$46
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$47,LEFDE25-LASFDE25
	.long L$set$47
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB15-.
	.set L$set$48,LFE15-LFB15
	.quad L$set$48
	.uleb128 0
	.byte	0x4
	.set L$set$49,LCFI22-LFB15
	.long L$set$49
	.byte	0xe
	.uleb128 0x90
	.byte	0x9d
	.uleb128 0x12
	.byte	0x9e
	.uleb128 0x11
	.byte	0x4
	.set L$set$50,LCFI23-LCFI22
	.long L$set$50
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$51,LCFI24-LCFI23
	.long L$set$51
	.byte	0x93
	.uleb128 0x10
	.byte	0x94
	.uleb128 0xf
	.byte	0x4
	.set L$set$52,LCFI25-LCFI24
	.long L$set$52
	.byte	0x95
	.uleb128 0xe
	.byte	0x96
	.uleb128 0xd
	.byte	0x4
	.set L$set$53,LCFI26-LCFI25
	.long L$set$53
	.byte	0x97
	.uleb128 0xc
	.byte	0x98
	.uleb128 0xb
	.byte	0x4
	.set L$set$54,LCFI27-LCFI26
	.long L$set$54
	.byte	0x99
	.uleb128 0xa
	.byte	0x4
	.set L$set$55,LCFI28-LCFI27
	.long L$set$55
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
LEFDE25:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
