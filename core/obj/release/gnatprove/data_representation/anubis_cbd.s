	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_cbd__TpolynomialBIP
_anubis_cbd__TpolynomialBIP:
LFB2:
	ret
LFE2:
	.const
	.align	3
lC2:
	.ascii "anubis_cbd.adb"
	.space 1
	.align	3
lC3:
	.ascii "failed postcondition from anubis_cbd.ads:42"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_cbd__cbd_2
_anubis_cbd__cbd_2:
LFB5:
	add	x10, x1, 512
	mov	x6, x1
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	w5, 3329
	mov	w8, 65535
	mov	w9, 85
	mov	x29, sp
LCFI1:
	.p2align 5,,15
L28:
	ldrb	w2, [x0]
	ldrb	w3, [x0, 1]
	and	w7, w2, 1
	ubfx	x4, x2, 1, 1
	add	w4, w4, w7
	and	w11, w3, 1
	ubfx	x7, x3, 1, 1
	add	w7, w7, w11
	sub	w4, w4, w7
	add	w4, w4, 3329
	udiv	w7, w4, w5
	msub	w7, w7, w5, w4
	cmp	w7, w8
	bgt	L36
	ubfx	x4, x2, 2, 1
	ubfx	x12, x2, 3, 1
	strh	w7, [x6]
	ubfx	x11, x3, 3, 1
	ubfx	x7, x3, 2, 1
	add	w7, w7, w11
	add	w4, w4, w12
	sub	w4, w4, w7
	add	w4, w4, 3329
	udiv	w7, w4, w5
	msub	w7, w7, w5, w4
	cmp	w7, w8
	bgt	L37
	ubfx	x4, x2, 4, 1
	ubfx	x12, x2, 5, 1
	strh	w7, [x6, 2]
	ubfx	x11, x3, 5, 1
	ubfx	x7, x3, 4, 1
	add	w7, w7, w11
	add	w4, w4, w12
	sub	w4, w4, w7
	add	w4, w4, 3329
	udiv	w7, w4, w5
	msub	w4, w7, w5, w4
	cmp	w4, w8
	bgt	L38
	and	w7, w9, w2, lsr 6
	strh	w4, [x6, 4]
	and	w4, w9, w3, lsr 6
	add	w2, w7, w2, lsr 7
	add	w3, w4, w3, lsr 7
	sub	w2, w2, w3
	add	w2, w2, 3329
	udiv	w3, w2, w5
	msub	w2, w3, w5, w2
	cmp	w2, w8
	bgt	L39
	add	x6, x6, 8
	strh	w2, [x6, -2]
	add	x0, x0, 2
	cmp	x10, x6
	bne	L28
	ldrh	w0, [x1]
	cmp	w0, 3328
	bhi	L29
	.p2align 5,,15
L41:
	add	x1, x1, 2
	cmp	x10, x1
	beq	L40
	ldrh	w0, [x1]
	cmp	w0, 3328
	bls	L41
L29:
	adrp	x0, lC3@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC3@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L40:
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
L39:
LCFI3:
	adrp	x0, lC2@PAGE
	mov	w1, 62
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L38:
	adrp	x0, lC2@PAGE
	mov	w1, 57
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L37:
	adrp	x0, lC2@PAGE
	mov	w1, 52
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L36:
	adrp	x0, lC2@PAGE
	mov	w1, 47
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE5:
	.const
	.align	2
lC0:
	.word	1
	.word	43
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_cbd__cbd_3
_anubis_cbd__cbd_3:
LFB7:
	stp	x29, x30, [sp, -48]!
LCFI4:
	mov	x29, sp
LCFI5:
	add	x2, x29, 24
	mov	w13, 0
	mov	x11, x2
	mov	w4, 85
	ldp	x3, x5, [x0]
	mov	w9, 51
	mov	w8, 3329
	mov	w12, 65535
	ldr	x0, [x0, 16]
	ubfx	x10, x3, 16, 8
	and	w7, w3, 1048575
	str	x3, [x29, 24]
	stp	x5, x0, [x2, 8]
	.p2align 5,,15
L43:
	mov	x3, 0
	ldrb	w14, [x11, 3]
	lsl	w0, w14, 4
	orr	w10, w0, w10, lsr 4
L50:
	add	w2, w3, w3, lsl 1
	lsr	w0, w7, w2
	lsr	w2, w10, w2
	and	w6, w0, 7
	and	w5, w2, 7
	ubfx	x0, x0, 1, 2
	ubfx	x2, x2, 1, 2
	and	w6, w6, w4
	and	w5, w5, w4
	and	w0, w0, w4
	and	w2, w2, w4
	add	w0, w0, w6
	add	w2, w2, w5
	and	w6, w0, w9
	and	w5, w2, w9
	add	w2, w5, w2, lsr 2
	add	w0, w6, w0, lsr 2
	sub	w0, w0, w2
	add	w0, w0, 3329
	udiv	w2, w0, w8
	msub	w0, w2, w8, w0
	cmp	w0, w12
	bgt	L56
	strh	w0, [x1, x3, lsl 1]
	add	x3, x3, 1
	cmp	x3, 4
	bne	L50
	ldrb	w7, [x11, 4]
	add	w13, w13, 4
	add	x1, x1, 8
	add	x11, x11, 3
	ldrb	w10, [x11, 2]
	orr	w7, w14, w7, lsl 8
	ubfiz	w0, w10, 16, 4
	orr	w7, w7, w0
	cmp	w13, 28
	bne	L43
	adrp	x0, lC2@PAGE
	mov	w1, 102
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L56:
	adrp	x0, lC2@PAGE
	mov	w1, 120
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE7:
	.globl _anubis_cbd_E
	.data
	.align	1
_anubis_cbd_E:
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
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$6,LCFI1-LCFI0
	.long L$set$6
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$7,LCFI2-LCFI1
	.long L$set$7
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$8,LCFI3-LCFI2
	.long L$set$8
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$9,LEFDE5-LASFDE5
	.long L$set$9
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB7-.
	.set L$set$10,LFE7-LFB7
	.quad L$set$10
	.uleb128 0
	.byte	0x4
	.set L$set$11,LCFI4-LFB7
	.long L$set$11
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE5:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
