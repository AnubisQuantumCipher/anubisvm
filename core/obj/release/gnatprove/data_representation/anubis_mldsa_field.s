	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_field__field_arrayIP
_anubis_mldsa_field__field_arrayIP:
LFB2:
	ret
LFE2:
	.const
	.align	3
lC1:
	.ascii "anubis_mldsa_field.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_field__power2round
_anubis_mldsa_field__power2round:
LFB25:
	cmp	w0, 8380416
	bhi	L10
	and	w3, w0, 8191
	cmp	w3, 4096
	bhi	L11
	subs	w1, w0, w3
	mov	w0, 8191
	add	w2, w1, w0
	csel	w1, w2, w1, mi
	asr	w1, w1, 13
	orr	x0, x1, x3, lsl 32
	ret
	.p2align 2,,3
L11:
	sub	w1, w0, w3
	mov	w2, 16383
	add	w2, w1, w2
	sub	w3, w3, #8192
	adds	w1, w1, 8192
	csel	w1, w2, w1, mi
	asr	w1, w1, 13
	orr	x0, x1, x3, lsl 32
	ret
L10:
	adrp	x0, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	w1, 132
	mov	x29, sp
LCFI1:
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_field__decompose
_anubis_mldsa_field__decompose:
LFB27:
	cmp	w0, 8380416
	bhi	L24
	mov	w1, 523776
	mov	w4, 261888
	udiv	w2, w0, w1
	mul	w2, w2, w1
	sub	w3, w0, w2
	cmp	w3, w4
	bgt	L14
	cmp	w2, 8380416
	beq	L17
	mov	w1, 2051
	movk	w1, 0x8020, lsl 16
	smull	x1, w2, w1
	lsr	x1, x1, 32
	add	w1, w2, w1
	asr	w1, w1, 18
	sub	w0, w1, w2, asr 31
	uxtw	x0, w0
	orr	x0, x0, x3, lsl 32
	ret
	.p2align 2,,3
L14:
	sub	w3, w3, w1
	sub	w1, w0, w3
	cmp	w1, 8380416
	beq	L15
	mov	w0, 8201
	lsr	w1, w1, 9
	movk	w0, 0x80, lsl 16
	umull	x1, w1, w0
	lsr	x0, x1, 33
	uxtw	x0, w0
	orr	x0, x0, x3, lsl 32
	ret
	.p2align 2,,3
L17:
	mov	w3, 0
L15:
	mov	w0, 0
	sub	w3, w3, #1
	uxtw	x0, w0
	orr	x0, x0, x3, lsl 32
	ret
L24:
	adrp	x0, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI2:
	mov	w1, 167
	mov	x29, sp
LCFI3:
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE27:
	.globl _anubis_mldsa_field_E
	.data
	.align	1
_anubis_mldsa_field_E:
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
	.quad	LFB25-.
	.set L$set$4,LFE25-LFB25
	.quad L$set$4
	.uleb128 0
	.byte	0x4
	.set L$set$5,LCFI0-LFB25
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
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$7,LEFDE5-LASFDE5
	.long L$set$7
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB27-.
	.set L$set$8,LFE27-LFB27
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI2-LFB27
	.long L$set$9
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$10,LCFI3-LCFI2
	.long L$set$10
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE5:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
