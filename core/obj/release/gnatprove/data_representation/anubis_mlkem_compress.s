	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC4:
	.ascii "Loop_Invariant failed at anubis_mlkem_compress.adb:25"
	.align	3
lC5:
	.ascii "failed postcondition from anubis_mlkem_compress.ads:34"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_compress__compress_du
_anubis_mlkem_compress__compress_du:
LFB2:
	mov	x5, x1
	stp	x29, x30, [sp, -32]!
LCFI0:
	mov	x29, sp
LCFI1:
	mov	x2, 512
	mov	w1, 0
	str	x19, [sp, 16]
LCFI2:
	mov	x19, x0
	mov	x0, x5
	bl	_memset
	mov	w7, 30337
	mov	x5, x0
	add	x6, x19, 2
	add	x4, x0, 2
	mov	x3, -1
	movk	w7, 0x3afb, lsl 16
	.p2align 5,,15
L7:
	cmn	w3, #1
	bne	L15
	ldrh	w0, [x6, -2]
	mov	x3, 0
	lsl	w0, w0, 11
	add	w0, w0, 1664
	umull	x1, w0, w7
	lsr	x1, x1, 32
	sub	w0, w0, w1
	add	w0, w1, w0, lsr 1
	ubfx	x0, x0, 11, 11
	strh	w0, [x4, -2]
L15:
	add	x2, x4, x3, lsl 1
	mov	x0, x5
	b	L4
	.p2align 2,,3
L19:
	add	x0, x0, 2
	cmp	x2, x0
	beq	L18
L4:
	ldrh	w1, [x0]
	cmp	w1, 2047
	bls	L19
	adrp	x0, lC4@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L18:
	ldrh	w0, [x6, x3, lsl 1]
	lsl	w0, w0, 11
	add	w0, w0, 1664
	umull	x1, w0, w7
	lsr	x1, x1, 32
	sub	w0, w0, w1
	add	w0, w1, w0, lsr 1
	ubfx	x0, x0, 11, 11
	strh	w0, [x4, x3, lsl 1]
	add	x3, x3, 1
	cmp	x3, 255
	bne	L7
	mov	x0, x5
	add	x2, x5, 512
	b	L9
	.p2align 2,,3
L21:
	add	x0, x0, 2
	cmp	x2, x0
	beq	L20
L9:
	ldrh	w1, [x0]
	cmp	w1, 2047
	bls	L21
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L20:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI3:
	ret
LFE2:
	.const
	.align	2
lC1:
	.word	1
	.word	53
	.align	2
lC0:
	.word	1
	.word	54
	.text
	.const
	.align	3
lC6:
	.ascii "Loop_Invariant failed at anubis_mlkem_compress.adb:50"
	.align	3
lC7:
	.ascii "failed postcondition from anubis_mlkem_compress.ads:41"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_compress__compress_dv
_anubis_mlkem_compress__compress_dv:
LFB4:
	mov	x5, x1
	stp	x29, x30, [sp, -32]!
LCFI4:
	mov	x29, sp
LCFI5:
	mov	x2, 512
	mov	w1, 0
	str	x19, [sp, 16]
LCFI6:
	mov	x19, x0
	mov	x0, x5
	bl	_memset
	mov	w7, 30337
	mov	x5, x0
	add	x6, x19, 2
	add	x4, x0, 2
	mov	x3, -1
	movk	w7, 0x3afb, lsl 16
	.p2align 5,,15
L28:
	cmn	w3, #1
	bne	L36
	ldrh	w0, [x6, -2]
	mov	x3, 0
	lsl	w0, w0, 5
	add	w0, w0, 1664
	umull	x1, w0, w7
	lsr	x1, x1, 32
	sub	w0, w0, w1
	add	w0, w1, w0, lsr 1
	ubfx	x0, x0, 11, 5
	strh	w0, [x4, -2]
L36:
	add	x2, x4, x3, lsl 1
	mov	x0, x5
	b	L25
	.p2align 2,,3
L39:
	add	x0, x0, 2
	cmp	x2, x0
	beq	L38
L25:
	ldrh	w1, [x0]
	cmp	w1, 31
	bls	L39
	adrp	x0, lC6@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L38:
	ldrh	w0, [x6, x3, lsl 1]
	lsl	w0, w0, 5
	add	w0, w0, 1664
	umull	x1, w0, w7
	lsr	x1, x1, 32
	sub	w0, w0, w1
	add	w0, w1, w0, lsr 1
	ubfx	x0, x0, 11, 5
	strh	w0, [x4, x3, lsl 1]
	add	x3, x3, 1
	cmp	x3, 255
	bne	L28
	mov	x0, x5
	add	x2, x5, 512
	b	L30
	.p2align 2,,3
L41:
	add	x0, x0, 2
	cmp	x2, x0
	beq	L40
L30:
	ldrh	w1, [x0]
	cmp	w1, 31
	bls	L41
	adrp	x0, lC7@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L40:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI7:
	ret
LFE4:
	.const
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_mlkem_compress.adb:77"
	.align	3
lC9:
	.ascii "anubis_mlkem_compress.adb:87"
	.align	3
lC10:
	.ascii "failed postcondition from anubis_mlkem_compress.ads:54"
	.align	3
lC11:
	.ascii "failed precondition from anubis_mlkem_compress.ads:53"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_compress__decompress_du
_anubis_mlkem_compress__decompress_du:
LFB6:
	stp	x29, x30, [sp, -32]!
LCFI8:
	mov	x29, sp
LCFI9:
	mov	x4, x1
	add	x2, x0, 512
	str	x19, [sp, 16]
LCFI10:
	mov	x19, x0
	b	L44
	.p2align 2,,3
L68:
	add	x0, x0, 2
	cmp	x0, x2
	beq	L67
L44:
	ldrh	w1, [x0]
	cmp	w1, 2047
	bls	L68
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L67:
	mov	x0, x4
	mov	x2, 512
	mov	w1, 0
	bl	_memset
	mov	w7, 2047
	mov	x4, x0
	add	x6, x19, 2
	mov	x3, -1
	mov	w8, 3329
	movk	w7, 0x68, lsl 16
	.p2align 5,,15
L52:
	cmn	w3, #1
	beq	L46
L64:
	add	x5, x4, 2
	mov	x0, x4
	add	x2, x5, x3, lsl 1
	b	L48
	.p2align 2,,3
L70:
	add	x0, x0, 2
	cmp	x2, x0
	beq	L69
L48:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L70
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L69:
	ldrh	w0, [x6, x3, lsl 1]
	mul	w0, w0, w8
	add	w0, w0, 1024
	lsr	w1, w0, 11
	cmp	w0, w7
	bhi	L56
	strh	w1, [x5, x3, lsl 1]
	add	x3, x3, 1
	cmp	x3, 255
	bne	L52
	add	x1, x4, 512
	b	L54
	.p2align 2,,3
L72:
	add	x4, x4, 2
	cmp	x1, x4
	beq	L71
L54:
	ldrh	w0, [x4]
	cmp	w0, 3328
	bls	L72
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L46:
	ldrh	w0, [x6, -2]
	mul	w0, w0, w8
	add	w0, w0, 1024
	lsr	w1, w0, 11
	cmp	w0, w7
	bhi	L56
	mov	x3, 0
	strh	w1, [x4]
	b	L64
L71:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI11:
	ret
L56:
LCFI12:
	adrp	x0, lC9@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE6:
	.const
	.align	2
lC2:
	.word	1
	.word	28
	.text
	.const
	.align	3
lC12:
	.ascii "Loop_Invariant failed at anubis_mlkem_compress.adb:106"
	.align	3
lC13:
	.ascii "anubis_mlkem_compress.adb:113"
	.align	3
lC14:
	.ascii "failed postcondition from anubis_mlkem_compress.ads:62"
	.align	3
lC15:
	.ascii "failed precondition from anubis_mlkem_compress.ads:61"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_compress__decompress_dv
_anubis_mlkem_compress__decompress_dv:
LFB8:
	stp	x29, x30, [sp, -32]!
LCFI13:
	mov	x29, sp
LCFI14:
	mov	x4, x1
	add	x2, x0, 512
	str	x19, [sp, 16]
LCFI15:
	mov	x19, x0
	b	L75
	.p2align 2,,3
L99:
	add	x0, x0, 2
	cmp	x0, x2
	beq	L98
L75:
	ldrh	w1, [x0]
	cmp	w1, 31
	bls	L99
	adrp	x0, lC15@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L98:
	mov	x0, x4
	mov	x2, 512
	mov	w1, 0
	bl	_memset
	mov	w7, 40991
	mov	x4, x0
	add	x6, x19, 2
	mov	x3, -1
	mov	w8, 3329
	movk	w7, 0x1, lsl 16
	.p2align 5,,15
L83:
	cmn	w3, #1
	beq	L77
L95:
	add	x5, x4, 2
	mov	x0, x4
	add	x2, x5, x3, lsl 1
	b	L79
	.p2align 2,,3
L101:
	add	x0, x0, 2
	cmp	x2, x0
	beq	L100
L79:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L101
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L100:
	ldrh	w0, [x6, x3, lsl 1]
	mul	w0, w0, w8
	add	w0, w0, 16
	lsr	w1, w0, 5
	cmp	w0, w7
	bhi	L87
	strh	w1, [x5, x3, lsl 1]
	add	x3, x3, 1
	cmp	x3, 255
	bne	L83
	add	x1, x4, 512
	b	L85
	.p2align 2,,3
L103:
	add	x4, x4, 2
	cmp	x1, x4
	beq	L102
L85:
	ldrh	w0, [x4]
	cmp	w0, 3328
	bls	L103
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L77:
	ldrh	w0, [x6, -2]
	mul	w0, w0, w8
	add	w0, w0, 16
	lsr	w1, w0, 5
	cmp	w0, w7
	bhi	L87
	mov	x3, 0
	strh	w1, [x4]
	b	L95
L102:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI16:
	ret
L87:
LCFI17:
	adrp	x0, lC13@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE8:
	.const
	.align	2
lC3:
	.word	1
	.word	29
	.text
	.const
	.align	3
lC16:
	.ascii "Loop_Invariant failed at anubis_mlkem_compress.adb:128"
	.align	3
lC17:
	.ascii "failed postcondition from anubis_mlkem_compress.ads:70"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_compress__compress_vector_du
_anubis_mlkem_compress__compress_vector_du:
LFB10:
	stp	x29, x30, [sp, -80]!
LCFI18:
	mov	x29, sp
LCFI19:
	mov	x2, 2048
	stp	x21, x22, [sp, 32]
LCFI20:
	mov	x21, x1
	add	x22, x21, 512
	stp	x19, x20, [sp, 16]
LCFI21:
	mov	w20, -1
	stp	x23, x24, [sp, 48]
LCFI22:
	add	x24, x21, 1024
	mov	w23, 512
	stp	x25, x26, [sp, 64]
LCFI23:
	mov	x25, x0
	mov	x26, x21
	mov	x0, x1
	mov	w1, 0
	bl	_memset
L111:
	cmn	w20, #1
	bne	L110
	mov	x1, x26
	mov	x0, x25
	add	x26, x26, 512
	add	x25, x25, 512
	mov	w20, 0
	bl	_anubis_mlkem_compress__compress_du
L110:
	umaddl	x5, w20, w23, x24
	mov	x19, x22
	mov	x4, x22
	.p2align 5,,15
L108:
	sub	x2, x4, #512
	b	L107
	.p2align 2,,3
L125:
	add	x2, x2, 2
	cmp	x2, x4
	beq	L124
L107:
	ldrh	w3, [x2]
	cmp	w3, 2047
	bls	L125
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L124:
	add	x4, x2, 512
	cmp	x4, x5
	bne	L108
	mov	x1, x26
	mov	x0, x25
	add	w20, w20, 1
	add	x25, x25, 512
	bl	_anubis_mlkem_compress__compress_du
	add	x26, x26, 512
	cmp	w20, 3
	bne	L111
	add	x21, x21, 2560
L114:
	sub	x0, x19, #512
	b	L113
	.p2align 2,,3
L127:
	add	x0, x0, 2
	cmp	x19, x0
	beq	L126
L113:
	ldrh	w1, [x0]
	cmp	w1, 2047
	bls	L127
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L126:
	add	x19, x19, 512
	cmp	x19, x21
	bne	L114
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 80
LCFI24:
	ret
LFE10:
	.const
	.align	3
lC18:
	.ascii "Loop_Invariant failed at anubis_mlkem_compress.adb:146"
	.align	3
lC19:
	.ascii "Loop_Invariant failed at anubis_mlkem_compress.adb:150"
	.align	3
lC20:
	.ascii "failed postcondition from anubis_mlkem_compress.ads:80"
	.align	3
lC21:
	.ascii "failed precondition from anubis_mlkem_compress.ads:78"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mlkem_compress__decompress_vector_du
_anubis_mlkem_compress__decompress_vector_du:
LFB12:
	stp	x29, x30, [sp, -80]!
LCFI25:
	mov	x29, sp
LCFI26:
	add	x2, x0, 512
	add	x3, x0, 2560
	stp	x19, x20, [sp, 16]
LCFI27:
	mov	x19, x1
	stp	x23, x24, [sp, 48]
LCFI28:
	mov	x24, x0
	stp	x21, x22, [sp, 32]
	stp	x25, x26, [sp, 64]
LCFI29:
L131:
	sub	x0, x2, #512
	b	L130
	.p2align 2,,3
L163:
	add	x0, x0, 2
	cmp	x0, x2
	beq	L162
L130:
	ldrh	w1, [x0]
	cmp	w1, 2047
	bls	L163
	adrp	x0, lC21@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC21@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L162:
	add	x2, x0, 512
	cmp	x2, x3
	bne	L131
	mov	x2, 2048
	mov	w1, 0
	mov	x0, x19
	mov	x26, x19
	add	x21, x19, 1024
	mov	w23, -1
	mov	w20, 512
	bl	_memset
L144:
	add	x25, x24, 512
	mov	w4, w23
	mov	x3, x25
	.p2align 5,,15
L135:
	add	w4, w4, 1
	sub	x0, x3, #512
	b	L134
	.p2align 2,,3
L165:
	add	x0, x0, 2
	cmp	x0, x3
	beq	L164
L134:
	ldrh	w2, [x0]
	cmp	w2, 2047
	bls	L165
	adrp	x0, lC18@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L164:
	add	x3, x0, 512
	cmp	w4, 3
	bne	L135
	cmn	w23, #1
	beq	L137
	add	x22, x19, 512
	umaddl	x4, w23, w20, x21
	mov	x3, x22
	.p2align 5,,15
L140:
	sub	x0, x3, #512
	b	L139
	.p2align 2,,3
L167:
	add	x0, x0, 2
	cmp	x0, x3
	beq	L166
L139:
	ldrh	w2, [x0]
	cmp	w2, 3328
	bls	L167
	adrp	x0, lC19@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L166:
	add	x3, x0, 512
	cmp	x3, x4
	bne	L140
	mov	x0, x24
	mov	x1, x26
	add	w23, w23, 1
	bl	_anubis_mlkem_compress__decompress_du
	cmp	w23, 3
	beq	L143
	add	x26, x26, 512
	mov	x24, x25
	b	L144
L137:
	mov	x0, x24
	mov	x1, x26
	mov	w23, 0
	add	x26, x26, 512
	mov	x24, x25
	bl	_anubis_mlkem_compress__decompress_du
	b	L144
L143:
	add	x19, x19, 2560
L147:
	sub	x0, x22, #512
	b	L146
	.p2align 2,,3
L169:
	add	x0, x0, 2
	cmp	x22, x0
	beq	L168
L146:
	ldrh	w1, [x0]
	cmp	w1, 3328
	bls	L169
	adrp	x0, lC20@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L168:
	add	x22, x22, 512
	cmp	x19, x22
	bne	L147
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 80
LCFI30:
	ret
LFE12:
	.globl _anubis_mlkem_compress_E
	.data
	.align	1
_anubis_mlkem_compress_E:
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
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
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
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$13,LEFDE5-LASFDE5
	.long L$set$13
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB6-.
	.set L$set$14,LFE6-LFB6
	.quad L$set$14
	.uleb128 0
	.byte	0x4
	.set L$set$15,LCFI8-LFB6
	.long L$set$15
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$16,LCFI9-LCFI8
	.long L$set$16
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$17,LCFI10-LCFI9
	.long L$set$17
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$20,LEFDE7-LASFDE7
	.long L$set$20
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB8-.
	.set L$set$21,LFE8-LFB8
	.quad L$set$21
	.uleb128 0
	.byte	0x4
	.set L$set$22,LCFI13-LFB8
	.long L$set$22
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$23,LCFI14-LCFI13
	.long L$set$23
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$24,LCFI15-LCFI14
	.long L$set$24
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$25,LCFI16-LCFI15
	.long L$set$25
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$26,LCFI17-LCFI16
	.long L$set$26
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$27,LEFDE9-LASFDE9
	.long L$set$27
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB10-.
	.set L$set$28,LFE10-LFB10
	.quad L$set$28
	.uleb128 0
	.byte	0x4
	.set L$set$29,LCFI18-LFB10
	.long L$set$29
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$30,LCFI19-LCFI18
	.long L$set$30
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$31,LCFI20-LCFI19
	.long L$set$31
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x4
	.set L$set$32,LCFI21-LCFI20
	.long L$set$32
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$33,LCFI22-LCFI21
	.long L$set$33
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x4
	.set L$set$34,LCFI23-LCFI22
	.long L$set$34
	.byte	0x99
	.uleb128 0x2
	.byte	0x9a
	.uleb128 0x1
	.byte	0x4
	.set L$set$35,LCFI24-LCFI23
	.long L$set$35
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
LEFDE9:
LSFDE11:
	.set L$set$36,LEFDE11-LASFDE11
	.long L$set$36
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB12-.
	.set L$set$37,LFE12-LFB12
	.quad L$set$37
	.uleb128 0
	.byte	0x4
	.set L$set$38,LCFI25-LFB12
	.long L$set$38
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$39,LCFI26-LCFI25
	.long L$set$39
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$40,LCFI27-LCFI26
	.long L$set$40
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$41,LCFI28-LCFI27
	.long L$set$41
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x4
	.set L$set$42,LCFI29-LCFI28
	.long L$set$42
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x99
	.uleb128 0x2
	.byte	0x9a
	.uleb128 0x1
	.byte	0x4
	.set L$set$43,LCFI30-LCFI29
	.long L$set$43
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
LEFDE11:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
