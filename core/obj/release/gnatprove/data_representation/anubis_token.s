	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC2:
	.ascii "anubis_token.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_token__serialize_token_state__write_u64.12:
LFB51:
	ldp	x7, x1, [x16]
	mov	w2, 8
	mov	w8, 2147483647
	ldp	x6, x1, [x1]
	ldp	w5, w4, [x1]
L8:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	blt	L15
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	x29, sp
LCFI1:
L9:
	cmp	w5, w1
	bgt	L16
	sxtw	x3, w1
	sub	x3, x3, x7
	strb	w0, [x6, x3]
	lsr	x0, x0, 8
	cmp	w1, w8
	beq	L17
	add	w1, w1, 1
	subs	w2, w2, #1
	str	w1, [x16, 16]
	beq	L18
L5:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	bge	L9
	subs	w2, w2, #1
	bne	L5
L18:
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
	.p2align 2,,3
L15:
	subs	w2, w2, #1
	bne	L8
	ret
L17:
LCFI3:
	adrp	x0, lC2@PAGE
	mov	w1, 714
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L16:
	adrp	x0, lC2@PAGE
	mov	w1, 712
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE51:
	.align	2
	.p2align 5,,15
_anubis_token__deserialize_token_state__read_u64.13:
LFB53:
	stp	x29, x30, [sp, -64]!
LCFI4:
	mov	x29, sp
LCFI5:
	stp	x19, x20, [sp, 16]
LCFI6:
	mov	w20, -1
	mov	x19, x16
	stp	x21, x22, [sp, 32]
LCFI7:
	mov	w21, 2147483647
	str	x23, [sp, 48]
LCFI8:
	mov	x23, 0
	.p2align 5,,15
L23:
	ldr	x3, [x19, 8]
	add	w20, w20, 1
	ldr	w0, [x19, 16]
	ldr	x1, [x3, 8]
	ldr	w2, [x1, 4]
	cmp	w2, w0
	blt	L20
	ldr	w1, [x1]
	cmp	w1, w0
	bgt	L26
	ldr	x4, [x19]
	sxtw	x2, w0
	mov	w1, w20
	mov	x0, 256
	ldr	x3, [x3]
	sub	x2, x2, x4
	ldrb	w22, [x3, x2]
	bl	_system__exp_llu__exp_long_long_unsigned
	ldr	w1, [x19, 16]
	madd	x23, x22, x0, x23
	cmp	w1, w21
	beq	L27
	add	w1, w1, 1
	str	w1, [x19, 16]
L20:
	cmp	w20, 7
	bne	L23
	mov	x0, x23
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI9:
	ret
L26:
LCFI10:
	adrp	x0, lC2@PAGE
	mov	w1, 750
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L27:
	adrp	x0, lC2@PAGE
	mov	w1, 751
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE53:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__allocation_categoryH
_anubis_token__allocation_categoryH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L31
	sub	w1, w1, w2
	cmp	w1, 7
	bgt	L32
L31:
	mov	x3, 0
	mov	x1, 0
L29:
	adrp	x0, _allocation_categoryG.26@PAGE
	mov	w2, 7
	add	x0, x0, _allocation_categoryG.26@PAGEOFF;
	ldrb	w1, [x0, x1]
	ldrb	w0, [x0, x3]
	add	w1, w1, w0
	udiv	w2, w1, w2
	lsl	w0, w2, 3
	sub	w0, w0, w2
	sub	w0, w1, w0
	ret
	.p2align 2,,3
L32:
	ldrb	w2, [x0, 8]
	mov	w0, 61681
	movk	w0, 0xf0f0, lsl 16
	lsl	w1, w2, 4
	lsl	w3, w2, 3
	sub	w1, w1, w2
	umull	x2, w3, w0
	umull	x0, w1, w0
	lsr	x2, x2, 36
	lsr	x0, x0, 36
	add	w2, w2, w2, lsl 4
	add	w0, w0, w0, lsl 4
	sub	w3, w3, w2
	sub	w1, w1, w0
	sxtw	x3, w3
	sxtw	x1, w1
	b	L29
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__certification_levelH
_anubis_token__certification_levelH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L35
	ldrb	w2, [x0]
	mov	w1, 34953
	movk	w1, 0x8888, lsl 16
	add	w3, w2, w2, lsl 1
	lsl	w0, w2, 3
	sub	w0, w0, w2
	umull	x2, w3, w1
	umull	x1, w0, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	lsl	w5, w2, 4
	lsl	w4, w1, 4
	sub	w2, w5, w2
	sub	w1, w4, w1
	sub	w3, w3, w2
	sub	w0, w0, w1
	sxtw	x3, w3
	sxtw	x0, w0
L34:
	adrp	x2, _certification_levelG.22@PAGE
	mov	w1, 52429
	add	x2, x2, _certification_levelG.22@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x2, x0]
	ldrb	w2, [x2, x3]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
	.p2align 2,,3
L35:
	mov	x3, 0
	mov	x0, 0
	b	L34
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__transfer_resultH
_anubis_token__transfer_resultH:
LFB4:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _transfer_resultP.21@PAGE
	add	w5, w10, 1
	add	x9, x9, _transfer_resultP.21@PAGEOFF;
	adrp	x12, _transfer_resultT1.20@PAGE
	adrp	x11, _transfer_resultT2.19@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _transfer_resultT1.20@PAGEOFF;
	add	x11, x11, _transfer_resultT2.19@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 13
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L38
L42:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	add	w6, w2, w2, lsl 1
	add	w6, w2, w6, lsl 2
	add	w2, w5, w5, lsl 1
	sub	w6, w3, w6
	add	w2, w5, w2, lsl 2
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L38
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L42
L38:
	adrp	x3, _transfer_resultG.18@PAGE
	mov	w1, 43691
	add	x3, x3, _transfer_resultG.18@PAGEOFF;
	movk	w1, 0xaaaa, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 1
	sub	w0, w0, w1, lsl 1
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__burn_resultH
_anubis_token__burn_resultH:
LFB5:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L46
	sub	w1, w1, w2
	cmp	w1, 3
	bgt	L47
L46:
	adrp	x1, _burn_resultG.14@PAGE
	mov	x2, 0
	add	x1, x1, _burn_resultG.14@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L47:
	ldrb	w2, [x0, 4]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w0, w2, w2, lsl 1
	add	w2, w2, w2, lsl 3
	umull	x3, w0, w1
	umull	x1, w2, w1
	lsr	x3, x3, 35
	lsr	x1, x1, 35
	add	w5, w3, w3, lsl 2
	add	w4, w1, w1, lsl 2
	add	w3, w3, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w0, w0, w3
	sub	w2, w2, w1
	adrp	x1, _burn_resultG.14@PAGE
	sxtw	x2, w2
	add	x1, x1, _burn_resultG.14@PAGEOFF;
	sxtw	x0, w0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__account_stateIP
_anubis_token__account_stateIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__account_arrayIP
_anubis_token__account_arrayIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__token_stateIP
_anubis_token__token_stateIP:
LFB105:
	ret
LFE105:
	.const
	.align	3
lC3:
	.ascii "failed precondition from anubis_token.ads:169"
	.align	3
lC4:
	.ascii "failed postcondition from anubis_token.ads:170"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__transfer
_anubis_token__transfer:
LFB9:
	stp	x29, x30, [sp, -16]!
LCFI11:
	mov	x29, sp
LCFI12:
	cbz	x2, L67
	mov	x3, x0
	ldrb	w0, [x0, 48]
	cmp	w0, 1
	bhi	L68
	cbnz	w0, L59
	ldrb	w0, [x1, 48]
	cmp	w0, 1
	bhi	L69
	cbnz	w0, L60
	ldr	x0, [x3, 32]
	cmp	x2, x0
	bhi	L61
	ldr	x5, [x1, 32]
	adds	x4, x2, x5
	bcs	L58
	sub	x4, x0, x2
	mov	w0, 0
	str	x4, [x3, 32]
	ldr	x6, [x1, 32]
	add	x2, x2, x6
	str	x2, [x1, 32]
	ldr	x1, [x3, 32]
	cmp	x1, x4
	ccmp	x5, x6, 0, eq
	bne	L70
L64:
	ldp	x29, x30, [sp], 16
LCFI13:
	ret
	.p2align 2,,3
L60:
LCFI14:
	mov	w0, 5
	ldp	x29, x30, [sp], 16
LCFI15:
	ret
	.p2align 2,,3
L59:
LCFI16:
	mov	w0, 4
	ldp	x29, x30, [sp], 16
LCFI17:
	ret
	.p2align 2,,3
L61:
LCFI18:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI19:
	ret
L67:
LCFI20:
	adrp	x0, lC3@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC3@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L68:
	adrp	x0, lC2@PAGE
	mov	w1, 25
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L69:
	adrp	x0, lC2@PAGE
	mov	w1, 31
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L70:
	adrp	x0, lC4@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L58:
	mov	w0, 3
	b	L64
LFE9:
	.const
	.align	2
lC0:
	.word	1
	.word	45
	.align	2
lC1:
	.word	1
	.word	46
	.text
	.const
	.align	3
lC5:
	.ascii "failed precondition from anubis_token.ads:182"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__burn
_anubis_token__burn:
LFB11:
	cbz	x2, L78
	mov	x3, x0
	mov	w0, 1
	ldr	x4, [x3, 32]
	cmp	x2, x4
	bhi	L73
	ldp	x5, x6, [x1]
	sub	x4, x4, x2
	mov	w0, 0
	str	x4, [x3, 32]
	sub	x3, x5, x2
	add	x2, x2, x6
	stp	x3, x2, [x1]
L73:
	ret
L78:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	stp	x29, x30, [sp, -16]!
LCFI21:
	add	x0, x0, lC5@PAGEOFF;
	mov	x29, sp
LCFI22:
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE11:
	.const
	.align	3
lC6:
	.ascii "failed precondition from anubis_token.ads:198"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__genesis_mint
_anubis_token__genesis_mint:
LFB13:
	stp	x29, x30, [sp, -16]!
LCFI23:
	mov	x4, x0
	mov	x29, sp
LCFI24:
	ldr	x0, [x0, 24]
	cmp	x0, 0
	ccmp	x3, 0, 4, eq
	beq	L106
	cset	w0, eq
	cmp	w1, 6
	bhi	L107
	cmp	w1, 4
	bhi	L108
	cmp	w1, 3
	beq	L85
	adrp	x5, _CSWTCH.198@PAGE
	uxtw	x6, w1
	add	x5, x5, _CSWTCH.198@PAGEOFF;
	ldr	x5, [x5, x6, lsl 3]
	cmp	w1, 4
	beq	L86
	cmp	w1, 1
	beq	L87
	cmp	w1, 2
	beq	L88
	ldr	x6, [x4, 40]
	.p2align 5,,15
L89:
	cmp	x5, x6
	bls	L90
	sub	x5, x5, x6
	cmp	x3, x5
	bhi	L90
	ldr	x5, [x2, 32]
	adds	x5, x3, x5
	bcs	L90
	str	x5, [x2, 32]
	cmp	w1, 3
	beq	L93
	bhi	L94
	cmp	w1, 1
	beq	L95
	cmp	w1, 2
	beq	L109
	ldr	x0, [x4, 32]
	add	x0, x0, x3
	str	x0, [x4, 32]
	mov	w0, 1
L90:
	ldp	x29, x30, [sp], 16
LCFI25:
	ret
	.p2align 2,,3
L108:
LCFI26:
	cmp	w1, 5
	bne	L110
	mov	x5, 61568
	ldr	x6, [x4, 80]
	movk	x5, 0x2fa, lsl 16
	b	L89
	.p2align 2,,3
L86:
	ldr	x6, [x4, 72]
	b	L89
	.p2align 2,,3
L94:
	cmp	w1, 4
	beq	L98
	cmp	w1, 5
	bne	L111
	ldr	x0, [x4, 80]
	add	x0, x0, x3
	str	x0, [x4, 80]
	mov	w0, 1
	b	L90
	.p2align 2,,3
L110:
	mov	x5, 61568
	ldr	x6, [x4, 88]
	movk	x5, 0x2fa, lsl 16
	b	L89
	.p2align 2,,3
L88:
	ldr	x6, [x4, 56]
	b	L89
	.p2align 2,,3
L87:
	ldr	x6, [x4, 48]
	b	L89
	.p2align 2,,3
L85:
	mov	x5, 46080
	ldr	x6, [x4, 64]
	movk	x5, 0x4c4, lsl 16
	b	L89
	.p2align 2,,3
L111:
	ldr	x0, [x4, 88]
	add	x0, x0, x3
	str	x0, [x4, 88]
	mov	w0, 1
	b	L90
	.p2align 2,,3
L98:
	ldr	x0, [x4, 72]
	add	x0, x0, x3
	str	x0, [x4, 72]
	mov	w0, 1
	b	L90
	.p2align 2,,3
L109:
	ldr	x0, [x4, 56]
	add	x0, x0, x3
	str	x0, [x4, 56]
	mov	w0, 1
	b	L90
	.p2align 2,,3
L95:
	ldr	x0, [x4, 48]
	add	x0, x0, x3
	str	x0, [x4, 48]
	mov	w0, 1
	b	L90
	.p2align 2,,3
L93:
	ldr	x0, [x4, 64]
	add	x0, x0, x3
	str	x0, [x4, 64]
	mov	w0, 1
	b	L90
L106:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L107:
	adrp	x0, lC2@PAGE
	mov	w1, 90
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__balance_of
_anubis_token__balance_of:
LFB14:
	ldr	x0, [x0, 32]
	ret
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__get_total_supply
_anubis_token__get_total_supply:
LFB16:
	ldr	x0, [x0]
	ret
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__get_total_burned
_anubis_token__get_total_burned:
LFB18:
	ldr	x0, [x0, 8]
	ret
LFE18:
	.const
	.align	3
lC7:
	.ascii "anubis_token.ads"
	.space 1
	.align	3
lC8:
	.ascii "failed precondition from anubis_token.ads:228"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__apply_certification
_anubis_token__apply_certification:
LFB20:
	stp	x29, x30, [sp, -16]!
LCFI27:
	mov	x29, sp
LCFI28:
	cmp	w1, 4
	bhi	L127
	mov	x3, x0
	beq	L128
	adrp	x0, _CSWTCH.190@PAGE
	uxtw	x4, w1
	add	x0, x0, _CSWTCH.190@PAGEOFF;
	cmp	w1, 0
	ldr	x0, [x0, x4, lsl 3]
	ccmp	x2, x0, 0, ne
	bcc	L118
L119:
	ldrb	w4, [x3, 49]
	cmp	w4, 4
	bhi	L129
	cmp	w4, 0
	ccmp	x2, x0, 0, eq
	cset	w0, cc
	bcs	L130
	mov	w0, 0
L121:
	ldp	x29, x30, [sp], 16
LCFI29:
	ret
	.p2align 2,,3
L128:
LCFI30:
	mov	x0, 34463
	movk	x0, 0x1, lsl 16
	cmp	x2, x0
	bls	L118
	add	x0, x0, 1
	b	L119
	.p2align 2,,3
L130:
	ldr	x4, [x3, 32]
	cmp	x4, x2
	bcc	L121
	sub	x4, x4, x2
	mov	w0, 1
	strb	w1, [x3, 49]
	str	x2, [x3, 56]
	str	x4, [x3, 32]
	ldp	x29, x30, [sp], 16
LCFI31:
	ret
L118:
LCFI32:
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L127:
	adrp	x0, lC7@PAGE
	mov	w1, 228
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L129:
	adrp	x0, lC2@PAGE
	mov	w1, 155
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.const
	.align	3
lC9:
	.ascii "failed precondition from anubis_token.ads:239"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__upgrade_certification
_anubis_token__upgrade_certification:
LFB21:
	stp	x29, x30, [sp, -16]!
LCFI33:
	mov	x29, sp
LCFI34:
	cmp	w1, 4
	bhi	L133
	mov	x3, x0
	ldrb	w0, [x0, 49]
	cmp	w0, 4
	bhi	L133
	cmp	w1, w0
	bls	L141
	cmp	w1, 4
	beq	L137
	adrp	x0, _CSWTCH.190@PAGE
	uxtw	x4, w1
	add	x0, x0, _CSWTCH.190@PAGEOFF;
	ldr	x5, [x0, x4, lsl 3]
L135:
	mov	w0, 0
	ldr	x4, [x3, 56]
	add	x4, x2, x4
	cmp	x4, x5
	bcs	L142
L136:
	ldp	x29, x30, [sp], 16
LCFI35:
	ret
	.p2align 2,,3
L142:
LCFI36:
	ldr	x5, [x3, 32]
	cmp	x5, x2
	bcc	L136
	sub	x5, x5, x2
	mov	w0, 1
	strb	w1, [x3, 49]
	str	x4, [x3, 56]
	str	x5, [x3, 32]
	ldp	x29, x30, [sp], 16
LCFI37:
	ret
	.p2align 2,,3
L137:
LCFI38:
	mov	x5, 34464
	movk	x5, 0x1, lsl 16
	b	L135
L133:
	adrp	x0, lC7@PAGE
	mov	w1, 239
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L141:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE21:
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_token.ads:249"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__revoke_certification
_anubis_token__revoke_certification:
LFB22:
	stp	x29, x30, [sp, -16]!
LCFI39:
	mov	x3, x0
	mov	x29, sp
LCFI40:
	ldrb	w0, [x0, 49]
	cmp	w0, 4
	bhi	L152
	cbz	w0, L153
	ldr	x0, [x3, 56]
	cmp	w2, 1
	bhi	L154
	ldr	x4, [x3, 32]
	add	x4, x0, x4
	cbz	w2, L147
	ldp	x2, x5, [x1]
	lsr	x0, x0, 1
	sub	x4, x4, x0
	sub	x2, x2, x0
	add	x0, x5, x0
	stp	x2, x0, [x1]
L147:
	mov	w0, 1
	str	x4, [x3, 32]
	strb	wzr, [x3, 49]
	str	xzr, [x3, 56]
	ldp	x29, x30, [sp], 16
LCFI41:
	ret
L154:
LCFI42:
	adrp	x0, lC2@PAGE
	mov	w1, 229
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L153:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L152:
	adrp	x0, lC7@PAGE
	mov	w1, 249
	add	x0, x0, lC7@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__get_gas_discount
_anubis_token__get_gas_discount:
LFB23:
	uxtw	x1, w0
	cmp	w1, 4
	bhi	L162
	mov	w0, 3000
	beq	L155
	adrp	x0, _CSWTCH.164@PAGE
	add	x0, x0, _CSWTCH.164@PAGEOFF;
	ldr	w0, [x0, x1, lsl 2]
L155:
	ret
L162:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI43:
	mov	w1, 248
	mov	x29, sp
LCFI44:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__get_required_deposit
_anubis_token__get_required_deposit:
LFB25:
	cmp	w0, 4
	bhi	L170
	beq	L166
	adrp	x1, _CSWTCH.190@PAGE
	uxtw	x0, w0
	add	x1, x1, _CSWTCH.190@PAGEOFF;
	ldr	x0, [x1, x0, lsl 3]
	ret
	.p2align 2,,3
L166:
	mov	x0, 34464
	movk	x0, 0x1, lsl 16
	ret
L170:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI45:
	mov	w1, 259
	mov	x29, sp
LCFI46:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__burn_base_fee
_anubis_token__burn_base_fee:
LFB26:
	add	x1, x1, x1, lsl 2
	mov	x2, 9999
	add	x1, x1, x1, lsl 2
	add	x1, x1, x1, lsl 2
	lsl	x1, x1, 6
	cmp	x1, x2
	bls	L171
	mov	x3, 22859
	ldr	x2, [x0]
	movk	x3, 0x3886, lsl 16
	movk	x3, 0xc5d6, lsl 32
	movk	x3, 0x346d, lsl 48
	umulh	x1, x1, x3
	lsr	x1, x1, 11
	cmp	x2, x1
	bcc	L171
	ldr	x3, [x0, 8]
	sub	x2, x2, x1
	add	x1, x3, x1
	stp	x2, x1, [x0]
L171:
	ret
LFE26:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__burn_proposal_bond
_anubis_token__burn_proposal_bond:
LFB27:
	mov	x3, 10000
	mul	x2, x2, x3
	mov	x3, 9999
	cmp	x2, x3
	bls	L173
	mov	x4, 22859
	ldr	x3, [x0, 32]
	movk	x4, 0x3886, lsl 16
	movk	x4, 0xc5d6, lsl 32
	movk	x4, 0x346d, lsl 48
	umulh	x2, x2, x4
	lsr	x2, x2, 11
	cmp	x3, x2
	bcc	L173
	ldp	x4, x5, [x1]
	sub	x3, x3, x2
	str	x3, [x0, 32]
	sub	x0, x4, x2
	add	x2, x5, x2
	stp	x0, x2, [x1]
L173:
	ret
LFE27:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__burn_slashing
_anubis_token__burn_slashing:
LFB28:
	mov	x3, 5000
	mul	x2, x2, x3
	mov	x3, 9999
	cmp	x2, x3
	bls	L175
	mov	x4, 22859
	ldr	x3, [x0, 32]
	movk	x4, 0x3886, lsl 16
	movk	x4, 0xc5d6, lsl 32
	movk	x4, 0x346d, lsl 48
	umulh	x2, x2, x4
	lsr	x2, x2, 11
	cmp	x3, x2
	bcc	L175
	ldp	x4, x5, [x1]
	sub	x3, x3, x2
	str	x3, [x0, 32]
	sub	x0, x4, x2
	add	x2, x5, x2
	stp	x0, x2, [x1]
L175:
	ret
LFE28:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__burn_privacy_fee
_anubis_token__burn_privacy_fee:
LFB29:
	add	x1, x1, x1, lsl 2
	mov	x2, 9999
	add	x1, x1, x1, lsl 2
	add	x1, x1, x1, lsl 2
	lsl	x1, x1, 2
	cmp	x1, x2
	bls	L177
	mov	x3, 22859
	ldr	x2, [x0]
	movk	x3, 0x3886, lsl 16
	movk	x3, 0xc5d6, lsl 32
	movk	x3, 0x346d, lsl 48
	umulh	x1, x1, x3
	lsr	x1, x1, 11
	cmp	x2, x1
	bcc	L177
	ldr	x3, [x0, 8]
	sub	x2, x2, x1
	add	x1, x3, x1
	stp	x2, x1, [x0]
L177:
	ret
LFE29:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__get_allocation
_anubis_token__get_allocation:
LFB30:
	cmp	w0, 6
	bhi	L186
	cmp	w0, 4
	bhi	L182
	adrp	x1, _CSWTCH.198@PAGE
	uxtw	x0, w0
	add	x1, x1, _CSWTCH.198@PAGEOFF;
	ldr	x0, [x1, x0, lsl 3]
	ret
	.p2align 2,,3
L182:
	mov	x0, 61568
	movk	x0, 0x2fa, lsl 16
	ret
L186:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI47:
	mov	w1, 334
	mov	x29, sp
LCFI48:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE30:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__get_remaining
_anubis_token__get_remaining:
LFB31:
	cmp	w1, 6
	bhi	L204
	cmp	w1, 4
	bhi	L205
	cmp	w1, 3
	beq	L192
	adrp	x2, _CSWTCH.198@PAGE
	uxtw	x3, w1
	add	x2, x2, _CSWTCH.198@PAGEOFF;
	ldr	x2, [x2, x3, lsl 3]
	cmp	w1, 4
	beq	L193
	cmp	w1, 1
	beq	L194
	cmp	w1, 2
	beq	L195
	ldr	x0, [x0, 40]
	cmp	x0, x2
	sub	x0, x2, x0
	csel	x0, x0, xzr, cc
	ret
	.p2align 2,,3
L205:
	cmp	w1, 5
	bne	L206
	ldr	x0, [x0, 80]
	mov	x2, 61568
	movk	x2, 0x2fa, lsl 16
	cmp	x0, x2
	sub	x0, x2, x0
	csel	x0, x0, xzr, cc
	ret
	.p2align 2,,3
L193:
	ldr	x0, [x0, 72]
	cmp	x0, x2
	sub	x0, x2, x0
	csel	x0, x0, xzr, cc
	ret
	.p2align 2,,3
L195:
	ldr	x0, [x0, 56]
	cmp	x0, x2
	sub	x0, x2, x0
	csel	x0, x0, xzr, cc
	ret
	.p2align 2,,3
L194:
	ldr	x0, [x0, 48]
	cmp	x0, x2
	sub	x0, x2, x0
	csel	x0, x0, xzr, cc
	ret
	.p2align 2,,3
L192:
	ldr	x0, [x0, 64]
	mov	x2, 46080
	movk	x2, 0x4c4, lsl 16
	cmp	x0, x2
	sub	x0, x2, x0
	csel	x0, x0, xzr, cc
	ret
	.p2align 2,,3
L206:
	ldr	x0, [x0, 88]
	mov	x2, 61568
	movk	x2, 0x2fa, lsl 16
	cmp	x0, x2
	sub	x0, x2, x0
	csel	x0, x0, xzr, cc
	ret
L204:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI49:
	mov	w1, 349
	mov	x29, sp
LCFI50:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE31:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__is_exhausted
_anubis_token__is_exhausted:
LFB32:
	cmp	w1, 6
	bhi	L222
	cmp	w1, 4
	bhi	L223
	cmp	w1, 3
	beq	L212
	adrp	x2, _CSWTCH.198@PAGE
	uxtw	x3, w1
	add	x2, x2, _CSWTCH.198@PAGEOFF;
	ldr	x2, [x2, x3, lsl 3]
	cmp	w1, 4
	beq	L213
	cmp	w1, 1
	beq	L214
	cmp	w1, 2
	beq	L215
	ldr	x0, [x0, 40]
	cmp	x2, x0
	cset	w0, ls
	ret
	.p2align 2,,3
L223:
	cmp	w1, 5
	bne	L224
	ldr	x0, [x0, 80]
	mov	x2, 61568
	movk	x2, 0x2fa, lsl 16
	cmp	x2, x0
	cset	w0, ls
	ret
	.p2align 2,,3
L213:
	ldr	x0, [x0, 72]
	cmp	x2, x0
	cset	w0, ls
	ret
	.p2align 2,,3
L215:
	ldr	x0, [x0, 56]
	cmp	x2, x0
	cset	w0, ls
	ret
	.p2align 2,,3
L214:
	ldr	x0, [x0, 48]
	cmp	x2, x0
	cset	w0, ls
	ret
	.p2align 2,,3
L212:
	ldr	x0, [x0, 64]
	mov	x2, 46080
	movk	x2, 0x4c4, lsl 16
	cmp	x2, x0
	cset	w0, ls
	ret
	.p2align 2,,3
L224:
	ldr	x0, [x0, 88]
	mov	x2, 61568
	movk	x2, 0x2fa, lsl 16
	cmp	x2, x0
	cset	w0, ls
	ret
L222:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI51:
	mov	w1, 374
	mov	x29, sp
LCFI52:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE32:
	.const
	.align	3
lC11:
	.ascii "failed precondition from anubis_token.ads:327"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__to_display_string
_anubis_token__to_display_string:
LFB33:
	stp	x29, x30, [sp, -48]!
LCFI53:
	mov	x29, sp
LCFI54:
	stp	x19, x20, [sp, 16]
LCFI55:
	mov	x20, x0
	stp	x21, x22, [sp, 32]
LCFI56:
	mov	x22, x2
	ldp	w3, w19, [x2]
	sxtw	x21, w3
	sxtw	x2, w19
	add	x0, x21, 30
	cmp	x0, x2
	bge	L247
	tbnz	w19, #31, L248
	mov	x6, x1
	cmp	w3, w19
	mov	w1, 32
	mov	x0, x6
	sub	x2, x2, x21
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldp	w1, w5, [x22]
	mov	x6, x0
	cbz	x20, L249
	mov	x8, -3689348814741910324
	mov	w0, 0
	mov	w9, 46
	movk	x8, 0xcccd, lsl 0
	mov	w7, 2147483647
	b	L229
	.p2align 2,,3
L233:
	cmp	w5, w19
	blt	L250
	umulh	x2, x20, x8
	sxtw	x4, w19
	sub	w19, w19, #1
	sub	x4, x4, x21
	lsr	x2, x2, 3
	add	x3, x2, x2, lsl 2
	sub	x3, x20, x3, lsl 1
	add	w3, w3, 48
	strb	w3, [x6, x4]
	cmn	w19, #1
	beq	L251
	cmp	w0, w7
	beq	L252
	add	w0, w0, 1
	cmp	x20, 9
	bls	L232
	mov	x20, x2
L229:
	cmp	w1, w19
	bgt	L232
	cmp	w0, 18
	bne	L233
	cmp	w5, w19
	blt	L253
	sxtw	x2, w19
	sub	w19, w19, #1
	sub	x2, x2, x21
	strb	w9, [x6, x2]
	cmn	w19, #1
	beq	L254
	cmp	w1, w19
	ble	L233
	.p2align 5,,15
L232:
	subs	w0, w5, w19
	bvs	L240
	tbz	w0, #31, L225
	adrp	x0, lC2@PAGE
	mov	w1, 419
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L249:
	cmp	w1, w5
	bgt	L255
	sxtw	x1, w1
	mov	w2, 48
	sub	x1, x1, x21
	mov	w0, 1
	strb	w2, [x6, x1]
L225:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI57:
	ret
L252:
LCFI58:
	adrp	x0, lC2@PAGE
	mov	w1, 415
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L250:
	adrp	x0, lC2@PAGE
	mov	w1, 412
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L251:
	adrp	x0, lC2@PAGE
	mov	w1, 414
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L254:
	adrp	x0, lC2@PAGE
	mov	w1, 406
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L253:
	adrp	x0, lC2@PAGE
	mov	w1, 405
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L247:
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L255:
	adrp	x0, lC2@PAGE
	mov	w1, 396
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L240:
	adrp	x0, lC2@PAGE
	mov	w1, 419
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L248:
	adrp	x0, lC2@PAGE
	mov	w1, 387
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE33:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__from_display_string
_anubis_token__from_display_string:
LFB34:
	ldp	w4, w7, [x1]
	cmp	w4, w7
	bgt	L269
	sxtw	x2, w4
	stp	x29, x30, [sp, -16]!
LCFI59:
	mov	x29, sp
LCFI60:
	sub	x1, x2, #1
	sub	x5, x0, x2
	mov	x0, x1
	sxtw	x3, w7
	add	x0, x0, 1
	ldrsb	w2, [x5, x0]
	cmp	w2, 46
	beq	L292
	.p2align 5,,15
L258:
	cmp	x3, x0
	beq	L293
	add	x0, x0, 1
	ldrsb	w2, [x5, x0]
	cmp	w2, 46
	bne	L258
L292:
	mov	w8, w0
	tst	w4, w0
	bmi	L294
	cmp	w0, 0
	cset	w10, ne
L261:
	mov	x9, -7378697629483820647
	mov	w4, 0
	mov	x0, 0
	sxtw	x7, w7
	movk	x9, 0x1999, lsl 48
	mov	w11, 2147483647
	b	L267
	.p2align 2,,3
L298:
	cmp	x0, x9
	bhi	L273
	add	x0, x0, x0, lsl 2
	sxtw	x3, w3
	add	x0, x3, x0, lsl 1
	tbnz	w8, #31, L295
	cmp	w10, 0
	ccmp	w8, w1, 0, ne
	blt	L296
L262:
	cmp	x7, x1
	beq	L297
L267:
	add	x1, x1, 1
	ldrsb	w2, [x5, x1]
	cmp	w2, 46
	beq	L262
	sub	w3, w2, #48
	and	w6, w3, 255
	cmp	w6, 9
	bls	L298
	cmp	w2, 32
	beq	L262
L273:
	mov	w1, 0
	mov	x0, 0
	and	x1, x1, 1
	ldp	x29, x30, [sp], 16
LCFI61:
	ret
	.p2align 2,,3
L269:
	mov	x1, -7378697629483820647
	mov	w4, 0
	mov	x0, 0
	movk	x1, 0x1999, lsl 48
	b	L285
	.p2align 2,,3
L300:
	add	x0, x0, x0, lsl 2
	add	w4, w4, 1
	lsl	x0, x0, 1
	cmp	w4, 18
	beq	L299
L285:
	cmp	x0, x1
	bls	L300
	mov	w1, 0
	mov	x0, 0
	and	x1, x1, 1
	ret
	.p2align 2,,3
L296:
LCFI62:
	cmp	w4, w11
	beq	L301
	add	w4, w4, 1
	cmp	x7, x1
	bne	L267
	.p2align 5,,15
L297:
	mov	x1, -7378697629483820647
	movk	x1, 0x1999, lsl 48
	cmp	w4, 17
	ble	L268
	b	L272
	.p2align 2,,3
L302:
	add	x0, x0, x0, lsl 2
	add	w4, w4, 1
	lsl	x0, x0, 1
	cmp	w4, 18
	beq	L272
L268:
	cmp	x0, x1
	bls	L302
	b	L273
	.p2align 2,,3
L293:
	mov	w10, 0
	mov	w8, 0
	b	L261
	.p2align 2,,3
L272:
	mov	w1, 1
	and	x1, x1, 1
	ldp	x29, x30, [sp], 16
LCFI63:
	ret
	.p2align 2,,3
L299:
	mov	w1, 1
	and	x1, x1, 1
	ret
L295:
LCFI64:
	adrp	x0, lC2@PAGE
	mov	w1, 451
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L294:
	adrp	x0, lC2@PAGE
	mov	w1, 437
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L301:
	adrp	x0, lC2@PAGE
	mov	w1, 452
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE34:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__is_dust
_anubis_token__is_dust:
LFB35:
	mov	x1, 4095
	movk	x1, 0xd4a5, lsl 16
	movk	x1, 0xe8, lsl 32
	cmp	x0, x1
	cset	w0, ls
	ret
LFE35:
	.const
	.align	3
lC12:
	.ascii "failed precondition from anubis_token.ads:354"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__init_account
_anubis_token__init_account:
LFB36:
	stp	x29, x30, [sp, -16]!
LCFI65:
	mov	x29, sp
LCFI66:
	ldp	w2, w3, [x2]
	sxtw	x5, w2
	sxtw	x4, w3
	add	x6, x5, 31
	cmp	x6, x4
	bne	L316
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w2, w3
	bgt	L306
	sub	x6, x1, x5
	sub	x1, x5, #1
	sub	x5, x0, x5
	.p2align 5,,15
L312:
	add	x1, x1, 1
	subs	w3, w1, w2
	bvs	L308
	cmp	w3, 31
	bgt	L310
	bhi	L317
	ldrb	w3, [x6, x1]
	strb	w3, [x5, x1]
L310:
	cmp	x1, x4
	bne	L312
L306:
	movi	v31.4s, 0
	strh	wzr, [x0, 48]
	str	q31, [x0, 32]
	str	q31, [x0, 56]
	ldp	x29, x30, [sp], 16
LCFI67:
	ret
L308:
LCFI68:
	adrp	x0, lC2@PAGE
	mov	w1, 488
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L317:
	adrp	x0, lC2@PAGE
	mov	w1, 489
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L316:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE36:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__freeze_account
_anubis_token__freeze_account:
LFB38:
	mov	w1, 1
	strb	w1, [x0, 48]
	ret
LFE38:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__unfreeze_account
_anubis_token__unfreeze_account:
LFB40:
	strb	wzr, [x0, 48]
	ret
LFE40:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__increment_nonce
_anubis_token__increment_nonce:
LFB42:
	ldr	x1, [x0, 40]
	add	x1, x1, 1
	str	x1, [x0, 40]
	ret
LFE42:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__init_token_state
_anubis_token__init_token_state:
LFB44:
	adrp	x2, lC13@PAGE
	stp	x1, xzr, [x0, 16]
	ldr	q31, [x2, #lC13@PAGEOFF]
	stp	xzr, xzr, [x0, 32]
	stp	xzr, xzr, [x0, 48]
	str	q31, [x0]
	stp	xzr, xzr, [x0, 64]
	stp	xzr, xzr, [x0, 80]
	ret
LFE44:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__advance_block
_anubis_token__advance_block:
LFB46:
	ldr	x1, [x0, 24]
	add	x1, x1, 1
	str	x1, [x0, 24]
	ret
LFE46:
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_token.ads:414"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__serialize_account
_anubis_token__serialize_account:
LFB48:
	stp	x29, x30, [sp, -48]!
LCFI69:
	mov	x29, sp
LCFI70:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI71:
	mov	x22, x2
	mov	x21, x0
	ldp	w19, w3, [x2]
	sxtw	x20, w19
	sxtw	x2, w3
	add	x0, x20, 126
	cmp	x0, x2
	bge	L361
	tbnz	w19, #31, L362
	mov	x6, x1
	cmp	w19, w3
	mov	w1, 0
	mov	x0, x6
	sub	x2, x2, x20
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldp	w1, w4, [x22]
	mov	x6, x0
	mov	x3, x21
	add	x5, x21, 32
	mov	w8, 2147483647
	.p2align 5,,15
L330:
	cmp	w4, w19
	blt	L327
	cmp	w1, w19
	bgt	L363
	ldrb	w7, [x3]
	sxtw	x2, w19
	sub	x2, x2, x20
	strb	w7, [x6, x2]
	cmp	w19, w8
	beq	L364
	add	w19, w19, 1
L327:
	add	x3, x3, 1
	cmp	x3, x5
	bne	L330
	ldr	x5, [x21, 32]
	mov	w2, 8
	mov	w0, 2147483647
	.p2align 5,,15
L334:
	cmp	w4, w19
	blt	L331
	cmp	w1, w19
	bgt	L365
	sxtw	x3, w19
	sub	x3, x3, x20
	strb	w5, [x6, x3]
	lsr	x5, x5, 8
	cmp	w19, w0
	beq	L366
	add	w19, w19, 1
L331:
	subs	w2, w2, #1
	bne	L334
	ldr	x5, [x21, 40]
	mov	w2, 8
	mov	w0, 2147483647
	.p2align 5,,15
L338:
	cmp	w4, w19
	blt	L335
	cmp	w1, w19
	bgt	L367
	sxtw	x3, w19
	sub	x3, x3, x20
	strb	w5, [x6, x3]
	lsr	x5, x5, 8
	cmp	w19, w0
	beq	L368
	add	w19, w19, 1
L335:
	subs	w2, w2, #1
	bne	L338
	cmp	w4, w19
	blt	L339
	cmp	w1, w19
	bgt	L369
	ldrb	w2, [x21, 48]
	cmp	w2, 1
	bhi	L370
	sxtw	x0, w19
	mov	w3, 2147483647
	sub	x0, x0, x20
	strb	w2, [x6, x0]
	cmp	w19, w3
	beq	L371
	add	w0, w19, 1
	cmp	w4, w0
	blt	L354
	ldrb	w5, [x21, 49]
	cmp	w5, 4
	bhi	L372
	sxtw	x2, w0
	add	w19, w19, 2
	sub	x2, x2, x20
	strb	w5, [x6, x2]
	cmp	w0, w3
	beq	L373
L339:
	ldr	x3, [x21, 56]
	mov	w0, 8
	mov	w5, 2147483647
	.p2align 5,,15
L348:
	cmp	w4, w19
	blt	L345
	cmp	w1, w19
	bgt	L374
	sxtw	x2, w19
	sub	x2, x2, x20
	strb	w3, [x6, x2]
	lsr	x3, x3, 8
	cmp	w19, w5
	beq	L375
	add	w19, w19, 1
L345:
	subs	w0, w0, #1
	bne	L348
	subs	w0, w19, w1
	bvs	L350
	tbnz	w0, #31, L376
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI72:
	ret
L354:
LCFI73:
	mov	w19, w0
	b	L339
L363:
	adrp	x0, lC2@PAGE
	mov	w1, 567
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L364:
	adrp	x0, lC2@PAGE
	mov	w1, 568
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L371:
	adrp	x0, lC2@PAGE
	mov	w1, 601
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L372:
	adrp	x0, lC2@PAGE
	mov	w1, 606
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L373:
	adrp	x0, lC2@PAGE
	mov	w1, 607
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L362:
	adrp	x0, lC2@PAGE
	mov	w1, 560
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L361:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L368:
	adrp	x0, lC2@PAGE
	mov	w1, 593
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L367:
	adrp	x0, lC2@PAGE
	mov	w1, 591
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L366:
	adrp	x0, lC2@PAGE
	mov	w1, 580
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L365:
	adrp	x0, lC2@PAGE
	mov	w1, 578
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L376:
	adrp	x0, lC2@PAGE
	mov	w1, 623
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L350:
	adrp	x0, lC2@PAGE
	mov	w1, 623
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L375:
	adrp	x0, lC2@PAGE
	mov	w1, 618
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L374:
	adrp	x0, lC2@PAGE
	mov	w1, 616
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L369:
	adrp	x0, lC2@PAGE
	mov	w1, 600
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L370:
	adrp	x0, lC2@PAGE
	mov	w1, 600
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE48:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__deserialize_account
_anubis_token__deserialize_account:
LFB49:
	stp	x29, x30, [sp, -96]!
LCFI74:
	mov	x29, sp
LCFI75:
	stp	x19, x20, [sp, 16]
LCFI76:
	mov	x20, x2
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
LCFI77:
	ldp	w2, w4, [x1]
	tbnz	w2, #31, L408
	movi	v31.4s, 0
	sxtw	x23, w2
	str	xzr, [x20, 64]
	mov	x22, x0
	add	x0, x23, 56
	stp	q31, q31, [x20]
	stp	q31, q31, [x20, 32]
	cmp	x0, w4, sxtw
	bge	L401
	mov	x21, x1
	mov	x3, x20
	add	x0, x20, 32
	mov	w19, w2
	mov	w6, 2147483647
	.p2align 5,,15
L383:
	cmp	w4, w19
	blt	L380
	cmp	w2, w19
	bgt	L409
	sxtw	x5, w19
	sub	x5, x5, x23
	ldrb	w1, [x22, x5]
	strb	w1, [x3]
	cmp	w19, w6
	beq	L410
	add	w19, w19, 1
L380:
	add	x3, x3, 1
	cmp	x3, x0
	bne	L383
	mov	w24, -1
	mov	w26, 2147483647
	.p2align 5,,15
L387:
	add	w24, w24, 1
	cmp	w4, w19
	blt	L384
	ldr	w0, [x21]
	cmp	w0, w19
	bgt	L411
	sxtw	x2, w19
	ldr	x27, [x20, 32]
	mov	w1, w24
	sub	x2, x2, x23
	mov	x0, 256
	ldrb	w25, [x22, x2]
	bl	_system__exp_llu__exp_long_long_unsigned
	madd	x25, x25, x0, x27
	str	x25, [x20, 32]
	cmp	w19, w26
	beq	L412
	ldr	w4, [x21, 4]
	add	w19, w19, 1
L384:
	cmp	w24, 7
	bne	L387
	mov	w24, -1
	mov	w26, 2147483647
	.p2align 5,,15
L391:
	add	w24, w24, 1
	cmp	w19, w4
	bgt	L388
	ldr	w0, [x21]
	cmp	w0, w19
	bgt	L413
	sxtw	x2, w19
	ldr	x27, [x20, 40]
	mov	w1, w24
	sub	x2, x2, x23
	mov	x0, 256
	ldrb	w25, [x22, x2]
	bl	_system__exp_llu__exp_long_long_unsigned
	madd	x25, x25, x0, x27
	str	x25, [x20, 40]
	cmp	w19, w26
	beq	L414
	ldr	w4, [x21, 4]
	add	w19, w19, 1
L388:
	cmp	w24, 7
	bne	L391
	cmp	w19, w4
	bgt	L392
	ldr	w0, [x21]
	cmp	w0, w19
	bgt	L415
	sxtw	x0, w19
	mov	w1, 2147483647
	sub	x0, x0, x23
	ldrb	w0, [x22, x0]
	cmp	w0, 0
	cset	w0, ne
	strb	w0, [x20, 48]
	cmp	w19, w1
	beq	L416
	add	w0, w19, 1
	cmp	w4, w0
	blt	L402
	sxtw	x1, w0
	sub	x1, x1, x23
	ldrb	w1, [x22, x1]
	cmp	w1, 4
	bls	L417
L395:
	mov	w1, 2147483647
	add	w19, w19, 2
	cmp	w0, w1
	beq	L418
L392:
	mov	w24, -1
	mov	w26, 2147483647
	b	L400
	.p2align 2,,3
L421:
	ldr	w4, [x21, 4]
L400:
	add	w24, w24, 1
	cmp	w19, w4
	bgt	L397
	ldr	w0, [x21]
	cmp	w0, w19
	bgt	L419
	sxtw	x2, w19
	ldr	x27, [x20, 56]
	mov	w1, w24
	sub	x2, x2, x23
	mov	x0, 256
	ldrb	w25, [x22, x2]
	bl	_system__exp_llu__exp_long_long_unsigned
	madd	x25, x25, x0, x27
	str	x25, [x20, 56]
	cmp	w19, w26
	beq	L420
	add	w19, w19, 1
L397:
	cmp	w24, 7
	bne	L421
	mov	w0, 1
	ldr	x27, [sp, 80]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 96
LCFI78:
	ret
	.p2align 2,,3
L401:
LCFI79:
	mov	w0, 0
	ldr	x27, [sp, 80]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x29, x30, [sp], 96
LCFI80:
	ret
L417:
LCFI81:
	strb	w1, [x20, 49]
	b	L395
L402:
	mov	w19, w0
	b	L392
L409:
	adrp	x0, lC2@PAGE
	mov	w1, 651
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L410:
	adrp	x0, lC2@PAGE
	mov	w1, 652
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L411:
	adrp	x0, lC2@PAGE
	mov	w1, 660
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L412:
	adrp	x0, lC2@PAGE
	mov	w1, 661
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L418:
	adrp	x0, lC2@PAGE
	mov	w1, 685
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L419:
	adrp	x0, lC2@PAGE
	mov	w1, 692
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L420:
	adrp	x0, lC2@PAGE
	mov	w1, 693
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L408:
	adrp	x0, lC2@PAGE
	mov	w1, 631
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L413:
	adrp	x0, lC2@PAGE
	mov	w1, 669
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L414:
	adrp	x0, lC2@PAGE
	mov	w1, 670
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L415:
	adrp	x0, lC2@PAGE
	mov	w1, 676
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L416:
	adrp	x0, lC2@PAGE
	mov	w1, 677
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE49:
	.const
	.align	3
lC15:
	.ascii "failed precondition from anubis_token.ads:431"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_token__serialize_token_state
_anubis_token__serialize_token_state:
LFB50:
	stp	x29, x30, [sp, -96]!
LCFI82:
	mov	x29, sp
LCFI83:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
LCFI84:
	mov	x19, x0
	add	x0, x29, 96
	str	x21, [sp, 32]
LCFI85:
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w21, w4, [x2]
	str	x0, [x29, 88]
	sxtw	x3, w21
	sxtw	x2, w4
	add	x0, x3, 254
	str	x3, [x29, 64]
	cmp	x0, x2
	bge	L434
	tbnz	w21, #31, L435
	ldr	x0, [x29, 48]
	cmp	w21, w4
	mov	w1, 0
	sub	x2, x2, x3
	add	x20, x29, 64
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldr	x0, [x19]
	mov	x16, x20
	str	w21, [x29, 80]
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 8]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 16]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 24]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 32]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 40]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 48]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 56]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 64]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 72]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 80]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	x0, [x19, 88]
	mov	x16, x20
	bl	_anubis_token__serialize_token_state__write_u64.12
	ldr	w0, [x29, 80]
	tbnz	w0, #31, L436
	ldr	x1, [x29, 56]
	ldr	w1, [x1]
	subs	w0, w0, w1
	bvs	L428
	tbnz	w0, #31, L437
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI86:
	ret
L428:
LCFI87:
	adrp	x0, lC2@PAGE
	mov	w1, 735
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L436:
	adrp	x0, lC2@PAGE
	mov	w1, 735
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L435:
	adrp	x0, lC2@PAGE
	mov	w1, 705
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L434:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L437:
	adrp	x0, lC2@PAGE
	mov	w1, 735
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE50:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__deserialize_token_state
_anubis_token__deserialize_token_state:
LFB52:
	stp	x29, x30, [sp, -80]!
LCFI88:
	mov	x29, sp
LCFI89:
	add	x3, x29, 32
	stp	x0, x1, [x29, 32]
	add	x0, x29, 80
	stp	x19, x20, [sp, 16]
LCFI90:
	mov	x19, x2
	str	x3, [x29, 56]
	str	x0, [x29, 72]
	ldp	w1, w2, [x1]
	sxtw	x0, w1
	str	x0, [x29, 48]
	tbnz	w1, #31, L443
	movi	v31.4s, 0
	str	w1, [x29, 64]
	add	x1, x0, 94
	mov	w0, 0
	stp	q31, q31, [x19]
	stp	q31, q31, [x19, 32]
	stp	q31, q31, [x19, 64]
	cmp	x1, w2, sxtw
	blt	L444
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI91:
	ret
	.p2align 2,,3
L444:
LCFI92:
	add	x20, x29, 48
	mov	x16, x20
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 8]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 16]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 24]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 32]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 40]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 48]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 56]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 64]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 72]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x16, x20
	str	x0, [x19, 80]
	bl	_anubis_token__deserialize_token_state__read_u64.13
	mov	x1, x0
	mov	w0, 1
	str	x1, [x19, 88]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI93:
	ret
L443:
LCFI94:
	adrp	x0, lC2@PAGE
	mov	w1, 743
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE52:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__zeroize_account
_anubis_token__zeroize_account:
LFB54:
	movi	v31.4s, 0
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	str	q31, [x0, 32]
	strh	wzr, [x0, 48]
	str	q31, [x0, 56]
	ret
LFE54:
	.align	2
	.p2align 5,,15
	.globl _anubis_token__zeroize_token_state
_anubis_token__zeroize_token_state:
LFB55:
	movi	v31.4s, 0
	stp	q31, q31, [x0]
	stp	q31, q31, [x0, 32]
	stp	q31, q31, [x0, 64]
	ret
LFE55:
	.const
	.align	3
_CSWTCH.198:
	.xword	300000000
	.xword	300000000
	.xword	150000000
	.xword	80000000
	.xword	70000000
	.align	3
_CSWTCH.190:
	.xword	0
	.xword	1000
	.xword	10000
	.xword	50000
	.align	2
_CSWTCH.164:
	.word	0
	.word	0
	.word	1000
	.word	2000
	.align	3
_burn_resultG.14:
	.byte	0
	.byte	3
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.space 5
	.align	3
_transfer_resultG.18:
	.byte	2
	.byte	0
	.byte	4
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	5
	.space 3
	.align	1
_transfer_resultT2.19:
	.byte	2
	.byte	0
	.align	1
_transfer_resultT1.20:
	.byte	3
	.byte	8
	.align	3
_transfer_resultP.21:
	.word	1
	.word	3
	.align	3
_certification_levelG.22:
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	1
	.byte	0
	.byte	0
	.space 1
_allocation_categoryG.26:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	1
	.byte	0
	.byte	6
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	1
	.byte	2
	.byte	0
	.globl _anubis_token__burn_resultN
	.align	3
_anubis_token__burn_resultN:
	.byte	1
	.byte	7
	.byte	27
	.byte	38
	.byte	52
	.space 3
	.globl _anubis_token__burn_resultS
	.align	3
_anubis_token__burn_resultS:
	.ascii "BURNEDINSUFFICIENT_BALANCEAMOUNT_ZEROBURN_FORBIDDEN"
	.globl _anubis_token__transfer_resultN
	.align	3
_anubis_token__transfer_resultN:
	.byte	1
	.byte	8
	.byte	28
	.byte	45
	.byte	60
	.byte	73
	.byte	89
	.space 1
	.globl _anubis_token__transfer_resultS
	.align	3
_anubis_token__transfer_resultS:
	.ascii "SUCCESSINSUFFICIENT_BALANCEINVALID_RECIPIENTAMOUNT_OVERFLOWSENDER_FROZENRECIPIENT_FROZEN"
	.globl _anubis_token__certification_levelN
	.align	3
_anubis_token__certification_levelN:
	.byte	1
	.byte	5
	.byte	11
	.byte	17
	.byte	21
	.byte	29
	.space 2
	.globl _anubis_token__certification_levelS
	.align	3
_anubis_token__certification_levelS:
	.ascii "NONEBRONZESILVERGOLDPLATINUM"
	.globl _anubis_token__allocation_categoryN
	.align	3
_anubis_token__allocation_categoryN:
	.byte	1
	.byte	13
	.byte	30
	.byte	48
	.byte	63
	.byte	82
	.byte	99
	.byte	111
	.globl _anubis_token__allocation_categoryS
	.align	3
_anubis_token__allocation_categoryS:
	.ascii "SOLO_BUILDERPROTOCOL_TREASURYGENESIS_VALIDATORSGENESIS_PROVERSDEVELOPER_ECOSYSTEMQUANTUM_INSURANCEBUG_BOUNTIES"
	.globl _anubis_token__token_name
	.align	3
_anubis_token__token_name:
	.ascii "Anubis Token"
	.space 4
	.globl _anubis_token__token_symbol
	.align	3
_anubis_token__token_symbol:
	.ascii "ANUBIS"
	.space 2
	.globl _anubis_token_E
	.data
	.align	1
_anubis_token_E:
	.space 2
	.literal16
	.align	4
lC13:
	.xword	-6930898827444486144
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
	.quad	LFB51-.
	.set L$set$2,LFE51-LFB51
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB51
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
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$7,LEFDE3-LASFDE3
	.long L$set$7
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB53-.
	.set L$set$8,LFE53-LFB53
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB53
	.long L$set$9
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$10,LCFI5-LCFI4
	.long L$set$10
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$11,LCFI6-LCFI5
	.long L$set$11
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0xa
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
	.byte	0x4
	.set L$set$15,LCFI10-LCFI9
	.long L$set$15
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$16,LEFDE5-LASFDE5
	.long L$set$16
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB2-.
	.set L$set$17,LFE2-LFB2
	.quad L$set$17
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$18,LEFDE7-LASFDE7
	.long L$set$18
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB3-.
	.set L$set$19,LFE3-LFB3
	.quad L$set$19
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$20,LEFDE9-LASFDE9
	.long L$set$20
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB4-.
	.set L$set$21,LFE4-LFB4
	.quad L$set$21
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$22,LEFDE11-LASFDE11
	.long L$set$22
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB5-.
	.set L$set$23,LFE5-LFB5
	.quad L$set$23
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$24,LEFDE13-LASFDE13
	.long L$set$24
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB6-.
	.set L$set$25,LFE6-LFB6
	.quad L$set$25
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$26,LEFDE15-LASFDE15
	.long L$set$26
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB7-.
	.set L$set$27,LFE7-LFB7
	.quad L$set$27
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$28,LEFDE17-LASFDE17
	.long L$set$28
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB105-.
	.set L$set$29,LFE105-LFB105
	.quad L$set$29
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$30,LEFDE19-LASFDE19
	.long L$set$30
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB9-.
	.set L$set$31,LFE9-LFB9
	.quad L$set$31
	.uleb128 0
	.byte	0x4
	.set L$set$32,LCFI11-LFB9
	.long L$set$32
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$33,LCFI12-LCFI11
	.long L$set$33
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$34,LCFI13-LCFI12
	.long L$set$34
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$35,LCFI14-LCFI13
	.long L$set$35
	.byte	0xb
	.byte	0x4
	.set L$set$36,LCFI15-LCFI14
	.long L$set$36
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$37,LCFI16-LCFI15
	.long L$set$37
	.byte	0xb
	.byte	0x4
	.set L$set$38,LCFI17-LCFI16
	.long L$set$38
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$39,LCFI18-LCFI17
	.long L$set$39
	.byte	0xb
	.byte	0x4
	.set L$set$40,LCFI19-LCFI18
	.long L$set$40
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$41,LCFI20-LCFI19
	.long L$set$41
	.byte	0xb
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$42,LEFDE21-LASFDE21
	.long L$set$42
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB11-.
	.set L$set$43,LFE11-LFB11
	.quad L$set$43
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI21-LFB11
	.long L$set$44
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$45,LCFI22-LCFI21
	.long L$set$45
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$46,LEFDE23-LASFDE23
	.long L$set$46
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB13-.
	.set L$set$47,LFE13-LFB13
	.quad L$set$47
	.uleb128 0
	.byte	0x4
	.set L$set$48,LCFI23-LFB13
	.long L$set$48
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$49,LCFI24-LCFI23
	.long L$set$49
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$50,LCFI25-LCFI24
	.long L$set$50
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$51,LCFI26-LCFI25
	.long L$set$51
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$52,LEFDE25-LASFDE25
	.long L$set$52
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB14-.
	.set L$set$53,LFE14-LFB14
	.quad L$set$53
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$54,LEFDE27-LASFDE27
	.long L$set$54
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB16-.
	.set L$set$55,LFE16-LFB16
	.quad L$set$55
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$56,LEFDE29-LASFDE29
	.long L$set$56
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB18-.
	.set L$set$57,LFE18-LFB18
	.quad L$set$57
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
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$61,LCFI28-LCFI27
	.long L$set$61
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$62,LCFI29-LCFI28
	.long L$set$62
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI30-LCFI29
	.long L$set$63
	.byte	0xb
	.byte	0x4
	.set L$set$64,LCFI31-LCFI30
	.long L$set$64
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI32-LCFI31
	.long L$set$65
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$66,LEFDE33-LASFDE33
	.long L$set$66
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB21-.
	.set L$set$67,LFE21-LFB21
	.quad L$set$67
	.uleb128 0
	.byte	0x4
	.set L$set$68,LCFI33-LFB21
	.long L$set$68
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$69,LCFI34-LCFI33
	.long L$set$69
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$70,LCFI35-LCFI34
	.long L$set$70
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$71,LCFI36-LCFI35
	.long L$set$71
	.byte	0xb
	.byte	0x4
	.set L$set$72,LCFI37-LCFI36
	.long L$set$72
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$73,LCFI38-LCFI37
	.long L$set$73
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$74,LEFDE35-LASFDE35
	.long L$set$74
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB22-.
	.set L$set$75,LFE22-LFB22
	.quad L$set$75
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI39-LFB22
	.long L$set$76
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$77,LCFI40-LCFI39
	.long L$set$77
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$78,LCFI41-LCFI40
	.long L$set$78
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$79,LCFI42-LCFI41
	.long L$set$79
	.byte	0xb
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$80,LEFDE37-LASFDE37
	.long L$set$80
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB23-.
	.set L$set$81,LFE23-LFB23
	.quad L$set$81
	.uleb128 0
	.byte	0x4
	.set L$set$82,LCFI43-LFB23
	.long L$set$82
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$83,LCFI44-LCFI43
	.long L$set$83
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$84,LEFDE39-LASFDE39
	.long L$set$84
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB25-.
	.set L$set$85,LFE25-LFB25
	.quad L$set$85
	.uleb128 0
	.byte	0x4
	.set L$set$86,LCFI45-LFB25
	.long L$set$86
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$87,LCFI46-LCFI45
	.long L$set$87
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$88,LEFDE41-LASFDE41
	.long L$set$88
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB26-.
	.set L$set$89,LFE26-LFB26
	.quad L$set$89
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$90,LEFDE43-LASFDE43
	.long L$set$90
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB27-.
	.set L$set$91,LFE27-LFB27
	.quad L$set$91
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$92,LEFDE45-LASFDE45
	.long L$set$92
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB28-.
	.set L$set$93,LFE28-LFB28
	.quad L$set$93
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$94,LEFDE47-LASFDE47
	.long L$set$94
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB29-.
	.set L$set$95,LFE29-LFB29
	.quad L$set$95
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$96,LEFDE49-LASFDE49
	.long L$set$96
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB30-.
	.set L$set$97,LFE30-LFB30
	.quad L$set$97
	.uleb128 0
	.byte	0x4
	.set L$set$98,LCFI47-LFB30
	.long L$set$98
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$99,LCFI48-LCFI47
	.long L$set$99
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$100,LEFDE51-LASFDE51
	.long L$set$100
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB31-.
	.set L$set$101,LFE31-LFB31
	.quad L$set$101
	.uleb128 0
	.byte	0x4
	.set L$set$102,LCFI49-LFB31
	.long L$set$102
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$103,LCFI50-LCFI49
	.long L$set$103
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$104,LEFDE53-LASFDE53
	.long L$set$104
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB32-.
	.set L$set$105,LFE32-LFB32
	.quad L$set$105
	.uleb128 0
	.byte	0x4
	.set L$set$106,LCFI51-LFB32
	.long L$set$106
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$107,LCFI52-LCFI51
	.long L$set$107
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$108,LEFDE55-LASFDE55
	.long L$set$108
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB33-.
	.set L$set$109,LFE33-LFB33
	.quad L$set$109
	.uleb128 0
	.byte	0x4
	.set L$set$110,LCFI53-LFB33
	.long L$set$110
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$111,LCFI54-LCFI53
	.long L$set$111
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$112,LCFI55-LCFI54
	.long L$set$112
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$113,LCFI56-LCFI55
	.long L$set$113
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$114,LCFI57-LCFI56
	.long L$set$114
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
	.set L$set$115,LCFI58-LCFI57
	.long L$set$115
	.byte	0xb
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$116,LEFDE57-LASFDE57
	.long L$set$116
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB34-.
	.set L$set$117,LFE34-LFB34
	.quad L$set$117
	.uleb128 0
	.byte	0x4
	.set L$set$118,LCFI59-LFB34
	.long L$set$118
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$119,LCFI60-LCFI59
	.long L$set$119
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$120,LCFI61-LCFI60
	.long L$set$120
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$121,LCFI62-LCFI61
	.long L$set$121
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$122,LCFI63-LCFI62
	.long L$set$122
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$123,LCFI64-LCFI63
	.long L$set$123
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$124,LEFDE59-LASFDE59
	.long L$set$124
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB35-.
	.set L$set$125,LFE35-LFB35
	.quad L$set$125
	.uleb128 0
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$126,LEFDE61-LASFDE61
	.long L$set$126
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB36-.
	.set L$set$127,LFE36-LFB36
	.quad L$set$127
	.uleb128 0
	.byte	0x4
	.set L$set$128,LCFI65-LFB36
	.long L$set$128
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$129,LCFI66-LCFI65
	.long L$set$129
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$130,LCFI67-LCFI66
	.long L$set$130
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$131,LCFI68-LCFI67
	.long L$set$131
	.byte	0xb
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$132,LEFDE63-LASFDE63
	.long L$set$132
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB38-.
	.set L$set$133,LFE38-LFB38
	.quad L$set$133
	.uleb128 0
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$134,LEFDE65-LASFDE65
	.long L$set$134
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB40-.
	.set L$set$135,LFE40-LFB40
	.quad L$set$135
	.uleb128 0
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$136,LEFDE67-LASFDE67
	.long L$set$136
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB42-.
	.set L$set$137,LFE42-LFB42
	.quad L$set$137
	.uleb128 0
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$138,LEFDE69-LASFDE69
	.long L$set$138
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB44-.
	.set L$set$139,LFE44-LFB44
	.quad L$set$139
	.uleb128 0
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$140,LEFDE71-LASFDE71
	.long L$set$140
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB46-.
	.set L$set$141,LFE46-LFB46
	.quad L$set$141
	.uleb128 0
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$142,LEFDE73-LASFDE73
	.long L$set$142
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB48-.
	.set L$set$143,LFE48-LFB48
	.quad L$set$143
	.uleb128 0
	.byte	0x4
	.set L$set$144,LCFI69-LFB48
	.long L$set$144
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$145,LCFI70-LCFI69
	.long L$set$145
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$146,LCFI71-LCFI70
	.long L$set$146
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$147,LCFI72-LCFI71
	.long L$set$147
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
	.set L$set$148,LCFI73-LCFI72
	.long L$set$148
	.byte	0xb
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$149,LEFDE75-LASFDE75
	.long L$set$149
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB49-.
	.set L$set$150,LFE49-LFB49
	.quad L$set$150
	.uleb128 0
	.byte	0x4
	.set L$set$151,LCFI74-LFB49
	.long L$set$151
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$152,LCFI75-LCFI74
	.long L$set$152
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$153,LCFI76-LCFI75
	.long L$set$153
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$154,LCFI77-LCFI76
	.long L$set$154
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x97
	.uleb128 0x6
	.byte	0x98
	.uleb128 0x5
	.byte	0x99
	.uleb128 0x4
	.byte	0x9a
	.uleb128 0x3
	.byte	0x9b
	.uleb128 0x2
	.byte	0x4
	.set L$set$155,LCFI78-LCFI77
	.long L$set$155
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
	.set L$set$156,LCFI79-LCFI78
	.long L$set$156
	.byte	0xb
	.byte	0x4
	.set L$set$157,LCFI80-LCFI79
	.long L$set$157
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
	.set L$set$158,LCFI81-LCFI80
	.long L$set$158
	.byte	0xb
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$159,LEFDE77-LASFDE77
	.long L$set$159
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB50-.
	.set L$set$160,LFE50-LFB50
	.quad L$set$160
	.uleb128 0
	.byte	0x4
	.set L$set$161,LCFI82-LFB50
	.long L$set$161
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$162,LCFI83-LCFI82
	.long L$set$162
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$163,LCFI84-LCFI83
	.long L$set$163
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$164,LCFI85-LCFI84
	.long L$set$164
	.byte	0x95
	.uleb128 0x8
	.byte	0x4
	.set L$set$165,LCFI86-LCFI85
	.long L$set$165
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
	.set L$set$166,LCFI87-LCFI86
	.long L$set$166
	.byte	0xb
	.align	3
LEFDE77:
LSFDE79:
	.set L$set$167,LEFDE79-LASFDE79
	.long L$set$167
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB52-.
	.set L$set$168,LFE52-LFB52
	.quad L$set$168
	.uleb128 0
	.byte	0x4
	.set L$set$169,LCFI88-LFB52
	.long L$set$169
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$170,LCFI89-LCFI88
	.long L$set$170
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$171,LCFI90-LCFI89
	.long L$set$171
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$172,LCFI91-LCFI90
	.long L$set$172
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$173,LCFI92-LCFI91
	.long L$set$173
	.byte	0xb
	.byte	0x4
	.set L$set$174,LCFI93-LCFI92
	.long L$set$174
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$175,LCFI94-LCFI93
	.long L$set$175
	.byte	0xb
	.align	3
LEFDE79:
LSFDE81:
	.set L$set$176,LEFDE81-LASFDE81
	.long L$set$176
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB54-.
	.set L$set$177,LFE54-LFB54
	.quad L$set$177
	.uleb128 0
	.align	3
LEFDE81:
LSFDE83:
	.set L$set$178,LEFDE83-LASFDE83
	.long L$set$178
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB55-.
	.set L$set$179,LFE55-LFB55
	.quad L$set$179
	.uleb128 0
	.align	3
LEFDE83:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
