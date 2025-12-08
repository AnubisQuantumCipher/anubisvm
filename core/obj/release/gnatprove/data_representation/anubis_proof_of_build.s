	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC4:
	.ascii "anubis_proof_of_build.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_proof_of_build__generate_collection_metadata__write_u64.1:
LFB26:
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
	adrp	x0, lC4@PAGE
	mov	w1, 461
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L16:
	adrp	x0, lC4@PAGE
	mov	w1, 459
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE26:
	.align	2
	.p2align 5,,15
_anubis_proof_of_build__serialize_nft__write_u64.3:
LFB29:
	ldp	x7, x1, [x16]
	mov	w2, 8
	mov	w8, 2147483647
	ldp	x6, x1, [x1]
	ldp	w5, w4, [x1]
L26:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	blt	L32
	stp	x29, x30, [sp, -16]!
LCFI4:
	mov	x29, sp
LCFI5:
L27:
	cmp	w5, w1
	bgt	L33
	sxtw	x3, w1
	sub	x3, x3, x7
	strb	w0, [x6, x3]
	lsr	x0, x0, 8
	cmp	w1, w8
	beq	L34
	add	w1, w1, 1
	subs	w2, w2, #1
	str	w1, [x16, 16]
	beq	L35
L23:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	bge	L27
	subs	w2, w2, #1
	bne	L23
L35:
	ldp	x29, x30, [sp], 16
LCFI6:
	ret
	.p2align 2,,3
L32:
	subs	w2, w2, #1
	bne	L26
	ret
L34:
LCFI7:
	adrp	x0, lC4@PAGE
	mov	w1, 536
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L33:
	adrp	x0, lC4@PAGE
	mov	w1, 534
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE29:
	.align	2
	.p2align 5,,15
_anubis_proof_of_build__serialize_collection_state__write_u64.6:
LFB35:
	ldp	x7, x1, [x16]
	mov	w2, 8
	mov	w8, 2147483647
	ldp	x6, x1, [x1]
	ldp	w5, w4, [x1]
L43:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	blt	L49
	stp	x29, x30, [sp, -16]!
LCFI8:
	mov	x29, sp
LCFI9:
L44:
	cmp	w5, w1
	bgt	L50
	sxtw	x3, w1
	sub	x3, x3, x7
	strb	w0, [x6, x3]
	lsr	x0, x0, 8
	cmp	w1, w8
	beq	L51
	add	w1, w1, 1
	subs	w2, w2, #1
	str	w1, [x16, 16]
	beq	L52
L40:
	ldr	w1, [x16, 16]
	cmp	w4, w1
	bge	L44
	subs	w2, w2, #1
	bne	L40
L52:
	ldp	x29, x30, [sp], 16
LCFI10:
	ret
	.p2align 2,,3
L49:
	subs	w2, w2, #1
	bne	L43
	ret
L51:
LCFI11:
	adrp	x0, lC4@PAGE
	mov	w1, 723
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L50:
	adrp	x0, lC4@PAGE
	mov	w1, 721
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE35:
	.align	2
	.p2align 5,,15
_anubis_proof_of_build__serialize_nft__write_u32.2:
LFB30:
	stp	x29, x30, [sp, -16]!
LCFI12:
	mov	x29, sp
LCFI13:
	mov	w2, 4
	mov	w8, 2147483647
	ldp	x7, x1, [x16]
	ldp	x6, x1, [x1]
	ldp	w5, w4, [x1]
L58:
	ldr	w1, [x16, 16]
	tbnz	w1, #31, L61
	cmp	w4, w1
	blt	L55
	cmp	w5, w1
	bgt	L62
	sxtw	x3, w1
	sub	x3, x3, x7
	strb	w0, [x6, x3]
	lsr	w0, w0, 8
	cmp	w1, w8
	beq	L63
	add	w1, w1, 1
	str	w1, [x16, 16]
L55:
	subs	w2, w2, #1
	bne	L58
	ldp	x29, x30, [sp], 16
LCFI14:
	ret
L61:
LCFI15:
	adrp	x0, lC4@PAGE
	mov	w1, 545
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L63:
	adrp	x0, lC4@PAGE
	mov	w1, 548
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L62:
	adrp	x0, lC4@PAGE
	mov	w1, 546
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE30:
	.align	2
	.p2align 5,,15
_anubis_proof_of_build__deserialize_nft__read_u32.4:
LFB33:
	stp	x29, x30, [sp, -64]!
LCFI16:
	mov	x29, sp
LCFI17:
	stp	x19, x20, [sp, 16]
LCFI18:
	mov	w20, -1
	mov	x19, x16
	stp	x21, x22, [sp, 32]
LCFI19:
	mov	w22, 0
	mov	w21, 2147483647
	str	x23, [sp, 48]
LCFI20:
L70:
	ldr	w0, [x19, 16]
	add	w20, w20, 1
	tbnz	w0, #31, L73
	ldr	x3, [x19, 8]
	ldr	x1, [x3, 8]
	ldr	w2, [x1, 4]
	cmp	w2, w0
	blt	L66
	ldr	w1, [x1]
	cmp	w1, w0
	bgt	L74
	ldr	x4, [x19]
	sxtw	x2, w0
	mov	w1, w20
	mov	w0, 256
	ldr	x3, [x3]
	sub	x2, x2, x4
	ldrb	w23, [x3, x2]
	bl	_system__exp_uns__exp_unsigned
	ldr	w1, [x19, 16]
	madd	w22, w23, w0, w22
	tbnz	w1, #31, L75
	cmp	w1, w21
	beq	L76
	add	w1, w1, 1
	str	w1, [x19, 16]
L66:
	cmp	w20, 3
	bne	L70
	mov	w0, w22
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI21:
	ret
L73:
LCFI22:
	adrp	x0, lC4@PAGE
	mov	w1, 625
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L76:
	adrp	x0, lC4@PAGE
	mov	w1, 627
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L75:
	adrp	x0, lC4@PAGE
	mov	w1, 627
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L74:
	adrp	x0, lC4@PAGE
	mov	w1, 626
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE33:
	.align	2
	.p2align 5,,15
_anubis_proof_of_build__deserialize_nft__read_u64.5:
LFB32:
	stp	x29, x30, [sp, -64]!
LCFI23:
	mov	x29, sp
LCFI24:
	stp	x19, x20, [sp, 16]
LCFI25:
	mov	w20, -1
	mov	x19, x16
	stp	x21, x22, [sp, 32]
LCFI26:
	mov	w21, 2147483647
	str	x23, [sp, 48]
LCFI27:
	mov	x23, 0
	.p2align 5,,15
L81:
	ldr	x3, [x19, 8]
	add	w20, w20, 1
	ldr	w0, [x19, 16]
	ldr	x1, [x3, 8]
	ldr	w2, [x1, 4]
	cmp	w2, w0
	blt	L78
	ldr	w1, [x1]
	cmp	w1, w0
	bgt	L84
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
	beq	L85
	add	w1, w1, 1
	str	w1, [x19, 16]
L78:
	cmp	w20, 7
	bne	L81
	mov	x0, x23
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI28:
	ret
L84:
LCFI29:
	adrp	x0, lC4@PAGE
	mov	w1, 615
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L85:
	adrp	x0, lC4@PAGE
	mov	w1, 616
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE32:
	.align	2
	.p2align 5,,15
_anubis_proof_of_build__deserialize_collection_state__read_u64.7:
LFB37:
	stp	x29, x30, [sp, -64]!
LCFI30:
	mov	x29, sp
LCFI31:
	stp	x19, x20, [sp, 16]
LCFI32:
	mov	w20, -1
	mov	x19, x16
	stp	x21, x22, [sp, 32]
LCFI33:
	mov	w21, 2147483647
	str	x23, [sp, 48]
LCFI34:
	mov	x23, 0
	.p2align 5,,15
L90:
	ldr	x3, [x19, 8]
	add	w20, w20, 1
	ldr	w0, [x19, 16]
	ldr	x1, [x3, 8]
	ldr	w2, [x1, 4]
	cmp	w2, w0
	blt	L87
	ldr	w1, [x1]
	cmp	w1, w0
	bgt	L93
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
	beq	L94
	add	w1, w1, 1
	str	w1, [x19, 16]
L87:
	cmp	w20, 7
	bne	L90
	mov	x0, x23
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI35:
	ret
L93:
LCFI36:
	adrp	x0, lC4@PAGE
	mov	w1, 761
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L94:
	adrp	x0, lC4@PAGE
	mov	w1, 762
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE37:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__build_categoryH
_anubis_proof_of_build__build_categoryH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L98
	sub	w1, w1, w2
	cmp	w1, 1
	ble	L98
	ldrb	w1, [x0, 2]
	mov	w0, 17097
	mov	w5, 23
	movk	w0, 0xb216, lsl 16
	add	w4, w1, w1, lsl 3
	lsl	w4, w4, 1
	add	w2, w4, w1
	umull	x1, w4, w0
	umull	x3, w2, w0
	lsr	x0, x1, 36
	lsr	x3, x3, 36
	msub	w0, w0, w5, w4
	msub	w3, w3, w5, w2
	sxtw	x0, w0
	sxtw	x3, w3
	b	L96
	.p2align 2,,3
L98:
	mov	x3, 0
	mov	x0, 0
L96:
	adrp	x2, _build_categoryG.17@PAGE
	mov	w1, 36409
	add	x2, x2, _build_categoryG.17@PAGEOFF;
	movk	w1, 0x38e3, lsl 16
	ldrb	w0, [x2, x0]
	ldrb	w2, [x2, x3]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 33
	add	w1, w1, w1, lsl 3
	sub	w0, w0, w1
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__proof_of_build_nftIP
_anubis_proof_of_build__proof_of_build_nftIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__nft_arrayIP
_anubis_proof_of_build__nft_arrayIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__Tcategory_count_arrayBIP
_anubis_proof_of_build__Tcategory_count_arrayBIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__collection_stateIP
_anubis_proof_of_build__collection_stateIP:
LFB81:
	ret
LFE81:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__weekly_update_nftIP
_anubis_proof_of_build__weekly_update_nftIP:
LFB83:
	ret
LFE83:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__mint_resultH
_anubis_proof_of_build__mint_resultH:
LFB8:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _mint_resultP.16@PAGE
	add	w5, w10, 1
	add	x9, x9, _mint_resultP.16@PAGEOFF;
	adrp	x12, _mint_resultT1.15@PAGE
	adrp	x11, _mint_resultT2.14@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _mint_resultT1.15@PAGEOFF;
	add	x11, x11, _mint_resultT2.14@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 15
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L106
L110:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	lsl	w6, w2, 4
	sub	w6, w6, w2
	sub	w6, w3, w6
	lsl	w2, w5, 4
	sub	w2, w2, w5
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L106
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L110
L106:
	adrp	x3, _mint_resultG.13@PAGE
	mov	w1, 43691
	add	x3, x3, _mint_resultG.13@PAGEOFF;
	movk	w1, 0xaaaa, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 1
	sub	w0, w0, w1, lsl 1
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__verify_resultH
_anubis_proof_of_build__verify_resultH:
LFB9:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L113
	ldrb	w0, [x0]
	mov	w2, 36409
	movk	w2, 0x38e3, lsl 16
	add	w1, w0, w0, lsl 1
	lsl	w0, w0, 2
	lsl	w1, w1, 1
	umull	x3, w0, w2
	umull	x2, w1, w2
	lsr	x3, x3, 33
	add	w3, w3, w3, lsl 3
	lsr	x2, x2, 33
	add	w2, w2, w2, lsl 3
	sub	w0, w0, w3
	sxtw	x0, w0
	sub	w1, w1, w2
	adrp	x2, _verify_resultG.9@PAGE
	add	x2, x2, _verify_resultG.9@PAGEOFF;
	sxtw	x1, w1
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L113:
	adrp	x2, _verify_resultG.9@PAGE
	mov	x1, 0
	add	x2, x2, _verify_resultG.9@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__build_statisticsIP
_anubis_proof_of_build__build_statisticsIP:
LFB85:
	ret
LFE85:
	.const
	.align	3
lC5:
	.ascii "failed precondition from anubis_proof_of_build.ads:157"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__init_collection
_anubis_proof_of_build__init_collection:
LFB11:
	stp	x29, x30, [sp, -16]!
LCFI37:
	mov	x29, sp
LCFI38:
	ldp	w4, w8, [x2]
	sxtw	x2, w4
	sxtw	x5, w8
	add	x6, x2, 31
	cmp	x6, x5
	bne	L127
	add	x6, x0, 60
	stp	xzr, xzr, [x0]
	str	xzr, [x0, 16]
	stp	xzr, xzr, [x0, 24]
	stp	xzr, xzr, [x0, 40]
	str	wzr, [x0, 56]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	cmp	w4, w8
	bgt	L117
	sub	x7, x0, x2
	sub	x6, x1, x2
	add	x7, x7, 60
	sub	x1, x2, #1
	.p2align 5,,15
L123:
	add	x1, x1, 1
	subs	w2, w1, w4
	bvs	L119
	cmp	w2, 31
	bgt	L121
	bhi	L128
	ldrb	w2, [x6, x1]
	strb	w2, [x7, x1]
L121:
	cmp	x1, x5
	bne	L123
L117:
	stp	x3, x3, [x0, 96]
	ldp	x29, x30, [sp], 16
LCFI39:
	ret
L119:
LCFI40:
	adrp	x0, lC4@PAGE
	mov	w1, 38
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L128:
	adrp	x0, lC4@PAGE
	mov	w1, 39
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L127:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE11:
	.const
	.align	2
lC0:
	.word	1
	.word	54
	.text
	.const
	.align	3
lC6:
	.ascii "failed precondition from anubis_proof_of_build.ads:178"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__mint_build_nft
_anubis_proof_of_build__mint_build_nft:
LFB13:
	stp	x29, x30, [sp, -64]!
LCFI41:
	mov	x29, sp
LCFI42:
	mov	x17, x0
	stp	x19, x20, [sp, 16]
LCFI43:
	ldp	x19, x10, [x29, 72]
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI44:
	ldp	w11, w22, [x3]
	ldp	w12, w21, [x5]
	ldp	w10, w20, [x10]
	sxtw	x8, w11
	sxtw	x15, w22
	add	x0, x8, 31
	ldr	x13, [x29, 88]
	sxtw	x9, w12
	cmp	x0, x15
	add	x5, x9, 31
	sxtw	x16, w21
	ldr	w30, [x29, 64]
	sxtw	x3, w10
	ccmp	x5, x16, 0, eq
	add	x0, x3, 45
	sxtw	x14, w20
	ccmp	x0, x14, 0, eq
	bne	L164
	cmp	w6, 99
	bls	L165
	cmp	w30, 9
	bls	L166
	adrp	x23, _anubis_proof_of_build__nft_counter@PAGE
	ldr	q30, [x17, 60]
	mov	x5, x13
	ldr	x0, [x23, #_anubis_proof_of_build__nft_counter@PAGEOFF]
	ldr	q31, [x17, 76]
	add	x0, x0, 1
	str	x0, [x5], 8
	str	q30, [x13, 8]
	str	x0, [x23, #_anubis_proof_of_build__nft_counter@PAGEOFF]
	str	q31, [x5, 16]
	cmp	w1, 8
	bhi	L167
	add	x5, x13, 41
	add	x0, x13, 73
	strb	w1, [x13, 40]
	stp	xzr, xzr, [x5]
	stp	xzr, xzr, [x5, 16]
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w11, w22
	bgt	L135
	sub	x0, x13, x8
	sub	x2, x2, x8
	add	x0, x0, 73
	sub	x5, x8, #1
	.p2align 5,,15
L141:
	add	x5, x5, 1
	subs	w8, w5, w11
	bvs	L137
	cmp	w8, 31
	bgt	L139
	bhi	L168
	ldrb	w8, [x2, x5]
	strb	w8, [x0, x5]
L139:
	cmp	x15, x5
	bne	L141
L135:
	add	x0, x13, 105
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w12, w21
	bgt	L142
	sub	x0, x13, x9
	sub	x4, x4, x9
	add	x0, x0, 105
	sub	x2, x9, #1
	.p2align 5,,15
L148:
	add	x2, x2, 1
	subs	w5, w2, w12
	bvs	L144
	cmp	w5, 31
	bgt	L146
	bhi	L169
	ldrb	w5, [x4, x2]
	strb	w5, [x0, x2]
L146:
	cmp	x16, x2
	bne	L148
L142:
	movi	v31.4s, 0
	stp	w6, w7, [x13, 140]
	stp	w30, wzr, [x13, 148]
	str	q31, [x13, 172]
	str	q31, [x13, 156]
	str	q31, [x13, 186]
	cmp	w10, w20
	bgt	L149
	sub	x0, x13, x3
	sub	x4, x19, x3
	add	x0, x0, 156
	sub	x2, x3, #1
	.p2align 5,,15
L155:
	add	x2, x2, 1
	subs	w3, w2, w10
	bvs	L151
	cmp	w3, 45
	bgt	L153
	bhi	L170
	ldrb	w3, [x4, x2]
	strb	w3, [x0, x2]
L153:
	cmp	x14, x2
	bne	L155
L149:
	ldr	x5, [x17, 104]
	adrp	x7, lC7@PAGE
	add	x4, x13, 202
	fmov	s29, w6
	movi	v31.4s, 0
	add	x3, x13, 257
	ldr	q30, [x7, #lC7@PAGEOFF]
	add	x2, x13, 289
	ubfiz	x1, x1, 2, 8
	stp	xzr, xzr, [x4]
	add	x1, x1, 16
	mov	w0, 0
	add	x1, x17, x1
	stp	xzr, xzr, [x4, 16]
	stp	x5, xzr, [x13, 240]
	ins	v30.d[1], v29.d[0]
	strb	wzr, [x13, 256]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	stp	q31, q31, [x2]
	stp	q31, q31, [x2, 32]
	stp	q31, q31, [x2, 64]
	stp	q31, q31, [x2, 96]
	ldr	q31, [x17]
	ldr	x2, [x17, 16]
	add	x30, x2, w30, uxtw
	str	x30, [x17, 16]
	add	v30.2d, v30.2d, v31.2d
	str	q30, [x17]
	ldr	w2, [x1, 8]
	add	w2, w2, 1
	str	w2, [x1, 8]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI45:
	ret
	.p2align 2,,3
L165:
LCFI46:
	movi	v31.4s, 0
	mov	x2, x13
	add	x8, x13, 41
	add	x7, x13, 73
	add	x6, x13, 105
	add	x4, x13, 202
	add	x3, x13, 257
	str	xzr, [x2], 8
	add	x1, x13, 289
	mov	w0, 1
	stp	xzr, xzr, [x13, 8]
L163:
	stp	xzr, xzr, [x2, 16]
	strb	wzr, [x13, 40]
	stp	xzr, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
	stp	xzr, xzr, [x7]
	stp	xzr, xzr, [x7, 16]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	str	q31, [x13, 172]
	str	xzr, [x13, 140]
	str	xzr, [x13, 148]
	str	q31, [x13, 156]
	str	q31, [x13, 186]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	stp	xzr, xzr, [x13, 240]
	strb	wzr, [x13, 256]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	stp	q31, q31, [x1]
	stp	q31, q31, [x1, 32]
	stp	q31, q31, [x1, 64]
	stp	q31, q31, [x1, 96]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI47:
	ret
	.p2align 2,,3
L166:
LCFI48:
	mov	x2, x13
	movi	v31.4s, 0
	add	x8, x13, 41
	add	x7, x13, 73
	add	x6, x13, 105
	str	xzr, [x2], 8
	add	x4, x13, 202
	add	x3, x13, 257
	add	x1, x13, 289
	mov	w0, 2
	stp	xzr, xzr, [x13, 8]
	b	L163
L151:
	adrp	x0, lC4@PAGE
	mov	w1, 145
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L137:
	adrp	x0, lC4@PAGE
	mov	w1, 124
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L144:
	adrp	x0, lC4@PAGE
	mov	w1, 132
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L169:
	adrp	x0, lC4@PAGE
	mov	w1, 133
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L170:
	adrp	x0, lC4@PAGE
	mov	w1, 146
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L168:
	adrp	x0, lC4@PAGE
	mov	w1, 125
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L164:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L167:
	adrp	x0, lC4@PAGE
	mov	w1, 118
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.const
	.align	3
lC8:
	.ascii "anubis_proof_of_build.ads"
	.space 1
	.align	3
lC9:
	.ascii "failed precondition from anubis_proof_of_build.ads:197"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__mint_weekly_update
_anubis_proof_of_build__mint_weekly_update:
LFB14:
	fmov	s0, w4
	fmov	s31, w5
	stp	x29, x30, [sp, -16]!
LCFI49:
	mov	x29, sp
LCFI50:
	ldp	x12, x7, [x29, 16]
	ldp	x13, x4, [x29, 32]
	fmov	s30, s0
	ldr	x11, [x29, 48]
	ins	v30.s[1], v31.s[0]
	ldp	w7, w15, [x7]
	ldp	w8, w17, [x4]
	tbnz	w6, #31, L193
	sxtw	x4, w7
	mov	x14, x0
	sxtw	x9, w15
	add	x0, x4, 31
	sxtw	x5, w8
	cmp	x0, x9
	add	x16, x5, 45
	sxtw	x10, w17
	mov	w0, 100
	ccmp	x16, x10, 0, eq
	ccmp	w6, w0, 0, eq
	bgt	L194
	fmov	s29, w3
	adrp	x16, _anubis_proof_of_build__weekly_nft_counter@PAGE
	uxtl	v30.2d, v30.2s
	str	w1, [x11, 8]
	fmov	w1, s31
	ldr	x0, [x16, #_anubis_proof_of_build__weekly_nft_counter@PAGEOFF]
	strh	w2, [x11, 12]
	stp	xzr, xzr, [x11, 32]
	add	x0, x0, 1
	stp	xzr, xzr, [x11, 48]
	stp	s29, s0, [x11, 16]
	stp	w1, w6, [x11, 24]
	str	x0, [x16, #_anubis_proof_of_build__weekly_nft_counter@PAGEOFF]
	str	x0, [x11]
	cmp	w7, w15
	bgt	L174
	sub	x6, x11, x4
	sub	x3, x12, x4
	add	x6, x6, 32
	sub	x1, x4, #1
	.p2align 5,,15
L180:
	add	x1, x1, 1
	subs	w2, w1, w7
	bvs	L176
	cmp	w2, 31
	bgt	L178
	bhi	L195
	ldrb	w2, [x3, x1]
	strb	w2, [x6, x1]
L178:
	cmp	x9, x1
	bne	L180
L174:
	movi	v31.4s, 0
	stp	q31, q31, [x11, 64]
	str	q31, [x11, 94]
	cmp	w8, w17
	bgt	L181
	sub	x4, x11, x5
	sub	x3, x13, x5
	add	x4, x4, 64
	sub	x1, x5, #1
	.p2align 5,,15
L187:
	add	x1, x1, 1
	subs	w2, w1, w8
	bvs	L183
	cmp	w2, 45
	bgt	L185
	bhi	L196
	ldrb	w2, [x3, x1]
	strb	w2, [x4, x1]
L185:
	cmp	x10, x1
	bne	L187
L181:
	ldr	q31, [x14, 8]
	add	x1, x11, 110
	mov	w0, 0
	ldr	x2, [x14, 104]
	stp	xzr, xzr, [x1]
	add	v31.2d, v31.2d, v30.2d
	stp	xzr, xzr, [x1, 16]
	stp	x2, xzr, [x11, 144]
	str	xzr, [x11, 160]
	str	q31, [x14, 8]
	ldp	x29, x30, [sp], 16
LCFI51:
	ret
L176:
LCFI52:
	adrp	x0, lC4@PAGE
	mov	w1, 193
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L183:
	adrp	x0, lC4@PAGE
	mov	w1, 201
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L195:
	adrp	x0, lC4@PAGE
	mov	w1, 194
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L196:
	adrp	x0, lC4@PAGE
	mov	w1, 202
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L194:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L193:
	adrp	x0, lC8@PAGE
	mov	w1, 199
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE14:
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_proof_of_build.ads:214"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__mint_milestone_nft
_anubis_proof_of_build__mint_milestone_nft:
LFB15:
	stp	x29, x30, [sp, -64]!
LCFI53:
	mov	x29, sp
LCFI54:
	mov	x17, x0
	ldp	x16, x1, [x29, 64]
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI55:
	ldp	w9, w19, [x3]
	ldp	w10, w20, [x5]
	ldp	w8, w30, [x1]
	sxtw	x3, w9
	sxtw	x14, w19
	add	x0, x3, 31
	ldr	x13, [x29, 80]
	sxtw	x5, w10
	cmp	x0, x14
	add	x11, x5, 31
	sxtw	x15, w20
	sxtw	x1, w8
	ccmp	x11, x15, 0, eq
	add	x0, x1, 45
	sxtw	x12, w30
	ccmp	x0, x12, 0, eq
	bne	L227
	adrp	x21, _anubis_proof_of_build__nft_counter@PAGE
	ldr	q30, [x17, 60]
	mov	x11, x13
	mov	w23, 8
	add	x22, x13, 41
	ldr	x0, [x21, #_anubis_proof_of_build__nft_counter@PAGEOFF]
	ldr	q31, [x17, 76]
	add	x0, x0, 1
	str	x0, [x11], 8
	str	q30, [x13, 8]
	str	q31, [x11, 16]
	add	x11, x13, 73
	strb	w23, [x13, 40]
	stp	xzr, xzr, [x22]
	stp	xzr, xzr, [x22, 16]
	str	x0, [x21, #_anubis_proof_of_build__nft_counter@PAGEOFF]
	stp	xzr, xzr, [x11]
	stp	xzr, xzr, [x11, 16]
	cmp	w9, w19
	bgt	L199
	sub	x0, x13, x3
	sub	x2, x2, x3
	add	x0, x0, 73
	sub	x3, x3, #1
	.p2align 5,,15
L205:
	add	x3, x3, 1
	subs	w11, w3, w9
	bvs	L201
	cmp	w11, 31
	bgt	L203
	bhi	L228
	ldrb	w11, [x2, x3]
	strb	w11, [x0, x3]
L203:
	cmp	x14, x3
	bne	L205
L199:
	add	x0, x13, 105
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w10, w20
	ble	L229
L206:
	movi	v31.4s, 0
	mov	w2, 100
	stp	w6, wzr, [x13, 140]
	stp	w7, w2, [x13, 148]
	str	q31, [x13, 172]
	str	q31, [x13, 156]
	str	q31, [x13, 186]
	cmp	w8, w30
	bgt	L213
	sub	x4, x13, x1
	sub	x3, x16, x1
	add	x4, x4, 156
	sub	x1, x1, #1
	.p2align 5,,15
L219:
	add	x1, x1, 1
	subs	w2, w1, w8
	bvs	L215
	cmp	w2, 45
	bgt	L217
	bhi	L230
	ldrb	w2, [x3, x1]
	strb	w2, [x4, x1]
L217:
	cmp	x12, x1
	bne	L219
L213:
	ldr	x4, [x17, 104]
	add	x3, x13, 202
	movi	v31.4s, 0
	add	x2, x13, 257
	add	x1, x13, 289
	stp	xzr, xzr, [x3]
	mov	w0, 0
	stp	xzr, xzr, [x3, 16]
	stp	x4, xzr, [x13, 240]
	strb	wzr, [x13, 256]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	stp	q31, q31, [x1]
	stp	q31, q31, [x1, 32]
	stp	q31, q31, [x1, 64]
	stp	q31, q31, [x1, 96]
	ldr	x2, [x17]
	ldr	w1, [x17, 56]
	add	x2, x2, 1
	add	w1, w1, 1
	str	x2, [x17]
	str	w1, [x17, 56]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI56:
	ret
	.p2align 2,,3
L229:
LCFI57:
	sub	x0, x13, x5
	sub	x4, x4, x5
	add	x0, x0, 105
	sub	x2, x5, #1
	.p2align 5,,15
L212:
	add	x2, x2, 1
	subs	w3, w2, w10
	bvs	L208
	cmp	w3, 31
	bgt	L210
	bhi	L231
	ldrb	w3, [x4, x2]
	strb	w3, [x0, x2]
L210:
	cmp	x15, x2
	bne	L212
	b	L206
L215:
	adrp	x0, lC4@PAGE
	mov	w1, 263
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L201:
	adrp	x0, lC4@PAGE
	mov	w1, 242
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L208:
	adrp	x0, lC4@PAGE
	mov	w1, 250
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L230:
	adrp	x0, lC4@PAGE
	mov	w1, 264
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L228:
	adrp	x0, lC4@PAGE
	mov	w1, 243
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L231:
	adrp	x0, lC4@PAGE
	mov	w1, 251
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L227:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE15:
	.const
	.align	3
lC11:
	.ascii "failed precondition from anubis_proof_of_build.ads:230"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__verify_nft
_anubis_proof_of_build__verify_nft:
LFB16:
	stp	x29, x30, [sp, -16]!
LCFI58:
	mov	x29, sp
LCFI59:
	mov	x11, x0
	ldp	w5, w9, [x2]
	ldp	w6, w12, [x4]
	sxtw	x2, w5
	sxtw	x7, w9
	add	x8, x2, 31
	sxtw	x4, w6
	cmp	x8, x7
	add	x0, x4, 62
	sxtw	x8, w12
	ccmp	x0, x8, 0, eq
	bge	L256
	ldrb	w0, [x11, 256]
	cmp	w0, 1
	bhi	L257
	cbnz	w0, L250
	cmp	w5, w9
	bgt	L236
	sub	x10, x11, x2
	sub	x9, x1, x2
	add	x10, x10, 257
	sub	x1, x2, #1
	.p2align 5,,15
L242:
	add	x1, x1, 1
	subs	w2, w1, w5
	bvs	L238
	cmp	w2, 31
	bgt	L240
	bhi	L258
	ldrb	w2, [x9, x1]
	strb	w2, [x10, x1]
L240:
	cmp	x7, x1
	bne	L242
L236:
	cmp	w6, w12
	bgt	L243
	sub	x5, x11, x4
	sub	x3, x3, x4
	add	x5, x5, 289
	sub	x1, x4, #1
	.p2align 5,,15
L249:
	add	x1, x1, 1
	subs	w2, w1, w6
	bvs	L245
	cmp	w2, 127
	bgt	L247
	bhi	L259
	ldrb	w2, [x3, x1]
	strb	w2, [x5, x1]
L247:
	cmp	x8, x1
	bne	L249
L243:
	mov	w1, 1
	mov	w0, 0
	strb	w1, [x11, 256]
	ldp	x29, x30, [sp], 16
LCFI60:
	ret
	.p2align 2,,3
L250:
LCFI61:
	mov	w0, 2
	ldp	x29, x30, [sp], 16
LCFI62:
	ret
L238:
LCFI63:
	adrp	x0, lC4@PAGE
	mov	w1, 301
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L245:
	adrp	x0, lC4@PAGE
	mov	w1, 308
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L258:
	adrp	x0, lC4@PAGE
	mov	w1, 302
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L259:
	adrp	x0, lC4@PAGE
	mov	w1, 309
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L256:
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L257:
	adrp	x0, lC4@PAGE
	mov	w1, 294
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE16:
	.const
	.align	3
lC12:
	.ascii "failed precondition from anubis_proof_of_build.ads:239"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__verify_commit
_anubis_proof_of_build__verify_commit:
LFB17:
	stp	x29, x30, [sp, -16]!
LCFI64:
	mov	x29, sp
LCFI65:
	ldp	w3, w5, [x2]
	sxtw	x2, w3
	sxtw	x4, w5
	add	x6, x2, 31
	cmp	x6, x4
	bne	L274
	cmp	w3, w5
	bgt	L269
	sub	x0, x0, x2
	sub	x6, x1, x2
	add	x5, x0, 73
	sub	x0, x2, #1
	.p2align 5,,15
L268:
	add	x0, x0, 1
	subs	w1, w0, w3
	bvs	L264
	cmp	w1, 31
	bgt	L266
	bhi	L275
	ldrb	w2, [x6, x0]
	ldrb	w1, [x5, x0]
	cmp	w2, w1
	bne	L270
L266:
	cmp	x4, x0
	bne	L268
L269:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI66:
	ret
	.p2align 2,,3
L270:
LCFI67:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI68:
	ret
L264:
LCFI69:
	adrp	x0, lC4@PAGE
	mov	w1, 324
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L275:
	adrp	x0, lC4@PAGE
	mov	w1, 325
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L274:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE17:
	.const
	.align	3
lC13:
	.ascii "failed precondition from anubis_proof_of_build.ads:247"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__verify_proof
_anubis_proof_of_build__verify_proof:
LFB18:
	stp	x29, x30, [sp, -16]!
LCFI70:
	mov	x29, sp
LCFI71:
	ldp	w3, w5, [x2]
	sxtw	x2, w3
	sxtw	x4, w5
	add	x6, x2, 31
	cmp	x6, x4
	bne	L290
	cmp	w3, w5
	bgt	L285
	sub	x0, x0, x2
	sub	x6, x1, x2
	add	x5, x0, 105
	sub	x0, x2, #1
	.p2align 5,,15
L284:
	add	x0, x0, 1
	subs	w1, w0, w3
	bvs	L280
	cmp	w1, 31
	bgt	L282
	bhi	L291
	ldrb	w2, [x6, x0]
	ldrb	w1, [x5, x0]
	cmp	w2, w1
	bne	L286
L282:
	cmp	x4, x0
	bne	L284
L285:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI72:
	ret
	.p2align 2,,3
L286:
LCFI73:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI74:
	ret
L280:
LCFI75:
	adrp	x0, lC4@PAGE
	mov	w1, 340
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L291:
	adrp	x0, lC4@PAGE
	mov	w1, 341
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L290:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE18:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__get_nft_by_id
_anubis_proof_of_build__get_nft_by_id:
LFB19:
	mov	x5, x3
	ldr	w7, [x1]
	add	x12, x3, 41
	movi	v31.4s, 0
	add	x11, x3, 73
	add	x10, x3, 105
	str	xzr, [x5], 8
	add	x8, x3, 202
	add	x6, x3, 257
	stp	xzr, xzr, [x3, 8]
	add	x4, x3, 289
	stp	xzr, xzr, [x5, 16]
	strb	wzr, [x3, 40]
	stp	xzr, xzr, [x12]
	stp	xzr, xzr, [x12, 16]
	stp	xzr, xzr, [x11]
	stp	xzr, xzr, [x11, 16]
	stp	xzr, xzr, [x10]
	stp	xzr, xzr, [x10, 16]
	str	q31, [x3, 172]
	str	xzr, [x3, 140]
	str	xzr, [x3, 148]
	str	q31, [x3, 156]
	str	q31, [x3, 186]
	stp	xzr, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
	stp	xzr, xzr, [x3, 240]
	strb	wzr, [x3, 256]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	stp	q31, q31, [x4]
	stp	q31, q31, [x4, 32]
	stp	q31, q31, [x4, 64]
	stp	q31, q31, [x4, 96]
	ldp	w5, w6, [x1]
	cmp	w5, w6
	bgt	L296
	sxtw	x5, w5
	sxtw	x7, w7
	sub	x5, x5, #1
	sxtw	x6, w6
	b	L295
	.p2align 2,,3
L294:
	cmp	x6, x5
	beq	L296
L295:
	add	x5, x5, 1
	sub	x4, x5, x7
	add	x1, x4, x4, lsl 1
	add	x1, x4, x1, lsl 2
	add	x1, x4, x1, lsl 2
	ldr	x4, [x0, x1, lsl 3]
	add	x1, x0, x1, lsl 3
	cmp	x4, x2
	bne	L294
	stp	x29, x30, [sp, -16]!
LCFI76:
	mov	x0, x3
	mov	x29, sp
LCFI77:
	mov	x2, 424
	bl	_memcpy
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI78:
	ret
	.p2align 2,,3
L296:
	mov	w0, 0
	ret
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__get_total_minted
_anubis_proof_of_build__get_total_minted:
LFB20:
	ldr	x0, [x0]
	ret
LFE20:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__get_total_lines
_anubis_proof_of_build__get_total_lines:
LFB21:
	ldr	x0, [x0, 8]
	ret
LFE21:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__get_total_vcs
_anubis_proof_of_build__get_total_vcs:
LFB22:
	ldr	x0, [x0, 16]
	ret
LFE22:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__get_category_count
_anubis_proof_of_build__get_category_count:
LFB23:
	cmp	w1, 8
	bhi	L310
	add	x1, x0, w1, uxtb 2
	ldr	w0, [x1, 24]
	ret
L310:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI79:
	mov	w1, 411
	mov	x29, sp
LCFI80:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_proof_of_build.ads:292"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__generate_token_uri
_anubis_proof_of_build__generate_token_uri:
LFB24:
	stp	x29, x30, [sp, -48]!
LCFI81:
	mov	x29, sp
LCFI82:
	stp	x19, x20, [sp, 16]
LCFI83:
	mov	x20, x0
	stp	x21, x22, [sp, 32]
LCFI84:
	mov	x22, x2
	ldp	w19, w3, [x2]
	sxtw	x21, w19
	sxtw	x2, w3
	add	x0, x21, 254
	cmp	x0, x2
	bge	L337
	tbnz	w19, #31, L338
	mov	x6, x1
	cmp	w19, w3
	mov	w1, 0
	mov	x0, x6
	sub	x2, x2, x21
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldp	w1, w5, [x22]
	adrp	x4, _prefix.8@PAGE
	mov	x6, x0
	add	x4, x4, _prefix.8@PAGEOFF;
	mov	x0, 0
	mov	w7, 2147483647
	.p2align 5,,15
L318:
	cmp	w5, w19
	blt	L315
	cmp	w1, w19
	bgt	L339
	ldrsb	w3, [x4, x0]
	sxtw	x2, w19
	sub	x2, x2, x21
	strb	w3, [x6, x2]
	cmp	w19, w7
	beq	L340
	add	w19, w19, 1
L315:
	add	x0, x0, 1
	cmp	x0, 7
	bne	L318
	add	x2, x20, 156
	add	x3, x20, 202
	mov	w7, 2147483647
	.p2align 5,,15
L323:
	ldrb	w0, [x2]
	cbz	w0, L319
	tbnz	w19, #31, L341
	cmp	w5, w19
	blt	L319
	cmp	w1, w19
	bgt	L342
	sxtw	x4, w19
	sub	x4, x4, x21
	strb	w0, [x6, x4]
	cmp	w19, w7
	beq	L343
	add	w19, w19, 1
L319:
	add	x2, x2, 1
	cmp	x2, x3
	bne	L323
	tbnz	w19, #31, L344
	subs	w0, w19, w1
	bvs	L326
	tbnz	w0, #31, L345
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI85:
	ret
L341:
LCFI86:
	adrp	x0, lC4@PAGE
	mov	w1, 438
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L343:
	adrp	x0, lC4@PAGE
	mov	w1, 440
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L342:
	adrp	x0, lC4@PAGE
	mov	w1, 439
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L344:
	adrp	x0, lC4@PAGE
	mov	w1, 444
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L326:
	adrp	x0, lC4@PAGE
	mov	w1, 444
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L345:
	adrp	x0, lC4@PAGE
	mov	w1, 444
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L339:
	adrp	x0, lC4@PAGE
	mov	w1, 431
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L338:
	adrp	x0, lC4@PAGE
	mov	w1, 423
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L337:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L340:
	adrp	x0, lC4@PAGE
	mov	w1, 432
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE24:
	.const
	.align	3
lC15:
	.ascii "failed precondition from anubis_proof_of_build.ads:301"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__generate_collection_metadata
_anubis_proof_of_build__generate_collection_metadata:
LFB25:
	stp	x29, x30, [sp, -96]!
LCFI87:
	mov	x29, sp
LCFI88:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI89:
	mov	x21, x0
	add	x0, x29, 96
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w22, w3, [x2]
	str	x0, [x29, 88]
	sxtw	x19, w22
	sxtw	x2, w3
	add	x0, x19, 510
	str	x19, [x29, 64]
	cmp	x0, x2
	bge	L364
	tbnz	w22, #31, L365
	ldr	x0, [x29, 48]
	mov	w1, 0
	cmp	w22, w3
	add	x20, x29, 64
	sub	x2, x2, x19
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldr	x0, [x21]
	mov	x16, x20
	str	w22, [x29, 80]
	bl	_anubis_proof_of_build__generate_collection_metadata__write_u64.1
	ldr	x0, [x21, 8]
	mov	x16, x20
	bl	_anubis_proof_of_build__generate_collection_metadata__write_u64.1
	ldr	x0, [x21, 16]
	mov	x16, x20
	bl	_anubis_proof_of_build__generate_collection_metadata__write_u64.1
	ldp	x8, x0, [x29, 48]
	add	x2, x21, 60
	add	x5, x21, 92
	mov	w9, 2147483647
	ldr	w1, [x29, 80]
	ldp	w7, w4, [x0]
	tbnz	w1, #31, L366
	.p2align 5,,15
L350:
	cmp	w4, w1
	blt	L351
	cmp	w7, w1
	bgt	L367
	ldrb	w6, [x2]
	sxtw	x3, w1
	sub	x3, x3, x19
	strb	w6, [x8, x3]
	cmp	w1, w9
	beq	L368
	add	w1, w1, 1
L351:
	add	x2, x2, 1
	cmp	x5, x2
	bne	L350
	ldr	x0, [x21, 96]
	mov	x16, x20
	str	w1, [x29, 80]
	bl	_anubis_proof_of_build__generate_collection_metadata__write_u64.1
	ldr	w0, [x29, 80]
	tbnz	w0, #31, L369
	ldr	x1, [x29, 56]
	ldr	w1, [x1]
	subs	w0, w0, w1
	bvs	L356
	tbnz	w0, #31, L370
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI90:
	ret
L366:
LCFI91:
	adrp	x0, lC4@PAGE
	mov	w1, 475
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L368:
	adrp	x0, lC4@PAGE
	mov	w1, 477
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L367:
	adrp	x0, lC4@PAGE
	mov	w1, 476
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L365:
	adrp	x0, lC4@PAGE
	mov	w1, 452
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L364:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L370:
	adrp	x0, lC4@PAGE
	mov	w1, 483
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L356:
	adrp	x0, lC4@PAGE
	mov	w1, 483
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L369:
	adrp	x0, lC4@PAGE
	mov	w1, 483
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__calculate_statistics
_anubis_proof_of_build__calculate_statistics:
LFB27:
	stp	x29, x30, [sp, -16]!
LCFI92:
	mov	x29, sp
LCFI93:
	ldr	x1, [x0, 16]
	ldp	x4, x2, [x0]
	str	x1, [x3, 16]
	stp	x4, x2, [x3]
	cbz	x4, L372
	udiv	x2, x2, x4
	mov	x5, 4294967295
	cmp	x2, x5
	bhi	L378
	udiv	x1, x1, x4
	str	w2, [x3, 24]
	cmp	x1, x5
	bls	L375
	adrp	x0, lC4@PAGE
	mov	w1, 506
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L372:
	mov	w1, 0
	str	wzr, [x3, 24]
L375:
	ldr	w2, [x0, 56]
	stp	w1, wzr, [x3, 28]
	tbnz	w2, #31, L379
	ldr	q31, [x0, 96]
	str	w2, [x3, 36]
	str	q31, [x3, 40]
	ldp	x29, x30, [sp], 16
LCFI94:
	ret
L378:
LCFI95:
	adrp	x0, lC4@PAGE
	mov	w1, 504
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L379:
	adrp	x0, lC4@PAGE
	mov	w1, 513
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE27:
	.const
	.align	3
lC16:
	.ascii "failed precondition from anubis_proof_of_build.ads:336"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__serialize_nft
_anubis_proof_of_build__serialize_nft:
LFB28:
	stp	x29, x30, [sp, -112]!
LCFI96:
	mov	x29, sp
LCFI97:
	add	x3, x29, 64
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI98:
	mov	x22, x0
	add	x0, x29, 112
	str	x23, [sp, 48]
LCFI99:
	stp	x1, x2, [x29, 64]
	str	x3, [x29, 88]
	ldp	w23, w3, [x2]
	str	x0, [x29, 104]
	sxtw	x21, w23
	sxtw	x2, w3
	add	x0, x21, 510
	str	x21, [x29, 80]
	cmp	x0, x2
	bge	L416
	tbnz	w23, #31, L417
	ldr	x0, [x29, 64]
	cmp	w23, w3
	mov	w1, 0
	sub	x2, x2, x21
	mov	x19, x22
	csinc	x2, xzr, x2, gt
	add	x20, x29, 80
	bl	_memset
	ldr	x0, [x19], 8
	mov	x16, x20
	str	w23, [x29, 96]
	bl	_anubis_proof_of_build__serialize_nft__write_u64.3
	ldp	x6, x0, [x29, 64]
	add	x4, x22, 40
	mov	w8, 2147483647
	ldr	w1, [x29, 96]
	ldp	w5, w3, [x0]
	tbnz	w1, #31, L418
	.p2align 5,,15
L384:
	cmp	w3, w1
	blt	L385
	cmp	w5, w1
	bgt	L419
	ldrb	w7, [x19]
	sxtw	x2, w1
	sub	x2, x2, x21
	strb	w7, [x6, x2]
	cmp	w1, w8
	beq	L420
	add	w1, w1, 1
L385:
	add	x19, x19, 1
	cmp	x4, x19
	bne	L384
	cmp	w3, w1
	bge	L421
L388:
	add	x2, x22, 73
	add	x7, x22, 105
	mov	w0, 2147483647
	.p2align 5,,15
L395:
	cmp	w3, w1
	blt	L392
	cmp	w5, w1
	bgt	L422
	ldrb	w8, [x2]
	sxtw	x4, w1
	sub	x4, x4, x21
	strb	w8, [x6, x4]
	cmp	w1, w0
	beq	L423
	add	w1, w1, 1
L392:
	add	x2, x2, 1
	cmp	x2, x7
	bne	L395
	add	x7, x22, 137
	mov	w0, 2147483647
	.p2align 5,,15
L399:
	cmp	w3, w1
	blt	L396
	cmp	w5, w1
	bgt	L424
	ldrb	w8, [x2]
	sxtw	x4, w1
	sub	x4, x4, x21
	strb	w8, [x6, x4]
	cmp	w1, w0
	beq	L425
	add	w1, w1, 1
L396:
	add	x2, x2, 1
	cmp	x2, x7
	bne	L399
	ldr	w0, [x22, 140]
	mov	x16, x20
	str	w1, [x29, 96]
	bl	_anubis_proof_of_build__serialize_nft__write_u32.2
	ldr	w0, [x22, 144]
	mov	x16, x20
	bl	_anubis_proof_of_build__serialize_nft__write_u32.2
	ldr	w0, [x22, 148]
	mov	x16, x20
	bl	_anubis_proof_of_build__serialize_nft__write_u32.2
	ldr	x0, [x22, 240]
	mov	x16, x20
	bl	_anubis_proof_of_build__serialize_nft__write_u64.3
	ldr	w0, [x29, 96]
	tbnz	w0, #31, L426
	ldr	x1, [x29, 72]
	ldp	w1, w2, [x1]
	cmp	w2, w0
	blt	L401
	cmp	w1, w0
	bgt	L427
	ldrb	w3, [x22, 256]
	cmp	w3, 1
	bhi	L428
	ldr	x5, [x29, 64]
	sxtw	x2, w0
	mov	w4, 2147483647
	sub	x2, x2, x21
	strb	w3, [x5, x2]
	cmp	w0, w4
	beq	L429
	add	w0, w0, 1
L401:
	subs	w0, w0, w1
	bvs	L406
	tbnz	w0, #31, L430
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 112
LCFI100:
	ret
	.p2align 2,,3
L421:
LCFI101:
	cmp	w5, w1
	bgt	L431
	ldrb	w2, [x22, 40]
	cmp	w2, 8
	bhi	L432
	sxtw	x0, w1
	mov	w4, 2147483647
	sub	x0, x0, x21
	strb	w2, [x6, x0]
	cmp	w1, w4
	beq	L433
	add	w1, w1, 1
	b	L388
L418:
	adrp	x0, lC4@PAGE
	mov	w1, 560
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L419:
	adrp	x0, lC4@PAGE
	mov	w1, 561
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L420:
	adrp	x0, lC4@PAGE
	mov	w1, 562
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L425:
	adrp	x0, lC4@PAGE
	mov	w1, 584
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L424:
	adrp	x0, lC4@PAGE
	mov	w1, 583
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L423:
	adrp	x0, lC4@PAGE
	mov	w1, 576
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L422:
	adrp	x0, lC4@PAGE
	mov	w1, 575
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L416:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L417:
	adrp	x0, lC4@PAGE
	mov	w1, 527
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L429:
	adrp	x0, lC4@PAGE
	mov	w1, 597
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L430:
	adrp	x0, lC4@PAGE
	mov	w1, 600
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L406:
	adrp	x0, lC4@PAGE
	mov	w1, 600
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L431:
	adrp	x0, lC4@PAGE
	mov	w1, 568
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L432:
	adrp	x0, lC4@PAGE
	mov	w1, 568
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L433:
	adrp	x0, lC4@PAGE
	mov	w1, 569
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L427:
	adrp	x0, lC4@PAGE
	mov	w1, 596
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L428:
	adrp	x0, lC4@PAGE
	mov	w1, 596
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L426:
	adrp	x0, lC4@PAGE
	mov	w1, 595
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE28:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__deserialize_nft
_anubis_proof_of_build__deserialize_nft:
LFB31:
	stp	x29, x30, [sp, -112]!
LCFI102:
	mov	x29, sp
LCFI103:
	add	x3, x29, 64
	stp	x0, x1, [x29, 64]
	add	x0, x29, 112
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	ldr	w7, [x1]
	str	x23, [sp, 48]
LCFI104:
	str	x3, [x29, 88]
	str	x0, [x29, 104]
	tbnz	w7, #31, L463
	mov	x20, x2
	mov	x22, x2
	movi	v31.4s, 0
	add	x6, x2, 41
	add	x19, x2, 73
	str	xzr, [x20], 8
	add	x23, x2, 105
	add	x4, x2, 202
	stp	xzr, xzr, [x2, 8]
	add	x3, x2, 257
	add	x2, x2, 289
	mov	w0, 0
	stp	xzr, xzr, [x20, 16]
	strb	wzr, [x22, 40]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	stp	xzr, xzr, [x19]
	stp	xzr, xzr, [x19, 16]
	stp	xzr, xzr, [x23]
	stp	xzr, xzr, [x23, 16]
	str	q31, [x22, 172]
	str	xzr, [x22, 140]
	str	xzr, [x22, 148]
	str	q31, [x22, 156]
	str	q31, [x22, 186]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	stp	xzr, xzr, [x22, 240]
	strb	wzr, [x22, 256]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	stp	q31, q31, [x2]
	stp	q31, q31, [x2, 32]
	stp	q31, q31, [x2, 64]
	stp	q31, q31, [x2, 96]
	ldpsw	x2, x3, [x1]
	add	x1, x2, 108
	cmp	x1, x3
	blt	L464
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 112
LCFI105:
	ret
	.p2align 2,,3
L464:
LCFI106:
	sxtw	x0, w7
	add	x21, x29, 80
	str	w7, [x29, 96]
	mov	x16, x21
	str	x0, [x29, 80]
	bl	_anubis_proof_of_build__deserialize_nft__read_u64.5
	mov	x2, x22
	ldr	w1, [x29, 96]
	mov	x3, x20
	mov	w8, 2147483647
	str	x0, [x2], 40
	ldp	x6, x4, [x29, 64]
	ldr	x7, [x29, 80]
	ldp	w5, w4, [x4]
	tbnz	w1, #31, L465
	.p2align 5,,15
L437:
	cmp	w4, w1
	blt	L438
	cmp	w5, w1
	bgt	L466
	sxtw	x0, w1
	sub	x0, x0, x7
	ldrb	w0, [x6, x0]
	strb	w0, [x3]
	cmp	w1, w8
	beq	L467
	add	w1, w1, 1
L438:
	add	x3, x3, 1
	cmp	x2, x3
	bne	L437
	cmp	w4, w1
	bge	L468
L441:
	mov	x0, x19
	mov	w3, 2147483647
	.p2align 5,,15
L448:
	cmp	w4, w1
	blt	L445
	cmp	w5, w1
	bgt	L469
	sxtw	x2, w1
	sub	x2, x2, x7
	ldrb	w2, [x6, x2]
	strb	w2, [x0]
	cmp	w1, w3
	beq	L470
	add	w1, w1, 1
L445:
	add	x0, x0, 1
	cmp	x0, x23
	bne	L448
	add	x3, x22, 137
	mov	w8, 2147483647
	.p2align 5,,15
L452:
	cmp	w4, w1
	blt	L449
	cmp	w5, w1
	bgt	L471
	sxtw	x2, w1
	sub	x2, x2, x7
	ldrb	w2, [x6, x2]
	strb	w2, [x0]
	cmp	w1, w8
	beq	L472
	add	w1, w1, 1
L449:
	add	x0, x0, 1
	cmp	x0, x3
	bne	L452
	mov	x16, x21
	str	w1, [x29, 96]
	bl	_anubis_proof_of_build__deserialize_nft__read_u32.4
	mov	x16, x21
	str	w0, [x22, 140]
	bl	_anubis_proof_of_build__deserialize_nft__read_u32.4
	mov	x16, x21
	str	w0, [x22, 144]
	bl	_anubis_proof_of_build__deserialize_nft__read_u32.4
	mov	x16, x21
	str	w0, [x22, 148]
	bl	_anubis_proof_of_build__deserialize_nft__read_u64.5
	ldr	w1, [x29, 96]
	str	x0, [x22, 240]
	ldr	x3, [x29, 80]
	tbnz	w1, #31, L473
	ldr	x0, [x29, 72]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L456
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L474
	ldr	x4, [x29, 64]
	sxtw	x0, w1
	mov	w2, 2147483647
	sub	x0, x0, x3
	ldrb	w0, [x4, x0]
	cmp	w0, 0
	cset	w0, ne
	strb	w0, [x22, 256]
	cmp	w1, w2
	beq	L475
L456:
	mov	w0, 1
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 112
LCFI107:
	ret
	.p2align 2,,3
L468:
LCFI108:
	cmp	w5, w1
	bgt	L476
	sxtw	x0, w1
	sub	x0, x0, x7
	ldrb	w0, [x6, x0]
	cmp	w0, 8
	bhi	L443
	strb	w0, [x22, 40]
L443:
	mov	w0, 2147483647
	cmp	w1, w0
	beq	L477
	add	w1, w1, 1
	b	L441
L465:
	adrp	x0, lC4@PAGE
	mov	w1, 663
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L469:
	adrp	x0, lC4@PAGE
	mov	w1, 681
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L470:
	adrp	x0, lC4@PAGE
	mov	w1, 682
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L471:
	adrp	x0, lC4@PAGE
	mov	w1, 689
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L472:
	adrp	x0, lC4@PAGE
	mov	w1, 690
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L466:
	adrp	x0, lC4@PAGE
	mov	w1, 664
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L467:
	adrp	x0, lC4@PAGE
	mov	w1, 665
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L463:
	adrp	x0, lC4@PAGE
	mov	w1, 608
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L473:
	adrp	x0, lC4@PAGE
	mov	w1, 701
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L475:
	adrp	x0, lC4@PAGE
	mov	w1, 703
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L474:
	adrp	x0, lC4@PAGE
	mov	w1, 702
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L477:
	adrp	x0, lC4@PAGE
	mov	w1, 675
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L476:
	adrp	x0, lC4@PAGE
	mov	w1, 671
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE31:
	.const
	.align	3
lC17:
	.ascii "failed precondition from anubis_proof_of_build.ads:351"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__serialize_collection_state
_anubis_proof_of_build__serialize_collection_state:
LFB34:
	stp	x29, x30, [sp, -96]!
LCFI109:
	mov	x29, sp
LCFI110:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
LCFI111:
	mov	x20, x0
	add	x0, x29, 96
	stp	x21, x22, [sp, 32]
LCFI112:
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w22, w3, [x2]
	str	x0, [x29, 88]
	sxtw	x19, w22
	sxtw	x2, w3
	add	x0, x19, 254
	str	x19, [x29, 64]
	cmp	x0, x2
	bge	L496
	tbnz	w22, #31, L497
	ldr	x0, [x29, 48]
	mov	w1, 0
	cmp	w22, w3
	add	x21, x29, 64
	sub	x2, x2, x19
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldr	x0, [x20]
	mov	x16, x21
	str	w22, [x29, 80]
	bl	_anubis_proof_of_build__serialize_collection_state__write_u64.6
	ldr	x0, [x20, 8]
	mov	x16, x21
	bl	_anubis_proof_of_build__serialize_collection_state__write_u64.6
	ldr	x0, [x20, 16]
	mov	x16, x21
	bl	_anubis_proof_of_build__serialize_collection_state__write_u64.6
	ldp	x7, x0, [x29, 48]
	add	x2, x20, 60
	add	x5, x20, 92
	mov	w8, 2147483647
	ldr	w1, [x29, 80]
	ldp	w0, w4, [x0]
	tbnz	w1, #31, L498
	.p2align 5,,15
L482:
	cmp	w4, w1
	blt	L483
	cmp	w0, w1
	bgt	L499
	ldrb	w6, [x2]
	sxtw	x3, w1
	sub	x3, x3, x19
	strb	w6, [x7, x3]
	cmp	w1, w8
	beq	L500
	add	w1, w1, 1
L483:
	add	x2, x2, 1
	cmp	x2, x5
	bne	L482
	ldr	x0, [x20, 96]
	mov	x16, x21
	str	w1, [x29, 80]
	bl	_anubis_proof_of_build__serialize_collection_state__write_u64.6
	ldr	x0, [x20, 104]
	mov	x16, x21
	bl	_anubis_proof_of_build__serialize_collection_state__write_u64.6
	ldr	w0, [x29, 80]
	tbnz	w0, #31, L501
	ldr	x1, [x29, 56]
	ldr	w1, [x1]
	subs	w0, w0, w1
	bvs	L488
	tbnz	w0, #31, L502
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI113:
	ret
L498:
LCFI114:
	adrp	x0, lC4@PAGE
	mov	w1, 737
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L500:
	adrp	x0, lC4@PAGE
	mov	w1, 739
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L499:
	adrp	x0, lC4@PAGE
	mov	w1, 738
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L497:
	adrp	x0, lC4@PAGE
	mov	w1, 714
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L496:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L502:
	adrp	x0, lC4@PAGE
	mov	w1, 746
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L488:
	adrp	x0, lC4@PAGE
	mov	w1, 746
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L501:
	adrp	x0, lC4@PAGE
	mov	w1, 746
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE34:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__deserialize_collection_state
_anubis_proof_of_build__deserialize_collection_state:
LFB36:
	stp	x29, x30, [sp, -96]!
LCFI115:
	mov	x29, sp
LCFI116:
	add	x3, x29, 48
	stp	x0, x1, [x29, 48]
	add	x0, x29, 96
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI117:
	ldr	w21, [x1]
	str	x3, [x29, 72]
	str	x0, [x29, 88]
	tbnz	w21, #31, L514
	mov	x19, x2
	adrp	x1, lC2@PAGE
	adrp	x2, lC3@PAGE
	add	x1, x1, lC2@PAGEOFF;
	add	x2, x2, lC3@PAGEOFF;
	mov	x0, x19
	mov	x3, 0
	bl	_anubis_proof_of_build__init_collection
	ldr	x2, [x29, 56]
	mov	w0, 0
	ldpsw	x1, x2, [x2]
	add	x1, x1, 70
	cmp	x1, x2
	blt	L515
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI118:
	ret
	.p2align 2,,3
L515:
LCFI119:
	add	x20, x29, 64
	sxtw	x0, w21
	str	w21, [x29, 80]
	mov	x16, x20
	str	x0, [x29, 64]
	bl	_anubis_proof_of_build__deserialize_collection_state__read_u64.7
	mov	x16, x20
	str	x0, [x19]
	bl	_anubis_proof_of_build__deserialize_collection_state__read_u64.7
	mov	x16, x20
	str	x0, [x19, 8]
	bl	_anubis_proof_of_build__deserialize_collection_state__read_u64.7
	ldp	x7, x4, [x29, 48]
	add	x3, x19, 60
	add	x6, x19, 92
	mov	w8, 2147483647
	str	x0, [x19, 16]
	ldr	w1, [x29, 80]
	ldr	x2, [x29, 64]
	ldp	w0, w5, [x4]
	tbnz	w1, #31, L516
	.p2align 5,,15
L506:
	cmp	w5, w1
	blt	L507
	cmp	w0, w1
	bgt	L517
	sxtw	x4, w1
	sub	x4, x4, x2
	ldrb	w4, [x7, x4]
	strb	w4, [x3]
	cmp	w1, w8
	beq	L518
	add	w1, w1, 1
L507:
	add	x3, x3, 1
	cmp	x6, x3
	bne	L506
	mov	x16, x20
	str	w1, [x29, 80]
	bl	_anubis_proof_of_build__deserialize_collection_state__read_u64.7
	mov	x16, x20
	str	x0, [x19, 96]
	bl	_anubis_proof_of_build__deserialize_collection_state__read_u64.7
	mov	x1, x0
	mov	w0, 1
	str	x1, [x19, 104]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI120:
	ret
L516:
LCFI121:
	adrp	x0, lC4@PAGE
	mov	w1, 781
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L517:
	adrp	x0, lC4@PAGE
	mov	w1, 782
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L518:
	adrp	x0, lC4@PAGE
	mov	w1, 783
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L514:
	adrp	x0, lC4@PAGE
	mov	w1, 754
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE36:
	.const
lC2:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.align	2
lC3:
	.word	0
	.word	31
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__zeroize_collection_state
_anubis_proof_of_build__zeroize_collection_state:
LFB38:
	movi	v31.4s, 0
	add	x1, x0, 60
	str	xzr, [x0, 16]
	stp	xzr, xzr, [x0, 24]
	str	q31, [x0]
	stp	xzr, xzr, [x0, 40]
	str	wzr, [x0, 56]
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	str	q31, [x0, 96]
	ret
LFE38:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__zeroize_nft
_anubis_proof_of_build__zeroize_nft:
LFB39:
	mov	x2, x0
	add	x8, x0, 41
	movi	v31.4s, 0
	add	x7, x0, 73
	add	x6, x0, 105
	str	xzr, [x2], 8
	add	x4, x0, 202
	add	x3, x0, 257
	stp	xzr, xzr, [x0, 8]
	add	x1, x0, 289
	stp	xzr, xzr, [x2, 16]
	strb	wzr, [x0, 40]
	stp	xzr, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
	stp	xzr, xzr, [x7]
	stp	xzr, xzr, [x7, 16]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	str	q31, [x0, 172]
	str	xzr, [x0, 140]
	str	xzr, [x0, 148]
	str	q31, [x0, 156]
	str	q31, [x0, 186]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	stp	xzr, xzr, [x0, 240]
	strb	wzr, [x0, 256]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	stp	q31, q31, [x1]
	stp	q31, q31, [x1, 32]
	stp	q31, q31, [x1, 64]
	stp	q31, q31, [x1, 96]
	ret
LFE39:
	.align	2
	.p2align 5,,15
	.globl _anubis_proof_of_build__zeroize_weekly_update
_anubis_proof_of_build__zeroize_weekly_update:
LFB40:
	movi	v31.4s, 0
	add	x1, x0, 110
	str	xzr, [x0]
	str	wzr, [x0, 8]
	strh	wzr, [x0, 12]
	stp	xzr, xzr, [x0, 16]
	stp	q31, q31, [x0, 64]
	stp	xzr, xzr, [x0, 32]
	stp	xzr, xzr, [x0, 48]
	str	q31, [x0, 94]
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	stp	xzr, xzr, [x0, 144]
	str	xzr, [x0, 160]
	ret
LFE40:
	.const
	.align	3
_prefix.8:
	.ascii "ipfs://"
	.space 1
	.align	3
_verify_resultG.9:
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	2
	.space 7
	.align	3
_mint_resultG.13:
	.byte	0
	.byte	0
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	5
	.byte	4
	.byte	0
	.byte	2
	.space 1
	.align	1
_mint_resultT2.14:
	.byte	5
	.byte	1
	.align	1
_mint_resultT1.15:
	.byte	7
	.byte	3
	.align	3
_mint_resultP.16:
	.word	3
	.word	14
_build_categoryG.17:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	7
	.byte	5
	.byte	1
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	7
	.byte	3
	.byte	0
	.byte	0
	.byte	4
	.globl _anubis_proof_of_build__verify_resultN
	.align	3
_anubis_proof_of_build__verify_resultN:
	.byte	1
	.byte	9
	.byte	26
	.byte	42
	.byte	51
	.space 3
	.globl _anubis_proof_of_build__verify_resultS
	.align	3
_anubis_proof_of_build__verify_resultS:
	.ascii "VERIFIEDINVALID_SIGNATUREALREADY_VERIFIEDNOT_FOUND"
	.globl _anubis_proof_of_build__mint_resultN
	.align	3
_anubis_proof_of_build__mint_resultN:
	.byte	1
	.byte	7
	.byte	25
	.byte	41
	.byte	57
	.byte	70
	.byte	81
	.space 1
	.globl _anubis_proof_of_build__mint_resultS
	.align	3
_anubis_proof_of_build__mint_resultS:
	.ascii "MINTEDINSUFFICIENT_LINESINSUFFICIENT_VCSDUPLICATE_COMMITINVALID_PROOFNOT_BUILDER"
	.globl _anubis_proof_of_build__build_categoryN
	.align	3
_anubis_proof_of_build__build_categoryN:
	.byte	1
	.byte	18
	.byte	35
	.byte	48
	.byte	60
	.byte	71
	.byte	84
	.byte	91
	.byte	102
	.byte	111
	.space 6
	.globl _anubis_proof_of_build__build_categoryS
	.align	3
_anubis_proof_of_build__build_categoryS:
	.ascii "CORE_CRYPTOGRAPHYVM_IMPLEMENTATIONPRIVACY_LAYERPROOF_SYSTEMSDK_TOOLINGDOCUMENTATIONTESTINGINTEGRATIONMILESTONE"
	.globl _anubis_proof_of_build__collection_symbol
	.align	2
_anubis_proof_of_build__collection_symbol:
	.ascii "APOB"
	.globl _anubis_proof_of_build__collection_name
	.align	3
_anubis_proof_of_build__collection_name:
	.ascii "ANUBIS Proof-of-Build"
	.data
	.align	3
_anubis_proof_of_build__weekly_nft_counter:
	.space 8
	.align	3
_anubis_proof_of_build__nft_counter:
	.space 8
	.globl _anubis_proof_of_build_E
	.align	1
_anubis_proof_of_build_E:
	.space 2
	.literal16
	.align	4
lC7:
	.xword	1
	.xword	1
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
	.quad	LFB26-.
	.set L$set$2,LFE26-LFB26
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB26
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
	.quad	LFB29-.
	.set L$set$8,LFE29-LFB29
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB29
	.long L$set$9
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$10,LCFI5-LCFI4
	.long L$set$10
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$11,LCFI6-LCFI5
	.long L$set$11
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$13,LEFDE5-LASFDE5
	.long L$set$13
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB35-.
	.set L$set$14,LFE35-LFB35
	.quad L$set$14
	.uleb128 0
	.byte	0x4
	.set L$set$15,LCFI8-LFB35
	.long L$set$15
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$16,LCFI9-LCFI8
	.long L$set$16
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$17,LCFI10-LCFI9
	.long L$set$17
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$19,LEFDE7-LASFDE7
	.long L$set$19
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB30-.
	.set L$set$20,LFE30-LFB30
	.quad L$set$20
	.uleb128 0
	.byte	0x4
	.set L$set$21,LCFI12-LFB30
	.long L$set$21
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$22,LCFI13-LCFI12
	.long L$set$22
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$23,LCFI14-LCFI13
	.long L$set$23
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$24,LCFI15-LCFI14
	.long L$set$24
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$25,LEFDE9-LASFDE9
	.long L$set$25
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB33-.
	.set L$set$26,LFE33-LFB33
	.quad L$set$26
	.uleb128 0
	.byte	0x4
	.set L$set$27,LCFI16-LFB33
	.long L$set$27
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$28,LCFI17-LCFI16
	.long L$set$28
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$29,LCFI18-LCFI17
	.long L$set$29
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$30,LCFI19-LCFI18
	.long L$set$30
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$31,LCFI20-LCFI19
	.long L$set$31
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$32,LCFI21-LCFI20
	.long L$set$32
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
	.set L$set$33,LCFI22-LCFI21
	.long L$set$33
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$34,LEFDE11-LASFDE11
	.long L$set$34
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB32-.
	.set L$set$35,LFE32-LFB32
	.quad L$set$35
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI23-LFB32
	.long L$set$36
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$37,LCFI24-LCFI23
	.long L$set$37
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$38,LCFI25-LCFI24
	.long L$set$38
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$39,LCFI26-LCFI25
	.long L$set$39
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$40,LCFI27-LCFI26
	.long L$set$40
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$41,LCFI28-LCFI27
	.long L$set$41
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
	.set L$set$42,LCFI29-LCFI28
	.long L$set$42
	.byte	0xb
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$43,LEFDE13-LASFDE13
	.long L$set$43
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB37-.
	.set L$set$44,LFE37-LFB37
	.quad L$set$44
	.uleb128 0
	.byte	0x4
	.set L$set$45,LCFI30-LFB37
	.long L$set$45
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$46,LCFI31-LCFI30
	.long L$set$46
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$47,LCFI32-LCFI31
	.long L$set$47
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$48,LCFI33-LCFI32
	.long L$set$48
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$49,LCFI34-LCFI33
	.long L$set$49
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$50,LCFI35-LCFI34
	.long L$set$50
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
	.quad	LFB2-.
	.set L$set$53,LFE2-LFB2
	.quad L$set$53
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$54,LEFDE17-LASFDE17
	.long L$set$54
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB3-.
	.set L$set$55,LFE3-LFB3
	.quad L$set$55
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$56,LEFDE19-LASFDE19
	.long L$set$56
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB4-.
	.set L$set$57,LFE4-LFB4
	.quad L$set$57
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$58,LEFDE21-LASFDE21
	.long L$set$58
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB5-.
	.set L$set$59,LFE5-LFB5
	.quad L$set$59
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$60,LEFDE23-LASFDE23
	.long L$set$60
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB81-.
	.set L$set$61,LFE81-LFB81
	.quad L$set$61
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$62,LEFDE25-LASFDE25
	.long L$set$62
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB83-.
	.set L$set$63,LFE83-LFB83
	.quad L$set$63
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$64,LEFDE27-LASFDE27
	.long L$set$64
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB8-.
	.set L$set$65,LFE8-LFB8
	.quad L$set$65
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$66,LEFDE29-LASFDE29
	.long L$set$66
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB9-.
	.set L$set$67,LFE9-LFB9
	.quad L$set$67
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$68,LEFDE31-LASFDE31
	.long L$set$68
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB85-.
	.set L$set$69,LFE85-LFB85
	.quad L$set$69
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$70,LEFDE33-LASFDE33
	.long L$set$70
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB11-.
	.set L$set$71,LFE11-LFB11
	.quad L$set$71
	.uleb128 0
	.byte	0x4
	.set L$set$72,LCFI37-LFB11
	.long L$set$72
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$73,LCFI38-LCFI37
	.long L$set$73
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$74,LCFI39-LCFI38
	.long L$set$74
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$75,LCFI40-LCFI39
	.long L$set$75
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$76,LEFDE35-LASFDE35
	.long L$set$76
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB13-.
	.set L$set$77,LFE13-LFB13
	.quad L$set$77
	.uleb128 0
	.byte	0x4
	.set L$set$78,LCFI41-LFB13
	.long L$set$78
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$79,LCFI42-LCFI41
	.long L$set$79
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$80,LCFI43-LCFI42
	.long L$set$80
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$81,LCFI44-LCFI43
	.long L$set$81
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$82,LCFI45-LCFI44
	.long L$set$82
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
	.set L$set$83,LCFI46-LCFI45
	.long L$set$83
	.byte	0xb
	.byte	0x4
	.set L$set$84,LCFI47-LCFI46
	.long L$set$84
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
	.set L$set$85,LCFI48-LCFI47
	.long L$set$85
	.byte	0xb
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$86,LEFDE37-LASFDE37
	.long L$set$86
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB14-.
	.set L$set$87,LFE14-LFB14
	.quad L$set$87
	.uleb128 0
	.byte	0x4
	.set L$set$88,LCFI49-LFB14
	.long L$set$88
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$89,LCFI50-LCFI49
	.long L$set$89
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$90,LCFI51-LCFI50
	.long L$set$90
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$91,LCFI52-LCFI51
	.long L$set$91
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$92,LEFDE39-LASFDE39
	.long L$set$92
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB15-.
	.set L$set$93,LFE15-LFB15
	.quad L$set$93
	.uleb128 0
	.byte	0x4
	.set L$set$94,LCFI53-LFB15
	.long L$set$94
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$95,LCFI54-LCFI53
	.long L$set$95
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$96,LCFI55-LCFI54
	.long L$set$96
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$97,LCFI56-LCFI55
	.long L$set$97
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
	.set L$set$98,LCFI57-LCFI56
	.long L$set$98
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$99,LEFDE41-LASFDE41
	.long L$set$99
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB16-.
	.set L$set$100,LFE16-LFB16
	.quad L$set$100
	.uleb128 0
	.byte	0x4
	.set L$set$101,LCFI58-LFB16
	.long L$set$101
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$102,LCFI59-LCFI58
	.long L$set$102
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$103,LCFI60-LCFI59
	.long L$set$103
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$104,LCFI61-LCFI60
	.long L$set$104
	.byte	0xb
	.byte	0x4
	.set L$set$105,LCFI62-LCFI61
	.long L$set$105
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$106,LCFI63-LCFI62
	.long L$set$106
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$107,LEFDE43-LASFDE43
	.long L$set$107
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB17-.
	.set L$set$108,LFE17-LFB17
	.quad L$set$108
	.uleb128 0
	.byte	0x4
	.set L$set$109,LCFI64-LFB17
	.long L$set$109
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$110,LCFI65-LCFI64
	.long L$set$110
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$111,LCFI66-LCFI65
	.long L$set$111
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI67-LCFI66
	.long L$set$112
	.byte	0xb
	.byte	0x4
	.set L$set$113,LCFI68-LCFI67
	.long L$set$113
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$114,LCFI69-LCFI68
	.long L$set$114
	.byte	0xb
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$115,LEFDE45-LASFDE45
	.long L$set$115
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB18-.
	.set L$set$116,LFE18-LFB18
	.quad L$set$116
	.uleb128 0
	.byte	0x4
	.set L$set$117,LCFI70-LFB18
	.long L$set$117
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$118,LCFI71-LCFI70
	.long L$set$118
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$119,LCFI72-LCFI71
	.long L$set$119
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$120,LCFI73-LCFI72
	.long L$set$120
	.byte	0xb
	.byte	0x4
	.set L$set$121,LCFI74-LCFI73
	.long L$set$121
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$122,LCFI75-LCFI74
	.long L$set$122
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$123,LEFDE47-LASFDE47
	.long L$set$123
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB19-.
	.set L$set$124,LFE19-LFB19
	.quad L$set$124
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI76-LFB19
	.long L$set$125
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$126,LCFI77-LCFI76
	.long L$set$126
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$127,LCFI78-LCFI77
	.long L$set$127
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$128,LEFDE49-LASFDE49
	.long L$set$128
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB20-.
	.set L$set$129,LFE20-LFB20
	.quad L$set$129
	.uleb128 0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$130,LEFDE51-LASFDE51
	.long L$set$130
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB21-.
	.set L$set$131,LFE21-LFB21
	.quad L$set$131
	.uleb128 0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$132,LEFDE53-LASFDE53
	.long L$set$132
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB22-.
	.set L$set$133,LFE22-LFB22
	.quad L$set$133
	.uleb128 0
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$134,LEFDE55-LASFDE55
	.long L$set$134
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB23-.
	.set L$set$135,LFE23-LFB23
	.quad L$set$135
	.uleb128 0
	.byte	0x4
	.set L$set$136,LCFI79-LFB23
	.long L$set$136
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$137,LCFI80-LCFI79
	.long L$set$137
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$138,LEFDE57-LASFDE57
	.long L$set$138
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB24-.
	.set L$set$139,LFE24-LFB24
	.quad L$set$139
	.uleb128 0
	.byte	0x4
	.set L$set$140,LCFI81-LFB24
	.long L$set$140
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$141,LCFI82-LCFI81
	.long L$set$141
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$142,LCFI83-LCFI82
	.long L$set$142
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$143,LCFI84-LCFI83
	.long L$set$143
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$144,LCFI85-LCFI84
	.long L$set$144
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
	.set L$set$145,LCFI86-LCFI85
	.long L$set$145
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$146,LEFDE59-LASFDE59
	.long L$set$146
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB25-.
	.set L$set$147,LFE25-LFB25
	.quad L$set$147
	.uleb128 0
	.byte	0x4
	.set L$set$148,LCFI87-LFB25
	.long L$set$148
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$149,LCFI88-LCFI87
	.long L$set$149
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$150,LCFI89-LCFI88
	.long L$set$150
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$151,LCFI90-LCFI89
	.long L$set$151
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
	.set L$set$152,LCFI91-LCFI90
	.long L$set$152
	.byte	0xb
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$153,LEFDE61-LASFDE61
	.long L$set$153
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB27-.
	.set L$set$154,LFE27-LFB27
	.quad L$set$154
	.uleb128 0
	.byte	0x4
	.set L$set$155,LCFI92-LFB27
	.long L$set$155
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$156,LCFI93-LCFI92
	.long L$set$156
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$157,LCFI94-LCFI93
	.long L$set$157
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$158,LCFI95-LCFI94
	.long L$set$158
	.byte	0xb
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$159,LEFDE63-LASFDE63
	.long L$set$159
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB28-.
	.set L$set$160,LFE28-LFB28
	.quad L$set$160
	.uleb128 0
	.byte	0x4
	.set L$set$161,LCFI96-LFB28
	.long L$set$161
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$162,LCFI97-LCFI96
	.long L$set$162
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$163,LCFI98-LCFI97
	.long L$set$163
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x96
	.uleb128 0x9
	.byte	0x4
	.set L$set$164,LCFI99-LCFI98
	.long L$set$164
	.byte	0x97
	.uleb128 0x8
	.byte	0x4
	.set L$set$165,LCFI100-LCFI99
	.long L$set$165
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
	.set L$set$166,LCFI101-LCFI100
	.long L$set$166
	.byte	0xb
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$167,LEFDE65-LASFDE65
	.long L$set$167
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB31-.
	.set L$set$168,LFE31-LFB31
	.quad L$set$168
	.uleb128 0
	.byte	0x4
	.set L$set$169,LCFI102-LFB31
	.long L$set$169
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$170,LCFI103-LCFI102
	.long L$set$170
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$171,LCFI104-LCFI103
	.long L$set$171
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x96
	.uleb128 0x9
	.byte	0x97
	.uleb128 0x8
	.byte	0x4
	.set L$set$172,LCFI105-LCFI104
	.long L$set$172
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
	.set L$set$173,LCFI106-LCFI105
	.long L$set$173
	.byte	0xb
	.byte	0x4
	.set L$set$174,LCFI107-LCFI106
	.long L$set$174
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
	.set L$set$175,LCFI108-LCFI107
	.long L$set$175
	.byte	0xb
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$176,LEFDE67-LASFDE67
	.long L$set$176
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB34-.
	.set L$set$177,LFE34-LFB34
	.quad L$set$177
	.uleb128 0
	.byte	0x4
	.set L$set$178,LCFI109-LFB34
	.long L$set$178
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$179,LCFI110-LCFI109
	.long L$set$179
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$180,LCFI111-LCFI110
	.long L$set$180
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$181,LCFI112-LCFI111
	.long L$set$181
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$182,LCFI113-LCFI112
	.long L$set$182
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
	.set L$set$183,LCFI114-LCFI113
	.long L$set$183
	.byte	0xb
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$184,LEFDE69-LASFDE69
	.long L$set$184
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB36-.
	.set L$set$185,LFE36-LFB36
	.quad L$set$185
	.uleb128 0
	.byte	0x4
	.set L$set$186,LCFI115-LFB36
	.long L$set$186
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$187,LCFI116-LCFI115
	.long L$set$187
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$188,LCFI117-LCFI116
	.long L$set$188
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x4
	.set L$set$189,LCFI118-LCFI117
	.long L$set$189
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
	.set L$set$190,LCFI119-LCFI118
	.long L$set$190
	.byte	0xb
	.byte	0x4
	.set L$set$191,LCFI120-LCFI119
	.long L$set$191
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
	.set L$set$192,LCFI121-LCFI120
	.long L$set$192
	.byte	0xb
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$193,LEFDE71-LASFDE71
	.long L$set$193
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB38-.
	.set L$set$194,LFE38-LFB38
	.quad L$set$194
	.uleb128 0
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$195,LEFDE73-LASFDE73
	.long L$set$195
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB39-.
	.set L$set$196,LFE39-LFB39
	.quad L$set$196
	.uleb128 0
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$197,LEFDE75-LASFDE75
	.long L$set$197
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB40-.
	.set L$set$198,LFE40-LFB40
	.quad L$set$198
	.uleb128 0
	.align	3
LEFDE75:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
