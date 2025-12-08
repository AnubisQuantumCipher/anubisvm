	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC2:
	.ascii "anubis_treasury.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_treasury__serialize_treasury_state__write_u64.8:
LFB50:
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
	mov	w1, 926
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L16:
	adrp	x0, lC2@PAGE
	mov	w1, 924
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE50:
	.align	2
	.p2align 5,,15
_anubis_treasury__serialize_proposal__write_u64.10:
LFB54:
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
	adrp	x0, lC2@PAGE
	mov	w1, 1018
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L33:
	adrp	x0, lC2@PAGE
	mov	w1, 1016
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE54:
	.align	2
	.p2align 5,,15
_anubis_treasury__deserialize_treasury_state__read_u64.9:
LFB52:
	stp	x29, x30, [sp, -64]!
LCFI8:
	mov	x29, sp
LCFI9:
	stp	x19, x20, [sp, 16]
LCFI10:
	mov	w20, -1
	mov	x19, x16
	stp	x21, x22, [sp, 32]
LCFI11:
	mov	w21, 2147483647
	str	x23, [sp, 48]
LCFI12:
	mov	x23, 0
	.p2align 5,,15
L40:
	ldr	x3, [x19, 8]
	add	w20, w20, 1
	ldr	w0, [x19, 16]
	ldr	x1, [x3, 8]
	ldr	w2, [x1, 4]
	cmp	w2, w0
	blt	L37
	ldr	w1, [x1]
	cmp	w1, w0
	bgt	L43
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
	beq	L44
	add	w1, w1, 1
	str	w1, [x19, 16]
L37:
	cmp	w20, 7
	bne	L40
	mov	x0, x23
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI13:
	ret
L43:
LCFI14:
	adrp	x0, lC2@PAGE
	mov	w1, 968
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L44:
	adrp	x0, lC2@PAGE
	mov	w1, 969
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE52:
	.align	2
	.p2align 5,,15
_anubis_treasury__deserialize_proposal__read_u64.11:
LFB56:
	stp	x29, x30, [sp, -64]!
LCFI15:
	mov	x29, sp
LCFI16:
	stp	x19, x20, [sp, 16]
LCFI17:
	mov	w20, -1
	mov	x19, x16
	stp	x21, x22, [sp, 32]
LCFI18:
	mov	w21, 2147483647
	str	x23, [sp, 48]
LCFI19:
	mov	x23, 0
	.p2align 5,,15
L49:
	ldr	x3, [x19, 8]
	add	w20, w20, 1
	ldr	w0, [x19, 16]
	ldr	x1, [x3, 8]
	ldr	w2, [x1, 4]
	cmp	w2, w0
	blt	L46
	ldr	w1, [x1]
	cmp	w1, w0
	bgt	L52
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
	beq	L53
	add	w1, w1, 1
	str	w1, [x19, 16]
L46:
	cmp	w20, 7
	bne	L49
	mov	x0, x23
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI20:
	ret
L52:
LCFI21:
	adrp	x0, lC2@PAGE
	mov	w1, 1075
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L53:
	adrp	x0, lC2@PAGE
	mov	w1, 1076
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE56:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__proposal_categoryH
_anubis_treasury__proposal_categoryH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L56
	ldrb	w0, [x0]
	mov	w1, 43691
	movk	w1, 0xaaaa, lsl 16
	lsl	w2, w0, 3
	sub	w3, w2, w0
	add	w2, w2, w0
	umull	x0, w3, w1
	umull	x1, w2, w1
	lsr	x0, x0, 35
	lsr	x1, x1, 35
	add	w0, w0, w0, lsl 1
	add	w1, w1, w1, lsl 1
	sub	w0, w3, w0, lsl 2
	sub	w1, w2, w1, lsl 2
	adrp	x2, _proposal_categoryG.32@PAGE
	add	x2, x2, _proposal_categoryG.32@PAGEOFF;
	sxtw	x0, w0
	sxtw	x1, w1
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L56:
	adrp	x2, _proposal_categoryG.32@PAGE
	mov	x1, 0
	add	x2, x2, _proposal_categoryG.32@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__proposal_typeH
_anubis_treasury__proposal_typeH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L59
	ldrb	w0, [x0]
	mov	w2, 43691
	movk	w2, 0xaaaa, lsl 16
	lsl	w1, w0, 3
	sub	w3, w1, w0
	add	w1, w1, w0
	umull	x0, w3, w2
	umull	x2, w1, w2
	lsr	x0, x0, 35
	lsr	x2, x2, 35
	add	w0, w0, w0, lsl 1
	add	w2, w2, w2, lsl 1
	sub	w0, w3, w0, lsl 2
	sub	w2, w1, w2, lsl 2
	sxtw	x0, w0
	sxtw	x2, w2
L58:
	adrp	x3, _proposal_typeG.28@PAGE
	mov	w1, 52429
	add	x3, x3, _proposal_typeG.28@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, x0]
	ldrb	w2, [x3, x2]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
	.p2align 2,,3
L59:
	mov	x2, 0
	mov	x0, 0
	b	L58
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__proposal_stateH
_anubis_treasury__proposal_stateH:
LFB4:
	ldp	w4, w1, [x1]
	mov	w10, -1
	adrp	x9, _proposal_stateP.27@PAGE
	add	w6, w10, 1
	add	x9, x9, _proposal_stateP.27@PAGEOFF;
	adrp	x12, _proposal_stateT1.26@PAGE
	adrp	x11, _proposal_stateT2.25@PAGE
	mov	w2, 0
	mov	w3, 0
	add	x12, x12, _proposal_stateT1.26@PAGEOFF;
	add	x11, x11, _proposal_stateT2.25@PAGEOFF;
	add	w8, w4, w10
	cmp	w4, w1
	sxtw	x15, w4
	sub	w1, w1, w4
	ldr	w4, [x9, w6, sxtw 2]
	sxtw	x8, w8
	csinc	w5, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w4, sxtw
	mov	w13, 17
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w5, w4
	blt	L62
L66:
	ldrb	w7, [x0, x1]
	ldrb	w4, [x12, w6, sxtw]
	ldrb	w1, [x11, w6, sxtw]
	madd	w4, w4, w7, w3
	madd	w1, w1, w7, w2
	sdiv	w3, w4, w13
	sdiv	w2, w1, w13
	add	w3, w3, w3, lsl 4
	sub	w3, w4, w3
	add	w2, w2, w2, lsl 4
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L62
	add	w6, w10, 1
	mov	w14, 1
	ldr	w4, [x9, w6, sxtw 2]
	mov	w10, 0
	add	x1, x8, w4, sxtw
	sub	x1, x1, x15
	cmp	w5, w4
	bge	L66
L62:
	adrp	x1, _proposal_stateG.24@PAGE
	add	x1, x1, _proposal_stateG.24@PAGEOFF;
	ldrb	w0, [x1, w3, sxtw]
	ldrb	w1, [x1, w2, sxtw]
	add	w0, w0, w1
	and	w0, w0, 7
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__proposal_recordIP
_anubis_treasury__proposal_recordIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__Tproposal_arrayBIP
_anubis_treasury__Tproposal_arrayBIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__vote_recordIP
_anubis_treasury__vote_recordIP:
LFB136:
	ret
LFE136:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__vote_arrayIP
_anubis_treasury__vote_arrayIP:
LFB8:
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__treasury_stateIP
_anubis_treasury__treasury_stateIP:
LFB138:
	ret
LFE138:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__propose_resultH
_anubis_treasury__propose_resultH:
LFB10:
	ldp	w4, w1, [x1]
	mov	w10, -1
	adrp	x9, _propose_resultP.23@PAGE
	add	w6, w10, 1
	add	x9, x9, _propose_resultP.23@PAGEOFF;
	adrp	x12, _propose_resultT1.22@PAGE
	adrp	x11, _propose_resultT2.21@PAGE
	mov	w2, 0
	mov	w3, 0
	add	x12, x12, _propose_resultT1.22@PAGEOFF;
	add	x11, x11, _propose_resultT2.21@PAGEOFF;
	add	w8, w4, w10
	cmp	w4, w1
	sxtw	x15, w4
	sub	w1, w1, w4
	ldr	w4, [x9, w6, sxtw 2]
	sxtw	x8, w8
	csinc	w5, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w4, sxtw
	mov	w13, 18
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w5, w4
	blt	L74
L78:
	ldrb	w7, [x0, x1]
	ldrb	w4, [x12, w6, sxtw]
	ldrb	w1, [x11, w6, sxtw]
	madd	w4, w4, w7, w3
	madd	w1, w1, w7, w2
	sdiv	w3, w4, w13
	sdiv	w2, w1, w13
	add	w3, w3, w3, lsl 3
	sub	w3, w4, w3, lsl 1
	add	w2, w2, w2, lsl 3
	sub	w2, w1, w2, lsl 1
	cmp	w14, 1
	beq	L74
	add	w6, w10, 1
	mov	w14, 1
	ldr	w4, [x9, w6, sxtw 2]
	mov	w10, 0
	add	x1, x8, w4, sxtw
	sub	x1, x1, x15
	cmp	w5, w4
	bge	L78
L74:
	adrp	x1, _propose_resultG.20@PAGE
	add	x1, x1, _propose_resultG.20@PAGEOFF;
	ldrb	w0, [x1, w3, sxtw]
	ldrb	w1, [x1, w2, sxtw]
	add	w0, w0, w1
	and	w0, w0, 7
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__vote_resultH
_anubis_treasury__vote_resultH:
LFB11:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L82
	sub	w1, w1, w2
	cmp	w1, 3
	bgt	L83
L82:
	mov	x3, 0
	mov	x0, 0
L80:
	adrp	x2, _vote_resultG.16@PAGE
	mov	w1, 43691
	add	x2, x2, _vote_resultG.16@PAGEOFF;
	movk	w1, 0xaaaa, lsl 16
	ldrb	w0, [x2, x0]
	ldrb	w2, [x2, x3]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 1
	sub	w0, w0, w1, lsl 1
	ret
	.p2align 2,,3
L83:
	ldrb	w2, [x0, 4]
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
	b	L80
LFE11:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__execute_resultH
_anubis_treasury__execute_resultH:
LFB12:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L87
	sub	w1, w1, w2
	cmp	w1, 6
	bgt	L88
L87:
	mov	x3, 0
	mov	x0, 0
L85:
	adrp	x2, _execute_resultG.12@PAGE
	mov	w1, 43691
	add	x2, x2, _execute_resultG.12@PAGEOFF;
	movk	w1, 0xaaaa, lsl 16
	ldrb	w0, [x2, x0]
	ldrb	w2, [x2, x3]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 1
	sub	w0, w0, w1, lsl 1
	ret
	.p2align 2,,3
L88:
	ldrb	w3, [x0, 7]
	mov	w1, 60495
	movk	w1, 0x4ec4, lsl 16
	add	w0, w3, w3, lsl 1
	lsl	w3, w3, 3
	umull	x2, w3, w1
	umull	x1, w0, w1
	lsr	x2, x2, 34
	lsr	x1, x1, 34
	add	w5, w2, w2, lsl 1
	add	w4, w1, w1, lsl 1
	add	w2, w2, w5, lsl 2
	add	w1, w1, w4, lsl 2
	sub	w3, w3, w2
	sub	w0, w0, w1
	sxtw	x3, w3
	sxtw	x0, w0
	b	L85
LFE12:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__init_treasury
_anubis_treasury__init_treasury:
LFB13:
	adrp	x2, lC3@PAGE
	stp	x29, x30, [sp, -32]!
LCFI22:
	mov	x29, sp
LCFI23:
	movi	v31.4s, 0
	ldr	q30, [x2, #lC3@PAGEOFF]
	mov	x2, 2880
	stp	x19, x20, [sp, 16]
LCFI24:
	mov	x19, x0
	mov	x20, x1
	add	x0, x0, 32
	mov	w1, 0
	stp	q30, q31, [x19]
	bl	_memset
	movi	v31.4s, 0
	add	x0, x19, 3072
	str	wzr, [x19, 2912]
	str	q31, [x0, -152]
	str	q31, [x0, -136]
	str	q31, [x0, -120]
	str	x20, [x19, 2968]
	str	x20, [x19, 2976]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI25:
	ret
LFE13:
	.const
	.align	3
lC4:
	.ascii "failed precondition from anubis_treasury.ads:266"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__create_spending_proposal
_anubis_treasury__create_spending_proposal:
LFB15:
	stp	x29, x30, [sp, -176]!
LCFI26:
	mov	x29, sp
LCFI27:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI28:
	mov	x21, x1
	stp	x23, x24, [sp, 48]
LCFI29:
	mov	x24, x0
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI30:
	ldp	w22, w2, [x2]
	str	x3, [x29, 112]
	ldr	x14, [x29, 200]
	str	w2, [x29, 132]
	sxtw	x15, w22
	sxtw	x11, w2
	ldp	w26, w4, [x4]
	add	x1, x15, 31
	str	x6, [x29, 104]
	cmp	x1, x11
	sxtw	x13, w4
	str	w4, [x29, 120]
	sxtw	x3, w26
	ldp	w27, w4, [x7]
	add	x0, x3, 31
	ldp	x7, x12, [x29, 176]
	ccmp	x0, x13, 0, eq
	ccmp	x5, 0, 4, eq
	cset	w0, eq
	str	w4, [x29, 124]
	sxtw	x19, w27
	sxtw	x28, w4
	ldp	w23, w6, [x12]
	add	x2, x19, 31
	cmp	x2, x28
	str	x7, [x29, 96]
	sxtw	x20, w23
	sxtw	x25, w6
	str	w6, [x29, 128]
	add	x1, x20, 31
	ccmp	x1, x25, 0, eq
	cset	w1, ne
	orr	w0, w0, w1
	cbnz	w0, L154
	mov	x0, x14
	mov	x2, 288
	stp	x11, x15, [x29, 136]
	mov	w1, 0
	stp	x13, x3, [x29, 152]
	str	x5, [x29, 168]
	bl	_memset
	mov	x14, x0
	ldr	w0, [x24, 2912]
	ldp	x11, x15, [x29, 136]
	ldp	x13, x3, [x29, 152]
	ldr	x5, [x29, 168]
	tbnz	w0, #31, L155
	cmp	w0, 9
	bgt	L137
	ldr	x1, [x24, 8]
	cmp	x1, x5
	bcc	L138
	mov	x0, 10000
	mul	x0, x5, x0
	udiv	x0, x0, x1
	cmp	x0, 50
	bls	L139
	cmp	x0, 100
	bls	L140
	mov	w1, 2
	cmp	x0, 200
	bls	L95
	ldr	x2, [x29, 192]
	mov	x0, 34464
	mov	w1, 3
	movk	x0, 0x1, lsl 16
	cmp	x2, x0
	bcc	L142
L156:
	mov	x0, 0
	.p2align 5,,15
L98:
	add	x2, x0, x0, lsl 3
	add	x2, x24, x2, lsl 5
	ldr	x2, [x2, 32]
	cbz	x2, L97
	add	x0, x0, 1
	cmp	x0, 10
	bne	L98
L137:
	mov	w0, 3
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 176
LCFI31:
	ret
	.p2align 2,,3
L138:
LCFI32:
	mov	w0, 2
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 176
LCFI33:
	ret
	.p2align 2,,3
L140:
LCFI34:
	mov	w1, 1
L95:
	adrp	x0, _CSWTCH.352@PAGE
	uxtw	x2, w1
	add	x0, x0, _CSWTCH.352@PAGEOFF;
	ldr	x0, [x0, x2, lsl 3]
	ldr	x2, [x29, 192]
	cmp	x2, x0
	bcs	L156
L142:
	mov	w0, 1
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 176
LCFI35:
	ret
	.p2align 2,,3
L139:
LCFI36:
	mov	w1, 0
	b	L95
	.p2align 2,,3
L97:
	adrp	x16, _anubis_treasury__proposal_counter@PAGE
	mov	w17, 1
	strb	w1, [x14, 8]
	ldr	x2, [x16, #_anubis_treasury__proposal_counter@PAGEOFF]
	strb	w17, [x14, 10]
	add	x2, x2, 1
	str	x2, [x16, #_anubis_treasury__proposal_counter@PAGEOFF]
	str	x2, [x14]
	ldr	w2, [x29, 132]
	cmp	w22, w2
	bgt	L107
	sub	x16, x14, x15
	sub	x21, x21, x15
	add	x16, x16, 11
	sub	x2, x15, #1
	.p2align 5,,15
L106:
	add	x2, x2, 1
	subs	w15, w2, w22
	bvs	L102
	cmp	w15, 31
	bgt	L104
	bhi	L157
	ldrb	w15, [x21, x2]
	strb	w15, [x16, x2]
L104:
	cmp	x11, x2
	bne	L106
L107:
	ldr	x2, [x29, 192]
	str	x2, [x14, 48]
	ldr	w2, [x29, 124]
	cmp	w27, w2
	bgt	L100
	ldr	x2, [x29, 104]
	sub	x8, x14, x19
	add	x8, x8, 57
	sub	x22, x2, x19
	sub	x19, x19, #1
	.p2align 5,,15
L115:
	add	x19, x19, 1
	subs	w2, w19, w27
	bvs	L111
	cmp	w2, 31
	bgt	L113
	bhi	L158
	ldrb	w2, [x22, x19]
	strb	w2, [x8, x19]
L113:
	cmp	x28, x19
	bne	L115
L100:
	ldr	w2, [x29, 128]
	cmp	w23, w2
	bgt	L109
	ldr	x2, [x29, 96]
	sub	x4, x14, x20
	add	x4, x4, 89
	sub	x27, x2, x20
	sub	x20, x20, #1
	.p2align 5,,15
L123:
	add	x20, x20, 1
	subs	w2, w20, w23
	bvs	L119
	cmp	w2, 31
	bgt	L121
	bhi	L159
	ldrb	w2, [x27, x20]
	strb	w2, [x4, x20]
L121:
	cmp	x25, x20
	bne	L123
L109:
	ldr	w2, [x29, 120]
	cmp	w26, w2
	bgt	L117
	ldr	x2, [x29, 112]
	sub	x4, x14, x3
	add	x4, x4, 167
	sub	x23, x2, x3
	sub	x3, x3, #1
	.p2align 5,,15
L133:
	add	x3, x3, 1
	subs	w2, w3, w26
	bvs	L129
	cmp	w2, 31
	bgt	L131
	bhi	L160
	ldrb	w2, [x23, x3]
	strb	w2, [x4, x3]
L131:
	cmp	x13, x3
	bne	L133
L117:
	str	x5, [x14, 200]
	ldr	x2, [x24, 2976]
	stp	x2, x2, [x14, 248]
	ldr	d31, [x24, 2976]
	dup	v31.2d, v31.d[0]
	cmp	w1, 1
	beq	L161
	cmp	w1, 2
	beq	L162
	cbnz	w1, L127
	adrp	x1, lC7@PAGE
	ldr	q1, [x1, #lC7@PAGEOFF]
	add	v30.2d, v31.2d, v1.2d
L125:
	sbfiz	x1, x0, 3, 32
	add	x3, x14, 512
	add	x0, x1, w0, sxtw
	add	x0, x24, x0, lsl 5
	mov	x1, x14
	mov	x2, 288
	str	q30, [x3, -248]
	add	x0, x0, 32
	bl	_memcpy
	ldr	w0, [x24, 2912]
	tbnz	w0, #31, L163
	mov	w1, 2147483647
	cmp	w0, w1
	beq	L164
	ldr	x2, [x24, 2920]
	add	w1, w0, 1
	mov	w0, 0
	str	w1, [x24, 2912]
	add	x1, x2, 1
	str	x1, [x24, 2920]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 176
LCFI37:
	ret
	.p2align 2,,3
L127:
LCFI38:
	adrp	x1, lC8@PAGE
	ldr	q30, [x1, #lC8@PAGEOFF]
	add	v30.2d, v31.2d, v30.2d
	b	L125
	.p2align 2,,3
L162:
	adrp	x1, lC6@PAGE
	ldr	q29, [x1, #lC6@PAGEOFF]
	add	v30.2d, v31.2d, v29.2d
	b	L125
	.p2align 2,,3
L161:
	adrp	x1, lC5@PAGE
	ldr	q0, [x1, #lC5@PAGEOFF]
	add	v30.2d, v31.2d, v0.2d
	b	L125
L129:
	adrp	x0, lC2@PAGE
	mov	w1, 185
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L119:
	adrp	x0, lC2@PAGE
	mov	w1, 178
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L111:
	adrp	x0, lC2@PAGE
	mov	w1, 171
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L102:
	adrp	x0, lC2@PAGE
	mov	w1, 161
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L158:
	adrp	x0, lC2@PAGE
	mov	w1, 172
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L157:
	adrp	x0, lC2@PAGE
	mov	w1, 162
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L155:
	adrp	x0, lC2@PAGE
	mov	w1, 116
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L154:
	adrp	x0, lC4@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L160:
	adrp	x0, lC2@PAGE
	mov	w1, 186
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L159:
	adrp	x0, lC2@PAGE
	mov	w1, 179
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L164:
	adrp	x0, lC2@PAGE
	mov	w1, 219
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L163:
	adrp	x0, lC2@PAGE
	mov	w1, 219
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE15:
	.const
	.align	2
lC1:
	.word	1
	.word	48
	.text
	.const
	.align	3
lC9:
	.ascii "failed precondition from anubis_treasury.ads:283"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__create_parameter_proposal
_anubis_treasury__create_parameter_proposal:
LFB16:
	stp	x29, x30, [sp, -144]!
LCFI39:
	mov	x29, sp
LCFI40:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI41:
	mov	x26, x0
	stp	x27, x28, [sp, 80]
LCFI42:
	ldp	w23, w22, [x2]
	stp	x5, x3, [x29, 96]
	str	x1, [x29, 112]
	str	x7, [x29, 128]
	sxtw	x19, w23
	sxtw	x11, w22
	ldp	w24, w2, [x4]
	add	x0, x19, 31
	cmp	x0, x11
	ldp	w25, w3, [x6]
	ldr	x9, [x29, 144]
	sxtw	x20, w24
	sxtw	x28, w2
	add	x1, x20, 31
	sxtw	x21, w25
	ccmp	x1, x28, 0, eq
	stp	w2, w3, [x29, 120]
	add	x0, x21, 31
	sxtw	x27, w3
	ccmp	x0, x27, 0, eq
	bne	L206
	mov	x0, x9
	mov	x2, 288
	str	x11, [x29, 136]
	mov	w1, 0
	bl	_memset
	mov	x9, x0
	mov	w0, 257
	ldr	x11, [x29, 136]
	strh	w0, [x9, 8]
	ldr	w0, [x26, 2912]
	tbnz	w0, #31, L207
	cmp	w0, 9
	bgt	L195
	ldr	x1, [x29, 128]
	mov	x0, 14999
	cmp	x1, x0
	bls	L196
	mov	x3, 0
	.p2align 5,,15
L170:
	add	x1, x3, x3, lsl 3
	add	x1, x26, x1, lsl 5
	ldr	x0, [x1, 32]
	cbz	x0, L169
	add	x3, x3, 1
	cmp	x3, 10
	bne	L170
L195:
	mov	w0, 3
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 144
LCFI43:
	ret
	.p2align 2,,3
L196:
LCFI44:
	mov	w0, 1
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 144
LCFI45:
	ret
	.p2align 2,,3
L169:
LCFI46:
	adrp	x1, _anubis_treasury__proposal_counter@PAGE
	mov	w2, 1
	ldr	x0, [x1, #_anubis_treasury__proposal_counter@PAGEOFF]
	strb	w2, [x9, 10]
	add	x0, x0, 1
	str	x0, [x1, #_anubis_treasury__proposal_counter@PAGEOFF]
	str	x0, [x9]
	cmp	w23, w22
	bgt	L194
	ldr	x0, [x29, 112]
	sub	x1, x9, x19
	sub	x2, x19, #1
	add	x1, x1, 11
	sub	x22, x0, x19
	.p2align 5,,15
L176:
	add	x2, x2, 1
	subs	w0, w2, w23
	bvs	L172
	cmp	w0, 31
	bgt	L174
	bhi	L208
	ldrb	w0, [x22, x2]
	strb	w0, [x1, x2]
L174:
	cmp	x11, x2
	bne	L176
L194:
	ldr	x0, [x29, 128]
	str	x0, [x9, 48]
	ldr	w0, [x29, 120]
	cmp	w24, w0
	bgt	L177
	ldr	x0, [x29, 104]
	sub	x2, x9, x20
	sub	x1, x20, #1
	add	x2, x2, 57
	sub	x23, x0, x20
	.p2align 5,,15
L183:
	add	x1, x1, 1
	subs	w0, w1, w24
	bvs	L179
	cmp	w0, 31
	bgt	L181
	bhi	L209
	ldrb	w0, [x23, x1]
	strb	w0, [x2, x1]
L181:
	cmp	x28, x1
	bne	L183
L177:
	ldr	w0, [x29, 124]
	cmp	w25, w0
	bgt	L184
	ldr	x0, [x29, 96]
	sub	x2, x9, x21
	sub	x1, x21, #1
	add	x2, x2, 89
	sub	x24, x0, x21
	.p2align 5,,15
L190:
	add	x1, x1, 1
	subs	w0, w1, w25
	bvs	L186
	cmp	w0, 31
	bgt	L188
	bhi	L210
	ldrb	w0, [x24, x1]
	strb	w0, [x2, x1]
L188:
	cmp	x27, x1
	bne	L190
L184:
	ldr	x4, [x26, 2976]
	adrp	x2, lC5@PAGE
	add	x5, x9, 512
	sbfiz	x0, x3, 3, 32
	add	x0, x0, w3, sxtw
	add	x0, x26, x0, lsl 5
	ldr	q30, [x2, #lC5@PAGEOFF]
	mov	x1, x9
	mov	x2, 288
	add	x0, x0, 32
	stp	x4, x4, [x9, 248]
	ldr	d31, [x26, 2976]
	dup	v31.2d, v31.d[0]
	add	v30.2d, v31.2d, v30.2d
	str	q30, [x5, -248]
	bl	_memcpy
	ldr	w0, [x26, 2912]
	tbnz	w0, #31, L211
	mov	w1, 2147483647
	cmp	w0, w1
	beq	L212
	ldr	x2, [x26, 2920]
	add	w1, w0, 1
	mov	w0, 0
	str	w1, [x26, 2912]
	add	x1, x2, 1
	str	x1, [x26, 2920]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 144
LCFI47:
	ret
L172:
LCFI48:
	adrp	x0, lC2@PAGE
	mov	w1, 300
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L186:
	adrp	x0, lC2@PAGE
	mov	w1, 317
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L179:
	adrp	x0, lC2@PAGE
	mov	w1, 310
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L208:
	adrp	x0, lC2@PAGE
	mov	w1, 301
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L209:
	adrp	x0, lC2@PAGE
	mov	w1, 311
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L210:
	adrp	x0, lC2@PAGE
	mov	w1, 318
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L206:
	adrp	x0, lC9@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L207:
	adrp	x0, lC2@PAGE
	mov	w1, 265
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L211:
	adrp	x0, lC2@PAGE
	mov	w1, 335
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L212:
	adrp	x0, lC2@PAGE
	mov	w1, 335
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__categorize_proposal
_anubis_treasury__categorize_proposal:
LFB17:
	mov	x2, x0
	mov	w0, 3
	cbz	x1, L214
	mov	x3, 10000
	mov	w0, 0
	mul	x2, x2, x3
	udiv	x1, x2, x1
	cmp	x1, 50
	bls	L214
	mov	w0, 1
	cmp	x1, 100
	bls	L214
	cmp	x1, 200
	cset	w0, hi
	add	w0, w0, 2
L214:
	ret
LFE17:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__get_required_bond
_anubis_treasury__get_required_bond:
LFB18:
	cmp	w0, 3
	bhi	L226
	beq	L222
	adrp	x1, _CSWTCH.352@PAGE
	uxtw	x0, w0
	add	x1, x1, _CSWTCH.352@PAGEOFF;
	ldr	x0, [x1, x0, lsl 3]
	ret
	.p2align 2,,3
L222:
	mov	x0, 34464
	movk	x0, 0x1, lsl 16
	ret
L226:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI49:
	mov	w1, 369
	mov	x29, sp
LCFI50:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE18:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__get_required_quorum
_anubis_treasury__get_required_quorum:
LFB19:
	cmp	w0, 3
	bhi	L234
	mov	x2, 2000
	bne	L235
	mov	x0, 22859
	mul	x1, x1, x2
	movk	x0, 0x3886, lsl 16
	movk	x0, 0xc5d6, lsl 32
	movk	x0, 0x346d, lsl 48
	umulh	x0, x1, x0
	lsr	x0, x0, 11
	ret
	.p2align 2,,3
L235:
	adrp	x2, _CSWTCH.359@PAGE
	uxtw	x0, w0
	add	x2, x2, _CSWTCH.359@PAGEOFF;
	ldrsw	x2, [x2, x0, lsl 2]
	mov	x0, 22859
	movk	x0, 0x3886, lsl 16
	movk	x0, 0xc5d6, lsl 32
	movk	x0, 0x346d, lsl 48
	mul	x1, x1, x2
	umulh	x0, x1, x0
	lsr	x0, x0, 11
	ret
L234:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI51:
	mov	w1, 383
	mov	x29, sp
LCFI52:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__get_required_threshold
_anubis_treasury__get_required_threshold:
LFB20:
	cmp	w0, 3
	bhi	L243
	mov	w3, 500
	mov	w2, 5500
	mov	w1, 7500
	madd	w0, w0, w3, w2
	csel	w0, w0, w1, ne
	ret
L243:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI53:
	mov	w1, 397
	mov	x29, sp
LCFI54:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_treasury.ads:329"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__cast_vote
_anubis_treasury__cast_vote:
LFB22:
	stp	x29, x30, [sp, -16]!
LCFI55:
	mov	x29, sp
LCFI56:
	mov	x6, x0
	mov	x2, 0
	ldpsw	x8, x9, [x3]
	ldpsw	x0, x3, [x7]
	add	x8, x8, 31
	cmp	x8, x9
	add	x0, x0, 62
	ccmp	x0, x3, 0, eq
	ccmp	x5, 0, 4, lt
	beq	L266
	.p2align 5,,15
L245:
	add	x3, x2, x2, lsl 3
	add	x3, x6, x3, lsl 5
	ldr	x3, [x3, 32]
	cmp	x3, x1
	beq	L246
	add	x2, x2, 1
	cmp	x2, 10
	bne	L245
	mov	w0, 2
L258:
	ldp	x29, x30, [sp], 16
LCFI57:
	ret
	.p2align 2,,3
L246:
LCFI58:
	sbfiz	x3, x2, 3, 32
	add	x1, x3, w2, sxtw
	add	x1, x6, x1, lsl 5
	sxtw	x2, w2
	ldrb	w7, [x1, 42]
	cmp	w7, 7
	bhi	L267
	mov	w0, 2
	cmp	w7, 1
	bne	L258
	add	x0, x2, x2, lsl 3
	ldr	x7, [x6, 2976]
	add	x0, x6, x0, lsl 5
	ldr	x0, [x0, 296]
	cmp	x7, x0
	bhi	L257
	cmp	w4, 2
	bhi	L268
	cbz	w4, L249
	cmp	w4, 1
	beq	L250
	ldr	x0, [x1, 264]
	add	x0, x5, x0
	str	x0, [x1, 264]
L252:
	add	x1, x3, x2
	ldr	x2, [x6, 2944]
	mov	w0, 0
	add	x1, x6, x1, lsl 5
	ldr	w3, [x1, 272]
	add	x2, x2, x5
	add	w3, w3, 1
	str	w3, [x1, 272]
	str	x2, [x6, 2944]
	ldp	x29, x30, [sp], 16
LCFI59:
	ret
	.p2align 2,,3
L249:
LCFI60:
	ldr	x0, [x1, 248]
	add	x0, x5, x0
	str	x0, [x1, 248]
	b	L252
	.p2align 2,,3
L257:
	mov	w0, 5
	ldp	x29, x30, [sp], 16
LCFI61:
	ret
	.p2align 2,,3
L250:
LCFI62:
	ldr	x0, [x1, 256]
	add	x0, x5, x0
	str	x0, [x1, 256]
	b	L252
L266:
	adrp	x0, lC10@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L267:
	adrp	x0, lC2@PAGE
	mov	w1, 437
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L268:
	adrp	x0, lC2@PAGE
	mov	w1, 449
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.const
	.align	3
lC11:
	.ascii "failed precondition from anubis_treasury.ads:342"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__delegate_vote
_anubis_treasury__delegate_vote:
LFB23:
	ldpsw	x0, x2, [x2]
	ldpsw	x1, x3, [x4]
	add	x0, x0, 31
	cmp	x0, x2
	ldpsw	x0, x2, [x6]
	add	x1, x1, 31
	ccmp	x1, x3, 0, eq
	add	x0, x0, 62
	ccmp	x0, x2, 0, eq
	bge	L274
	mov	w0, 1
	ret
L274:
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI63:
	add	x0, x0, lC11@PAGEOFF;
	mov	x29, sp
LCFI64:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE23:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__quorum_reached
_anubis_treasury__quorum_reached:
LFB24:
	ldp	x3, x5, [x0, 216]
	ldr	x4, [x0, 232]
	add	x0, x3, x5
	add	x0, x0, x4
	cmp	w1, 3
	bhi	L282
	mov	x3, 2000
	bne	L283
L277:
	mov	x1, 22859
	mul	x2, x2, x3
	movk	x1, 0x3886, lsl 16
	movk	x1, 0xc5d6, lsl 32
	movk	x1, 0x346d, lsl 48
	umulh	x2, x2, x1
	cmp	x0, x2, lsr 11
	cset	w0, cs
	ret
	.p2align 2,,3
L283:
	adrp	x3, _CSWTCH.359@PAGE
	uxtw	x1, w1
	add	x3, x3, _CSWTCH.359@PAGEOFF;
	ldrsw	x3, [x3, x1, lsl 2]
	b	L277
L282:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI65:
	mov	w1, 490
	mov	x29, sp
LCFI66:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__threshold_reached
_anubis_treasury__threshold_reached:
LFB25:
	ldp	x2, x0, [x0, 216]
	adds	x0, x2, x0
	beq	L288
	cmp	w1, 3
	bhi	L293
	mov	w6, 500
	mov	w5, 5500
	mov	x3, 22859
	mov	x4, 7500
	movk	x3, 0x3886, lsl 16
	madd	w1, w1, w6, w5
	csel	x1, x1, x4, ne
	movk	x3, 0xc5d6, lsl 32
	mul	x0, x0, x1
	movk	x3, 0x346d, lsl 48
	umulh	x0, x0, x3
	cmp	x2, x0, lsr 11
	cset	w0, cs
	ret
	.p2align 2,,3
L288:
	mov	w0, 0
	ret
L293:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI67:
	mov	w1, 507
	mov	x29, sp
LCFI68:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.const
	.align	3
lC12:
	.ascii "failed precondition from anubis_treasury.ads:367"
	.align	3
lC13:
	.ascii "failed postcondition from anubis_treasury.ads:368"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__vote_percentage
_anubis_treasury__vote_percentage:
LFB26:
	stp	x29, x30, [sp, -16]!
LCFI69:
	mov	x29, sp
LCFI70:
	cbz	x1, L299
	mov	x2, 10000
	mov	x3, 2147483647
	mul	x0, x0, x2
	udiv	x0, x0, x1
	cmp	x0, x3
	bhi	L300
	cmp	w0, w2
	bgt	L301
	ldp	x29, x30, [sp], 16
LCFI71:
	ret
L299:
LCFI72:
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L301:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L300:
	adrp	x0, lC2@PAGE
	mov	w1, 517
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE26:
	.const
	.align	2
lC0:
	.word	1
	.word	49
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__finalize_voting
_anubis_treasury__finalize_voting:
LFB28:
	stp	x29, x30, [sp, -48]!
LCFI73:
	mov	x29, sp
LCFI74:
	mov	x3, 0
	stp	x19, x20, [sp, 16]
LCFI75:
	mov	x20, x0
	str	x21, [sp, 32]
LCFI76:
	str	x2, [x0, 2976]
	.p2align 5,,15
L304:
	add	x4, x3, x3, lsl 3
	add	x4, x20, x4, lsl 5
	ldr	x4, [x4, 32]
	cmp	x4, x1
	beq	L303
	add	x3, x3, 1
	cmp	x3, 10
	bne	L304
L305:
	mov	w0, 0
L314:
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI77:
	ret
	.p2align 2,,3
L303:
LCFI78:
	sxtw	x19, w3
	add	x0, x19, x19, lsl 3
	add	x0, x20, x0, lsl 5
	ldr	x0, [x0, 296]
	cmp	x2, x0
	bcc	L305
	add	x0, x19, x19, lsl 3
	lsl	x21, x19, 3
	add	x0, x20, x0, lsl 5
	ldrb	w1, [x0, 40]
	cmp	w1, 3
	bhi	L317
	ldp	x2, x6, [x0, 248]
	mov	x4, 2000
	ldr	x5, [x0, 264]
	add	x0, x2, x6
	ldr	x3, [x20, 2960]
	add	x0, x0, x5
	bne	L318
	mul	x2, x3, x4
	mov	x3, 22859
	movk	x3, 0x3886, lsl 16
	movk	x3, 0xc5d6, lsl 32
	movk	x3, 0x346d, lsl 48
	umulh	x2, x2, x3
	cmp	x0, x2, lsr 11
	bcc	L319
L308:
	mov	w0, 288
	umaddl	x0, w19, w0, x20
	add	x0, x0, 32
	bl	_anubis_treasury__threshold_reached
	tbz	x0, 0, L310
	ldr	x1, [x20, 2928]
	add	x2, x21, x19
	mov	w3, 2
	add	x2, x20, x2, lsl 5
	strb	w3, [x2, 42]
	add	x1, x1, 1
	str	x1, [x20, 2928]
	b	L314
	.p2align 2,,3
L319:
	ldr	x0, [x20, 2936]
	add	x1, x21, x19
	mov	w2, 6
	add	x1, x20, x1, lsl 5
	strb	w2, [x1, 42]
	add	x0, x0, 1
	str	x0, [x20, 2936]
	b	L305
	.p2align 2,,3
L318:
	adrp	x2, _CSWTCH.359@PAGE
	uxtw	x4, w1
	add	x2, x2, _CSWTCH.359@PAGEOFF;
	ldrsw	x4, [x2, x4, lsl 2]
	mul	x2, x3, x4
	mov	x3, 22859
	movk	x3, 0x3886, lsl 16
	movk	x3, 0xc5d6, lsl 32
	movk	x3, 0x346d, lsl 48
	umulh	x2, x2, x3
	cmp	x0, x2, lsr 11
	bcs	L308
	b	L319
L310:
	ldr	x1, [x20, 2936]
	add	x0, x21, x19
	mov	w2, 3
	add	x0, x20, x0, lsl 5
	strb	w2, [x0, 42]
	add	x0, x1, 1
	str	x0, [x20, 2936]
	b	L305
L317:
	adrp	x0, lC2@PAGE
	mov	w1, 557
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE28:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__execute_proposal
_anubis_treasury__execute_proposal:
LFB29:
	mov	x3, 0
	mov	x5, x0
	str	x2, [x0, 2976]
	.p2align 5,,15
L322:
	add	x4, x3, x3, lsl 3
	add	x4, x5, x4, lsl 5
	ldr	x4, [x4, 32]
	cmp	x4, x1
	beq	L321
	add	x3, x3, 1
	cmp	x3, 10
	bne	L322
	mov	w0, 1
	ret
	.p2align 2,,3
L321:
	sbfiz	x4, x3, 3, 32
	add	x1, x4, w3, sxtw
	add	x1, x5, x1, lsl 5
	stp	x29, x30, [sp, -16]!
LCFI79:
	mov	x29, sp
LCFI80:
	sxtw	x3, w3
	ldrb	w0, [x1, 42]
	cmp	w0, 7
	bhi	L339
	cmp	w0, 2
	beq	L340
	mov	w0, 1
L333:
	ldp	x29, x30, [sp], 16
LCFI81:
	ret
	.p2align 2,,3
L340:
LCFI82:
	ldr	x6, [x1, 304]
	cmp	x2, x6
	bcc	L333
	ldr	x0, [x1, 312]
	cbnz	x0, L331
	ldrb	w0, [x1, 41]
	cmp	w0, 4
	bhi	L341
	cbnz	w0, L325
	ldr	x0, [x5, 8]
	ldr	x1, [x1, 232]
	cmp	x0, x1
	bcc	L332
	ldr	x6, [x5, 16]
	sub	x0, x0, x1
	add	x1, x6, x1
	stp	x0, x1, [x5, 8]
L325:
	add	x0, x4, x3
	mov	w3, 4
	add	x0, x5, x0, lsl 5
	ldr	w1, [x5, 2912]
	strb	w3, [x0, 42]
	str	x2, [x0, 312]
	cmp	w1, 0
	blt	L342
	beq	L327
	sub	w1, w1, #1
	str	w1, [x5, 2912]
L327:
	mov	w0, 0
	b	L333
L331:
	mov	w0, 3
	b	L333
L332:
	mov	w0, 4
	b	L333
L339:
	adrp	x0, lC2@PAGE
	mov	w1, 604
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L342:
	adrp	x0, lC2@PAGE
	mov	w1, 638
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L341:
	adrp	x0, lC2@PAGE
	mov	w1, 622
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE29:
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_treasury.ads:401"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__cancel_proposal
_anubis_treasury__cancel_proposal:
LFB30:
	stp	x29, x30, [sp, -16]!
LCFI83:
	mov	x29, sp
LCFI84:
	mov	x4, x0
	ldpsw	x6, x7, [x3]
	ldpsw	x0, x3, [x5]
	add	x6, x6, 31
	cmp	x6, x7
	add	x0, x0, 62
	ccmp	x0, x3, 0, eq
	bge	L359
	mov	x2, 0
	cset	w0, ge
	.p2align 5,,15
L344:
	add	x3, x2, x2, lsl 3
	add	x3, x4, x3, lsl 5
	ldr	x3, [x3, 32]
	cmp	x3, x1
	beq	L345
	add	x2, x2, 1
	cmp	x2, 10
	bne	L344
L352:
	ldp	x29, x30, [sp], 16
LCFI85:
	ret
	.p2align 2,,3
L345:
LCFI86:
	sbfiz	x1, x2, 3, 32
	add	x2, x1, w2, sxtw
	add	x2, x4, x2, lsl 5
	ldrb	w1, [x2, 42]
	cmp	w1, 7
	bhi	L360
	cmp	w1, 1
	bhi	L352
	ldr	w0, [x4, 2912]
	mov	w1, 5
	strb	w1, [x2, 42]
	cmp	w0, 0
	blt	L361
	beq	L348
	sub	w0, w0, #1
	str	w0, [x4, 2912]
L348:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI87:
	ret
L359:
LCFI88:
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L360:
	adrp	x0, lC2@PAGE
	mov	w1, 671
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L361:
	adrp	x0, lC2@PAGE
	mov	w1, 680
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE30:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__return_bond
_anubis_treasury__return_bond:
LFB31:
	mov	w0, 1
	ret
LFE31:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__burn_bond
_anubis_treasury__burn_bond:
LFB32:
	mov	x2, 0
	mov	x4, x0
	.p2align 5,,15
L365:
	add	x3, x2, x2, lsl 3
	add	x3, x4, x3, lsl 5
	ldr	x3, [x3, 32]
	cmp	x3, x1
	beq	L364
	add	x2, x2, 1
	cmp	x2, 10
	bne	L365
	mov	w0, 0
	ret
	.p2align 2,,3
L364:
	sbfiz	x0, x2, 3, 32
	add	x2, x0, w2, sxtw
	add	x2, x4, x2, lsl 5
	ldr	x1, [x4, 24]
	mov	w0, 1
	ldr	x2, [x2, 80]
	add	x1, x1, x2
	str	x1, [x4, 24]
	ret
LFE32:
	.const
	.align	3
lC15:
	.ascii "failed precondition from anubis_treasury.ads:433"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__emergency_veto
_anubis_treasury__emergency_veto:
LFB33:
	stp	x29, x30, [sp, -16]!
LCFI89:
	mov	x29, sp
LCFI90:
	mov	x2, 0
	ldpsw	x3, x4, [x5]
	add	x3, x3, 31
	cmp	x3, x4
	bne	L384
	.p2align 5,,15
L370:
	add	x3, x2, x2, lsl 3
	add	x3, x0, x3, lsl 5
	ldr	x3, [x3, 32]
	cmp	x3, x1
	beq	L371
	add	x2, x2, 1
	cmp	x2, 10
	bne	L370
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI91:
	ret
	.p2align 2,,3
L371:
LCFI92:
	ldr	w1, [x0, 2912]
	sbfiz	x3, x2, 3, 32
	add	x2, x3, w2, sxtw
	add	x2, x0, x2, lsl 5
	mov	w3, 7
	strb	w3, [x2, 42]
	cmp	w1, 0
	blt	L385
	beq	L374
	sub	w1, w1, #1
	str	w1, [x0, 2912]
L374:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI93:
	ret
L384:
LCFI94:
	adrp	x0, lC15@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L385:
	adrp	x0, lC2@PAGE
	mov	w1, 757
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE33:
	.const
	.align	3
lC16:
	.ascii "failed precondition from anubis_treasury.ads:445"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__emergency_release
_anubis_treasury__emergency_release:
LFB34:
	ldpsw	x4, x5, [x2]
	mov	x1, x0
	ldpsw	x0, x2, [x7]
	add	x4, x4, 31
	cmp	x4, x5
	add	x0, x0, 31
	ccmp	x0, x2, 0, eq
	bne	L392
	ldr	x2, [x1, 8]
	cset	w0, ne
	cmp	x2, x3
	bcs	L393
	ret
	.p2align 2,,3
L393:
	ldr	x4, [x1, 16]
	sub	x2, x2, x3
	mov	w0, 1
	add	x3, x4, x3
	stp	x2, x3, [x1, 8]
	ret
L392:
	adrp	x0, lC16@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI95:
	add	x0, x0, lC16@PAGEOFF;
	mov	x29, sp
LCFI96:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE34:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__get_balance
_anubis_treasury__get_balance:
LFB35:
	ldr	x0, [x0, 8]
	ret
LFE35:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__get_total_spent
_anubis_treasury__get_total_spent:
LFB37:
	ldr	x0, [x0, 16]
	ret
LFE37:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__get_active_count
_anubis_treasury__get_active_count:
LFB39:
	ldr	w0, [x0, 2912]
	tbnz	w0, #31, L401
	ret
L401:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI97:
	mov	w1, 800
	mov	x29, sp
LCFI98:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE39:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__get_proposal
_anubis_treasury__get_proposal:
LFB41:
	stp	x29, x30, [sp, -32]!
LCFI99:
	mov	x29, sp
LCFI100:
	stp	x19, x20, [sp, 16]
LCFI101:
	mov	x20, x0
	mov	x19, x1
	mov	x0, x2
	mov	w1, 0
	mov	x2, 288
	bl	_memset
	mov	x6, x0
	mov	x3, 0
	.p2align 5,,15
L405:
	add	x4, x3, x3, lsl 3
	add	x3, x3, 1
	add	x4, x20, x4, lsl 5
	ldr	x5, [x4, 32]
	cmp	x5, x19
	beq	L408
	cmp	x3, 10
	bne	L405
	mov	w0, 0
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI102:
	ret
	.p2align 2,,3
L408:
LCFI103:
	add	x1, x4, 32
	mov	x0, x6
	mov	x2, 288
	bl	_memcpy
	mov	w0, 1
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI104:
	ret
LFE41:
	.const
	.align	3
lC17:
	.ascii "failed precondition from anubis_treasury.ads:482"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__is_proposer
_anubis_treasury__is_proposer:
LFB42:
	stp	x29, x30, [sp, -16]!
LCFI105:
	mov	x29, sp
LCFI106:
	ldp	w3, w5, [x2]
	sxtw	x2, w3
	sxtw	x4, w5
	add	x6, x2, 31
	cmp	x6, x4
	bne	L423
	cmp	w3, w5
	bgt	L418
	sub	x0, x0, x2
	sub	x6, x1, x2
	add	x5, x0, 11
	sub	x0, x2, #1
	.p2align 5,,15
L417:
	add	x0, x0, 1
	subs	w1, w0, w3
	bvs	L413
	cmp	w1, 31
	bgt	L415
	bhi	L424
	ldrb	w2, [x6, x0]
	ldrb	w1, [x5, x0]
	cmp	w2, w1
	bne	L419
L415:
	cmp	x4, x0
	bne	L417
L418:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI107:
	ret
	.p2align 2,,3
L419:
LCFI108:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI109:
	ret
L413:
LCFI110:
	adrp	x0, lC2@PAGE
	mov	w1, 852
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L424:
	adrp	x0, lC2@PAGE
	mov	w1, 853
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L423:
	adrp	x0, lC17@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE42:
	.const
	.align	3
lC18:
	.ascii "failed precondition from anubis_treasury.ads:497"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__is_builder_address
_anubis_treasury__is_builder_address:
LFB43:
	stp	x29, x30, [sp, -16]!
LCFI111:
	mov	x29, sp
LCFI112:
	mov	x8, x0
	ldp	w4, w7, [x1]
	ldp	w9, w3, [x3]
	sxtw	x1, w4
	sxtw	x5, w7
	add	x0, x1, 31
	sxtw	x6, w9
	cmp	x0, x5
	add	x10, x6, 31
	sxtw	x0, w3
	ccmp	x10, x0, 0, eq
	bne	L438
	cset	w0, ne
	cmp	w4, w7
	bgt	L434
	add	x6, x6, x1
	sub	x8, x8, x1
	sub	x7, x2, x6
	sub	x1, x1, #1
	.p2align 5,,15
L433:
	add	x1, x1, 1
	subs	w2, w1, w4
	bvs	L429
	cmp	w3, w2
	blt	L431
	cmp	w9, w2
	bgt	L439
	ldrb	w6, [x8, x1]
	ldrb	w2, [x7, x1]
	cmp	w6, w2
	bne	L427
L431:
	cmp	x5, x1
	bne	L433
L434:
	mov	w0, 1
L427:
	ldp	x29, x30, [sp], 16
LCFI113:
	ret
L429:
LCFI114:
	adrp	x0, lC2@PAGE
	mov	w1, 872
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L439:
	adrp	x0, lC2@PAGE
	mov	w1, 873
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L438:
	adrp	x0, lC18@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE43:
	.const
	.align	3
lC19:
	.ascii "failed precondition from anubis_treasury.ads:506"
	.align	3
lC20:
	.ascii "failed postcondition from anubis_treasury.ads:508"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__verify_non_builder
_anubis_treasury__verify_non_builder:
LFB44:
	stp	x29, x30, [sp, -48]!
LCFI115:
	mov	x29, sp
LCFI116:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI117:
	ldpsw	x5, x7, [x1]
	ldpsw	x4, x6, [x3]
	add	x5, x5, 31
	cmp	x5, x7
	add	x4, x4, 31
	ccmp	x4, x6, 0, eq
	bne	L444
	mov	x19, x1
	mov	x20, x3
	mov	x21, x0
	mov	x22, x2
	bl	_anubis_treasury__is_builder_address
	eor	w4, w0, 1
	mov	x1, x19
	mov	x2, x22
	mov	x3, x20
	mov	x0, x21
	and	w19, w4, 255
	bl	_anubis_treasury__is_builder_address
	eor	w1, w0, 1
	and	w0, w1, 255
	cmp	w19, w1, uxtb
	bne	L445
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI118:
	ret
L444:
LCFI119:
	adrp	x0, lC19@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L445:
	adrp	x0, lC20@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE44:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__update_circulating_supply
_anubis_treasury__update_circulating_supply:
LFB46:
	str	x1, [x0, 2960]
	ret
LFE46:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__available_spend_capacity
_anubis_treasury__available_spend_capacity:
LFB48:
	ldr	x0, [x0, 8]
	mov	x1, 22859
	movk	x1, 0x3886, lsl 16
	movk	x1, 0xc5d6, lsl 32
	movk	x1, 0x346d, lsl 48
	add	x0, x0, x0, lsl 2
	add	x0, x0, x0, lsl 2
	lsl	x0, x0, 3
	umulh	x0, x0, x1
	lsr	x0, x0, 11
	ret
LFE48:
	.const
	.align	3
lC21:
	.ascii "failed precondition from anubis_treasury.ads:539"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__serialize_treasury_state
_anubis_treasury__serialize_treasury_state:
LFB49:
	stp	x29, x30, [sp, -96]!
LCFI120:
	mov	x29, sp
LCFI121:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
LCFI122:
	mov	x19, x0
	add	x0, x29, 96
	stp	x21, x22, [sp, 32]
LCFI123:
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w21, w3, [x2]
	str	x0, [x29, 88]
	sxtw	x22, w21
	sxtw	x2, w3
	add	x0, x22, 4094
	str	x22, [x29, 64]
	cmp	x0, x2
	bge	L466
	tbnz	w21, #31, L467
	ldr	x0, [x29, 48]
	mov	w1, 0
	cmp	w21, w3
	add	x20, x29, 64
	sub	x2, x2, x22
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldr	x0, [x19]
	mov	x16, x20
	str	w21, [x29, 80]
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 8]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 16]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 24]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	w1, [x29, 80]
	tbnz	w1, #31, L468
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L453
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L469
	ldr	w2, [x19, 2912]
	tbnz	w2, #31, L470
	cmp	w2, 255
	bhi	L471
	ldr	x4, [x29, 48]
	sxtw	x0, w1
	mov	w3, 2147483647
	sub	x0, x0, x22
	strb	w2, [x4, x0]
	cmp	w1, w3
	beq	L472
	add	w1, w1, 1
L453:
	ldr	x0, [x19, 2920]
	mov	x16, x20
	str	w1, [x29, 80]
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 2928]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 2936]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 2944]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 2952]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 2960]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 2968]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	x0, [x19, 2976]
	mov	x16, x20
	bl	_anubis_treasury__serialize_treasury_state__write_u64.8
	ldr	w0, [x29, 80]
	tbnz	w0, #31, L473
	ldr	x1, [x29, 56]
	ldr	w1, [x1]
	subs	w0, w0, w1
	bvs	L460
	tbnz	w0, #31, L474
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI124:
	ret
L460:
LCFI125:
	adrp	x0, lC2@PAGE
	mov	w1, 953
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L473:
	adrp	x0, lC2@PAGE
	mov	w1, 953
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L468:
	adrp	x0, lC2@PAGE
	mov	w1, 939
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L467:
	adrp	x0, lC2@PAGE
	mov	w1, 917
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L466:
	adrp	x0, lC21@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC21@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L474:
	adrp	x0, lC2@PAGE
	mov	w1, 953
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L471:
	adrp	x0, lC2@PAGE
	mov	w1, 940
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L469:
	adrp	x0, lC2@PAGE
	mov	w1, 940
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L470:
	adrp	x0, lC2@PAGE
	mov	w1, 940
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L472:
	adrp	x0, lC2@PAGE
	mov	w1, 941
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE49:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__deserialize_treasury_state
_anubis_treasury__deserialize_treasury_state:
LFB51:
	stp	x29, x30, [sp, -96]!
LCFI126:
	mov	x29, sp
LCFI127:
	add	x3, x29, 48
	stp	x0, x1, [x29, 48]
	add	x0, x29, 96
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI128:
	ldr	w21, [x1]
	str	x3, [x29, 72]
	str	x0, [x29, 88]
	tbnz	w21, #31, L484
	mov	x0, x2
	mov	x20, x1
	mov	x1, 0
	mov	x19, x2
	bl	_anubis_treasury__init_treasury
	ldpsw	x1, x2, [x20]
	mov	w0, 0
	add	x1, x1, 98
	cmp	x1, x2
	blt	L485
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI129:
	ret
	.p2align 2,,3
L485:
LCFI130:
	add	x20, x29, 64
	sxtw	x0, w21
	str	w21, [x29, 80]
	mov	x16, x20
	str	x0, [x29, 64]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 8]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 16]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	ldr	w1, [x29, 80]
	str	x0, [x19, 24]
	ldr	x3, [x29, 64]
	tbnz	w1, #31, L486
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L479
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L487
	ldr	x4, [x29, 48]
	sxtw	x0, w1
	mov	w2, 2147483647
	sub	x0, x0, x3
	ldrb	w0, [x4, x0]
	str	w0, [x19, 2912]
	cmp	w1, w2
	beq	L488
	add	w1, w1, 1
L479:
	mov	x16, x20
	str	w1, [x29, 80]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 2920]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 2928]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 2936]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 2944]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 2952]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 2960]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x16, x20
	str	x0, [x19, 2968]
	bl	_anubis_treasury__deserialize_treasury_state__read_u64.9
	mov	x1, x0
	mov	w0, 1
	str	x1, [x19, 2976]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI131:
	ret
L484:
LCFI132:
	adrp	x0, lC2@PAGE
	mov	w1, 961
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L486:
	adrp	x0, lC2@PAGE
	mov	w1, 987
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L487:
	adrp	x0, lC2@PAGE
	mov	w1, 988
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L488:
	adrp	x0, lC2@PAGE
	mov	w1, 989
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE51:
	.const
	.align	3
lC22:
	.ascii "failed precondition from anubis_treasury.ads:554"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__serialize_proposal
_anubis_treasury__serialize_proposal:
LFB53:
	stp	x29, x30, [sp, -96]!
LCFI133:
	mov	x29, sp
LCFI134:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
LCFI135:
	mov	x20, x0
	add	x0, x29, 96
	stp	x21, x22, [sp, 32]
LCFI136:
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w22, w3, [x2]
	str	x0, [x29, 88]
	sxtw	x19, w22
	sxtw	x2, w3
	add	x0, x19, 510
	str	x19, [x29, 64]
	cmp	x0, x2
	bge	L519
	tbnz	w22, #31, L520
	ldr	x0, [x29, 48]
	cmp	w22, w3
	mov	w1, 0
	sub	x2, x2, x19
	add	x21, x29, 64
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldr	x0, [x20]
	mov	x16, x21
	str	w22, [x29, 80]
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	w2, [x29, 80]
	tbnz	w2, #31, L521
	ldr	x0, [x29, 56]
	ldp	w7, w4, [x0]
	cmp	w4, w2
	blt	L522
	cmp	w2, w7
	blt	L523
	ldrb	w3, [x20, 8]
	cmp	w3, 3
	bhi	L524
	ldr	x0, [x29, 48]
	sxtw	x1, w2
	mov	w5, 2147483647
	sub	x1, x1, x19
	strb	w3, [x0, x1]
	cmp	w2, w5
	beq	L525
	add	w1, w2, 1
	cmp	w1, w4
	bgt	L498
	ldrb	w6, [x20, 9]
	cmp	w6, 4
	bhi	L526
	sxtw	x3, w1
	sub	x3, x3, x19
	strb	w6, [x0, x3]
	cmp	w1, w5
	beq	L527
	add	w3, w2, 2
	cmp	w3, w4
	bgt	L528
	ldrb	w6, [x20, 10]
	cmp	w6, 7
	bhi	L529
	sxtw	x5, w3
	mov	w8, 2147483647
	sub	x5, x5, x19
	add	w1, w2, 3
	strb	w6, [x0, x5]
	cmp	w3, w8
	beq	L530
L498:
	add	x2, x20, 11
	add	x5, x20, 43
	mov	w8, 2147483647
	.p2align 5,,15
L507:
	cmp	w4, w1
	blt	L504
	cmp	w1, w7
	blt	L531
	ldrb	w6, [x2]
	sxtw	x3, w1
	sub	x3, x3, x19
	strb	w6, [x0, x3]
	cmp	w1, w8
	beq	L532
	add	w1, w1, 1
L504:
	add	x2, x2, 1
	cmp	x5, x2
	bne	L507
	ldr	x0, [x20, 48]
	mov	x16, x21
	str	w1, [x29, 80]
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	x0, [x20, 200]
	mov	x16, x21
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	x0, [x20, 216]
	mov	x16, x21
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	x0, [x20, 224]
	mov	x16, x21
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	x0, [x20, 248]
	mov	x16, x21
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	x0, [x20, 264]
	mov	x16, x21
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	x0, [x20, 272]
	mov	x16, x21
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	x0, [x20, 280]
	mov	x16, x21
	bl	_anubis_treasury__serialize_proposal__write_u64.10
	ldr	w0, [x29, 80]
	tbnz	w0, #31, L533
	ldr	x1, [x29, 56]
	ldr	w1, [x1]
	subs	w0, w0, w1
	bvs	L510
	tbnz	w0, #31, L534
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI137:
	ret
	.p2align 2,,3
L522:
LCFI138:
	ldr	x0, [x29, 48]
	mov	w1, w2
	b	L498
L531:
	adrp	x0, lC2@PAGE
	mov	w1, 1046
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L532:
	adrp	x0, lC2@PAGE
	mov	w1, 1047
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L520:
	adrp	x0, lC2@PAGE
	mov	w1, 1009
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L519:
	adrp	x0, lC22@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC22@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L523:
	adrp	x0, lC2@PAGE
	mov	w1, 1029
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L510:
	adrp	x0, lC2@PAGE
	mov	w1, 1060
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L534:
	adrp	x0, lC2@PAGE
	mov	w1, 1060
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L533:
	adrp	x0, lC2@PAGE
	mov	w1, 1060
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L521:
	adrp	x0, lC2@PAGE
	mov	w1, 1028
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L529:
	adrp	x0, lC2@PAGE
	mov	w1, 1039
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L527:
	adrp	x0, lC2@PAGE
	mov	w1, 1035
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L528:
	mov	w2, w3
	mov	w1, w2
	b	L498
L530:
	adrp	x0, lC2@PAGE
	mov	w1, 1040
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L524:
	adrp	x0, lC2@PAGE
	mov	w1, 1029
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L525:
	adrp	x0, lC2@PAGE
	mov	w1, 1030
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L526:
	adrp	x0, lC2@PAGE
	mov	w1, 1034
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE53:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__deserialize_proposal
_anubis_treasury__deserialize_proposal:
LFB55:
	stp	x29, x30, [sp, -96]!
LCFI139:
	mov	x29, sp
LCFI140:
	add	x3, x29, 48
	stp	x0, x1, [x29, 48]
	add	x0, x29, 96
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI141:
	str	x3, [x29, 72]
	str	x0, [x29, 88]
	ldp	w21, w20, [x1]
	tbnz	w21, #31, L558
	mov	x19, x2
	mov	w1, 0
	mov	x2, 288
	mov	x0, x19
	bl	_memset
	sxtw	x1, w21
	mov	w0, 0
	add	x2, x1, 98
	cmp	x2, w20, sxtw
	blt	L559
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI142:
	ret
	.p2align 2,,3
L559:
LCFI143:
	add	x20, x29, 64
	str	x1, [x29, 64]
	mov	x16, x20
	str	w21, [x29, 80]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	ldr	w3, [x29, 80]
	str	x0, [x19]
	ldr	x0, [x29, 64]
	tbnz	w3, #31, L560
	ldr	x1, [x29, 56]
	ldp	w7, w5, [x1]
	cmp	w5, w3
	blt	L561
	cmp	w3, w7
	blt	L562
	ldr	x2, [x29, 48]
	sxtw	x1, w3
	sub	x1, x1, x0
	ldrb	w1, [x2, x1]
	cmp	w1, 3
	bhi	L541
	strb	w1, [x19, 8]
L541:
	mov	w1, 2147483647
	cmp	w3, w1
	beq	L563
	add	w1, w3, 1
	cmp	w1, w5
	bgt	L543
	sxtw	x4, w1
	sub	x4, x4, x0
	ldrb	w4, [x2, x4]
	cmp	w4, 4
	bhi	L544
	strb	w4, [x19, 9]
L544:
	mov	w4, 2147483647
	cmp	w1, w4
	beq	L564
	add	w4, w3, 2
	cmp	w4, w5
	bgt	L565
	sxtw	x1, w4
	sub	x1, x1, x0
	ldrb	w1, [x2, x1]
	cmp	w1, 7
	bhi	L547
	strb	w1, [x19, 10]
L547:
	mov	w6, 2147483647
	add	w1, w3, 3
	cmp	w4, w6
	beq	L566
L543:
	add	x3, x19, 11
	add	x6, x19, 43
	mov	w8, 2147483647
	.p2align 5,,15
L552:
	cmp	w5, w1
	blt	L549
	cmp	w1, w7
	blt	L567
	sxtw	x4, w1
	sub	x4, x4, x0
	ldrb	w4, [x2, x4]
	strb	w4, [x3]
	cmp	w1, w8
	beq	L568
	add	w1, w1, 1
L549:
	add	x3, x3, 1
	cmp	x3, x6
	bne	L552
	mov	x16, x20
	str	w1, [x29, 80]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	mov	x16, x20
	str	x0, [x19, 48]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	mov	x16, x20
	str	x0, [x19, 200]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	mov	x16, x20
	str	x0, [x19, 216]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	mov	x16, x20
	str	x0, [x19, 224]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	mov	x16, x20
	str	x0, [x19, 248]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	mov	x16, x20
	str	x0, [x19, 264]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	mov	x16, x20
	str	x0, [x19, 272]
	bl	_anubis_treasury__deserialize_proposal__read_u64.11
	mov	x1, x0
	mov	w0, 1
	str	x1, [x19, 280]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI144:
	ret
	.p2align 2,,3
L561:
LCFI145:
	ldr	x2, [x29, 48]
	mov	w1, w3
	b	L543
L567:
	adrp	x0, lC2@PAGE
	mov	w1, 1139
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L568:
	adrp	x0, lC2@PAGE
	mov	w1, 1140
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L560:
	adrp	x0, lC2@PAGE
	mov	w1, 1115
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L562:
	adrp	x0, lC2@PAGE
	mov	w1, 1116
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L558:
	adrp	x0, lC2@PAGE
	mov	w1, 1068
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L566:
	adrp	x0, lC2@PAGE
	mov	w1, 1133
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L565:
	mov	w3, w4
	mov	w1, w3
	b	L543
L564:
	adrp	x0, lC2@PAGE
	mov	w1, 1126
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L563:
	adrp	x0, lC2@PAGE
	mov	w1, 1119
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE55:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__zeroize_treasury_state
_anubis_treasury__zeroize_treasury_state:
LFB57:
	movi	v31.4s, 0
	movi	v30.4s, 0
	mov	x3, 0
	add	x2, x0, 40
	stp	q31, q31, [x0]
	.p2align 5,,15
L570:
	add	x1, x3, x3, lsl 3
	add	x3, x3, 1
	add	x1, x0, x1, lsl 5
	add	x8, x1, 43
	add	x7, x1, 89
	str	xzr, [x1, 32]
	add	x6, x1, 121
	add	x4, x1, 199
	strh	wzr, [x2]
	add	x2, x2, 288
	strb	wzr, [x1, 42]
	stp	xzr, xzr, [x8]
	stp	xzr, xzr, [x8, 16]
	str	xzr, [x1, 80]
	strb	wzr, [x1, 88]
	stp	xzr, xzr, [x7]
	stp	xzr, xzr, [x7, 16]
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	str	q30, [x1, 169]
	str	q30, [x1, 153]
	str	q30, [x1, 183]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	stp	q31, q31, [x2, -96]
	str	xzr, [x1, 264]
	str	wzr, [x1, 272]
	stp	q31, q31, [x2, -48]
	str	xzr, [x1, 312]
	cmp	x3, 10
	bne	L570
	add	x1, x0, 3072
	str	wzr, [x0, 2912]
	sub	x0, x1, #152
	stp	q31, q31, [x0]
	stp	q31, q31, [x0, 32]
	ret
LFE57:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__zeroize_proposal
_anubis_treasury__zeroize_proposal:
LFB58:
	add	x5, x0, 11
	str	xzr, [x0]
	movi	v30.4s, 0
	strh	wzr, [x0, 8]
	add	x4, x0, 57
	add	x3, x0, 89
	strb	wzr, [x0, 10]
	movi	v31.4s, 0
	add	x1, x0, 167
	stp	xzr, xzr, [x5]
	add	x6, x0, 512
	stp	xzr, xzr, [x5, 16]
	str	xzr, [x0, 48]
	strb	wzr, [x0, 56]
	stp	xzr, xzr, [x4]
	stp	xzr, xzr, [x4, 16]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	str	q30, [x0, 137]
	str	q30, [x0, 121]
	str	q30, [x0, 151]
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	str	q31, [x0, 200]
	str	q31, [x0, 216]
	str	xzr, [x0, 232]
	str	wzr, [x0, 240]
	str	q31, [x0, 248]
	str	q31, [x6, -248]
	str	xzr, [x0, 280]
	ret
LFE58:
	.align	2
	.p2align 5,,15
	.globl _anubis_treasury__zeroize_vote
_anubis_treasury__zeroize_vote:
LFB59:
	movi	v31.4s, 0
	add	x2, x0, 57
	stp	xzr, xzr, [x0]
	add	x1, x0, 104
	stp	xzr, xzr, [x0, 16]
	str	xzr, [x0, 32]
	strb	wzr, [x0, 40]
	str	xzr, [x0, 48]
	strb	wzr, [x0, 56]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	str	xzr, [x0, 96]
	str	q31, [x0, 104]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	str	q31, [x0, 216]
	ret
LFE59:
	.const
	.align	2
_CSWTCH.359:
	.word	700
	.word	1000
	.word	1200
	.align	3
_CSWTCH.352:
	.xword	5000
	.xword	15000
	.xword	30000
	.align	3
_execute_resultG.12:
	.byte	0
	.byte	0
	.byte	1
	.byte	4
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.space 3
	.align	3
_vote_resultG.16:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	5
	.byte	4
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.space 1
_propose_resultG.20:
	.byte	0
	.byte	0
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	5
	.byte	4
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	3
	.byte	0
	.byte	6
	.byte	0
	.align	1
_propose_resultT2.21:
	.byte	17
	.byte	1
	.align	1
_propose_resultT1.22:
	.byte	13
	.byte	15
	.align	3
_propose_resultP.23:
	.word	5
	.word	9
_proposal_stateG.24:
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	5
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	5
	.byte	6
	.byte	2
	.byte	0
	.align	1
_proposal_stateT2.25:
	.byte	10
	.byte	13
	.align	1
_proposal_stateT1.26:
	.byte	10
	.byte	10
	.align	3
_proposal_stateP.27:
	.word	1
	.word	3
	.align	3
_proposal_typeG.28:
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	1
	.byte	3
	.byte	0
	.byte	0
	.space 4
	.align	3
_proposal_categoryG.32:
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.space 4
	.globl _anubis_treasury__execute_resultN
	.align	3
_anubis_treasury__execute_resultN:
	.byte	1
	.byte	9
	.byte	19
	.byte	34
	.byte	50
	.byte	71
	.byte	87
	.space 1
	.globl _anubis_treasury__execute_resultS
	.align	3
_anubis_treasury__execute_resultS:
	.ascii "EXECUTEDNOT_PASSEDTIMELOCK_ACTIVEALREADY_EXECUTEDINSUFFICIENT_TREASURYEXECUTION_FAILED"
	.globl _anubis_treasury__vote_resultN
	.align	3
_anubis_treasury__vote_resultN:
	.byte	1
	.byte	6
	.byte	19
	.byte	38
	.byte	58
	.byte	75
	.byte	87
	.space 1
	.globl _anubis_treasury__vote_resultS
	.align	3
_anubis_treasury__vote_resultS:
	.ascii "VOTEDALREADY_VOTEDPROPOSAL_NOT_ACTIVEINSUFFICIENT_BALANCEINVALID_SIGNATUREVOTING_ENDED"
	.globl _anubis_treasury__propose_resultN
	.align	3
_anubis_treasury__propose_resultN:
	.byte	1
	.byte	9
	.byte	26
	.byte	40
	.byte	55
	.byte	69
	.byte	86
	.byte	101
	.byte	118
	.space 7
	.globl _anubis_treasury__propose_resultS
	.align	3
_anubis_treasury__propose_resultS:
	.ascii "PROPOSEDINSUFFICIENT_BONDTREASURY_EMPTYTOO_MANY_ACTIVEINVALID_AMOUNTINVALID_RECIPIENTPROPOSER_BANNEDCATEGORY_MISMATCH"
	.globl _anubis_treasury__vote_choiceN
	.align	2
_anubis_treasury__vote_choiceN:
	.byte	1
	.byte	13
	.byte	29
	.byte	36
	.globl _anubis_treasury__vote_choiceS
	.align	3
_anubis_treasury__vote_choiceS:
	.ascii "FOR_PROPOSALAGAINST_PROPOSALABSTAIN"
	.globl _anubis_treasury__proposal_stateN
	.align	3
_anubis_treasury__proposal_stateN:
	.byte	1
	.byte	6
	.byte	12
	.byte	18
	.byte	26
	.byte	34
	.byte	43
	.byte	50
	.byte	56
	.space 7
	.globl _anubis_treasury__proposal_stateS
	.align	3
_anubis_treasury__proposal_stateS:
	.ascii "DRAFTACTIVEPASSEDREJECTEDEXECUTEDCANCELLEDEXPIREDVETOED"
	.globl _anubis_treasury__proposal_typeN
	.align	3
_anubis_treasury__proposal_typeN:
	.byte	1
	.byte	9
	.byte	25
	.byte	32
	.byte	41
	.byte	65
	.space 2
	.globl _anubis_treasury__proposal_typeS
	.align	3
_anubis_treasury__proposal_typeS:
	.ascii "SPENDINGPARAMETER_CHANGEUPGRADEEMERGENCYCONSTITUTIONAL_AMENDMENT"
	.globl _anubis_treasury__proposal_categoryN
	.align	3
_anubis_treasury__proposal_categoryN:
	.byte	1
	.byte	12
	.byte	24
	.byte	35
	.byte	49
	.space 3
	.globl _anubis_treasury__proposal_categoryS
	.align	3
_anubis_treasury__proposal_categoryS:
	.ascii "SMALL_GRANTMEDIUM_GRANTLARGE_GRANTCONSTITUTIONAL"
	.data
	.align	3
_anubis_treasury__proposal_counter:
	.space 8
	.globl _anubis_treasury_E
	.align	1
_anubis_treasury_E:
	.space 2
	.literal16
	.align	4
lC3:
	.xword	300000000
	.xword	300000000
	.align	4
lC5:
	.xword	100800
	.xword	302400
	.align	4
lC6:
	.xword	144000
	.xword	446400
	.align	4
lC7:
	.xword	72000
	.xword	172800
	.align	4
lC8:
	.xword	201600
	.xword	806400
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
	.quad	LFB50-.
	.set L$set$2,LFE50-LFB50
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB50
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
	.quad	LFB54-.
	.set L$set$8,LFE54-LFB54
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB54
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
	.quad	LFB52-.
	.set L$set$14,LFE52-LFB52
	.quad L$set$14
	.uleb128 0
	.byte	0x4
	.set L$set$15,LCFI8-LFB52
	.long L$set$15
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$16,LCFI9-LCFI8
	.long L$set$16
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$17,LCFI10-LCFI9
	.long L$set$17
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
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
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$22,LEFDE7-LASFDE7
	.long L$set$22
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB56-.
	.set L$set$23,LFE56-LFB56
	.quad L$set$23
	.uleb128 0
	.byte	0x4
	.set L$set$24,LCFI15-LFB56
	.long L$set$24
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$25,LCFI16-LCFI15
	.long L$set$25
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$26,LCFI17-LCFI16
	.long L$set$26
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$27,LCFI18-LCFI17
	.long L$set$27
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$28,LCFI19-LCFI18
	.long L$set$28
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$29,LCFI20-LCFI19
	.long L$set$29
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
	.quad	LFB2-.
	.set L$set$32,LFE2-LFB2
	.quad L$set$32
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$33,LEFDE11-LASFDE11
	.long L$set$33
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB3-.
	.set L$set$34,LFE3-LFB3
	.quad L$set$34
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$35,LEFDE13-LASFDE13
	.long L$set$35
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB4-.
	.set L$set$36,LFE4-LFB4
	.quad L$set$36
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$37,LEFDE15-LASFDE15
	.long L$set$37
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB5-.
	.set L$set$38,LFE5-LFB5
	.quad L$set$38
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$39,LEFDE17-LASFDE17
	.long L$set$39
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB6-.
	.set L$set$40,LFE6-LFB6
	.quad L$set$40
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$41,LEFDE19-LASFDE19
	.long L$set$41
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB136-.
	.set L$set$42,LFE136-LFB136
	.quad L$set$42
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$43,LEFDE21-LASFDE21
	.long L$set$43
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB8-.
	.set L$set$44,LFE8-LFB8
	.quad L$set$44
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$45,LEFDE23-LASFDE23
	.long L$set$45
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB138-.
	.set L$set$46,LFE138-LFB138
	.quad L$set$46
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$47,LEFDE25-LASFDE25
	.long L$set$47
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB10-.
	.set L$set$48,LFE10-LFB10
	.quad L$set$48
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$49,LEFDE27-LASFDE27
	.long L$set$49
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB11-.
	.set L$set$50,LFE11-LFB11
	.quad L$set$50
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$51,LEFDE29-LASFDE29
	.long L$set$51
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB12-.
	.set L$set$52,LFE12-LFB12
	.quad L$set$52
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$53,LEFDE31-LASFDE31
	.long L$set$53
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB13-.
	.set L$set$54,LFE13-LFB13
	.quad L$set$54
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI22-LFB13
	.long L$set$55
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$56,LCFI23-LCFI22
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$57,LCFI24-LCFI23
	.long L$set$57
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$58,LCFI25-LCFI24
	.long L$set$58
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$59,LEFDE33-LASFDE33
	.long L$set$59
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB15-.
	.set L$set$60,LFE15-LFB15
	.quad L$set$60
	.uleb128 0
	.byte	0x4
	.set L$set$61,LCFI26-LFB15
	.long L$set$61
	.byte	0xe
	.uleb128 0xb0
	.byte	0x9d
	.uleb128 0x16
	.byte	0x9e
	.uleb128 0x15
	.byte	0x4
	.set L$set$62,LCFI27-LCFI26
	.long L$set$62
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$63,LCFI28-LCFI27
	.long L$set$63
	.byte	0x93
	.uleb128 0x14
	.byte	0x94
	.uleb128 0x13
	.byte	0x95
	.uleb128 0x12
	.byte	0x96
	.uleb128 0x11
	.byte	0x4
	.set L$set$64,LCFI29-LCFI28
	.long L$set$64
	.byte	0x97
	.uleb128 0x10
	.byte	0x98
	.uleb128 0xf
	.byte	0x4
	.set L$set$65,LCFI30-LCFI29
	.long L$set$65
	.byte	0x99
	.uleb128 0xe
	.byte	0x9a
	.uleb128 0xd
	.byte	0x9b
	.uleb128 0xc
	.byte	0x9c
	.uleb128 0xb
	.byte	0x4
	.set L$set$66,LCFI31-LCFI30
	.long L$set$66
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
	.set L$set$67,LCFI32-LCFI31
	.long L$set$67
	.byte	0xb
	.byte	0x4
	.set L$set$68,LCFI33-LCFI32
	.long L$set$68
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
	.set L$set$69,LCFI34-LCFI33
	.long L$set$69
	.byte	0xb
	.byte	0x4
	.set L$set$70,LCFI35-LCFI34
	.long L$set$70
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
	.set L$set$71,LCFI36-LCFI35
	.long L$set$71
	.byte	0xb
	.byte	0x4
	.set L$set$72,LCFI37-LCFI36
	.long L$set$72
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
	.quad	LFB16-.
	.set L$set$75,LFE16-LFB16
	.quad L$set$75
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI39-LFB16
	.long L$set$76
	.byte	0xe
	.uleb128 0x90
	.byte	0x9d
	.uleb128 0x12
	.byte	0x9e
	.uleb128 0x11
	.byte	0x4
	.set L$set$77,LCFI40-LCFI39
	.long L$set$77
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$78,LCFI41-LCFI40
	.long L$set$78
	.byte	0x93
	.uleb128 0x10
	.byte	0x94
	.uleb128 0xf
	.byte	0x95
	.uleb128 0xe
	.byte	0x96
	.uleb128 0xd
	.byte	0x97
	.uleb128 0xc
	.byte	0x98
	.uleb128 0xb
	.byte	0x99
	.uleb128 0xa
	.byte	0x9a
	.uleb128 0x9
	.byte	0x4
	.set L$set$79,LCFI42-LCFI41
	.long L$set$79
	.byte	0x9b
	.uleb128 0x8
	.byte	0x9c
	.uleb128 0x7
	.byte	0x4
	.set L$set$80,LCFI43-LCFI42
	.long L$set$80
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
	.set L$set$81,LCFI44-LCFI43
	.long L$set$81
	.byte	0xb
	.byte	0x4
	.set L$set$82,LCFI45-LCFI44
	.long L$set$82
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
	.set L$set$83,LCFI46-LCFI45
	.long L$set$83
	.byte	0xb
	.byte	0x4
	.set L$set$84,LCFI47-LCFI46
	.long L$set$84
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
	.quad	LFB17-.
	.set L$set$87,LFE17-LFB17
	.quad L$set$87
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$88,LEFDE39-LASFDE39
	.long L$set$88
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB18-.
	.set L$set$89,LFE18-LFB18
	.quad L$set$89
	.uleb128 0
	.byte	0x4
	.set L$set$90,LCFI49-LFB18
	.long L$set$90
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$91,LCFI50-LCFI49
	.long L$set$91
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$92,LEFDE41-LASFDE41
	.long L$set$92
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB19-.
	.set L$set$93,LFE19-LFB19
	.quad L$set$93
	.uleb128 0
	.byte	0x4
	.set L$set$94,LCFI51-LFB19
	.long L$set$94
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$95,LCFI52-LCFI51
	.long L$set$95
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$96,LEFDE43-LASFDE43
	.long L$set$96
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB20-.
	.set L$set$97,LFE20-LFB20
	.quad L$set$97
	.uleb128 0
	.byte	0x4
	.set L$set$98,LCFI53-LFB20
	.long L$set$98
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$99,LCFI54-LCFI53
	.long L$set$99
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$100,LEFDE45-LASFDE45
	.long L$set$100
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB22-.
	.set L$set$101,LFE22-LFB22
	.quad L$set$101
	.uleb128 0
	.byte	0x4
	.set L$set$102,LCFI55-LFB22
	.long L$set$102
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$103,LCFI56-LCFI55
	.long L$set$103
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$104,LCFI57-LCFI56
	.long L$set$104
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$105,LCFI58-LCFI57
	.long L$set$105
	.byte	0xb
	.byte	0x4
	.set L$set$106,LCFI59-LCFI58
	.long L$set$106
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$107,LCFI60-LCFI59
	.long L$set$107
	.byte	0xb
	.byte	0x4
	.set L$set$108,LCFI61-LCFI60
	.long L$set$108
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$109,LCFI62-LCFI61
	.long L$set$109
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$110,LEFDE47-LASFDE47
	.long L$set$110
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB23-.
	.set L$set$111,LFE23-LFB23
	.quad L$set$111
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI63-LFB23
	.long L$set$112
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$113,LCFI64-LCFI63
	.long L$set$113
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$114,LEFDE49-LASFDE49
	.long L$set$114
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB24-.
	.set L$set$115,LFE24-LFB24
	.quad L$set$115
	.uleb128 0
	.byte	0x4
	.set L$set$116,LCFI65-LFB24
	.long L$set$116
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$117,LCFI66-LCFI65
	.long L$set$117
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$118,LEFDE51-LASFDE51
	.long L$set$118
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB25-.
	.set L$set$119,LFE25-LFB25
	.quad L$set$119
	.uleb128 0
	.byte	0x4
	.set L$set$120,LCFI67-LFB25
	.long L$set$120
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$121,LCFI68-LCFI67
	.long L$set$121
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$122,LEFDE53-LASFDE53
	.long L$set$122
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB26-.
	.set L$set$123,LFE26-LFB26
	.quad L$set$123
	.uleb128 0
	.byte	0x4
	.set L$set$124,LCFI69-LFB26
	.long L$set$124
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$125,LCFI70-LCFI69
	.long L$set$125
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$126,LCFI71-LCFI70
	.long L$set$126
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$127,LCFI72-LCFI71
	.long L$set$127
	.byte	0xb
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$128,LEFDE55-LASFDE55
	.long L$set$128
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB28-.
	.set L$set$129,LFE28-LFB28
	.quad L$set$129
	.uleb128 0
	.byte	0x4
	.set L$set$130,LCFI73-LFB28
	.long L$set$130
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$131,LCFI74-LCFI73
	.long L$set$131
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$132,LCFI75-LCFI74
	.long L$set$132
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$133,LCFI76-LCFI75
	.long L$set$133
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$134,LCFI77-LCFI76
	.long L$set$134
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
	.set L$set$135,LCFI78-LCFI77
	.long L$set$135
	.byte	0xb
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$136,LEFDE57-LASFDE57
	.long L$set$136
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB29-.
	.set L$set$137,LFE29-LFB29
	.quad L$set$137
	.uleb128 0
	.byte	0x4
	.set L$set$138,LCFI79-LFB29
	.long L$set$138
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$139,LCFI80-LCFI79
	.long L$set$139
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$140,LCFI81-LCFI80
	.long L$set$140
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$141,LCFI82-LCFI81
	.long L$set$141
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$142,LEFDE59-LASFDE59
	.long L$set$142
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB30-.
	.set L$set$143,LFE30-LFB30
	.quad L$set$143
	.uleb128 0
	.byte	0x4
	.set L$set$144,LCFI83-LFB30
	.long L$set$144
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$145,LCFI84-LCFI83
	.long L$set$145
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$146,LCFI85-LCFI84
	.long L$set$146
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$147,LCFI86-LCFI85
	.long L$set$147
	.byte	0xb
	.byte	0x4
	.set L$set$148,LCFI87-LCFI86
	.long L$set$148
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$149,LCFI88-LCFI87
	.long L$set$149
	.byte	0xb
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$150,LEFDE61-LASFDE61
	.long L$set$150
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB31-.
	.set L$set$151,LFE31-LFB31
	.quad L$set$151
	.uleb128 0
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$152,LEFDE63-LASFDE63
	.long L$set$152
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB32-.
	.set L$set$153,LFE32-LFB32
	.quad L$set$153
	.uleb128 0
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$154,LEFDE65-LASFDE65
	.long L$set$154
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB33-.
	.set L$set$155,LFE33-LFB33
	.quad L$set$155
	.uleb128 0
	.byte	0x4
	.set L$set$156,LCFI89-LFB33
	.long L$set$156
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$157,LCFI90-LCFI89
	.long L$set$157
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$158,LCFI91-LCFI90
	.long L$set$158
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$159,LCFI92-LCFI91
	.long L$set$159
	.byte	0xb
	.byte	0x4
	.set L$set$160,LCFI93-LCFI92
	.long L$set$160
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$161,LCFI94-LCFI93
	.long L$set$161
	.byte	0xb
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$162,LEFDE67-LASFDE67
	.long L$set$162
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB34-.
	.set L$set$163,LFE34-LFB34
	.quad L$set$163
	.uleb128 0
	.byte	0x4
	.set L$set$164,LCFI95-LFB34
	.long L$set$164
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$165,LCFI96-LCFI95
	.long L$set$165
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$166,LEFDE69-LASFDE69
	.long L$set$166
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB35-.
	.set L$set$167,LFE35-LFB35
	.quad L$set$167
	.uleb128 0
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$168,LEFDE71-LASFDE71
	.long L$set$168
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB37-.
	.set L$set$169,LFE37-LFB37
	.quad L$set$169
	.uleb128 0
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$170,LEFDE73-LASFDE73
	.long L$set$170
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB39-.
	.set L$set$171,LFE39-LFB39
	.quad L$set$171
	.uleb128 0
	.byte	0x4
	.set L$set$172,LCFI97-LFB39
	.long L$set$172
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$173,LCFI98-LCFI97
	.long L$set$173
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$174,LEFDE75-LASFDE75
	.long L$set$174
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB41-.
	.set L$set$175,LFE41-LFB41
	.quad L$set$175
	.uleb128 0
	.byte	0x4
	.set L$set$176,LCFI99-LFB41
	.long L$set$176
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$177,LCFI100-LCFI99
	.long L$set$177
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$178,LCFI101-LCFI100
	.long L$set$178
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$179,LCFI102-LCFI101
	.long L$set$179
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$180,LCFI103-LCFI102
	.long L$set$180
	.byte	0xb
	.byte	0x4
	.set L$set$181,LCFI104-LCFI103
	.long L$set$181
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$182,LEFDE77-LASFDE77
	.long L$set$182
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB42-.
	.set L$set$183,LFE42-LFB42
	.quad L$set$183
	.uleb128 0
	.byte	0x4
	.set L$set$184,LCFI105-LFB42
	.long L$set$184
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$185,LCFI106-LCFI105
	.long L$set$185
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$186,LCFI107-LCFI106
	.long L$set$186
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$187,LCFI108-LCFI107
	.long L$set$187
	.byte	0xb
	.byte	0x4
	.set L$set$188,LCFI109-LCFI108
	.long L$set$188
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$189,LCFI110-LCFI109
	.long L$set$189
	.byte	0xb
	.align	3
LEFDE77:
LSFDE79:
	.set L$set$190,LEFDE79-LASFDE79
	.long L$set$190
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB43-.
	.set L$set$191,LFE43-LFB43
	.quad L$set$191
	.uleb128 0
	.byte	0x4
	.set L$set$192,LCFI111-LFB43
	.long L$set$192
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$193,LCFI112-LCFI111
	.long L$set$193
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$194,LCFI113-LCFI112
	.long L$set$194
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$195,LCFI114-LCFI113
	.long L$set$195
	.byte	0xb
	.align	3
LEFDE79:
LSFDE81:
	.set L$set$196,LEFDE81-LASFDE81
	.long L$set$196
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB44-.
	.set L$set$197,LFE44-LFB44
	.quad L$set$197
	.uleb128 0
	.byte	0x4
	.set L$set$198,LCFI115-LFB44
	.long L$set$198
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$199,LCFI116-LCFI115
	.long L$set$199
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$200,LCFI117-LCFI116
	.long L$set$200
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$201,LCFI118-LCFI117
	.long L$set$201
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
	.set L$set$202,LCFI119-LCFI118
	.long L$set$202
	.byte	0xb
	.align	3
LEFDE81:
LSFDE83:
	.set L$set$203,LEFDE83-LASFDE83
	.long L$set$203
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB46-.
	.set L$set$204,LFE46-LFB46
	.quad L$set$204
	.uleb128 0
	.align	3
LEFDE83:
LSFDE85:
	.set L$set$205,LEFDE85-LASFDE85
	.long L$set$205
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB48-.
	.set L$set$206,LFE48-LFB48
	.quad L$set$206
	.uleb128 0
	.align	3
LEFDE85:
LSFDE87:
	.set L$set$207,LEFDE87-LASFDE87
	.long L$set$207
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB49-.
	.set L$set$208,LFE49-LFB49
	.quad L$set$208
	.uleb128 0
	.byte	0x4
	.set L$set$209,LCFI120-LFB49
	.long L$set$209
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$210,LCFI121-LCFI120
	.long L$set$210
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$211,LCFI122-LCFI121
	.long L$set$211
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$212,LCFI123-LCFI122
	.long L$set$212
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$213,LCFI124-LCFI123
	.long L$set$213
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
	.set L$set$214,LCFI125-LCFI124
	.long L$set$214
	.byte	0xb
	.align	3
LEFDE87:
LSFDE89:
	.set L$set$215,LEFDE89-LASFDE89
	.long L$set$215
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB51-.
	.set L$set$216,LFE51-LFB51
	.quad L$set$216
	.uleb128 0
	.byte	0x4
	.set L$set$217,LCFI126-LFB51
	.long L$set$217
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$218,LCFI127-LCFI126
	.long L$set$218
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$219,LCFI128-LCFI127
	.long L$set$219
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x4
	.set L$set$220,LCFI129-LCFI128
	.long L$set$220
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
	.set L$set$221,LCFI130-LCFI129
	.long L$set$221
	.byte	0xb
	.byte	0x4
	.set L$set$222,LCFI131-LCFI130
	.long L$set$222
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
	.set L$set$223,LCFI132-LCFI131
	.long L$set$223
	.byte	0xb
	.align	3
LEFDE89:
LSFDE91:
	.set L$set$224,LEFDE91-LASFDE91
	.long L$set$224
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB53-.
	.set L$set$225,LFE53-LFB53
	.quad L$set$225
	.uleb128 0
	.byte	0x4
	.set L$set$226,LCFI133-LFB53
	.long L$set$226
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$227,LCFI134-LCFI133
	.long L$set$227
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$228,LCFI135-LCFI134
	.long L$set$228
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$229,LCFI136-LCFI135
	.long L$set$229
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$230,LCFI137-LCFI136
	.long L$set$230
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
	.set L$set$231,LCFI138-LCFI137
	.long L$set$231
	.byte	0xb
	.align	3
LEFDE91:
LSFDE93:
	.set L$set$232,LEFDE93-LASFDE93
	.long L$set$232
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB55-.
	.set L$set$233,LFE55-LFB55
	.quad L$set$233
	.uleb128 0
	.byte	0x4
	.set L$set$234,LCFI139-LFB55
	.long L$set$234
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$235,LCFI140-LCFI139
	.long L$set$235
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$236,LCFI141-LCFI140
	.long L$set$236
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x4
	.set L$set$237,LCFI142-LCFI141
	.long L$set$237
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
	.set L$set$238,LCFI143-LCFI142
	.long L$set$238
	.byte	0xb
	.byte	0x4
	.set L$set$239,LCFI144-LCFI143
	.long L$set$239
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
	.set L$set$240,LCFI145-LCFI144
	.long L$set$240
	.byte	0xb
	.align	3
LEFDE93:
LSFDE95:
	.set L$set$241,LEFDE95-LASFDE95
	.long L$set$241
LASFDE95:
	.long	LASFDE95-EH_frame1
	.quad	LFB57-.
	.set L$set$242,LFE57-LFB57
	.quad L$set$242
	.uleb128 0
	.align	3
LEFDE95:
LSFDE97:
	.set L$set$243,LEFDE97-LASFDE97
	.long L$set$243
LASFDE97:
	.long	LASFDE97-EH_frame1
	.quad	LFB58-.
	.set L$set$244,LFE58-LFB58
	.quad L$set$244
	.uleb128 0
	.align	3
LEFDE97:
LSFDE99:
	.set L$set$245,LEFDE99-LASFDE99
	.long L$set$245
LASFDE99:
	.long	LASFDE99-EH_frame1
	.quad	LFB59-.
	.set L$set$246,LFE59-LFB59
	.quad L$set$246
	.uleb128 0
	.align	3
LEFDE99:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
