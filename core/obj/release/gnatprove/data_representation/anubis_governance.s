	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC12:
	.ascii "anubis_governance.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_governance__serialize_governance_state__write_u64.2:
LFB38:
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
	adrp	x0, lC12@PAGE
	mov	w1, 605
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L16:
	adrp	x0, lC12@PAGE
	mov	w1, 603
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE38:
	.align	2
	.p2align 5,,15
_anubis_governance__deserialize_governance_state__read_u64.3:
LFB40:
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
	adrp	x0, lC12@PAGE
	mov	w1, 656
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L27:
	adrp	x0, lC12@PAGE
	mov	w1, 657
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE40:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__governance_phaseH
_anubis_governance__governance_phaseH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L31
	sub	w1, w1, w2
	cmp	w1, 4
	bgt	L32
L31:
	adrp	x1, _governance_phaseG.16@PAGE
	mov	x2, 0
	add	x1, x1, _governance_phaseG.16@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L32:
	ldrb	w0, [x0, 5]
	mov	w1, 36409
	movk	w1, 0x38e3, lsl 16
	add	w2, w0, w0, lsl 1
	lsl	w0, w0, 2
	lsl	w2, w2, 1
	umull	x3, w0, w1
	umull	x1, w2, w1
	lsr	x3, x3, 33
	add	w3, w3, w3, lsl 3
	lsr	x1, x1, 33
	add	w1, w1, w1, lsl 3
	sub	w0, w0, w3
	sxtw	x0, w0
	sub	w2, w2, w1
	adrp	x1, _governance_phaseG.16@PAGE
	sxtw	x2, w2
	add	x1, x1, _governance_phaseG.16@PAGEOFF;
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__privilege_typeH
_anubis_governance__privilege_typeH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L36
	sub	w1, w1, w2
	cmp	w1, 3
	bgt	L37
L36:
	mov	x3, 0
	mov	x0, 0
L34:
	adrp	x2, _privilege_typeG.12@PAGE
	mov	w1, 43691
	add	x2, x2, _privilege_typeG.12@PAGEOFF;
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
L37:
	ldrb	w1, [x0, 4]
	mov	w0, 36409
	movk	w0, 0x38e3, lsl 16
	add	w2, w1, w1, lsl 1
	lsl	w4, w1, 4
	sub	w4, w4, w1
	add	w1, w1, w2, lsl 2
	umull	x3, w4, w0
	umull	x0, w1, w0
	lsr	x3, x3, 34
	add	w3, w3, w3, lsl 3
	lsr	x0, x0, 34
	add	w0, w0, w0, lsl 3
	sub	w3, w4, w3, lsl 1
	sub	w0, w1, w0, lsl 1
	sxtw	x3, w3
	sxtw	x0, w0
	b	L34
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__privilege_holderH
_anubis_governance__privilege_holderH:
LFB4:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L40
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
	adrp	x2, _privilege_holderG.8@PAGE
	add	x2, x2, _privilege_holderG.8@PAGEOFF;
	sxtw	x1, w1
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L40:
	adrp	x2, _privilege_holderG.8@PAGE
	mov	x1, 0
	add	x2, x2, _privilege_holderG.8@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__privilege_recordIP
_anubis_governance__privilege_recordIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__Tprivilege_arrayBIP
_anubis_governance__Tprivilege_arrayBIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__council_memberIP
_anubis_governance__council_memberIP:
LFB96:
	ret
LFE96:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__Tcouncil_arrayBIP
_anubis_governance__Tcouncil_arrayBIP:
LFB8:
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__governance_stateIP
_anubis_governance__governance_stateIP:
LFB98:
	ret
LFE98:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__action_resultH
_anubis_governance__action_resultH:
LFB10:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	blt	L49
	mov	x3, 0
	mov	x1, 0
L47:
	adrp	x0, _action_resultG.4@PAGE
	mov	w2, 7
	add	x0, x0, _action_resultG.4@PAGEOFF;
	ldrb	w1, [x0, x1]
	ldrb	w0, [x0, x3]
	add	w1, w1, w0
	udiv	w2, w1, w2
	lsl	w0, w2, 3
	sub	w0, w0, w2
	sub	w0, w1, w0
	ret
	.p2align 2,,3
L49:
	ldrb	w3, [x0, 1]
	mov	w0, 19
	add	w3, w3, w3, lsl 1
	lsl	w1, w3, 2
	udiv	w2, w3, w0
	udiv	w0, w1, w0
	add	w4, w2, w2, lsl 3
	add	w4, w2, w4, lsl 1
	add	w2, w0, w0, lsl 3
	sub	w3, w3, w4
	sxtw	x3, w3
	add	w0, w0, w2, lsl 1
	sub	w1, w1, w0
	sxtw	x1, w1
	b	L47
LFE10:
	.const
	.align	3
lC13:
	.ascii "failed precondition from anubis_governance.ads:149"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__init_governance
_anubis_governance__init_governance:
LFB11:
	stp	x29, x30, [sp, -32]!
LCFI11:
	mov	x29, sp
LCFI12:
	ldrsw	x1, [x2]
	str	x19, [sp, 16]
LCFI13:
	mov	x19, x0
	ldrsw	x0, [x2, 4]
	add	x1, x1, 31
	cmp	x1, x0
	bne	L57
	add	x5, x3, 10510336
	mov	x1, 0
	strb	wzr, [x19]
	add	x5, x5, 1664
	mov	w6, 773
	stp	x3, x3, [x19, 8]
	mov	w4, 2
L55:
	and	w2, w1, 255
	mov	x0, x1
	cmp	w2, 5
	beq	L52
L58:
	add	x0, x19, x1, lsl 5
	add	x1, x1, 1
	add	x3, x19, x1, lsl 5
	strb	w2, [x0, 24]
	and	w2, w1, 255
	strb	wzr, [x0, 25]
	str	x5, [x0, 32]
	mov	x0, x1
	str	xzr, [x3, 8]
	strb	w4, [x3, 16]
	cmp	w2, 5
	bne	L58
L52:
	add	x3, x19, x1, lsl 5
	add	x1, x1, 1
	add	x2, x19, x1, lsl 5
	strh	w6, [x3, 24]
	str	x5, [x3, 32]
	str	xzr, [x2, 8]
	strb	w4, [x2, 16]
	cmp	x0, 5
	bne	L55
	mov	w3, 3
	mov	x2, 792
	str	xzr, [x19, 192]
	mov	w1, 0
	add	x0, x19, 216
	strb	w3, [x19, 185]
	bl	_memset
	add	x0, x19, 1024
	str	wzr, [x19, 1008]
	strb	wzr, [x19, 1012]
	stp	xzr, xzr, [x0, -8]
	str	wzr, [x19, 1032]
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI14:
	ret
L57:
LCFI15:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE11:
	.const
	.align	2
lC0:
	.word	1
	.word	50
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__update_phase
_anubis_governance__update_phase:
LFB13:
	mov	x3, 39359
	str	x1, [x0, 8]
	movk	x3, 0xf0, lsl 16
	ldrb	w2, [x0]
	cmp	x1, x3
	bhi	L77
	mov	x3, 26239
	movk	x3, 0xa0, lsl 16
	cmp	x1, x3
	bhi	L63
	mov	x3, 6559
	movk	x3, 0x28, lsl 16
	cmp	x1, x3
	bls	L78
	cmp	w2, 3
	bhi	L61
	cmp	w2, 1
	beq	L59
	mov	w1, 1
	strb	w1, [x0]
	strb	w1, [x0, 89]
L59:
	ret
	.p2align 2,,3
L78:
	cmp	w2, 3
	bhi	L61
	cbz	w2, L59
	strb	wzr, [x0]
	ret
	.p2align 2,,3
L77:
	cmp	w2, 3
	bhi	L61
	beq	L59
	mov	w2, 3
	mov	w1, 2
L76:
	strb	w2, [x0]
	strb	w1, [x0, 25]
	strb	w1, [x0, 57]
	strb	w1, [x0, 89]
	strb	w1, [x0, 121]
	strb	w1, [x0, 153]
	ret
	.p2align 2,,3
L63:
	cmp	w2, 3
	bhi	L61
	cmp	w2, 2
	beq	L59
	mov	w2, 2
	mov	w1, 1
	b	L76
L61:
	adrp	x0, lC12@PAGE
	stp	x29, x30, [sp, -16]!
LCFI16:
	mov	w1, 74
	mov	x29, sp
LCFI17:
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__get_current_phase
_anubis_governance__get_current_phase:
LFB14:
	ldrb	w0, [x0]
	cmp	w0, 3
	bhi	L84
	ret
L84:
	adrp	x0, lC12@PAGE
	stp	x29, x30, [sp, -16]!
LCFI18:
	mov	w1, 109
	mov	x29, sp
LCFI19:
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__calculate_phase
_anubis_governance__calculate_phase:
LFB15:
	mov	x2, 39359
	mov	x1, x0
	movk	x2, 0xf0, lsl 16
	mov	w0, 3
	cmp	x1, x2
	bhi	L86
	mov	x2, 26239
	mov	w0, 2
	movk	x2, 0xa0, lsl 16
	cmp	x1, x2
	bhi	L86
	mov	x0, 6559
	movk	x0, 0x28, lsl 16
	cmp	x1, x0
	cset	w0, hi
L86:
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__blocks_until_next_phase
_anubis_governance__blocks_until_next_phase:
LFB16:
	ldrb	w2, [x0]
	cmp	w2, 3
	bhi	L97
	mov	x1, x0
	mov	x0, 0
	beq	L89
	adrp	x0, _CSWTCH.178@PAGE
	ldr	x1, [x1, 8]
	add	x0, x0, _CSWTCH.178@PAGEOFF;
	ldr	x0, [x0, x2, lsl 3]
	cmp	x1, x0
	sub	x0, x0, x1
	csel	x0, x0, xzr, cc
L89:
	ret
L97:
	adrp	x0, lC12@PAGE
	stp	x29, x30, [sp, -16]!
LCFI20:
	mov	w1, 132
	mov	x29, sp
LCFI21:
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__has_privilege
_anubis_governance__has_privilege:
LFB17:
	stp	x29, x30, [sp, -16]!
LCFI22:
	mov	x29, sp
LCFI23:
	cmp	w1, 5
	bhi	L101
	bne	L100
	cmp	w2, 3
	bhi	L101
L105:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI24:
	ret
	.p2align 2,,3
L100:
LCFI25:
	ubfiz	x1, x1, 5, 8
	add	x3, x0, x1
	ldr	x3, [x3, 32]
	cbz	x3, L103
	ldr	x4, [x0, 8]
	cmp	x3, x4
	bls	L105
L103:
	add	x0, x0, x1
	ldrb	w0, [x0, 25]
	orr	w1, w2, w0
	cmp	w1, 3
	bhi	L110
	cmp	w2, w0
	cset	w0, eq
	ldp	x29, x30, [sp], 16
LCFI26:
	ret
L101:
LCFI27:
	adrp	x0, lC12@PAGE
	mov	w1, 161
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L110:
	adrp	x0, lC12@PAGE
	mov	w1, 170
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE17:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__get_privilege_holder
_anubis_governance__get_privilege_holder:
LFB18:
	cmp	w1, 5
	bhi	L113
	ubfiz	x1, x1, 5, 8
	add	x0, x0, x1
	ldrb	w0, [x0, 25]
	cmp	w0, 3
	bhi	L113
	ret
L113:
	adrp	x0, lC12@PAGE
	stp	x29, x30, [sp, -16]!
LCFI28:
	mov	w1, 178
	mov	x29, sp
LCFI29:
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE18:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__privilege_expired__2
_anubis_governance__privilege_expired__2:
LFB19:
	cmp	w1, 5
	bhi	L125
	mov	x2, x0
	mov	w0, 1
	beq	L118
	ubfiz	x1, x1, 5, 8
	mov	w0, 0
	add	x1, x2, x1
	ldr	x1, [x1, 32]
	cbz	x1, L118
	ldr	x0, [x2, 8]
	cmp	x0, x1
	cset	w0, cs
L118:
	ret
L125:
	adrp	x0, lC12@PAGE
	stp	x29, x30, [sp, -16]!
LCFI30:
	mov	w1, 187
	mov	x29, sp
LCFI31:
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE19:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__is_valid_parameter_change
_anubis_governance__is_valid_parameter_change:
LFB23:
	tbnz	w2, #31, L131
	sxtw	x2, w2
	mov	x3, 22859
	movk	x3, 0x3886, lsl 16
	mul	x2, x2, x0
	movk	x3, 0xc5d6, lsl 32
	movk	x3, 0x346d, lsl 48
	umulh	x2, x2, x3
	lsr	x2, x2, 11
	add	x3, x2, x0
	sub	x0, x0, x2
	cmp	x3, x1
	ccmp	x0, x1, 2, cs
	cset	w0, ls
	ret
L131:
	adrp	x0, lC12@PAGE
	stp	x29, x30, [sp, -16]!
LCFI32:
	mov	w1, 328
	mov	x29, sp
LCFI33:
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.const
	.align	3
lC14:
	.ascii "anubis_governance.ads"
	.space 1
	.align	3
lC15:
	.ascii "failed precondition from anubis_governance.ads:271"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__form_council
_anubis_governance__form_council:
LFB24:
	stp	x29, x30, [sp, -16]!
LCFI34:
	mov	x11, x0
	mov	x29, sp
LCFI35:
	ldrb	w0, [x0, 1012]
	cmp	w0, 1
	bhi	L148
	cbnz	w0, L149
	ldrb	w3, [x11]
	cmp	w3, 3
	bhi	L150
	cbz	w3, L137
	mov	x5, 0
	mov	x6, 0
	add	x8, x11, 216
	mov	w10, 0
	mov	w12, 2147483647
	.p2align 5,,15
L136:
	add	x4, x1, x5
	ldr	q27, [x1, x5]
	add	x3, x6, x6, lsl 2
	add	x7, x8, x5
	ldp	q29, q28, [x4, 16]
	add	x3, x6, x3, lsl 1
	ldp	q31, q30, [x4, 48]
	add	x3, x1, x3, lsl 3
	ldr	x9, [x4, 80]
	str	q27, [x8, x5]
	stp	q29, q28, [x7, 16]
	stp	q31, q30, [x7, 48]
	str	x9, [x7, 80]
	ldrb	w3, [x3, 80]
	cmp	w3, 1
	bhi	L151
	cbz	w3, L139
	cmp	w10, w12
	beq	L152
	add	w10, w10, 1
L139:
	add	x6, x6, 1
	add	x5, x5, 88
	cmp	x6, 9
	bne	L136
	cmp	w10, 4
	ble	L137
	mov	w1, 1
	mov	w0, 1
	str	x2, [x11, 8]
	str	w10, [x11, 1008]
	strb	w1, [x11, 1012]
L137:
	ldp	x29, x30, [sp], 16
LCFI36:
	ret
L151:
LCFI37:
	adrp	x0, lC12@PAGE
	mov	w1, 356
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L152:
	adrp	x0, lC12@PAGE
	mov	w1, 357
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L150:
	adrp	x0, lC12@PAGE
	mov	w1, 348
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L149:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L148:
	adrp	x0, lC14@PAGE
	mov	w1, 271
	add	x0, x0, lC14@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__elect_council_member
_anubis_governance__elect_council_member:
LFB26:
	stp	x29, x30, [sp, -16]!
LCFI38:
	mov	x29, sp
LCFI39:
	cmp	w1, 8
	bhi	L176
	sxtw	x3, w1
	add	x4, x3, w1, sxtw 2
	mov	w5, 0
	ldp	q27, q29, [x2]
	add	x4, x3, x4, lsl 1
	mov	x1, 0
	mov	w3, 0
	mov	w6, 2147483647
	ldp	q28, q31, [x2, 32]
	add	x4, x0, x4, lsl 3
	ldr	q30, [x2, 64]
	add	x7, x4, 216
	ldr	x2, [x2, 80]
	str	q27, [x4, 216]
	stp	q29, q28, [x7, 16]
	stp	q31, q30, [x7, 48]
	str	x2, [x4, 296]
	str	wzr, [x0, 1008]
	.p2align 5,,15
L160:
	add	x2, x1, x1, lsl 2
	add	x2, x1, x2, lsl 1
	add	x2, x0, x2, lsl 3
	ldrb	w2, [x2, 296]
	cmp	w2, 1
	bhi	L177
	cbz	w2, L157
	cmp	w3, w6
	beq	L178
	add	w3, w3, 1
	mov	w5, 1
L157:
	add	x1, x1, 1
	cmp	x1, 9
	bne	L160
	cbz	w5, L161
	str	w3, [x0, 1008]
L161:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI40:
	ret
L177:
LCFI41:
	cbz	w5, L156
	str	w3, [x0, 1008]
L156:
	adrp	x0, lC12@PAGE
	mov	w1, 391
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L178:
	cbz	w5, L159
	str	w3, [x0, 1008]
L159:
	adrp	x0, lC12@PAGE
	mov	w1, 392
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L176:
	adrp	x0, lC12@PAGE
	mov	w1, 386
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.const
	.align	3
lC16:
	.ascii "failed precondition from anubis_governance.ads:292"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__remove_council_member
_anubis_governance__remove_council_member:
LFB27:
	stp	x29, x30, [sp, -16]!
LCFI42:
	mov	x29, sp
LCFI43:
	ldpsw	x2, x3, [x3]
	add	x2, x2, 31
	cmp	x2, x3
	bne	L203
	cmp	w1, 8
	bhi	L204
	sxtw	x3, w1
	add	x2, x3, w1, sxtw 2
	mov	w4, 0
	add	x2, x3, x2, lsl 1
	mov	x1, 0
	mov	w3, 0
	mov	w5, 2147483647
	add	x2, x0, x2, lsl 3
	strb	wzr, [x2, 296]
	str	wzr, [x0, 1008]
	.p2align 5,,15
L187:
	add	x2, x1, x1, lsl 2
	add	x2, x1, x2, lsl 1
	add	x2, x0, x2, lsl 3
	ldrb	w2, [x2, 296]
	cmp	w2, 1
	bhi	L205
	cbz	w2, L184
	cmp	w3, w5
	beq	L206
	add	w3, w3, 1
	mov	w4, 1
L184:
	add	x1, x1, 1
	cmp	x1, 9
	bne	L187
	cbz	w4, L188
	str	w3, [x0, 1008]
L188:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI44:
	ret
L205:
LCFI45:
	cbz	w4, L183
	str	w3, [x0, 1008]
L183:
	adrp	x0, lC12@PAGE
	mov	w1, 413
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L206:
	cbz	w4, L186
	str	w3, [x0, 1008]
L186:
	adrp	x0, lC12@PAGE
	mov	w1, 414
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L204:
	adrp	x0, lC12@PAGE
	mov	w1, 408
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L203:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE27:
	.const
	.align	3
lC17:
	.ascii "failed precondition from anubis_governance.ads:301"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__verify_council_signatures
_anubis_governance__verify_council_signatures:
LFB28:
	stp	x29, x30, [sp, -16]!
LCFI46:
	mov	x29, sp
LCFI47:
	ldpsw	x1, x2, [x2]
	add	x1, x1, 31
	cmp	x1, x2
	bne	L218
	mov	x1, 0
	mov	w3, 0
	mov	w4, 2147483647
	.p2align 5,,15
L208:
	add	x2, x1, x1, lsl 2
	add	x2, x1, x2, lsl 1
	add	x2, x0, x2, lsl 3
	ldrb	w2, [x2, 296]
	cmp	w2, 1
	bhi	L219
	cbz	w2, L210
	cmp	w3, w4
	beq	L220
	add	w3, w3, 1
L210:
	add	x1, x1, 1
	cmp	x1, 9
	bne	L208
	cmp	w3, 4
	cset	w0, gt
	ldp	x29, x30, [sp], 16
LCFI48:
	ret
L219:
LCFI49:
	adrp	x0, lC12@PAGE
	mov	w1, 432
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L220:
	adrp	x0, lC12@PAGE
	mov	w1, 433
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L218:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE28:
	.const
	.align	3
lC18:
	.ascii "failed precondition from anubis_governance.ads:235"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__execute_pause
_anubis_governance__execute_pause:
LFB21:
	stp	x29, x30, [sp, -32]!
LCFI50:
	mov	x29, sp
LCFI51:
	ldpsw	x8, x4, [x2]
	str	x19, [sp, 16]
LCFI52:
	ldpsw	x1, x2, [x5]
	add	x8, x8, 31
	cmp	x8, x4
	add	x1, x1, 31
	ccmp	x1, x2, 0, eq
	bne	L229
	ldrb	w1, [x0]
	mov	x19, x0
	cmp	w1, 3
	bhi	L230
	cbnz	w1, L224
	mov	x1, 43200
	mov	w0, 6
	cmp	x3, x1
	bhi	L225
L226:
	ldr	w1, [x19, 1032]
	mov	w0, 0
	add	w1, w1, 1
	str	w1, [x19, 1032]
L225:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI53:
	ret
	.p2align 2,,3
L224:
LCFI54:
	adrp	x1, lC2@PAGE
	adrp	x2, lC3@PAGE
	mov	x3, x6
	mov	x4, x7
	add	x1, x1, lC2@PAGEOFF;
	add	x2, x2, lC3@PAGEOFF;
	bl	_anubis_governance__verify_council_signatures
	tbnz	x0, 0, L226
	mov	w0, 3
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI55:
	ret
L229:
LCFI56:
	adrp	x0, lC18@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L230:
	adrp	x0, lC12@PAGE
	mov	w1, 256
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE21:
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
	.const
	.align	3
lC19:
	.ascii "failed precondition from anubis_governance.ads:249"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__tune_parameter
_anubis_governance__tune_parameter:
LFB22:
	stp	x29, x30, [sp, -16]!
LCFI57:
	mov	x29, sp
LCFI58:
	ldpsw	x8, x3, [x2]
	add	x8, x8, 31
	cmp	x8, x3
	bne	L241
	ldrb	w2, [x0]
	cmp	w2, 3
	bhi	L242
	cbnz	w2, L234
	add	x0, x4, x4, lsl 2
	mov	x2, 22859
	movk	x2, 0x3886, lsl 16
	add	x0, x0, x0, lsl 2
	movk	x2, 0xc5d6, lsl 32
	movk	x2, 0x346d, lsl 48
	add	x0, x0, x0, lsl 2
	lsl	x0, x0, 4
	umulh	x0, x0, x2
	lsr	x0, x0, 11
	add	x2, x4, x0
	sub	x1, x4, x0
	cmp	x5, x2
	mov	w0, 6
	ccmp	x5, x1, 0, ls
	bcc	L236
L235:
	mov	w0, 0
L236:
	ldp	x29, x30, [sp], 16
LCFI59:
	ret
	.p2align 2,,3
L234:
LCFI60:
	adrp	x1, lC2@PAGE
	adrp	x2, lC3@PAGE
	add	x1, x1, lC2@PAGEOFF;
	mov	x3, x6
	mov	x4, x7
	add	x2, x2, lC3@PAGEOFF;
	bl	_anubis_governance__verify_council_signatures
	mov	w1, w0
	mov	w0, 3
	tbnz	x1, 0, L235
	ldp	x29, x30, [sp], 16
LCFI61:
	ret
L242:
LCFI62:
	adrp	x0, lC12@PAGE
	mov	w1, 294
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L241:
	adrp	x0, lC19@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE22:
	.const
	.align	3
lC20:
	.ascii "failed precondition from anubis_governance.ads:222"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__execute_upgrade
_anubis_governance__execute_upgrade:
LFB20:
	stp	x29, x30, [sp, -32]!
LCFI63:
	mov	x1, x4
	mov	x29, sp
LCFI64:
	ldrsw	x3, [x4, 4]
	ldpsw	x7, x4, [x2]
	ldrsw	x2, [x1]
	str	x19, [sp, 16]
LCFI65:
	add	x7, x7, 31
	cmp	x7, x4
	add	x1, x2, 31
	ccmp	x1, x3, 0, eq
	bne	L250
	ldr	x1, [x0, 1016]
	mov	x19, x0
	ldrb	w2, [x0]
	add	x1, x1, 1
	str	x1, [x0, 1016]
	cmp	w2, 3
	bhi	L251
	cmp	w2, 1
	beq	L246
L248:
	ldr	x1, [x19, 1024]
	mov	w0, 0
	add	x1, x1, 1
	str	x1, [x19, 1024]
L247:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI66:
	ret
	.p2align 2,,3
L246:
LCFI67:
	adrp	x1, lC2@PAGE
	adrp	x2, lC3@PAGE
	mov	x3, x5
	mov	x4, x6
	add	x1, x1, lC2@PAGEOFF;
	add	x2, x2, lC3@PAGEOFF;
	bl	_anubis_governance__verify_council_signatures
	tbnz	x0, 0, L248
	mov	w0, 3
	b	L247
L250:
	adrp	x0, lC20@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L251:
	adrp	x0, lC12@PAGE
	mov	w1, 216
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__process_handoffs
_anubis_governance__process_handoffs:
LFB29:
	stp	x29, x30, [sp, -32]!
LCFI68:
	mov	x29, sp
LCFI69:
	stp	x19, x20, [sp, 16]
LCFI70:
	mov	x19, x0
	mov	x20, x1
	bl	_anubis_governance__update_phase
	mov	x0, 6559
	movk	x0, 0x28, lsl 16
	cmp	x20, x0
	bls	L252
	ldrb	w0, [x19, 89]
	cmp	w0, 3
	bhi	L271
	cbnz	w0, L255
	mov	w0, 1
	str	x20, [x19, 104]
	strb	w0, [x19, 89]
L255:
	mov	x0, 13119
	movk	x0, 0x50, lsl 16
	cmp	x20, x0
	bls	L252
	ldrb	w0, [x19, 25]
	cmp	w0, 3
	bhi	L272
	cbnz	w0, L257
	mov	w0, 1
	str	x20, [x19, 40]
	strb	w0, [x19, 25]
L257:
	mov	x0, 19679
	movk	x0, 0x78, lsl 16
	cmp	x20, x0
	bls	L252
	ldrb	w0, [x19, 57]
	cmp	w0, 3
	bhi	L273
	cbnz	w0, L259
	mov	w0, 1
	str	x20, [x19, 72]
	strb	w0, [x19, 57]
L259:
	mov	x0, 26239
	movk	x0, 0xa0, lsl 16
	cmp	x20, x0
	bls	L252
	mov	x0, 0
	mov	w3, 2
L263:
	and	w1, w0, 255
	cmp	w1, 5
	beq	L260
L274:
	add	x2, x19, x0, lsl 5
	ldrb	w1, [x2, 25]
	cmp	w1, 3
	bhi	L261
	cbnz	w1, L270
	add	x0, x0, 1
	strb	w3, [x2, 25]
	add	x1, x19, x0, lsl 5
	str	x20, [x1, 8]
	and	w1, w0, 255
	cmp	w1, 5
	bne	L274
L260:
	cmp	x0, 5
	bne	L270
L252:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI71:
	ret
	.p2align 2,,3
L270:
LCFI72:
	add	x0, x0, 1
	b	L263
L271:
	adrp	x0, lC12@PAGE
	mov	w1, 453
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L261:
	adrp	x0, lC12@PAGE
	mov	w1, 476
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L272:
	adrp	x0, lC12@PAGE
	mov	w1, 460
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L273:
	adrp	x0, lC12@PAGE
	mov	w1, 467
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE29:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__handoff_due
_anubis_governance__handoff_due:
LFB30:
	ldr	x0, [x0, 8]
	mov	x2, 6559
	movk	x2, 0x28, lsl 16
	cmp	x0, x2
	bls	L281
	mov	x2, 13119
	movk	x2, 0x50, lsl 16
	cmp	x0, x2
	bls	L281
	mov	x2, 19679
	movk	x2, 0x78, lsl 16
	cmp	x0, x2
	bls	L281
	mov	x2, 26239
	movk	x2, 0xa0, lsl 16
	cmp	x0, x2
	bls	L281
	mov	x2, 39359
	movk	x2, 0xf0, lsl 16
	cmp	x0, x2
	ccmp	x1, x2, 2, ls
	cset	w0, hi
	ret
	.p2align 2,,3
L281:
	cmp	x1, x2
	cset	w0, hi
	ret
LFE30:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__next_handoff_block
_anubis_governance__next_handoff_block:
LFB31:
	mov	x2, 6559
	mov	x1, x0
	movk	x2, 0x28, lsl 16
	add	x0, x2, 1
	cmp	x1, x2
	bls	L282
	mov	x2, 13119
	movk	x2, 0x50, lsl 16
	add	x0, x2, 1
	cmp	x1, x2
	bls	L282
	mov	x0, 19679
	movk	x0, 0x78, lsl 16
	cmp	x1, x0
	bls	L286
	mov	x0, 26239
	movk	x0, 0xa0, lsl 16
	cmp	x1, x0
	bls	L287
	mov	x2, 39359
	mov	x0, 39360
	movk	x2, 0xf0, lsl 16
	movk	x0, 0xf0, lsl 16
	cmp	x1, x2
	csel	x0, xzr, x0, hi
L282:
	ret
	.p2align 2,,3
L286:
	mov	x0, 19680
	movk	x0, 0x78, lsl 16
	ret
	.p2align 2,,3
L287:
	mov	x0, 26240
	movk	x0, 0xa0, lsl 16
	ret
LFE31:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__requires_dao_vote
_anubis_governance__requires_dao_vote:
LFB32:
	stp	x29, x30, [sp, -16]!
LCFI73:
	mov	x29, sp
LCFI74:
	ldrb	w2, [x0]
	cmp	w2, 3
	bhi	L296
	sub	w2, w2, #2
	mov	x3, x0
	and	w2, w2, 255
	mov	w0, 1
	cmp	w2, w0
	bhi	L297
	ldp	x29, x30, [sp], 16
LCFI75:
	ret
	.p2align 2,,3
L297:
LCFI76:
	cmp	w1, 5
	bhi	L293
	ubfiz	x1, x1, 5, 8
	add	x3, x3, x1
	ldrb	w0, [x3, 25]
	cmp	w0, 3
	bhi	L293
	cmp	w0, 2
	cset	w0, eq
	ldp	x29, x30, [sp], 16
LCFI77:
	ret
L296:
LCFI78:
	adrp	x0, lC12@PAGE
	mov	w1, 521
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L293:
	adrp	x0, lC12@PAGE
	mov	w1, 523
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE32:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__verify_dao_vote
_anubis_governance__verify_dao_vote:
LFB33:
	mov	w0, 1
	ret
LFE33:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__builder_status
_anubis_governance__builder_status:
LFB34:
	stp	x29, x30, [sp, -16]!
LCFI79:
	mov	x29, sp
LCFI80:
	ldrb	w0, [x0]
	cmp	w0, 3
	bhi	L307
	mov	x1, 4
	cmp	w0, 1
	beq	L301
	cmp	w0, 2
	beq	L302
	cbnz	w0, L303
	mov	x0, 32
	bl	_system__secondary_stack__ss_allocate
	adrp	x3, lC4@PAGE
	add	x3, x3, lC4@PAGEOFF;
L306:
	ldp	q31, q30, [x3]
	mov	x2, x0
	add	x0, x0, 8
	mov	x1, x2
	stp	q31, q30, [x2]
	ldp	x29, x30, [sp], 16
LCFI81:
	ret
	.p2align 2,,3
L302:
LCFI82:
	mov	x0, 48
	bl	_system__secondary_stack__ss_allocate
	adrp	x3, lC6@PAGE
	mov	x2, x0
	add	x3, x3, lC6@PAGEOFF;
	add	x0, x0, 8
	mov	x1, x2
	ldp	q30, q29, [x3]
	ldr	q31, [x3, 32]
	str	q31, [x2, 32]
	stp	q30, q29, [x2]
	ldp	x29, x30, [sp], 16
LCFI83:
	ret
	.p2align 2,,3
L301:
LCFI84:
	mov	x0, 36
	bl	_system__secondary_stack__ss_allocate
	adrp	x3, lC5@PAGE
	mov	x2, x0
	add	x3, x3, lC5@PAGEOFF;
	add	x0, x0, 8
	ldr	w4, [x3, 32]
	mov	x1, x2
	ldp	q31, q30, [x3]
	str	w4, [x2, 32]
	stp	q31, q30, [x2]
	ldp	x29, x30, [sp], 16
LCFI85:
	ret
	.p2align 2,,3
L303:
LCFI86:
	mov	x0, 32
	bl	_system__secondary_stack__ss_allocate
	adrp	x3, lC7@PAGE
	add	x3, x3, lC7@PAGEOFF;
	b	L306
L307:
	adrp	x0, lC12@PAGE
	mov	w1, 546
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE34:
	.const
	.align	2
lC4:
	.word	1
	.word	24
	.ascii "Active - Limited Control"
	.align	2
lC6:
	.word	1
	.word	37
	.ascii "Limited - Most Privileges Transferred"
	.space 3
	.align	2
lC5:
	.word	1
	.word	28
	.ascii "Active - Shared with Council"
	.align	2
lC7:
	.word	1
	.word	24
	.ascii "Expired - DAO Autonomous"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__builder_has_privileges
_anubis_governance__builder_has_privileges:
LFB35:
	mov	x1, 0
	.p2align 5,,15
L311:
	add	x2, x0, x1, lsl 5
	ldrb	w2, [x2, 25]
	cmp	w2, 3
	bhi	L317
	cbz	w2, L312
	add	x1, x1, 1
	cmp	x1, 6
	bne	L311
	mov	w0, 0
	ret
	.p2align 2,,3
L312:
	mov	w0, 1
	ret
L317:
	adrp	x0, lC12@PAGE
	stp	x29, x30, [sp, -16]!
LCFI87:
	mov	w1, 563
	mov	x29, sp
LCFI88:
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE35:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__phase_description
_anubis_governance__phase_description:
LFB36:
	stp	x29, x30, [sp, -16]!
LCFI89:
	mov	x29, sp
LCFI90:
	cmp	w0, 3
	bhi	L327
	mov	x1, 4
	cmp	w0, 1
	beq	L320
	cmp	w0, 2
	beq	L321
	cbnz	w0, L322
	mov	x0, 36
	bl	_system__secondary_stack__ss_allocate
	adrp	x3, lC8@PAGE
	add	x3, x3, lC8@PAGEOFF;
L325:
	ldp	q31, q30, [x3]
	mov	x2, x0
	add	x0, x0, 8
	mov	x1, x2
	ldr	w4, [x3, 32]
	str	w4, [x2, 32]
	stp	q31, q30, [x2]
	ldp	x29, x30, [sp], 16
LCFI91:
	ret
	.p2align 2,,3
L321:
LCFI92:
	mov	x0, 32
	bl	_system__secondary_stack__ss_allocate
	adrp	x3, lC10@PAGE
	add	x3, x3, lC10@PAGEOFF;
L326:
	ldp	q31, q30, [x3]
	mov	x2, x0
	add	x0, x0, 8
	mov	x1, x2
	stp	q31, q30, [x2]
	ldp	x29, x30, [sp], 16
LCFI93:
	ret
	.p2align 2,,3
L320:
LCFI94:
	mov	x0, 36
	bl	_system__secondary_stack__ss_allocate
	adrp	x3, lC9@PAGE
	add	x3, x3, lC9@PAGEOFF;
	b	L325
	.p2align 2,,3
L322:
	mov	x0, 32
	bl	_system__secondary_stack__ss_allocate
	adrp	x3, lC11@PAGE
	add	x3, x3, lC11@PAGEOFF;
	b	L326
L327:
	adrp	x0, lC12@PAGE
	mov	w1, 575
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE36:
	.const
	.align	2
lC8:
	.word	1
	.word	26
	.ascii "Builder Control (Year 0-1)"
	.space 2
	.align	2
lC10:
	.word	1
	.word	22
	.ascii "DAO Control (Year 2-3)"
	.space 2
	.align	2
lC9:
	.word	1
	.word	25
	.ascii "Shared Control (Year 1-2)"
	.space 3
	.align	2
lC11:
	.word	1
	.word	23
	.ascii "Full Autonomy (Year 3+)"
	.space 1
	.text
	.const
	.align	3
lC21:
	.ascii "failed precondition from anubis_governance.ads:378"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__serialize_governance_state
_anubis_governance__serialize_governance_state:
LFB37:
	stp	x29, x30, [sp, -96]!
LCFI95:
	mov	x29, sp
LCFI96:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
LCFI97:
	mov	x19, x0
	add	x0, x29, 96
	stp	x21, x22, [sp, 32]
LCFI98:
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w20, w3, [x2]
	str	x0, [x29, 88]
	sxtw	x21, w20
	sxtw	x2, w3
	add	x0, x21, 2046
	str	x21, [x29, 64]
	cmp	x0, x2
	bge	L355
	tbnz	w20, #31, L356
	ldr	x0, [x29, 48]
	mov	w1, 0
	cmp	w20, w3
	sub	x2, x2, x21
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldr	x0, [x29, 56]
	ldr	w1, [x0, 4]
	cmp	w20, w1
	bgt	L332
	ldr	w0, [x0]
	cmp	w20, w0
	blt	L357
	ldrb	w0, [x19]
	cmp	w0, 3
	bhi	L358
	ldr	x1, [x29, 48]
	add	w20, w20, 1
	strb	w0, [x1]
L332:
	add	x22, x29, 64
	ldr	x0, [x19, 8]
	mov	x16, x22
	str	w20, [x29, 80]
	bl	_anubis_governance__serialize_governance_state__write_u64.2
	ldr	x0, [x19, 16]
	mov	x16, x22
	bl	_anubis_governance__serialize_governance_state__write_u64.2
	ldr	w1, [x29, 80]
	tbnz	w1, #31, L359
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L336
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L360
	ldrb	w3, [x19, 1012]
	cmp	w3, 1
	bhi	L361
	ldr	x5, [x29, 48]
	sxtw	x0, w1
	mov	w4, 2147483647
	sub	x0, x0, x21
	strb	w3, [x5, x0]
	cmp	w1, w4
	beq	L362
	add	w0, w1, 1
	cmp	w0, w2
	bgt	L352
	ldr	w3, [x19, 1008]
	tbnz	w3, #31, L363
	cmp	w3, 255
	bhi	L364
	sxtw	x2, w0
	add	w1, w1, 2
	sub	x2, x2, x21
	strb	w3, [x5, x2]
	cmp	w0, w4
	beq	L365
L336:
	ldr	x0, [x19, 1016]
	mov	x16, x22
	str	w1, [x29, 80]
	bl	_anubis_governance__serialize_governance_state__write_u64.2
	ldr	x0, [x19, 1024]
	mov	x16, x22
	bl	_anubis_governance__serialize_governance_state__write_u64.2
	ldr	w1, [x29, 80]
	tbnz	w1, #31, L366
	ldr	x0, [x29, 56]
	ldp	w0, w2, [x0]
	cmp	w1, w2
	bgt	L344
	cmp	w1, w0
	blt	L367
	ldr	x4, [x29, 48]
	sxtw	x2, w1
	mov	w3, 2147483647
	sub	x2, x2, x21
	ldr	w5, [x19, 1032]
	strb	w5, [x4, x2]
	cmp	w1, w3
	beq	L368
	add	w1, w1, 1
L344:
	subs	w0, w1, w0
	bvs	L348
	tbnz	w0, #31, L369
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI99:
	ret
L365:
LCFI100:
	adrp	x0, lC12@PAGE
	mov	w1, 630
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L348:
	adrp	x0, lC12@PAGE
	mov	w1, 641
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L369:
	adrp	x0, lC12@PAGE
	mov	w1, 641
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L356:
	adrp	x0, lC12@PAGE
	mov	w1, 596
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L355:
	adrp	x0, lC21@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC21@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L359:
	adrp	x0, lC12@PAGE
	mov	w1, 623
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L366:
	adrp	x0, lC12@PAGE
	mov	w1, 636
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L363:
	adrp	x0, lC12@PAGE
	mov	w1, 629
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L364:
	adrp	x0, lC12@PAGE
	mov	w1, 629
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L358:
	adrp	x0, lC12@PAGE
	mov	w1, 615
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L357:
	adrp	x0, lC12@PAGE
	mov	w1, 615
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L360:
	adrp	x0, lC12@PAGE
	mov	w1, 624
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L361:
	adrp	x0, lC12@PAGE
	mov	w1, 624
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L362:
	adrp	x0, lC12@PAGE
	mov	w1, 625
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L367:
	adrp	x0, lC12@PAGE
	mov	w1, 637
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L368:
	adrp	x0, lC12@PAGE
	mov	w1, 638
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L352:
	mov	w1, w0
	b	L336
LFE37:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__deserialize_governance_state
_anubis_governance__deserialize_governance_state:
LFB39:
	stp	x29, x30, [sp, -96]!
LCFI101:
	mov	x29, sp
LCFI102:
	add	x3, x29, 48
	stp	x0, x1, [x29, 48]
	add	x0, x29, 96
	stp	x19, x20, [sp, 16]
LCFI103:
	ldr	w20, [x1]
	str	x21, [sp, 32]
LCFI104:
	str	x3, [x29, 72]
	str	x0, [x29, 88]
	tbnz	w20, #31, L389
	mov	x19, x2
	adrp	x1, lC2@PAGE
	adrp	x2, lC3@PAGE
	add	x1, x1, lC2@PAGEOFF;
	add	x2, x2, lC3@PAGEOFF;
	mov	x0, x19
	mov	x3, 0
	bl	_anubis_governance__init_governance
	ldr	x0, [x29, 56]
	ldp	w2, w1, [x0]
	sxtw	x0, w2
	add	x0, x0, 38
	cmp	x0, w1, sxtw
	bge	L386
	sxtw	x0, w20
	cmp	w20, w1
	bgt	L373
	cmp	w20, w2
	blt	L390
	ldr	x1, [x29, 48]
	ldrb	w1, [x1]
	cmp	w1, 3
	bls	L391
L375:
	mov	w1, 2147483647
	cmp	w20, w1
	beq	L392
	add	w20, w20, 1
L373:
	add	x21, x29, 64
	str	x0, [x29, 64]
	mov	x16, x21
	str	w20, [x29, 80]
	bl	_anubis_governance__deserialize_governance_state__read_u64.3
	mov	x16, x21
	str	x0, [x19, 8]
	bl	_anubis_governance__deserialize_governance_state__read_u64.3
	ldr	w1, [x29, 80]
	str	x0, [x19, 16]
	ldr	x3, [x29, 64]
	tbnz	w1, #31, L393
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L378
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L394
	ldr	x5, [x29, 48]
	sxtw	x0, w1
	mov	w4, 2147483647
	sub	x0, x0, x3
	ldrb	w0, [x5, x0]
	cmp	w0, 0
	cset	w0, ne
	strb	w0, [x19, 1012]
	cmp	w1, w4
	beq	L395
	add	w0, w1, 1
	cmp	w0, w2
	bgt	L387
	sxtw	x2, w0
	add	w1, w1, 2
	sub	x2, x2, x3
	ldrb	w2, [x5, x2]
	str	w2, [x19, 1008]
	cmp	w0, w4
	beq	L396
L378:
	mov	x16, x21
	str	w1, [x29, 80]
	bl	_anubis_governance__deserialize_governance_state__read_u64.3
	mov	x16, x21
	str	x0, [x19, 1016]
	bl	_anubis_governance__deserialize_governance_state__read_u64.3
	ldr	w1, [x29, 80]
	str	x0, [x19, 1024]
	ldr	x3, [x29, 64]
	tbnz	w1, #31, L397
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L385
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L398
	ldr	x4, [x29, 48]
	sxtw	x0, w1
	mov	w2, 2147483647
	sub	x0, x0, x3
	ldrb	w0, [x4, x0]
	str	w0, [x19, 1032]
	cmp	w1, w2
	beq	L399
L385:
	mov	w0, 1
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI105:
	ret
	.p2align 2,,3
L386:
LCFI106:
	mov	w0, 0
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI107:
	ret
	.p2align 2,,3
L391:
LCFI108:
	strb	w1, [x19]
	b	L375
L399:
	adrp	x0, lC12@PAGE
	mov	w1, 699
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L389:
	adrp	x0, lC12@PAGE
	mov	w1, 649
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L397:
	adrp	x0, lC12@PAGE
	mov	w1, 697
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L393:
	adrp	x0, lC12@PAGE
	mov	w1, 684
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L392:
	adrp	x0, lC12@PAGE
	mov	w1, 677
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L396:
	adrp	x0, lC12@PAGE
	mov	w1, 691
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L398:
	adrp	x0, lC12@PAGE
	mov	w1, 698
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L390:
	adrp	x0, lC12@PAGE
	mov	w1, 672
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L394:
	adrp	x0, lC12@PAGE
	mov	w1, 685
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L395:
	adrp	x0, lC12@PAGE
	mov	w1, 686
	add	x0, x0, lC12@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L387:
	mov	w1, w0
	b	L378
LFE39:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__zeroize_governance_state
_anubis_governance__zeroize_governance_state:
LFB41:
	movi	v31.4s, 0
	mov	x1, 0
	mov	w4, 3
	add	x7, x0, 32
	strb	wzr, [x0]
	str	q31, [x0, 8]
	.p2align 5,,15
L401:
	lsl	x2, x1, 5
	mov	x5, x1
	add	x3, x0, x2
	strb	w1, [x3, 24]
	add	x1, x1, 1
	add	x6, x0, x1, lsl 5
	strb	w4, [x3, 25]
	str	q31, [x7, x2]
	strb	w4, [x6, 16]
	cmp	x5, 5
	bne	L401
	movi	v31.4s, 0
	mov	x2, 0
	add	x3, x0, 280
	.p2align 5,,15
L402:
	add	x1, x2, x2, lsl 2
	add	x1, x2, x1, lsl 1
	add	x2, x2, 1
	add	x1, x0, x1, lsl 3
	add	x4, x1, 248
	stp	xzr, xzr, [x1, 216]
	stp	xzr, xzr, [x1, 232]
	stp	xzr, xzr, [x1, 248]
	stp	xzr, xzr, [x4, 16]
	str	q31, [x3], 88
	strb	wzr, [x1, 296]
	cmp	x2, 9
	bne	L402
	add	x1, x0, 1024
	str	wzr, [x0, 1008]
	strb	wzr, [x0, 1012]
	str	q31, [x1, -8]
	str	wzr, [x0, 1032]
	ret
LFE41:
	.align	2
	.p2align 5,,15
	.globl _anubis_governance__zeroize_council_member
_anubis_governance__zeroize_council_member:
LFB42:
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	stp	xzr, xzr, [x0, 32]
	stp	xzr, xzr, [x0, 48]
	stp	xzr, xzr, [x0, 64]
	strb	wzr, [x0, 80]
	ret
LFE42:
	.const
	.align	3
_CSWTCH.178:
	.xword	2628000
	.xword	10512000
	.xword	15768000
_action_resultG.4:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	1
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	2
	.align	3
_privilege_holderG.8:
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	2
	.byte	0
	.space 7
_privilege_typeG.12:
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	5
	.align	3
_governance_phaseG.16:
	.byte	2
	.byte	3
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.space 7
	.globl _anubis_governance__action_resultN
	.align	3
_anubis_governance__action_resultN:
	.byte	1
	.byte	9
	.byte	26
	.byte	43
	.byte	59
	.byte	76
	.byte	93
	.byte	107
	.globl _anubis_governance__action_resultS
	.align	3
_anubis_governance__action_resultS:
	.ascii "APPROVEDPHASE_NOT_ALLOWEDPRIVILEGE_EXPIREDCOUNCIL_REQUIREDDAO_VOTE_REQUIREDINVALID_SIGNATUREEXCEEDS_LIMITS"
	.globl _anubis_governance__privilege_holderN
	.align	3
_anubis_governance__privilege_holderN:
	.byte	1
	.byte	8
	.byte	15
	.byte	18
	.byte	22
	.space 3
	.globl _anubis_governance__privilege_holderS
	.align	3
_anubis_governance__privilege_holderS:
	.ascii "BUILDERCOUNCILDAONONE"
	.globl _anubis_governance__privilege_typeN
	.align	3
_anubis_governance__privilege_typeN:
	.byte	1
	.byte	17
	.byte	32
	.byte	48
	.byte	67
	.byte	84
	.byte	99
	.space 1
	.globl _anubis_governance__privilege_typeS
	.align	3
_anubis_governance__privilege_typeS:
	.ascii "PROTOCOL_UPGRADEEMERGENCY_PAUSEPARAMETER_TUNINGBUG_BOUNTY_APPROVALVALIDATOR_REMOVALTREASURY_ACCESS"
	.globl _anubis_governance__governance_phaseN
	.align	3
_anubis_governance__governance_phaseN:
	.byte	1
	.byte	23
	.byte	44
	.byte	62
	.byte	82
	.space 3
	.globl _anubis_governance__governance_phaseS
	.align	3
_anubis_governance__governance_phaseS:
	.ascii "PHASE1_BUILDER_CONTROLPHASE2_SHARED_CONTROLPHASE3_DAO_CONTROLPHASE4_FULL_AUTONOMY"
	.globl _anubis_governance_E
	.data
	.align	1
_anubis_governance_E:
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
	.quad	LFB38-.
	.set L$set$2,LFE38-LFB38
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB38
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
	.quad	LFB40-.
	.set L$set$8,LFE40-LFB40
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB40
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
	.quad	LFB96-.
	.set L$set$27,LFE96-LFB96
	.quad L$set$27
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$28,LEFDE17-LASFDE17
	.long L$set$28
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB8-.
	.set L$set$29,LFE8-LFB8
	.quad L$set$29
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$30,LEFDE19-LASFDE19
	.long L$set$30
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB98-.
	.set L$set$31,LFE98-LFB98
	.quad L$set$31
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$32,LEFDE21-LASFDE21
	.long L$set$32
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB10-.
	.set L$set$33,LFE10-LFB10
	.quad L$set$33
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$34,LEFDE23-LASFDE23
	.long L$set$34
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB11-.
	.set L$set$35,LFE11-LFB11
	.quad L$set$35
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI11-LFB11
	.long L$set$36
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$37,LCFI12-LCFI11
	.long L$set$37
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$38,LCFI13-LCFI12
	.long L$set$38
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$39,LCFI14-LCFI13
	.long L$set$39
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$40,LCFI15-LCFI14
	.long L$set$40
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$41,LEFDE25-LASFDE25
	.long L$set$41
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB13-.
	.set L$set$42,LFE13-LFB13
	.quad L$set$42
	.uleb128 0
	.byte	0x4
	.set L$set$43,LCFI16-LFB13
	.long L$set$43
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$44,LCFI17-LCFI16
	.long L$set$44
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$45,LEFDE27-LASFDE27
	.long L$set$45
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB14-.
	.set L$set$46,LFE14-LFB14
	.quad L$set$46
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI18-LFB14
	.long L$set$47
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$48,LCFI19-LCFI18
	.long L$set$48
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$49,LEFDE29-LASFDE29
	.long L$set$49
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB15-.
	.set L$set$50,LFE15-LFB15
	.quad L$set$50
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$51,LEFDE31-LASFDE31
	.long L$set$51
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB16-.
	.set L$set$52,LFE16-LFB16
	.quad L$set$52
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI20-LFB16
	.long L$set$53
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$54,LCFI21-LCFI20
	.long L$set$54
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$55,LEFDE33-LASFDE33
	.long L$set$55
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB17-.
	.set L$set$56,LFE17-LFB17
	.quad L$set$56
	.uleb128 0
	.byte	0x4
	.set L$set$57,LCFI22-LFB17
	.long L$set$57
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$58,LCFI23-LCFI22
	.long L$set$58
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$59,LCFI24-LCFI23
	.long L$set$59
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$60,LCFI25-LCFI24
	.long L$set$60
	.byte	0xb
	.byte	0x4
	.set L$set$61,LCFI26-LCFI25
	.long L$set$61
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$62,LCFI27-LCFI26
	.long L$set$62
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$63,LEFDE35-LASFDE35
	.long L$set$63
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB18-.
	.set L$set$64,LFE18-LFB18
	.quad L$set$64
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI28-LFB18
	.long L$set$65
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$66,LCFI29-LCFI28
	.long L$set$66
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$67,LEFDE37-LASFDE37
	.long L$set$67
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB19-.
	.set L$set$68,LFE19-LFB19
	.quad L$set$68
	.uleb128 0
	.byte	0x4
	.set L$set$69,LCFI30-LFB19
	.long L$set$69
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$70,LCFI31-LCFI30
	.long L$set$70
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$71,LEFDE39-LASFDE39
	.long L$set$71
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB23-.
	.set L$set$72,LFE23-LFB23
	.quad L$set$72
	.uleb128 0
	.byte	0x4
	.set L$set$73,LCFI32-LFB23
	.long L$set$73
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$74,LCFI33-LCFI32
	.long L$set$74
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$75,LEFDE41-LASFDE41
	.long L$set$75
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB24-.
	.set L$set$76,LFE24-LFB24
	.quad L$set$76
	.uleb128 0
	.byte	0x4
	.set L$set$77,LCFI34-LFB24
	.long L$set$77
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$78,LCFI35-LCFI34
	.long L$set$78
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$79,LCFI36-LCFI35
	.long L$set$79
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$80,LCFI37-LCFI36
	.long L$set$80
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$81,LEFDE43-LASFDE43
	.long L$set$81
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB26-.
	.set L$set$82,LFE26-LFB26
	.quad L$set$82
	.uleb128 0
	.byte	0x4
	.set L$set$83,LCFI38-LFB26
	.long L$set$83
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$84,LCFI39-LCFI38
	.long L$set$84
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$85,LCFI40-LCFI39
	.long L$set$85
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$86,LCFI41-LCFI40
	.long L$set$86
	.byte	0xb
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$87,LEFDE45-LASFDE45
	.long L$set$87
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB27-.
	.set L$set$88,LFE27-LFB27
	.quad L$set$88
	.uleb128 0
	.byte	0x4
	.set L$set$89,LCFI42-LFB27
	.long L$set$89
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$90,LCFI43-LCFI42
	.long L$set$90
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$91,LCFI44-LCFI43
	.long L$set$91
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$92,LCFI45-LCFI44
	.long L$set$92
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$93,LEFDE47-LASFDE47
	.long L$set$93
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB28-.
	.set L$set$94,LFE28-LFB28
	.quad L$set$94
	.uleb128 0
	.byte	0x4
	.set L$set$95,LCFI46-LFB28
	.long L$set$95
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$96,LCFI47-LCFI46
	.long L$set$96
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$97,LCFI48-LCFI47
	.long L$set$97
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$98,LCFI49-LCFI48
	.long L$set$98
	.byte	0xb
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$99,LEFDE49-LASFDE49
	.long L$set$99
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB21-.
	.set L$set$100,LFE21-LFB21
	.quad L$set$100
	.uleb128 0
	.byte	0x4
	.set L$set$101,LCFI50-LFB21
	.long L$set$101
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$102,LCFI51-LCFI50
	.long L$set$102
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$103,LCFI52-LCFI51
	.long L$set$103
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$104,LCFI53-LCFI52
	.long L$set$104
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$105,LCFI54-LCFI53
	.long L$set$105
	.byte	0xb
	.byte	0x4
	.set L$set$106,LCFI55-LCFI54
	.long L$set$106
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
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
	.quad	LFB22-.
	.set L$set$109,LFE22-LFB22
	.quad L$set$109
	.uleb128 0
	.byte	0x4
	.set L$set$110,LCFI57-LFB22
	.long L$set$110
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
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
	.quad	LFB20-.
	.set L$set$117,LFE20-LFB20
	.quad L$set$117
	.uleb128 0
	.byte	0x4
	.set L$set$118,LCFI63-LFB20
	.long L$set$118
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$119,LCFI64-LCFI63
	.long L$set$119
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$120,LCFI65-LCFI64
	.long L$set$120
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$121,LCFI66-LCFI65
	.long L$set$121
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$122,LCFI67-LCFI66
	.long L$set$122
	.byte	0xb
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$123,LEFDE55-LASFDE55
	.long L$set$123
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB29-.
	.set L$set$124,LFE29-LFB29
	.quad L$set$124
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI68-LFB29
	.long L$set$125
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$126,LCFI69-LCFI68
	.long L$set$126
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$127,LCFI70-LCFI69
	.long L$set$127
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$128,LCFI71-LCFI70
	.long L$set$128
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$129,LCFI72-LCFI71
	.long L$set$129
	.byte	0xb
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$130,LEFDE57-LASFDE57
	.long L$set$130
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB30-.
	.set L$set$131,LFE30-LFB30
	.quad L$set$131
	.uleb128 0
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$132,LEFDE59-LASFDE59
	.long L$set$132
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB31-.
	.set L$set$133,LFE31-LFB31
	.quad L$set$133
	.uleb128 0
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$134,LEFDE61-LASFDE61
	.long L$set$134
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB32-.
	.set L$set$135,LFE32-LFB32
	.quad L$set$135
	.uleb128 0
	.byte	0x4
	.set L$set$136,LCFI73-LFB32
	.long L$set$136
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$137,LCFI74-LCFI73
	.long L$set$137
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$138,LCFI75-LCFI74
	.long L$set$138
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$139,LCFI76-LCFI75
	.long L$set$139
	.byte	0xb
	.byte	0x4
	.set L$set$140,LCFI77-LCFI76
	.long L$set$140
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$141,LCFI78-LCFI77
	.long L$set$141
	.byte	0xb
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$142,LEFDE63-LASFDE63
	.long L$set$142
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB33-.
	.set L$set$143,LFE33-LFB33
	.quad L$set$143
	.uleb128 0
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$144,LEFDE65-LASFDE65
	.long L$set$144
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB34-.
	.set L$set$145,LFE34-LFB34
	.quad L$set$145
	.uleb128 0
	.byte	0x4
	.set L$set$146,LCFI79-LFB34
	.long L$set$146
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$147,LCFI80-LCFI79
	.long L$set$147
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$148,LCFI81-LCFI80
	.long L$set$148
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$149,LCFI82-LCFI81
	.long L$set$149
	.byte	0xb
	.byte	0x4
	.set L$set$150,LCFI83-LCFI82
	.long L$set$150
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$151,LCFI84-LCFI83
	.long L$set$151
	.byte	0xb
	.byte	0x4
	.set L$set$152,LCFI85-LCFI84
	.long L$set$152
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$153,LCFI86-LCFI85
	.long L$set$153
	.byte	0xb
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$154,LEFDE67-LASFDE67
	.long L$set$154
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB35-.
	.set L$set$155,LFE35-LFB35
	.quad L$set$155
	.uleb128 0
	.byte	0x4
	.set L$set$156,LCFI87-LFB35
	.long L$set$156
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$157,LCFI88-LCFI87
	.long L$set$157
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$158,LEFDE69-LASFDE69
	.long L$set$158
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB36-.
	.set L$set$159,LFE36-LFB36
	.quad L$set$159
	.uleb128 0
	.byte	0x4
	.set L$set$160,LCFI89-LFB36
	.long L$set$160
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$161,LCFI90-LCFI89
	.long L$set$161
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$162,LCFI91-LCFI90
	.long L$set$162
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$163,LCFI92-LCFI91
	.long L$set$163
	.byte	0xb
	.byte	0x4
	.set L$set$164,LCFI93-LCFI92
	.long L$set$164
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$165,LCFI94-LCFI93
	.long L$set$165
	.byte	0xb
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$166,LEFDE71-LASFDE71
	.long L$set$166
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB37-.
	.set L$set$167,LFE37-LFB37
	.quad L$set$167
	.uleb128 0
	.byte	0x4
	.set L$set$168,LCFI95-LFB37
	.long L$set$168
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$169,LCFI96-LCFI95
	.long L$set$169
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$170,LCFI97-LCFI96
	.long L$set$170
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$171,LCFI98-LCFI97
	.long L$set$171
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$172,LCFI99-LCFI98
	.long L$set$172
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
	.set L$set$173,LCFI100-LCFI99
	.long L$set$173
	.byte	0xb
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$174,LEFDE73-LASFDE73
	.long L$set$174
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB39-.
	.set L$set$175,LFE39-LFB39
	.quad L$set$175
	.uleb128 0
	.byte	0x4
	.set L$set$176,LCFI101-LFB39
	.long L$set$176
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$177,LCFI102-LCFI101
	.long L$set$177
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$178,LCFI103-LCFI102
	.long L$set$178
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$179,LCFI104-LCFI103
	.long L$set$179
	.byte	0x95
	.uleb128 0x8
	.byte	0x4
	.set L$set$180,LCFI105-LCFI104
	.long L$set$180
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
	.set L$set$181,LCFI106-LCFI105
	.long L$set$181
	.byte	0xb
	.byte	0x4
	.set L$set$182,LCFI107-LCFI106
	.long L$set$182
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
	.set L$set$183,LCFI108-LCFI107
	.long L$set$183
	.byte	0xb
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$184,LEFDE75-LASFDE75
	.long L$set$184
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB41-.
	.set L$set$185,LFE41-LFB41
	.quad L$set$185
	.uleb128 0
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$186,LEFDE77-LASFDE77
	.long L$set$186
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB42-.
	.set L$set$187,LFE42-LFB42
	.quad L$set$187
	.uleb128 0
	.align	3
LEFDE77:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
