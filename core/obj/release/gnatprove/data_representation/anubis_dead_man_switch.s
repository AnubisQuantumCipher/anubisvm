	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC4:
	.ascii "anubis_dead_man_switch.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_dead_man_switch__serialize_switch_state__write_u64.9:
LFB57:
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
	mov	w1, 733
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L16:
	adrp	x0, lC4@PAGE
	mov	w1, 731
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE57:
	.align	2
	.p2align 5,,15
_anubis_dead_man_switch__deserialize_switch_state__read_u64.10:
LFB59:
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
	adrp	x0, lC4@PAGE
	mov	w1, 813
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L27:
	adrp	x0, lC4@PAGE
	mov	w1, 814
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE59:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__privilege_typeH
_anubis_dead_man_switch__privilege_typeH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L31
	sub	w1, w1, w2
	cmp	w1, 3
	bgt	L32
L31:
	mov	x3, 0
	mov	x0, 0
L29:
	adrp	x2, _privilege_typeG.44@PAGE
	mov	w1, 43691
	add	x2, x2, _privilege_typeG.44@PAGEOFF;
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
L32:
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
	b	L29
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__governance_phaseH
_anubis_dead_man_switch__governance_phaseH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L36
	sub	w1, w1, w2
	cmp	w1, 4
	bgt	L37
L36:
	adrp	x1, _governance_phaseG.40@PAGE
	mov	x2, 0
	add	x1, x1, _governance_phaseG.40@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L37:
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
	adrp	x1, _governance_phaseG.40@PAGE
	sxtw	x2, w2
	add	x1, x1, _governance_phaseG.40@PAGEOFF;
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__switch_stateH
_anubis_dead_man_switch__switch_stateH:
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
	adrp	x2, _switch_stateG.36@PAGE
	add	x2, x2, _switch_stateG.36@PAGEOFF;
	sxtw	x1, w1
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L40:
	adrp	x2, _switch_stateG.36@PAGE
	mov	x1, 0
	add	x2, x2, _switch_stateG.36@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__pause_stateH
_anubis_dead_man_switch__pause_stateH:
LFB5:
	ldp	w4, w1, [x1]
	mov	w10, -1
	adrp	x9, _pause_stateP.35@PAGE
	add	w6, w10, 1
	add	x9, x9, _pause_stateP.35@PAGEOFF;
	adrp	x12, _pause_stateT1.34@PAGE
	adrp	x11, _pause_stateT2.33@PAGE
	mov	w2, 0
	mov	w3, 0
	add	x12, x12, _pause_stateT1.34@PAGEOFF;
	add	x11, x11, _pause_stateT2.33@PAGEOFF;
	add	w8, w4, w10
	cmp	w4, w1
	sxtw	x15, w4
	sub	w1, w1, w4
	ldr	w4, [x9, w6, sxtw 2]
	sxtw	x8, w8
	csinc	w5, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w4, sxtw
	mov	w13, 9
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w5, w4
	blt	L43
L47:
	ldrb	w7, [x0, x1]
	ldrb	w4, [x12, w6, sxtw]
	ldrb	w1, [x11, w6, sxtw]
	madd	w4, w4, w7, w3
	madd	w1, w1, w7, w2
	sdiv	w3, w4, w13
	sdiv	w2, w1, w13
	add	w3, w3, w3, lsl 3
	sub	w3, w4, w3
	add	w2, w2, w2, lsl 3
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L43
	add	w6, w10, 1
	mov	w14, 1
	ldr	w4, [x9, w6, sxtw 2]
	mov	w10, 0
	add	x1, x8, w4, sxtw
	sub	x1, x1, x15
	cmp	w5, w4
	bge	L47
L43:
	adrp	x1, _pause_stateG.32@PAGE
	add	x1, x1, _pause_stateG.32@PAGEOFF;
	ldrb	w0, [x1, w3, sxtw]
	ldrb	w1, [x1, w2, sxtw]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__recovery_stateH
_anubis_dead_man_switch__recovery_stateH:
LFB6:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L50
	ldrb	w0, [x0]
	mov	w1, 14
	add	w2, w0, w0, lsl 1
	lsl	w4, w0, 3
	sub	w4, w4, w0
	add	w2, w0, w2, lsl 2
	udiv	w0, w4, w1
	udiv	w3, w2, w1
	msub	w0, w0, w1, w4
	sxtw	x0, w0
	msub	w3, w3, w1, w2
	sxtw	x3, w3
L49:
	adrp	x2, _recovery_stateG.28@PAGE
	mov	w1, 43691
	add	x2, x2, _recovery_stateG.28@PAGEOFF;
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
L50:
	mov	x3, 0
	mov	x0, 0
	b	L49
LFE6:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__heartbeat_recordIP
_anubis_dead_man_switch__heartbeat_recordIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__dead_man_stateIP
_anubis_dead_man_switch__dead_man_stateIP:
LFB165:
	ret
LFE165:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__council_memberIP
_anubis_dead_man_switch__council_memberIP:
LFB167:
	ret
LFE167:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__Tcouncil_arrayBIP
_anubis_dead_man_switch__Tcouncil_arrayBIP:
LFB10:
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__council_stateIP
_anubis_dead_man_switch__council_stateIP:
LFB169:
	ret
LFE169:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__privilege_recordIP
_anubis_dead_man_switch__privilege_recordIP:
LFB171:
	ret
LFE171:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__Tprivilege_arrayBIP
_anubis_dead_man_switch__Tprivilege_arrayBIP:
LFB13:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__heartbeat_resultH
_anubis_dead_man_switch__heartbeat_resultH:
LFB14:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L60
	ldrb	w0, [x0]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w3, w0, w0, lsl 1
	add	w0, w0, w0, lsl 3
	umull	x2, w3, w1
	umull	x1, w0, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	add	w5, w2, w2, lsl 2
	add	w4, w1, w1, lsl 2
	add	w2, w2, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w3, w3, w2
	adrp	x2, _heartbeat_resultG.24@PAGE
	sub	w0, w0, w1
	add	x2, x2, _heartbeat_resultG.24@PAGEOFF;
	sxtw	x3, w3
	sxtw	x1, w0
	ldrb	w1, [x2, x1]
	ldrb	w0, [x2, x3]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L60:
	adrp	x2, _heartbeat_resultG.24@PAGE
	mov	x3, 0
	add	x2, x2, _heartbeat_resultG.24@PAGEOFF;
	mov	x1, 0
	ldrb	w0, [x2, x3]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE14:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__pause_resultH
_anubis_dead_man_switch__pause_resultH:
LFB15:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	blt	L64
	mov	x3, 0
	mov	x0, 0
L62:
	adrp	x2, _pause_resultG.20@PAGE
	mov	w1, 52429
	add	x2, x2, _pause_resultG.20@PAGEOFF;
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
L64:
	ldrb	w0, [x0, 1]
	mov	w1, 14
	add	w2, w0, w0, lsl 1
	lsl	w4, w0, 3
	sub	w4, w4, w0
	add	w2, w0, w2, lsl 2
	udiv	w0, w4, w1
	udiv	w3, w2, w1
	msub	w0, w0, w1, w4
	sxtw	x0, w0
	msub	w3, w3, w1, w2
	sxtw	x3, w3
	b	L62
LFE15:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__recovery_resultH
_anubis_dead_man_switch__recovery_resultH:
LFB16:
	ldp	w4, w1, [x1]
	mov	w10, -1
	adrp	x9, _recovery_resultP.19@PAGE
	add	w6, w10, 1
	add	x9, x9, _recovery_resultP.19@PAGEOFF;
	adrp	x12, _recovery_resultT1.18@PAGE
	adrp	x11, _recovery_resultT2.17@PAGE
	mov	w2, 0
	mov	w3, 0
	add	x12, x12, _recovery_resultT1.18@PAGEOFF;
	add	x11, x11, _recovery_resultT2.17@PAGEOFF;
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
	blt	L67
L71:
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
	beq	L67
	add	w6, w10, 1
	mov	w14, 1
	ldr	w4, [x9, w6, sxtw 2]
	mov	w10, 0
	add	x1, x8, w4, sxtw
	sub	x1, x1, x15
	cmp	w5, w4
	bge	L71
L67:
	adrp	x1, _recovery_resultG.16@PAGE
	add	x1, x1, _recovery_resultG.16@PAGEOFF;
	ldrb	w0, [x1, w3, sxtw]
	ldrb	w1, [x1, w2, sxtw]
	add	w0, w0, w1
	and	w0, w0, 7
	ret
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__parameter_idH
_anubis_dead_man_switch__parameter_idH:
LFB17:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L75
	sub	w1, w1, w2
	cmp	w1, 3
	bgt	L76
L75:
	mov	x3, 0
	mov	x0, 0
L73:
	adrp	x2, _parameter_idG.12@PAGE
	mov	w1, 43691
	add	x2, x2, _parameter_idG.12@PAGEOFF;
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
L76:
	ldrb	w2, [x0, 4]
	mov	w1, 61681
	movk	w1, 0xf0f0, lsl 16
	lsl	w0, w2, 4
	lsl	w3, w2, 3
	sub	w0, w0, w2
	umull	x2, w3, w1
	umull	x1, w0, w1
	lsr	x2, x2, 36
	lsr	x1, x1, 36
	add	w2, w2, w2, lsl 4
	add	w1, w1, w1, lsl 4
	sub	w3, w3, w2
	sub	w0, w0, w1
	sxtw	x3, w3
	sxtw	x0, w0
	b	L73
LFE17:
	.const
	.align	3
lC5:
	.ascii "failed precondition from anubis_dead_man_switch.ads:237"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__init_switch
_anubis_dead_man_switch__init_switch:
LFB18:
	stp	x29, x30, [sp, -16]!
LCFI11:
	mov	x29, sp
LCFI12:
	ldp	w2, w4, [x2]
	sxtw	x6, w2
	sxtw	x5, w4
	add	x7, x6, 31
	cmp	x7, x5
	bne	L89
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	cmp	w2, w4
	bgt	L79
	sub	x7, x1, x6
	sub	x1, x6, #1
	sub	x6, x0, x6
	.p2align 5,,15
L85:
	add	x1, x1, 1
	subs	w4, w1, w2
	bvs	L81
	cmp	w4, 31
	bgt	L83
	bhi	L90
	ldrb	w4, [x7, x1]
	strb	w4, [x6, x1]
L83:
	cmp	x1, x5
	bne	L85
L79:
	movi	v31.4s, 0
	movi	v30.4s, 0
	add	x1, x0, 104
	strh	wzr, [x0, 32]
	stp	x3, x3, [x0, 40]
	str	x3, [x0, 56]
	str	q30, [x0, 64]
	strb	wzr, [x0, 80]
	str	q30, [x0, 88]
	str	q31, [x0, 104]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	stp	q31, q31, [x1, 112]
	stp	q31, q31, [x1, 144]
	stp	q31, q31, [x1, 176]
	stp	q31, q31, [x1, 208]
	str	q31, [x1, 240]
	strb	wzr, [x0, 360]
	stp	q30, q30, [x0, 368]
	str	xzr, [x0, 400]
	ldp	x29, x30, [sp], 16
LCFI13:
	ret
L81:
LCFI14:
	adrp	x0, lC4@PAGE
	mov	w1, 26
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L90:
	adrp	x0, lC4@PAGE
	mov	w1, 27
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L89:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE18:
	.const
	.align	2
lC0:
	.word	1
	.word	55
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__init_council
_anubis_dead_man_switch__init_council:
LFB20:
	stp	x29, x30, [sp, -32]!
LCFI15:
	mov	x29, sp
LCFI16:
	mov	x2, 720
	stp	x19, x20, [sp, 16]
LCFI17:
	mov	x19, x0
	mov	x20, x1
	mov	w1, 0
	bl	_memset
	str	wzr, [x19, 720]
	str	x20, [x19, 728]
	str	x20, [x19, 736]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI18:
	ret
LFE20:
	.const
	.align	3
lC6:
	.ascii "failed precondition from anubis_dead_man_switch.ads:263"
	.align	3
lC7:
	.ascii "failed postcondition from anubis_dead_man_switch.ads:265"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__record_heartbeat
_anubis_dead_man_switch__record_heartbeat:
LFB22:
	stp	x29, x30, [sp, -16]!
LCFI19:
	mov	x29, sp
LCFI20:
	ldpsw	x3, x6, [x2]
	ldpsw	x1, x2, [x4]
	add	x3, x3, 62
	cmp	x3, x6
	add	x1, x1, 30
	ccmp	x1, x2, 0, lt
	bge	L104
	ldrb	w1, [x0, 32]
	cmp	w1, 3
	bhi	L105
	cmp	w1, 2
	beq	L99
	ldr	x2, [x0, 392]
	str	x5, [x0, 40]
	str	x5, [x0, 56]
	add	x2, x2, 1
	str	x2, [x0, 392]
	cmp	w1, 1
	beq	L106
	cbnz	w1, L107
L98:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI21:
	ret
	.p2align 2,,3
L106:
LCFI22:
	strb	wzr, [x0, 32]
	str	xzr, [x0, 64]
	b	L98
	.p2align 2,,3
L99:
	mov	w0, 3
	ldp	x29, x30, [sp], 16
LCFI23:
	ret
L107:
LCFI24:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L105:
	adrp	x0, lC4@PAGE
	mov	w1, 89
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L104:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE22:
	.const
	.align	2
lC1:
	.word	1
	.word	56
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__check_heartbeat_status
_anubis_dead_man_switch__check_heartbeat_status:
LFB24:
	ldrb	w3, [x0, 32]
	str	x1, [x0, 40]
	cmp	w3, 3
	bhi	L117
	sub	w2, w3, #2
	and	w2, w2, 255
	cmp	w2, 1
	bls	L108
	ldr	x2, [x0, 56]
	cmp	x2, x1
	bcs	L108
	mov	x4, 38783
	sub	x2, x1, x2
	movk	x4, 0x6, lsl 16
	eor	w3, w3, 1
	cmp	x2, x4
	cset	w2, hi
	tst	w2, w3
	beq	L108
	ldr	w2, [x0, 400]
	mov	w3, 1
	str	x1, [x0, 64]
	strb	w3, [x0, 32]
	add	w1, w2, 1
	str	w1, [x0, 400]
L108:
	ret
L117:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI25:
	mov	w1, 117
	mov	x29, sp
LCFI26:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__blocks_until_heartbeat
_anubis_dead_man_switch__blocks_until_heartbeat:
LFB25:
	ldr	x2, [x0, 56]
	cmp	x2, x1
	bcs	L120
	mov	x3, 38783
	sub	x4, x1, x2
	movk	x3, 0x6, lsl 16
	add	x0, x3, 1
	cmp	x4, x3
	sub	x0, x0, x1
	add	x0, x0, x2
	csel	x0, x0, xzr, ls
	ret
	.p2align 2,,3
L120:
	mov	x0, 38784
	movk	x0, 0x6, lsl 16
	ret
LFE25:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__is_heartbeat_overdue
_anubis_dead_man_switch__is_heartbeat_overdue:
LFB26:
	ldr	x3, [x0, 56]
	mov	w0, 0
	cmp	x1, x3
	bls	L123
	mov	x2, 38783
	sub	x4, x1, x3
	movk	x2, 0x6, lsl 16
	mov	w0, 1
	cmp	x4, x2
	bhi	L123
	add	x2, x2, 1
	sub	x2, x2, x1
	cmn	x2, x3
	cset	w0, eq
L123:
	ret
LFE26:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__should_trigger
_anubis_dead_man_switch__should_trigger:
LFB29:
	ldrb	w3, [x0, 32]
	cmp	w3, 3
	bhi	L133
	mov	x2, x0
	mov	w0, 0
	cmp	w3, 1
	beq	L134
	ret
	.p2align 2,,3
L134:
	ldr	x0, [x2, 64]
	add	x0, x0, 430080
	add	x0, x0, 1920
	cmp	x1, x0
	cset	w0, cs
	ret
L133:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI27:
	mov	w1, 188
	mov	x29, sp
LCFI28:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE29:
	.const
	.align	3
lC8:
	.ascii "failed precondition from anubis_dead_man_switch.ads:322"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__transfer_all_privileges
_anubis_dead_man_switch__transfer_all_privileges:
LFB31:
	stp	x29, x30, [sp, -16]!
LCFI29:
	mov	x29, sp
LCFI30:
	ldp	w2, w10, [x2]
	sxtw	x11, w2
	sxtw	x5, w10
	add	x4, x11, 31
	cmp	x4, x5
	bne	L149
	mov	x9, 1
	mov	x8, 0
	sub	x6, x9, x11
	sub	x7, x1, x11
	add	x6, x0, x6
	b	L144
	.p2align 2,,3
L137:
	add	x1, x0, x8, lsl 6
	add	x8, x8, 1
	add	x6, x6, 64
	strb	w9, [x1, 48]
	str	x3, [x1, 56]
	cmp	x8, 6
	beq	L150
L144:
	cmp	w2, w10
	bgt	L137
	sub	x1, x11, #1
	b	L143
	.p2align 2,,3
L152:
	cmp	w4, 31
	bgt	L141
	bhi	L151
	ldrb	w4, [x7, x1]
	strb	w4, [x6, x1]
L141:
	cmp	x5, x1
	beq	L137
L143:
	add	x1, x1, 1
	subs	w4, w1, w2
	bvc	L152
	adrp	x0, lC4@PAGE
	mov	w1, 201
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L150:
	ldp	x29, x30, [sp], 16
LCFI31:
	ret
L151:
LCFI32:
	adrp	x0, lC4@PAGE
	mov	w1, 202
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L149:
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE31:
	.const
	.align	3
lC9:
	.ascii "anubis_dead_man_switch.ads"
	.space 1
	.align	3
lC10:
	.ascii "failed precondition from anubis_dead_man_switch.ads:301"
	.align	3
lC11:
	.ascii "failed postcondition from anubis_dead_man_switch.ads:302"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__trigger_switch
_anubis_dead_man_switch__trigger_switch:
LFB27:
	stp	x29, x30, [sp, -32]!
LCFI33:
	mov	x29, sp
LCFI34:
	stp	x19, x20, [sp, 16]
LCFI35:
	ldrb	w3, [x0, 32]
	cmp	w3, 3
	bhi	L159
	cmp	w3, 1
	bne	L160
	mov	x19, x0
	mov	x20, x2
	mov	w3, 2
	mov	x0, x1
	adrp	x2, lC2@PAGE
	adrp	x1, _dao_address.11@PAGE
	str	x20, [x19, 72]
	add	x1, x1, _dao_address.11@PAGEOFF;
	add	x2, x2, lC2@PAGEOFF;
	strb	w3, [x19, 32]
	mov	x3, x20
	bl	_anubis_dead_man_switch__transfer_all_privileges
	ldrb	w0, [x19, 32]
	cmp	w0, 3
	bhi	L161
	ldr	x1, [x19, 72]
	cmp	x1, x20
	ccmp	w0, 2, 0, eq
	bne	L162
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI36:
	ret
L159:
LCFI37:
	adrp	x0, lC9@PAGE
	mov	w1, 301
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L162:
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L161:
	adrp	x0, lC9@PAGE
	mov	w1, 302
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L160:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE27:
	.const
	.align	2
lC2:
	.word	0
	.word	31
	.text
	.const
	.align	3
lC12:
	.ascii "failed precondition from anubis_dead_man_switch.ads:337"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__initiate_recovery
_anubis_dead_man_switch__initiate_recovery:
LFB32:
	stp	x29, x30, [sp, -16]!
LCFI38:
	mov	x29, sp
LCFI39:
	mov	x1, x0
	ldpsw	x3, x6, [x2]
	ldpsw	x0, x2, [x4]
	add	x3, x3, 62
	cmp	x3, x6
	add	x0, x0, 62
	ccmp	x0, x2, 0, lt
	bge	L169
	ldrb	w2, [x1, 32]
	cmp	w2, 3
	bhi	L170
	mov	w0, 6
	cmp	w2, 2
	beq	L171
	ldp	x29, x30, [sp], 16
LCFI40:
	ret
	.p2align 2,,3
L171:
LCFI41:
	add	x5, x5, 602112
	mov	w0, 0
	strb	w2, [x1, 360]
	add	x5, x5, 2688
	stp	xzr, xzr, [x1, 368]
	str	x5, [x1, 384]
	ldp	x29, x30, [sp], 16
LCFI42:
	ret
L170:
LCFI43:
	adrp	x0, lC4@PAGE
	mov	w1, 227
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L169:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE32:
	.const
	.align	3
lC13:
	.ascii "failed precondition from anubis_dead_man_switch.ads:352"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__vote_recovery
_anubis_dead_man_switch__vote_recovery:
LFB34:
	stp	x29, x30, [sp, -16]!
LCFI44:
	mov	x29, sp
LCFI45:
	ldrb	w6, [x0, 360]
	ldp	w1, w2, [x2]
	cmp	w6, 5
	bhi	L181
	sxtw	x1, w1
	sxtw	x2, w2
	add	x1, x1, 31
	cmp	x3, 0
	ccmp	x1, x2, 0, ne
	ccmp	w6, 2, 0, eq
	bne	L182
	ldr	x1, [x0, 384]
	cmp	x1, x5
	bcc	L179
	cmp	w4, 1
	bhi	L183
	cbz	w4, L177
	ldr	x1, [x0, 368]
	add	x1, x1, x3
	str	x1, [x0, 368]
	mov	w0, 2
L184:
	ldp	x29, x30, [sp], 16
LCFI46:
	ret
	.p2align 2,,3
L179:
LCFI47:
	mov	w0, 7
	ldp	x29, x30, [sp], 16
LCFI48:
	ret
	.p2align 2,,3
L177:
LCFI49:
	ldr	x1, [x0, 376]
	add	x1, x1, x3
	str	x1, [x0, 376]
	mov	w0, 2
	b	L184
L182:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L181:
	adrp	x0, lC9@PAGE
	mov	w1, 354
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L183:
	adrp	x0, lC4@PAGE
	mov	w1, 263
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE34:
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_dead_man_switch.ads:365"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__finalize_recovery
_anubis_dead_man_switch__finalize_recovery:
LFB35:
	stp	x29, x30, [sp, -16]!
LCFI50:
	mov	x8, x0
	mov	x29, sp
LCFI51:
	ldrb	w0, [x0, 360]
	ldp	w3, w10, [x3]
	cmp	w0, 5
	bhi	L203
	sxtw	x11, w3
	sxtw	x5, w10
	add	x6, x11, 31
	cmp	x6, x5
	ccmp	w0, 2, 0, eq
	bne	L204
	ldp	x6, x0, [x8, 368]
	adds	x0, x6, x0
	beq	L190
	mov	x9, 6700
	mov	x7, 22859
	movk	x7, 0x3886, lsl 16
	mul	x0, x0, x9
	movk	x7, 0xc5d6, lsl 32
	movk	x7, 0x346d, lsl 48
	umulh	x0, x0, x7
	cmp	x6, x0, lsr 11
	bcc	L190
	ldr	w0, [x8, 404]
	sub	x7, x2, x11
	mov	w2, 3
	sub	x6, x1, x11
	mov	x9, 0
	str	x4, [x8, 56]
	add	x6, x6, 1
	strb	w2, [x8, 32]
	strb	w2, [x8, 360]
	add	w0, w0, 1
	str	w0, [x8, 404]
	b	L198
	.p2align 2,,3
L191:
	add	x0, x1, x9, lsl 6
	add	x9, x9, 1
	add	x6, x6, 64
	strb	wzr, [x0, 48]
	cmp	x9, 6
	beq	L205
L198:
	cmp	w3, w10
	bgt	L191
	sub	x0, x11, #1
	b	L197
	.p2align 2,,3
L207:
	cmp	w2, 31
	bgt	L195
	bhi	L206
	ldrb	w2, [x7, x0]
	strb	w2, [x6, x0]
L195:
	cmp	x5, x0
	beq	L191
L197:
	add	x0, x0, 1
	subs	w2, w0, w3
	bvc	L207
	adrp	x0, lC4@PAGE
	mov	w1, 311
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L190:
	mov	w1, 4
	mov	w0, 4
	strb	w1, [x8, 360]
	ldp	x29, x30, [sp], 16
LCFI52:
	ret
	.p2align 2,,3
L205:
LCFI53:
	mov	w0, 3
	ldp	x29, x30, [sp], 16
LCFI54:
	ret
L206:
LCFI55:
	adrp	x0, lC4@PAGE
	mov	w1, 312
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L204:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L203:
	adrp	x0, lC9@PAGE
	mov	w1, 366
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE35:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__update_phase
_anubis_dead_man_switch__update_phase:
LFB36:
	mov	x3, 13119
	mov	w2, 0
	movk	x3, 0x50, lsl 16
	cmp	x1, x3
	bls	L209
	mov	x3, 26239
	mov	w2, 1
	movk	x3, 0xa0, lsl 16
	cmp	x1, x3
	bhi	L213
L209:
	strb	w2, [x0, 33]
	str	x1, [x0, 40]
	ret
	.p2align 2,,3
L213:
	mov	x2, 39359
	str	x1, [x0, 40]
	movk	x2, 0xf0, lsl 16
	cmp	x1, x2
	cset	w2, hi
	add	w2, w2, 2
	strb	w2, [x0, 33]
	ret
LFE36:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__get_phase
_anubis_dead_man_switch__get_phase:
LFB37:
	ldrb	w0, [x0, 33]
	cmp	w0, 3
	bhi	L219
	ret
L219:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI56:
	mov	w1, 352
	mov	x29, sp
LCFI57:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE37:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__privilege_available
_anubis_dead_man_switch__privilege_available:
LFB39:
	stp	x29, x30, [sp, -16]!
LCFI58:
	mov	x29, sp
LCFI59:
	ldrb	w0, [x0, 33]
	cmp	w0, 3
	bhi	L230
	cmp	w0, 1
	beq	L222
	cmp	w0, 2
	beq	L223
	cbnz	w0, L228
	cmp	w1, 5
	bhi	L231
	cset	w0, ne
	ldp	x29, x30, [sp], 16
LCFI60:
	ret
	.p2align 2,,3
L223:
LCFI61:
	cmp	w1, 5
	bhi	L232
	cmp	w1, 1
	cset	w0, eq
	ldp	x29, x30, [sp], 16
LCFI62:
	ret
	.p2align 2,,3
L222:
LCFI63:
	cmp	w1, 5
	bhi	L233
	cmp	w1, 1
	cset	w0, ls
	ldp	x29, x30, [sp], 16
LCFI64:
	ret
	.p2align 2,,3
L228:
LCFI65:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI66:
	ret
L230:
LCFI67:
	adrp	x0, lC4@PAGE
	mov	w1, 360
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L231:
	adrp	x0, lC4@PAGE
	mov	w1, 362
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L232:
	adrp	x0, lC4@PAGE
	mov	w1, 369
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L233:
	adrp	x0, lC4@PAGE
	mov	w1, 365
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE39:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__privilege_expires_at
_anubis_dead_man_switch__privilege_expires_at:
LFB40:
	uxtw	x1, w0
	cmp	w1, 5
	bhi	L241
	mov	x0, 0
	bne	L242
	ret
	.p2align 2,,3
L242:
	adrp	x0, _CSWTCH.349@PAGE
	add	x0, x0, _CSWTCH.349@PAGEOFF;
	ldr	x0, [x0, x1, lsl 3]
	ret
L241:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI68:
	mov	w1, 380
	mov	x29, sp
LCFI69:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE40:
	.const
	.align	3
lC15:
	.ascii "failed precondition from anubis_dead_man_switch.ads:412"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__builder_emergency_pause
_anubis_dead_man_switch__builder_emergency_pause:
LFB41:
	stp	x29, x30, [sp, -16]!
LCFI70:
	mov	x29, sp
LCFI71:
	mov	x5, x0
	mov	x6, 43200
	ldpsw	x7, x8, [x2]
	add	x0, x7, 255
	cmp	x0, x8
	ccmp	x3, x6, 2, ge
	bhi	L261
	ldrb	w0, [x5, 33]
	cmp	w0, 3
	bhi	L262
	beq	L256
	ldrb	w0, [x5, 80]
	cmp	w0, 3
	bhi	L263
	cbz	w0, L264
	mov	w0, 1
L246:
	ldp	x29, x30, [sp], 16
LCFI72:
	ret
	.p2align 2,,3
L264:
LCFI73:
	movi	v31.4s, 0
	add	x3, x3, x4
	mov	w8, 1
	add	x6, x5, 104
	strb	w8, [x5, 80]
	stp	x4, x3, [x5, 88]
	str	q31, [x5, 104]
	stp	q31, q31, [x6, 16]
	stp	q31, q31, [x6, 48]
	stp	q31, q31, [x6, 80]
	stp	q31, q31, [x6, 112]
	stp	q31, q31, [x6, 144]
	stp	q31, q31, [x6, 176]
	stp	q31, q31, [x6, 208]
	str	q31, [x6, 240]
	ldp	w3, w6, [x2]
	cmp	w3, w6
	bgt	L246
	sxtw	x2, w3
	sub	x4, x1, x7
	sub	x5, x5, x2
	sxtw	x6, w6
	add	x5, x5, 104
	sub	x1, x2, #1
	b	L254
	.p2align 2,,3
L266:
	cmp	w2, 255
	bgt	L252
	bhi	L265
	ldrb	w2, [x4, x1]
	strb	w2, [x5, x1]
L252:
	cmp	x6, x1
	beq	L246
L254:
	add	x1, x1, 1
	subs	w2, w1, w3
	bvc	L266
	adrp	x0, lC4@PAGE
	mov	w1, 430
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L256:
	mov	w0, 4
	ldp	x29, x30, [sp], 16
LCFI74:
	ret
L265:
LCFI75:
	adrp	x0, lC4@PAGE
	mov	w1, 431
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L261:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L263:
	adrp	x0, lC4@PAGE
	mov	w1, 411
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L262:
	adrp	x0, lC4@PAGE
	mov	w1, 405
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE41:
	.const
	.align	3
lC16:
	.ascii "failed precondition from anubis_dead_man_switch.ads:426"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__council_emergency_pause
_anubis_dead_man_switch__council_emergency_pause:
LFB42:
	stp	x29, x30, [sp, -16]!
LCFI76:
	mov	x29, sp
LCFI77:
	mov	x3, x0
	mov	x2, 43200
	ldpsw	x8, x9, [x5]
	add	x0, x8, 255
	cmp	x0, x9
	ccmp	x6, x2, 2, ge
	bhi	L285
	ldr	w0, [x1, 720]
	tbnz	w0, #31, L286
	cmp	w0, 4
	ble	L280
	ldrb	w0, [x3, 80]
	cmp	w0, 3
	bhi	L287
	cbz	w0, L288
	mov	w0, 1
L270:
	ldp	x29, x30, [sp], 16
LCFI78:
	ret
	.p2align 2,,3
L288:
LCFI79:
	movi	v31.4s, 0
	add	x6, x6, x7
	mov	w2, 2
	add	x1, x3, 104
	strb	w2, [x3, 80]
	stp	x7, x6, [x3, 88]
	str	q31, [x3, 104]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	stp	q31, q31, [x1, 112]
	stp	q31, q31, [x1, 144]
	stp	q31, q31, [x1, 176]
	stp	q31, q31, [x1, 208]
	str	q31, [x1, 240]
	ldp	w2, w6, [x5]
	cmp	w2, w6
	bgt	L270
	sxtw	x1, w2
	sub	x5, x4, x8
	sub	x3, x3, x1
	sxtw	x6, w6
	add	x3, x3, 104
	sub	x1, x1, #1
	b	L278
	.p2align 2,,3
L290:
	cmp	w4, 255
	bgt	L276
	bhi	L289
	ldrb	w4, [x5, x1]
	strb	w4, [x3, x1]
L276:
	cmp	x6, x1
	beq	L270
L278:
	add	x1, x1, 1
	subs	w4, w1, w2
	bvc	L290
	adrp	x0, lC4@PAGE
	mov	w1, 475
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L280:
	mov	w0, 2
	ldp	x29, x30, [sp], 16
LCFI80:
	ret
L289:
LCFI81:
	adrp	x0, lC4@PAGE
	mov	w1, 476
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L285:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L286:
	adrp	x0, lC4@PAGE
	mov	w1, 450
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L287:
	adrp	x0, lC4@PAGE
	mov	w1, 456
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE42:
	.const
	.align	3
lC17:
	.ascii "failed precondition from anubis_dead_man_switch.ads:437"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__unpause
_anubis_dead_man_switch__unpause:
LFB43:
	stp	x29, x30, [sp, -16]!
LCFI82:
	mov	x29, sp
LCFI83:
	mov	x1, x0
	ldpsw	x3, x5, [x2]
	ldpsw	x0, x2, [x4]
	add	x3, x3, 31
	cmp	x3, x5
	add	x0, x0, 62
	ccmp	x0, x2, 0, eq
	bge	L298
	ldrb	w2, [x1, 80]
	cmp	w2, 3
	bhi	L299
	mov	w0, 1
	cbnz	w2, L300
	ldp	x29, x30, [sp], 16
LCFI84:
	ret
	.p2align 2,,3
L300:
LCFI85:
	movi	v31.4s, 0
	add	x2, x1, 104
	mov	w0, 0
	strb	wzr, [x1, 80]
	stp	xzr, xzr, [x1, 88]
	str	q31, [x1, 104]
	stp	q31, q31, [x2, 16]
	stp	q31, q31, [x2, 48]
	stp	q31, q31, [x2, 80]
	stp	q31, q31, [x2, 112]
	stp	q31, q31, [x2, 144]
	stp	q31, q31, [x2, 176]
	stp	q31, q31, [x2, 208]
	str	q31, [x2, 240]
	ldp	x29, x30, [sp], 16
LCFI86:
	ret
L299:
LCFI87:
	adrp	x0, lC4@PAGE
	mov	w1, 491
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L298:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE43:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__is_paused
_anubis_dead_man_switch__is_paused:
LFB44:
	ldrb	w0, [x0, 80]
	cmp	w0, 3
	bhi	L306
	cmp	w0, 0
	cset	w0, ne
	ret
L306:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI88:
	mov	w1, 506
	mov	x29, sp
LCFI89:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE44:
	.const
	.align	3
lC18:
	.ascii "failed precondition from anubis_dead_man_switch.ads:468"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__tune_parameter
_anubis_dead_man_switch__tune_parameter:
LFB46:
	stp	x29, x30, [sp, -16]!
LCFI90:
	mov	x29, sp
LCFI91:
	ldpsw	x1, x4, [x5]
	add	x1, x1, 62
	cmp	x1, x4
	bge	L315
	ldrb	w1, [x0, 33]
	cmp	w1, 3
	bhi	L316
	mov	w0, 0
	cbz	w1, L317
	ldp	x29, x30, [sp], 16
LCFI92:
	ret
	.p2align 2,,3
L317:
LCFI93:
	add	x0, x2, x2, lsl 2
	mov	x1, 22859
	cmp	x2, x3
	movk	x1, 0x3886, lsl 16
	sub	x4, x3, x2
	add	x0, x0, x0, lsl 2
	movk	x1, 0xc5d6, lsl 32
	sub	x2, x2, x3
	movk	x1, 0x346d, lsl 48
	csel	x2, x2, x4, cs
	add	x0, x0, x0, lsl 2
	lsl	x0, x0, 4
	umulh	x0, x0, x1
	cmp	x2, x0, lsr 11
	cset	w0, ls
	ldp	x29, x30, [sp], 16
LCFI94:
	ret
L316:
LCFI95:
	adrp	x0, lC4@PAGE
	mov	w1, 524
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L315:
	adrp	x0, lC18@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE46:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__is_valid_change
_anubis_dead_man_switch__is_valid_change:
LFB47:
	add	x2, x0, x0, lsl 2
	mov	x3, 22859
	cmp	x0, x1
	movk	x3, 0x3886, lsl 16
	sub	x4, x1, x0
	add	x2, x2, x2, lsl 2
	movk	x3, 0xc5d6, lsl 32
	sub	x0, x0, x1
	movk	x3, 0x346d, lsl 48
	csel	x0, x0, x4, cs
	add	x2, x2, x2, lsl 2
	lsl	x2, x2, 4
	umulh	x2, x2, x3
	cmp	x0, x2, lsr 11
	cset	w0, ls
	ret
LFE47:
	.const
	.align	3
lC19:
	.ascii "failed precondition from anubis_dead_man_switch.ads:490"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__add_council_member
_anubis_dead_man_switch__add_council_member:
LFB48:
	stp	x29, x30, [sp, -16]!
LCFI96:
	mov	x29, sp
LCFI97:
	ldr	w12, [x0, 720]
	ldp	w7, w15, [x4]
	ldp	w6, w16, [x2]
	tbnz	w12, #31, L350
	sxtw	x14, w6
	mov	x8, x0
	sxtw	x10, w16
	add	x0, x14, 31
	sxtw	x13, w7
	cmp	x0, x10
	sxtw	x11, w15
	add	x0, x13, 31
	ccmp	x0, x11, 0, eq
	mov	x2, 0
	ccmp	w12, 8, 0, eq
	cset	w0, gt
	bgt	L351
	.p2align 5,,15
L323:
	add	x4, x2, x2, lsl 2
	lsl	x9, x2, 2
	add	x4, x8, x4, lsl 4
	ldrb	w4, [x4, 72]
	cmp	w4, 1
	bhi	L352
	cbz	w4, L353
	add	x2, x2, 1
	cmp	x2, 9
	bne	L323
	ldp	x29, x30, [sp], 16
LCFI98:
	ret
	.p2align 2,,3
L353:
LCFI99:
	cmp	w6, w16
	bgt	L326
	mov	w0, 80
	sub	x4, x1, x14
	sub	x1, x14, #1
	umaddl	x0, w2, w0, x8
	sub	x14, x0, x14
	.p2align 5,,15
L332:
	add	x1, x1, 1
	subs	w0, w1, w6
	bvs	L328
	cmp	w0, 31
	bgt	L330
	bhi	L354
	ldrb	w0, [x4, x1]
	strb	w0, [x14, x1]
L330:
	cmp	x10, x1
	bne	L332
L326:
	cmp	w7, w15
	bgt	L333
	mov	w4, 80
	sub	x3, x3, x13
	sub	x1, x13, #1
	umull	x4, w2, w4
	sub	x4, x4, x13
	add	x4, x4, 32
	add	x4, x8, x4
	.p2align 5,,15
L339:
	add	x1, x1, 1
	subs	w0, w1, w7
	bvs	L335
	cmp	w0, 31
	bgt	L337
	bhi	L355
	ldrb	w0, [x3, x1]
	strb	w0, [x4, x1]
L337:
	cmp	x11, x1
	bne	L339
L333:
	add	x9, x9, x2
	mov	w1, 1
	add	x9, x8, x9, lsl 4
	add	w12, w12, 1
	mov	w0, 1
	str	x5, [x9, 64]
	strb	w1, [x9, 72]
	str	wzr, [x9, 76]
	str	w12, [x8, 720]
	ldp	x29, x30, [sp], 16
LCFI100:
	ret
L352:
LCFI101:
	adrp	x0, lC4@PAGE
	mov	w1, 572
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L335:
	adrp	x0, lC4@PAGE
	mov	w1, 583
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L328:
	adrp	x0, lC4@PAGE
	mov	w1, 575
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L355:
	adrp	x0, lC4@PAGE
	mov	w1, 584
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L354:
	adrp	x0, lC4@PAGE
	mov	w1, 576
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L351:
	adrp	x0, lC19@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L350:
	adrp	x0, lC9@PAGE
	mov	w1, 492
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE48:
	.const
	.align	3
lC20:
	.ascii "failed precondition from anubis_dead_man_switch.ads:501"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__remove_council_member
_anubis_dead_man_switch__remove_council_member:
LFB49:
	stp	x29, x30, [sp, -16]!
LCFI102:
	mov	x29, sp
LCFI103:
	cmp	w1, 8
	bhi	L358
	sbfiz	x2, x1, 2, 32
	add	x1, x2, w1, sxtw
	add	x1, x0, x1, lsl 4
	ldrb	w2, [x1, 72]
	cmp	w2, 1
	bhi	L358
	cbz	w2, L363
	ldr	w2, [x0, 720]
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	stp	xzr, xzr, [x1, 32]
	stp	xzr, xzr, [x1, 48]
	str	xzr, [x1, 64]
	strb	wzr, [x1, 72]
	str	wzr, [x1, 76]
	cmp	w2, 0
	blt	L364
	beq	L361
	sub	w2, w2, #1
	str	w2, [x0, 720]
L361:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI104:
	ret
L358:
LCFI105:
	adrp	x0, lC9@PAGE
	mov	w1, 501
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L364:
	adrp	x0, lC4@PAGE
	mov	w1, 619
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L363:
	adrp	x0, lC20@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE49:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__check_council_threshold
_anubis_dead_man_switch__check_council_threshold:
LFB50:
	tbnz	w1, #31, L370
	cmp	w1, 4
	cset	w0, gt
	ret
L370:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI106:
	mov	w1, 632
	mov	x29, sp
LCFI107:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE50:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__get_privilege_holder
_anubis_dead_man_switch__get_privilege_holder:
LFB52:
	stp	x29, x30, [sp, -32]!
LCFI108:
	mov	x29, sp
LCFI109:
	stp	x19, x20, [sp, 16]
LCFI110:
	cmp	w1, 5
	bhi	L374
	mov	x20, x0
	mov	w19, w1
	mov	x0, 40
	mov	x1, 4
	bl	_system__secondary_stack__ss_allocate
	ubfiz	x19, x19, 6, 8
	adrp	x1, lC21@PAGE
	add	x2, x20, x19
	mov	x3, x0
	ldr	d29, [x1, #lC21@PAGEOFF]
	mov	x1, x0
	ldr	q30, [x2, 1]
	ldr	q31, [x2, 17]
	str	d29, [x0], 8
	str	q30, [x3, 8]
	str	q31, [x0, 16]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI111:
	ret
L374:
LCFI112:
	adrp	x0, lC4@PAGE
	mov	w1, 644
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE52:
	.const
	.align	3
lC22:
	.ascii "failed precondition from anubis_dead_man_switch.ads:529"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__has_privilege
_anubis_dead_man_switch__has_privilege:
LFB53:
	stp	x29, x30, [sp, -16]!
LCFI113:
	mov	x29, sp
LCFI114:
	ldp	w5, w3, [x3]
	sxtw	x7, w5
	add	x6, x7, 31
	cmp	x6, w3, sxtw
	bne	L394
	cmp	w5, w3
	bgt	L377
	cmp	w1, 5
	bhi	L395
	ubfiz	x8, x1, 6, 8
	mov	x3, x7
	sub	x8, x8, x7
	mov	w4, 0
	add	x8, x8, 1
	sub	x7, x2, x7
	uxtw	x9, w1
	add	x8, x0, x8
	b	L383
	.p2align 2,,3
L384:
	cmp	x6, x3
	beq	L388
	add	x3, x3, 1
	subs	w4, w3, w5
	bvs	L396
L383:
	cmp	w4, 31
	bgt	L384
	bhi	L397
	ldrb	w2, [x7, x3]
	ldrb	w1, [x8, x3]
	cmp	w2, w1
	beq	L384
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI115:
	ret
	.p2align 2,,3
L377:
LCFI116:
	uxtw	x9, w1
	cmp	w1, 5
	bhi	L389
L388:
	add	x9, x0, x9, lsl 6
	ldrb	w0, [x9, 48]
	cmp	w0, 1
	bhi	L389
	eor	w0, w0, 1
	ldp	x29, x30, [sp], 16
LCFI117:
	ret
L395:
LCFI118:
	adrp	x0, lC4@PAGE
	mov	w1, 655
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L396:
	adrp	x0, lC4@PAGE
	mov	w1, 655
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L397:
	adrp	x0, lC4@PAGE
	mov	w1, 657
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L389:
	adrp	x0, lC4@PAGE
	mov	w1, 665
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L394:
	adrp	x0, lC22@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC22@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE53:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__check_handoff
_anubis_dead_man_switch__check_handoff:
LFB54:
	stp	x29, x30, [sp, -48]!
LCFI119:
	mov	x29, sp
LCFI120:
	stp	x19, x20, [sp, 16]
LCFI121:
	mov	x20, x0
	mov	x0, 13119
	movk	x0, 0x50, lsl 16
	mov	x19, x2
	str	x21, [sp, 32]
LCFI122:
	mov	x21, x1
	str	x2, [x20, 40]
	cmp	x2, x0
	bls	L406
	mov	x0, 26239
	movk	x0, 0xa0, lsl 16
	cmp	x2, x0
	bhi	L401
	mov	w0, 1
	strb	w0, [x20, 33]
L402:
	mov	x0, 13120
	mov	x1, x19
	str	xzr, [x21, 360]
	movk	x0, 0x50, lsl 16
	str	x0, [x21, 168]
	str	x0, [x21, 232]
	str	x0, [x21, 296]
	mov	x0, x20
	bl	_anubis_dead_man_switch__should_trigger
	tbz	x0, 0, L398
L407:
	mov	x2, x19
	mov	x1, x21
	ldr	x21, [sp, 32]
	mov	x0, x20
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI123:
	b	_anubis_dead_man_switch__trigger_switch
	.p2align 2,,3
L406:
LCFI124:
	strb	wzr, [x20, 33]
	mov	x1, x19
	mov	x0, x20
	str	xzr, [x21, 360]
	bl	_anubis_dead_man_switch__should_trigger
	tbnz	x0, 0, L407
L398:
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI125:
	ret
	.p2align 2,,3
L401:
LCFI126:
	mov	x1, 39359
	movk	x1, 0xf0, lsl 16
	cmp	x2, x1
	bhi	L403
	mov	w1, 2
	add	x0, x0, 1
	strb	w1, [x20, 33]
	str	x0, [x21, 40]
	b	L402
	.p2align 2,,3
L403:
	mov	w2, 3
	mov	x1, 39360
	add	x0, x0, 1
	movk	x1, 0xf0, lsl 16
	strb	w2, [x20, 33]
	str	x0, [x21, 40]
	str	x1, [x21, 104]
	b	L402
LFE54:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__next_handoff_block
_anubis_dead_man_switch__next_handoff_block:
LFB55:
	mov	x2, 6559
	mov	x1, x0
	movk	x2, 0x28, lsl 16
	add	x0, x2, 1
	cmp	x1, x2
	bls	L408
	mov	x2, 13119
	movk	x2, 0x50, lsl 16
	add	x0, x2, 1
	cmp	x1, x2
	bls	L408
	mov	x0, 19679
	movk	x0, 0x78, lsl 16
	cmp	x1, x0
	bls	L412
	mov	x0, 26239
	movk	x0, 0xa0, lsl 16
	cmp	x1, x0
	bls	L413
	mov	x2, 39359
	mov	x0, 39360
	movk	x2, 0xf0, lsl 16
	movk	x0, 0xf0, lsl 16
	cmp	x1, x2
	csel	x0, xzr, x0, hi
L408:
	ret
	.p2align 2,,3
L412:
	mov	x0, 19680
	movk	x0, 0x78, lsl 16
	ret
	.p2align 2,,3
L413:
	mov	x0, 26240
	movk	x0, 0xa0, lsl 16
	ret
LFE55:
	.const
	.align	3
lC23:
	.ascii "failed precondition from anubis_dead_man_switch.ads:572"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__serialize_switch_state
_anubis_dead_man_switch__serialize_switch_state:
LFB56:
	stp	x29, x30, [sp, -96]!
LCFI127:
	mov	x29, sp
LCFI128:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI129:
	mov	x21, x0
	add	x0, x29, 96
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w19, w3, [x2]
	str	x0, [x29, 88]
	sxtw	x20, w19
	sxtw	x2, w3
	add	x0, x20, 1022
	str	x20, [x29, 64]
	cmp	x0, x2
	bge	L455
	tbnz	w19, #31, L456
	ldr	x0, [x29, 48]
	cmp	w19, w3
	mov	w1, 0
	sub	x2, x2, x20
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldp	x0, x2, [x29, 48]
	mov	x1, x21
	add	x4, x21, 32
	mov	w7, 2147483647
	ldp	w6, w3, [x2]
	.p2align 5,,15
L422:
	cmp	w3, w19
	blt	L419
	cmp	w6, w19
	bgt	L457
	ldrb	w5, [x1]
	sxtw	x2, w19
	sub	x2, x2, x20
	strb	w5, [x0, x2]
	cmp	w19, w7
	beq	L458
	add	w19, w19, 1
L419:
	add	x1, x1, 1
	cmp	x4, x1
	bne	L422
	cmp	w3, w19
	bge	L459
L423:
	add	x22, x29, 64
	ldr	x0, [x21, 40]
	mov	x16, x22
	str	w19, [x29, 80]
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	x0, [x21, 48]
	mov	x16, x22
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	x0, [x21, 56]
	mov	x16, x22
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	x0, [x21, 64]
	mov	x16, x22
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	x0, [x21, 72]
	mov	x16, x22
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	w1, [x29, 80]
	tbnz	w1, #31, L460
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L430
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L461
	ldrb	w2, [x21, 80]
	cmp	w2, 3
	bhi	L462
	ldr	x4, [x29, 48]
	sxtw	x0, w1
	mov	w3, 2147483647
	sub	x0, x0, x20
	strb	w2, [x4, x0]
	cmp	w1, w3
	beq	L463
	add	w1, w1, 1
L430:
	ldr	x0, [x21, 88]
	mov	x16, x22
	str	w1, [x29, 80]
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	x0, [x21, 96]
	mov	x16, x22
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	w1, [x29, 80]
	tbnz	w1, #31, L464
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L435
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L465
	ldrb	w2, [x21, 360]
	cmp	w2, 5
	bhi	L466
	ldr	x4, [x29, 48]
	sxtw	x0, w1
	mov	w3, 2147483647
	sub	x0, x0, x20
	strb	w2, [x4, x0]
	cmp	w1, w3
	beq	L467
	add	w1, w1, 1
L435:
	ldr	x0, [x21, 368]
	mov	x16, x22
	str	w1, [x29, 80]
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	x0, [x21, 376]
	mov	x16, x22
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	x0, [x21, 384]
	mov	x16, x22
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	x0, [x21, 392]
	mov	x16, x22
	bl	_anubis_dead_man_switch__serialize_switch_state__write_u64.9
	ldr	w0, [x29, 80]
	tbnz	w0, #31, L468
	ldr	x1, [x29, 56]
	ldp	w1, w3, [x1]
	cmp	w0, w3
	bgt	L444
	cmp	w1, w0
	bgt	L469
	ldr	x5, [x29, 48]
	sxtw	x2, w0
	mov	w4, 2147483647
	sub	x2, x2, x20
	ldr	w6, [x21, 400]
	strb	w6, [x5, x2]
	cmp	w0, w4
	beq	L470
	add	w2, w0, 1
	cmp	w2, w3
	bgt	L471
	ldr	w6, [x21, 404]
	sxtw	x3, w2
	add	w0, w0, 2
	sub	x3, x3, x20
	strb	w6, [x5, x3]
	cmp	w2, w4
	beq	L472
L444:
	subs	w0, w0, w1
	bvs	L447
	tbnz	w0, #31, L473
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI130:
	ret
	.p2align 2,,3
L459:
LCFI131:
	cmp	w6, w19
	bgt	L474
	ldrb	w2, [x21, 32]
	cmp	w2, 3
	bhi	L475
	sxtw	x1, w19
	mov	w4, 2147483647
	sub	x1, x1, x20
	strb	w2, [x0, x1]
	cmp	w19, w4
	beq	L476
	add	w1, w19, 1
	cmp	w3, w1
	blt	L451
	ldrb	w3, [x21, 33]
	cmp	w3, 3
	bhi	L477
	sxtw	x2, w1
	add	w19, w19, 2
	sub	x2, x2, x20
	strb	w3, [x0, x2]
	cmp	w1, w4
	bne	L423
	adrp	x0, lC4@PAGE
	mov	w1, 757
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L458:
	adrp	x0, lC4@PAGE
	mov	w1, 745
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L457:
	adrp	x0, lC4@PAGE
	mov	w1, 744
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L460:
	adrp	x0, lC4@PAGE
	mov	w1, 767
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L456:
	adrp	x0, lC4@PAGE
	mov	w1, 724
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L455:
	adrp	x0, lC23@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC23@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L473:
	adrp	x0, lC4@PAGE
	mov	w1, 798
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L447:
	adrp	x0, lC4@PAGE
	mov	w1, 798
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L468:
	adrp	x0, lC4@PAGE
	mov	w1, 788
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L467:
	adrp	x0, lC4@PAGE
	mov	w1, 778
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L466:
	adrp	x0, lC4@PAGE
	mov	w1, 777
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L465:
	adrp	x0, lC4@PAGE
	mov	w1, 777
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L464:
	adrp	x0, lC4@PAGE
	mov	w1, 776
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L463:
	adrp	x0, lC4@PAGE
	mov	w1, 769
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L462:
	adrp	x0, lC4@PAGE
	mov	w1, 768
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L461:
	adrp	x0, lC4@PAGE
	mov	w1, 768
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L469:
	adrp	x0, lC4@PAGE
	mov	w1, 789
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L470:
	adrp	x0, lC4@PAGE
	mov	w1, 790
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L471:
	mov	w0, w2
	b	L444
L472:
	adrp	x0, lC4@PAGE
	mov	w1, 795
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L474:
	adrp	x0, lC4@PAGE
	mov	w1, 751
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L475:
	adrp	x0, lC4@PAGE
	mov	w1, 751
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L476:
	adrp	x0, lC4@PAGE
	mov	w1, 752
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L451:
	mov	w19, w1
	b	L423
L477:
	adrp	x0, lC4@PAGE
	mov	w1, 756
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE56:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__deserialize_switch_state
_anubis_dead_man_switch__deserialize_switch_state:
LFB58:
	stp	x29, x30, [sp, -96]!
LCFI132:
	mov	x29, sp
LCFI133:
	add	x3, x29, 48
	stp	x0, x1, [x29, 48]
	add	x0, x29, 96
	stp	x19, x20, [sp, 16]
LCFI134:
	ldr	w19, [x1]
	str	x21, [sp, 32]
LCFI135:
	str	x3, [x29, 72]
	str	x0, [x29, 88]
	tbnz	w19, #31, L510
	mov	x20, x2
	adrp	x1, lC3@PAGE
	adrp	x2, lC2@PAGE
	mov	x3, 0
	add	x2, x2, lC2@PAGEOFF;
	mov	x0, x20
	add	x1, x1, lC3@PAGEOFF;
	bl	_anubis_dead_man_switch__init_switch
	ldr	x0, [x29, 56]
	ldp	w2, w3, [x0]
	sxtw	x0, w2
	add	x0, x0, 98
	cmp	x0, w3, sxtw
	bge	L506
	ldr	x6, [x29, 48]
	sxtw	x5, w19
	mov	x0, x20
	add	x4, x20, 32
	mov	w7, 2147483647
	.p2align 5,,15
L484:
	cmp	w3, w19
	blt	L481
	cmp	w2, w19
	bgt	L511
	sxtw	x1, w19
	sub	x1, x1, x5
	ldrb	w1, [x6, x1]
	strb	w1, [x0]
	cmp	w19, w7
	beq	L512
	add	w19, w19, 1
L481:
	add	x0, x0, 1
	cmp	x0, x4
	bne	L484
	cmp	w3, w19
	blt	L485
	cmp	w2, w19
	bgt	L513
	sxtw	x0, w19
	sub	x0, x0, x5
	ldrb	w0, [x6, x0]
	cmp	w0, 3
	bls	L514
L487:
	mov	w0, 2147483647
	cmp	w19, w0
	beq	L515
	add	w0, w19, 1
	cmp	w3, w0
	blt	L507
	sxtw	x1, w0
	sub	x1, x1, x5
	ldrb	w1, [x6, x1]
	cmp	w1, 3
	bls	L516
L489:
	mov	w1, 2147483647
	add	w19, w19, 2
	cmp	w0, w1
	beq	L517
L485:
	add	x21, x29, 64
	str	x5, [x29, 64]
	mov	x16, x21
	str	w19, [x29, 80]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	mov	x16, x21
	str	x0, [x20, 40]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	mov	x16, x21
	str	x0, [x20, 48]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	mov	x16, x21
	str	x0, [x20, 56]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	mov	x16, x21
	str	x0, [x20, 64]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	ldr	w1, [x29, 80]
	str	x0, [x20, 72]
	ldr	x3, [x29, 64]
	tbnz	w1, #31, L518
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L492
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L519
	ldr	x2, [x29, 48]
	sxtw	x0, w1
	sub	x0, x0, x3
	ldrb	w0, [x2, x0]
	cmp	w0, 3
	bls	L520
L494:
	mov	w0, 2147483647
	cmp	w1, w0
	beq	L521
	add	w1, w1, 1
L492:
	mov	x16, x21
	str	w1, [x29, 80]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	mov	x16, x21
	str	x0, [x20, 88]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	ldr	w1, [x29, 80]
	str	x0, [x20, 96]
	ldr	x3, [x29, 64]
	tbnz	w1, #31, L522
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L497
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L523
	ldr	x2, [x29, 48]
	sxtw	x0, w1
	sub	x0, x0, x3
	ldrb	w0, [x2, x0]
	cmp	w0, 5
	bls	L524
L499:
	mov	w0, 2147483647
	cmp	w1, w0
	beq	L525
	add	w1, w1, 1
L497:
	mov	x16, x21
	str	w1, [x29, 80]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	mov	x16, x21
	str	x0, [x20, 368]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	mov	x16, x21
	str	x0, [x20, 376]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	mov	x16, x21
	str	x0, [x20, 384]
	bl	_anubis_dead_man_switch__deserialize_switch_state__read_u64.10
	ldr	w1, [x29, 80]
	str	x0, [x20, 392]
	ldr	x3, [x29, 64]
	tbnz	w1, #31, L526
	ldr	x0, [x29, 56]
	ldr	w2, [x0, 4]
	cmp	w2, w1
	blt	L505
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L527
	ldr	x5, [x29, 48]
	sxtw	x0, w1
	mov	w4, 2147483647
	sub	x0, x0, x3
	ldrb	w0, [x5, x0]
	str	w0, [x20, 400]
	cmp	w1, w4
	beq	L528
	add	w1, w1, 1
	cmp	w2, w1
	blt	L505
	sxtw	x0, w1
	sub	x0, x0, x3
	ldrb	w0, [x5, x0]
	str	w0, [x20, 404]
	cmp	w1, w4
	beq	L529
L505:
	mov	w0, 1
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI136:
	ret
	.p2align 2,,3
L506:
LCFI137:
	mov	w0, 0
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI138:
	ret
	.p2align 2,,3
L520:
LCFI139:
	strb	w0, [x20, 80]
	b	L494
	.p2align 2,,3
L516:
	strb	w1, [x20, 33]
	b	L489
	.p2align 2,,3
L524:
	strb	w0, [x20, 360]
	b	L499
L514:
	strb	w0, [x20, 32]
	b	L487
L512:
	adrp	x0, lC4@PAGE
	mov	w1, 832
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L511:
	adrp	x0, lC4@PAGE
	mov	w1, 831
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L513:
	adrp	x0, lC4@PAGE
	mov	w1, 838
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L529:
	adrp	x0, lC4@PAGE
	mov	w1, 890
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L510:
	adrp	x0, lC4@PAGE
	mov	w1, 806
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L515:
	adrp	x0, lC4@PAGE
	mov	w1, 841
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L507:
	mov	w19, w0
	b	L485
L518:
	adrp	x0, lC4@PAGE
	mov	w1, 858
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L519:
	adrp	x0, lC4@PAGE
	mov	w1, 859
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L525:
	adrp	x0, lC4@PAGE
	mov	w1, 873
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L526:
	adrp	x0, lC4@PAGE
	mov	w1, 883
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L527:
	adrp	x0, lC4@PAGE
	mov	w1, 884
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L528:
	adrp	x0, lC4@PAGE
	mov	w1, 885
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L521:
	adrp	x0, lC4@PAGE
	mov	w1, 862
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L522:
	adrp	x0, lC4@PAGE
	mov	w1, 869
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L523:
	adrp	x0, lC4@PAGE
	mov	w1, 870
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L517:
	adrp	x0, lC4@PAGE
	mov	w1, 848
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE58:
	.const
lC3:
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
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__zeroize_switch_state
_anubis_dead_man_switch__zeroize_switch_state:
LFB60:
	movi	v31.4s, 0
	movi	v30.4s, 0
	add	x1, x0, 104
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	strh	wzr, [x0, 32]
	str	q30, [x0, 40]
	str	q30, [x0, 56]
	str	xzr, [x0, 72]
	strb	wzr, [x0, 80]
	str	q30, [x0, 88]
	str	q31, [x0, 104]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	stp	q31, q31, [x1, 112]
	stp	q31, q31, [x1, 144]
	stp	q31, q31, [x1, 176]
	stp	q31, q31, [x1, 208]
	str	q31, [x1, 240]
	strb	wzr, [x0, 360]
	stp	q30, q30, [x0, 368]
	str	xzr, [x0, 400]
	ret
LFE60:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__zeroize_council_state
_anubis_dead_man_switch__zeroize_council_state:
LFB61:
	mov	x2, 0
	.p2align 5,,15
L532:
	add	x1, x2, x2, lsl 2
	add	x2, x2, 1
	add	x1, x0, x1, lsl 4
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	stp	xzr, xzr, [x1, 32]
	stp	xzr, xzr, [x1, 48]
	str	xzr, [x1, 64]
	strb	wzr, [x1, 72]
	str	wzr, [x1, 76]
	cmp	x2, 9
	bne	L532
	add	x1, x0, 512
	str	wzr, [x0, 720]
	stp	xzr, xzr, [x1, 216]
	ret
LFE61:
	.align	2
	.p2align 5,,15
	.globl _anubis_dead_man_switch__zeroize_heartbeat
_anubis_dead_man_switch__zeroize_heartbeat:
LFB62:
	movi	v31.4s, 0
	add	x1, x0, 48
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	stp	xzr, xzr, [x0, 32]
	str	q31, [x0, 48]
	stp	q31, q31, [x1, 16]
	stp	q31, q31, [x1, 48]
	stp	q31, q31, [x1, 80]
	str	q31, [x0, 160]
	stp	xzr, xzr, [x0, 176]
	stp	xzr, xzr, [x0, 192]
	ret
LFE62:
	.const
	.align	3
_CSWTCH.349:
	.xword	10512000
	.xword	15768000
	.xword	5256000
	.xword	5256000
	.xword	5256000
_dao_address.11:
	.space 32
_parameter_idG.12:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	2
	.byte	4
_recovery_resultG.16:
	.byte	0
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	0
	.byte	7
	.byte	0
	.byte	6
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	2
	.byte	0
	.align	1
_recovery_resultT2.17:
	.byte	1
	.byte	9
	.align	1
_recovery_resultT1.18:
	.byte	6
	.byte	16
	.align	3
_recovery_resultP.19:
	.word	3
	.word	6
	.align	3
_pause_resultG.20:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	3
	.space 2
	.align	3
_heartbeat_resultG.24:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	2
	.byte	1
	.space 5
	.align	3
_recovery_stateG.28:
	.byte	0
	.byte	2
	.byte	4
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.space 2
	.align	3
_pause_stateG.32:
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	3
	.space 7
	.align	1
_pause_stateT2.33:
	.byte	8
	.byte	1
	.align	1
_pause_stateT1.34:
	.byte	4
	.byte	6
	.align	3
_pause_stateP.35:
	.word	1
	.word	11
	.align	3
_switch_stateG.36:
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.space 7
	.align	3
_governance_phaseG.40:
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
_privilege_typeG.44:
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
	.globl _anubis_dead_man_switch__parameter_idN
	.align	3
_anubis_dead_man_switch__parameter_idN:
	.byte	1
	.byte	16
	.byte	25
	.byte	35
	.byte	51
	.byte	64
	.byte	75
	.space 1
	.globl _anubis_dead_man_switch__parameter_idS
	.align	3
_anubis_dead_man_switch__parameter_idS:
	.ascii "BLOCK_GAS_LIMITMIN_STAKESLASH_RATEUNBONDING_PERIODPROOF_TIMEOUTPRIVACY_FEE"
	.globl _anubis_dead_man_switch__recovery_resultN
	.align	3
_anubis_dead_man_switch__recovery_resultN:
	.byte	1
	.byte	19
	.byte	36
	.byte	49
	.byte	57
	.byte	65
	.byte	78
	.byte	92
	.byte	102
	.space 7
	.globl _anubis_dead_man_switch__recovery_resultS
	.align	3
_anubis_dead_man_switch__recovery_resultS:
	.ascii "RECOVERY_INITIATEDIDENTITY_VERIFIEDVOTE_RECORDEDAPPROVEDREJECTEDINVALID_PROOFALREADY_ACTIVEVOTE_ENDED"
	.globl _anubis_dead_man_switch__pause_resultN
	.align	3
_anubis_dead_man_switch__pause_resultN:
	.byte	1
	.byte	7
	.byte	21
	.byte	33
	.byte	50
	.byte	67
	.space 2
	.globl _anubis_dead_man_switch__pause_resultS
	.align	3
_anubis_dead_man_switch__pause_resultS:
	.ascii "PAUSEDALREADY_PAUSEDUNAUTHORIZEDDURATION_EXCEEDEDPHASE_NOT_ALLOWED"
	.globl _anubis_dead_man_switch__heartbeat_resultN
	.align	3
_anubis_dead_man_switch__heartbeat_resultN:
	.byte	1
	.byte	9
	.byte	26
	.byte	37
	.byte	61
	.space 3
	.globl _anubis_dead_man_switch__heartbeat_resultS
	.align	3
_anubis_dead_man_switch__heartbeat_resultS:
	.ascii "RECORDEDINVALID_SIGNATURENOT_BUILDERSWITCH_ALREADY_TRIGGERED"
	.globl _anubis_dead_man_switch__recovery_stateN
	.align	3
_anubis_dead_man_switch__recovery_stateN:
	.byte	1
	.byte	5
	.byte	13
	.byte	19
	.byte	27
	.byte	35
	.byte	43
	.space 1
	.globl _anubis_dead_man_switch__recovery_stateS
	.align	3
_anubis_dead_man_switch__recovery_stateS:
	.ascii "NONEPROPOSEDVOTINGAPPROVEDREJECTEDEXECUTED"
	.globl _anubis_dead_man_switch__pause_stateN
	.align	3
_anubis_dead_man_switch__pause_stateN:
	.byte	1
	.byte	11
	.byte	28
	.byte	45
	.byte	58
	.space 3
	.globl _anubis_dead_man_switch__pause_stateS
	.align	3
_anubis_dead_man_switch__pause_stateS:
	.ascii "NOT_PAUSEDPAUSED_BY_BUILDERPAUSED_BY_COUNCILPAUSED_BY_DAO"
	.globl _anubis_dead_man_switch__switch_stateN
	.align	3
_anubis_dead_man_switch__switch_stateN:
	.byte	1
	.byte	7
	.byte	14
	.byte	23
	.byte	32
	.space 3
	.globl _anubis_dead_man_switch__switch_stateS
	.align	3
_anubis_dead_man_switch__switch_stateS:
	.ascii "ACTIVEWARNINGTRIGGEREDRECOVERED"
	.globl _anubis_dead_man_switch__governance_phaseN
	.align	3
_anubis_dead_man_switch__governance_phaseN:
	.byte	1
	.byte	23
	.byte	44
	.byte	62
	.byte	82
	.space 3
	.globl _anubis_dead_man_switch__governance_phaseS
	.align	3
_anubis_dead_man_switch__governance_phaseS:
	.ascii "PHASE1_BUILDER_CONTROLPHASE2_SHARED_CONTROLPHASE3_DAO_CONTROLPHASE4_FULL_AUTONOMY"
	.globl _anubis_dead_man_switch__privilege_typeN
	.align	3
_anubis_dead_man_switch__privilege_typeN:
	.byte	1
	.byte	17
	.byte	32
	.byte	48
	.byte	67
	.byte	84
	.byte	99
	.space 1
	.globl _anubis_dead_man_switch__privilege_typeS
	.align	3
_anubis_dead_man_switch__privilege_typeS:
	.ascii "PROTOCOL_UPGRADEEMERGENCY_PAUSEPARAMETER_TUNINGBUG_BOUNTY_APPROVALVALIDATOR_REMOVALTREASURY_ACCESS"
	.globl _anubis_dead_man_switch_E
	.data
	.align	1
_anubis_dead_man_switch_E:
	.space 2
	.const
	.align	3
lC21:
	.word	0
	.word	31
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
	.quad	LFB57-.
	.set L$set$2,LFE57-LFB57
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB57
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
	.quad	LFB59-.
	.set L$set$8,LFE59-LFB59
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB59
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
	.quad	LFB165-.
	.set L$set$29,LFE165-LFB165
	.quad L$set$29
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$30,LEFDE19-LASFDE19
	.long L$set$30
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB167-.
	.set L$set$31,LFE167-LFB167
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
	.quad	LFB169-.
	.set L$set$35,LFE169-LFB169
	.quad L$set$35
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$36,LEFDE25-LASFDE25
	.long L$set$36
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB171-.
	.set L$set$37,LFE171-LFB171
	.quad L$set$37
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$38,LEFDE27-LASFDE27
	.long L$set$38
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB13-.
	.set L$set$39,LFE13-LFB13
	.quad L$set$39
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$40,LEFDE29-LASFDE29
	.long L$set$40
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB14-.
	.set L$set$41,LFE14-LFB14
	.quad L$set$41
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$42,LEFDE31-LASFDE31
	.long L$set$42
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB15-.
	.set L$set$43,LFE15-LFB15
	.quad L$set$43
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$44,LEFDE33-LASFDE33
	.long L$set$44
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB16-.
	.set L$set$45,LFE16-LFB16
	.quad L$set$45
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$46,LEFDE35-LASFDE35
	.long L$set$46
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB17-.
	.set L$set$47,LFE17-LFB17
	.quad L$set$47
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$48,LEFDE37-LASFDE37
	.long L$set$48
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB18-.
	.set L$set$49,LFE18-LFB18
	.quad L$set$49
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI11-LFB18
	.long L$set$50
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$51,LCFI12-LCFI11
	.long L$set$51
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$52,LCFI13-LCFI12
	.long L$set$52
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI14-LCFI13
	.long L$set$53
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$54,LEFDE39-LASFDE39
	.long L$set$54
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB20-.
	.set L$set$55,LFE20-LFB20
	.quad L$set$55
	.uleb128 0
	.byte	0x4
	.set L$set$56,LCFI15-LFB20
	.long L$set$56
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$57,LCFI16-LCFI15
	.long L$set$57
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$58,LCFI17-LCFI16
	.long L$set$58
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$59,LCFI18-LCFI17
	.long L$set$59
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$60,LEFDE41-LASFDE41
	.long L$set$60
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB22-.
	.set L$set$61,LFE22-LFB22
	.quad L$set$61
	.uleb128 0
	.byte	0x4
	.set L$set$62,LCFI19-LFB22
	.long L$set$62
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$63,LCFI20-LCFI19
	.long L$set$63
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$64,LCFI21-LCFI20
	.long L$set$64
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI22-LCFI21
	.long L$set$65
	.byte	0xb
	.byte	0x4
	.set L$set$66,LCFI23-LCFI22
	.long L$set$66
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$67,LCFI24-LCFI23
	.long L$set$67
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$68,LEFDE43-LASFDE43
	.long L$set$68
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB24-.
	.set L$set$69,LFE24-LFB24
	.quad L$set$69
	.uleb128 0
	.byte	0x4
	.set L$set$70,LCFI25-LFB24
	.long L$set$70
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$71,LCFI26-LCFI25
	.long L$set$71
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$72,LEFDE45-LASFDE45
	.long L$set$72
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB25-.
	.set L$set$73,LFE25-LFB25
	.quad L$set$73
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$74,LEFDE47-LASFDE47
	.long L$set$74
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB26-.
	.set L$set$75,LFE26-LFB26
	.quad L$set$75
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$76,LEFDE49-LASFDE49
	.long L$set$76
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB29-.
	.set L$set$77,LFE29-LFB29
	.quad L$set$77
	.uleb128 0
	.byte	0x4
	.set L$set$78,LCFI27-LFB29
	.long L$set$78
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$79,LCFI28-LCFI27
	.long L$set$79
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$80,LEFDE51-LASFDE51
	.long L$set$80
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB31-.
	.set L$set$81,LFE31-LFB31
	.quad L$set$81
	.uleb128 0
	.byte	0x4
	.set L$set$82,LCFI29-LFB31
	.long L$set$82
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$83,LCFI30-LCFI29
	.long L$set$83
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$84,LCFI31-LCFI30
	.long L$set$84
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$85,LCFI32-LCFI31
	.long L$set$85
	.byte	0xb
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$86,LEFDE53-LASFDE53
	.long L$set$86
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB27-.
	.set L$set$87,LFE27-LFB27
	.quad L$set$87
	.uleb128 0
	.byte	0x4
	.set L$set$88,LCFI33-LFB27
	.long L$set$88
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$89,LCFI34-LCFI33
	.long L$set$89
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$90,LCFI35-LCFI34
	.long L$set$90
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$91,LCFI36-LCFI35
	.long L$set$91
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$92,LCFI37-LCFI36
	.long L$set$92
	.byte	0xb
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$93,LEFDE55-LASFDE55
	.long L$set$93
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB32-.
	.set L$set$94,LFE32-LFB32
	.quad L$set$94
	.uleb128 0
	.byte	0x4
	.set L$set$95,LCFI38-LFB32
	.long L$set$95
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$96,LCFI39-LCFI38
	.long L$set$96
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$97,LCFI40-LCFI39
	.long L$set$97
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$98,LCFI41-LCFI40
	.long L$set$98
	.byte	0xb
	.byte	0x4
	.set L$set$99,LCFI42-LCFI41
	.long L$set$99
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$100,LCFI43-LCFI42
	.long L$set$100
	.byte	0xb
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$101,LEFDE57-LASFDE57
	.long L$set$101
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB34-.
	.set L$set$102,LFE34-LFB34
	.quad L$set$102
	.uleb128 0
	.byte	0x4
	.set L$set$103,LCFI44-LFB34
	.long L$set$103
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$104,LCFI45-LCFI44
	.long L$set$104
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$105,LCFI46-LCFI45
	.long L$set$105
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$106,LCFI47-LCFI46
	.long L$set$106
	.byte	0xb
	.byte	0x4
	.set L$set$107,LCFI48-LCFI47
	.long L$set$107
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$108,LCFI49-LCFI48
	.long L$set$108
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$109,LEFDE59-LASFDE59
	.long L$set$109
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB35-.
	.set L$set$110,LFE35-LFB35
	.quad L$set$110
	.uleb128 0
	.byte	0x4
	.set L$set$111,LCFI50-LFB35
	.long L$set$111
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$112,LCFI51-LCFI50
	.long L$set$112
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$113,LCFI52-LCFI51
	.long L$set$113
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$114,LCFI53-LCFI52
	.long L$set$114
	.byte	0xb
	.byte	0x4
	.set L$set$115,LCFI54-LCFI53
	.long L$set$115
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$116,LCFI55-LCFI54
	.long L$set$116
	.byte	0xb
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$117,LEFDE61-LASFDE61
	.long L$set$117
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB36-.
	.set L$set$118,LFE36-LFB36
	.quad L$set$118
	.uleb128 0
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$119,LEFDE63-LASFDE63
	.long L$set$119
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB37-.
	.set L$set$120,LFE37-LFB37
	.quad L$set$120
	.uleb128 0
	.byte	0x4
	.set L$set$121,LCFI56-LFB37
	.long L$set$121
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$122,LCFI57-LCFI56
	.long L$set$122
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$123,LEFDE65-LASFDE65
	.long L$set$123
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB39-.
	.set L$set$124,LFE39-LFB39
	.quad L$set$124
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI58-LFB39
	.long L$set$125
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$126,LCFI59-LCFI58
	.long L$set$126
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$127,LCFI60-LCFI59
	.long L$set$127
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$128,LCFI61-LCFI60
	.long L$set$128
	.byte	0xb
	.byte	0x4
	.set L$set$129,LCFI62-LCFI61
	.long L$set$129
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$130,LCFI63-LCFI62
	.long L$set$130
	.byte	0xb
	.byte	0x4
	.set L$set$131,LCFI64-LCFI63
	.long L$set$131
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$132,LCFI65-LCFI64
	.long L$set$132
	.byte	0xb
	.byte	0x4
	.set L$set$133,LCFI66-LCFI65
	.long L$set$133
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$134,LCFI67-LCFI66
	.long L$set$134
	.byte	0xb
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$135,LEFDE67-LASFDE67
	.long L$set$135
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB40-.
	.set L$set$136,LFE40-LFB40
	.quad L$set$136
	.uleb128 0
	.byte	0x4
	.set L$set$137,LCFI68-LFB40
	.long L$set$137
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$138,LCFI69-LCFI68
	.long L$set$138
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$139,LEFDE69-LASFDE69
	.long L$set$139
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB41-.
	.set L$set$140,LFE41-LFB41
	.quad L$set$140
	.uleb128 0
	.byte	0x4
	.set L$set$141,LCFI70-LFB41
	.long L$set$141
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$142,LCFI71-LCFI70
	.long L$set$142
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$143,LCFI72-LCFI71
	.long L$set$143
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$144,LCFI73-LCFI72
	.long L$set$144
	.byte	0xb
	.byte	0x4
	.set L$set$145,LCFI74-LCFI73
	.long L$set$145
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$146,LCFI75-LCFI74
	.long L$set$146
	.byte	0xb
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$147,LEFDE71-LASFDE71
	.long L$set$147
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB42-.
	.set L$set$148,LFE42-LFB42
	.quad L$set$148
	.uleb128 0
	.byte	0x4
	.set L$set$149,LCFI76-LFB42
	.long L$set$149
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$150,LCFI77-LCFI76
	.long L$set$150
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$151,LCFI78-LCFI77
	.long L$set$151
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$152,LCFI79-LCFI78
	.long L$set$152
	.byte	0xb
	.byte	0x4
	.set L$set$153,LCFI80-LCFI79
	.long L$set$153
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$154,LCFI81-LCFI80
	.long L$set$154
	.byte	0xb
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$155,LEFDE73-LASFDE73
	.long L$set$155
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB43-.
	.set L$set$156,LFE43-LFB43
	.quad L$set$156
	.uleb128 0
	.byte	0x4
	.set L$set$157,LCFI82-LFB43
	.long L$set$157
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$158,LCFI83-LCFI82
	.long L$set$158
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$159,LCFI84-LCFI83
	.long L$set$159
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$160,LCFI85-LCFI84
	.long L$set$160
	.byte	0xb
	.byte	0x4
	.set L$set$161,LCFI86-LCFI85
	.long L$set$161
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$162,LCFI87-LCFI86
	.long L$set$162
	.byte	0xb
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$163,LEFDE75-LASFDE75
	.long L$set$163
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB44-.
	.set L$set$164,LFE44-LFB44
	.quad L$set$164
	.uleb128 0
	.byte	0x4
	.set L$set$165,LCFI88-LFB44
	.long L$set$165
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$166,LCFI89-LCFI88
	.long L$set$166
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$167,LEFDE77-LASFDE77
	.long L$set$167
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB46-.
	.set L$set$168,LFE46-LFB46
	.quad L$set$168
	.uleb128 0
	.byte	0x4
	.set L$set$169,LCFI90-LFB46
	.long L$set$169
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$170,LCFI91-LCFI90
	.long L$set$170
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$171,LCFI92-LCFI91
	.long L$set$171
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$172,LCFI93-LCFI92
	.long L$set$172
	.byte	0xb
	.byte	0x4
	.set L$set$173,LCFI94-LCFI93
	.long L$set$173
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$174,LCFI95-LCFI94
	.long L$set$174
	.byte	0xb
	.align	3
LEFDE77:
LSFDE79:
	.set L$set$175,LEFDE79-LASFDE79
	.long L$set$175
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB47-.
	.set L$set$176,LFE47-LFB47
	.quad L$set$176
	.uleb128 0
	.align	3
LEFDE79:
LSFDE81:
	.set L$set$177,LEFDE81-LASFDE81
	.long L$set$177
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB48-.
	.set L$set$178,LFE48-LFB48
	.quad L$set$178
	.uleb128 0
	.byte	0x4
	.set L$set$179,LCFI96-LFB48
	.long L$set$179
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$180,LCFI97-LCFI96
	.long L$set$180
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$181,LCFI98-LCFI97
	.long L$set$181
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$182,LCFI99-LCFI98
	.long L$set$182
	.byte	0xb
	.byte	0x4
	.set L$set$183,LCFI100-LCFI99
	.long L$set$183
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$184,LCFI101-LCFI100
	.long L$set$184
	.byte	0xb
	.align	3
LEFDE81:
LSFDE83:
	.set L$set$185,LEFDE83-LASFDE83
	.long L$set$185
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB49-.
	.set L$set$186,LFE49-LFB49
	.quad L$set$186
	.uleb128 0
	.byte	0x4
	.set L$set$187,LCFI102-LFB49
	.long L$set$187
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$188,LCFI103-LCFI102
	.long L$set$188
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$189,LCFI104-LCFI103
	.long L$set$189
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$190,LCFI105-LCFI104
	.long L$set$190
	.byte	0xb
	.align	3
LEFDE83:
LSFDE85:
	.set L$set$191,LEFDE85-LASFDE85
	.long L$set$191
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB50-.
	.set L$set$192,LFE50-LFB50
	.quad L$set$192
	.uleb128 0
	.byte	0x4
	.set L$set$193,LCFI106-LFB50
	.long L$set$193
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$194,LCFI107-LCFI106
	.long L$set$194
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE85:
LSFDE87:
	.set L$set$195,LEFDE87-LASFDE87
	.long L$set$195
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB52-.
	.set L$set$196,LFE52-LFB52
	.quad L$set$196
	.uleb128 0
	.byte	0x4
	.set L$set$197,LCFI108-LFB52
	.long L$set$197
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$198,LCFI109-LCFI108
	.long L$set$198
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$199,LCFI110-LCFI109
	.long L$set$199
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$200,LCFI111-LCFI110
	.long L$set$200
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$201,LCFI112-LCFI111
	.long L$set$201
	.byte	0xb
	.align	3
LEFDE87:
LSFDE89:
	.set L$set$202,LEFDE89-LASFDE89
	.long L$set$202
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB53-.
	.set L$set$203,LFE53-LFB53
	.quad L$set$203
	.uleb128 0
	.byte	0x4
	.set L$set$204,LCFI113-LFB53
	.long L$set$204
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$205,LCFI114-LCFI113
	.long L$set$205
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$206,LCFI115-LCFI114
	.long L$set$206
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$207,LCFI116-LCFI115
	.long L$set$207
	.byte	0xb
	.byte	0x4
	.set L$set$208,LCFI117-LCFI116
	.long L$set$208
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$209,LCFI118-LCFI117
	.long L$set$209
	.byte	0xb
	.align	3
LEFDE89:
LSFDE91:
	.set L$set$210,LEFDE91-LASFDE91
	.long L$set$210
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB54-.
	.set L$set$211,LFE54-LFB54
	.quad L$set$211
	.uleb128 0
	.byte	0x4
	.set L$set$212,LCFI119-LFB54
	.long L$set$212
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$213,LCFI120-LCFI119
	.long L$set$213
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$214,LCFI121-LCFI120
	.long L$set$214
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$215,LCFI122-LCFI121
	.long L$set$215
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$216,LCFI123-LCFI122
	.long L$set$216
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
	.set L$set$217,LCFI124-LCFI123
	.long L$set$217
	.byte	0xb
	.byte	0x4
	.set L$set$218,LCFI125-LCFI124
	.long L$set$218
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
	.set L$set$219,LCFI126-LCFI125
	.long L$set$219
	.byte	0xb
	.align	3
LEFDE91:
LSFDE93:
	.set L$set$220,LEFDE93-LASFDE93
	.long L$set$220
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB55-.
	.set L$set$221,LFE55-LFB55
	.quad L$set$221
	.uleb128 0
	.align	3
LEFDE93:
LSFDE95:
	.set L$set$222,LEFDE95-LASFDE95
	.long L$set$222
LASFDE95:
	.long	LASFDE95-EH_frame1
	.quad	LFB56-.
	.set L$set$223,LFE56-LFB56
	.quad L$set$223
	.uleb128 0
	.byte	0x4
	.set L$set$224,LCFI127-LFB56
	.long L$set$224
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$225,LCFI128-LCFI127
	.long L$set$225
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$226,LCFI129-LCFI128
	.long L$set$226
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$227,LCFI130-LCFI129
	.long L$set$227
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
	.set L$set$228,LCFI131-LCFI130
	.long L$set$228
	.byte	0xb
	.align	3
LEFDE95:
LSFDE97:
	.set L$set$229,LEFDE97-LASFDE97
	.long L$set$229
LASFDE97:
	.long	LASFDE97-EH_frame1
	.quad	LFB58-.
	.set L$set$230,LFE58-LFB58
	.quad L$set$230
	.uleb128 0
	.byte	0x4
	.set L$set$231,LCFI132-LFB58
	.long L$set$231
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$232,LCFI133-LCFI132
	.long L$set$232
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$233,LCFI134-LCFI133
	.long L$set$233
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$234,LCFI135-LCFI134
	.long L$set$234
	.byte	0x95
	.uleb128 0x8
	.byte	0x4
	.set L$set$235,LCFI136-LCFI135
	.long L$set$235
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
	.set L$set$236,LCFI137-LCFI136
	.long L$set$236
	.byte	0xb
	.byte	0x4
	.set L$set$237,LCFI138-LCFI137
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
	.set L$set$238,LCFI139-LCFI138
	.long L$set$238
	.byte	0xb
	.align	3
LEFDE97:
LSFDE99:
	.set L$set$239,LEFDE99-LASFDE99
	.long L$set$239
LASFDE99:
	.long	LASFDE99-EH_frame1
	.quad	LFB60-.
	.set L$set$240,LFE60-LFB60
	.quad L$set$240
	.uleb128 0
	.align	3
LEFDE99:
LSFDE101:
	.set L$set$241,LEFDE101-LASFDE101
	.long L$set$241
LASFDE101:
	.long	LASFDE101-EH_frame1
	.quad	LFB61-.
	.set L$set$242,LFE61-LFB61
	.quad L$set$242
	.uleb128 0
	.align	3
LEFDE101:
LSFDE103:
	.set L$set$243,LEFDE103-LASFDE103
	.long L$set$243
LASFDE103:
	.long	LASFDE103-EH_frame1
	.quad	LFB62-.
	.set L$set$244,LFE62-LFB62
	.quad L$set$244
	.uleb128 0
	.align	3
LEFDE103:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
