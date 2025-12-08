	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__Tfunction_selectorBIP
_aegis_contract__Tfunction_selectorBIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__Tparameter_slotBIP
_aegis_contract__Tparameter_slotBIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__Treturn_arrayBIP
_aegis_contract__Treturn_arrayBIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__return_dataIP
_aegis_contract__return_dataIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__Tparameters_arrayBIP
_aegis_contract__Tparameters_arrayBIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__call_dataIP
_aegis_contract__call_dataIP:
LFB77:
	ret
LFE77:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__function_mutabilityH
_aegis_contract__function_mutabilityH:
LFB8:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L11
	sub	w1, w1, w2
	cmp	w1, 4
	bgt	L12
L11:
	adrp	x1, _function_mutabilityG.5@PAGE
	mov	x2, 0
	add	x1, x1, _function_mutabilityG.5@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L12:
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
	adrp	x1, _function_mutabilityG.5@PAGE
	sxtw	x2, w2
	add	x1, x1, _function_mutabilityG.5@PAGEOFF;
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__function_visibilityH
_aegis_contract__function_visibilityH:
LFB9:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L16
	sub	w1, w1, w2
	cmp	w1, 4
	bgt	L17
L16:
	adrp	x1, _function_visibilityG.1@PAGE
	mov	x2, 0
	add	x1, x1, _function_visibilityG.1@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L17:
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
	adrp	x1, _function_visibilityG.1@PAGE
	sxtw	x2, w2
	add	x1, x1, _function_visibilityG.1@PAGEOFF;
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__function_entryIP
_aegis_contract__function_entryIP:
LFB79:
	ret
LFE79:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__Tfunction_tableBIP
_aegis_contract__Tfunction_tableBIP:
LFB11:
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__Ttopic_arrayBIP
_aegis_contract__Ttopic_arrayBIP:
LFB12:
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__Tindexed_flagsBIP
_aegis_contract__Tindexed_flagsBIP:
LFB13:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__event_entryIP
_aegis_contract__event_entryIP:
LFB81:
	ret
LFE81:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__Tevent_tableBIP
_aegis_contract__Tevent_tableBIP:
LFB15:
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__contract_manifestIP
_aegis_contract__contract_manifestIP:
LFB83:
	ret
LFE83:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__contract_stateIP
_aegis_contract__contract_stateIP:
LFB85:
	ret
LFE85:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__entry_point_resultIP
_aegis_contract__entry_point_resultIP:
LFB87:
	ret
LFE87:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__encode_u256
_aegis_contract__encode_u256:
LFB19:
	stp	x29, x30, [sp, -64]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
LCFI2:
	add	x19, x29, 32
	mov	x20, x8
	mov	x8, x19
	bl	_aegis_u256__to_bytes_be
	ldp	q31, q30, [x19]
	stp	q31, q30, [x20]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI3:
	ret
LFE19:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__decode_u256
_aegis_contract__decode_u256:
LFB20:
	mov	x2, x0
	stp	x29, x30, [sp, -48]!
LCFI4:
	mov	x29, sp
LCFI5:
	add	x1, x29, 16
	mov	x0, x1
	ldp	q31, q30, [x2]
	stp	q31, q30, [x1]
	bl	_aegis_u256__from_bytes_be
	ldp	x29, x30, [sp], 48
LCFI6:
	ret
LFE20:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__encode_address
_aegis_contract__encode_address:
LFB21:
	ldp	q31, q30, [x0]
	stp	q31, q30, [x8]
	ret
LFE21:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__decode_address
_aegis_contract__decode_address:
LFB22:
	ldp	q31, q30, [x0]
	stp	q31, q30, [x8]
	ret
LFE22:
	.const
	.align	3
lC2:
	.ascii "aegis_contract.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__encode_bool
_aegis_contract__encode_bool:
LFB23:
	movi	v31.4s, 0
	str	q31, [x8]
	str	q31, [x8, 15]
	cmp	w0, 1
	bhi	L38
	strb	w0, [x8, 31]
	ret
L38:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI7:
	mov	w1, 56
	mov	x29, sp
LCFI8:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__decode_bool
_aegis_contract__decode_bool:
LFB24:
	add	x2, x0, 32
	b	L41
	.p2align 2,,3
L45:
	cmp	x2, x0
	beq	L44
L41:
	ldrb	w1, [x0]
	add	x0, x0, 1
	cbz	w1, L45
	mov	w0, 1
	ret
	.p2align 2,,3
L44:
	mov	w0, 0
	ret
LFE24:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__compute_selector
_aegis_contract__compute_selector:
LFB25:
	ldr	w0, [x0]
	ret
LFE25:
	.const
	.align	3
lC3:
	.ascii "aegis_contract.ads"
	.space 1
	.align	3
lC4:
	.ascii "failed precondition from aegis_contract.ads:245"
	.align	3
lC5:
	.ascii "failed postcondition from aegis_contract.ads:246"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__find_function
_aegis_contract__find_function:
LFB26:
	stp	x29, x30, [sp, -16]!
LCFI9:
	mov	x29, sp
LCFI10:
	tbnz	w1, #31, L56
	cmp	w1, 256
	bgt	L57
	sub	w5, w1, #1
	cbz	w1, L53
	mov	x4, x0
	mov	w0, -1
	b	L52
	.p2align 2,,3
L59:
	cmp	w5, w0
	beq	L58
L52:
	add	w0, w0, 1
	sbfiz	x3, x0, 3, 16
	sub	x3, x3, w0, sxth
	lsl	x3, x3, 3
	ldr	w3, [x4, x3]
	cmp	w3, w2
	bne	L59
	cmp	w1, w0
	blt	L60
	ldp	x29, x30, [sp], 16
LCFI11:
	ret
	.p2align 2,,3
L53:
LCFI12:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI13:
	ret
	.p2align 2,,3
L58:
LCFI14:
	mov	w0, w1
	ldp	x29, x30, [sp], 16
LCFI15:
	ret
L60:
LCFI16:
	adrp	x0, lC5@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L57:
	adrp	x0, lC4@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L56:
	adrp	x0, lC3@PAGE
	mov	w1, 245
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.const
	.align	2
lC1:
	.word	1
	.word	48
	.align	2
lC0:
	.word	1
	.word	47
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__validate_manifest
_aegis_contract__validate_manifest:
LFB28:
	stp	x29, x30, [sp, -16]!
LCFI17:
	mov	x2, x0
	mov	x29, sp
LCFI18:
	ldr	w0, [x0, 14528]
	tbnz	w0, #31, L73
	cmp	w0, 256
	bgt	L68
	add	x0, x2, 16384
	ldr	w0, [x0, 708]
	tbnz	w0, #31, L74
	cmp	w0, 64
	bgt	L68
	ldr	w1, [x2, 128]
	tbnz	w1, #31, L75
	mov	w0, 0
	cmp	w1, 64
	bgt	L63
	add	x1, x2, 32
	add	x2, x2, 64
	b	L66
	.p2align 2,,3
L76:
	add	x1, x1, 1
	cmp	x1, x2
	beq	L63
L66:
	ldrb	w0, [x1]
	cbz	w0, L76
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI19:
	ret
	.p2align 2,,3
L68:
LCFI20:
	mov	w0, 0
L63:
	ldp	x29, x30, [sp], 16
LCFI21:
	ret
L75:
LCFI22:
	adrp	x0, lC2@PAGE
	mov	w1, 115
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L74:
	adrp	x0, lC2@PAGE
	mov	w1, 111
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L73:
	adrp	x0, lC2@PAGE
	mov	w1, 107
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE28:
	.align	2
	.p2align 5,,15
	.globl _aegis_contract__can_call_function
_aegis_contract__can_call_function:
LFB29:
	stp	x29, x30, [sp, -16]!
LCFI23:
	mov	x29, sp
LCFI24:
	ldrb	w3, [x0, 45]
	cmp	w3, 3
	bhi	L87
	sub	w3, w3, #2
	and	w3, w3, 255
	cmp	w3, 1
	bls	L85
	cmp	w1, 1
	bhi	L88
	ldrb	w0, [x0, 44]
	cmp	w0, 3
	cbnz	w1, L89
	bhi	L84
L83:
	cmp	w2, 1
	bhi	L84
	cmp	w2, 0
	ccmp	w0, 3, 4, ne
	cset	w0, eq
	ldp	x29, x30, [sp], 16
LCFI25:
	ret
	.p2align 2,,3
L89:
LCFI26:
	bhi	L90
	cmp	w0, 1
	bls	L83
L85:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI27:
	ret
L84:
LCFI28:
	adrp	x0, lC2@PAGE
	mov	w1, 151
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L87:
	adrp	x0, lC2@PAGE
	mov	w1, 136
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L88:
	adrp	x0, lC2@PAGE
	mov	w1, 141
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L90:
	adrp	x0, lC2@PAGE
	mov	w1, 142
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE29:
	.const
	.align	3
_function_visibilityG.1:
	.byte	2
	.byte	1
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.space 7
	.align	3
_function_mutabilityG.5:
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.space 7
	.globl _aegis_contract__function_visibilityN
	.align	3
_aegis_contract__function_visibilityN:
	.byte	1
	.byte	11
	.byte	23
	.byte	35
	.byte	46
	.space 3
	.globl _aegis_contract__function_visibilityS
	.align	3
_aegis_contract__function_visibilityS:
	.ascii "VIS_PUBLICVIS_EXTERNALVIS_INTERNALVIS_PRIVATE"
	.globl _aegis_contract__function_mutabilityN
	.align	3
_aegis_contract__function_mutabilityN:
	.byte	1
	.byte	9
	.byte	17
	.byte	31
	.byte	42
	.space 3
	.globl _aegis_contract__function_mutabilityS
	.align	3
_aegis_contract__function_mutabilityS:
	.ascii "MUT_PUREMUT_VIEWMUT_NONPAYABLEMUT_PAYABLE"
	.globl _aegis_contract__empty_return
	.align	2
_aegis_contract__empty_return:
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
	.word	0
	.byte	1
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
	.space 3
	.globl _aegis_contract__null_selector
	.align	2
_aegis_contract__null_selector:
	.space 4
	.globl _aegis_contract_E
	.data
	.align	1
_aegis_contract_E:
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
	.quad	LFB3-.
	.set L$set$4,LFE3-LFB3
	.quad L$set$4
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$5,LEFDE5-LASFDE5
	.long L$set$5
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB4-.
	.set L$set$6,LFE4-LFB4
	.quad L$set$6
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$7,LEFDE7-LASFDE7
	.long L$set$7
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB5-.
	.set L$set$8,LFE5-LFB5
	.quad L$set$8
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$9,LEFDE9-LASFDE9
	.long L$set$9
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB6-.
	.set L$set$10,LFE6-LFB6
	.quad L$set$10
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$11,LEFDE11-LASFDE11
	.long L$set$11
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB77-.
	.set L$set$12,LFE77-LFB77
	.quad L$set$12
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$13,LEFDE13-LASFDE13
	.long L$set$13
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB8-.
	.set L$set$14,LFE8-LFB8
	.quad L$set$14
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$15,LEFDE15-LASFDE15
	.long L$set$15
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB9-.
	.set L$set$16,LFE9-LFB9
	.quad L$set$16
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$17,LEFDE17-LASFDE17
	.long L$set$17
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB79-.
	.set L$set$18,LFE79-LFB79
	.quad L$set$18
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$19,LEFDE19-LASFDE19
	.long L$set$19
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB11-.
	.set L$set$20,LFE11-LFB11
	.quad L$set$20
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$21,LEFDE21-LASFDE21
	.long L$set$21
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB12-.
	.set L$set$22,LFE12-LFB12
	.quad L$set$22
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$23,LEFDE23-LASFDE23
	.long L$set$23
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB13-.
	.set L$set$24,LFE13-LFB13
	.quad L$set$24
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$25,LEFDE25-LASFDE25
	.long L$set$25
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB81-.
	.set L$set$26,LFE81-LFB81
	.quad L$set$26
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$27,LEFDE27-LASFDE27
	.long L$set$27
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB15-.
	.set L$set$28,LFE15-LFB15
	.quad L$set$28
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$29,LEFDE29-LASFDE29
	.long L$set$29
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB83-.
	.set L$set$30,LFE83-LFB83
	.quad L$set$30
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$31,LEFDE31-LASFDE31
	.long L$set$31
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB85-.
	.set L$set$32,LFE85-LFB85
	.quad L$set$32
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$33,LEFDE33-LASFDE33
	.long L$set$33
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB87-.
	.set L$set$34,LFE87-LFB87
	.quad L$set$34
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$35,LEFDE35-LASFDE35
	.long L$set$35
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB19-.
	.set L$set$36,LFE19-LFB19
	.quad L$set$36
	.uleb128 0
	.byte	0x4
	.set L$set$37,LCFI0-LFB19
	.long L$set$37
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$38,LCFI1-LCFI0
	.long L$set$38
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$39,LCFI2-LCFI1
	.long L$set$39
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$40,LCFI3-LCFI2
	.long L$set$40
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$41,LEFDE37-LASFDE37
	.long L$set$41
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB20-.
	.set L$set$42,LFE20-LFB20
	.quad L$set$42
	.uleb128 0
	.byte	0x4
	.set L$set$43,LCFI4-LFB20
	.long L$set$43
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$44,LCFI5-LCFI4
	.long L$set$44
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$45,LCFI6-LCFI5
	.long L$set$45
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$46,LEFDE39-LASFDE39
	.long L$set$46
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB21-.
	.set L$set$47,LFE21-LFB21
	.quad L$set$47
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$48,LEFDE41-LASFDE41
	.long L$set$48
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB22-.
	.set L$set$49,LFE22-LFB22
	.quad L$set$49
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$50,LEFDE43-LASFDE43
	.long L$set$50
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB23-.
	.set L$set$51,LFE23-LFB23
	.quad L$set$51
	.uleb128 0
	.byte	0x4
	.set L$set$52,LCFI7-LFB23
	.long L$set$52
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$53,LCFI8-LCFI7
	.long L$set$53
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$54,LEFDE45-LASFDE45
	.long L$set$54
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB24-.
	.set L$set$55,LFE24-LFB24
	.quad L$set$55
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$56,LEFDE47-LASFDE47
	.long L$set$56
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB25-.
	.set L$set$57,LFE25-LFB25
	.quad L$set$57
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$58,LEFDE49-LASFDE49
	.long L$set$58
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB26-.
	.set L$set$59,LFE26-LFB26
	.quad L$set$59
	.uleb128 0
	.byte	0x4
	.set L$set$60,LCFI9-LFB26
	.long L$set$60
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$61,LCFI10-LCFI9
	.long L$set$61
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$62,LCFI11-LCFI10
	.long L$set$62
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI12-LCFI11
	.long L$set$63
	.byte	0xb
	.byte	0x4
	.set L$set$64,LCFI13-LCFI12
	.long L$set$64
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI14-LCFI13
	.long L$set$65
	.byte	0xb
	.byte	0x4
	.set L$set$66,LCFI15-LCFI14
	.long L$set$66
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$67,LCFI16-LCFI15
	.long L$set$67
	.byte	0xb
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$68,LEFDE51-LASFDE51
	.long L$set$68
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB28-.
	.set L$set$69,LFE28-LFB28
	.quad L$set$69
	.uleb128 0
	.byte	0x4
	.set L$set$70,LCFI17-LFB28
	.long L$set$70
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$71,LCFI18-LCFI17
	.long L$set$71
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$72,LCFI19-LCFI18
	.long L$set$72
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$73,LCFI20-LCFI19
	.long L$set$73
	.byte	0xb
	.byte	0x4
	.set L$set$74,LCFI21-LCFI20
	.long L$set$74
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$75,LCFI22-LCFI21
	.long L$set$75
	.byte	0xb
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$76,LEFDE53-LASFDE53
	.long L$set$76
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB29-.
	.set L$set$77,LFE29-LFB29
	.quad L$set$77
	.uleb128 0
	.byte	0x4
	.set L$set$78,LCFI23-LFB29
	.long L$set$78
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$79,LCFI24-LCFI23
	.long L$set$79
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$80,LCFI25-LCFI24
	.long L$set$80
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$81,LCFI26-LCFI25
	.long L$set$81
	.byte	0xb
	.byte	0x4
	.set L$set$82,LCFI27-LCFI26
	.long L$set$82
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$83,LCFI28-LCFI27
	.long L$set$83
	.byte	0xb
	.align	3
LEFDE53:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
