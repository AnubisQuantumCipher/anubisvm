	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC2:
	.ascii "anubis_genesis_validators.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_anubis_genesis_validators__serialize_validator_set__write_u64.6:
LFB48:
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
	mov	w1, 721
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L16:
	adrp	x0, lC2@PAGE
	mov	w1, 719
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE48:
	.align	2
	.p2align 5,,15
_anubis_genesis_validators__deserialize_validator_set__read_u64.7:
LFB50:
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
	mov	w1, 773
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L27:
	adrp	x0, lC2@PAGE
	mov	w1, 774
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE50:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__validator_statusH
_anubis_genesis_validators__validator_statusH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L31
	sub	w1, w1, w2
	cmp	w1, 1
	ble	L31
	ldrb	w2, [x0, 2]
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
	.p2align 2,,3
L31:
	mov	x3, 0
	mov	x1, 0
L29:
	adrp	x0, _validator_statusG.28@PAGE
	mov	w2, 7
	add	x0, x0, _validator_statusG.28@PAGEOFF;
	ldrb	w1, [x0, x1]
	ldrb	w0, [x0, x3]
	add	w1, w1, w0
	udiv	w2, w1, w2
	lsl	w0, w2, 3
	sub	w0, w0, w2
	sub	w0, w1, w0
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__application_statusH
_anubis_genesis_validators__application_statusH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L34
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
L33:
	adrp	x3, _application_statusG.24@PAGE
	mov	w1, 52429
	add	x3, x3, _application_statusG.24@PAGEOFF;
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
L34:
	mov	x2, 0
	mov	x0, 0
	b	L33
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__geographic_regionH
_anubis_genesis_validators__geographic_regionH:
LFB4:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _geographic_regionP.23@PAGE
	add	w5, w10, 1
	add	x9, x9, _geographic_regionP.23@PAGEOFF;
	adrp	x12, _geographic_regionT1.22@PAGE
	adrp	x11, _geographic_regionT2.21@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _geographic_regionT1.22@PAGEOFF;
	add	x11, x11, _geographic_regionT2.21@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 21
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L37
L41:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	add	w6, w2, w2, lsl 2
	add	w6, w2, w6, lsl 2
	add	w2, w5, w5, lsl 2
	sub	w6, w3, w6
	add	w2, w5, w2, lsl 2
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L37
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L41
L37:
	adrp	x3, _geographic_regionG.20@PAGE
	mov	w1, 52429
	add	x3, x3, _geographic_regionG.20@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 35
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1, lsl 1
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__hardware_specsIP
_anubis_genesis_validators__hardware_specsIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__validator_recordIP
_anubis_genesis_validators__validator_recordIP:
LFB127:
	ret
LFE127:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__Tvalidator_arrayBIP
_anubis_genesis_validators__Tvalidator_arrayBIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__validator_applicationIP
_anubis_genesis_validators__validator_applicationIP:
LFB129:
	ret
LFE129:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__application_arrayIP
_anubis_genesis_validators__application_arrayIP:
LFB9:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__validator_set_stateIP
_anubis_genesis_validators__validator_set_stateIP:
LFB131:
	ret
LFE131:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__apply_resultH
_anubis_genesis_validators__apply_resultH:
LFB11:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _apply_resultP.19@PAGE
	add	w5, w10, 1
	add	x9, x9, _apply_resultP.19@PAGEOFF;
	adrp	x12, _apply_resultT1.18@PAGE
	adrp	x11, _apply_resultT2.17@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _apply_resultT1.18@PAGEOFF;
	add	x11, x11, _apply_resultT2.17@PAGEOFF;
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
	blt	L50
L54:
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
	beq	L50
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L54
L50:
	adrp	x3, _apply_resultG.16@PAGE
	mov	w1, 43691
	add	x3, x3, _apply_resultG.16@PAGEOFF;
	movk	w1, 0xaaaa, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 1
	sub	w0, w0, w1, lsl 1
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__register_resultH
_anubis_genesis_validators__register_resultH:
LFB12:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L58
	sub	w1, w1, w2
	cmp	w1, 1
	ble	L58
	ldrb	w3, [x0, 2]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w0, w3, w3, lsl 1
	add	w3, w3, w3, lsl 3
	umull	x2, w0, w1
	umull	x1, w3, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	add	w5, w2, w2, lsl 2
	add	w4, w1, w1, lsl 2
	add	w2, w2, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w0, w0, w2
	sub	w3, w3, w1
	sxtw	x0, w0
	sxtw	x3, w3
	b	L56
	.p2align 2,,3
L58:
	mov	x3, 0
	mov	x0, 0
L56:
	adrp	x2, _register_resultG.12@PAGE
	mov	w1, 52429
	add	x2, x2, _register_resultG.12@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x2, x0]
	ldrb	w2, [x2, x3]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__slash_resultH
_anubis_genesis_validators__slash_resultH:
LFB13:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L61
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
	adrp	x2, _slash_resultG.8@PAGE
	sub	w0, w0, w1
	add	x2, x2, _slash_resultG.8@PAGEOFF;
	sxtw	x3, w3
	sxtw	x1, w0
	ldrb	w1, [x2, x1]
	ldrb	w0, [x2, x3]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L61:
	adrp	x2, _slash_resultG.8@PAGE
	mov	x3, 0
	add	x2, x2, _slash_resultG.8@PAGEOFF;
	mov	x1, 0
	ldrb	w0, [x2, x3]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__init_validator_set
_anubis_genesis_validators__init_validator_set:
LFB14:
	movi	v31.4s, 0
	mov	x3, 0
	.p2align 5,,15
L63:
	lsl	x2, x3, 5
	sub	x2, x2, x3
	lsl	x4, x2, 3
	add	x2, x0, x2, lsl 3
	str	q31, [x0, x4]
	stp	q31, q31, [x2, 16]
	stp	q31, q31, [x2, 48]
	stp	q31, q31, [x2, 80]
	stp	q31, q31, [x2, 112]
	stp	q31, q31, [x2, 144]
	stp	q31, q31, [x2, 176]
	stp	q31, q31, [x2, 208]
	str	xzr, [x2, 240]
	str	w3, [x0, x4]
	add	x3, x3, 1
	cmp	x3, 100
	bne	L63
	adrp	x3, lC3@PAGE
	add	x2, x0, 24576
	str	xzr, [x0, 24800]
	ldr	q31, [x3, #lC3@PAGEOFF]
	add	x3, x0, 16384
	stp	xzr, xzr, [x2, 232]
	str	q31, [x2, 248]
	str	x1, [x0, 24840]
	str	x1, [x0, 24848]
	str	xzr, [x0, 24856]
	strb	wzr, [x2, 288]
	str	wzr, [x3, 8484]
	ret
LFE14:
	.const
	.align	3
lC4:
	.ascii "failed precondition from anubis_genesis_validators.ads:297"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__review_application
_anubis_genesis_validators__review_application:
LFB17:
	stp	x29, x30, [sp, -16]!
LCFI11:
	mov	x12, x0
	mov	x29, sp
LCFI12:
	ldp	w6, w0, [x2]
	ldp	w7, w13, [x5]
	sxtw	x2, w6
	sxtw	x8, w0
	add	x9, x2, 31
	sxtw	x5, w7
	cmp	x9, x8
	add	x10, x5, 31
	sxtw	x9, w13
	ccmp	x10, x9, 0, eq
	bne	L89
	cmp	w6, w0
	bgt	L67
	sub	x11, x12, x2
	sub	x10, x1, x2
	add	x11, x11, 256
	sub	x1, x2, #1
	.p2align 5,,15
L73:
	add	x1, x1, 1
	subs	w2, w1, w6
	bvs	L69
	cmp	w2, 31
	bgt	L71
	bhi	L90
	ldrb	w2, [x10, x1]
	strb	w2, [x11, x1]
L71:
	cmp	x8, x1
	bne	L73
L67:
	cmp	w7, w13
	bgt	L74
	sub	x6, x12, x5
	sub	x4, x4, x5
	add	x6, x6, 288
	sub	x1, x5, #1
	.p2align 5,,15
L80:
	add	x1, x1, 1
	subs	w2, w1, w7
	bvs	L76
	cmp	w2, 31
	bgt	L78
	bhi	L91
	ldrb	w2, [x4, x1]
	strb	w2, [x6, x1]
L78:
	cmp	x9, x1
	bne	L80
L74:
	cmp	w3, 1
	bhi	L92
	cmp	w3, 0
	cset	w0, eq
	add	w0, w0, 2
	strb	w0, [x12, 32]
	ldp	x29, x30, [sp], 16
LCFI13:
	ret
L69:
LCFI14:
	adrp	x0, lC2@PAGE
	mov	w1, 99
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L76:
	adrp	x0, lC2@PAGE
	mov	w1, 106
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L90:
	adrp	x0, lC2@PAGE
	mov	w1, 100
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L91:
	adrp	x0, lC2@PAGE
	mov	w1, 107
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L89:
	adrp	x0, lC4@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC4@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L92:
	adrp	x0, lC2@PAGE
	mov	w1, 111
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE17:
	.const
	.align	2
lC1:
	.word	1
	.word	58
	.text
	.const
	.align	3
lC5:
	.ascii "anubis_genesis_validators.ads"
	.space 1
	.align	3
lC6:
	.ascii "failed postcondition from anubis_genesis_validators.ads:305"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__verify_hardware
_anubis_genesis_validators__verify_hardware:
LFB18:
	stp	x29, x30, [sp, -16]!
LCFI15:
	mov	x29, sp
LCFI16:
	ldr	w1, [x0]
	tbnz	w1, #31, L108
	ldr	w2, [x0, 4]
	cmp	w1, 7
	ble	L95
	tbnz	w2, #31, L109
	ldr	w3, [x0, 8]
	cmp	w2, 31
	ble	L97
	tbnz	w3, #31, L110
	cmp	w3, 1
	ble	L111
	ldr	w4, [x0, 12]
	tbnz	w4, #31, L102
	cmp	w4, 99
	cset	w0, gt
	b	L103
	.p2align 2,,3
L95:
	tbnz	w2, #31, L112
	ldr	w3, [x0, 8]
L97:
	tbnz	w3, #31, L113
	ldr	w4, [x0, 12]
	mov	w0, 0
	tbnz	w4, #31, L100
L103:
	cmp	w1, 7
	ccmp	w2, 31, 4, gt
	cset	w1, gt
	cmp	w4, 99
	ccmp	w3, 1, 4, gt
	cset	w2, gt
	and	w1, w1, w2
	cmp	w1, w0
	bne	L114
	ldp	x29, x30, [sp], 16
LCFI17:
	ret
	.p2align 2,,3
L111:
LCFI18:
	ldr	w0, [x0, 12]
	tbnz	w0, #31, L100
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI19:
	ret
L108:
LCFI20:
	adrp	x0, lC2@PAGE
	mov	w1, 124
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L113:
	adrp	x0, lC5@PAGE
	mov	w1, 308
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L112:
	adrp	x0, lC5@PAGE
	mov	w1, 307
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L114:
	adrp	x0, lC6@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L100:
	adrp	x0, lC5@PAGE
	mov	w1, 309
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L109:
	adrp	x0, lC2@PAGE
	mov	w1, 125
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L102:
	adrp	x0, lC2@PAGE
	mov	w1, 127
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L110:
	adrp	x0, lC2@PAGE
	mov	w1, 126
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE18:
	.const
	.align	2
lC0:
	.word	1
	.word	59
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__submit_application
_anubis_genesis_validators__submit_application:
LFB16:
	stp	x29, x30, [sp, -32]!
LCFI21:
	mov	x29, sp
LCFI22:
	str	x19, [sp, 16]
LCFI23:
	mov	x19, x0
	add	x0, x0, 36
	bl	_anubis_genesis_validators__verify_hardware
	tbz	x0, 0, L117
	ldr	x2, [x19, 200]
	mov	x1, 24999
	mov	w0, 5
	cmp	x2, x1
	csel	w0, wzr, w0, hi
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI24:
	ret
	.p2align 2,,3
L117:
LCFI25:
	mov	w0, 1
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI26:
	ret
LFE16:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__check_regional_balance
_anubis_genesis_validators__check_regional_balance:
LFB20:
	stp	x29, x30, [sp, -16]!
LCFI27:
	mov	x3, 0
	mov	x29, sp
LCFI28:
	mov	w5, 0
	mov	w6, 2147483647
	b	L126
	.p2align 2,,3
L122:
	add	x3, x3, 1
	cmp	x3, 100
	beq	L129
L126:
	lsl	x2, x3, 5
	sub	x2, x2, x3
	add	x2, x0, x2, lsl 3
	ldrb	w4, [x2, 132]
	cmp	w4, 6
	bhi	L130
	cmp	w4, 2
	bne	L122
	ldrb	w2, [x2, 134]
	cmp	w2, 9
	bhi	L124
	cmp	w1, 9
	bhi	L124
	cmp	w2, w1
	bne	L122
	cmp	w5, w6
	beq	L131
	add	x3, x3, 1
	add	w5, w5, 1
	cmp	x3, 100
	bne	L126
L129:
	cmp	w5, 19
	cset	w0, le
	ldp	x29, x30, [sp], 16
LCFI29:
	ret
L130:
LCFI30:
	adrp	x0, lC2@PAGE
	mov	w1, 138
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L124:
	adrp	x0, lC2@PAGE
	mov	w1, 139
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L131:
	adrp	x0, lC2@PAGE
	mov	w1, 141
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE20:
	.const
	.align	3
lC7:
	.ascii "failed precondition from anubis_genesis_validators.ads:332"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__register_validator
_anubis_genesis_validators__register_validator:
LFB21:
	stp	x29, x30, [sp, -16]!
LCFI31:
	mov	x29, sp
LCFI32:
	mov	x7, x0
	mov	x9, 24999
	ldp	w10, w11, [x4]
	ldpsw	x5, x4, [x6]
	sxtw	x8, w10
	sxtw	x12, w11
	add	x0, x8, 63
	add	x5, x5, 62
	cmp	x5, x4
	ccmp	x0, x12, 0, lt
	ccmp	x2, x9, 0, eq
	bls	L160
	ldrb	w4, [x1, 32]
	cmp	w4, 4
	bhi	L161
	mov	w0, 1
	cmp	w4, 2
	beq	L162
L154:
	ldp	x29, x30, [sp], 16
LCFI33:
	ret
	.p2align 2,,3
L162:
LCFI34:
	mov	x0, 19264
	movk	x0, 0x4c, lsl 16
	cmp	x2, x0
	bhi	L150
	mov	x5, 0
	b	L139
	.p2align 2,,3
L137:
	add	x5, x5, 1
	cmp	x5, 100
	beq	L163
L139:
	lsl	x4, x5, 5
	sub	x4, x4, x5
	add	x4, x7, x4, lsl 3
	ldrb	w0, [x4, 132]
	cmp	w0, 6
	bhi	L164
	cbnz	w0, L137
	ldr	x6, [x4, 216]
	cbnz	x6, L137
	mov	w4, 248
	mov	w14, w5
	sxtw	x13, w5
	umaddl	x4, w5, w4, x7
	add	x9, x4, 4
	.p2align 5,,15
L140:
	ldrb	w5, [x1, x6]
	strb	w5, [x9, x6]
	add	x6, x6, 1
	cmp	x6, 32
	bne	L140
	cmp	w10, w11
	bgt	L146
	mov	x5, x8
	add	x4, x4, 68
	sub	x3, x3, x8
	b	L145
	.p2align 2,,3
L151:
	mov	x5, x8
L145:
	cmp	w5, 63
	bhi	L165
L144:
	ldrb	w6, [x3, x5]
	add	x8, x5, 1
	strb	w6, [x4, x5]
	cmp	x12, x5
	bne	L151
L146:
	lsl	x3, x13, 5
	ldrb	w5, [x1, 33]
	mov	w4, 257
	sub	x1, x3, x13
	lsl	x3, x1, 3
	add	x1, x7, x1, lsl 3
	str	w14, [x7, x3]
	strh	w4, [x1, 132]
	cmp	w5, 9
	bhi	L166
	ldr	x6, [x7, 24848]
	add	x4, x7, 16384
	ldr	w3, [x4, 8420]
	strb	w5, [x1, 134]
	str	x2, [x1, 136]
	str	x2, [x1, 152]
	str	x6, [x1, 216]
	tbnz	w3, #31, L167
	mov	w1, 2147483647
	cmp	w3, w1
	beq	L168
	ldr	x1, [x7, 24808]
	add	w3, w3, 1
	str	w3, [x4, 8420]
	add	x1, x1, x2
	str	x1, [x7, 24808]
	ldp	x29, x30, [sp], 16
LCFI35:
	ret
	.p2align 2,,3
L150:
LCFI36:
	mov	w0, 3
	ldp	x29, x30, [sp], 16
LCFI37:
	ret
L163:
LCFI38:
	mov	w0, 1
	b	L154
L164:
	adrp	x0, lC2@PAGE
	mov	w1, 183
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L165:
	cmp	w11, 63
	ccmp	w10, 0, 1, le
	bge	L144
	adrp	x0, lC2@PAGE
	mov	w1, 204
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L160:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L161:
	adrp	x0, lC2@PAGE
	mov	w1, 165
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L166:
	adrp	x0, lC2@PAGE
	mov	w1, 210
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L168:
	adrp	x0, lC2@PAGE
	mov	w1, 215
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L167:
	adrp	x0, lC2@PAGE
	mov	w1, 215
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE21:
	.const
	.align	3
lC8:
	.ascii "failed precondition from anubis_genesis_validators.ads:345"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__add_stake
_anubis_genesis_validators__add_stake:
LFB22:
	stp	x29, x30, [sp, -16]!
LCFI39:
	mov	x29, sp
LCFI40:
	cbz	x2, L179
	cmp	w1, 99
	bhi	L172
	sxtw	x6, w1
	sbfiz	x1, x1, 5, 32
	sub	x4, x1, x6
	mov	x5, x0
	add	x4, x0, x4, lsl 3
	ldrb	w0, [x4, 132]
	cmp	w0, 6
	bhi	L172
	cmp	w0, 0
	ccmp	w0, 6, 4, ne
	cset	w0, eq
	bne	L180
	mov	w0, 0
L173:
	ldp	x29, x30, [sp], 16
LCFI41:
	ret
	.p2align 2,,3
L180:
LCFI42:
	ldr	x7, [x4, 152]
	mov	x8, 19264
	movk	x8, 0x4c, lsl 16
	add	x7, x2, x7
	cmp	x7, x8
	bhi	L173
	cmp	w3, 1
	bhi	L181
	cbz	w3, L175
	ldr	x0, [x4, 136]
	add	x0, x2, x0
	str	x0, [x4, 136]
L176:
	ldr	x3, [x5, 24808]
	sub	x1, x1, x6
	mov	w0, 1
	add	x1, x5, x1, lsl 3
	str	x7, [x1, 152]
	add	x1, x3, x2
	str	x1, [x5, 24808]
	ldp	x29, x30, [sp], 16
LCFI43:
	ret
	.p2align 2,,3
L175:
LCFI44:
	ldr	x0, [x4, 144]
	add	x0, x2, x0
	str	x0, [x4, 144]
	b	L176
L172:
	adrp	x0, lC2@PAGE
	mov	w1, 229
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L179:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L181:
	adrp	x0, lC2@PAGE
	mov	w1, 242
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.const
	.align	3
lC9:
	.ascii "failed precondition from anubis_genesis_validators.ads:356"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__begin_unstake
_anubis_genesis_validators__begin_unstake:
LFB23:
	stp	x29, x30, [sp, -16]!
LCFI45:
	mov	x29, sp
LCFI46:
	cbz	x2, L189
	cmp	w1, 99
	bhi	L190
	sbfiz	x4, x1, 5, 32
	sub	x1, x4, w1, sxtw
	add	x1, x0, x1, lsl 3
	mov	w0, 0
	ldr	x4, [x1, 136]
	cmp	x2, x4
	bhi	L185
	sub	x6, x4, x2
	mov	x5, 24999
	cmp	x6, x5
	ccmp	x2, x4, 4, ls
	bne	L185
	add	x3, x3, 200704
	mov	w2, 4
	add	x3, x3, 896
	mov	w0, 1
	strb	w2, [x1, 132]
	str	x3, [x1, 240]
L185:
	ldp	x29, x30, [sp], 16
LCFI47:
	ret
L190:
LCFI48:
	adrp	x0, lC2@PAGE
	mov	w1, 265
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L189:
	adrp	x0, lC9@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE23:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__complete_unstake
_anubis_genesis_validators__complete_unstake:
LFB24:
	stp	x29, x30, [sp, -16]!
LCFI49:
	mov	x29, sp
LCFI50:
	cmp	w1, 99
	bhi	L193
	sbfiz	x3, x1, 5, 32
	sub	x3, x3, w1, sxtw
	add	x3, x0, x3, lsl 3
	mov	x4, x0
	ldrb	w5, [x3, 132]
	cmp	w5, 6
	bhi	L193
	mov	w1, 0
	mov	x0, 0
	cmp	w5, 4
	beq	L200
L194:
	and	x1, x1, 1
	ldp	x29, x30, [sp], 16
LCFI51:
	ret
	.p2align 2,,3
L200:
LCFI52:
	ldr	x5, [x3, 240]
	cmp	x5, x2
	bhi	L194
	ldr	x1, [x4, 24808]
	add	x5, x4, 16384
	mov	w7, 6
	ldp	x0, x6, [x3, 136]
	ldr	w2, [x5, 8416]
	sub	x1, x1, x0
	str	x1, [x4, 24808]
	strb	w7, [x3, 132]
	str	xzr, [x3, 136]
	str	x6, [x3, 152]
	cmp	w2, 0
	blt	L201
	beq	L196
	sub	w2, w2, #1
	str	w2, [x5, 8416]
L196:
	mov	w1, 1
	b	L194
L193:
	adrp	x0, lC2@PAGE
	mov	w1, 295
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L201:
	adrp	x0, lC2@PAGE
	mov	w1, 312
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__record_block_produced
_anubis_genesis_validators__record_block_produced:
LFB25:
	cmp	w1, 99
	bhi	L204
	sbfiz	x3, x1, 5, 32
	sub	x1, x3, w1, sxtw
	add	x0, x0, x1, lsl 3
	ldrb	w1, [x0, 132]
	cmp	w1, 6
	bhi	L204
	cmp	w1, 2
	beq	L208
	ret
	.p2align 2,,3
L208:
	ldr	x1, [x0, 160]
	str	x2, [x0, 232]
	add	x1, x1, 1
	str	x1, [x0, 160]
	ret
L204:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI53:
	mov	w1, 329
	mov	x29, sp
LCFI54:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__record_block_missed
_anubis_genesis_validators__record_block_missed:
LFB26:
	cmp	w1, 99
	bhi	L211
	sbfiz	x2, x1, 5, 32
	sub	x1, x2, w1, sxtw
	add	x0, x0, x1, lsl 3
	ldrb	w1, [x0, 132]
	cmp	w1, 6
	bhi	L211
	cmp	w1, 2
	beq	L215
	ret
	.p2align 2,,3
L215:
	ldr	x1, [x0, 168]
	add	x1, x1, 1
	str	x1, [x0, 168]
	ret
L211:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI55:
	mov	w1, 343
	mov	x29, sp
LCFI56:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__calculate_block_reward
_anubis_genesis_validators__calculate_block_reward:
LFB27:
	ldr	x0, [x0, 24832]
	ret
LFE27:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__distribute_block_reward
_anubis_genesis_validators__distribute_block_reward:
LFB28:
	ldr	x4, [x0, 24824]
	cmp	x4, x2
	bcc	L217
	cmp	w1, 99
	bhi	L223
	sbfiz	x3, x1, 5, 32
	sub	x1, x3, w1, sxtw
	add	x1, x0, x1, lsl 3
	ldr	x3, [x0, 24816]
	sub	x4, x4, x2
	ldr	x5, [x1, 184]
	add	x3, x3, x2
	add	x2, x2, x5
	str	x2, [x1, 184]
	str	x3, [x0, 24816]
	str	x4, [x0, 24824]
L217:
	ret
L223:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI57:
	mov	w1, 366
	mov	x29, sp
LCFI58:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE28:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__claim_rewards
_anubis_genesis_validators__claim_rewards:
LFB29:
	cmp	w1, 99
	bhi	L232
	sbfiz	x2, x1, 5, 32
	sub	x2, x2, w1, sxtw
	add	x2, x0, x2, lsl 3
	mov	w1, 0
	ldr	x0, [x2, 184]
	cbnz	x0, L233
	and	x1, x1, 1
	ret
	.p2align 2,,3
L233:
	ldr	x3, [x2, 176]
	mov	w1, 1
	and	x1, x1, 1
	add	x3, x0, x3
	stp	x3, xzr, [x2, 176]
	ret
L232:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI59:
	mov	w1, 379
	mov	x29, sp
LCFI60:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE29:
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_genesis_validators.ads:423"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__slash_double_sign
_anubis_genesis_validators__slash_double_sign:
LFB30:
	stp	x29, x30, [sp, -16]!
LCFI61:
	mov	x4, x0
	mov	x29, sp
LCFI62:
	ldpsw	x2, x0, [x3]
	add	x2, x2, 31
	cmp	x2, x0
	bne	L248
	cmp	w1, 99
	bhi	L237
	sxtw	x3, w1
	sbfiz	x1, x1, 5, 32
	sub	x0, x1, x3
	add	x0, x4, x0, lsl 3
	ldrb	w2, [x0, 132]
	cmp	w2, 6
	bhi	L237
	cmp	w2, 3
	beq	L244
	cmp	w2, 6
	beq	L246
	cbz	w2, L246
	ldr	x5, [x0, 152]
	mov	x6, 22859
	movk	x6, 0x3886, lsl 16
	movk	x6, 0xc5d6, lsl 32
	movk	x6, 0x346d, lsl 48
	add	x2, x5, x5, lsl 2
	add	x2, x2, x2, lsl 2
	add	x2, x2, x2, lsl 2
	lsl	x2, x2, 2
	umulh	x2, x2, x6
	lsr	x2, x2, 11
	cmp	x5, x2
	bls	L249
	ldr	x6, [x0, 136]
	sub	x5, x5, x2
	str	x5, [x0, 152]
	cmp	x2, x6
	bcs	L241
	sub	x0, x6, x2
L240:
	sub	x1, x1, x3
	add	x1, x4, x1, lsl 3
	ldr	x3, [x1, 200]
	str	x0, [x1, 136]
	ldr	w0, [x1, 196]
	add	x3, x2, x3
	str	x3, [x1, 200]
	tbnz	w0, #31, L250
	mov	w3, 2147483647
	cmp	w0, w3
	beq	L251
	ldr	x5, [x4, 24808]
	add	w3, w0, 1
	mov	w6, 5
	mov	w0, 0
	strb	w6, [x1, 132]
	str	w3, [x1, 196]
	sub	x1, x5, x2
	str	x1, [x4, 24808]
	ldp	x29, x30, [sp], 16
LCFI63:
	ret
	.p2align 2,,3
L246:
LCFI64:
	mov	w0, 3
	ldp	x29, x30, [sp], 16
LCFI65:
	ret
	.p2align 2,,3
L244:
LCFI66:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI67:
	ret
	.p2align 2,,3
L249:
LCFI68:
	str	xzr, [x0, 152]
L241:
	mov	x0, 0
	b	L240
L237:
	adrp	x0, lC2@PAGE
	mov	w1, 406
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L248:
	adrp	x0, lC10@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L251:
	adrp	x0, lC2@PAGE
	mov	w1, 440
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L250:
	adrp	x0, lC2@PAGE
	mov	w1, 440
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE30:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__slash_downtime
_anubis_genesis_validators__slash_downtime:
LFB31:
	stp	x29, x30, [sp, -16]!
LCFI69:
	mov	x29, sp
LCFI70:
	cmp	w1, 99
	bhi	L254
	sbfiz	x2, x1, 5, 32
	sub	x1, x2, w1, sxtw
	add	x1, x0, x1, lsl 3
	mov	x3, x0
	ldrb	w0, [x1, 132]
	cmp	w0, 6
	bhi	L254
	cmp	w0, 3
	beq	L259
	cmp	w0, 6
	beq	L261
	cbz	w0, L261
	ldr	x0, [x1, 152]
	mov	x6, 22859
	movk	x6, 0x3886, lsl 16
	movk	x6, 0xc5d6, lsl 32
	ldr	x5, [x1, 200]
	movk	x6, 0x346d, lsl 48
	ldr	w4, [x1, 196]
	add	x2, x0, x0, lsl 2
	lsl	x2, x2, 1
	umulh	x2, x2, x6
	lsr	x2, x2, 11
	add	x5, x2, x5
	subs	x0, x0, x2
	csel	x0, x0, xzr, hi
	str	x0, [x1, 152]
	str	x5, [x1, 200]
	tbnz	w4, #31, L264
	mov	w0, 2147483647
	cmp	w4, w0
	beq	L265
	ldr	x5, [x3, 24808]
	add	w4, w4, 1
	mov	w0, 0
	str	w4, [x1, 196]
	sub	x1, x5, x2
	str	x1, [x3, 24808]
	ldp	x29, x30, [sp], 16
LCFI71:
	ret
	.p2align 2,,3
L261:
LCFI72:
	mov	w0, 3
	ldp	x29, x30, [sp], 16
LCFI73:
	ret
	.p2align 2,,3
L259:
LCFI74:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI75:
	ret
L254:
LCFI76:
	adrp	x0, lC2@PAGE
	mov	w1, 456
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L264:
	adrp	x0, lC2@PAGE
	mov	w1, 483
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L265:
	adrp	x0, lC2@PAGE
	mov	w1, 483
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE31:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__jail_validator
_anubis_genesis_validators__jail_validator:
LFB32:
	stp	x29, x30, [sp, -16]!
LCFI77:
	mov	x29, sp
LCFI78:
	cmp	w1, 99
	bhi	L268
	sbfiz	x3, x1, 5, 32
	sub	x1, x3, w1, sxtw
	add	x1, x0, x1, lsl 3
	ldrb	w3, [x1, 132]
	cmp	w3, 6
	bhi	L268
	cmp	w3, 2
	beq	L274
L266:
	ldp	x29, x30, [sp], 16
LCFI79:
	ret
	.p2align 2,,3
L274:
LCFI80:
	ldr	x3, [x0, 24848]
	add	x0, x0, 16384
	mov	w5, 3
	ldr	w4, [x0, 8416]
	strb	w5, [x1, 132]
	add	x2, x3, x2
	str	x2, [x1, 208]
	cmp	w4, 0
	blt	L275
	beq	L266
	sub	w4, w4, #1
	str	w4, [x0, 8416]
	ldp	x29, x30, [sp], 16
LCFI81:
	ret
L268:
LCFI82:
	adrp	x0, lC2@PAGE
	mov	w1, 495
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L275:
	adrp	x0, lC2@PAGE
	mov	w1, 500
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE32:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__unjail_validator
_anubis_genesis_validators__unjail_validator:
LFB33:
	stp	x29, x30, [sp, -16]!
LCFI83:
	mov	x29, sp
LCFI84:
	cmp	w1, 99
	bhi	L278
	sbfiz	x3, x1, 5, 32
	sub	x1, x3, w1, sxtw
	add	x1, x0, x1, lsl 3
	mov	x4, x0
	ldrb	w3, [x1, 132]
	cmp	w3, 6
	bhi	L278
	mov	w0, 0
	cmp	w3, 3
	beq	L285
L279:
	ldp	x29, x30, [sp], 16
LCFI85:
	ret
	.p2align 2,,3
L285:
LCFI86:
	ldr	x3, [x1, 208]
	cmp	x3, x2
	bhi	L279
	mov	w2, 2
	add	x4, x4, 16384
	ldr	w0, [x4, 8416]
	strb	w2, [x1, 132]
	tbnz	w0, #31, L286
	mov	w1, 2147483647
	cmp	w0, w1
	beq	L287
	add	w1, w0, 1
	mov	w0, 1
	str	w1, [x4, 8416]
	ldp	x29, x30, [sp], 16
LCFI87:
	ret
L278:
LCFI88:
	adrp	x0, lC2@PAGE
	mov	w1, 513
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L287:
	adrp	x0, lC2@PAGE
	mov	w1, 524
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L286:
	adrp	x0, lC2@PAGE
	mov	w1, 524
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE33:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__get_validator
_anubis_genesis_validators__get_validator:
LFB34:
	cmp	w1, 99
	bhi	L293
	sbfiz	x2, x1, 5, 32
	sub	x1, x2, w1, sxtw
	add	x2, x0, x1, lsl 3
	lsl	x1, x1, 3
	ldr	x3, [x2, 240]
	ldr	q30, [x0, x1]
	ldr	q28, [x2, 48]
	str	x3, [x8, 240]
	ldp	q29, q31, [x2, 16]
	stp	q30, q29, [x8]
	ldp	q30, q29, [x2, 64]
	stp	q31, q28, [x8, 32]
	ldp	q31, q28, [x2, 96]
	stp	q30, q29, [x8, 64]
	ldp	q30, q29, [x2, 128]
	stp	q31, q28, [x8, 96]
	ldp	q31, q28, [x2, 160]
	stp	q30, q29, [x8, 128]
	ldp	q30, q29, [x2, 192]
	stp	q31, q28, [x8, 160]
	ldr	q31, [x2, 224]
	stp	q30, q29, [x8, 192]
	str	q31, [x8, 224]
	ret
L293:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI89:
	mov	w1, 538
	mov	x29, sp
LCFI90:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE34:
	.const
	.align	3
lC11:
	.ascii "failed precondition from anubis_genesis_validators.ads:470"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__find_validator
_anubis_genesis_validators__find_validator:
LFB35:
	stp	x29, x30, [sp, -16]!
LCFI91:
	mov	x29, sp
LCFI92:
	ldp	w2, w9, [x2]
	sxtw	x10, w2
	sxtw	x5, w9
	add	x3, x10, 31
	cmp	x3, x5
	bne	L312
	sub	x6, x0, x10
	mov	x7, 0
	sub	x8, x1, x10
	add	x6, x6, 4
	b	L306
	.p2align 2,,3
L296:
	lsl	x1, x7, 5
	sub	x1, x1, x7
	add	x1, x0, x1, lsl 3
	ldrb	w1, [x1, 132]
	cmp	w1, 6
	bhi	L313
	cbnz	w1, L305
L302:
	add	x7, x7, 1
	add	x6, x6, 248
	cmp	x7, 100
	beq	L314
L306:
	cmp	w2, w9
	bgt	L296
	sub	x1, x10, #1
	b	L303
	.p2align 2,,3
L316:
	cmp	w3, 31
	bgt	L300
	bhi	L315
	ldrb	w4, [x8, x1]
	ldrb	w3, [x6, x1]
	cmp	w4, w3
	bne	L302
L300:
	cmp	x5, x1
	beq	L296
L303:
	add	x1, x1, 1
	subs	w3, w1, w2
	bvc	L316
	adrp	x0, lC2@PAGE
	mov	w1, 556
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L305:
	mov	w1, 1
	uxtw	x0, w7
	orr	x0, x0, x1, lsl 32
	ldp	x29, x30, [sp], 16
LCFI93:
	ret
L314:
LCFI94:
	mov	w7, 0
	mov	w1, 0
	uxtw	x0, w7
	orr	x0, x0, x1, lsl 32
	ldp	x29, x30, [sp], 16
LCFI95:
	ret
L315:
LCFI96:
	adrp	x0, lC2@PAGE
	mov	w1, 558
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L313:
	adrp	x0, lC2@PAGE
	mov	w1, 566
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L312:
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE35:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__get_active_count
_anubis_genesis_validators__get_active_count:
LFB36:
	add	x0, x0, 16384
	ldr	w0, [x0, 8416]
	tbnz	w0, #31, L322
	ret
L322:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI97:
	mov	w1, 579
	mov	x29, sp
LCFI98:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE36:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__get_total_stake
_anubis_genesis_validators__get_total_stake:
LFB38:
	ldr	x0, [x0, 24808]
	ret
LFE38:
	.const
	.align	3
lC12:
	.ascii "failed postcondition from anubis_genesis_validators.ads:491"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__calculate_uptime
_anubis_genesis_validators__calculate_uptime:
LFB40:
	ldp	x0, x1, [x0, 160]
	adds	x1, x0, x1
	beq	L327
	mov	x2, 10000
	stp	x29, x30, [sp, -16]!
LCFI99:
	mov	x3, 2147483647
	mov	x29, sp
LCFI100:
	mul	x0, x0, x2
	udiv	x1, x0, x1
	cmp	x1, x3
	bhi	L332
	mov	w0, w1
	cmp	w1, w2
	bgt	L333
	ldp	x29, x30, [sp], 16
LCFI101:
	ret
	.p2align 2,,3
L327:
	mov	w0, 10000
	ret
L333:
LCFI102:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L332:
	adrp	x0, lC2@PAGE
	mov	w1, 600
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE40:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__meets_uptime_requirement
_anubis_genesis_validators__meets_uptime_requirement:
LFB42:
	ldp	x0, x1, [x0, 160]
	adds	x1, x0, x1
	beq	L338
	mov	x2, 10000
	stp	x29, x30, [sp, -16]!
LCFI103:
	mov	x3, 2147483647
	mov	x29, sp
LCFI104:
	mul	x0, x0, x2
	udiv	x0, x0, x1
	cmp	x0, x3
	bhi	L343
	cmp	w0, w2
	bgt	L337
	mov	w1, 9499
	cmp	w0, w1
	cset	w0, gt
	ldp	x29, x30, [sp], 16
LCFI105:
	ret
	.p2align 2,,3
L338:
	mov	w0, 1
	ret
L337:
LCFI106:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L343:
	adrp	x0, lC2@PAGE
	mov	w1, 600
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE42:
	.const
	.align	3
lC13:
	.ascii "failed precondition from anubis_genesis_validators.ads:509"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__enable_open_entry
_anubis_genesis_validators__enable_open_entry:
LFB43:
	mov	x2, 6559
	movk	x2, 0x28, lsl 16
	cmp	x1, x2
	bls	L349
	add	x0, x0, 24576
	mov	w1, 1
	strb	w1, [x0, 288]
	ret
L349:
	adrp	x0, lC13@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI107:
	add	x0, x0, lC13@PAGEOFF;
	mov	x29, sp
LCFI108:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE43:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__is_open_entry
_anubis_genesis_validators__is_open_entry:
LFB45:
	add	x0, x0, 24576
	ldrb	w0, [x0, 288]
	cmp	w0, 1
	bhi	L355
	cmp	w0, 0
	mov	x0, 6559
	movk	x0, 0x28, lsl 16
	ccmp	x1, x0, 2, eq
	cset	w0, hi
	ret
L355:
	adrp	x0, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI109:
	mov	w1, 628
	mov	x29, sp
LCFI110:
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE45:
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_genesis_validators.ads:529"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__register_open_entry
_anubis_genesis_validators__register_open_entry:
LFB46:
	stp	x29, x30, [sp, -16]!
LCFI111:
	mov	x29, sp
LCFI112:
	mov	x6, x0
	mov	x10, 24999
	ldpsw	x8, x9, [x7]
	add	x0, x8, 62
	ldp	w8, w7, [x2]
	cmp	x0, x9
	ldp	w9, w14, [x5]
	ccmp	x3, x10, 0, lt
	cset	w0, ls
	sxtw	x13, w8
	sxtw	x10, w7
	add	x11, x13, 31
	sxtw	x12, w9
	cmp	x11, x10
	add	x2, x12, 63
	sxtw	x11, w14
	ccmp	x2, x11, 0, eq
	cset	w2, ne
	orr	w0, w0, w2
	cbnz	w0, L390
	add	x0, x6, 24576
	ldrb	w0, [x0, 288]
	cmp	w0, 1
	bhi	L391
	cbz	w0, L381
	mov	x0, 19264
	movk	x0, 0x4c, lsl 16
	cmp	x3, x0
	bhi	L382
	mov	x5, 0
	b	L363
	.p2align 2,,3
L361:
	add	x5, x5, 1
	cmp	x5, 100
	beq	L381
L363:
	lsl	x2, x5, 5
	sub	x2, x2, x5
	add	x2, x6, x2, lsl 3
	ldrb	w0, [x2, 132]
	cmp	w0, 6
	bhi	L392
	cbnz	w0, L361
	ldr	x2, [x2, 216]
	cbnz	x2, L361
	mov	w15, w5
	cmp	w8, w7
	bgt	L380
	mov	w2, 248
	sub	x1, x1, x13
	sub	x7, x13, #1
	umull	x2, w5, w2
	sub	x2, x2, x13
	add	x2, x2, 4
	add	x2, x6, x2
	.p2align 5,,15
L369:
	add	x7, x7, 1
	subs	w5, w7, w8
	bvs	L365
	cmp	w5, 31
	bgt	L367
	bhi	L393
	ldrb	w5, [x1, x7]
	strb	w5, [x2, x7]
L367:
	cmp	x10, x7
	bne	L369
L380:
	cmp	w9, w14
	bgt	L370
	mov	w5, 248
	sub	x4, x4, x12
	sub	x1, x12, #1
	umull	x5, w15, w5
	sub	x5, x5, x12
	add	x5, x5, 68
	add	x5, x6, x5
	.p2align 5,,15
L376:
	add	x1, x1, 1
	subs	w2, w1, w9
	bvs	L372
	cmp	w2, 63
	bgt	L374
	bhi	L394
	ldrb	w2, [x4, x1]
	strb	w2, [x5, x1]
L374:
	cmp	x11, x1
	bne	L376
L370:
	sbfiz	x1, x15, 5, 32
	sub	x1, x1, w15, sxtw
	lsl	x5, x1, 3
	add	x1, x6, x1, lsl 3
	mov	w2, 1
	add	x4, x6, 16384
	str	w15, [x6, x5]
	strh	w2, [x1, 132]
	ldr	x5, [x6, 24848]
	ldr	w2, [x4, 8484]
	str	x3, [x1, 136]
	str	x3, [x1, 152]
	str	x5, [x1, 216]
	tbnz	w2, #31, L395
	mov	w1, 2147483647
	cmp	w2, w1
	beq	L396
	ldr	x1, [x6, 24808]
	add	w2, w2, 1
	str	w2, [x4, 8484]
	add	x1, x1, x3
	str	x1, [x6, 24808]
	ldp	x29, x30, [sp], 16
LCFI113:
	ret
	.p2align 2,,3
L381:
LCFI114:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI115:
	ret
	.p2align 2,,3
L382:
LCFI116:
	mov	w0, 3
	ldp	x29, x30, [sp], 16
LCFI117:
	ret
L392:
LCFI118:
	adrp	x0, lC2@PAGE
	mov	w1, 661
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L372:
	adrp	x0, lC2@PAGE
	mov	w1, 684
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L365:
	adrp	x0, lC2@PAGE
	mov	w1, 677
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L396:
	adrp	x0, lC2@PAGE
	mov	w1, 697
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L395:
	adrp	x0, lC2@PAGE
	mov	w1, 697
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L394:
	adrp	x0, lC2@PAGE
	mov	w1, 685
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L393:
	adrp	x0, lC2@PAGE
	mov	w1, 678
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L391:
	adrp	x0, lC2@PAGE
	mov	w1, 644
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L390:
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE46:
	.const
	.align	3
lC15:
	.ascii "failed precondition from anubis_genesis_validators.ads:544"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__serialize_validator_set
_anubis_genesis_validators__serialize_validator_set:
LFB47:
	stp	x29, x30, [sp, -96]!
LCFI119:
	mov	x29, sp
LCFI120:
	add	x3, x29, 48
	stp	x19, x20, [sp, 16]
LCFI121:
	mov	x19, x0
	add	x0, x29, 96
	stp	x21, x22, [sp, 32]
LCFI122:
	stp	x1, x2, [x29, 48]
	str	x3, [x29, 72]
	ldp	w21, w3, [x2]
	str	x0, [x29, 88]
	mov	x0, 65534
	sxtw	x22, w21
	sxtw	x2, w3
	add	x0, x22, x0
	str	x22, [x29, 64]
	cmp	x0, x2
	bge	L423
	tbnz	w21, #31, L424
	ldr	x0, [x29, 48]
	mov	w1, 0
	cmp	w21, w3
	sub	x2, x2, x22
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldr	x0, [x29, 56]
	ldr	w1, [x0, 4]
	cmp	w21, w1
	bgt	L401
	ldr	w4, [x0]
	cmp	w21, w4
	blt	L425
	add	x3, x19, 16384
	ldr	w5, [x3, 8416]
	tbnz	w5, #31, L426
	ldr	x2, [x29, 48]
	add	w0, w21, 1
	strb	w5, [x2]
	cmp	w1, w0
	blt	L420
	cmp	w4, w0
	bgt	L427
	ldr	w1, [x3, 8420]
	tbnz	w1, #31, L428
	sxtw	x0, w0
	add	w21, w21, 2
	sub	x0, x0, x22
	strb	w1, [x2, x0]
L401:
	add	x20, x29, 64
	ldr	x0, [x19, 24808]
	mov	x16, x20
	str	w21, [x29, 80]
	bl	_anubis_genesis_validators__serialize_validator_set__write_u64.6
	ldr	x0, [x19, 24816]
	mov	x16, x20
	bl	_anubis_genesis_validators__serialize_validator_set__write_u64.6
	ldr	x0, [x19, 24824]
	mov	x16, x20
	bl	_anubis_genesis_validators__serialize_validator_set__write_u64.6
	ldr	x0, [x19, 24832]
	mov	x16, x20
	bl	_anubis_genesis_validators__serialize_validator_set__write_u64.6
	ldr	x0, [x19, 24840]
	mov	x16, x20
	bl	_anubis_genesis_validators__serialize_validator_set__write_u64.6
	ldr	x0, [x19, 24848]
	mov	x16, x20
	bl	_anubis_genesis_validators__serialize_validator_set__write_u64.6
	ldr	x0, [x19, 24856]
	mov	x16, x20
	bl	_anubis_genesis_validators__serialize_validator_set__write_u64.6
	ldr	w0, [x29, 80]
	tbnz	w0, #31, L429
	ldr	x1, [x29, 56]
	ldp	w1, w3, [x1]
	cmp	w0, w3
	bgt	L412
	cmp	w1, w0
	bgt	L430
	add	x2, x19, 24576
	ldrb	w4, [x2, 288]
	cmp	w4, 1
	bhi	L431
	ldr	x6, [x29, 48]
	sxtw	x2, w0
	mov	w5, 2147483647
	sub	x2, x2, x22
	strb	w4, [x6, x2]
	cmp	w0, w5
	beq	L432
	add	w2, w0, 1
	cmp	w2, w3
	bgt	L433
	add	x19, x19, 16384
	ldr	w4, [x19, 8484]
	tbnz	w4, #31, L434
	sxtw	x3, w2
	add	w0, w0, 2
	sub	x3, x3, x22
	strb	w4, [x6, x3]
	cmp	w2, w5
	beq	L435
L412:
	subs	w0, w0, w1
	bvs	L416
	tbnz	w0, #31, L436
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI123:
	ret
L435:
LCFI124:
	adrp	x0, lC2@PAGE
	mov	w1, 755
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L416:
	adrp	x0, lC2@PAGE
	mov	w1, 758
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L429:
	adrp	x0, lC2@PAGE
	mov	w1, 748
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L436:
	adrp	x0, lC2@PAGE
	mov	w1, 758
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L424:
	adrp	x0, lC2@PAGE
	mov	w1, 712
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L423:
	adrp	x0, lC15@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L428:
	adrp	x0, lC2@PAGE
	mov	w1, 736
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L427:
	adrp	x0, lC2@PAGE
	mov	w1, 736
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L432:
	adrp	x0, lC2@PAGE
	mov	w1, 750
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L426:
	adrp	x0, lC2@PAGE
	mov	w1, 731
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L425:
	adrp	x0, lC2@PAGE
	mov	w1, 731
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L434:
	adrp	x0, lC2@PAGE
	mov	w1, 754
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L430:
	adrp	x0, lC2@PAGE
	mov	w1, 749
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L431:
	adrp	x0, lC2@PAGE
	mov	w1, 749
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L420:
	mov	w21, w0
	b	L401
L433:
	mov	w0, w2
	b	L412
LFE47:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__deserialize_validator_set
_anubis_genesis_validators__deserialize_validator_set:
LFB49:
	stp	x29, x30, [sp, -80]!
LCFI125:
	mov	x29, sp
LCFI126:
	add	x3, x29, 32
	stp	x0, x1, [x29, 32]
	add	x0, x29, 80
	stp	x19, x20, [sp, 16]
LCFI127:
	ldr	w6, [x1]
	str	x3, [x29, 56]
	str	x0, [x29, 72]
	tbnz	w6, #31, L449
	mov	x0, x2
	mov	x5, x1
	mov	x1, 0
	mov	x19, x2
	bl	_anubis_genesis_validators__init_validator_set
	sxtw	x0, w6
	ldr	w1, [x5, 4]
	add	x2, x0, 78
	cmp	x2, w1, sxtw
	blt	L450
	mov	w0, 0
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI128:
	ret
	.p2align 2,,3
L450:
LCFI129:
	cmp	w6, w1
	bgt	L440
	ldr	x4, [x29, 32]
	add	x3, x19, 16384
	add	w2, w6, 1
	ldrb	w5, [x4]
	str	w5, [x3, 8416]
	cmp	w1, w2
	blt	L447
	sxtw	x2, w2
	add	w6, w6, 2
	sub	x2, x2, x0
	ldrb	w1, [x4, x2]
	str	w1, [x3, 8420]
L440:
	add	x20, x29, 48
	str	x0, [x29, 48]
	mov	x16, x20
	str	w6, [x29, 64]
	bl	_anubis_genesis_validators__deserialize_validator_set__read_u64.7
	mov	x16, x20
	str	x0, [x19, 24808]
	bl	_anubis_genesis_validators__deserialize_validator_set__read_u64.7
	mov	x16, x20
	str	x0, [x19, 24816]
	bl	_anubis_genesis_validators__deserialize_validator_set__read_u64.7
	mov	x16, x20
	str	x0, [x19, 24824]
	bl	_anubis_genesis_validators__deserialize_validator_set__read_u64.7
	mov	x16, x20
	str	x0, [x19, 24832]
	bl	_anubis_genesis_validators__deserialize_validator_set__read_u64.7
	mov	x16, x20
	str	x0, [x19, 24840]
	bl	_anubis_genesis_validators__deserialize_validator_set__read_u64.7
	mov	x16, x20
	str	x0, [x19, 24848]
	bl	_anubis_genesis_validators__deserialize_validator_set__read_u64.7
	ldr	w1, [x29, 64]
	str	x0, [x19, 24856]
	ldr	x3, [x29, 48]
	tbnz	w1, #31, L451
	ldr	x0, [x29, 40]
	ldr	w2, [x0, 4]
	cmp	w1, w2
	bgt	L445
	ldr	w0, [x0]
	cmp	w0, w1
	bgt	L452
	ldr	x6, [x29, 32]
	sxtw	x0, w1
	add	x4, x19, 24576
	sub	x0, x0, x3
	mov	w5, 2147483647
	ldrb	w0, [x6, x0]
	cmp	w0, 0
	cset	w7, ne
	strb	w7, [x4, 288]
	cmp	w1, w5
	beq	L453
	add	w1, w1, 1
	cmp	w1, w2
	bgt	L445
	sxtw	x0, w1
	add	x19, x19, 16384
	sub	x0, x0, x3
	ldrb	w0, [x6, x0]
	str	w0, [x19, 8484]
	cmp	w1, w5
	beq	L454
L445:
	mov	w0, 1
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI130:
	ret
	.p2align 2,,3
L447:
LCFI131:
	mov	w6, w2
	b	L440
L454:
	adrp	x0, lC2@PAGE
	mov	w1, 812
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L449:
	adrp	x0, lC2@PAGE
	mov	w1, 766
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L451:
	adrp	x0, lC2@PAGE
	mov	w1, 805
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L452:
	adrp	x0, lC2@PAGE
	mov	w1, 806
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L453:
	adrp	x0, lC2@PAGE
	mov	w1, 807
	add	x0, x0, lC2@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE49:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__zeroize_validator_set
_anubis_genesis_validators__zeroize_validator_set:
LFB51:
	movi	v30.4s, 0
	movi	v31.4s, 0
	mov	x4, 0
	add	x2, x0, 136
	.p2align 5,,15
L456:
	lsl	x3, x4, 5
	sub	x3, x3, x4
	add	x4, x4, 1
	add	x1, x0, x3, lsl 3
	lsl	x3, x3, 3
	add	x6, x1, 4
	add	x5, x1, 36
	str	wzr, [x0, x3]
	add	x3, x1, 68
	stp	xzr, xzr, [x6]
	stp	xzr, xzr, [x6, 16]
	stp	xzr, xzr, [x5]
	stp	xzr, xzr, [x5, 16]
	str	q30, [x1, 68]
	stp	q30, q30, [x3, 16]
	str	q30, [x1, 116]
	strb	wzr, [x1, 132]
	strb	wzr, [x1, 133]
	strb	wzr, [x1, 134]
	stp	q31, q31, [x2]
	str	q31, [x2, 32]
	str	xzr, [x1, 184]
	str	xzr, [x2, 56]
	stp	q31, q31, [x2, 64]
	str	q31, [x2, 96]
	add	x2, x2, 248
	cmp	x4, 100
	bne	L456
	add	x1, x0, 24576
	mov	x2, 25088
	str	xzr, [x0, 24800]
	add	x3, x0, x2
	add	x2, x0, 16384
	str	q31, [x1, 232]
	str	q31, [x1, 248]
	str	q31, [x3, -248]
	str	xzr, [x0, 24856]
	strb	wzr, [x1, 288]
	str	wzr, [x2, 8484]
	ret
LFE51:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__zeroize_validator
_anubis_genesis_validators__zeroize_validator:
LFB52:
	mov	x1, x0
	movi	v30.4s, 0
	add	x3, x0, 36
	movi	v31.4s, 0
	add	x2, x0, 68
	str	wzr, [x1], 4
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	add	x1, x0, 152
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	str	q30, [x0, 68]
	stp	q30, q30, [x2, 16]
	str	q30, [x0, 116]
	strb	wzr, [x0, 132]
	strb	wzr, [x0, 133]
	strb	wzr, [x0, 134]
	str	q31, [x0, 136]
	stp	xzr, xzr, [x0, 184]
	stp	q31, q31, [x1]
	stp	q31, q31, [x1, 48]
	str	q31, [x0, 232]
	ret
LFE52:
	.align	2
	.p2align 5,,15
	.globl _anubis_genesis_validators__zeroize_application
_anubis_genesis_validators__zeroize_application:
LFB53:
	movi	v31.4s, 0
	add	x2, x0, 36
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	strh	wzr, [x0, 32]
	stp	xzr, xzr, [x2]
	str	q31, [x0, 168]
	str	wzr, [x0, 52]
	stp	xzr, xzr, [x0, 56]
	stp	xzr, xzr, [x0, 72]
	stp	xzr, xzr, [x0, 88]
	stp	xzr, xzr, [x0, 104]
	stp	xzr, xzr, [x0, 120]
	stp	xzr, xzr, [x0, 136]
	str	q31, [x0, 152]
	str	q31, [x0, 182]
	str	xzr, [x0, 200]
	stp	xzr, xzr, [x0, 208]
	stp	xzr, xzr, [x0, 224]
	stp	xzr, xzr, [x0, 240]
	stp	xzr, xzr, [x0, 256]
	stp	xzr, xzr, [x0, 272]
	stp	xzr, xzr, [x0, 288]
	stp	xzr, xzr, [x0, 304]
	ret
LFE53:
	.const
	.align	3
_slash_resultG.8:
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	3
	.byte	0
	.space 5
	.align	3
_register_resultG.12:
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	1
	.space 5
	.align	3
_apply_resultG.16:
	.byte	5
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	1
	.byte	0
	.byte	1
	.space 3
	.align	1
_apply_resultT2.17:
	.byte	2
	.byte	0
	.align	1
_apply_resultT1.18:
	.byte	3
	.byte	8
	.align	3
_apply_resultP.19:
	.word	3
	.word	9
_geographic_regionG.20:
	.byte	1
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	6
	.byte	0
	.byte	8
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	7
	.byte	8
	.byte	1
	.byte	0
	.byte	0
	.byte	7
	.align	1
_geographic_regionT2.21:
	.byte	0
	.byte	18
	.align	1
_geographic_regionT1.22:
	.byte	1
	.byte	3
	.align	3
_geographic_regionP.23:
	.word	1
	.word	9
	.align	3
_application_statusG.24:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	4
	.byte	3
	.byte	3
	.space 4
_validator_statusG.28:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	5
	.byte	5
	.byte	0
	.byte	2
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.globl _anubis_genesis_validators__slash_resultN
	.align	3
_anubis_genesis_validators__slash_resultN:
	.byte	1
	.byte	8
	.byte	22
	.byte	38
	.byte	51
	.space 3
	.globl _anubis_genesis_validators__slash_resultS
	.align	3
_anubis_genesis_validators__slash_resultS:
	.ascii "SLASHEDALREADY_JAILEDINVALID_EVIDENCENOT_VALIDATOR"
	.globl _anubis_genesis_validators__register_resultN
	.align	3
_anubis_genesis_validators__register_resultN:
	.byte	1
	.byte	11
	.byte	23
	.byte	41
	.byte	59
	.byte	76
	.space 2
	.globl _anubis_genesis_validators__register_resultS
	.align	3
_anubis_genesis_validators__register_resultS:
	.ascii "REGISTEREDNOT_APPROVEDINSUFFICIENT_STAKESTAKE_CAP_EXCEEDEDINVALID_SIGNATURE"
	.globl _anubis_genesis_validators__apply_resultN
	.align	3
_anubis_genesis_validators__apply_resultN:
	.byte	1
	.byte	22
	.byte	40
	.byte	55
	.byte	76
	.byte	90
	.byte	103
	.space 1
	.globl _anubis_genesis_validators__apply_resultS
	.align	3
_anubis_genesis_validators__apply_resultS:
	.ascii "APPLICATION_SUBMITTEDINSUFFICIENT_SPECSALREADY_APPLIEDGENESIS_PERIOD_CLOSEDINVALID_REGIONINVALID_STAKE"
	.globl _anubis_genesis_validators__hardware_tierN
	.align	2
_anubis_genesis_validators__hardware_tierN:
	.byte	1
	.byte	8
	.byte	19
	.byte	31
	.globl _anubis_genesis_validators__hardware_tierS
	.align	3
_anubis_genesis_validators__hardware_tierS:
	.ascii "MINIMUMRECOMMENDEDPROFESSIONAL"
	.globl _anubis_genesis_validators__geographic_regionN
	.align	3
_anubis_genesis_validators__geographic_regionN:
	.byte	1
	.byte	14
	.byte	27
	.byte	33
	.byte	39
	.byte	50
	.byte	62
	.byte	72
	.byte	81
	.byte	95
	.byte	102
	.space 5
	.globl _anubis_genesis_validators__geographic_regionS
	.align	3
_anubis_genesis_validators__geographic_regionS:
	.ascii "NORTH_AMERICASOUTH_AMERICAEUROPEAFRICAMIDDLE_EASTCENTRAL_ASIASOUTH_ASIAEAST_ASIASOUTHEAST_ASIAOCEANIA"
	.globl _anubis_genesis_validators__application_statusN
	.align	3
_anubis_genesis_validators__application_statusN:
	.byte	1
	.byte	10
	.byte	22
	.byte	30
	.byte	38
	.byte	48
	.space 2
	.globl _anubis_genesis_validators__application_statusS
	.align	3
_anubis_genesis_validators__application_statusS:
	.ascii "SUBMITTEDUNDER_REVIEWAPPROVEDREJECTEDWAITLISTED"
	.globl _anubis_genesis_validators__validator_statusN
	.align	3
_anubis_genesis_validators__validator_statusN:
	.byte	1
	.byte	20
	.byte	28
	.byte	34
	.byte	40
	.byte	49
	.byte	56
	.byte	63
	.globl _anubis_genesis_validators__validator_statusS
	.align	3
_anubis_genesis_validators__validator_statusS:
	.ascii "PENDING_APPLICATIONAPPROVEDACTIVEJAILEDUNBONDINGSLASHEDREMOVED"
	.globl _anubis_genesis_validators_E
	.data
	.align	1
_anubis_genesis_validators_E:
	.space 2
	.literal16
	.align	4
lC3:
	.xword	150000000
	.xword	5
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
	.quad	LFB48-.
	.set L$set$2,LFE48-LFB48
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB48
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
	.quad	LFB50-.
	.set L$set$8,LFE50-LFB50
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB50
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
	.quad	LFB127-.
	.set L$set$25,LFE127-LFB127
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
	.quad	LFB129-.
	.set L$set$29,LFE129-LFB129
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
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$32,LEFDE21-LASFDE21
	.long L$set$32
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB131-.
	.set L$set$33,LFE131-LFB131
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
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$36,LEFDE25-LASFDE25
	.long L$set$36
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB12-.
	.set L$set$37,LFE12-LFB12
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
	.quad	LFB17-.
	.set L$set$43,LFE17-LFB17
	.quad L$set$43
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI11-LFB17
	.long L$set$44
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$45,LCFI12-LCFI11
	.long L$set$45
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$46,LCFI13-LCFI12
	.long L$set$46
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI14-LCFI13
	.long L$set$47
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$48,LEFDE33-LASFDE33
	.long L$set$48
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB18-.
	.set L$set$49,LFE18-LFB18
	.quad L$set$49
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI15-LFB18
	.long L$set$50
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$51,LCFI16-LCFI15
	.long L$set$51
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$52,LCFI17-LCFI16
	.long L$set$52
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI18-LCFI17
	.long L$set$53
	.byte	0xb
	.byte	0x4
	.set L$set$54,LCFI19-LCFI18
	.long L$set$54
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI20-LCFI19
	.long L$set$55
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$56,LEFDE35-LASFDE35
	.long L$set$56
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB16-.
	.set L$set$57,LFE16-LFB16
	.quad L$set$57
	.uleb128 0
	.byte	0x4
	.set L$set$58,LCFI21-LFB16
	.long L$set$58
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$59,LCFI22-LCFI21
	.long L$set$59
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$60,LCFI23-LCFI22
	.long L$set$60
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$61,LCFI24-LCFI23
	.long L$set$61
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$62,LCFI25-LCFI24
	.long L$set$62
	.byte	0xb
	.byte	0x4
	.set L$set$63,LCFI26-LCFI25
	.long L$set$63
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$64,LEFDE37-LASFDE37
	.long L$set$64
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB20-.
	.set L$set$65,LFE20-LFB20
	.quad L$set$65
	.uleb128 0
	.byte	0x4
	.set L$set$66,LCFI27-LFB20
	.long L$set$66
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$67,LCFI28-LCFI27
	.long L$set$67
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$68,LCFI29-LCFI28
	.long L$set$68
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$69,LCFI30-LCFI29
	.long L$set$69
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$70,LEFDE39-LASFDE39
	.long L$set$70
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB21-.
	.set L$set$71,LFE21-LFB21
	.quad L$set$71
	.uleb128 0
	.byte	0x4
	.set L$set$72,LCFI31-LFB21
	.long L$set$72
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$73,LCFI32-LCFI31
	.long L$set$73
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$74,LCFI33-LCFI32
	.long L$set$74
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$75,LCFI34-LCFI33
	.long L$set$75
	.byte	0xb
	.byte	0x4
	.set L$set$76,LCFI35-LCFI34
	.long L$set$76
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$77,LCFI36-LCFI35
	.long L$set$77
	.byte	0xb
	.byte	0x4
	.set L$set$78,LCFI37-LCFI36
	.long L$set$78
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$79,LCFI38-LCFI37
	.long L$set$79
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$80,LEFDE41-LASFDE41
	.long L$set$80
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB22-.
	.set L$set$81,LFE22-LFB22
	.quad L$set$81
	.uleb128 0
	.byte	0x4
	.set L$set$82,LCFI39-LFB22
	.long L$set$82
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$83,LCFI40-LCFI39
	.long L$set$83
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$84,LCFI41-LCFI40
	.long L$set$84
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$85,LCFI42-LCFI41
	.long L$set$85
	.byte	0xb
	.byte	0x4
	.set L$set$86,LCFI43-LCFI42
	.long L$set$86
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$87,LCFI44-LCFI43
	.long L$set$87
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$88,LEFDE43-LASFDE43
	.long L$set$88
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB23-.
	.set L$set$89,LFE23-LFB23
	.quad L$set$89
	.uleb128 0
	.byte	0x4
	.set L$set$90,LCFI45-LFB23
	.long L$set$90
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$91,LCFI46-LCFI45
	.long L$set$91
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$92,LCFI47-LCFI46
	.long L$set$92
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$93,LCFI48-LCFI47
	.long L$set$93
	.byte	0xb
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$94,LEFDE45-LASFDE45
	.long L$set$94
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB24-.
	.set L$set$95,LFE24-LFB24
	.quad L$set$95
	.uleb128 0
	.byte	0x4
	.set L$set$96,LCFI49-LFB24
	.long L$set$96
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$97,LCFI50-LCFI49
	.long L$set$97
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$98,LCFI51-LCFI50
	.long L$set$98
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$99,LCFI52-LCFI51
	.long L$set$99
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$100,LEFDE47-LASFDE47
	.long L$set$100
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB25-.
	.set L$set$101,LFE25-LFB25
	.quad L$set$101
	.uleb128 0
	.byte	0x4
	.set L$set$102,LCFI53-LFB25
	.long L$set$102
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$103,LCFI54-LCFI53
	.long L$set$103
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$104,LEFDE49-LASFDE49
	.long L$set$104
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB26-.
	.set L$set$105,LFE26-LFB26
	.quad L$set$105
	.uleb128 0
	.byte	0x4
	.set L$set$106,LCFI55-LFB26
	.long L$set$106
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$107,LCFI56-LCFI55
	.long L$set$107
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$108,LEFDE51-LASFDE51
	.long L$set$108
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB27-.
	.set L$set$109,LFE27-LFB27
	.quad L$set$109
	.uleb128 0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$110,LEFDE53-LASFDE53
	.long L$set$110
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB28-.
	.set L$set$111,LFE28-LFB28
	.quad L$set$111
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI57-LFB28
	.long L$set$112
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$113,LCFI58-LCFI57
	.long L$set$113
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$114,LEFDE55-LASFDE55
	.long L$set$114
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB29-.
	.set L$set$115,LFE29-LFB29
	.quad L$set$115
	.uleb128 0
	.byte	0x4
	.set L$set$116,LCFI59-LFB29
	.long L$set$116
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$117,LCFI60-LCFI59
	.long L$set$117
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$118,LEFDE57-LASFDE57
	.long L$set$118
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB30-.
	.set L$set$119,LFE30-LFB30
	.quad L$set$119
	.uleb128 0
	.byte	0x4
	.set L$set$120,LCFI61-LFB30
	.long L$set$120
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$121,LCFI62-LCFI61
	.long L$set$121
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$122,LCFI63-LCFI62
	.long L$set$122
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$123,LCFI64-LCFI63
	.long L$set$123
	.byte	0xb
	.byte	0x4
	.set L$set$124,LCFI65-LCFI64
	.long L$set$124
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI66-LCFI65
	.long L$set$125
	.byte	0xb
	.byte	0x4
	.set L$set$126,LCFI67-LCFI66
	.long L$set$126
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$127,LCFI68-LCFI67
	.long L$set$127
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$128,LEFDE59-LASFDE59
	.long L$set$128
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB31-.
	.set L$set$129,LFE31-LFB31
	.quad L$set$129
	.uleb128 0
	.byte	0x4
	.set L$set$130,LCFI69-LFB31
	.long L$set$130
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$131,LCFI70-LCFI69
	.long L$set$131
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$132,LCFI71-LCFI70
	.long L$set$132
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$133,LCFI72-LCFI71
	.long L$set$133
	.byte	0xb
	.byte	0x4
	.set L$set$134,LCFI73-LCFI72
	.long L$set$134
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$135,LCFI74-LCFI73
	.long L$set$135
	.byte	0xb
	.byte	0x4
	.set L$set$136,LCFI75-LCFI74
	.long L$set$136
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$137,LCFI76-LCFI75
	.long L$set$137
	.byte	0xb
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$138,LEFDE61-LASFDE61
	.long L$set$138
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB32-.
	.set L$set$139,LFE32-LFB32
	.quad L$set$139
	.uleb128 0
	.byte	0x4
	.set L$set$140,LCFI77-LFB32
	.long L$set$140
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$141,LCFI78-LCFI77
	.long L$set$141
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$142,LCFI79-LCFI78
	.long L$set$142
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$143,LCFI80-LCFI79
	.long L$set$143
	.byte	0xb
	.byte	0x4
	.set L$set$144,LCFI81-LCFI80
	.long L$set$144
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$145,LCFI82-LCFI81
	.long L$set$145
	.byte	0xb
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$146,LEFDE63-LASFDE63
	.long L$set$146
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB33-.
	.set L$set$147,LFE33-LFB33
	.quad L$set$147
	.uleb128 0
	.byte	0x4
	.set L$set$148,LCFI83-LFB33
	.long L$set$148
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$149,LCFI84-LCFI83
	.long L$set$149
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$150,LCFI85-LCFI84
	.long L$set$150
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$151,LCFI86-LCFI85
	.long L$set$151
	.byte	0xb
	.byte	0x4
	.set L$set$152,LCFI87-LCFI86
	.long L$set$152
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$153,LCFI88-LCFI87
	.long L$set$153
	.byte	0xb
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$154,LEFDE65-LASFDE65
	.long L$set$154
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB34-.
	.set L$set$155,LFE34-LFB34
	.quad L$set$155
	.uleb128 0
	.byte	0x4
	.set L$set$156,LCFI89-LFB34
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
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$158,LEFDE67-LASFDE67
	.long L$set$158
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB35-.
	.set L$set$159,LFE35-LFB35
	.quad L$set$159
	.uleb128 0
	.byte	0x4
	.set L$set$160,LCFI91-LFB35
	.long L$set$160
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$161,LCFI92-LCFI91
	.long L$set$161
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$162,LCFI93-LCFI92
	.long L$set$162
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$163,LCFI94-LCFI93
	.long L$set$163
	.byte	0xb
	.byte	0x4
	.set L$set$164,LCFI95-LCFI94
	.long L$set$164
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$165,LCFI96-LCFI95
	.long L$set$165
	.byte	0xb
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$166,LEFDE69-LASFDE69
	.long L$set$166
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB36-.
	.set L$set$167,LFE36-LFB36
	.quad L$set$167
	.uleb128 0
	.byte	0x4
	.set L$set$168,LCFI97-LFB36
	.long L$set$168
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$169,LCFI98-LCFI97
	.long L$set$169
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$170,LEFDE71-LASFDE71
	.long L$set$170
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB38-.
	.set L$set$171,LFE38-LFB38
	.quad L$set$171
	.uleb128 0
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$172,LEFDE73-LASFDE73
	.long L$set$172
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB40-.
	.set L$set$173,LFE40-LFB40
	.quad L$set$173
	.uleb128 0
	.byte	0x4
	.set L$set$174,LCFI99-LFB40
	.long L$set$174
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$175,LCFI100-LCFI99
	.long L$set$175
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$176,LCFI101-LCFI100
	.long L$set$176
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$177,LCFI102-LCFI101
	.long L$set$177
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$178,LEFDE75-LASFDE75
	.long L$set$178
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB42-.
	.set L$set$179,LFE42-LFB42
	.quad L$set$179
	.uleb128 0
	.byte	0x4
	.set L$set$180,LCFI103-LFB42
	.long L$set$180
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$181,LCFI104-LCFI103
	.long L$set$181
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$182,LCFI105-LCFI104
	.long L$set$182
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$183,LCFI106-LCFI105
	.long L$set$183
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$184,LEFDE77-LASFDE77
	.long L$set$184
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB43-.
	.set L$set$185,LFE43-LFB43
	.quad L$set$185
	.uleb128 0
	.byte	0x4
	.set L$set$186,LCFI107-LFB43
	.long L$set$186
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$187,LCFI108-LCFI107
	.long L$set$187
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE77:
LSFDE79:
	.set L$set$188,LEFDE79-LASFDE79
	.long L$set$188
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB45-.
	.set L$set$189,LFE45-LFB45
	.quad L$set$189
	.uleb128 0
	.byte	0x4
	.set L$set$190,LCFI109-LFB45
	.long L$set$190
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$191,LCFI110-LCFI109
	.long L$set$191
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE79:
LSFDE81:
	.set L$set$192,LEFDE81-LASFDE81
	.long L$set$192
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB46-.
	.set L$set$193,LFE46-LFB46
	.quad L$set$193
	.uleb128 0
	.byte	0x4
	.set L$set$194,LCFI111-LFB46
	.long L$set$194
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$195,LCFI112-LCFI111
	.long L$set$195
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$196,LCFI113-LCFI112
	.long L$set$196
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$197,LCFI114-LCFI113
	.long L$set$197
	.byte	0xb
	.byte	0x4
	.set L$set$198,LCFI115-LCFI114
	.long L$set$198
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$199,LCFI116-LCFI115
	.long L$set$199
	.byte	0xb
	.byte	0x4
	.set L$set$200,LCFI117-LCFI116
	.long L$set$200
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$201,LCFI118-LCFI117
	.long L$set$201
	.byte	0xb
	.align	3
LEFDE81:
LSFDE83:
	.set L$set$202,LEFDE83-LASFDE83
	.long L$set$202
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB47-.
	.set L$set$203,LFE47-LFB47
	.quad L$set$203
	.uleb128 0
	.byte	0x4
	.set L$set$204,LCFI119-LFB47
	.long L$set$204
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$205,LCFI120-LCFI119
	.long L$set$205
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$206,LCFI121-LCFI120
	.long L$set$206
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$207,LCFI122-LCFI121
	.long L$set$207
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$208,LCFI123-LCFI122
	.long L$set$208
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
	.set L$set$209,LCFI124-LCFI123
	.long L$set$209
	.byte	0xb
	.align	3
LEFDE83:
LSFDE85:
	.set L$set$210,LEFDE85-LASFDE85
	.long L$set$210
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB49-.
	.set L$set$211,LFE49-LFB49
	.quad L$set$211
	.uleb128 0
	.byte	0x4
	.set L$set$212,LCFI125-LFB49
	.long L$set$212
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$213,LCFI126-LCFI125
	.long L$set$213
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$214,LCFI127-LCFI126
	.long L$set$214
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$215,LCFI128-LCFI127
	.long L$set$215
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$216,LCFI129-LCFI128
	.long L$set$216
	.byte	0xb
	.byte	0x4
	.set L$set$217,LCFI130-LCFI129
	.long L$set$217
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$218,LCFI131-LCFI130
	.long L$set$218
	.byte	0xb
	.align	3
LEFDE85:
LSFDE87:
	.set L$set$219,LEFDE87-LASFDE87
	.long L$set$219
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB51-.
	.set L$set$220,LFE51-LFB51
	.quad L$set$220
	.uleb128 0
	.align	3
LEFDE87:
LSFDE89:
	.set L$set$221,LEFDE89-LASFDE89
	.long L$set$221
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB52-.
	.set L$set$222,LFE52-LFB52
	.quad L$set$222
	.uleb128 0
	.align	3
LEFDE89:
LSFDE91:
	.set L$set$223,LEFDE91-LASFDE91
	.long L$set$223
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB53-.
	.set L$set$224,LFE53-LFB53
	.quad L$set$224
	.uleb128 0
	.align	3
LEFDE91:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
