	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_types__map_entryIP
_khepri_types__map_entryIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Tstorage_mapBIP
_khepri_types__Tstorage_mapBIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__address_entryIP
_khepri_types__address_entryIP:
LFB78:
	ret
LFE78:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Taddress_mapBIP
_khepri_types__Taddress_mapBIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Tevent_topicsBIP
_khepri_types__Tevent_topicsBIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Tevent_dataBIP
_khepri_types__Tevent_dataBIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__eventIP
_khepri_types__eventIP:
LFB80:
	ret
LFE80:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Tcontract_stringBIP
_khepri_types__Tcontract_stringBIP:
LFB9:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__bounded_stringIP
_khepri_types__bounded_stringIP:
LFB82:
	ret
LFE82:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__error_codeH
_khepri_types__error_codeH:
LFB11:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _error_codeP.7@PAGE
	add	w5, w10, 1
	add	x9, x9, _error_codeP.7@PAGEOFF;
	adrp	x12, _error_codeT1.6@PAGE
	adrp	x11, _error_codeT2.5@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _error_codeT1.6@PAGEOFF;
	add	x11, x11, _error_codeT2.5@PAGEOFF;
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
	blt	L13
L17:
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
	beq	L13
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L17
L13:
	adrp	x3, _error_codeG.4@PAGE
	mov	w1, 52429
	add	x3, x3, _error_codeG.4@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 35
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1, lsl 1
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__resultD2
_khepri_types__resultD2:
LFB12:
	eor	w0, w0, 1
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__resultD3
_khepri_types__resultD3:
LFB13:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__resultEQ
_khepri_types__resultEQ:
LFB14:
	ldrb	w3, [x0]
	ldrb	w2, [x1]
	cmp	w2, w3
	bne	L25
	cbz	w2, L22
	ldr	x3, [x0, 8]
	add	x4, x1, 8
	mov	w2, 1
	add	x0, x0, 8
	ldr	x1, [x1, 8]
	cmp	x3, x1
	beq	L26
L24:
	mov	w2, 0
L23:
	and	w0, w2, 255
	ret
	.p2align 2,,3
L25:
	mov	w0, 0
	ret
	.p2align 2,,3
L22:
	ldrb	w2, [x0, 8]
	ldrb	w0, [x1, 8]
	cmp	w2, w0
	cset	w0, eq
	ret
	.p2align 2,,3
L26:
	ldr	x3, [x0, 8]
	ldr	x1, [x4, 8]
	cmp	x3, x1
	bne	L24
	ldr	x3, [x0, 16]
	ldr	x1, [x4, 16]
	cmp	x3, x1
	bne	L24
	ldr	x1, [x0, 24]
	ldr	x0, [x4, 24]
	cmp	x1, x0
	beq	L23
	mov	w2, 0
	b	L23
LFE14:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__resultIP
_khepri_types__resultIP:
LFB15:
	strb	w1, [x0]
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__bool_resultD2
_khepri_types__bool_resultD2:
LFB90:
	eor	w0, w0, 1
	ret
LFE90:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__bool_resultD3
_khepri_types__bool_resultD3:
LFB92:
	ret
LFE92:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__bool_resultEQ
_khepri_types__bool_resultEQ:
LFB18:
	and	w2, w1, 255
	cmp	w2, w0, uxtb
	bne	L32
	ubfx	x1, x1, 8, 8
	ubfx	x0, x0, 8, 8
	cmp	w1, w0
	cset	w0, eq
	ret
	.p2align 2,,3
L32:
	mov	w0, 0
	ret
LFE18:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__bool_resultIP
_khepri_types__bool_resultIP:
LFB19:
	ret
LFE19:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__msg_contextIP
_khepri_types__msg_contextIP:
LFB84:
	ret
LFE84:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__block_contextIP
_khepri_types__block_contextIP:
LFB86:
	ret
LFE86:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__tx_contextIP
_khepri_types__tx_contextIP:
LFB88:
	ret
LFE88:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Tbytes4BIP
_khepri_types__Tbytes4BIP:
LFB23:
	ret
LFE23:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Tbytes8BIP
_khepri_types__Tbytes8BIP:
LFB24:
	ret
LFE24:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Tbytes20BIP
_khepri_types__Tbytes20BIP:
LFB25:
	ret
LFE25:
	.const
	.align	3
lC3:
	.ascii "khepri_types.adb"
	.space 1
	.align	3
lC4:
	.ascii "khepri_types.ads"
	.space 1
	.align	3
lC5:
	.ascii "failed postcondition from khepri_types.ads:69"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_types__from_natural
_khepri_types__from_natural:
LFB26:
	stp	x29, x30, [sp, -112]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI2:
	tbnz	w0, #31, L45
	sxtw	x19, w0
	add	x21, x29, 48
	mov	x0, x19
	mov	x20, x8
	mov	x8, x21
	bl	_aegis_u256__from_word64
	add	x1, x29, 80
	mov	x0, x19
	mov	x8, x1
	mov	x19, x1
	bl	_aegis_u256__from_word64
	mov	x1, x19
	mov	x0, x21
	bl	_aegis_u256__equal
	cmp	w0, 1
	bhi	L46
	cbz	w0, L47
	ld1	{v30.16b - v31.16b}, [x21]
	st1	{v30.16b - v31.16b}, [x20]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI3:
	ret
L45:
LCFI4:
	adrp	x0, lC3@PAGE
	mov	w1, 11
	add	x0, x0, lC3@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L47:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L46:
	adrp	x0, lC4@PAGE
	mov	w1, 69
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.const
	.align	2
lC0:
	.word	1
	.word	45
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Oadd
_khepri_types__Oadd:
LFB28:
	stp	x29, x30, [sp, -16]!
LCFI5:
	mov	x29, sp
LCFI6:
	bl	_aegis_u256__add_mod
	ldp	x29, x30, [sp], 16
LCFI7:
	ret
LFE28:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Osubtract
_khepri_types__Osubtract:
LFB29:
	stp	x29, x30, [sp, -16]!
LCFI8:
	mov	x29, sp
LCFI9:
	bl	_aegis_u256__sub_mod
	ldp	x29, x30, [sp], 16
LCFI10:
	ret
LFE29:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Omultiply
_khepri_types__Omultiply:
LFB30:
	stp	x29, x30, [sp, -16]!
LCFI11:
	mov	x29, sp
LCFI12:
	bl	_aegis_u256__mul_mod
	ldp	x29, x30, [sp], 16
LCFI13:
	ret
LFE30:
	.const
	.align	3
lC6:
	.ascii "failed precondition from khepri_types.ads:90"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Odivide
_khepri_types__Odivide:
LFB31:
	stp	x29, x30, [sp, -48]!
LCFI14:
	mov	x29, sp
LCFI15:
	stp	x19, x20, [sp, 16]
LCFI16:
	mov	x20, x0
	mov	x0, x1
	mov	x19, x1
	str	x21, [sp, 32]
LCFI17:
	mov	x21, x8
	bl	_aegis_u256__is_zero
	cmp	w0, 1
	bhi	L58
	cbnz	w0, L59
	mov	x8, x21
	mov	x1, x19
	mov	x0, x20
	bl	_aegis_u256__div
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI18:
	ret
L58:
LCFI19:
	adrp	x0, lC4@PAGE
	mov	w1, 90
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L59:
	adrp	x0, lC6@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE31:
	.const
	.align	2
lC1:
	.word	1
	.word	44
	.text
	.const
	.align	3
lC7:
	.ascii "failed precondition from khepri_types.ads:95"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_types__Omod
_khepri_types__Omod:
LFB32:
	stp	x29, x30, [sp, -48]!
LCFI20:
	mov	x29, sp
LCFI21:
	stp	x19, x20, [sp, 16]
LCFI22:
	mov	x20, x0
	mov	x0, x1
	mov	x19, x1
	str	x21, [sp, 32]
LCFI23:
	mov	x21, x8
	bl	_aegis_u256__is_zero
	cmp	w0, 1
	bhi	L64
	cbnz	w0, L65
	mov	x8, x21
	mov	x1, x19
	mov	x0, x20
	bl	_aegis_u256__mod_op
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI24:
	ret
L64:
LCFI25:
	adrp	x0, lC4@PAGE
	mov	w1, 95
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L65:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE32:
	.const
	.align	3
lC8:
	.ascii "failed postcondition from khepri_types.ads:224"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_types__ok
_khepri_types__ok:
LFB33:
	mov	x1, x0
	stp	x29, x30, [sp, -80]!
LCFI26:
	mov	x29, sp
LCFI27:
	add	x2, x29, 40
	mov	w0, 1
	add	x3, x29, 48
	ld1	{v30.16b - v31.16b}, [x1]
	str	x19, [sp, 16]
LCFI28:
	strb	w0, [x29, 40]
	st1	{v30.16b - v31.16b}, [x3]
	ldp	q31, q30, [x2]
	ldr	x0, [x2, 32]
	str	x0, [x8, 32]
	umov	w0, v31.b[0]
	stp	q31, q30, [x8]
	tst	w0, 255
	beq	L71
	add	x0, x8, 8
	mov	x19, x8
	bl	_aegis_u256__equal
	cmp	w0, 1
	bhi	L72
	cbz	w0, L73
	mov	x0, x19
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI29:
	ret
L71:
LCFI30:
	adrp	x0, lC4@PAGE
	mov	w1, 224
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Discriminant_Check
L73:
	adrp	x0, lC8@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L72:
	adrp	x0, lC4@PAGE
	mov	w1, 224
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE33:
	.const
	.align	2
lC2:
	.word	1
	.word	46
	.text
	.const
	.align	3
lC9:
	.ascii "failed precondition from khepri_types.ads:228"
	.align	3
lC10:
	.ascii "failed postcondition from khepri_types.ads:229"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_types__err
_khepri_types__err:
LFB35:
	stp	x29, x30, [sp, -16]!
LCFI31:
	mov	x29, sp
LCFI32:
	mov	w1, w0
	cmp	w0, 9
	bhi	L83
	cbz	w0, L84
	mov	x2, 0
	mov	w3, 0
	bfi	x2, x1, 0, 8
	mov	x0, x8
	stp	xzr, x2, [x8]
	cbnz	w3, L85
	ldrb	w2, [x8, 8]
	cmp	w2, 9
	bhi	L79
	cmp	w1, w2
	bne	L86
	ldp	x29, x30, [sp], 16
LCFI33:
	ret
L79:
LCFI34:
	adrp	x0, lC4@PAGE
	mov	w1, 229
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L85:
	adrp	x0, lC4@PAGE
	mov	w1, 229
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Discriminant_Check
L84:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L83:
	adrp	x0, lC4@PAGE
	mov	w1, 228
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L86:
	adrp	x0, lC10@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE35:
	.align	2
	.p2align 5,,15
	.globl _khepri_types__ok_bool
_khepri_types__ok_bool:
LFB37:
	cmp	w0, 1
	bhi	L92
	mov	w1, w0
	mov	w2, 1
	mov	w0, 0
	bfi	w0, w2, 0, 8
	bfi	w0, w1, 8, 8
	ret
L92:
	adrp	x0, lC4@PAGE
	stp	x29, x30, [sp, -16]!
LCFI35:
	mov	w1, 246
	mov	x29, sp
LCFI36:
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE37:
	.align	2
	.p2align 5,,15
	.globl _khepri_types___elabs
_khepri_types___elabs:
LFB1:
	adrp	x0, _khepri_types__empty_address_map@PAGE
	mov	x2, 18432
	add	x0, x0, _khepri_types__empty_address_map@PAGEOFF;
	mov	w1, 0
	b	_memset
LFE1:
	.const
_error_codeG.4:
	.byte	9
	.byte	8
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	7
	.byte	7
	.byte	0
	.byte	0
	.byte	4
	.byte	4
	.byte	3
	.byte	0
	.byte	6
	.byte	0
	.byte	0
	.align	1
_error_codeT2.5:
	.byte	4
	.byte	9
	.align	1
_error_codeT1.6:
	.byte	6
	.byte	4
	.align	3
_error_codeP.7:
	.word	1
	.word	3
	.globl _khepri_types__error_codeN
	.align	3
_khepri_types__error_codeN:
	.byte	1
	.byte	9
	.byte	29
	.byte	41
	.byte	54
	.byte	62
	.byte	71
	.byte	80
	.byte	94
	.byte	104
	.byte	118
	.space 5
	.globl _khepri_types__error_codeS
	.align	3
_khepri_types__error_codeS:
	.ascii "NO_ERRORINSUFFICIENT_BALANCEUNAUTHORIZEDINVALID_INPUTOVERFLOWUNDERFLOWNOT_FOUNDALREADY_EXISTSOUT_OF_GASCONTRACT_ERROR"
	.globl _khepri_types__empty_string
	.align	2
_khepri_types__empty_string:
	.space 36
	.globl _khepri_types__empty_event
	.align	2
_khepri_types__empty_event:
	.space 392
	.globl _khepri_types__empty_address_map
	.zerofill __DATA,__common,_khepri_types__empty_address_map,18432,3
	.globl _khepri_types__empty_map
	.const
	.align	3
_khepri_types__empty_map:
	.space 18432
	.globl _khepri_types__bytes32_zero
_khepri_types__bytes32_zero:
	.space 32
	.globl _khepri_types__null_address
_khepri_types__null_address:
	.space 32
	.globl _khepri_types__max
	.align	3
_khepri_types__max:
	.xword	-1
	.xword	-1
	.xword	-1
	.xword	-1
	.globl _khepri_types__one
	.align	3
_khepri_types__one:
	.xword	1
	.xword	0
	.xword	0
	.xword	0
	.globl _khepri_types__zero
	.align	3
_khepri_types__zero:
	.space 32
	.globl _khepri_types_E
	.data
	.align	1
_khepri_types_E:
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
	.quad	LFB78-.
	.set L$set$6,LFE78-LFB78
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
	.quad	LFB7-.
	.set L$set$12,LFE7-LFB7
	.quad L$set$12
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$13,LEFDE13-LASFDE13
	.long L$set$13
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB80-.
	.set L$set$14,LFE80-LFB80
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
	.quad	LFB82-.
	.set L$set$18,LFE82-LFB82
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
	.quad	LFB14-.
	.set L$set$26,LFE14-LFB14
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
	.quad	LFB90-.
	.set L$set$30,LFE90-LFB90
	.quad L$set$30
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$31,LEFDE31-LASFDE31
	.long L$set$31
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB92-.
	.set L$set$32,LFE92-LFB92
	.quad L$set$32
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$33,LEFDE33-LASFDE33
	.long L$set$33
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB18-.
	.set L$set$34,LFE18-LFB18
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
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$37,LEFDE37-LASFDE37
	.long L$set$37
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB84-.
	.set L$set$38,LFE84-LFB84
	.quad L$set$38
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$39,LEFDE39-LASFDE39
	.long L$set$39
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB86-.
	.set L$set$40,LFE86-LFB86
	.quad L$set$40
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$41,LEFDE41-LASFDE41
	.long L$set$41
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB88-.
	.set L$set$42,LFE88-LFB88
	.quad L$set$42
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$43,LEFDE43-LASFDE43
	.long L$set$43
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB23-.
	.set L$set$44,LFE23-LFB23
	.quad L$set$44
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$45,LEFDE45-LASFDE45
	.long L$set$45
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB24-.
	.set L$set$46,LFE24-LFB24
	.quad L$set$46
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$47,LEFDE47-LASFDE47
	.long L$set$47
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB25-.
	.set L$set$48,LFE25-LFB25
	.quad L$set$48
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$49,LEFDE49-LASFDE49
	.long L$set$49
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB26-.
	.set L$set$50,LFE26-LFB26
	.quad L$set$50
	.uleb128 0
	.byte	0x4
	.set L$set$51,LCFI0-LFB26
	.long L$set$51
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$52,LCFI1-LCFI0
	.long L$set$52
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$53,LCFI2-LCFI1
	.long L$set$53
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x4
	.set L$set$54,LCFI3-LCFI2
	.long L$set$54
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
	.set L$set$55,LCFI4-LCFI3
	.long L$set$55
	.byte	0xb
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$56,LEFDE51-LASFDE51
	.long L$set$56
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB28-.
	.set L$set$57,LFE28-LFB28
	.quad L$set$57
	.uleb128 0
	.byte	0x4
	.set L$set$58,LCFI5-LFB28
	.long L$set$58
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$59,LCFI6-LCFI5
	.long L$set$59
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$60,LCFI7-LCFI6
	.long L$set$60
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$61,LEFDE53-LASFDE53
	.long L$set$61
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB29-.
	.set L$set$62,LFE29-LFB29
	.quad L$set$62
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI8-LFB29
	.long L$set$63
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$64,LCFI9-LCFI8
	.long L$set$64
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$65,LCFI10-LCFI9
	.long L$set$65
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$66,LEFDE55-LASFDE55
	.long L$set$66
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB30-.
	.set L$set$67,LFE30-LFB30
	.quad L$set$67
	.uleb128 0
	.byte	0x4
	.set L$set$68,LCFI11-LFB30
	.long L$set$68
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$69,LCFI12-LCFI11
	.long L$set$69
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$70,LCFI13-LCFI12
	.long L$set$70
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$71,LEFDE57-LASFDE57
	.long L$set$71
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB31-.
	.set L$set$72,LFE31-LFB31
	.quad L$set$72
	.uleb128 0
	.byte	0x4
	.set L$set$73,LCFI14-LFB31
	.long L$set$73
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$74,LCFI15-LCFI14
	.long L$set$74
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$75,LCFI16-LCFI15
	.long L$set$75
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$76,LCFI17-LCFI16
	.long L$set$76
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$77,LCFI18-LCFI17
	.long L$set$77
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
	.set L$set$78,LCFI19-LCFI18
	.long L$set$78
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$79,LEFDE59-LASFDE59
	.long L$set$79
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB32-.
	.set L$set$80,LFE32-LFB32
	.quad L$set$80
	.uleb128 0
	.byte	0x4
	.set L$set$81,LCFI20-LFB32
	.long L$set$81
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$82,LCFI21-LCFI20
	.long L$set$82
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$83,LCFI22-LCFI21
	.long L$set$83
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$84,LCFI23-LCFI22
	.long L$set$84
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$85,LCFI24-LCFI23
	.long L$set$85
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
	.set L$set$86,LCFI25-LCFI24
	.long L$set$86
	.byte	0xb
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$87,LEFDE61-LASFDE61
	.long L$set$87
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB33-.
	.set L$set$88,LFE33-LFB33
	.quad L$set$88
	.uleb128 0
	.byte	0x4
	.set L$set$89,LCFI26-LFB33
	.long L$set$89
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$90,LCFI27-LCFI26
	.long L$set$90
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$91,LCFI28-LCFI27
	.long L$set$91
	.byte	0x93
	.uleb128 0x8
	.byte	0x4
	.set L$set$92,LCFI29-LCFI28
	.long L$set$92
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$93,LCFI30-LCFI29
	.long L$set$93
	.byte	0xb
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$94,LEFDE63-LASFDE63
	.long L$set$94
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB35-.
	.set L$set$95,LFE35-LFB35
	.quad L$set$95
	.uleb128 0
	.byte	0x4
	.set L$set$96,LCFI31-LFB35
	.long L$set$96
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$97,LCFI32-LCFI31
	.long L$set$97
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$98,LCFI33-LCFI32
	.long L$set$98
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$99,LCFI34-LCFI33
	.long L$set$99
	.byte	0xb
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$100,LEFDE65-LASFDE65
	.long L$set$100
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB37-.
	.set L$set$101,LFE37-LFB37
	.quad L$set$101
	.uleb128 0
	.byte	0x4
	.set L$set$102,LCFI35-LFB37
	.long L$set$102
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$103,LCFI36-LCFI35
	.long L$set$103
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$104,LEFDE67-LASFDE67
	.long L$set$104
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB1-.
	.set L$set$105,LFE1-LFB1
	.quad L$set$105
	.uleb128 0
	.align	3
LEFDE67:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
