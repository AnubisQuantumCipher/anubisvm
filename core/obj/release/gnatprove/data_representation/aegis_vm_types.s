	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__Tu256_limbsBIP
_aegis_vm_types__Tu256_limbsBIP:
LFB1:
	ret
LFE1:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__u256IP
_aegis_vm_types__u256IP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__Thash256BIP
_aegis_vm_types__Thash256BIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__Thash512BIP
_aegis_vm_types__Thash512BIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__Tcontract_addressBIP
_aegis_vm_types__Tcontract_addressBIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__certification_levelH
_aegis_vm_types__certification_levelH:
LFB6:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L9
	ldrb	w1, [x0]
	mov	w2, 60495
	movk	w2, 0x4ec4, lsl 16
	add	w0, w1, w1, lsl 1
	lsl	w1, w1, 3
	umull	x3, w1, w2
	umull	x2, w0, w2
	lsr	x3, x3, 34
	lsr	x2, x2, 34
	add	w5, w3, w3, lsl 1
	add	w4, w2, w2, lsl 1
	add	w3, w3, w5, lsl 2
	add	w2, w2, w4, lsl 2
	sub	w1, w1, w3
	sub	w0, w0, w2
	adrp	x2, _certification_levelG.12@PAGE
	add	x2, x2, _certification_levelG.12@PAGEOFF;
	sxtw	x1, w1
	sxtw	x0, w0
	ldrb	w1, [x2, x1]
	ldrb	w0, [x2, x0]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L9:
	adrp	x2, _certification_levelG.12@PAGE
	mov	x1, 0
	add	x2, x2, _certification_levelG.12@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x2, x0]
	ldrb	w1, [x2, x1]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__execution_statusH
_aegis_vm_types__execution_statusH:
LFB7:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _execution_statusP.11@PAGE
	add	w5, w10, 1
	add	x9, x9, _execution_statusP.11@PAGEOFF;
	adrp	x12, _execution_statusT1.10@PAGE
	adrp	x11, _execution_statusT2.9@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _execution_statusT1.10@PAGEOFF;
	add	x11, x11, _execution_statusT2.9@PAGEOFF;
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
	blt	L12
L16:
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
	beq	L12
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L16
L12:
	adrp	x3, _execution_statusG.8@PAGE
	mov	w0, 7
	add	x3, x3, _execution_statusG.8@PAGEOFF;
	ldrb	w2, [x3, w2, sxtw]
	ldrb	w1, [x3, w6, sxtw]
	add	w1, w1, w2
	udiv	w2, w1, w0
	lsl	w0, w2, 3
	sub	w0, w0, w2
	sub	w0, w1, w0
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__capability_typeH
_aegis_vm_types__capability_typeH:
LFB8:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _capability_typeP.7@PAGE
	add	w5, w10, 1
	add	x9, x9, _capability_typeP.7@PAGEOFF;
	adrp	x12, _capability_typeT1.6@PAGE
	adrp	x11, _capability_typeT2.5@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _capability_typeT1.6@PAGEOFF;
	add	x11, x11, _capability_typeT2.5@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 19
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L19
L23:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	add	w6, w2, w2, lsl 3
	add	w6, w2, w6, lsl 1
	add	w2, w5, w5, lsl 3
	sub	w6, w3, w6
	add	w2, w5, w2, lsl 1
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L19
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L23
L19:
	adrp	x3, _capability_typeG.4@PAGE
	mov	w1, 36409
	add	x3, x3, _capability_typeG.4@PAGEOFF;
	movk	w1, 0x38e3, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 33
	add	w1, w1, w1, lsl 3
	sub	w0, w0, w1
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__Tcapability_maskBIP
_aegis_vm_types__Tcapability_maskBIP:
LFB9:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__byte_arrayIP
_aegis_vm_types__byte_arrayIP:
LFB10:
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__gas_contextIP
_aegis_vm_types__gas_contextIP:
LFB82:
	ret
LFE82:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__memory_regionIP
_aegis_vm_types__memory_regionIP:
LFB84:
	ret
LFE84:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__Tmemory_regionsBIP
_aegis_vm_types__Tmemory_regionsBIP:
LFB13:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__storage_slotIP
_aegis_vm_types__storage_slotIP:
LFB86:
	ret
LFE86:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__call_typeH
_aegis_vm_types__call_typeH:
LFB15:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _call_typeP.3@PAGE
	add	w5, w10, 1
	add	x9, x9, _call_typeP.3@PAGEOFF;
	adrp	x12, _call_typeT1.2@PAGE
	adrp	x11, _call_typeT2.1@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _call_typeT1.2@PAGEOFF;
	add	x11, x11, _call_typeT2.1@PAGEOFF;
	add	w8, w3, w10
	cmp	w3, w1
	sxtw	x15, w3
	sub	w1, w1, w3
	ldr	w3, [x9, w5, sxtw 2]
	sxtw	x8, w8
	csinc	w4, wzr, w1, gt
	mov	w14, 2
	add	x1, x8, w3, sxtw
	mov	w13, 11
	mov	w10, 0
	sub	x1, x1, x15
	cmp	w4, w3
	blt	L32
L36:
	ldrb	w7, [x0, x1]
	ldrb	w3, [x12, w5, sxtw]
	ldrb	w1, [x11, w5, sxtw]
	madd	w3, w3, w7, w6
	madd	w1, w1, w7, w2
	sdiv	w2, w3, w13
	sdiv	w5, w1, w13
	add	w6, w2, w2, lsl 2
	add	w6, w2, w6, lsl 1
	add	w2, w5, w5, lsl 2
	sub	w6, w3, w6
	add	w2, w5, w2, lsl 1
	sub	w2, w1, w2
	cmp	w14, 1
	beq	L32
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L36
L32:
	adrp	x3, _call_typeG.0@PAGE
	mov	w1, 52429
	add	x3, x3, _call_typeG.0@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__execution_resultIP
_aegis_vm_types__execution_resultIP:
LFB88:
	ret
LFE88:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__versionIP
_aegis_vm_types__versionIP:
LFB17:
	sub	sp, sp, #16
LCFI0:
	mov	x1, 0
	stp	wzr, wzr, [sp]
	ldr	x0, [sp]
	add	sp, sp, 16
LCFI1:
	ret
LFE17:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__contract_manifestIP
_aegis_vm_types__contract_manifestIP:
LFB90:
	ret
LFE90:
	.const
	.align	3
lC1:
	.ascii "aegis_vm_types.ads"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__get_discount
_aegis_vm_types__get_discount:
LFB19:
	cmp	w0, 3
	bhi	L48
	mov	w3, -1000
	mov	w2, 10000
	mov	w1, 7000
	madd	w0, w0, w3, w2
	and	w0, w0, 65535
	csel	w0, w0, w1, ne
	sxth	w0, w0
	ret
L48:
	adrp	x0, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI2:
	mov	w1, 90
	mov	x29, sp
LCFI3:
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE19:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__initial_gas_context
_aegis_vm_types__initial_gas_context:
LFB20:
	cmp	w2, 3
	bhi	L56
	ld1	{v30.16b - v31.16b}, [x1]
	mov	w5, -1000
	mov	w4, 10000
	mov	w3, 7000
	add	x1, x8, 16
	stp	x0, xzr, [x8]
	madd	w2, w2, w5, w4
	and	w2, w2, 65535
	csel	w2, w2, w3, ne
	strh	w2, [x8, 48]
	st1	{v30.16b - v31.16b}, [x1]
	ret
L56:
	adrp	x0, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI4:
	mov	w1, 129
	mov	x29, sp
LCFI5:
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__success_result
_aegis_vm_types__success_result:
LFB21:
	strb	wzr, [x8]
	ldp	q31, q30, [x1]
	str	x0, [x8, 8]
	stp	q31, q30, [x8, 16]
	ret
LFE21:
	.const
	.align	3
lC2:
	.ascii "failed precondition from aegis_vm_types.ads:222"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_types__failure_result
_aegis_vm_types__failure_result:
LFB22:
	stp	x29, x30, [sp, -16]!
LCFI6:
	mov	x29, sp
LCFI7:
	cmp	w0, 6
	bhi	L62
	cbz	w0, L63
	stp	xzr, xzr, [x8]
	strb	w0, [x8]
	str	x1, [x8, 8]
	stp	xzr, xzr, [x8, 16]
	stp	xzr, xzr, [x8, 32]
	ldp	x29, x30, [sp], 16
LCFI8:
	ret
L62:
LCFI9:
	adrp	x0, lC1@PAGE
	mov	w1, 222
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L63:
	adrp	x0, lC2@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC2@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE22:
	.const
	.align	2
lC0:
	.word	1
	.word	47
	.text
	.const
	.align	3
_call_typeG.0:
	.byte	0
	.byte	0
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	2
	.byte	4
	.byte	0
	.byte	0
	.byte	3
	.space 5
	.align	1
_call_typeT2.1:
	.byte	3
	.byte	9
	.align	1
_call_typeT1.2:
	.byte	2
	.byte	2
	.align	3
_call_typeP.3:
	.word	2
	.word	7
_capability_typeG.4:
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	1
	.byte	0
	.byte	7
	.byte	2
	.byte	0
	.byte	8
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	3
	.align	1
_capability_typeT2.5:
	.byte	17
	.byte	17
	.align	1
_capability_typeT1.6:
	.byte	9
	.byte	17
	.align	3
_capability_typeP.7:
	.word	5
	.word	8
	.align	3
_execution_statusG.8:
	.byte	0
	.byte	0
	.byte	5
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	6
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.byte	1
	.space 1
	.align	1
_execution_statusT2.9:
	.byte	5
	.byte	1
	.align	1
_execution_statusT1.10:
	.byte	7
	.byte	3
	.align	3
_execution_statusP.11:
	.word	1
	.word	2
	.align	3
_certification_levelG.12:
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.space 3
	.globl _aegis_vm_types__call_typeN
	.align	3
_aegis_vm_types__call_typeN:
	.byte	1
	.byte	5
	.byte	16
	.byte	29
	.byte	35
	.byte	42
	.space 2
	.globl _aegis_vm_types__call_typeS
	.align	3
_aegis_vm_types__call_typeS:
	.ascii "CALLSTATIC_CALLDELEGATE_CALLCREATECREATE2"
	.globl _aegis_vm_types__memory_accessN
	.align	2
_aegis_vm_types__memory_accessN:
	.byte	1
	.byte	10
	.byte	20
	.byte	27
	.globl _aegis_vm_types__memory_accessS
	.align	3
_aegis_vm_types__memory_accessS:
	.ascii "READ_ONLYREAD_WRITEEXECUTE"
	.globl _aegis_vm_types__read_only_capabilities
	.align	3
_aegis_vm_types__read_only_capabilities:
	.byte	0
	.byte	1
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.space 7
	.globl _aegis_vm_types__full_capabilities
	.align	3
_aegis_vm_types__full_capabilities:
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.space 7
	.globl _aegis_vm_types__no_capabilities
	.align	3
_aegis_vm_types__no_capabilities:
	.space 16
	.globl _aegis_vm_types__capability_typeN
	.align	3
_aegis_vm_types__capability_typeN:
	.byte	1
	.byte	9
	.byte	25
	.byte	42
	.byte	50
	.byte	62
	.byte	72
	.byte	89
	.byte	99
	.byte	108
	.space 6
	.globl _aegis_vm_types__capability_typeS
	.align	3
_aegis_vm_types__capability_typeS:
	.ascii "CAP_NONECAP_READ_STORAGECAP_WRITE_STORAGECAP_CALLCAP_TRANSFERCAP_CREATECAP_SELF_DESTRUCTCAP_CRYPTOCAP_EVENT"
	.globl _aegis_vm_types__execution_statusN
	.align	3
_aegis_vm_types__execution_statusN:
	.byte	1
	.byte	8
	.byte	14
	.byte	24
	.byte	38
	.byte	52
	.byte	68
	.byte	82
	.globl _aegis_vm_types__execution_statusS
	.align	3
_aegis_vm_types__execution_statusS:
	.ascii "SUCCESSREVERTOUT_OF_GASSTACK_OVERFLOWINVALID_OPCODEACCESS_VIOLATIONCONTRACT_ERROR"
	.globl _aegis_vm_types__certification_levelN
	.align	3
_aegis_vm_types__certification_levelN:
	.byte	1
	.byte	7
	.byte	13
	.byte	17
	.byte	25
	.space 3
	.globl _aegis_vm_types__certification_levelS
	.align	3
_aegis_vm_types__certification_levelS:
	.ascii "BRONZESILVERGOLDPLATINUM"
	.globl _aegis_vm_types__address_zero
_aegis_vm_types__address_zero:
	.space 32
	.globl _aegis_vm_types__hash512_zero
_aegis_vm_types__hash512_zero:
	.space 64
	.globl _aegis_vm_types__hash256_zero
_aegis_vm_types__hash256_zero:
	.space 32
	.globl _aegis_vm_types__u256_max
	.align	3
_aegis_vm_types__u256_max:
	.xword	-1
	.xword	-1
	.xword	-1
	.xword	-1
	.globl _aegis_vm_types__u256_one
	.align	3
_aegis_vm_types__u256_one:
	.xword	1
	.xword	0
	.xword	0
	.xword	0
	.globl _aegis_vm_types__u256_zero
	.align	3
_aegis_vm_types__u256_zero:
	.space 32
	.globl _aegis_vm_types__max_gas_per_block
	.align	3
_aegis_vm_types__max_gas_per_block:
	.xword	100000000
	.globl _aegis_vm_types__max_gas_per_tx
	.align	3
_aegis_vm_types__max_gas_per_tx:
	.xword	30000000
	.globl _aegis_vm_types_E
	.data
	.align	1
_aegis_vm_types_E:
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
	.quad	LFB1-.
	.set L$set$2,LFE1-LFB1
	.quad L$set$2
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$3,LEFDE3-LASFDE3
	.long L$set$3
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB2-.
	.set L$set$4,LFE2-LFB2
	.quad L$set$4
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$5,LEFDE5-LASFDE5
	.long L$set$5
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB3-.
	.set L$set$6,LFE3-LFB3
	.quad L$set$6
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$7,LEFDE7-LASFDE7
	.long L$set$7
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB4-.
	.set L$set$8,LFE4-LFB4
	.quad L$set$8
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$9,LEFDE9-LASFDE9
	.long L$set$9
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB5-.
	.set L$set$10,LFE5-LFB5
	.quad L$set$10
	.uleb128 0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$11,LEFDE11-LASFDE11
	.long L$set$11
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB6-.
	.set L$set$12,LFE6-LFB6
	.quad L$set$12
	.uleb128 0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$13,LEFDE13-LASFDE13
	.long L$set$13
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB7-.
	.set L$set$14,LFE7-LFB7
	.quad L$set$14
	.uleb128 0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$15,LEFDE15-LASFDE15
	.long L$set$15
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB8-.
	.set L$set$16,LFE8-LFB8
	.quad L$set$16
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$17,LEFDE17-LASFDE17
	.long L$set$17
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB9-.
	.set L$set$18,LFE9-LFB9
	.quad L$set$18
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$19,LEFDE19-LASFDE19
	.long L$set$19
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB10-.
	.set L$set$20,LFE10-LFB10
	.quad L$set$20
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$21,LEFDE21-LASFDE21
	.long L$set$21
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB82-.
	.set L$set$22,LFE82-LFB82
	.quad L$set$22
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$23,LEFDE23-LASFDE23
	.long L$set$23
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB84-.
	.set L$set$24,LFE84-LFB84
	.quad L$set$24
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$25,LEFDE25-LASFDE25
	.long L$set$25
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB13-.
	.set L$set$26,LFE13-LFB13
	.quad L$set$26
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$27,LEFDE27-LASFDE27
	.long L$set$27
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB86-.
	.set L$set$28,LFE86-LFB86
	.quad L$set$28
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$29,LEFDE29-LASFDE29
	.long L$set$29
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB15-.
	.set L$set$30,LFE15-LFB15
	.quad L$set$30
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$31,LEFDE31-LASFDE31
	.long L$set$31
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB88-.
	.set L$set$32,LFE88-LFB88
	.quad L$set$32
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$33,LEFDE33-LASFDE33
	.long L$set$33
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB17-.
	.set L$set$34,LFE17-LFB17
	.quad L$set$34
	.uleb128 0
	.byte	0x4
	.set L$set$35,LCFI0-LFB17
	.long L$set$35
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$36,LCFI1-LCFI0
	.long L$set$36
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$37,LEFDE35-LASFDE35
	.long L$set$37
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB90-.
	.set L$set$38,LFE90-LFB90
	.quad L$set$38
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$39,LEFDE37-LASFDE37
	.long L$set$39
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB19-.
	.set L$set$40,LFE19-LFB19
	.quad L$set$40
	.uleb128 0
	.byte	0x4
	.set L$set$41,LCFI2-LFB19
	.long L$set$41
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$42,LCFI3-LCFI2
	.long L$set$42
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$43,LEFDE39-LASFDE39
	.long L$set$43
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB20-.
	.set L$set$44,LFE20-LFB20
	.quad L$set$44
	.uleb128 0
	.byte	0x4
	.set L$set$45,LCFI4-LFB20
	.long L$set$45
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$46,LCFI5-LCFI4
	.long L$set$46
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$47,LEFDE41-LASFDE41
	.long L$set$47
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB21-.
	.set L$set$48,LFE21-LFB21
	.quad L$set$48
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$49,LEFDE43-LASFDE43
	.long L$set$49
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB22-.
	.set L$set$50,LFE22-LFB22
	.quad L$set$50
	.uleb128 0
	.byte	0x4
	.set L$set$51,LCFI6-LFB22
	.long L$set$51
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$52,LCFI7-LCFI6
	.long L$set$52
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$53,LCFI8-LCFI7
	.long L$set$53
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$54,LCFI9-LCFI8
	.long L$set$54
	.byte	0xb
	.align	3
LEFDE43:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
