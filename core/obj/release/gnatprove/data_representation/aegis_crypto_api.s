	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC7:
	.ascii "failed precondition from aegis_crypto_api.adb:25"
	.text
	.align	2
	.p2align 5,,15
_aegis_crypto_api__to_input_slice:
LFB10:
	stp	x29, x30, [sp, -32]!
LCFI0:
	mov	x29, sp
LCFI1:
	mov	w2, 32767
	stp	x19, x20, [sp, 16]
LCFI2:
	sub	w20, w1, #1
	cmp	w20, w2
	bhi	L7
	mov	x19, x0
	add	w0, w1, 11
	sxtw	x0, w0
	mov	x1, 4
	and	x0, x0, -4
	bl	_system__secondary_stack__ss_allocate
	mov	x1, x0
	mov	x2, 0
	uxtw	x5, w20
	add	x0, x0, 8
	stp	wzr, w20, [x1]
	.p2align 5,,15
L3:
	ldrb	w4, [x19, x2]
	cmp	x2, x5
	strb	w4, [x0, x2]
	add	x2, x2, 1
	bne	L3
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI3:
	ret
L7:
LCFI4:
	adrp	x0, lC7@PAGE
	adrp	x1, lC3@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC3@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE10:
	.const
	.align	2
lC3:
	.word	1
	.word	48
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__Thash_input_bufferBIP
_aegis_crypto_api__Thash_input_bufferBIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__Tshake_output_bufferBIP
_aegis_crypto_api__Tshake_output_bufferBIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__Tmldsa87_signatureBIP
_aegis_crypto_api__Tmldsa87_signatureBIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__Tmldsa87_public_keyBIP
_aegis_crypto_api__Tmldsa87_public_keyBIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__Tmlkem1024_ciphertextBIP
_aegis_crypto_api__Tmlkem1024_ciphertextBIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__Tmlkem1024_shared_secretBIP
_aegis_crypto_api__Tmlkem1024_shared_secretBIP:
LFB7:
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__Tmlkem1024_decaps_keyBIP
_aegis_crypto_api__Tmlkem1024_decaps_keyBIP:
LFB8:
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__crypto_resultH
_aegis_crypto_api__crypto_resultH:
LFB9:
	ldp	w3, w1, [x1]
	mov	w10, -1
	adrp	x9, _crypto_resultP.11@PAGE
	add	w5, w10, 1
	add	x9, x9, _crypto_resultP.11@PAGEOFF;
	adrp	x12, _crypto_resultT1.10@PAGE
	adrp	x11, _crypto_resultT2.9@PAGE
	mov	w2, 0
	mov	w6, 0
	add	x12, x12, _crypto_resultT1.10@PAGEOFF;
	add	x11, x11, _crypto_resultT2.9@PAGEOFF;
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
	blt	L17
L21:
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
	beq	L17
	add	w5, w10, 1
	mov	w14, 1
	ldr	w3, [x9, w5, sxtw 2]
	mov	w10, 0
	add	x1, x8, w3, sxtw
	sub	x1, x1, x15
	cmp	w4, w3
	bge	L21
L17:
	adrp	x3, _crypto_resultG.8@PAGE
	mov	w1, 52429
	add	x3, x3, _crypto_resultG.8@PAGEOFF;
	movk	w1, 0xcccc, lsl 16
	ldrb	w0, [x3, w6, sxtw]
	ldrb	w2, [x3, w2, sxtw]
	add	w0, w0, w2
	umull	x1, w0, w1
	lsr	x1, x1, 34
	add	w1, w1, w1, lsl 2
	sub	w0, w0, w1
	ret
LFE9:
	.const
	.align	3
lC8:
	.ascii "aegis_crypto_api.ads"
	.space 1
	.align	3
lC9:
	.ascii "failed precondition from aegis_crypto_api.ads:114"
	.align	3
lC10:
	.ascii "aegis_crypto_api.adb"
	.space 1
	.align	3
lC11:
	.ascii "aegis_crypto_api.adb:75"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__sha3_256_hash
_aegis_crypto_api__sha3_256_hash:
LFB12:
	stp	x29, x30, [sp, -128]!
LCFI5:
	mov	x29, sp
LCFI6:
LEHB0:
	add	x4, x29, 128
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI7:
	str	x4, [x29, 120]
	tbnz	w2, #31, L37
	mov	w19, w2
	cmp	w2, 32768
	bgt	L38
	mov	x21, x0
	mov	w0, w2
	mov	x22, x1
	mov	x20, x3
	bl	_aegis_gas__gas_hash
	mov	x1, x0
	tbnz	x0, #63, L39
	mov	x0, x21
	bl	_aegis_execution__use_gas
LEHE0:
	cmp	w0, 1
	bhi	L40
	cbz	w0, L41
	cbz	w19, L42
	add	x21, x29, 96
	mov	x8, x21
LEHB1:
	bl	_system__secondary_stack__ss_mark
	mov	w1, w19
	mov	x0, x22
	bl	_aegis_crypto_api__to_input_slice
	ldp	w3, w2, [x1]
	cmp	w3, 0
	ccmp	w3, w2, 0, lt
	ble	L43
	sub	w19, w19, #1
	cmp	w19, w2
	bne	L44
	add	x19, x29, 64
	mov	x2, x19
	bl	_anubis_sha3__sha3_256
LEHE1:
	mov	x0, x21
LEHB2:
	bl	_system__secondary_stack__ss_release
L30:
	ldp	q31, q30, [x19]
	mov	w0, 0
	stp	q31, q30, [x20]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 128
LCFI8:
	ret
	.p2align 2,,3
L42:
LCFI9:
	add	x19, x29, 64
	adrp	x1, lC1@PAGE
	add	x1, x1, lC1@PAGEOFF;
	mov	x2, x19
	add	x0, x29, 56
	bl	_anubis_sha3__sha3_256
	b	L30
	.p2align 2,,3
L41:
	mov	w0, 4
	stp	xzr, xzr, [x20]
	stp	xzr, xzr, [x20, 16]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
LEHE2:
	ldp	x29, x30, [sp], 128
LCFI10:
	ret
L43:
LCFI11:
	adrp	x0, lC10@PAGE
	mov	w1, 72
	add	x0, x0, lC10@PAGEOFF;
LEHB3:
	bl	___gnat_rcheck_CE_Range_Check
L44:
	adrp	x0, lC11@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LEHE3:
L40:
	adrp	x0, lC10@PAGE
	mov	w1, 57
	add	x0, x0, lC10@PAGEOFF;
LEHB4:
	bl	___gnat_rcheck_CE_Invalid_Data
L39:
	adrp	x0, lC10@PAGE
	mov	w1, 52
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L38:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L37:
	adrp	x0, lC8@PAGE
	mov	w1, 114
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L34:
	mov	x19, x0
	mov	x0, x21
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE4:
LFE12:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
LLSDA12:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE12-LLSDACSB12
LLSDACSB12:
	.uleb128 LEHB0-LFB12
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB12
	.uleb128 LEHE1-LEHB1
	.uleb128 L34-LFB12
	.uleb128 0
	.uleb128 LEHB2-LFB12
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB3-LFB12
	.uleb128 LEHE3-LEHB3
	.uleb128 L34-LFB12
	.uleb128 0
	.uleb128 LEHB4-LFB12
	.uleb128 LEHE4-LEHB4
	.uleb128 0
	.uleb128 0
LLSDACSE12:
	.text
	.const
	.align	2
lC1:
	.word	0
	.word	-1
	.align	2
lC2:
	.word	1
	.word	23
	.align	2
lC0:
	.word	1
	.word	49
	.text
	.const
	.align	3
lC12:
	.ascii "failed precondition from aegis_crypto_api.ads:125"
	.align	3
lC13:
	.ascii "aegis_crypto_api.adb:118"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__sha3_512_hash
_aegis_crypto_api__sha3_512_hash:
LFB14:
	stp	x29, x30, [sp, -160]!
LCFI12:
	mov	x29, sp
LCFI13:
LEHB5:
	add	x4, x29, 160
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI14:
	str	x4, [x29, 88]
	tbnz	w2, #31, L60
	mov	w19, w2
	cmp	w2, 32768
	bgt	L61
	mov	x21, x0
	mov	w0, w2
	mov	x22, x1
	mov	x20, x3
	bl	_aegis_gas__gas_hash
	mov	x1, x0
	tbnz	x0, #63, L62
	mov	x0, x21
	bl	_aegis_execution__use_gas
LEHE5:
	cmp	w0, 1
	bhi	L63
	cbz	w0, L64
	cbz	w19, L65
	add	x21, x29, 64
	mov	x8, x21
LEHB6:
	bl	_system__secondary_stack__ss_mark
	mov	w1, w19
	mov	x0, x22
	bl	_aegis_crypto_api__to_input_slice
	ldp	w3, w2, [x1]
	cmp	w3, 0
	ccmp	w3, w2, 0, lt
	ble	L66
	sub	w19, w19, #1
	cmp	w19, w2
	bne	L67
	add	x19, x29, 96
	mov	x2, x19
	bl	_anubis_sha3__sha3_512
LEHE6:
	mov	x0, x21
LEHB7:
	bl	_system__secondary_stack__ss_release
L53:
	ldp	q29, q28, [x19]
	mov	w0, 0
	ldp	q31, q30, [x19, 32]
	stp	q29, q28, [x20]
	stp	q31, q30, [x20, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 160
LCFI15:
	ret
L65:
LCFI16:
	add	x19, x29, 96
	adrp	x1, lC1@PAGE
	add	x1, x1, lC1@PAGEOFF;
	mov	x2, x19
	add	x0, x29, 56
	bl	_anubis_sha3__sha3_512
	b	L53
L64:
	movi	v31.4s, 0
	mov	w0, 4
	stp	q31, q31, [x20]
	stp	q31, q31, [x20, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
LEHE7:
	ldp	x29, x30, [sp], 160
LCFI17:
	ret
L66:
LCFI18:
	adrp	x0, lC10@PAGE
	mov	w1, 116
	add	x0, x0, lC10@PAGEOFF;
LEHB8:
	bl	___gnat_rcheck_CE_Range_Check
L67:
	adrp	x0, lC13@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LEHE8:
L63:
	adrp	x0, lC10@PAGE
	mov	w1, 101
	add	x0, x0, lC10@PAGEOFF;
LEHB9:
	bl	___gnat_rcheck_CE_Invalid_Data
L62:
	adrp	x0, lC10@PAGE
	mov	w1, 97
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L61:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L60:
	adrp	x0, lC8@PAGE
	mov	w1, 125
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L57:
	mov	x19, x0
	mov	x0, x21
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE9:
LFE14:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table1:
LLSDA14:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE14-LLSDACSB14
LLSDACSB14:
	.uleb128 LEHB5-LFB14
	.uleb128 LEHE5-LEHB5
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB6-LFB14
	.uleb128 LEHE6-LEHB6
	.uleb128 L57-LFB14
	.uleb128 0
	.uleb128 LEHB7-LFB14
	.uleb128 LEHE7-LEHB7
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB8-LFB14
	.uleb128 LEHE8-LEHB8
	.uleb128 L57-LFB14
	.uleb128 0
	.uleb128 LEHB9-LFB14
	.uleb128 LEHE9-LEHB9
	.uleb128 0
	.uleb128 0
LLSDACSE14:
	.text
	.const
	.align	2
lC4:
	.word	1
	.word	24
	.text
	.const
	.align	3
lC14:
	.ascii "failed precondition from aegis_crypto_api.ads:136"
	.align	3
lC15:
	.ascii "aegis_crypto_api.adb:162"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__keccak256_hash
_aegis_crypto_api__keccak256_hash:
LFB16:
	stp	x29, x30, [sp, -128]!
LCFI19:
	mov	x29, sp
LCFI20:
LEHB10:
	add	x4, x29, 128
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI21:
	str	x4, [x29, 120]
	tbnz	w2, #31, L83
	mov	w19, w2
	cmp	w2, 32768
	bgt	L84
	mov	x21, x0
	mov	w0, w2
	mov	x22, x1
	mov	x20, x3
	bl	_aegis_gas__gas_hash
	mov	x1, x0
	tbnz	x0, #63, L85
	mov	x0, x21
	bl	_aegis_execution__use_gas
LEHE10:
	cmp	w0, 1
	bhi	L86
	cbz	w0, L87
	cbz	w19, L88
	add	x21, x29, 96
	mov	x8, x21
LEHB11:
	bl	_system__secondary_stack__ss_mark
	mov	w1, w19
	mov	x0, x22
	bl	_aegis_crypto_api__to_input_slice
	ldp	w3, w2, [x1]
	cmp	w3, 0
	ccmp	w3, w2, 0, lt
	ble	L89
	sub	w19, w19, #1
	cmp	w19, w2
	bne	L90
	add	x19, x29, 64
	mov	x2, x19
	bl	_anubis_sha3__keccak_256
LEHE11:
	mov	x0, x21
LEHB12:
	bl	_system__secondary_stack__ss_release
L76:
	ldp	q31, q30, [x19]
	mov	w0, 0
	stp	q31, q30, [x20]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 128
LCFI22:
	ret
	.p2align 2,,3
L88:
LCFI23:
	add	x19, x29, 64
	adrp	x1, lC1@PAGE
	add	x1, x1, lC1@PAGEOFF;
	mov	x2, x19
	add	x0, x29, 56
	bl	_anubis_sha3__keccak_256
	b	L76
	.p2align 2,,3
L87:
	mov	w0, 4
	stp	xzr, xzr, [x20]
	stp	xzr, xzr, [x20, 16]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
LEHE12:
	ldp	x29, x30, [sp], 128
LCFI24:
	ret
L89:
LCFI25:
	adrp	x0, lC10@PAGE
	mov	w1, 160
	add	x0, x0, lC10@PAGEOFF;
LEHB13:
	bl	___gnat_rcheck_CE_Range_Check
L90:
	adrp	x0, lC15@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LEHE13:
L86:
	adrp	x0, lC10@PAGE
	mov	w1, 144
	add	x0, x0, lC10@PAGEOFF;
LEHB14:
	bl	___gnat_rcheck_CE_Invalid_Data
L85:
	adrp	x0, lC10@PAGE
	mov	w1, 140
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L84:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L83:
	adrp	x0, lC8@PAGE
	mov	w1, 136
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L80:
	mov	x19, x0
	mov	x0, x21
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE14:
LFE16:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table2:
LLSDA16:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE16-LLSDACSB16
LLSDACSB16:
	.uleb128 LEHB10-LFB16
	.uleb128 LEHE10-LEHB10
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB11-LFB16
	.uleb128 LEHE11-LEHB11
	.uleb128 L80-LFB16
	.uleb128 0
	.uleb128 LEHB12-LFB16
	.uleb128 LEHE12-LEHB12
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB13-LFB16
	.uleb128 LEHE13-LEHB13
	.uleb128 L80-LFB16
	.uleb128 0
	.uleb128 LEHB14-LFB16
	.uleb128 LEHE14-LEHB14
	.uleb128 0
	.uleb128 0
LLSDACSE16:
	.text
	.const
	.align	3
lC16:
	.ascii "failed precondition from aegis_crypto_api.ads:153"
	.align	3
lC17:
	.ascii "aegis_crypto_api.adb:202"
	.align	3
lC18:
	.ascii "aegis_crypto_api.adb:213"
	.align	3
lC19:
	.ascii "aegis_crypto_api.adb:249"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__shake_xof
_aegis_crypto_api__shake_xof:
LFB18:
	stp	x29, x30, [sp, -160]!
LCFI26:
	mov	x29, sp
LCFI27:
LEHB15:
	add	x6, x29, 160
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI28:
	str	x6, [x29, 152]
	tbnz	w3, #31, L120
	mov	w25, w5
	tbnz	w5, #31, L121
	mov	x22, x0
	cmp	w3, 32768
	mov	w0, 1024
	mov	w20, w3
	ccmp	w5, w0, 0, le
	bgt	L122
	add	w19, w5, 31
	mov	w23, w1
	asr	w19, w19, 5
	mov	w1, 6
	mov	w0, w3
	mov	x24, x2
	smull	x19, w19, w1
	mov	x21, x4
	bl	_aegis_gas__gas_hash
	tbnz	x0, #63, L123
	mov	x2, 34917
	movk	x2, 0x5d63, lsl 16
	movk	x2, 0x46dc, lsl 32
	movk	x2, 0x3, lsl 48
	cmp	x0, x2
	bgt	L124
	sub	x1, x2, x19
	cmp	x1, x0
	bge	L125
	mov	x1, x2
L97:
	mov	x0, x22
	bl	_aegis_execution__use_gas
	cmp	w0, 1
	bhi	L126
	cbz	w0, L127
	mov	x2, 1024
	mov	w1, 0
	mov	x0, x21
	bl	_memset
	cbz	w25, L101
	sxtw	x19, w25
	mov	x22, sp
	add	x0, x19, 15
	sub	w28, w25, #1
	and	x0, x0, -16
	sub	sp, sp, x0
	mov	x26, sp
	cbnz	w20, L102
	cmp	w23, 1
	bhi	L128
	adrp	x1, lC1@PAGE
	mov	w4, w25
	cbz	w23, L129
	add	x1, x1, lC1@PAGEOFF;
	mov	x2, sp
	stp	wzr, w28, [x29, 104]
	add	x3, x29, 104
	add	x0, x29, 120
	bl	_anubis_sha3__shake256
L110:
	mov	x2, x19
	mov	x1, x26
	mov	x0, x21
	bl	_memcpy
	mov	sp, x22
L101:
	mov	w0, 0
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 160
LCFI29:
	ret
	.p2align 2,,3
L125:
LCFI30:
	add	x1, x19, x0
	cmp	x1, x2
	ble	L97
	adrp	x0, lC18@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L127:
	mov	x0, x21
	mov	x2, 1024
	mov	w1, 0
	bl	_memset
	mov	w0, 4
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 160
LCFI31:
	ret
	.p2align 2,,3
L129:
LCFI32:
	add	x1, x1, lC1@PAGEOFF;
	mov	x2, sp
	stp	wzr, w28, [x29, 96]
	add	x3, x29, 96
	add	x0, x29, 120
	bl	_anubis_sha3__shake128
LEHE15:
	b	L110
	.p2align 2,,3
L102:
	add	x27, x29, 128
	mov	x8, x27
LEHB16:
	bl	_system__secondary_stack__ss_mark
	mov	w1, w20
	mov	x0, x24
	bl	_aegis_crypto_api__to_input_slice
	ldp	w3, w2, [x1]
	cmp	w3, 0
	ccmp	w3, w2, 0, lt
	ble	L130
	sub	w20, w20, #1
	cmp	w20, w2
	bne	L131
	cmp	w23, 1
	bhi	L132
	mov	w4, w25
	mov	x2, sp
	cbnz	w23, L109
	add	x3, x29, 112
	stp	wzr, w28, [x29, 112]
	bl	_anubis_sha3__shake128
LEHE16:
L111:
	mov	x0, x27
LEHB17:
	bl	_system__secondary_stack__ss_release
LEHE17:
	b	L110
	.p2align 2,,3
L109:
	add	x3, x29, 120
	stp	wzr, w28, [x29, 120]
LEHB18:
	bl	_anubis_sha3__shake256
LEHE18:
	b	L111
L123:
	adrp	x0, lC10@PAGE
	mov	w1, 194
	add	x0, x0, lC10@PAGEOFF;
LEHB19:
	bl	___gnat_rcheck_CE_Invalid_Data
L122:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L124:
	adrp	x0, lC17@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L121:
	adrp	x0, lC8@PAGE
	mov	w1, 154
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L120:
	adrp	x0, lC8@PAGE
	mov	w1, 153
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L126:
	adrp	x0, lC10@PAGE
	mov	w1, 215
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L128:
	adrp	x0, lC10@PAGE
	mov	w1, 237
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE19:
L132:
	adrp	x0, lC10@PAGE
	mov	w1, 256
	add	x0, x0, lC10@PAGEOFF;
LEHB20:
	bl	___gnat_rcheck_CE_Invalid_Data
L131:
	adrp	x0, lC19@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L130:
	adrp	x0, lC10@PAGE
	mov	w1, 246
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LEHE20:
L114:
	mov	x19, x0
	mov	x0, x27
LEHB21:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE21:
LFE18:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table3:
LLSDA18:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE18-LLSDACSB18
LLSDACSB18:
	.uleb128 LEHB15-LFB18
	.uleb128 LEHE15-LEHB15
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB16-LFB18
	.uleb128 LEHE16-LEHB16
	.uleb128 L114-LFB18
	.uleb128 0
	.uleb128 LEHB17-LFB18
	.uleb128 LEHE17-LEHB17
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB18-LFB18
	.uleb128 LEHE18-LEHB18
	.uleb128 L114-LFB18
	.uleb128 0
	.uleb128 LEHB19-LFB18
	.uleb128 LEHE19-LEHB19
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB20-LFB18
	.uleb128 LEHE20-LEHB20
	.uleb128 L114-LFB18
	.uleb128 0
	.uleb128 LEHB21-LFB18
	.uleb128 LEHE21-LEHB21
	.uleb128 0
	.uleb128 0
LLSDACSE18:
	.text
	.const
	.align	3
lC20:
	.ascii "failed precondition from aegis_crypto_api.ads:172"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__mldsa87_verify
_aegis_crypto_api__mldsa87_verify:
LFB20:
	mov	x12, 7328
	mov	x5, 7328
	sub	sp, sp, x12
LCFI33:
	stp	x29, x30, [sp]
LCFI34:
	mov	x29, sp
LCFI35:
	add	x5, x29, x5
LEHB22:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI36:
	str	x5, [x29, 96]
	tbnz	w2, #31, L148
	mov	w20, w2
	cmp	w2, 32768
	bgt	L149
	mov	x23, x1
	mov	x1, 30000
	mov	x21, x3
	mov	x22, x4
	bl	_aegis_execution__use_gas
	mov	w19, w0
	cmp	w0, 1
	bhi	L150
	cbnz	w0, L151
	mov	w1, 4
L137:
	mov	x0, 0
	bfi	x0, x19, 0, 8
	bfi	x0, x1, 8, 8
	mov	x12, 7328
	ldr	x23, [sp, 48]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, x12
LCFI37:
	ret
L151:
LCFI38:
	add	x19, x29, 104
	mov	x1, x22
	mov	x2, 2592
	mov	x0, x19
	bl	_memcpy
	add	x22, x29, 2696
	mov	x1, x21
	mov	x0, x22
	mov	x2, 4627
	bl	_memcpy
	cbnz	w20, L138
	adrp	x2, lC1@PAGE
	mov	x0, x19
	mov	x3, x22
	add	x2, x2, lC1@PAGEOFF;
	add	x1, x29, 64
	bl	_anubis_mldsa__verify
LEHE22:
	mov	w19, w0
	cmp	w0, 1
	bhi	L152
	mov	w1, 0
	b	L137
L138:
	add	x21, x29, 72
	mov	x8, x21
LEHB23:
	bl	_system__secondary_stack__ss_mark
	mov	w1, w20
	mov	x0, x23
	bl	_aegis_crypto_api__to_input_slice
	mov	x2, x1
	mov	x1, x0
	ldp	w0, w3, [x2]
	cmp	w0, 0
	ccmp	w0, w3, 0, lt
	ble	L153
	mov	x3, x22
	mov	x0, x19
	bl	_anubis_mldsa__verify
LEHE23:
	mov	w19, w0
	cmp	w0, 1
	bhi	L154
	mov	x0, x21
LEHB24:
	bl	_system__secondary_stack__ss_release
	mov	w1, 0
	b	L137
L152:
	adrp	x0, lC10@PAGE
	mov	w1, 322
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE24:
L153:
	adrp	x0, lC10@PAGE
	mov	w1, 326
	add	x0, x0, lC10@PAGEOFF;
LEHB25:
	bl	___gnat_rcheck_CE_Range_Check
L154:
	adrp	x0, lC10@PAGE
	mov	w1, 328
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE25:
L150:
	adrp	x0, lC10@PAGE
	mov	w1, 291
	add	x0, x0, lC10@PAGEOFF;
LEHB26:
	bl	___gnat_rcheck_CE_Invalid_Data
L149:
	adrp	x0, lC20@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L148:
	adrp	x0, lC8@PAGE
	mov	w1, 172
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L145:
	mov	x19, x0
	mov	x0, x21
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE26:
LFE20:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table4:
LLSDA20:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE20-LLSDACSB20
LLSDACSB20:
	.uleb128 LEHB22-LFB20
	.uleb128 LEHE22-LEHB22
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB23-LFB20
	.uleb128 LEHE23-LEHB23
	.uleb128 L145-LFB20
	.uleb128 0
	.uleb128 LEHB24-LFB20
	.uleb128 LEHE24-LEHB24
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB25-LFB20
	.uleb128 LEHE25-LEHB25
	.uleb128 L145-LFB20
	.uleb128 0
	.uleb128 LEHB26-LFB20
	.uleb128 LEHE26-LEHB26
	.uleb128 0
	.uleb128 0
LLSDACSE20:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__mlkem1024_decaps
_aegis_crypto_api__mlkem1024_decaps:
LFB22:
	mov	x12, 4816
	sub	sp, sp, x12
LCFI39:
	stp	x29, x30, [sp]
LCFI40:
	mov	x29, sp
LCFI41:
	stp	x19, x20, [sp, 16]
LCFI42:
	mov	x20, x1
	mov	x1, 25000
	mov	x19, x3
	str	x21, [sp, 32]
LCFI43:
	mov	x21, x2
	bl	_aegis_execution__use_gas
	cmp	w0, 1
	bhi	L160
	cbz	w0, L161
	add	x0, x29, 1648
	mov	x1, x21
	mov	x2, 3168
	mov	x21, x0
	bl	_memcpy
	add	x3, x29, 80
	mov	x1, x20
	mov	x0, x3
	mov	x2, 1568
	bl	_memcpy
	mov	x1, x0
	add	x2, x29, 48
	mov	x0, x21
	bl	_anubis_mlkem__decaps
	ldp	q31, q30, [x29, 48]
	mov	w0, 0
	stp	q31, q30, [x19]
	mov	x12, 4816
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, x12
LCFI44:
	ret
L161:
LCFI45:
	mov	w0, 4
	stp	xzr, xzr, [x19]
	stp	xzr, xzr, [x19, 16]
	mov	x12, 4816
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, x12
LCFI46:
	ret
L160:
LCFI47:
	adrp	x0, lC10@PAGE
	mov	w1, 351
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__derive_address
_aegis_crypto_api__derive_address:
LFB23:
	sub	sp, sp, #2720
LCFI48:
	stp	x29, x30, [sp]
LCFI49:
	mov	x29, sp
LCFI50:
	stp	x21, x22, [sp, 32]
LCFI51:
	mov	x21, x0
	mov	w0, 2592
	mov	w22, w2
	stp	x19, x20, [sp, 16]
LCFI52:
	mov	x20, x3
	stp	x23, x24, [sp, 48]
LCFI53:
	mov	x23, x1
	bl	_aegis_gas__gas_hash
	tbnz	x0, #63, L168
	add	x24, x29, 104
	mov	x19, x0
	mov	w1, 0
	mov	x2, 2610
	mov	x0, x24
	bl	_memset
	mov	x1, x19
	mov	x0, x21
	bl	_aegis_execution__use_gas
	cmp	w0, 1
	bhi	L169
	cbz	w0, L170
	adrp	x3, _domain_prefix.7@PAGE
	mov	x1, x23
	strb	w22, [x29, 121]
	add	x3, x3, _domain_prefix.7@PAGEOFF;
	mov	x2, 2592
	ldp	x4, x5, [x3]
	add	x0, x29, 122
	add	x19, x29, 72
	ldrb	w3, [x3, 16]
	stp	x4, x5, [x29, 104]
	strb	w3, [x24, 16]
	bl	_memcpy
	adrp	x1, lC5@PAGE
	mov	x0, x24
	mov	x2, x19
	add	x1, x1, lC5@PAGEOFF;
	bl	_anubis_sha3__sha3_256
	ldp	q31, q30, [x19]
	mov	w0, 0
	stp	q31, q30, [x20]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 2720
LCFI54:
	ret
L170:
LCFI55:
	mov	w0, 4
	stp	xzr, xzr, [x20]
	stp	xzr, xzr, [x20, 16]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 2720
LCFI56:
	ret
L169:
LCFI57:
	adrp	x0, lC10@PAGE
	mov	w1, 412
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L168:
	adrp	x0, lC10@PAGE
	mov	w1, 397
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.const
	.align	2
lC5:
	.word	0
	.word	2609
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__constant_time_equal
_aegis_crypto_api__constant_time_equal:
LFB24:
	ldp	q30, q29, [x0]
	movi	v31.4s, 0
	ldp	q27, q28, [x1]
	eor	v29.16b, v29.16b, v28.16b
	eor	v30.16b, v30.16b, v27.16b
	orr	v30.16b, v30.16b, v29.16b
	ext	v29.16b, v30.16b, v31.16b, #8
	orr	v30.16b, v29.16b, v30.16b
	ext	v29.16b, v30.16b, v31.16b, #4
	orr	v29.16b, v29.16b, v30.16b
	ext	v30.16b, v29.16b, v31.16b, #2
	orr	v30.16b, v30.16b, v29.16b
	ext	v31.16b, v30.16b, v31.16b, #1
	orr	v31.16b, v31.16b, v30.16b
	umov	w0, v31.b[0]
	tst	w0, 255
	cset	w0, eq
	ret
LFE24:
	.const
	.align	3
lC21:
	.ascii "Loop_Invariant failed at aegis_crypto_api.adb:459"
	.align	3
lC22:
	.ascii "failed postcondition from aegis_crypto_api.ads:219"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_crypto_api__zeroize_secret
_aegis_crypto_api__zeroize_secret:
LFB25:
	mov	x3, x0
	add	x2, x0, 1
	stp	x29, x30, [sp, -16]!
LCFI58:
	add	x4, x0, 33
	mov	x29, sp
LCFI59:
	.p2align 5,,15
L176:
	mov	x0, x3
	strb	wzr, [x2, -1]
	b	L174
	.p2align 2,,3
L187:
	add	x0, x0, 1
	cmp	x0, x2
	beq	L186
L174:
	ldrb	w1, [x0]
	cbz	w1, L187
	adrp	x0, lC21@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC21@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L186:
	add	x2, x0, 1
	cmp	x2, x4
	bne	L176
	add	x1, x3, 32
	b	L178
	.p2align 2,,3
L189:
	add	x3, x3, 1
	cmp	x1, x3
	beq	L188
L178:
	ldrb	w0, [x3]
	cbz	w0, L189
	adrp	x0, lC22@PAGE
	adrp	x1, lC6@PAGE
	add	x0, x0, lC22@PAGEOFF;
	add	x1, x1, lC6@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L188:
	ldp	x29, x30, [sp], 16
LCFI60:
	ret
LFE25:
	.const
	.align	2
lC6:
	.word	1
	.word	50
	.text
	.const
_domain_prefix.7:
	.byte	97
	.byte	101
	.byte	103
	.byte	105
	.byte	115
	.byte	45
	.byte	118
	.byte	49
	.byte	45
	.byte	109
	.byte	108
	.byte	100
	.byte	115
	.byte	97
	.byte	56
	.byte	55
	.byte	45
	.align	3
_crypto_resultG.8:
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.space 5
	.align	1
_crypto_resultT2.9:
	.byte	3
	.byte	9
	.align	1
_crypto_resultT1.10:
	.byte	2
	.byte	2
	.align	3
_crypto_resultP.11:
	.word	9
	.word	16
	.globl _aegis_crypto_api__crypto_resultN
	.align	3
_aegis_crypto_api__crypto_resultN:
	.byte	1
	.byte	10
	.byte	30
	.byte	54
	.byte	75
	.byte	92
	.space 2
	.globl _aegis_crypto_api__crypto_resultS
	.align	3
_aegis_crypto_api__crypto_resultS:
	.ascii "CRYPTO_OKCRYPTO_INVALID_INPUTCRYPTO_INVALID_SIGNATURECRYPTO_DECAPS_FAILURECRYPTO_OUT_OF_GAS"
	.globl _aegis_crypto_api__shake_variantN
	.align	2
_aegis_crypto_api__shake_variantN:
	.byte	1
	.byte	10
	.byte	19
	.space 1
	.globl _aegis_crypto_api__shake_variantS
	.align	3
_aegis_crypto_api__shake_variantS:
	.ascii "SHAKE_128SHAKE_256"
	.globl _aegis_crypto_api_E
	.data
	.align	1
_aegis_crypto_api_E:
	.space 2
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0
	.byte	0x3
	.ascii "zPLR\0"
	.uleb128 0x1
	.sleb128 -8
	.uleb128 0x1e
	.uleb128 0x7
	.byte	0x9b
L_got_pcr0:
	.long	___gnat_personality_v0@GOT-L_got_pcr0
	.byte	0x10
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
	.quad	LFB10-.
	.set L$set$2,LFE10-LFB10
	.quad L$set$2
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$3,LCFI0-LFB10
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
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$8,LEFDE3-LASFDE3
	.long L$set$8
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB2-.
	.set L$set$9,LFE2-LFB2
	.quad L$set$9
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$10,LEFDE5-LASFDE5
	.long L$set$10
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB3-.
	.set L$set$11,LFE3-LFB3
	.quad L$set$11
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$12,LEFDE7-LASFDE7
	.long L$set$12
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB4-.
	.set L$set$13,LFE4-LFB4
	.quad L$set$13
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$14,LEFDE9-LASFDE9
	.long L$set$14
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB5-.
	.set L$set$15,LFE5-LFB5
	.quad L$set$15
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$16,LEFDE11-LASFDE11
	.long L$set$16
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB6-.
	.set L$set$17,LFE6-LFB6
	.quad L$set$17
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$18,LEFDE13-LASFDE13
	.long L$set$18
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB7-.
	.set L$set$19,LFE7-LFB7
	.quad L$set$19
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$20,LEFDE15-LASFDE15
	.long L$set$20
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB8-.
	.set L$set$21,LFE8-LFB8
	.quad L$set$21
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$22,LEFDE17-LASFDE17
	.long L$set$22
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB9-.
	.set L$set$23,LFE9-LFB9
	.quad L$set$23
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$24,LEFDE19-LASFDE19
	.long L$set$24
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB12-.
	.set L$set$25,LFE12-LFB12
	.quad L$set$25
	.uleb128 0x8
	.quad	LLSDA12-.
	.byte	0x4
	.set L$set$26,LCFI5-LFB12
	.long L$set$26
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$27,LCFI6-LCFI5
	.long L$set$27
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$28,LCFI7-LCFI6
	.long L$set$28
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x4
	.set L$set$29,LCFI8-LCFI7
	.long L$set$29
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
	.set L$set$30,LCFI9-LCFI8
	.long L$set$30
	.byte	0xb
	.byte	0x4
	.set L$set$31,LCFI10-LCFI9
	.long L$set$31
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
	.set L$set$32,LCFI11-LCFI10
	.long L$set$32
	.byte	0xb
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$33,LEFDE21-LASFDE21
	.long L$set$33
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB14-.
	.set L$set$34,LFE14-LFB14
	.quad L$set$34
	.uleb128 0x8
	.quad	LLSDA14-.
	.byte	0x4
	.set L$set$35,LCFI12-LFB14
	.long L$set$35
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$36,LCFI13-LCFI12
	.long L$set$36
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$37,LCFI14-LCFI13
	.long L$set$37
	.byte	0x93
	.uleb128 0x12
	.byte	0x94
	.uleb128 0x11
	.byte	0x95
	.uleb128 0x10
	.byte	0x96
	.uleb128 0xf
	.byte	0x4
	.set L$set$38,LCFI15-LCFI14
	.long L$set$38
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
	.set L$set$39,LCFI16-LCFI15
	.long L$set$39
	.byte	0xb
	.byte	0x4
	.set L$set$40,LCFI17-LCFI16
	.long L$set$40
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
	.set L$set$41,LCFI18-LCFI17
	.long L$set$41
	.byte	0xb
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$42,LEFDE23-LASFDE23
	.long L$set$42
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB16-.
	.set L$set$43,LFE16-LFB16
	.quad L$set$43
	.uleb128 0x8
	.quad	LLSDA16-.
	.byte	0x4
	.set L$set$44,LCFI19-LFB16
	.long L$set$44
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$45,LCFI20-LCFI19
	.long L$set$45
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$46,LCFI21-LCFI20
	.long L$set$46
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x4
	.set L$set$47,LCFI22-LCFI21
	.long L$set$47
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
	.set L$set$48,LCFI23-LCFI22
	.long L$set$48
	.byte	0xb
	.byte	0x4
	.set L$set$49,LCFI24-LCFI23
	.long L$set$49
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
	.set L$set$50,LCFI25-LCFI24
	.long L$set$50
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$51,LEFDE25-LASFDE25
	.long L$set$51
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB18-.
	.set L$set$52,LFE18-LFB18
	.quad L$set$52
	.uleb128 0x8
	.quad	LLSDA18-.
	.byte	0x4
	.set L$set$53,LCFI26-LFB18
	.long L$set$53
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$54,LCFI27-LCFI26
	.long L$set$54
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$55,LCFI28-LCFI27
	.long L$set$55
	.byte	0x93
	.uleb128 0x12
	.byte	0x94
	.uleb128 0x11
	.byte	0x95
	.uleb128 0x10
	.byte	0x96
	.uleb128 0xf
	.byte	0x97
	.uleb128 0xe
	.byte	0x98
	.uleb128 0xd
	.byte	0x99
	.uleb128 0xc
	.byte	0x9a
	.uleb128 0xb
	.byte	0x9b
	.uleb128 0xa
	.byte	0x9c
	.uleb128 0x9
	.byte	0x4
	.set L$set$56,LCFI29-LCFI28
	.long L$set$56
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
	.set L$set$57,LCFI30-LCFI29
	.long L$set$57
	.byte	0xb
	.byte	0x4
	.set L$set$58,LCFI31-LCFI30
	.long L$set$58
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
	.set L$set$59,LCFI32-LCFI31
	.long L$set$59
	.byte	0xb
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$60,LEFDE27-LASFDE27
	.long L$set$60
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB20-.
	.set L$set$61,LFE20-LFB20
	.quad L$set$61
	.uleb128 0x8
	.quad	LLSDA20-.
	.byte	0x4
	.set L$set$62,LCFI33-LFB20
	.long L$set$62
	.byte	0xe
	.uleb128 0x1ca0
	.byte	0x4
	.set L$set$63,LCFI34-LCFI33
	.long L$set$63
	.byte	0x9d
	.uleb128 0x394
	.byte	0x9e
	.uleb128 0x393
	.byte	0x4
	.set L$set$64,LCFI35-LCFI34
	.long L$set$64
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$65,LCFI36-LCFI35
	.long L$set$65
	.byte	0x93
	.uleb128 0x392
	.byte	0x94
	.uleb128 0x391
	.byte	0x95
	.uleb128 0x390
	.byte	0x96
	.uleb128 0x38f
	.byte	0x97
	.uleb128 0x38e
	.byte	0x4
	.set L$set$66,LCFI37-LCFI36
	.long L$set$66
	.byte	0xa
	.byte	0xd7
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$67,LCFI38-LCFI37
	.long L$set$67
	.byte	0xb
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$68,LEFDE29-LASFDE29
	.long L$set$68
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB22-.
	.set L$set$69,LFE22-LFB22
	.quad L$set$69
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$70,LCFI39-LFB22
	.long L$set$70
	.byte	0xe
	.uleb128 0x12d0
	.byte	0x4
	.set L$set$71,LCFI40-LCFI39
	.long L$set$71
	.byte	0x9d
	.uleb128 0x25a
	.byte	0x9e
	.uleb128 0x259
	.byte	0x4
	.set L$set$72,LCFI41-LCFI40
	.long L$set$72
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$73,LCFI42-LCFI41
	.long L$set$73
	.byte	0x93
	.uleb128 0x258
	.byte	0x94
	.uleb128 0x257
	.byte	0x4
	.set L$set$74,LCFI43-LCFI42
	.long L$set$74
	.byte	0x95
	.uleb128 0x256
	.byte	0x4
	.set L$set$75,LCFI44-LCFI43
	.long L$set$75
	.byte	0xa
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI45-LCFI44
	.long L$set$76
	.byte	0xb
	.byte	0x4
	.set L$set$77,LCFI46-LCFI45
	.long L$set$77
	.byte	0xa
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$78,LCFI47-LCFI46
	.long L$set$78
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$79,LEFDE31-LASFDE31
	.long L$set$79
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB23-.
	.set L$set$80,LFE23-LFB23
	.quad L$set$80
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$81,LCFI48-LFB23
	.long L$set$81
	.byte	0xe
	.uleb128 0xaa0
	.byte	0x4
	.set L$set$82,LCFI49-LCFI48
	.long L$set$82
	.byte	0x9d
	.uleb128 0x154
	.byte	0x9e
	.uleb128 0x153
	.byte	0x4
	.set L$set$83,LCFI50-LCFI49
	.long L$set$83
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$84,LCFI51-LCFI50
	.long L$set$84
	.byte	0x95
	.uleb128 0x150
	.byte	0x96
	.uleb128 0x14f
	.byte	0x4
	.set L$set$85,LCFI52-LCFI51
	.long L$set$85
	.byte	0x93
	.uleb128 0x152
	.byte	0x94
	.uleb128 0x151
	.byte	0x4
	.set L$set$86,LCFI53-LCFI52
	.long L$set$86
	.byte	0x97
	.uleb128 0x14e
	.byte	0x98
	.uleb128 0x14d
	.byte	0x4
	.set L$set$87,LCFI54-LCFI53
	.long L$set$87
	.byte	0xa
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$88,LCFI55-LCFI54
	.long L$set$88
	.byte	0xb
	.byte	0x4
	.set L$set$89,LCFI56-LCFI55
	.long L$set$89
	.byte	0xa
	.byte	0xd7
	.byte	0xd8
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$90,LCFI57-LCFI56
	.long L$set$90
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$91,LEFDE33-LASFDE33
	.long L$set$91
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB24-.
	.set L$set$92,LFE24-LFB24
	.quad L$set$92
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$93,LEFDE35-LASFDE35
	.long L$set$93
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB25-.
	.set L$set$94,LFE25-LFB25
	.quad L$set$94
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$95,LCFI58-LFB25
	.long L$set$95
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$96,LCFI59-LCFI58
	.long L$set$96
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$97,LCFI60-LCFI59
	.long L$set$97
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE35:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
