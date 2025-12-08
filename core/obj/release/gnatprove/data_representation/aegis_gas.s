	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC4:
	.ascii "aegis_gas.ads"
	.space 1
	.align	3
lC5:
	.ascii "failed precondition from aegis_gas.ads:157"
	.align	3
lC6:
	.ascii "aegis_gas.adb"
	.space 1
	.align	3
lC7:
	.ascii "failed postcondition from aegis_gas.ads:158"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__apply_discount
_aegis_gas__apply_discount:
LFB2:
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	x29, sp
LCFI1:
	tbnz	x0, #63, L8
	mov	x2, x0
	mov	x0, 34917
	movk	x0, 0x5d63, lsl 16
	movk	x0, 0x46dc, lsl 32
	movk	x0, 0x3, lsl 48
	cmp	x2, x0
	bgt	L9
	mov	w0, -7000
	add	w0, w1, w0
	and	w0, w0, 65535
	cmp	w0, 3000
	bhi	L10
	sxtw	x1, w1
	mov	x0, 22859
	movk	x0, 0x3886, lsl 16
	mul	x1, x2, x1
	movk	x0, 0xc5d6, lsl 32
	movk	x0, 0x346d, lsl 48
	umulh	x1, x1, x0
	lsr	x0, x1, 11
	cmp	x2, x0
	blt	L11
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
L8:
LCFI3:
	adrp	x0, lC4@PAGE
	mov	w1, 157
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L11:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L10:
	adrp	x0, lC6@PAGE
	mov	w1, 20
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L9:
	adrp	x0, lC5@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC5@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE2:
	.const
	.align	2
lC1:
	.word	1
	.word	43
	.align	2
lC0:
	.word	1
	.word	42
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__has_gas
_aegis_gas__has_gas:
LFB4:
	stp	x29, x30, [sp, -16]!
LCFI4:
	mov	x29, sp
LCFI5:
	ldr	x2, [x0, 8]
	tbnz	x2, #63, L14
	ldr	x0, [x0]
	tbnz	x0, #63, L14
	cmp	x2, x0
	bgt	L17
	tbnz	x1, #63, L19
	sub	x0, x0, x2
	cmp	x1, x0
	cset	w0, le
	ldp	x29, x30, [sp], 16
LCFI6:
	ret
	.p2align 2,,3
L17:
LCFI7:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI8:
	ret
L14:
LCFI9:
	adrp	x0, lC6@PAGE
	mov	w1, 36
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L19:
	adrp	x0, lC6@PAGE
	mov	w1, 39
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__consume_gas
_aegis_gas__consume_gas:
LFB6:
	stp	x29, x30, [sp, -32]!
LCFI10:
	mov	x29, sp
LCFI11:
	stp	x19, x20, [sp, 16]
LCFI12:
	tbnz	x1, #63, L32
	mov	x19, x1
	mov	x20, x0
	bl	_aegis_gas__has_gas
	tbz	x0, 0, L22
	ldr	x1, [x20, 8]
	tbnz	x1, #63, L33
	adds	x19, x19, x1
	bvs	L25
	str	x19, [x20, 8]
	tbnz	x19, #63, L34
L22:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI13:
	ret
L34:
LCFI14:
	adrp	x0, lC4@PAGE
	mov	w1, 178
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L32:
	adrp	x0, lC6@PAGE
	mov	w1, 48
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L33:
	adrp	x0, lC6@PAGE
	mov	w1, 49
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L25:
	adrp	x0, lC6@PAGE
	mov	w1, 49
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE6:
	.const
	.align	3
lC8:
	.ascii "failed precondition from aegis_gas.ads:189"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__consume_gas_discounted
_aegis_gas__consume_gas_discounted:
LFB8:
	stp	x29, x30, [sp, -32]!
LCFI15:
	mov	x29, sp
LCFI16:
	str	x19, [sp, 16]
LCFI17:
	tbnz	x1, #63, L40
	mov	x19, x0
	mov	x0, 34917
	movk	x0, 0x5d63, lsl 16
	mov	x2, x1
	movk	x0, 0x46dc, lsl 32
	movk	x0, 0x3, lsl 48
	cmp	x1, x0
	bgt	L41
	ldrsh	w1, [x19, 48]
	mov	w0, -7000
	add	w0, w1, w0
	and	w0, w0, 65535
	cmp	w0, 3000
	bhi	L42
	mov	x0, x2
	bl	_aegis_gas__apply_discount
	mov	x1, x0
	mov	x0, x19
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI18:
	b	_aegis_gas__consume_gas
L40:
LCFI19:
	adrp	x0, lC4@PAGE
	mov	w1, 189
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L42:
	adrp	x0, lC6@PAGE
	mov	w1, 61
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L41:
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE8:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__remaining_gas
_aegis_gas__remaining_gas:
LFB9:
	ldr	x1, [x0, 8]
	tbnz	x1, #63, L45
	ldr	x0, [x0]
	tbnz	x0, #63, L45
	cmp	x1, x0
	sub	x0, x0, x1
	csel	x0, x0, xzr, le
	ret
L45:
	adrp	x0, lC6@PAGE
	stp	x29, x30, [sp, -16]!
LCFI20:
	mov	w1, 68
	mov	x29, sp
LCFI21:
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE9:
	.const
	.align	3
lC9:
	.ascii "failed precondition from aegis_gas.ads:205"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__refund_gas
_aegis_gas__refund_gas:
LFB11:
	stp	x29, x30, [sp, -16]!
LCFI22:
	mov	x29, sp
LCFI23:
	ldr	x2, [x0, 8]
	orr	x3, x1, x2
	tbnz	x3, #63, L54
	cmp	x1, x2
	bgt	L55
	sub	x2, x2, x1
	str	x2, [x0, 8]
	ldp	x29, x30, [sp], 16
LCFI24:
	ret
L54:
LCFI25:
	adrp	x0, lC4@PAGE
	mov	w1, 205
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L55:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE11:
	.const
	.align	3
lC10:
	.ascii "failed precondition from aegis_gas.ads:222"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__gas_hash
_aegis_gas__gas_hash:
LFB13:
	stp	x29, x30, [sp, -16]!
LCFI26:
	mov	x29, sp
LCFI27:
	tbnz	w0, #31, L60
	mov	w1, 2147483616
	cmp	w0, w1
	bgt	L61
	add	w1, w0, 31
	mov	w2, 6
	asr	w1, w1, 5
	mov	x0, 30
	smaddl	x0, w1, w2, x0
	ldp	x29, x30, [sp], 16
LCFI28:
	ret
L60:
LCFI29:
	adrp	x0, lC4@PAGE
	mov	w1, 222
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L61:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE13:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__gas_memory_expansion
_aegis_gas__gas_memory_expansion:
LFB15:
	stp	x29, x30, [sp, -16]!
LCFI30:
	mov	w2, w0
	mov	x29, sp
LCFI31:
	orr	w0, w1, w0
	tbnz	w0, #31, L67
	mov	x0, 0
	cmp	w1, w2
	bgt	L68
L62:
	ldp	x29, x30, [sp], 16
LCFI32:
	ret
	.p2align 2,,3
L68:
LCFI33:
	umull	x3, w1, w1
	mov	w4, 3
	umull	x0, w2, w2
	asr	x3, x3, 9
	asr	x0, x0, 9
	smaddl	x1, w1, w4, x3
	smaddl	x2, w2, w4, x0
	subs	x0, x1, x2
	bpl	L62
	adrp	x0, lC6@PAGE
	mov	w1, 129
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L67:
	adrp	x0, lC6@PAGE
	mov	w1, 113
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE15:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__gas_call_complex
_aegis_gas__gas_call_complex:
LFB17:
	stp	x29, x30, [sp, -16]!
LCFI34:
	mov	x29, sp
LCFI35:
	cmp	w0, 1
	bhi	L79
	cmp	w0, 0
	mov	x3, 700
	mov	x0, 9700
	csel	x0, x0, x3, ne
	cmp	w1, 1
	bhi	L80
	cmp	w1, 0
	mov	x1, 25000
	add	x1, x0, x1
	csel	x0, x1, x0, ne
	tbnz	x2, #63, L81
	sub	x2, x2, x2, asr 6
	cmp	x2, x0
	csel	x0, x2, x0, le
	ldp	x29, x30, [sp], 16
LCFI36:
	ret
L79:
LCFI37:
	adrp	x0, lC6@PAGE
	mov	w1, 140
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L81:
	adrp	x0, lC6@PAGE
	mov	w1, 150
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L80:
	adrp	x0, lC6@PAGE
	mov	w1, 144
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE17:
	.const
	.align	3
lC11:
	.ascii "failed precondition from aegis_gas.ads:251"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__gas_log
_aegis_gas__gas_log:
LFB18:
	stp	x29, x30, [sp, -16]!
LCFI38:
	mov	x29, sp
LCFI39:
	tbnz	w0, #31, L85
	cmp	w0, 4
	bgt	L86
	tbnz	w1, #31, L85
	mov	w2, 16960
	movk	w2, 0xf, lsl 16
	cmp	w1, w2
	bgt	L86
	sxtw	x0, w0
	sbfiz	x1, x1, 3, 32
	mov	w2, 375
	add	x0, x0, 1
	smaddl	x0, w0, w2, x1
	ldp	x29, x30, [sp], 16
LCFI40:
	ret
L85:
LCFI41:
	adrp	x0, lC4@PAGE
	mov	w1, 251
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L86:
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE18:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__gas_create_contract
_aegis_gas__gas_create_contract:
LFB20:
	tbnz	w0, #31, L93
	mov	w2, 200
	mov	x1, 32000
	smaddl	x0, w0, w2, x1
	ret
L93:
	adrp	x0, lC6@PAGE
	stp	x29, x30, [sp, -16]!
LCFI42:
	mov	w1, 190
	mov	x29, sp
LCFI43:
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__gas_stark_batch_verify
_aegis_gas__gas_stark_batch_verify:
LFB21:
	tbnz	w0, #31, L99
	mov	x1, 3392
	mov	w2, 5000
	movk	x1, 0x3, lsl 16
	smaddl	x0, w0, w2, x1
	ret
L99:
	adrp	x0, lC6@PAGE
	stp	x29, x30, [sp, -16]!
LCFI44:
	mov	w1, 197
	mov	x29, sp
LCFI45:
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE21:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__gas_privacy_operation
_aegis_gas__gas_privacy_operation:
LFB23:
	stp	x29, x30, [sp, -16]!
LCFI46:
	mov	x29, sp
LCFI47:
	tbnz	w0, #31, L106
	mov	w2, w0
	mov	x0, 25000
	cmp	w2, 3
	bls	L107
L102:
	tbnz	w1, #31, L108
	mov	w2, 10
	smaddl	x0, w1, w2, x0
	ldp	x29, x30, [sp], 16
LCFI48:
	ret
	.p2align 2,,3
L107:
LCFI49:
	adrp	x0, _CSWTCH.101@PAGE
	add	x0, x0, _CSWTCH.101@PAGEOFF;
	ldr	x0, [x0, w2, uxtw 3]
	b	L102
L106:
	adrp	x0, lC6@PAGE
	mov	w1, 207
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L108:
	adrp	x0, lC6@PAGE
	mov	w1, 220
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__cycles_to_gas
_aegis_gas__cycles_to_gas:
LFB24:
	tbnz	w0, #31, L114
	mov	w1, 34079
	movk	w1, 0x51eb, lsl 16
	smull	x1, w0, w1
	asr	x1, x1, 37
	sub	w0, w1, w0, asr 31
	sxtw	x0, w0
	ret
L114:
	adrp	x0, lC6@PAGE
	stp	x29, x30, [sp, -16]!
LCFI50:
	mov	w1, 229
	mov	x29, sp
LCFI51:
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.align	2
	.p2align 5,,15
	.globl _aegis_gas__validate_wcet_bound
_aegis_gas__validate_wcet_bound:
LFB26:
	orr	x2, x0, x1
	tbnz	x2, #63, L120
	cmp	x0, x1
	cset	w0, le
	ret
L120:
	adrp	x0, lC6@PAGE
	stp	x29, x30, [sp, -16]!
LCFI52:
	mov	w1, 237
	mov	x29, sp
LCFI53:
	add	x0, x0, lC6@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.const
	.align	3
_CSWTCH.101:
	.xword	5000
	.xword	25000
	.xword	50000
	.xword	100000
	.globl _aegis_gas__max_log_data_length
	.align	2
_aegis_gas__max_log_data_length:
	.word	1000000
	.globl _aegis_gas__max_hash_byte_length
	.align	2
_aegis_gas__max_hash_byte_length:
	.word	2147483616
	.globl _aegis_gas__max_safe_base_gas
	.align	3
_aegis_gas__max_safe_base_gas:
	.xword	922337203685477
	.globl _aegis_gas__gas_selfdestruct
	.align	3
_aegis_gas__gas_selfdestruct:
	.xword	5000
	.globl _aegis_gas__gas_extcodecopy_per_word
	.align	3
_aegis_gas__gas_extcodecopy_per_word:
	.xword	3
	.globl _aegis_gas__gas_extcodecopy_base
	.align	3
_aegis_gas__gas_extcodecopy_base:
	.xword	700
	.globl _aegis_gas__gas_extcodesize
	.align	3
_aegis_gas__gas_extcodesize:
	.xword	700
	.globl _aegis_gas__gas_call
	.align	3
_aegis_gas__gas_call:
	.xword	700
	.globl _aegis_gas__gas_selfbalance
	.align	3
_aegis_gas__gas_selfbalance:
	.xword	5
	.globl _aegis_gas__gas_balance
	.align	3
_aegis_gas__gas_balance:
	.xword	400
	.globl _aegis_gas__gas_log_per_byte
	.align	3
_aegis_gas__gas_log_per_byte:
	.xword	8
	.globl _aegis_gas__gas_log_topic
	.align	3
_aegis_gas__gas_log_topic:
	.xword	375
	.globl _aegis_gas__gas_log_base
	.align	3
_aegis_gas__gas_log_base:
	.xword	375
	.globl _aegis_gas__gas_create_per_byte
	.align	3
_aegis_gas__gas_create_per_byte:
	.xword	200
	.globl _aegis_gas__gas_create2
	.align	3
_aegis_gas__gas_create2:
	.xword	32000
	.globl _aegis_gas__gas_create
	.align	3
_aegis_gas__gas_create:
	.xword	32000
	.globl _aegis_gas__gas_delegatecall
	.align	3
_aegis_gas__gas_delegatecall:
	.xword	700
	.globl _aegis_gas__gas_staticcall
	.align	3
_aegis_gas__gas_staticcall:
	.xword	700
	.globl _aegis_gas__gas_call_new_account
	.align	3
_aegis_gas__gas_call_new_account:
	.xword	25000
	.globl _aegis_gas__gas_call_value
	.align	3
_aegis_gas__gas_call_value:
	.xword	9000
	.globl _aegis_gas__gas_call_base
	.align	3
_aegis_gas__gas_call_base:
	.xword	700
	.globl _aegis_gas__gas_sstore_refund
	.align	3
_aegis_gas__gas_sstore_refund:
	.xword	15000
	.globl _aegis_gas__gas_sstore_clear
	.align	3
_aegis_gas__gas_sstore_clear:
	.xword	5000
	.globl _aegis_gas__gas_sstore_reset
	.align	3
_aegis_gas__gas_sstore_reset:
	.xword	5000
	.globl _aegis_gas__gas_sstore_set
	.align	3
_aegis_gas__gas_sstore_set:
	.xword	20000
	.globl _aegis_gas__gas_sload
	.align	3
_aegis_gas__gas_sload:
	.xword	200
	.globl _aegis_gas__gas_mcopy_per_word
	.align	3
_aegis_gas__gas_mcopy_per_word:
	.xword	3
	.globl _aegis_gas__gas_mcopy_base
	.align	3
_aegis_gas__gas_mcopy_base:
	.xword	3
	.globl _aegis_gas__gas_mstore8
	.align	3
_aegis_gas__gas_mstore8:
	.xword	3
	.globl _aegis_gas__gas_mstore
	.align	3
_aegis_gas__gas_mstore:
	.xword	3
	.globl _aegis_gas__gas_mload
	.align	3
_aegis_gas__gas_mload:
	.xword	3
	.globl _aegis_gas__gas_ring_verify
	.align	3
_aegis_gas__gas_ring_verify:
	.xword	100000
	.globl _aegis_gas__gas_ring_sign
	.align	3
_aegis_gas__gas_ring_sign:
	.xword	150000
	.globl _aegis_gas__gas_fri_verify
	.align	3
_aegis_gas__gas_fri_verify:
	.xword	50000
	.globl _aegis_gas__gas_stark_generate_base
	.align	3
_aegis_gas__gas_stark_generate_base:
	.xword	500000
	.globl _aegis_gas__gas_stark_verify_per_sig
	.align	3
_aegis_gas__gas_stark_verify_per_sig:
	.xword	5000
	.globl _aegis_gas__gas_stark_verify_base
	.align	3
_aegis_gas__gas_stark_verify_base:
	.xword	200000
	.globl _aegis_gas__gas_decrypt
	.align	3
_aegis_gas__gas_decrypt:
	.xword	8000
	.globl _aegis_gas__gas_encrypt
	.align	3
_aegis_gas__gas_encrypt:
	.xword	8000
	.globl _aegis_gas__gas_nullify
	.align	3
_aegis_gas__gas_nullify:
	.xword	15000
	.globl _aegis_gas__gas_verify_proof
	.align	3
_aegis_gas__gas_verify_proof:
	.xword	25000
	.globl _aegis_gas__gas_prove_linear
	.align	3
_aegis_gas__gas_prove_linear:
	.xword	50000
	.globl _aegis_gas__gas_prove_range
	.align	3
_aegis_gas__gas_prove_range:
	.xword	75000
	.globl _aegis_gas__gas_commit
	.align	3
_aegis_gas__gas_commit:
	.xword	10000
	.globl _aegis_gas__gas_unshield
	.align	3
_aegis_gas__gas_unshield:
	.xword	100000
	.globl _aegis_gas__gas_shield
	.align	3
_aegis_gas__gas_shield:
	.xword	50000
	.globl _aegis_gas__gas_private_store
	.align	3
_aegis_gas__gas_private_store:
	.xword	25000
	.globl _aegis_gas__gas_private_load
	.align	3
_aegis_gas__gas_private_load:
	.xword	5000
	.globl _aegis_gas__gas_mlkem_decaps
	.align	3
_aegis_gas__gas_mlkem_decaps:
	.xword	25000
	.globl _aegis_gas__gas_mlkem_encaps
	.align	3
_aegis_gas__gas_mlkem_encaps:
	.xword	25000
	.globl _aegis_gas__gas_mlkem_keygen
	.align	3
_aegis_gas__gas_mlkem_keygen:
	.xword	20000
	.globl _aegis_gas__gas_mldsa_verify
	.align	3
_aegis_gas__gas_mldsa_verify:
	.xword	30000
	.globl _aegis_gas__gas_mldsa_sign
	.align	3
_aegis_gas__gas_mldsa_sign:
	.xword	100000
	.globl _aegis_gas__gas_mldsa_keygen
	.align	3
_aegis_gas__gas_mldsa_keygen:
	.xword	50000
	.globl _aegis_gas__gas_keccak256_per_word
	.align	3
_aegis_gas__gas_keccak256_per_word:
	.xword	6
	.globl _aegis_gas__gas_keccak256_base
	.align	3
_aegis_gas__gas_keccak256_base:
	.xword	30
	.globl _aegis_gas__gas_sha3_per_word
	.align	3
_aegis_gas__gas_sha3_per_word:
	.xword	6
	.globl _aegis_gas__gas_sha3_base
	.align	3
_aegis_gas__gas_sha3_base:
	.xword	30
	.globl _aegis_gas__gas_shr
	.align	3
_aegis_gas__gas_shr:
	.xword	3
	.globl _aegis_gas__gas_shl
	.align	3
_aegis_gas__gas_shl:
	.xword	3
	.globl _aegis_gas__gas_not
	.align	3
_aegis_gas__gas_not:
	.xword	3
	.globl _aegis_gas__gas_xor
	.align	3
_aegis_gas__gas_xor:
	.xword	3
	.globl _aegis_gas__gas_or
	.align	3
_aegis_gas__gas_or:
	.xword	3
	.globl _aegis_gas__gas_and
	.align	3
_aegis_gas__gas_and:
	.xword	3
	.globl _aegis_gas__gas_eq
	.align	3
_aegis_gas__gas_eq:
	.xword	3
	.globl _aegis_gas__gas_gt
	.align	3
_aegis_gas__gas_gt:
	.xword	3
	.globl _aegis_gas__gas_lt
	.align	3
_aegis_gas__gas_lt:
	.xword	3
	.globl _aegis_gas__gas_exp_per_byte
	.align	3
_aegis_gas__gas_exp_per_byte:
	.xword	50
	.globl _aegis_gas__gas_exp_base
	.align	3
_aegis_gas__gas_exp_base:
	.xword	10
	.globl _aegis_gas__gas_mod
	.align	3
_aegis_gas__gas_mod:
	.xword	5
	.globl _aegis_gas__gas_div
	.align	3
_aegis_gas__gas_div:
	.xword	5
	.globl _aegis_gas__gas_mul
	.align	3
_aegis_gas__gas_mul:
	.xword	5
	.globl _aegis_gas__gas_sub
	.align	3
_aegis_gas__gas_sub:
	.xword	3
	.globl _aegis_gas__gas_add
	.align	3
_aegis_gas__gas_add:
	.xword	3
	.globl _aegis_gas__gas_base
	.align	3
_aegis_gas__gas_base:
	.xword	2
	.globl _aegis_gas_E
	.data
	.align	1
_aegis_gas_E:
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
	.byte	0x4
	.set L$set$3,LCFI0-LFB2
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
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$7,LEFDE3-LASFDE3
	.long L$set$7
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB4-.
	.set L$set$8,LFE4-LFB4
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB4
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
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xb
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$15,LEFDE5-LASFDE5
	.long L$set$15
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB6-.
	.set L$set$16,LFE6-LFB6
	.quad L$set$16
	.uleb128 0
	.byte	0x4
	.set L$set$17,LCFI10-LFB6
	.long L$set$17
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0xa
	.byte	0xde
	.byte	0xdd
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
	.quad	LFB8-.
	.set L$set$23,LFE8-LFB8
	.quad L$set$23
	.uleb128 0
	.byte	0x4
	.set L$set$24,LCFI15-LFB8
	.long L$set$24
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$25,LCFI16-LCFI15
	.long L$set$25
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$26,LCFI17-LCFI16
	.long L$set$26
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$27,LCFI18-LCFI17
	.long L$set$27
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$28,LCFI19-LCFI18
	.long L$set$28
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$29,LEFDE9-LASFDE9
	.long L$set$29
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB9-.
	.set L$set$30,LFE9-LFB9
	.quad L$set$30
	.uleb128 0
	.byte	0x4
	.set L$set$31,LCFI20-LFB9
	.long L$set$31
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$32,LCFI21-LCFI20
	.long L$set$32
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$33,LEFDE11-LASFDE11
	.long L$set$33
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB11-.
	.set L$set$34,LFE11-LFB11
	.quad L$set$34
	.uleb128 0
	.byte	0x4
	.set L$set$35,LCFI22-LFB11
	.long L$set$35
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$36,LCFI23-LCFI22
	.long L$set$36
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$37,LCFI24-LCFI23
	.long L$set$37
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$38,LCFI25-LCFI24
	.long L$set$38
	.byte	0xb
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$39,LEFDE13-LASFDE13
	.long L$set$39
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB13-.
	.set L$set$40,LFE13-LFB13
	.quad L$set$40
	.uleb128 0
	.byte	0x4
	.set L$set$41,LCFI26-LFB13
	.long L$set$41
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$42,LCFI27-LCFI26
	.long L$set$42
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$43,LCFI28-LCFI27
	.long L$set$43
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI29-LCFI28
	.long L$set$44
	.byte	0xb
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$45,LEFDE15-LASFDE15
	.long L$set$45
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB15-.
	.set L$set$46,LFE15-LFB15
	.quad L$set$46
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI30-LFB15
	.long L$set$47
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$48,LCFI31-LCFI30
	.long L$set$48
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$49,LCFI32-LCFI31
	.long L$set$49
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI33-LCFI32
	.long L$set$50
	.byte	0xb
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$51,LEFDE17-LASFDE17
	.long L$set$51
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB17-.
	.set L$set$52,LFE17-LFB17
	.quad L$set$52
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI34-LFB17
	.long L$set$53
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$54,LCFI35-LCFI34
	.long L$set$54
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$55,LCFI36-LCFI35
	.long L$set$55
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$56,LCFI37-LCFI36
	.long L$set$56
	.byte	0xb
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$57,LEFDE19-LASFDE19
	.long L$set$57
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB18-.
	.set L$set$58,LFE18-LFB18
	.quad L$set$58
	.uleb128 0
	.byte	0x4
	.set L$set$59,LCFI38-LFB18
	.long L$set$59
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$60,LCFI39-LCFI38
	.long L$set$60
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$61,LCFI40-LCFI39
	.long L$set$61
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$62,LCFI41-LCFI40
	.long L$set$62
	.byte	0xb
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$63,LEFDE21-LASFDE21
	.long L$set$63
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB20-.
	.set L$set$64,LFE20-LFB20
	.quad L$set$64
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI42-LFB20
	.long L$set$65
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$66,LCFI43-LCFI42
	.long L$set$66
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$67,LEFDE23-LASFDE23
	.long L$set$67
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB21-.
	.set L$set$68,LFE21-LFB21
	.quad L$set$68
	.uleb128 0
	.byte	0x4
	.set L$set$69,LCFI44-LFB21
	.long L$set$69
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$70,LCFI45-LCFI44
	.long L$set$70
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$71,LEFDE25-LASFDE25
	.long L$set$71
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB23-.
	.set L$set$72,LFE23-LFB23
	.quad L$set$72
	.uleb128 0
	.byte	0x4
	.set L$set$73,LCFI46-LFB23
	.long L$set$73
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$74,LCFI47-LCFI46
	.long L$set$74
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$75,LCFI48-LCFI47
	.long L$set$75
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI49-LCFI48
	.long L$set$76
	.byte	0xb
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$77,LEFDE27-LASFDE27
	.long L$set$77
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB24-.
	.set L$set$78,LFE24-LFB24
	.quad L$set$78
	.uleb128 0
	.byte	0x4
	.set L$set$79,LCFI50-LFB24
	.long L$set$79
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$80,LCFI51-LCFI50
	.long L$set$80
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$81,LEFDE29-LASFDE29
	.long L$set$81
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB26-.
	.set L$set$82,LFE26-LFB26
	.quad L$set$82
	.uleb128 0
	.byte	0x4
	.set L$set$83,LCFI52-LFB26
	.long L$set$83
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$84,LCFI53-LCFI52
	.long L$set$84
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE29:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
