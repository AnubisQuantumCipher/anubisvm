	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
_khepri_storage_trie__node_value_to_bytes:
LFB12:
	stp	x29, x30, [sp, -48]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI2:
	cbnz	w1, L9
	mov	x1, 4
	mov	x0, 8
	bl	_system__secondary_stack__ss_allocate
	movi	d31, 0xffffffff00000000
	mov	x4, x0
	mov	x20, x0
	mov	x1, x20
	str	d31, [x4], 8
	mov	x0, x4
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI3:
	ret
	.p2align 2,,3
L9:
LCFI4:
	sub	w22, w1, #1
	mov	x21, x0
	sxtw	x0, w22
	mov	w19, w1
	add	x0, x0, 12
	mov	x1, 4
	and	x0, x0, -4
	bl	_system__secondary_stack__ss_allocate
	mov	x20, x0
	add	x4, x0, 8
	sxtw	x2, w19
	mov	x0, x4
	mov	w1, 0
	stp	wzr, w22, [x20]
	bl	_memset
	mov	x4, x0
	uxtw	x3, w19
	mov	x2, 0
	.p2align 5,,15
L3:
	cmp	x2, 31
	bhi	L5
	ldrb	w0, [x21, x2]
	strb	w0, [x4, x2]
L5:
	add	x2, x2, 1
	cmp	x3, x2
	bne	L3
	mov	x0, x4
	mov	x1, x20
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI5:
	ret
LFE12:
	.const
	.align	3
lC1:
	.ascii "khepri_storage_trie.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_khepri_storage_trie__bytes_to_u256:
LFB11:
	stp	x29, x30, [sp, -208]!
LCFI6:
	mov	x29, sp
LCFI7:
	movi	v31.4s, 0
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI8:
	str	x8, [x29, 104]
	ldp	w20, w26, [x1]
	stp	q31, q31, [x29, 144]
	cmp	w20, w26
	ble	L23
	ldr	x0, [x29, 104]
	stp	q31, q31, [x0]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 208
LCFI9:
	ret
	.p2align 2,,3
L23:
LCFI10:
	sxtw	x24, w26
	sxtw	x27, w20
	sub	x24, x24, x27
	mov	x25, x0
	mov	x1, 2147483647
	add	x0, x24, 1
	cmp	x0, x1
	bgt	L13
	cmp	w0, 0
	ble	L13
	mov	x22, x27
	mov	w19, -1
	add	x21, x29, 144
	add	x28, x29, 112
	add	x23, x29, 176
	cmp	w24, 31
	mov	w0, 31
	csel	w24, w24, w0, le
	.p2align 5,,15
L14:
	mov	x0, x21
	mov	x8, x28
	mov	w1, 8
	add	w19, w19, 1
	bl	_aegis_u256__shift_left
	ld1	{v30.16b - v31.16b}, [x28]
	adds	w0, w20, w19
	st1	{v30.16b - v31.16b}, [x21]
	bvs	L17
	cmp	w26, w0
	ccmp	w20, w0, 0, ge
	bgt	L24
	sub	x0, x22, x27
	mov	x8, x23
	ldrb	w0, [x0, x25]
	add	x22, x22, 1
	bl	_aegis_u256__from_word64
	mov	x8, x28
	mov	x1, x23
	mov	x0, x21
	bl	_aegis_u256__bit_or
	ld1	{v30.16b - v31.16b}, [x28]
	st1	{v30.16b - v31.16b}, [x21]
	cmp	w24, w19
	bne	L14
	ldr	x0, [x29, 104]
	st1	{v30.16b - v31.16b}, [x0]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 208
LCFI11:
	ret
L24:
LCFI12:
	adrp	x0, lC1@PAGE
	mov	w1, 112
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L17:
	adrp	x0, lC1@PAGE
	mov	w1, 112
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L13:
	adrp	x0, lC1@PAGE
	mov	w1, 103
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE11:
	.align	2
	.p2align 5,,15
_khepri_storage_trie__find_contract_entry:
LFB8:
	adrp	x3, _khepri_storage_trie__contract_map@PAGE
	mov	x1, 0
	add	x3, x3, _khepri_storage_trie__contract_map@PAGEOFF;
	mov	x5, 10000
	mov	x4, x3
	.p2align 5,,15
L31:
	add	x2, x1, x1, lsl 2
	add	x2, x4, x2, lsl 3
	ldrb	w2, [x2, 36]
	cmp	w2, 1
	bhi	L40
	cbz	w2, L27
	ldr	q30, [x0]
	ldr	q31, [x3]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x2, d31
	cbnz	x2, L27
	ldr	q30, [x0, 16]
	ldr	q31, [x3, 16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x2, d31
	cbnz	x2, L27
	mov	w0, w1
	ret
	.p2align 2,,3
L27:
	add	x1, x1, 1
	add	x3, x3, 40
	cmp	x1, x5
	bne	L31
	mov	w1, 2147483647
	mov	w0, w1
	ret
L40:
	adrp	x0, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI13:
	mov	w1, 65
	mov	x29, sp
LCFI14:
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE8:
	.align	2
	.p2align 5,,15
_khepri_storage_trie__u256_to_key.constprop.0:
LFB73:
	stp	x29, x30, [sp, -64]!
LCFI15:
	mov	x29, sp
LCFI16:
	stp	x19, x20, [sp, 16]
LCFI17:
	add	x19, x29, 32
	mov	x20, x1
	mov	x8, x19
	bl	_aegis_u256__to_bytes_be
	ldp	q31, q30, [x19]
	stp	q31, q30, [x20]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI18:
	ret
LFE73:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__storage_errorH
_khepri_storage_trie__storage_errorH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L46
	sub	w1, w1, w2
	cmp	w1, 7
	bgt	L47
L46:
	mov	x3, 0
	mov	x0, 0
L44:
	adrp	x2, _storage_errorG.2@PAGE
	mov	w1, 52429
	add	x2, x2, _storage_errorG.2@PAGEOFF;
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
L47:
	ldrb	w2, [x0, 8]
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
	b	L44
LFE2:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__storage_entryIP
_khepri_storage_trie__storage_entryIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__Tstorage_array_typeBIP
_khepri_storage_trie__Tstorage_array_typeBIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__contract_entryIP
_khepri_storage_trie__contract_entryIP:
LFB72:
	ret
LFE72:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__Tcontract_map_typeBIP
_khepri_storage_trie__Tcontract_map_typeBIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__create_storage
_khepri_storage_trie__create_storage:
LFB13:
	stp	x29, x30, [sp, -64]!
LCFI19:
	mov	x29, sp
LCFI20:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI21:
	mov	x21, x0
	str	x23, [sp, 48]
LCFI22:
	bl	_khepri_storage_trie__find_contract_entry
	mov	w1, 2147483647
	cmp	w0, w1
	beq	L63
	adrp	x1, _khepri_storage_trie__contract_map@PAGE
	sbfiz	x2, x0, 2, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_storage_trie__contract_map@PAGEOFF;
	mov	w4, 10000
	add	x0, x1, x0, lsl 3
	mov	w3, 1
	ldr	w2, [x0, 32]
	cmp	w2, w4
	bhi	L74
L54:
	mov	x0, 0
	bfi	x0, x2, 0, 32
	bfi	x0, x3, 32, 8
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI23:
	ret
	.p2align 2,,3
L63:
LCFI24:
	adrp	x23, _khepri_storage_trie__storage_array@PAGE
	mov	x19, 0
	mov	x2, 10001
	add	x0, x23, _khepri_storage_trie__storage_array@PAGEOFF;
	.p2align 5,,15
L53:
	mov	x1, 1
	cmp	x19, 0
	csel	x19, x19, x1, ne
	add	x1, x19, x19, lsl 2
	add	x1, x0, x1, lsl 3
	ldrb	w1, [x1, 36]
	cmp	w1, 1
	bhi	L75
	cbz	w1, L66
	add	x19, x19, 1
	cmp	x19, x2
	bne	L53
L58:
	mov	w2, 0
	mov	x0, 0
	bfi	x0, x2, 0, 32
	mov	w3, 0
	bfi	x0, x3, 32, 8
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI25:
	ret
	.p2align 2,,3
L66:
LCFI26:
	adrp	x22, _khepri_storage_trie__contract_map@PAGE
	mov	x20, 0
	mov	x1, 10000
	add	x22, x22, _khepri_storage_trie__contract_map@PAGEOFF;
	b	L57
	.p2align 2,,3
L77:
	add	x20, x20, 1
	cmp	x20, x1
	beq	L58
L57:
	add	x0, x20, x20, lsl 2
	add	x0, x22, x0, lsl 3
	ldrb	w0, [x0, 36]
	cmp	w0, 1
	bhi	L76
	cbnz	w0, L77
	bl	_khepri_mpt__create_trie
	ubfx	x1, x0, 32, 8
	cmp	w1, 1
	bhi	L78
	cbz	w1, L58
	ldp	q30, q31, [x21]
	add	x23, x23, _khepri_storage_trie__storage_array@PAGEOFF;
	sbfiz	x4, x19, 2, 32
	sbfiz	x1, x20, 2, 32
	add	x4, x4, w19, sxtw
	add	x1, x1, w20, sxtw
	lsl	x7, x4, 3
	lsl	x6, x1, 3
	add	x4, x23, x4, lsl 3
	add	x1, x22, x1, lsl 3
	mov	w5, 1
	mov	w2, w19
	mov	w3, 1
	str	w0, [x4, 32]
	str	w19, [x1, 32]
	strb	w5, [x4, 36]
	strb	w5, [x1, 36]
	str	q30, [x23, x7]
	str	q30, [x22, x6]
	str	q31, [x1, 16]
	str	q31, [x4, 16]
	b	L54
L75:
	adrp	x0, lC1@PAGE
	mov	w1, 55
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L76:
	adrp	x0, lC1@PAGE
	mov	w1, 77
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L74:
	adrp	x0, lC1@PAGE
	mov	w1, 153
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L78:
	adrp	x0, lC1@PAGE
	mov	w1, 171
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__load_storage
_khepri_storage_trie__load_storage:
LFB14:
	stp	x29, x30, [sp, -96]!
LCFI27:
	mov	x29, sp
LCFI28:
	mov	x4, 10001
	stp	x21, x22, [sp, 32]
LCFI29:
	adrp	x22, _khepri_storage_trie__storage_array@PAGE
	add	x3, x22, _khepri_storage_trie__storage_array@PAGEOFF;
	stp	x19, x20, [sp, 16]
LCFI30:
	mov	x19, 0
	str	x23, [sp, 48]
LCFI31:
	mov	x23, x0
	.p2align 5,,15
L84:
	mov	x2, 1
	cmp	x19, 0
	csel	x19, x19, x2, ne
	add	x2, x19, x19, lsl 2
	add	x2, x3, x2, lsl 3
	ldrb	w2, [x2, 36]
	cmp	w2, 1
	bhi	L99
	cbz	w2, L91
	add	x19, x19, 1
	cmp	x19, x4
	bne	L84
L83:
	mov	w2, 0
	mov	w3, 0
L85:
	mov	x0, 0
	bfi	x0, x3, 0, 32
	bfi	x0, x2, 32, 8
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI32:
	ret
	.p2align 2,,3
L91:
LCFI33:
	adrp	x21, _khepri_storage_trie__contract_map@PAGE
	mov	x20, 0
	mov	x3, 10000
	add	x21, x21, _khepri_storage_trie__contract_map@PAGEOFF;
	b	L82
	.p2align 2,,3
L101:
	add	x20, x20, 1
	cmp	x20, x3
	beq	L83
L82:
	add	x2, x20, x20, lsl 2
	add	x2, x21, x2, lsl 3
	ldrb	w2, [x2, 36]
	cmp	w2, 1
	bhi	L100
	cbnz	w2, L101
	mov	x0, x1
	bl	_khepri_mpt__load_trie
	ubfx	x1, x0, 32, 8
	cmp	w1, 1
	bhi	L102
	cbz	w1, L83
	ldp	q30, q31, [x23]
	add	x22, x22, _khepri_storage_trie__storage_array@PAGEOFF;
	sbfiz	x4, x19, 2, 32
	sbfiz	x1, x20, 2, 32
	add	x4, x4, w19, sxtw
	add	x1, x1, w20, sxtw
	lsl	x7, x4, 3
	lsl	x6, x1, 3
	add	x4, x22, x4, lsl 3
	add	x1, x21, x1, lsl 3
	mov	w5, 1
	mov	w3, w19
	mov	w2, 1
	str	w0, [x4, 32]
	str	w19, [x1, 32]
	strb	w5, [x4, 36]
	strb	w5, [x1, 36]
	str	q30, [x22, x7]
	str	q30, [x21, x6]
	str	q31, [x1, 16]
	str	q31, [x4, 16]
	b	L85
L99:
	adrp	x0, lC1@PAGE
	mov	w1, 55
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L100:
	adrp	x0, lC1@PAGE
	mov	w1, 77
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L102:
	adrp	x0, lC1@PAGE
	mov	w1, 220
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE14:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__destroy_storage
_khepri_storage_trie__destroy_storage:
LFB15:
	stp	x29, x30, [sp, -64]!
LCFI34:
	mov	x29, sp
LCFI35:
	stp	x19, x20, [sp, 16]
LCFI36:
	mov	w20, 10000
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI37:
	cmp	w0, w20
	bhi	L106
	mov	w19, w0
	cbz	w0, L103
	sxtw	x23, w0
	sbfiz	x21, x0, 2, 32
	adrp	x22, _khepri_storage_trie__storage_array@PAGE
	add	x0, x21, x23
	add	x1, x22, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x0, x1, x0, lsl 3
	ldrb	w1, [x0, 36]
	cmp	w1, 1
	bhi	L106
	cbz	w1, L103
	ldr	w0, [x0, 32]
	cmp	w0, 255
	bhi	L124
	bl	_khepri_mpt__destroy_trie
	adrp	x0, _khepri_storage_trie__contract_map@PAGE
	mov	x1, 0
	mov	x4, 10000
	add	x0, x0, _khepri_storage_trie__contract_map@PAGEOFF;
	b	L112
	.p2align 2,,3
L109:
	add	x1, x1, 1
	cmp	x1, x4
	beq	L111
L112:
	add	x2, x1, x1, lsl 2
	add	x2, x0, x2, lsl 3
	ldrb	w3, [x2, 36]
	cmp	w3, 1
	bhi	L125
	cbz	w3, L109
	ldr	w3, [x2, 32]
	cmp	w3, w20
	bhi	L126
	cmp	w19, w3
	bne	L109
	strb	wzr, [x2, 36]
L111:
	add	x22, x22, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x0, x21, x23
	add	x0, x22, x0, lsl 3
	str	wzr, [x0, 32]
	strb	wzr, [x0, 36]
L103:
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI38:
	ret
L125:
LCFI39:
	adrp	x0, lC1@PAGE
	mov	w1, 250
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L126:
	adrp	x0, lC1@PAGE
	mov	w1, 251
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L106:
	adrp	x0, lC1@PAGE
	mov	w1, 244
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L124:
	adrp	x0, lC1@PAGE
	mov	w1, 246
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE15:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__sload
_khepri_storage_trie__sload:
LFB16:
	stp	x29, x30, [sp, -192]!
LCFI40:
	movi	v31.4s, 0
	mov	x29, sp
LCFI41:
LEHB0:
	mov	w3, w0
	mov	w0, 10000
	stp	x19, x20, [sp, 16]
LCFI42:
	mov	x20, x2
	add	x2, x29, 192
	stp	x21, x22, [sp, 32]
LCFI43:
	stp	xzr, xzr, [x29, 88]
	stp	xzr, xzr, [x29, 104]
	str	x2, [x29, 144]
	stp	q31, q31, [x20]
	cmp	w3, w0
	bhi	L130
	cbz	w3, L139
	adrp	x0, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x2, x3, 2, 32
	add	x3, x2, w3, sxtw
	add	x0, x0, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x19, x0, x3, lsl 3
	ldrb	w2, [x19, 36]
	cmp	w2, 1
	bhi	L130
	mov	w3, 3
	cbnz	w2, L146
L129:
	mov	x0, 0
	bfi	x0, x2, 0, 8
	bfi	x0, x3, 8, 8
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 192
LCFI44:
	ret
	.p2align 2,,3
L139:
LCFI45:
	mov	w2, 0
	mov	x0, 0
	bfi	x0, x2, 0, 8
	mov	w3, 3
	bfi	x0, x3, 8, 8
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 192
LCFI46:
	ret
	.p2align 2,,3
L146:
LCFI47:
	adrp	x21, lC0@PAGE
	add	x22, x29, 88
	add	x21, x21, lC0@PAGEOFF;
	mov	x0, x1
	mov	x2, x21
	mov	x1, x22
	bl	_khepri_storage_trie__u256_to_key.constprop.0
	ldr	w0, [x19, 32]
	cmp	w0, 255
	bhi	L147
	add	x19, x29, 152
	mov	x1, x22
	mov	x2, x21
	mov	x3, x19
	bl	_khepri_mpt__get
	ubfx	w1, w0, 8, 8
	movi	v31.4s, 0
	and	w0, w0, 255
	cmp	w1, 7
	bhi	L148
	cmp	w1, 1
	bhi	L141
	cmp	w0, 1
	bhi	L149
	cbnz	w0, L150
	stp	q31, q31, [x20]
LEHE0:
L137:
	mov	w3, 0
	mov	w2, 1
	b	L129
	.p2align 2,,3
L141:
	mov	w2, 0
	mov	w3, 4
	b	L129
	.p2align 2,,3
L150:
	add	x21, x29, 120
	mov	x8, x21
LEHB1:
	bl	_system__secondary_stack__ss_mark
	ldr	w1, [x29, 184]
	tbnz	w1, #31, L151
	mov	x0, x19
	bl	_khepri_storage_trie__node_value_to_bytes
	ldp	w2, w3, [x1]
	cmp	w2, 0
	ccmp	w2, w3, 0, lt
	ble	L152
	add	x19, x29, 48
	mov	x8, x19
	bl	_khepri_storage_trie__bytes_to_u256
	ld1	{v30.16b - v31.16b}, [x19]
LEHE1:
	mov	x0, x21
	st1	{v30.16b - v31.16b}, [x20]
LEHB2:
	bl	_system__secondary_stack__ss_release
	b	L137
L130:
	adrp	x0, lC1@PAGE
	mov	w1, 284
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L148:
	adrp	x0, lC1@PAGE
	mov	w1, 293
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L147:
	adrp	x0, lC1@PAGE
	mov	w1, 291
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L149:
	adrp	x0, lC1@PAGE
	mov	w1, 300
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE2:
L152:
	adrp	x0, lC1@PAGE
	mov	w1, 303
	add	x0, x0, lC1@PAGEOFF;
LEHB3:
	bl	___gnat_rcheck_CE_Range_Check
L151:
	adrp	x0, lC1@PAGE
	mov	w1, 303
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE3:
L142:
	mov	x19, x0
	mov	x0, x21
LEHB4:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE4:
LFE16:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
LLSDA16:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE16-LLSDACSB16
LLSDACSB16:
	.uleb128 LEHB0-LFB16
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB16
	.uleb128 LEHE1-LEHB1
	.uleb128 L142-LFB16
	.uleb128 0
	.uleb128 LEHB2-LFB16
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB3-LFB16
	.uleb128 LEHE3-LEHB3
	.uleb128 L142-LFB16
	.uleb128 0
	.uleb128 LEHB4-LFB16
	.uleb128 LEHE4-LEHB4
	.uleb128 0
	.uleb128 0
LLSDACSE16:
	.text
	.const
	.align	2
lC0:
	.word	0
	.word	31
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__sstore
_khepri_storage_trie__sstore:
LFB18:
	stp	x29, x30, [sp, -128]!
LCFI48:
	mov	x29, sp
LCFI49:
	mov	w3, w0
	mov	w0, 10000
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI50:
	stp	xzr, xzr, [x29, 64]
	stp	xzr, xzr, [x29, 80]
	stp	xzr, xzr, [x29, 96]
	stp	xzr, xzr, [x29, 112]
	cmp	w3, w0
	bhi	L156
	cbz	w3, L163
	adrp	x0, _khepri_storage_trie__storage_array@PAGE
	mov	x20, x2
	add	x0, x0, _khepri_storage_trie__storage_array@PAGEOFF;
	sbfiz	x2, x3, 2, 32
	add	x3, x2, w3, sxtw
	add	x19, x0, x3, lsl 3
	ldrb	w3, [x19, 36]
	cmp	w3, 1
	bhi	L156
	mov	w2, 3
	cbnz	w3, L168
L155:
	mov	x0, 0
	bfi	x0, x3, 0, 8
	bfi	x0, x2, 8, 8
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 128
LCFI51:
	ret
	.p2align 2,,3
L163:
LCFI52:
	mov	w3, 0
	mov	x0, 0
	bfi	x0, x3, 0, 8
	mov	w2, 3
	bfi	x0, x2, 8, 8
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 128
LCFI53:
	ret
	.p2align 2,,3
L168:
LCFI54:
	adrp	x21, lC0@PAGE
	add	x22, x29, 64
	add	x21, x21, lC0@PAGEOFF;
	mov	x0, x1
	mov	x2, x21
	mov	x1, x22
	bl	_khepri_storage_trie__u256_to_key.constprop.0
	add	x23, x29, 96
	mov	x0, x20
	mov	x1, x23
	mov	x2, x21
	bl	_khepri_storage_trie__u256_to_key.constprop.0
	adrp	x1, _aegis_vm_types__u256_zero@GOTPAGE
	ldr	x1, [x1, _aegis_vm_types__u256_zero@GOTPAGEOFF]
	mov	x0, x20
	bl	_aegis_u256__equal
	cmp	w0, 1
	bhi	L169
	cbz	w0, L158
	ldr	w0, [x19, 32]
	cmp	w0, 255
	bhi	L170
	mov	x1, x22
	mov	x2, x21
	bl	_khepri_mpt__delete
	ubfx	w1, w0, 8, 8
	and	w3, w0, 255
	cmp	w1, 7
	bhi	L171
	cmp	w1, 1
	beq	L165
L162:
	mov	x0, 0
	cmp	w1, 0
	cset	w2, ne
	bfi	x0, x3, 0, 8
	lsl	w2, w2, 2
	bfi	x0, x2, 8, 8
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 128
LCFI55:
	ret
	.p2align 2,,3
L158:
LCFI56:
	ldr	w0, [x19, 32]
	cmp	w0, 255
	bhi	L172
	mov	x3, x23
	mov	x1, x22
	mov	x4, x21
	mov	x2, x21
	bl	_khepri_mpt__put
	ubfx	w1, w0, 8, 8
	and	w3, w0, 255
	cmp	w1, 7
	bls	L162
	adrp	x0, lC1@PAGE
	mov	w1, 346
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L165:
	mov	w2, 0
	mov	w3, w1
	b	L155
L156:
	adrp	x0, lC1@PAGE
	mov	w1, 328
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L169:
	adrp	x0, lC1@PAGE
	mov	w1, 337
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L171:
	adrp	x0, lC1@PAGE
	mov	w1, 339
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L170:
	adrp	x0, lC1@PAGE
	mov	w1, 338
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L172:
	adrp	x0, lC1@PAGE
	mov	w1, 345
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE18:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__sdelete
_khepri_storage_trie__sdelete:
LFB19:
	stp	x29, x30, [sp, -80]!
LCFI57:
	mov	x29, sp
LCFI58:
	mov	w2, w0
	mov	w0, 10000
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI59:
	stp	xzr, xzr, [x29, 48]
	stp	xzr, xzr, [x29, 64]
	cmp	w2, w0
	bhi	L176
	cbz	w2, L179
	adrp	x0, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x3, x2, 2, 32
	add	x2, x3, w2, sxtw
	add	x0, x0, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x19, x0, x2, lsl 3
	ldrb	w2, [x19, 36]
	cmp	w2, 1
	bhi	L176
	mov	w3, 3
	cbnz	w2, L184
L175:
	mov	x0, 0
	bfi	x0, x2, 0, 8
	bfi	x0, x3, 8, 8
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI60:
	ret
	.p2align 2,,3
L179:
LCFI61:
	mov	w2, 0
	mov	x0, 0
	bfi	x0, x2, 0, 8
	mov	w3, 3
	bfi	x0, x3, 8, 8
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI62:
	ret
	.p2align 2,,3
L184:
LCFI63:
	adrp	x20, lC0@PAGE
	add	x21, x29, 48
	add	x20, x20, lC0@PAGEOFF;
	mov	x0, x1
	mov	x2, x20
	mov	x1, x21
	bl	_khepri_storage_trie__u256_to_key.constprop.0
	ldr	w0, [x19, 32]
	cmp	w0, 255
	bhi	L185
	mov	x1, x21
	mov	x2, x20
	bl	_khepri_mpt__delete
	ubfx	w1, w0, 8, 8
	and	w2, w0, 255
	cmp	w1, 7
	bhi	L186
	cmp	w1, 1
	beq	L181
	mov	x0, 0
	cmp	w1, 0
	cset	w3, ne
	bfi	x0, x2, 0, 8
	lsl	w3, w3, 2
	bfi	x0, x3, 8, 8
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI64:
	ret
	.p2align 2,,3
L181:
LCFI65:
	mov	w3, 0
	mov	w2, w1
	b	L175
L176:
	adrp	x0, lC1@PAGE
	mov	w1, 364
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L186:
	adrp	x0, lC1@PAGE
	mov	w1, 373
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L185:
	adrp	x0, lC1@PAGE
	mov	w1, 371
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE19:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__slot_exists
_khepri_storage_trie__slot_exists:
LFB20:
	stp	x29, x30, [sp, -80]!
LCFI66:
	mov	x29, sp
LCFI67:
	mov	w2, w0
	mov	w0, 10000
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI68:
	stp	xzr, xzr, [x29, 48]
	stp	xzr, xzr, [x29, 64]
	cmp	w2, w0
	bhi	L190
	cbz	w2, L193
	adrp	x0, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x3, x2, 2, 32
	add	x2, x3, w2, sxtw
	add	x0, x0, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x19, x0, x2, lsl 3
	ldrb	w0, [x19, 36]
	cmp	w0, 1
	bhi	L190
	cbnz	w0, L198
L189:
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI69:
	ret
	.p2align 2,,3
L198:
LCFI70:
	adrp	x20, lC0@PAGE
	add	x21, x29, 48
	add	x20, x20, lC0@PAGEOFF;
	mov	x0, x1
	mov	x2, x20
	mov	x1, x21
	bl	_khepri_storage_trie__u256_to_key.constprop.0
	ldr	w0, [x19, 32]
	cmp	w0, 255
	bhi	L192
	mov	x1, x21
	mov	x2, x20
	bl	_khepri_mpt__contains
	cmp	w0, 1
	bls	L189
L192:
	adrp	x0, lC1@PAGE
	mov	w1, 392
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L193:
	mov	w0, 0
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI71:
	ret
L190:
LCFI72:
	adrp	x0, lC1@PAGE
	mov	w1, 387
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__get_root
_khepri_storage_trie__get_root:
LFB21:
	stp	x29, x30, [sp, -16]!
LCFI73:
	mov	w2, 10000
	mov	x29, sp
LCFI74:
	cmp	w0, w2
	bhi	L202
	mov	x1, x8
	cbz	w0, L201
	adrp	x2, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x3, x0, 2, 32
	add	x0, x3, w0, sxtw
	add	x2, x2, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x0, x2, x0, lsl 3
	ldrb	w2, [x0, 36]
	cmp	w2, 1
	bhi	L202
	cbz	w2, L201
	ldr	w0, [x0, 32]
	cmp	w0, 255
	bhi	L210
	bl	_khepri_mpt__root_hash
	ldp	x29, x30, [sp], 16
LCFI75:
	ret
	.p2align 2,,3
L201:
LCFI76:
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	ldp	x29, x30, [sp], 16
LCFI77:
	ret
L202:
LCFI78:
	adrp	x0, lC1@PAGE
	mov	w1, 401
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L210:
	adrp	x0, lC1@PAGE
	mov	w1, 404
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE21:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__is_empty
_khepri_storage_trie__is_empty:
LFB22:
	stp	x29, x30, [sp, -16]!
LCFI79:
	mov	w1, 10000
	mov	x29, sp
LCFI80:
	cmp	w0, w1
	bhi	L214
	cbz	w0, L218
	adrp	x1, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x2, x0, 2, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x0, x1, x0, lsl 3
	ldrb	w1, [x0, 36]
	cmp	w1, 1
	bhi	L214
	cbz	w1, L218
	ldr	w0, [x0, 32]
	cmp	w0, 255
	bhi	L216
	bl	_khepri_mpt__is_empty
	cmp	w0, 1
	bls	L213
L216:
	adrp	x0, lC1@PAGE
	mov	w1, 412
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L218:
	mov	w0, 1
L213:
	ldp	x29, x30, [sp], 16
LCFI81:
	ret
L214:
LCFI82:
	adrp	x0, lC1@PAGE
	mov	w1, 409
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__slot_count
_khepri_storage_trie__slot_count:
LFB23:
	stp	x29, x30, [sp, -16]!
LCFI83:
	mov	w1, 10000
	mov	x29, sp
LCFI84:
	cmp	w0, w1
	bhi	L223
	cbz	w0, L227
	adrp	x1, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x2, x0, 2, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x0, x1, x0, lsl 3
	ldrb	w1, [x0, 36]
	cmp	w1, 1
	bhi	L223
	cbz	w1, L227
	ldr	w0, [x0, 32]
	cmp	w0, 255
	bhi	L225
	bl	_khepri_mpt__node_count
	tbz	w0, #31, L220
L225:
	adrp	x0, lC1@PAGE
	mov	w1, 420
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L227:
	mov	w0, 0
L220:
	ldp	x29, x30, [sp], 16
LCFI85:
	ret
L223:
LCFI86:
	adrp	x0, lC1@PAGE
	mov	w1, 417
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__clear_storage
_khepri_storage_trie__clear_storage:
LFB24:
	stp	x29, x30, [sp, -32]!
LCFI87:
	mov	x29, sp
LCFI88:
	mov	w1, 10000
	str	x19, [sp, 16]
LCFI89:
	cmp	w0, w1
	bhi	L232
	cbz	w0, L236
	adrp	x1, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x2, x0, 2, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x19, x1, x0, lsl 3
	ldrb	w1, [x19, 36]
	cmp	w1, 1
	bhi	L232
	cbz	w1, L237
	ldr	w0, [x19, 32]
	cmp	w0, 255
	bhi	L240
	bl	_khepri_mpt__destroy_trie
	bl	_khepri_mpt__create_trie
	ubfx	x1, x0, 32, 8
	cmp	w1, 1
	bhi	L241
	cbz	w1, L238
	cmp	w0, 255
	bhi	L242
	mov	w1, 1
	str	w0, [x19, 32]
	mov	x0, 0
	bfi	x0, x1, 0, 8
	mov	w2, 0
	bfi	x0, x2, 8, 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI90:
	ret
	.p2align 2,,3
L236:
LCFI91:
	mov	w1, 0
	mov	x0, 0
	bfi	x0, x1, 0, 8
	mov	w2, 3
	bfi	x0, x2, 8, 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI92:
	ret
	.p2align 2,,3
L237:
LCFI93:
	mov	x0, 0
	mov	w2, 3
	bfi	x0, x1, 0, 8
	bfi	x0, x2, 8, 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI94:
	ret
	.p2align 2,,3
L238:
LCFI95:
	mov	x0, 0
	mov	w2, 4
	bfi	x0, x1, 0, 8
	bfi	x0, x2, 8, 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI96:
	ret
L232:
LCFI97:
	adrp	x0, lC1@PAGE
	mov	w1, 438
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L240:
	adrp	x0, lC1@PAGE
	mov	w1, 444
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L241:
	adrp	x0, lC1@PAGE
	mov	w1, 447
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L242:
	adrp	x0, lC1@PAGE
	mov	w1, 452
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__create_snapshot
_khepri_storage_trie__create_snapshot:
LFB25:
	stp	x29, x30, [sp, -16]!
LCFI98:
	mov	w1, 10000
	mov	x29, sp
LCFI99:
	cmp	w0, w1
	bhi	L246
	cbz	w0, L248
	adrp	x1, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x2, x0, 2, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x1, x1, x0, lsl 3
	ldrb	w2, [x1, 36]
	cmp	w2, 1
	bhi	L246
	cbz	w2, L249
	ldr	w0, [x1, 32]
	cmp	w0, 255
	bhi	L251
	bl	_khepri_mpt__create_snapshot
	ubfx	x2, x0, 32, 8
	uxtw	x0, w0
	orr	x0, x0, x2, lsl 32
	ldp	x29, x30, [sp], 16
LCFI100:
	ret
	.p2align 2,,3
L248:
LCFI101:
	mov	w2, 0
	uxtw	x0, w0
	orr	x0, x0, x2, lsl 32
	ldp	x29, x30, [sp], 16
LCFI102:
	ret
	.p2align 2,,3
L249:
LCFI103:
	mov	w0, 0
	uxtw	x0, w0
	orr	x0, x0, x2, lsl 32
	ldp	x29, x30, [sp], 16
LCFI104:
	ret
L246:
LCFI105:
	adrp	x0, lC1@PAGE
	mov	w1, 469
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L251:
	adrp	x0, lC1@PAGE
	mov	w1, 474
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__revert_snapshot
_khepri_storage_trie__revert_snapshot:
LFB26:
	stp	x29, x30, [sp, -16]!
LCFI106:
	mov	w2, 10000
	mov	x29, sp
LCFI107:
	cmp	w0, w2
	bhi	L255
	cbz	w0, L258
	adrp	x2, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x4, x0, 2, 32
	add	x0, x4, w0, sxtw
	add	x2, x2, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x2, x2, x0, lsl 3
	ldrb	w0, [x2, 36]
	cmp	w0, 1
	bhi	L255
	cbz	w0, L254
	ldr	w0, [x2, 32]
	cmp	w0, 255
	bhi	L257
	tbnz	w1, #31, L257
	bl	_khepri_mpt__restore_snapshot
	and	w0, w0, 255
L254:
	ldp	x29, x30, [sp], 16
LCFI108:
	ret
	.p2align 2,,3
L258:
LCFI109:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI110:
	ret
L255:
LCFI111:
	adrp	x0, lC1@PAGE
	mov	w1, 486
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L257:
	adrp	x0, lC1@PAGE
	mov	w1, 491
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__discard_snapshot
_khepri_storage_trie__discard_snapshot:
LFB27:
	stp	x29, x30, [sp, -16]!
LCFI112:
	mov	w3, 10000
	mov	x29, sp
LCFI113:
	cmp	w0, w3
	bhi	L266
	cbz	w0, L263
	adrp	x3, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x4, x0, 2, 32
	add	x0, x4, w0, sxtw
	add	x3, x3, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x0, x3, x0, lsl 3
	ldrb	w3, [x0, 36]
	cmp	w3, 1
	bhi	L266
	cbz	w3, L263
	ldr	w0, [x0, 32]
	cmp	w0, 255
	bhi	L268
	tbnz	w1, #31, L268
	ldp	x29, x30, [sp], 16
LCFI114:
	b	_khepri_mpt__discard_snapshot
	.p2align 2,,3
L263:
LCFI115:
	ldp	x29, x30, [sp], 16
LCFI116:
	ret
L266:
LCFI117:
	adrp	x0, lC1@PAGE
	mov	w1, 499
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L268:
	adrp	x0, lC1@PAGE
	mov	w1, 501
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE27:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__generate_storage_proof
_khepri_storage_trie__generate_storage_proof:
LFB28:
	stp	x29, x30, [sp, -96]!
LCFI118:
	mov	x29, sp
LCFI119:
	mov	x4, x2
	stp	x19, x20, [sp, 16]
LCFI120:
	mov	x19, 0
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI121:
	mov	w21, w0
	mov	x22, x1
	str	x23, [sp, 48]
LCFI122:
	stp	xzr, xzr, [x29, 64]
	stp	xzr, xzr, [x29, 80]
	.p2align 5,,15
L277:
	mov	x0, x4
	mov	x2, 532
	mov	w1, 0
	bl	_memset
	add	x3, x19, x19, lsl 4
	add	x4, x0, 536
	lsl	x3, x3, 2
	sub	x3, x3, x19
	add	x19, x19, 1
	add	x3, x20, x3, lsl 3
	str	wzr, [x3, 532]
	cmp	x19, 64
	bne	L277
	movi	v31.4s, 0
	mov	x0, 34816
	add	x1, x20, 32768
	add	x0, x20, x0
	mov	w3, 10000
	sub	x2, x0, #508
	sub	x0, x0, #440
	str	wzr, [x1, 1536]
	stp	q31, q31, [x2]
	stp	q31, q31, [x2, 32]
	str	wzr, [x2, 64]
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	str	wzr, [x0, 32]
	strb	wzr, [x1, 1644]
	cmp	w21, w3
	bhi	L280
	cbz	w21, L282
	adrp	x0, _khepri_storage_trie__storage_array@PAGE
	sbfiz	x1, x21, 2, 32
	add	x19, x1, w21, sxtw
	add	x0, x0, _khepri_storage_trie__storage_array@PAGEOFF;
	add	x19, x0, x19, lsl 3
	ldrb	w0, [x19, 36]
	cmp	w0, 1
	bhi	L280
	cbnz	w0, L288
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI123:
	ret
L282:
LCFI124:
	mov	w0, 0
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI125:
	ret
L288:
LCFI126:
	adrp	x21, lC0@PAGE
	add	x23, x29, 64
	add	x21, x21, lC0@PAGEOFF;
	mov	x0, x22
	mov	x2, x21
	mov	x1, x23
	bl	_khepri_storage_trie__u256_to_key.constprop.0
	ldr	w0, [x19, 32]
	cmp	w0, 255
	bhi	L289
	mov	x3, x20
	mov	x1, x23
	mov	x2, x21
	bl	_khepri_mpt__generate_proof
	and	w0, w0, 255
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI127:
	ret
L289:
LCFI128:
	adrp	x0, lC1@PAGE
	mov	w1, 532
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L280:
	adrp	x0, lC1@PAGE
	mov	w1, 527
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE28:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__verify_storage_proof
_khepri_storage_trie__verify_storage_proof:
LFB29:
	stp	x29, x30, [sp, -128]!
LCFI129:
	movi	v31.4s, 0
	mov	x29, sp
LCFI130:
	add	x1, x29, 128
LEHB5:
	stp	x19, x20, [sp, 16]
LCFI131:
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI132:
	mov	x21, x3
	str	x23, [sp, 48]
LCFI133:
	str	x1, [x29, 120]
	mov	x1, x2
	stp	q31, q31, [x3]
	bl	_khepri_mpt__verify_proof
	and	w19, w0, 255
	cmp	w19, 1
	bhi	L293
	cbz	w19, L298
	add	x22, x20, 32768
	ldrb	w0, [x22, 1644]
	cmp	w0, 1
	bhi	L293
	cbnz	w0, L306
L298:
	mov	w0, w19
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
LEHE5:
	ldp	x29, x30, [sp], 128
LCFI134:
	ret
	.p2align 2,,3
L306:
LCFI135:
	add	x23, x29, 96
	mov	x8, x23
LEHB6:
	bl	_system__secondary_stack__ss_mark
	ldr	w1, [x22, 1640]
	tbnz	w1, #31, L307
	mov	x0, 34376
	add	x0, x20, x0
	bl	_khepri_storage_trie__node_value_to_bytes
	ldp	w2, w3, [x1]
	cmp	w2, 0
	ccmp	w2, w3, 0, lt
	ble	L308
	add	x20, x29, 64
	mov	x8, x20
	bl	_khepri_storage_trie__bytes_to_u256
	ld1	{v30.16b - v31.16b}, [x20]
LEHE6:
	mov	x0, x23
	st1	{v30.16b - v31.16b}, [x21]
LEHB7:
	bl	_system__secondary_stack__ss_release
	mov	w0, w19
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 128
LCFI136:
	ret
L293:
LCFI137:
	adrp	x0, lC1@PAGE
	mov	w1, 548
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE7:
L308:
	adrp	x0, lC1@PAGE
	mov	w1, 551
	add	x0, x0, lC1@PAGEOFF;
LEHB8:
	bl	___gnat_rcheck_CE_Range_Check
L307:
	adrp	x0, lC1@PAGE
	mov	w1, 551
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE8:
L297:
	mov	x19, x0
	mov	x0, x23
LEHB9:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE9:
LFE29:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table1:
LLSDA29:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE29-LLSDACSB29
LLSDACSB29:
	.uleb128 LEHB5-LFB29
	.uleb128 LEHE5-LEHB5
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB6-LFB29
	.uleb128 LEHE6-LEHB6
	.uleb128 L297-LFB29
	.uleb128 0
	.uleb128 LEHB7-LFB29
	.uleb128 LEHE7-LEHB7
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB8-LFB29
	.uleb128 LEHE8-LEHB8
	.uleb128 L297-LFB29
	.uleb128 0
	.uleb128 LEHB9-LFB29
	.uleb128 LEHE9-LEHB9
	.uleb128 0
	.uleb128 0
LLSDACSE29:
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie__find_storage
_khepri_storage_trie__find_storage:
LFB31:
	stp	x29, x30, [sp, -16]!
LCFI138:
	mov	x29, sp
LCFI139:
	bl	_khepri_storage_trie__find_contract_entry
	mov	w1, 9999
	cmp	w0, w1
	bgt	L311
	adrp	x1, _khepri_storage_trie__contract_map@PAGE
	sbfiz	x2, x0, 2, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_storage_trie__contract_map@PAGEOFF;
	mov	w2, 10000
	add	x0, x1, x0, lsl 3
	ldr	w0, [x0, 32]
	cmp	w0, w2
	bhi	L313
	ldp	x29, x30, [sp], 16
LCFI140:
	ret
	.p2align 2,,3
L311:
LCFI141:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI142:
	ret
L313:
LCFI143:
	adrp	x0, lC1@PAGE
	mov	w1, 566
	add	x0, x0, lC1@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE31:
	.align	2
	.p2align 5,,15
	.globl _khepri_storage_trie___elabb
_khepri_storage_trie___elabb:
LFB0:
	adrp	x0, _khepri_storage_trie__storage_array@PAGE
	mov	x2, 6824
	stp	x29, x30, [sp, -16]!
LCFI144:
	movk	x2, 0x6, lsl 16
	mov	x29, sp
LCFI145:
	mov	w1, 0
	add	x0, x0, _khepri_storage_trie__storage_array@PAGEOFF;
	bl	_memset
	mov	x1, 6824
	adrp	x0, _khepri_storage_trie__contract_map@PAGE
	ldp	x29, x30, [sp], 16
LCFI146:
	movk	x1, 0x6, lsl 16
	add	x0, x0, _khepri_storage_trie__contract_map@PAGEOFF;
	sub	x2, x1, #40
	mov	w1, 0
	b	_memset
LFE0:
	.const
	.align	3
_storage_errorG.2:
	.byte	0
	.byte	4
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.space 1
	.zerofill __DATA,__bss,_khepri_storage_trie__contract_map,400000,4
	.zerofill __DATA,__bss,_khepri_storage_trie__storage_array,400040,2
	.globl _khepri_storage_trie__storage_errorN
	.const
	.align	3
_khepri_storage_trie__storage_errorN:
	.byte	1
	.byte	11
	.byte	26
	.byte	36
	.byte	49
	.byte	65
	.space 2
	.globl _khepri_storage_trie__storage_errorS
	.align	3
_khepri_storage_trie__storage_errorS:
	.ascii "ERROR_NONEERROR_NOT_FOUNDERROR_FULLERROR_INVALIDERROR_TRIE_ERROR"
	.globl _khepri_storage_trie__null_storage
	.align	2
_khepri_storage_trie__null_storage:
	.space 4
	.globl _khepri_storage_trie_E
	.data
	.align	1
_khepri_storage_trie_E:
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
	.quad	LFB12-.
	.set L$set$2,LFE12-LFB12
	.quad L$set$2
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$3,LCFI0-LFB12
	.long L$set$3
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
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
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
	.byte	0xb
	.byte	0x4
	.set L$set$8,LCFI5-LCFI4
	.long L$set$8
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$9,LEFDE3-LASFDE3
	.long L$set$9
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB11-.
	.set L$set$10,LFE11-LFB11
	.quad L$set$10
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$11,LCFI6-LFB11
	.long L$set$11
	.byte	0xe
	.uleb128 0xd0
	.byte	0x9d
	.uleb128 0x1a
	.byte	0x9e
	.uleb128 0x19
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0x93
	.uleb128 0x18
	.byte	0x94
	.uleb128 0x17
	.byte	0x95
	.uleb128 0x16
	.byte	0x96
	.uleb128 0x15
	.byte	0x97
	.uleb128 0x14
	.byte	0x98
	.uleb128 0x13
	.byte	0x99
	.uleb128 0x12
	.byte	0x9a
	.uleb128 0x11
	.byte	0x9b
	.uleb128 0x10
	.byte	0x9c
	.uleb128 0xf
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
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
	.set L$set$15,LCFI10-LCFI9
	.long L$set$15
	.byte	0xb
	.byte	0x4
	.set L$set$16,LCFI11-LCFI10
	.long L$set$16
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
	.set L$set$17,LCFI12-LCFI11
	.long L$set$17
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$18,LEFDE5-LASFDE5
	.long L$set$18
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB8-.
	.set L$set$19,LFE8-LFB8
	.quad L$set$19
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$20,LCFI13-LFB8
	.long L$set$20
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$22,LEFDE7-LASFDE7
	.long L$set$22
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB73-.
	.set L$set$23,LFE73-LFB73
	.quad L$set$23
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$24,LCFI15-LFB73
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
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$28,LEFDE9-LASFDE9
	.long L$set$28
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB2-.
	.set L$set$29,LFE2-LFB2
	.quad L$set$29
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$30,LEFDE11-LASFDE11
	.long L$set$30
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB3-.
	.set L$set$31,LFE3-LFB3
	.quad L$set$31
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$32,LEFDE13-LASFDE13
	.long L$set$32
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB4-.
	.set L$set$33,LFE4-LFB4
	.quad L$set$33
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$34,LEFDE15-LASFDE15
	.long L$set$34
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB72-.
	.set L$set$35,LFE72-LFB72
	.quad L$set$35
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$36,LEFDE17-LASFDE17
	.long L$set$36
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB6-.
	.set L$set$37,LFE6-LFB6
	.quad L$set$37
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$38,LEFDE19-LASFDE19
	.long L$set$38
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB13-.
	.set L$set$39,LFE13-LFB13
	.quad L$set$39
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$40,LCFI19-LFB13
	.long L$set$40
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$41,LCFI20-LCFI19
	.long L$set$41
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$42,LCFI21-LCFI20
	.long L$set$42
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$43,LCFI22-LCFI21
	.long L$set$43
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$44,LCFI23-LCFI22
	.long L$set$44
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
	.set L$set$45,LCFI24-LCFI23
	.long L$set$45
	.byte	0xb
	.byte	0x4
	.set L$set$46,LCFI25-LCFI24
	.long L$set$46
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
	.set L$set$47,LCFI26-LCFI25
	.long L$set$47
	.byte	0xb
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$48,LEFDE21-LASFDE21
	.long L$set$48
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB14-.
	.set L$set$49,LFE14-LFB14
	.quad L$set$49
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$50,LCFI27-LFB14
	.long L$set$50
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$51,LCFI28-LCFI27
	.long L$set$51
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$52,LCFI29-LCFI28
	.long L$set$52
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$53,LCFI30-LCFI29
	.long L$set$53
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$54,LCFI31-LCFI30
	.long L$set$54
	.byte	0x97
	.uleb128 0x6
	.byte	0x4
	.set L$set$55,LCFI32-LCFI31
	.long L$set$55
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
	.set L$set$56,LCFI33-LCFI32
	.long L$set$56
	.byte	0xb
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$57,LEFDE23-LASFDE23
	.long L$set$57
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB15-.
	.set L$set$58,LFE15-LFB15
	.quad L$set$58
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$59,LCFI34-LFB15
	.long L$set$59
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$60,LCFI35-LCFI34
	.long L$set$60
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$61,LCFI36-LCFI35
	.long L$set$61
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$62,LCFI37-LCFI36
	.long L$set$62
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$63,LCFI38-LCFI37
	.long L$set$63
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
	.set L$set$64,LCFI39-LCFI38
	.long L$set$64
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$65,LEFDE25-LASFDE25
	.long L$set$65
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB16-.
	.set L$set$66,LFE16-LFB16
	.quad L$set$66
	.uleb128 0x8
	.quad	LLSDA16-.
	.byte	0x4
	.set L$set$67,LCFI40-LFB16
	.long L$set$67
	.byte	0xe
	.uleb128 0xc0
	.byte	0x9d
	.uleb128 0x18
	.byte	0x9e
	.uleb128 0x17
	.byte	0x4
	.set L$set$68,LCFI41-LCFI40
	.long L$set$68
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$69,LCFI42-LCFI41
	.long L$set$69
	.byte	0x93
	.uleb128 0x16
	.byte	0x94
	.uleb128 0x15
	.byte	0x4
	.set L$set$70,LCFI43-LCFI42
	.long L$set$70
	.byte	0x95
	.uleb128 0x14
	.byte	0x96
	.uleb128 0x13
	.byte	0x4
	.set L$set$71,LCFI44-LCFI43
	.long L$set$71
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
	.set L$set$72,LCFI45-LCFI44
	.long L$set$72
	.byte	0xb
	.byte	0x4
	.set L$set$73,LCFI46-LCFI45
	.long L$set$73
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
	.set L$set$74,LCFI47-LCFI46
	.long L$set$74
	.byte	0xb
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$75,LEFDE27-LASFDE27
	.long L$set$75
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB18-.
	.set L$set$76,LFE18-LFB18
	.quad L$set$76
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$77,LCFI48-LFB18
	.long L$set$77
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$78,LCFI49-LCFI48
	.long L$set$78
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$79,LCFI50-LCFI49
	.long L$set$79
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x97
	.uleb128 0xa
	.byte	0x4
	.set L$set$80,LCFI51-LCFI50
	.long L$set$80
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
	.set L$set$81,LCFI52-LCFI51
	.long L$set$81
	.byte	0xb
	.byte	0x4
	.set L$set$82,LCFI53-LCFI52
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
	.set L$set$83,LCFI54-LCFI53
	.long L$set$83
	.byte	0xb
	.byte	0x4
	.set L$set$84,LCFI55-LCFI54
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
	.set L$set$85,LCFI56-LCFI55
	.long L$set$85
	.byte	0xb
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$86,LEFDE29-LASFDE29
	.long L$set$86
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB19-.
	.set L$set$87,LFE19-LFB19
	.quad L$set$87
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$88,LCFI57-LFB19
	.long L$set$88
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$89,LCFI58-LCFI57
	.long L$set$89
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$90,LCFI59-LCFI58
	.long L$set$90
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x95
	.uleb128 0x6
	.byte	0x4
	.set L$set$91,LCFI60-LCFI59
	.long L$set$91
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
	.set L$set$92,LCFI61-LCFI60
	.long L$set$92
	.byte	0xb
	.byte	0x4
	.set L$set$93,LCFI62-LCFI61
	.long L$set$93
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
	.set L$set$94,LCFI63-LCFI62
	.long L$set$94
	.byte	0xb
	.byte	0x4
	.set L$set$95,LCFI64-LCFI63
	.long L$set$95
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
	.set L$set$96,LCFI65-LCFI64
	.long L$set$96
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$97,LEFDE31-LASFDE31
	.long L$set$97
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB20-.
	.set L$set$98,LFE20-LFB20
	.quad L$set$98
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$99,LCFI66-LFB20
	.long L$set$99
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$100,LCFI67-LCFI66
	.long L$set$100
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$101,LCFI68-LCFI67
	.long L$set$101
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x95
	.uleb128 0x6
	.byte	0x4
	.set L$set$102,LCFI69-LCFI68
	.long L$set$102
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
	.set L$set$103,LCFI70-LCFI69
	.long L$set$103
	.byte	0xb
	.byte	0x4
	.set L$set$104,LCFI71-LCFI70
	.long L$set$104
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
	.set L$set$105,LCFI72-LCFI71
	.long L$set$105
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$106,LEFDE33-LASFDE33
	.long L$set$106
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB21-.
	.set L$set$107,LFE21-LFB21
	.quad L$set$107
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$108,LCFI73-LFB21
	.long L$set$108
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$109,LCFI74-LCFI73
	.long L$set$109
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$110,LCFI75-LCFI74
	.long L$set$110
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$111,LCFI76-LCFI75
	.long L$set$111
	.byte	0xb
	.byte	0x4
	.set L$set$112,LCFI77-LCFI76
	.long L$set$112
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$113,LCFI78-LCFI77
	.long L$set$113
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$114,LEFDE35-LASFDE35
	.long L$set$114
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB22-.
	.set L$set$115,LFE22-LFB22
	.quad L$set$115
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$116,LCFI79-LFB22
	.long L$set$116
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$117,LCFI80-LCFI79
	.long L$set$117
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$118,LCFI81-LCFI80
	.long L$set$118
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$119,LCFI82-LCFI81
	.long L$set$119
	.byte	0xb
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$120,LEFDE37-LASFDE37
	.long L$set$120
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB23-.
	.set L$set$121,LFE23-LFB23
	.quad L$set$121
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$122,LCFI83-LFB23
	.long L$set$122
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$123,LCFI84-LCFI83
	.long L$set$123
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$124,LCFI85-LCFI84
	.long L$set$124
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI86-LCFI85
	.long L$set$125
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$126,LEFDE39-LASFDE39
	.long L$set$126
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB24-.
	.set L$set$127,LFE24-LFB24
	.quad L$set$127
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$128,LCFI87-LFB24
	.long L$set$128
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$129,LCFI88-LCFI87
	.long L$set$129
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$130,LCFI89-LCFI88
	.long L$set$130
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$131,LCFI90-LCFI89
	.long L$set$131
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$132,LCFI91-LCFI90
	.long L$set$132
	.byte	0xb
	.byte	0x4
	.set L$set$133,LCFI92-LCFI91
	.long L$set$133
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$134,LCFI93-LCFI92
	.long L$set$134
	.byte	0xb
	.byte	0x4
	.set L$set$135,LCFI94-LCFI93
	.long L$set$135
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$136,LCFI95-LCFI94
	.long L$set$136
	.byte	0xb
	.byte	0x4
	.set L$set$137,LCFI96-LCFI95
	.long L$set$137
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$138,LCFI97-LCFI96
	.long L$set$138
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$139,LEFDE41-LASFDE41
	.long L$set$139
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB25-.
	.set L$set$140,LFE25-LFB25
	.quad L$set$140
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$141,LCFI98-LFB25
	.long L$set$141
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$142,LCFI99-LCFI98
	.long L$set$142
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$143,LCFI100-LCFI99
	.long L$set$143
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$144,LCFI101-LCFI100
	.long L$set$144
	.byte	0xb
	.byte	0x4
	.set L$set$145,LCFI102-LCFI101
	.long L$set$145
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$146,LCFI103-LCFI102
	.long L$set$146
	.byte	0xb
	.byte	0x4
	.set L$set$147,LCFI104-LCFI103
	.long L$set$147
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$148,LCFI105-LCFI104
	.long L$set$148
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$149,LEFDE43-LASFDE43
	.long L$set$149
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB26-.
	.set L$set$150,LFE26-LFB26
	.quad L$set$150
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$151,LCFI106-LFB26
	.long L$set$151
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$152,LCFI107-LCFI106
	.long L$set$152
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$153,LCFI108-LCFI107
	.long L$set$153
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$154,LCFI109-LCFI108
	.long L$set$154
	.byte	0xb
	.byte	0x4
	.set L$set$155,LCFI110-LCFI109
	.long L$set$155
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$156,LCFI111-LCFI110
	.long L$set$156
	.byte	0xb
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$157,LEFDE45-LASFDE45
	.long L$set$157
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB27-.
	.set L$set$158,LFE27-LFB27
	.quad L$set$158
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$159,LCFI112-LFB27
	.long L$set$159
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$160,LCFI113-LCFI112
	.long L$set$160
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$161,LCFI114-LCFI113
	.long L$set$161
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$162,LCFI115-LCFI114
	.long L$set$162
	.byte	0xb
	.byte	0x4
	.set L$set$163,LCFI116-LCFI115
	.long L$set$163
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$164,LCFI117-LCFI116
	.long L$set$164
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$165,LEFDE47-LASFDE47
	.long L$set$165
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB28-.
	.set L$set$166,LFE28-LFB28
	.quad L$set$166
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$167,LCFI118-LFB28
	.long L$set$167
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$168,LCFI119-LCFI118
	.long L$set$168
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$169,LCFI120-LCFI119
	.long L$set$169
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$170,LCFI121-LCFI120
	.long L$set$170
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$171,LCFI122-LCFI121
	.long L$set$171
	.byte	0x97
	.uleb128 0x6
	.byte	0x4
	.set L$set$172,LCFI123-LCFI122
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
	.set L$set$173,LCFI124-LCFI123
	.long L$set$173
	.byte	0xb
	.byte	0x4
	.set L$set$174,LCFI125-LCFI124
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
	.set L$set$175,LCFI126-LCFI125
	.long L$set$175
	.byte	0xb
	.byte	0x4
	.set L$set$176,LCFI127-LCFI126
	.long L$set$176
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
	.set L$set$177,LCFI128-LCFI127
	.long L$set$177
	.byte	0xb
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$178,LEFDE49-LASFDE49
	.long L$set$178
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB29-.
	.set L$set$179,LFE29-LFB29
	.quad L$set$179
	.uleb128 0x8
	.quad	LLSDA29-.
	.byte	0x4
	.set L$set$180,LCFI129-LFB29
	.long L$set$180
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$181,LCFI130-LCFI129
	.long L$set$181
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$182,LCFI131-LCFI130
	.long L$set$182
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x4
	.set L$set$183,LCFI132-LCFI131
	.long L$set$183
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x4
	.set L$set$184,LCFI133-LCFI132
	.long L$set$184
	.byte	0x97
	.uleb128 0xa
	.byte	0x4
	.set L$set$185,LCFI134-LCFI133
	.long L$set$185
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
	.set L$set$186,LCFI135-LCFI134
	.long L$set$186
	.byte	0xb
	.byte	0x4
	.set L$set$187,LCFI136-LCFI135
	.long L$set$187
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
	.set L$set$188,LCFI137-LCFI136
	.long L$set$188
	.byte	0xb
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$189,LEFDE51-LASFDE51
	.long L$set$189
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB31-.
	.set L$set$190,LFE31-LFB31
	.quad L$set$190
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$191,LCFI138-LFB31
	.long L$set$191
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$192,LCFI139-LCFI138
	.long L$set$192
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$193,LCFI140-LCFI139
	.long L$set$193
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$194,LCFI141-LCFI140
	.long L$set$194
	.byte	0xb
	.byte	0x4
	.set L$set$195,LCFI142-LCFI141
	.long L$set$195
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$196,LCFI143-LCFI142
	.long L$set$196
	.byte	0xb
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$197,LEFDE53-LASFDE53
	.long L$set$197
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB0-.
	.set L$set$198,LFE0-LFB0
	.quad L$set$198
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$199,LCFI144-LFB0
	.long L$set$199
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$200,LCFI145-LCFI144
	.long L$set$200
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$201,LCFI146-LCFI145
	.long L$set$201
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE53:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
