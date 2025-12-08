	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__syscall_returnIP
_aegis_syscall__syscall_returnIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__Tsyscall_argsBIP
_aegis_syscall__Tsyscall_argsBIP:
LFB3:
	ret
LFE3:
	.const
	.align	3
lC10:
	.ascii "aegis_syscall.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_sload
_aegis_syscall__sys_handle_sload:
LFB5:
	stp	x29, x30, [sp, -160]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
LCFI2:
	add	x20, x29, 128
	mov	x19, x2
	mov	x8, x20
	stp	x21, x22, [sp, 32]
LCFI3:
	add	x22, x29, 96
	mov	x21, x0
	ld1	{v30.16b - v31.16b}, [x1]
	str	x23, [sp, 48]
LCFI4:
	add	x23, x29, 64
	st1	{v30.16b - v31.16b}, [x22]
	bl	_aegis_execution__get_address
	mov	x2, x22
	mov	x1, x20
	mov	x0, x21
	mov	x3, x23
	bl	_aegis_execution__storage_load
	cmp	w0, 1
	bhi	L9
	cbnz	w0, L10
	adrp	x0, lC3@PAGE
	add	x0, x0, lC3@PAGEOFF;
	ldr	x1, [x0, 48]
	ldp	q30, q29, [x0]
	ldr	q31, [x0, 32]
	str	x1, [x19, 48]
	str	q31, [x19, 32]
	stp	q30, q29, [x19]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 160
LCFI5:
	ret
	.p2align 2,,3
L10:
LCFI6:
	ld1	{v30.16b - v31.16b}, [x23]
	mov	w2, 1
	mov	x1, 200
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 160
LCFI7:
	ret
L9:
LCFI8:
	adrp	x0, lC10@PAGE
	mov	w1, 168
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE5:
	.const
	.align	3
lC3:
	.byte	0
	.space 7
	.xword	200
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.word	3
	.space 4
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_sstore
_aegis_syscall__sys_handle_sstore:
LFB6:
	stp	x29, x30, [sp, -160]!
LCFI9:
	mov	x29, sp
LCFI10:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI11:
	add	x21, x29, 64
	add	x22, x29, 96
	ld1	{v28.16b - v29.16b}, [x1]
	ld1	{v30.16b - v31.16b}, [x2]
	ldrb	w1, [x0, 160]
	str	x23, [sp, 48]
LCFI12:
	st1	{v28.16b - v29.16b}, [x21]
	st1	{v30.16b - v31.16b}, [x22]
	cmp	w1, 3
	bhi	L20
	mov	x19, x3
	ldrb	w3, [x0, 357]
	mov	x20, x0
	cmp	w3, 1
	bhi	L21
	cmp	w3, 0
	ccmp	w1, 1, 4, ne
	bne	L14
	movi	v31.4s, 0
	mov	w0, 7
	strb	wzr, [x19]
	str	xzr, [x19, 8]
	str	w0, [x19, 48]
	stp	q31, q31, [x19, 16]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 160
LCFI13:
	ret
	.p2align 2,,3
L14:
LCFI14:
	add	x23, x29, 128
	mov	x8, x23
	bl	_aegis_execution__get_address
	mov	x3, x22
	mov	x2, x21
	mov	x1, x23
	mov	x0, x20
	bl	_aegis_execution__storage_store
	cmp	w0, 1
	bhi	L22
	cbz	w0, L17
	adrp	x0, lC4@PAGE
	add	x0, x0, lC4@PAGEOFF;
L19:
	ldp	q30, q29, [x0]
	ldr	q31, [x0, 32]
	ldr	x1, [x0, 48]
	str	q31, [x19, 32]
	stp	q30, q29, [x19]
	str	x1, [x19, 48]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 160
LCFI15:
	ret
	.p2align 2,,3
L17:
LCFI16:
	adrp	x0, lC5@PAGE
	add	x0, x0, lC5@PAGEOFF;
	b	L19
L21:
	adrp	x0, lC10@PAGE
	mov	w1, 663
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L20:
	adrp	x0, lC10@PAGE
	mov	w1, 662
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L22:
	adrp	x0, lC10@PAGE
	mov	w1, 197
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE6:
	.const
	.align	3
lC4:
	.byte	1
	.space 7
	.xword	20000
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.word	0
	.space 4
	.align	3
lC5:
	.byte	0
	.space 7
	.xword	20000
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.word	3
	.space 4
	.text
	.const
	.align	3
lC11:
	.ascii "aegis_syscall.ads"
	.space 1
	.align	3
lC12:
	.ascii "failed precondition from aegis_syscall.ads:140"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_sha3
_aegis_syscall__sys_handle_sha3:
LFB7:
	mov	x12, 32848
	sub	sp, sp, x12
LCFI17:
	stp	x29, x30, [sp]
LCFI18:
	mov	x29, sp
LCFI19:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI20:
	tbnz	w2, #31, L30
	mov	w20, w2
	cmp	w2, 32768
	bgt	L31
	mov	x19, x3
	add	x3, x29, 80
	mov	x21, x0
	mov	x2, 32768
	mov	x0, x3
	mov	w1, 0
	bl	_memset
	mov	x1, x0
	add	x3, x29, 48
	mov	x0, x21
	mov	w2, w20
	bl	_aegis_crypto_api__sha3_256_hash
	cmp	w0, 4
	bhi	L32
	cbz	w0, L33
	mov	w2, 0
	mov	x0, 0
	mov	w1, 13
L27:
	movi	v31.4s, 0
	strb	w2, [x19]
	str	x0, [x19, 8]
	str	w1, [x19, 48]
	stp	q31, q31, [x19, 16]
	mov	x12, 32848
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, x12
LCFI21:
	ret
	.p2align 2,,3
L33:
LCFI22:
	mov	w0, w20
	bl	_aegis_gas__gas_hash
	mov	w2, 1
	mov	w1, 0
	b	L27
L32:
	adrp	x0, lC10@PAGE
	mov	w1, 220
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L31:
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L30:
	adrp	x0, lC11@PAGE
	mov	w1, 140
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE7:
	.const
	.align	2
lC0:
	.word	1
	.word	46
	.text
	.const
	.align	3
lC13:
	.ascii "failed precondition from aegis_syscall.ads:150"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_keccak256
_aegis_syscall__sys_handle_keccak256:
LFB8:
	mov	x12, 32848
	sub	sp, sp, x12
LCFI23:
	stp	x29, x30, [sp]
LCFI24:
	mov	x29, sp
LCFI25:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI26:
	tbnz	w2, #31, L41
	mov	w20, w2
	cmp	w2, 32768
	bgt	L42
	mov	x19, x3
	add	x3, x29, 80
	mov	x21, x0
	mov	x2, 32768
	mov	x0, x3
	mov	w1, 0
	bl	_memset
	mov	x1, x0
	add	x3, x29, 48
	mov	x0, x21
	mov	w2, w20
	bl	_aegis_crypto_api__keccak256_hash
	cmp	w0, 4
	bhi	L43
	cbz	w0, L44
	mov	w2, 0
	mov	x0, 0
	mov	w1, 13
L38:
	movi	v31.4s, 0
	strb	w2, [x19]
	str	x0, [x19, 8]
	str	w1, [x19, 48]
	stp	q31, q31, [x19, 16]
	mov	x12, 32848
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, x12
LCFI27:
	ret
	.p2align 2,,3
L44:
LCFI28:
	mov	w0, w20
	bl	_aegis_gas__gas_hash
	mov	w2, 1
	mov	w1, 0
	b	L38
L43:
	adrp	x0, lC10@PAGE
	mov	w1, 239
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L42:
	adrp	x0, lC13@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L41:
	adrp	x0, lC11@PAGE
	mov	w1, 150
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE8:
	.const
	.align	3
lC14:
	.ascii "failed precondition from aegis_syscall.ads:162"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_mldsa_verify
_aegis_syscall__sys_handle_mldsa_verify:
LFB9:
	mov	x12, 40064
	sub	sp, sp, x12
LCFI29:
	stp	x29, x30, [sp]
LCFI30:
	mov	x29, sp
LCFI31:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI32:
	tbnz	w2, #31, L55
	mov	w20, w2
	cmp	w2, 32768
	bgt	L56
	mov	x23, x0
	mov	x0, 7296
	add	x21, x29, x0
	mov	x2, 32768
	mov	w1, 0
	mov	x0, x21
	mov	x19, x5
	add	x22, x29, 2664
	bl	_memset
	mov	x2, 4627
	mov	w1, 0
	mov	x0, x22
	bl	_memset
	add	x4, x29, 72
	mov	x2, 2592
	mov	x0, x4
	mov	w1, 0
	bl	_memset
	mov	x4, x0
	mov	x1, x21
	mov	x3, x22
	mov	w2, w20
	mov	x0, x23
	bl	_aegis_crypto_api__mldsa87_verify
	ubfx	w1, w0, 8, 8
	and	w0, w0, 255
	cmp	w1, 4
	bhi	L57
	cbnz	w1, L49
	cmp	w0, 1
	bhi	L58
	cbz	w0, L51
	adrp	x0, lC6@PAGE
	add	x0, x0, lC6@PAGEOFF;
L54:
	ldp	q30, q29, [x0]
	ldr	q31, [x0, 32]
	ldr	x1, [x0, 48]
	str	q31, [x19, 32]
	stp	q30, q29, [x19]
	str	x1, [x19, 48]
	mov	x12, 40064
	ldr	x23, [sp, 48]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, x12
LCFI33:
	ret
	.p2align 2,,3
L49:
LCFI34:
	movi	v31.4s, 0
	mov	w0, 13
	strb	wzr, [x19]
	str	xzr, [x19, 8]
	str	w0, [x19, 48]
	stp	q31, q31, [x19, 16]
	mov	x12, 40064
	ldr	x23, [sp, 48]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, x12
LCFI35:
	ret
	.p2align 2,,3
L51:
LCFI36:
	adrp	x0, lC7@PAGE
	add	x0, x0, lC7@PAGEOFF;
	b	L54
L57:
	adrp	x0, lC10@PAGE
	mov	w1, 263
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L56:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L55:
	adrp	x0, lC11@PAGE
	mov	w1, 162
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L58:
	adrp	x0, lC10@PAGE
	mov	w1, 264
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE9:
	.const
	.align	3
lC6:
	.byte	1
	.space 7
	.xword	30000
	.xword	1
	.xword	0
	.xword	0
	.xword	0
	.word	0
	.space 4
	.align	3
lC7:
	.byte	1
	.space 7
	.xword	30000
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.word	0
	.space 4
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_mlkem_decaps
_aegis_syscall__sys_handle_mlkem_decaps:
LFB10:
	mov	x12, 4816
	mov	x2, 1568
	sub	sp, sp, x12
LCFI37:
	mov	w1, 0
	stp	x29, x30, [sp]
LCFI38:
	mov	x29, sp
LCFI39:
	stp	x19, x20, [sp, 16]
LCFI40:
	add	x20, x29, 80
	mov	x19, x3
	stp	x21, x22, [sp, 32]
LCFI41:
	mov	x21, x0
	mov	x0, x20
	add	x22, x29, 48
	bl	_memset
	add	x4, x29, 1648
	mov	x2, 3168
	mov	w1, 0
	mov	x0, x4
	bl	_memset
	mov	x2, x0
	mov	x1, x20
	mov	x0, x21
	mov	x3, x22
	bl	_aegis_crypto_api__mlkem1024_decaps
	cmp	w0, 4
	bhi	L64
	cbz	w0, L65
	movi	v31.4s, 0
	mov	w0, 13
	strb	wzr, [x19]
	str	xzr, [x19, 8]
	str	w0, [x19, 48]
	stp	q31, q31, [x19, 16]
	mov	x12, 4816
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, x12
LCFI42:
	ret
	.p2align 2,,3
L65:
LCFI43:
	adrp	x1, lC8@PAGE
	mov	x0, x22
	add	x1, x1, lC8@PAGEOFF;
	ldr	x2, [x1, 48]
	ldp	q30, q29, [x1]
	ldr	q31, [x1, 32]
	str	x2, [x19, 48]
	str	q31, [x19, 32]
	stp	q30, q29, [x19]
	bl	_aegis_crypto_api__zeroize_secret
	mov	x12, 4816
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	add	sp, sp, x12
LCFI44:
	ret
L64:
LCFI45:
	adrp	x0, lC10@PAGE
	mov	w1, 287
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE10:
	.const
	.align	3
lC8:
	.byte	1
	.space 7
	.xword	25000
	.xword	1
	.xword	0
	.xword	0
	.xword	0
	.word	0
	.space 4
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_caller
_aegis_syscall__sys_handle_caller:
LFB11:
	stp	x29, x30, [sp, -112]!
LCFI46:
	mov	x29, sp
LCFI47:
	str	x21, [sp, 32]
LCFI48:
	add	x21, x29, 80
	mov	x8, x21
	stp	x19, x20, [sp, 16]
LCFI49:
	mov	x19, x1
	add	x20, x29, 48
	bl	_aegis_execution__get_caller
	mov	x0, x21
	mov	x8, x20
	bl	_aegis_u256__address_to_u256
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 2
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI50:
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_address
_aegis_syscall__sys_handle_address:
LFB12:
	stp	x29, x30, [sp, -112]!
LCFI51:
	mov	x29, sp
LCFI52:
	str	x21, [sp, 32]
LCFI53:
	add	x21, x29, 80
	mov	x8, x21
	stp	x19, x20, [sp, 16]
LCFI54:
	mov	x19, x1
	add	x20, x29, 48
	bl	_aegis_execution__get_address
	mov	x0, x21
	mov	x8, x20
	bl	_aegis_u256__address_to_u256
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 2
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI55:
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_callvalue
_aegis_syscall__sys_handle_callvalue:
LFB13:
	stp	x29, x30, [sp, -64]!
LCFI56:
	mov	x29, sp
LCFI57:
	stp	x19, x20, [sp, 16]
LCFI58:
	add	x20, x29, 32
	mov	x19, x1
	mov	x8, x20
	bl	_aegis_execution__get_call_value
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 2
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI59:
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_blocknumber
_aegis_syscall__sys_handle_blocknumber:
LFB14:
	stp	x29, x30, [sp, -64]!
LCFI60:
	mov	x29, sp
LCFI61:
	stp	x19, x20, [sp, 16]
LCFI62:
	add	x20, x29, 32
	mov	x19, x1
	mov	x8, x20
	bl	_aegis_execution__get_block_number
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 2
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI63:
	ret
LFE14:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_timestamp
_aegis_syscall__sys_handle_timestamp:
LFB15:
	stp	x29, x30, [sp, -64]!
LCFI64:
	mov	x29, sp
LCFI65:
	stp	x19, x20, [sp, 16]
LCFI66:
	add	x20, x29, 32
	mov	x19, x1
	mov	x8, x20
	bl	_aegis_execution__get_timestamp
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 2
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI67:
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_chainid
_aegis_syscall__sys_handle_chainid:
LFB16:
	stp	x29, x30, [sp, -64]!
LCFI68:
	mov	x29, sp
LCFI69:
	stp	x19, x20, [sp, 16]
LCFI70:
	add	x20, x29, 32
	mov	x19, x1
	mov	x8, x20
	bl	_aegis_execution__get_chain_id
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 2
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI71:
	ret
LFE16:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_gasprice
_aegis_syscall__sys_handle_gasprice:
LFB17:
	add	x0, x0, 32
	mov	w3, 1
	mov	x2, 2
	ld1	{v30.16b - v31.16b}, [x0]
	add	x0, x1, 16
	strb	w3, [x1]
	str	x2, [x1, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x1, 48]
	ret
LFE17:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_gasremaining
_aegis_syscall__sys_handle_gasremaining:
LFB18:
	stp	x29, x30, [sp, -64]!
LCFI72:
	mov	x29, sp
LCFI73:
	stp	x19, x20, [sp, 16]
LCFI74:
	mov	x19, x1
	bl	_aegis_execution__gas_remaining
	tbnz	x0, #63, L82
	add	x20, x29, 32
	mov	x8, x20
	bl	_aegis_u256__gas_to_u256
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 2
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI75:
	ret
L82:
LCFI76:
	adrp	x0, lC10@PAGE
	mov	w1, 401
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE18:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_balance
_aegis_syscall__sys_handle_balance:
LFB19:
	stp	x29, x30, [sp, -64]!
LCFI77:
	mov	x29, sp
LCFI78:
	stp	x19, x20, [sp, 16]
LCFI79:
	add	x20, x29, 32
	mov	x19, x2
	mov	x8, x20
	bl	_aegis_execution__get_balance
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 400
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 64
LCFI80:
	ret
LFE19:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_selfbalance
_aegis_syscall__sys_handle_selfbalance:
LFB20:
	stp	x29, x30, [sp, -112]!
LCFI81:
	mov	x29, sp
LCFI82:
	stp	x21, x22, [sp, 32]
LCFI83:
	add	x21, x29, 80
	mov	x22, x0
	mov	x8, x21
	stp	x19, x20, [sp, 16]
LCFI84:
	mov	x19, x1
	add	x20, x29, 48
	bl	_aegis_execution__get_address
	mov	x1, x21
	mov	x0, x22
	mov	x8, x20
	bl	_aegis_execution__get_balance
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w2, 1
	mov	x1, 5
	add	x0, x19, 16
	strb	w2, [x19]
	str	x1, [x19, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x19, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 112
LCFI85:
	ret
LFE20:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_call
_aegis_syscall__sys_handle_call:
LFB21:
	stp	x29, x30, [sp, -80]!
LCFI86:
	mov	x29, sp
LCFI87:
	stp	x19, x20, [sp, 16]
LCFI88:
	mov	x19, x6
	mov	x20, x0
	str	x21, [sp, 32]
LCFI89:
	bl	_aegis_execution__current_depth
	and	w1, w0, 65535
	cmp	w1, 1024
	bhi	L97
	cmp	w0, 1024
	beq	L92
	add	x21, x29, 48
	mov	x0, x20
	mov	x8, x21
	bl	_aegis_execution__get_address
	adrp	x3, _aegis_vm_types__u256_zero@GOTPAGE
	ldr	x3, [x3, _aegis_vm_types__u256_zero@GOTPAGEOFF]
	mov	x1, x21
	mov	x0, x20
	mov	w5, 0
	mov	x4, 0
	adrp	x2, _aegis_vm_types__address_zero@GOTPAGE
	ldr	x2, [x2, _aegis_vm_types__address_zero@GOTPAGEOFF]
	bl	_aegis_execution__enter_call
	cmp	w0, 1
	bhi	L98
	cbnz	w0, L99
L92:
	movi	v31.4s, 0
	mov	w0, 8
	strb	wzr, [x19]
	str	xzr, [x19, 8]
	str	w0, [x19, 48]
	stp	q31, q31, [x19, 16]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI90:
	ret
	.p2align 2,,3
L99:
LCFI91:
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	ldr	x1, [x0, 48]
	ldp	q30, q29, [x0]
	ldr	q31, [x0, 32]
	str	x1, [x19, 48]
	str	q31, [x19, 32]
	stp	q30, q29, [x19]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI92:
	ret
L97:
LCFI93:
	adrp	x0, lC10@PAGE
	mov	w1, 456
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L98:
	adrp	x0, lC10@PAGE
	mov	w1, 464
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE21:
	.const
	.align	3
lC9:
	.byte	1
	.space 7
	.xword	700
	.xword	1
	.xword	0
	.xword	0
	.xword	0
	.word	0
	.space 4
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_staticcall
_aegis_syscall__sys_handle_staticcall:
LFB22:
	stp	x29, x30, [sp, -80]!
LCFI94:
	mov	x29, sp
LCFI95:
	stp	x19, x20, [sp, 16]
LCFI96:
	mov	x19, x5
	mov	x20, x0
	str	x21, [sp, 32]
LCFI97:
	bl	_aegis_execution__current_depth
	and	w1, w0, 65535
	cmp	w1, 1024
	bhi	L110
	cmp	w0, 1024
	beq	L105
	add	x21, x29, 48
	mov	x0, x20
	mov	x8, x21
	bl	_aegis_execution__get_address
	adrp	x3, _aegis_vm_types__u256_zero@GOTPAGE
	ldr	x3, [x3, _aegis_vm_types__u256_zero@GOTPAGEOFF]
	mov	x1, x21
	mov	x0, x20
	mov	w5, 1
	mov	x4, 0
	adrp	x2, _aegis_vm_types__address_zero@GOTPAGE
	ldr	x2, [x2, _aegis_vm_types__address_zero@GOTPAGEOFF]
	bl	_aegis_execution__enter_call
	cmp	w0, 1
	bhi	L111
	cbnz	w0, L112
L105:
	movi	v31.4s, 0
	mov	w0, 8
	strb	wzr, [x19]
	str	xzr, [x19, 8]
	str	w0, [x19, 48]
	stp	q31, q31, [x19, 16]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI98:
	ret
	.p2align 2,,3
L112:
LCFI99:
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	ldr	x1, [x0, 48]
	ldp	q30, q29, [x0]
	ldr	q31, [x0, 32]
	str	x1, [x19, 48]
	str	q31, [x19, 32]
	stp	q30, q29, [x19]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI100:
	ret
L110:
LCFI101:
	adrp	x0, lC10@PAGE
	mov	w1, 482
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L111:
	adrp	x0, lC10@PAGE
	mov	w1, 489
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_delegatecall
_aegis_syscall__sys_handle_delegatecall:
LFB23:
	stp	x29, x30, [sp, -80]!
LCFI102:
	mov	x29, sp
LCFI103:
	stp	x19, x20, [sp, 16]
LCFI104:
	mov	x19, x5
	mov	x20, x0
	str	x21, [sp, 32]
LCFI105:
	bl	_aegis_execution__current_depth
	and	w1, w0, 65535
	cmp	w1, 1024
	bhi	L123
	cmp	w0, 1024
	beq	L118
	add	x21, x29, 48
	mov	x0, x20
	mov	x8, x21
	bl	_aegis_execution__get_address
	adrp	x3, _aegis_vm_types__u256_zero@GOTPAGE
	ldr	x3, [x3, _aegis_vm_types__u256_zero@GOTPAGEOFF]
	mov	x1, x21
	mov	x0, x20
	mov	w5, 2
	mov	x4, 0
	adrp	x2, _aegis_vm_types__address_zero@GOTPAGE
	ldr	x2, [x2, _aegis_vm_types__address_zero@GOTPAGEOFF]
	bl	_aegis_execution__enter_call
	cmp	w0, 1
	bhi	L124
	cbnz	w0, L125
L118:
	movi	v31.4s, 0
	mov	w0, 8
	strb	wzr, [x19]
	str	xzr, [x19, 8]
	str	w0, [x19, 48]
	stp	q31, q31, [x19, 16]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI106:
	ret
	.p2align 2,,3
L125:
LCFI107:
	adrp	x0, lC9@PAGE
	add	x0, x0, lC9@PAGEOFF;
	ldr	x1, [x0, 48]
	ldp	q30, q29, [x0]
	ldr	q31, [x0, 32]
	str	x1, [x19, 48]
	str	q31, [x19, 32]
	stp	q30, q29, [x19]
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI108:
	ret
L123:
LCFI109:
	adrp	x0, lC10@PAGE
	mov	w1, 507
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L124:
	adrp	x0, lC10@PAGE
	mov	w1, 514
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_return
_aegis_syscall__sys_handle_return:
LFB24:
	stp	x29, x30, [sp, -80]!
LCFI110:
	mov	x29, sp
LCFI111:
	str	x19, [sp, 16]
LCFI112:
	mov	x19, x2
	add	x2, x29, 32
	bl	_aegis_execution__finalize_success
	ldr	x0, [x29, 40]
	movi	v31.4s, 0
	mov	w1, 1
	str	wzr, [x19, 48]
	strb	w1, [x19]
	stp	q31, q31, [x19, 16]
	str	x0, [x19, 8]
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI113:
	ret
LFE24:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_revert
_aegis_syscall__sys_handle_revert:
LFB25:
	stp	x29, x30, [sp, -80]!
LCFI114:
	mov	x29, sp
LCFI115:
	str	x19, [sp, 16]
LCFI116:
	mov	x19, x2
	add	x2, x29, 32
	bl	_aegis_execution__finalize_revert
	ldr	x0, [x29, 40]
	movi	v31.4s, 0
	mov	w1, 12
	strb	wzr, [x19]
	str	w1, [x19, 48]
	stp	q31, q31, [x19, 16]
	str	x0, [x19, 8]
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI117:
	ret
LFE25:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_stop
_aegis_syscall__sys_handle_stop:
LFB26:
	stp	x29, x30, [sp, -80]!
LCFI118:
	mov	x29, sp
LCFI119:
	add	x2, x29, 32
	str	x19, [sp, 16]
LCFI120:
	mov	x19, x1
	adrp	x1, _aegis_contract__empty_return@GOTPAGE
	ldr	x1, [x1, _aegis_contract__empty_return@GOTPAGEOFF]
	bl	_aegis_execution__finalize_success
	ldr	x0, [x29, 40]
	movi	v31.4s, 0
	mov	w1, 1
	str	wzr, [x19, 48]
	strb	w1, [x19]
	stp	q31, q31, [x19, 16]
	str	x0, [x19, 8]
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 80
LCFI121:
	ret
LFE26:
	.const
	.align	3
lC15:
	.ascii "failed precondition from aegis_syscall.ads:331"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__sys_handle_log
_aegis_syscall__sys_handle_log:
LFB27:
	mov	x12, 4160
	sub	sp, sp, x12
LCFI122:
	stp	x29, x30, [sp]
LCFI123:
	mov	x29, sp
LCFI124:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
LCFI125:
	tbnz	w2, #31, L144
	mov	w20, w2
	cmp	w2, 4
	bgt	L145
	add	x3, x29, 64
	mov	x21, x0
	mov	x23, x1
	mov	x0, x3
	mov	x2, 4096
	mov	w1, 0
	mov	w22, w4
	mov	x19, x5
	bl	_memset
	mov	x3, x0
	ldrb	w0, [x21, 160]
	cmp	w0, 3
	bhi	L146
	ldrb	w1, [x21, 357]
	cmp	w1, 1
	bhi	L147
	cmp	w1, 0
	ccmp	w0, 1, 4, ne
	cset	w24, ne
	beq	L140
	tbnz	w22, #31, L148
	mov	w0, 16960
	movk	w0, 0xf, lsl 16
	cmp	w22, w0
	ble	L149
	mov	w24, 0
	mov	x0, 0
	mov	w1, 4
L137:
	movi	v31.4s, 0
	strb	w24, [x19]
	str	x0, [x19, 8]
	str	w1, [x19, 48]
	stp	q31, q31, [x19, 16]
	mov	x12, 4160
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, x12
LCFI126:
	ret
	.p2align 2,,3
L149:
LCFI127:
	mov	x1, x23
	mov	x0, x21
	mov	w4, w22
	mov	w2, w20
	bl	_aegis_execution__emit_log
	cmp	w0, 1
	bhi	L150
	cbz	w0, L142
	mov	w1, w22
	mov	w0, w20
	bl	_aegis_gas__gas_log
	mov	w1, 0
	b	L137
	.p2align 2,,3
L140:
	mov	x0, 0
	mov	w1, 7
	b	L137
	.p2align 2,,3
L142:
	mov	w24, 0
	mov	x0, 0
	mov	w1, 3
	b	L137
L145:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L147:
	adrp	x0, lC10@PAGE
	mov	w1, 663
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L144:
	adrp	x0, lC11@PAGE
	mov	w1, 331
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L146:
	adrp	x0, lC10@PAGE
	mov	w1, 662
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L148:
	adrp	x0, lC10@PAGE
	mov	w1, 594
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L150:
	adrp	x0, lC10@PAGE
	mov	w1, 600
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE27:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__is_syscall_allowed
_aegis_syscall__is_syscall_allowed:
LFB28:
	stp	x29, x30, [sp, -16]!
LCFI128:
	mov	x29, sp
LCFI129:
	cmp	w1, 30
	bhi	L168
	cmp	w1, 19
	bhi	L153
	cmp	w1, 16
	bhi	L154
	cmp	w1, 1
	beq	L155
	bls	L156
	cmp	w1, 5
	bhi	L164
	ldrb	w0, [x0, 7]
	cmp	w0, 1
	bls	L157
	adrp	x0, lC10@PAGE
	mov	w1, 634
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L153:
	cmp	w1, 24
	bhi	L159
	cmp	w1, 21
	bhi	L164
	ldrb	w0, [x0, 5]
	cmp	w0, 1
	bhi	L169
L157:
	ldp	x29, x30, [sp], 16
LCFI130:
	ret
	.p2align 2,,3
L154:
LCFI131:
	ldrb	w0, [x0, 3]
	cmp	w0, 1
	bls	L157
	adrp	x0, lC10@PAGE
	mov	w1, 638
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L164:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI132:
	ret
	.p2align 2,,3
L159:
LCFI133:
	cmp	w1, 25
	bne	L170
	ldrb	w0, [x0, 6]
	cmp	w0, 1
	bls	L157
	adrp	x0, lC10@PAGE
	mov	w1, 650
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L156:
	ldrb	w0, [x0, 1]
	cmp	w0, 1
	bls	L157
	adrp	x0, lC10@PAGE
	mov	w1, 626
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L170:
	ldrb	w0, [x0, 8]
	cmp	w0, 1
	bls	L157
	adrp	x0, lC10@PAGE
	mov	w1, 654
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L155:
	ldrb	w0, [x0, 2]
	cmp	w0, 1
	bls	L157
	adrp	x0, lC10@PAGE
	mov	w1, 630
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L168:
	adrp	x0, lC10@PAGE
	mov	w1, 617
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L169:
	adrp	x0, lC10@PAGE
	mov	w1, 642
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE28:
	.const
	.align	3
lC16:
	.ascii "failed precondition from aegis_syscall.ads:105"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__dispatch
_aegis_syscall__dispatch:
LFB4:
	stp	x29, x30, [sp, -240]!
LCFI134:
	mov	x29, sp
LCFI135:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI136:
	tbnz	w3, #31, L234
	mov	w21, w3
	cmp	w3, 8
	bgt	L235
	mov	x22, x0
	add	x3, x0, 355
	mov	x23, x2
	mov	x20, x4
	ldr	x4, [x3]
	add	x0, x29, 96
	mov	w19, w1
	ldrb	w2, [x22, 363]
	str	x4, [x29, 96]
	strb	w2, [x0, 8]
	cmp	w1, 30
	bhi	L236
	bl	_aegis_syscall__is_syscall_allowed
	tbz	x0, 0, L237
	cmp	w19, 11
	beq	L177
	bhi	L178
	cmp	w19, 5
	beq	L179
	bhi	L180
	cmp	w19, 2
	beq	L181
	bhi	L182
	cbz	w19, L238
	cmp	w21, 1
	bgt	L239
L205:
	movi	v31.4s, 0
	mov	w0, 4
	strb	wzr, [x20]
	str	xzr, [x20, 8]
	str	w0, [x20, 48]
	stp	q31, q31, [x20, 16]
	b	L171
	.p2align 2,,3
L178:
	cmp	w19, 21
	bhi	L191
	cmp	w19, 16
	bhi	L192
	cmp	w19, 14
	beq	L193
	bls	L194
	cmp	w19, 15
	beq	L240
	add	x21, x29, 112
	mov	x0, x22
	mov	x8, x21
	add	x19, x29, 64
	bl	_aegis_execution__get_address
	mov	x1, x21
	mov	x0, x22
	mov	x8, x19
	bl	_aegis_execution__get_balance
	ld1	{v30.16b - v31.16b}, [x19]
	mov	w2, 1
	mov	x1, 5
	b	L233
	.p2align 2,,3
L237:
	movi	v31.4s, 0
	mov	w0, 2
	strb	wzr, [x20]
	str	xzr, [x20, 8]
	str	w0, [x20, 48]
	stp	q31, q31, [x20, 16]
L171:
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 240
LCFI137:
	ret
	.p2align 2,,3
L191:
LCFI138:
	cmp	w19, 24
	beq	L199
	bhi	L200
	cmp	w19, 22
	beq	L199
	adrp	x1, _aegis_vm_types__hash256_zero@GOTPAGE
	ldr	x1, [x1, _aegis_vm_types__hash256_zero@GOTPAGEOFF]
	mov	x0, x22
	add	x2, x29, 112
	bl	_aegis_execution__finalize_revert
	ldr	x0, [x29, 120]
	movi	v31.4s, 0
	mov	w1, 12
	strb	wzr, [x20]
	str	w1, [x20, 48]
	stp	q31, q31, [x20, 16]
	str	x0, [x20, 8]
	b	L171
	.p2align 2,,3
L180:
	cmp	w19, 8
	beq	L187
	bhi	L188
	cmp	w19, 6
	beq	L241
	add	x21, x29, 112
	mov	x0, x22
	mov	x8, x21
	bl	_aegis_execution__get_address
L231:
	add	x19, x29, 64
	mov	x0, x21
	mov	x8, x19
	bl	_aegis_u256__address_to_u256
L232:
	ld1	{v30.16b - v31.16b}, [x19]
	mov	w2, 1
	mov	x1, 2
L233:
	add	x0, x20, 16
	strb	w2, [x20]
	str	x1, [x20, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x20, 48]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 240
LCFI139:
	ret
	.p2align 2,,3
L199:
LCFI140:
	adrp	x1, _aegis_contract__empty_return@GOTPAGE
	ldr	x1, [x1, _aegis_contract__empty_return@GOTPAGEOFF]
	mov	x0, x22
	add	x2, x29, 112
	bl	_aegis_execution__finalize_success
	ldr	x0, [x29, 120]
	movi	v31.4s, 0
	mov	w1, 1
	str	wzr, [x20, 48]
	strb	w1, [x20]
	stp	q31, q31, [x20, 16]
	str	x0, [x20, 8]
	b	L171
	.p2align 2,,3
L194:
	cmp	w19, 12
	beq	L242
	add	x22, x22, 32
	mov	w2, 1
	mov	x1, 2
	add	x0, x20, 16
	ld1	{v30.16b - v31.16b}, [x22]
	strb	w2, [x20]
	str	x1, [x20, 8]
	st1	{v30.16b - v31.16b}, [x0]
	str	wzr, [x20, 48]
	b	L171
	.p2align 2,,3
L182:
	cmp	w19, 3
	beq	L243
	adrp	x4, _aegis_vm_types__hash256_zero@GOTPAGE
	ldr	x4, [x4, _aegis_vm_types__hash256_zero@GOTPAGEOFF]
	mov	x5, x20
	mov	x0, x22
	ldp	x19, x20, [sp, 16]
	mov	w2, 0
	ldp	x21, x22, [sp, 32]
	mov	x3, x4
	mov	x1, x4
	ldr	x23, [sp, 48]
	ldp	x29, x30, [sp], 240
LCFI141:
	b	_aegis_syscall__sys_handle_mldsa_verify
	.p2align 2,,3
L243:
LCFI142:
	cbz	w21, L207
	mov	x0, x23
	bl	_aegis_u256__u256_to_u64
	mov	x21, x0
	mov	w0, 32768
	cmp	x21, 32768
	csel	w21, w21, w0, ls
L207:
	adrp	x1, _data_hash.0@PAGE
	ldr	x23, [sp, 48]
	mov	x3, x20
	mov	w2, w21
	ldp	x19, x20, [sp, 16]
	mov	x0, x22
	add	x1, x1, _data_hash.0@PAGEOFF;
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 240
LCFI143:
	b	_aegis_syscall__sys_handle_keccak256
	.p2align 2,,3
L238:
LCFI144:
	cbz	w21, L205
	mov	x2, x20
	mov	x1, x23
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	mov	x0, x22
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 240
LCFI145:
	b	_aegis_syscall__sys_handle_sload
	.p2align 2,,3
L242:
LCFI146:
	add	x19, x29, 64
	mov	x0, x22
	mov	x8, x19
	bl	_aegis_execution__get_timestamp
	b	L232
	.p2align 2,,3
L241:
	add	x21, x29, 112
	mov	x0, x22
	mov	x8, x21
	bl	_aegis_execution__get_caller
	b	L231
	.p2align 2,,3
L240:
	adrp	x1, _aegis_vm_types__address_zero@GOTPAGE
	ldr	x1, [x1, _aegis_vm_types__address_zero@GOTPAGEOFF]
	add	x19, x29, 64
	mov	x0, x22
	mov	x8, x19
	bl	_aegis_execution__get_balance
	ld1	{v30.16b - v31.16b}, [x19]
	mov	w2, 1
	mov	x1, 400
	b	L233
	.p2align 2,,3
L200:
	cmp	w19, 25
	bne	L244
	ldrb	w0, [x22, 160]
	cmp	w0, 3
	bhi	L245
	ldrb	w1, [x22, 357]
	cmp	w1, 1
	bhi	L246
	cmp	w1, 0
	ccmp	w0, 1, 4, ne
	beq	L210
	adrp	x0, lC2@PAGE
	add	x0, x0, lC2@PAGEOFF;
L230:
	ldp	q30, q29, [x0]
	ldr	q31, [x0, 32]
	ldr	x1, [x0, 48]
	str	q31, [x20, 32]
	stp	q30, q29, [x20]
	str	x1, [x20, 48]
	b	L171
	.p2align 2,,3
L188:
	adrp	x0, lC1@PAGE
	add	x0, x0, lC1@PAGEOFF;
	b	L230
	.p2align 2,,3
L187:
	add	x19, x29, 64
	mov	x0, x22
	mov	x8, x19
	bl	_aegis_execution__get_call_value
	b	L232
L244:
	adrp	x3, _aegis_vm_types__hash256_zero@GOTPAGE
	ldr	x3, [x3, _aegis_vm_types__hash256_zero@GOTPAGEOFF]
	mov	x5, x20
	mov	x0, x22
	mov	w4, 0
	mov	w2, 0
	stp	xzr, xzr, [x29, 112]
	add	x1, x29, 112
	stp	xzr, xzr, [x29, 128]
	stp	xzr, xzr, [x29, 144]
	stp	xzr, xzr, [x29, 160]
	stp	xzr, xzr, [x29, 176]
	stp	xzr, xzr, [x29, 192]
	stp	xzr, xzr, [x29, 208]
	stp	xzr, xzr, [x29, 224]
	bl	_aegis_syscall__sys_handle_log
	b	L171
	.p2align 2,,3
L181:
	cbz	w21, L206
	mov	x0, x23
	bl	_aegis_u256__u256_to_u64
	mov	x21, x0
	mov	w0, 32768
	cmp	x21, 32768
	csel	w21, w21, w0, ls
L206:
	adrp	x1, _data_hash.1@PAGE
	ldr	x23, [sp, 48]
	mov	x3, x20
	mov	w2, w21
	ldp	x19, x20, [sp, 16]
	mov	x0, x22
	add	x1, x1, _data_hash.1@PAGEOFF;
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 240
LCFI147:
	b	_aegis_syscall__sys_handle_sha3
	.p2align 2,,3
L179:
LCFI148:
	adrp	x2, _aegis_vm_types__hash256_zero@GOTPAGE
	ldr	x2, [x2, _aegis_vm_types__hash256_zero@GOTPAGEOFF]
	mov	x3, x20
	mov	x0, x22
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	mov	x1, x2
	ldr	x23, [sp, 48]
	ldp	x29, x30, [sp], 240
LCFI149:
	b	_aegis_syscall__sys_handle_mlkem_decaps
	.p2align 2,,3
L177:
LCFI150:
	add	x19, x29, 64
	mov	x0, x22
	mov	x8, x19
	bl	_aegis_execution__get_block_number
	b	L232
	.p2align 2,,3
L193:
	ldr	x23, [sp, 48]
	mov	x1, x20
	mov	x0, x22
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 240
LCFI151:
	b	_aegis_syscall__sys_handle_gasremaining
	.p2align 2,,3
L192:
LCFI152:
	movi	v31.4s, 0
	mov	w0, 8
	strb	wzr, [x20]
	str	xzr, [x20, 8]
	str	w0, [x20, 48]
	stp	q31, q31, [x20, 16]
	b	L171
	.p2align 2,,3
L210:
	movi	v31.4s, 0
	mov	w0, 7
	strb	wzr, [x20]
	str	xzr, [x20, 8]
	str	w0, [x20, 48]
	stp	q31, q31, [x20, 16]
	b	L171
	.p2align 2,,3
L239:
	mov	x3, x20
	mov	x0, x22
	ldp	x19, x20, [sp, 16]
	add	x2, x23, 32
	mov	x1, x23
	ldp	x21, x22, [sp, 32]
	ldr	x23, [sp, 48]
	ldp	x29, x30, [sp], 240
LCFI153:
	b	_aegis_syscall__sys_handle_sstore
L235:
LCFI154:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L234:
	adrp	x0, lC11@PAGE
	mov	w1, 105
	add	x0, x0, lC11@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L236:
	adrp	x0, lC10@PAGE
	mov	w1, 25
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L245:
	adrp	x0, lC10@PAGE
	mov	w1, 662
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L246:
	adrp	x0, lC10@PAGE
	mov	w1, 663
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE4:
	.const
	.align	3
lC2:
	.byte	1
	.space 7
	.xword	5000
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.word	0
	.space 4
	.align	3
lC1:
	.byte	1
	.space 7
	.xword	3
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.word	0
	.space 4
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__can_modify_state
_aegis_syscall__can_modify_state:
LFB29:
	stp	x29, x30, [sp, -16]!
LCFI155:
	mov	x29, sp
LCFI156:
	ldrb	w1, [x0, 160]
	cmp	w1, 3
	bhi	L251
	ldrb	w0, [x0, 357]
	cmp	w0, 1
	bhi	L252
	cmp	w0, 0
	ccmp	w1, 1, 4, ne
	cset	w0, ne
	ldp	x29, x30, [sp], 16
LCFI157:
	ret
L251:
LCFI158:
	adrp	x0, lC10@PAGE
	mov	w1, 662
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L252:
	adrp	x0, lC10@PAGE
	mov	w1, 663
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE29:
	.align	2
	.p2align 5,,15
	.globl _aegis_syscall__can_transfer_value
_aegis_syscall__can_transfer_value:
LFB30:
	stp	x29, x30, [sp, -16]!
LCFI159:
	mov	x29, sp
LCFI160:
	ldrb	w1, [x0, 160]
	cmp	w1, 3
	bhi	L257
	ldrb	w0, [x0, 359]
	cmp	w0, 1
	bhi	L258
	cmp	w0, 0
	ccmp	w1, 1, 4, ne
	cset	w0, ne
	ldp	x29, x30, [sp], 16
LCFI161:
	ret
L257:
LCFI162:
	adrp	x0, lC10@PAGE
	mov	w1, 670
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L258:
	adrp	x0, lC10@PAGE
	mov	w1, 671
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE30:
	.const
_data_hash.0:
	.space 32
_data_hash.1:
	.space 32
	.globl _aegis_syscall__syscall_return_ok
	.align	3
_aegis_syscall__syscall_return_ok:
	.byte	1
	.space 7
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.word	0
	.space 4
	.globl _aegis_syscall__syscall_return_zero
	.align	3
_aegis_syscall__syscall_return_zero:
	.space 56
	.globl _aegis_syscall_E
	.data
	.align	1
_aegis_syscall_E:
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
	.quad	LFB5-.
	.set L$set$6,LFE5-LFB5
	.quad L$set$6
	.uleb128 0
	.byte	0x4
	.set L$set$7,LCFI0-LFB5
	.long L$set$7
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$8,LCFI1-LCFI0
	.long L$set$8
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$9,LCFI2-LCFI1
	.long L$set$9
	.byte	0x93
	.uleb128 0x12
	.byte	0x94
	.uleb128 0x11
	.byte	0x4
	.set L$set$10,LCFI3-LCFI2
	.long L$set$10
	.byte	0x95
	.uleb128 0x10
	.byte	0x96
	.uleb128 0xf
	.byte	0x4
	.set L$set$11,LCFI4-LCFI3
	.long L$set$11
	.byte	0x97
	.uleb128 0xe
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
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
	.set L$set$13,LCFI6-LCFI5
	.long L$set$13
	.byte	0xb
	.byte	0x4
	.set L$set$14,LCFI7-LCFI6
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
	.set L$set$15,LCFI8-LCFI7
	.long L$set$15
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$16,LEFDE7-LASFDE7
	.long L$set$16
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB6-.
	.set L$set$17,LFE6-LFB6
	.quad L$set$17
	.uleb128 0
	.byte	0x4
	.set L$set$18,LCFI9-LFB6
	.long L$set$18
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$19,LCFI10-LCFI9
	.long L$set$19
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$20,LCFI11-LCFI10
	.long L$set$20
	.byte	0x93
	.uleb128 0x12
	.byte	0x94
	.uleb128 0x11
	.byte	0x95
	.uleb128 0x10
	.byte	0x96
	.uleb128 0xf
	.byte	0x4
	.set L$set$21,LCFI12-LCFI11
	.long L$set$21
	.byte	0x97
	.uleb128 0xe
	.byte	0x4
	.set L$set$22,LCFI13-LCFI12
	.long L$set$22
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
	.set L$set$23,LCFI14-LCFI13
	.long L$set$23
	.byte	0xb
	.byte	0x4
	.set L$set$24,LCFI15-LCFI14
	.long L$set$24
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
	.set L$set$25,LCFI16-LCFI15
	.long L$set$25
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$26,LEFDE9-LASFDE9
	.long L$set$26
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB7-.
	.set L$set$27,LFE7-LFB7
	.quad L$set$27
	.uleb128 0
	.byte	0x4
	.set L$set$28,LCFI17-LFB7
	.long L$set$28
	.byte	0xe
	.uleb128 0x8050
	.byte	0x4
	.set L$set$29,LCFI18-LCFI17
	.long L$set$29
	.byte	0x9d
	.uleb128 0x100a
	.byte	0x9e
	.uleb128 0x1009
	.byte	0x4
	.set L$set$30,LCFI19-LCFI18
	.long L$set$30
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$31,LCFI20-LCFI19
	.long L$set$31
	.byte	0x93
	.uleb128 0x1008
	.byte	0x94
	.uleb128 0x1007
	.byte	0x95
	.uleb128 0x1006
	.byte	0x4
	.set L$set$32,LCFI21-LCFI20
	.long L$set$32
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
	.set L$set$33,LCFI22-LCFI21
	.long L$set$33
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$34,LEFDE11-LASFDE11
	.long L$set$34
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB8-.
	.set L$set$35,LFE8-LFB8
	.quad L$set$35
	.uleb128 0
	.byte	0x4
	.set L$set$36,LCFI23-LFB8
	.long L$set$36
	.byte	0xe
	.uleb128 0x8050
	.byte	0x4
	.set L$set$37,LCFI24-LCFI23
	.long L$set$37
	.byte	0x9d
	.uleb128 0x100a
	.byte	0x9e
	.uleb128 0x1009
	.byte	0x4
	.set L$set$38,LCFI25-LCFI24
	.long L$set$38
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$39,LCFI26-LCFI25
	.long L$set$39
	.byte	0x93
	.uleb128 0x1008
	.byte	0x94
	.uleb128 0x1007
	.byte	0x95
	.uleb128 0x1006
	.byte	0x4
	.set L$set$40,LCFI27-LCFI26
	.long L$set$40
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
	.set L$set$41,LCFI28-LCFI27
	.long L$set$41
	.byte	0xb
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$42,LEFDE13-LASFDE13
	.long L$set$42
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB9-.
	.set L$set$43,LFE9-LFB9
	.quad L$set$43
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI29-LFB9
	.long L$set$44
	.byte	0xe
	.uleb128 0x9c80
	.byte	0x4
	.set L$set$45,LCFI30-LCFI29
	.long L$set$45
	.byte	0x9d
	.uleb128 0x1390
	.byte	0x9e
	.uleb128 0x138f
	.byte	0x4
	.set L$set$46,LCFI31-LCFI30
	.long L$set$46
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$47,LCFI32-LCFI31
	.long L$set$47
	.byte	0x93
	.uleb128 0x138e
	.byte	0x94
	.uleb128 0x138d
	.byte	0x95
	.uleb128 0x138c
	.byte	0x96
	.uleb128 0x138b
	.byte	0x97
	.uleb128 0x138a
	.byte	0x4
	.set L$set$48,LCFI33-LCFI32
	.long L$set$48
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
	.set L$set$49,LCFI34-LCFI33
	.long L$set$49
	.byte	0xb
	.byte	0x4
	.set L$set$50,LCFI35-LCFI34
	.long L$set$50
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
	.set L$set$51,LCFI36-LCFI35
	.long L$set$51
	.byte	0xb
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$52,LEFDE15-LASFDE15
	.long L$set$52
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB10-.
	.set L$set$53,LFE10-LFB10
	.quad L$set$53
	.uleb128 0
	.byte	0x4
	.set L$set$54,LCFI37-LFB10
	.long L$set$54
	.byte	0xe
	.uleb128 0x12d0
	.byte	0x4
	.set L$set$55,LCFI38-LCFI37
	.long L$set$55
	.byte	0x9d
	.uleb128 0x25a
	.byte	0x9e
	.uleb128 0x259
	.byte	0x4
	.set L$set$56,LCFI39-LCFI38
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$57,LCFI40-LCFI39
	.long L$set$57
	.byte	0x93
	.uleb128 0x258
	.byte	0x94
	.uleb128 0x257
	.byte	0x4
	.set L$set$58,LCFI41-LCFI40
	.long L$set$58
	.byte	0x95
	.uleb128 0x256
	.byte	0x96
	.uleb128 0x255
	.byte	0x4
	.set L$set$59,LCFI42-LCFI41
	.long L$set$59
	.byte	0xa
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
	.set L$set$60,LCFI43-LCFI42
	.long L$set$60
	.byte	0xb
	.byte	0x4
	.set L$set$61,LCFI44-LCFI43
	.long L$set$61
	.byte	0xa
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
	.set L$set$62,LCFI45-LCFI44
	.long L$set$62
	.byte	0xb
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$63,LEFDE17-LASFDE17
	.long L$set$63
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB11-.
	.set L$set$64,LFE11-LFB11
	.quad L$set$64
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI46-LFB11
	.long L$set$65
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$66,LCFI47-LCFI46
	.long L$set$66
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$67,LCFI48-LCFI47
	.long L$set$67
	.byte	0x95
	.uleb128 0xa
	.byte	0x4
	.set L$set$68,LCFI49-LCFI48
	.long L$set$68
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x4
	.set L$set$69,LCFI50-LCFI49
	.long L$set$69
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$70,LEFDE19-LASFDE19
	.long L$set$70
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB12-.
	.set L$set$71,LFE12-LFB12
	.quad L$set$71
	.uleb128 0
	.byte	0x4
	.set L$set$72,LCFI51-LFB12
	.long L$set$72
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$73,LCFI52-LCFI51
	.long L$set$73
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$74,LCFI53-LCFI52
	.long L$set$74
	.byte	0x95
	.uleb128 0xa
	.byte	0x4
	.set L$set$75,LCFI54-LCFI53
	.long L$set$75
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x4
	.set L$set$76,LCFI55-LCFI54
	.long L$set$76
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$77,LEFDE21-LASFDE21
	.long L$set$77
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB13-.
	.set L$set$78,LFE13-LFB13
	.quad L$set$78
	.uleb128 0
	.byte	0x4
	.set L$set$79,LCFI56-LFB13
	.long L$set$79
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$80,LCFI57-LCFI56
	.long L$set$80
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$81,LCFI58-LCFI57
	.long L$set$81
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$82,LCFI59-LCFI58
	.long L$set$82
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$83,LEFDE23-LASFDE23
	.long L$set$83
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB14-.
	.set L$set$84,LFE14-LFB14
	.quad L$set$84
	.uleb128 0
	.byte	0x4
	.set L$set$85,LCFI60-LFB14
	.long L$set$85
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$86,LCFI61-LCFI60
	.long L$set$86
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$87,LCFI62-LCFI61
	.long L$set$87
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$88,LCFI63-LCFI62
	.long L$set$88
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$89,LEFDE25-LASFDE25
	.long L$set$89
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB15-.
	.set L$set$90,LFE15-LFB15
	.quad L$set$90
	.uleb128 0
	.byte	0x4
	.set L$set$91,LCFI64-LFB15
	.long L$set$91
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$92,LCFI65-LCFI64
	.long L$set$92
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$93,LCFI66-LCFI65
	.long L$set$93
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$94,LCFI67-LCFI66
	.long L$set$94
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$95,LEFDE27-LASFDE27
	.long L$set$95
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB16-.
	.set L$set$96,LFE16-LFB16
	.quad L$set$96
	.uleb128 0
	.byte	0x4
	.set L$set$97,LCFI68-LFB16
	.long L$set$97
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$98,LCFI69-LCFI68
	.long L$set$98
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$99,LCFI70-LCFI69
	.long L$set$99
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$100,LCFI71-LCFI70
	.long L$set$100
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$101,LEFDE29-LASFDE29
	.long L$set$101
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB17-.
	.set L$set$102,LFE17-LFB17
	.quad L$set$102
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$103,LEFDE31-LASFDE31
	.long L$set$103
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB18-.
	.set L$set$104,LFE18-LFB18
	.quad L$set$104
	.uleb128 0
	.byte	0x4
	.set L$set$105,LCFI72-LFB18
	.long L$set$105
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$106,LCFI73-LCFI72
	.long L$set$106
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$107,LCFI74-LCFI73
	.long L$set$107
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$108,LCFI75-LCFI74
	.long L$set$108
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$109,LCFI76-LCFI75
	.long L$set$109
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$110,LEFDE33-LASFDE33
	.long L$set$110
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB19-.
	.set L$set$111,LFE19-LFB19
	.quad L$set$111
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI77-LFB19
	.long L$set$112
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$113,LCFI78-LCFI77
	.long L$set$113
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$114,LCFI79-LCFI78
	.long L$set$114
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x4
	.set L$set$115,LCFI80-LCFI79
	.long L$set$115
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$116,LEFDE35-LASFDE35
	.long L$set$116
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB20-.
	.set L$set$117,LFE20-LFB20
	.quad L$set$117
	.uleb128 0
	.byte	0x4
	.set L$set$118,LCFI81-LFB20
	.long L$set$118
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$119,LCFI82-LCFI81
	.long L$set$119
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$120,LCFI83-LCFI82
	.long L$set$120
	.byte	0x95
	.uleb128 0xa
	.byte	0x96
	.uleb128 0x9
	.byte	0x4
	.set L$set$121,LCFI84-LCFI83
	.long L$set$121
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x4
	.set L$set$122,LCFI85-LCFI84
	.long L$set$122
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
LEFDE35:
LSFDE37:
	.set L$set$123,LEFDE37-LASFDE37
	.long L$set$123
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB21-.
	.set L$set$124,LFE21-LFB21
	.quad L$set$124
	.uleb128 0
	.byte	0x4
	.set L$set$125,LCFI86-LFB21
	.long L$set$125
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$126,LCFI87-LCFI86
	.long L$set$126
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$127,LCFI88-LCFI87
	.long L$set$127
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$128,LCFI89-LCFI88
	.long L$set$128
	.byte	0x95
	.uleb128 0x6
	.byte	0x4
	.set L$set$129,LCFI90-LCFI89
	.long L$set$129
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
	.set L$set$130,LCFI91-LCFI90
	.long L$set$130
	.byte	0xb
	.byte	0x4
	.set L$set$131,LCFI92-LCFI91
	.long L$set$131
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
	.set L$set$132,LCFI93-LCFI92
	.long L$set$132
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$133,LEFDE39-LASFDE39
	.long L$set$133
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB22-.
	.set L$set$134,LFE22-LFB22
	.quad L$set$134
	.uleb128 0
	.byte	0x4
	.set L$set$135,LCFI94-LFB22
	.long L$set$135
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$136,LCFI95-LCFI94
	.long L$set$136
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$137,LCFI96-LCFI95
	.long L$set$137
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$138,LCFI97-LCFI96
	.long L$set$138
	.byte	0x95
	.uleb128 0x6
	.byte	0x4
	.set L$set$139,LCFI98-LCFI97
	.long L$set$139
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
	.set L$set$140,LCFI99-LCFI98
	.long L$set$140
	.byte	0xb
	.byte	0x4
	.set L$set$141,LCFI100-LCFI99
	.long L$set$141
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
	.set L$set$142,LCFI101-LCFI100
	.long L$set$142
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$143,LEFDE41-LASFDE41
	.long L$set$143
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB23-.
	.set L$set$144,LFE23-LFB23
	.quad L$set$144
	.uleb128 0
	.byte	0x4
	.set L$set$145,LCFI102-LFB23
	.long L$set$145
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$146,LCFI103-LCFI102
	.long L$set$146
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$147,LCFI104-LCFI103
	.long L$set$147
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$148,LCFI105-LCFI104
	.long L$set$148
	.byte	0x95
	.uleb128 0x6
	.byte	0x4
	.set L$set$149,LCFI106-LCFI105
	.long L$set$149
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
	.set L$set$150,LCFI107-LCFI106
	.long L$set$150
	.byte	0xb
	.byte	0x4
	.set L$set$151,LCFI108-LCFI107
	.long L$set$151
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
	.set L$set$152,LCFI109-LCFI108
	.long L$set$152
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$153,LEFDE43-LASFDE43
	.long L$set$153
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB24-.
	.set L$set$154,LFE24-LFB24
	.quad L$set$154
	.uleb128 0
	.byte	0x4
	.set L$set$155,LCFI110-LFB24
	.long L$set$155
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$156,LCFI111-LCFI110
	.long L$set$156
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$157,LCFI112-LCFI111
	.long L$set$157
	.byte	0x93
	.uleb128 0x8
	.byte	0x4
	.set L$set$158,LCFI113-LCFI112
	.long L$set$158
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$159,LEFDE45-LASFDE45
	.long L$set$159
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB25-.
	.set L$set$160,LFE25-LFB25
	.quad L$set$160
	.uleb128 0
	.byte	0x4
	.set L$set$161,LCFI114-LFB25
	.long L$set$161
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$162,LCFI115-LCFI114
	.long L$set$162
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$163,LCFI116-LCFI115
	.long L$set$163
	.byte	0x93
	.uleb128 0x8
	.byte	0x4
	.set L$set$164,LCFI117-LCFI116
	.long L$set$164
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$165,LEFDE47-LASFDE47
	.long L$set$165
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB26-.
	.set L$set$166,LFE26-LFB26
	.quad L$set$166
	.uleb128 0
	.byte	0x4
	.set L$set$167,LCFI118-LFB26
	.long L$set$167
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$168,LCFI119-LCFI118
	.long L$set$168
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$169,LCFI120-LCFI119
	.long L$set$169
	.byte	0x93
	.uleb128 0x8
	.byte	0x4
	.set L$set$170,LCFI121-LCFI120
	.long L$set$170
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$171,LEFDE49-LASFDE49
	.long L$set$171
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB27-.
	.set L$set$172,LFE27-LFB27
	.quad L$set$172
	.uleb128 0
	.byte	0x4
	.set L$set$173,LCFI122-LFB27
	.long L$set$173
	.byte	0xe
	.uleb128 0x1040
	.byte	0x4
	.set L$set$174,LCFI123-LCFI122
	.long L$set$174
	.byte	0x9d
	.uleb128 0x208
	.byte	0x9e
	.uleb128 0x207
	.byte	0x4
	.set L$set$175,LCFI124-LCFI123
	.long L$set$175
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$176,LCFI125-LCFI124
	.long L$set$176
	.byte	0x93
	.uleb128 0x206
	.byte	0x94
	.uleb128 0x205
	.byte	0x95
	.uleb128 0x204
	.byte	0x96
	.uleb128 0x203
	.byte	0x97
	.uleb128 0x202
	.byte	0x98
	.uleb128 0x201
	.byte	0x4
	.set L$set$177,LCFI126-LCFI125
	.long L$set$177
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
	.set L$set$178,LCFI127-LCFI126
	.long L$set$178
	.byte	0xb
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$179,LEFDE51-LASFDE51
	.long L$set$179
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB28-.
	.set L$set$180,LFE28-LFB28
	.quad L$set$180
	.uleb128 0
	.byte	0x4
	.set L$set$181,LCFI128-LFB28
	.long L$set$181
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$182,LCFI129-LCFI128
	.long L$set$182
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$183,LCFI130-LCFI129
	.long L$set$183
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$184,LCFI131-LCFI130
	.long L$set$184
	.byte	0xb
	.byte	0x4
	.set L$set$185,LCFI132-LCFI131
	.long L$set$185
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$186,LCFI133-LCFI132
	.long L$set$186
	.byte	0xb
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$187,LEFDE53-LASFDE53
	.long L$set$187
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB4-.
	.set L$set$188,LFE4-LFB4
	.quad L$set$188
	.uleb128 0
	.byte	0x4
	.set L$set$189,LCFI134-LFB4
	.long L$set$189
	.byte	0xe
	.uleb128 0xf0
	.byte	0x9d
	.uleb128 0x1e
	.byte	0x9e
	.uleb128 0x1d
	.byte	0x4
	.set L$set$190,LCFI135-LCFI134
	.long L$set$190
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$191,LCFI136-LCFI135
	.long L$set$191
	.byte	0x93
	.uleb128 0x1c
	.byte	0x94
	.uleb128 0x1b
	.byte	0x95
	.uleb128 0x1a
	.byte	0x96
	.uleb128 0x19
	.byte	0x97
	.uleb128 0x18
	.byte	0x4
	.set L$set$192,LCFI137-LCFI136
	.long L$set$192
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
	.set L$set$193,LCFI138-LCFI137
	.long L$set$193
	.byte	0xb
	.byte	0x4
	.set L$set$194,LCFI139-LCFI138
	.long L$set$194
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
	.set L$set$195,LCFI140-LCFI139
	.long L$set$195
	.byte	0xb
	.byte	0x4
	.set L$set$196,LCFI141-LCFI140
	.long L$set$196
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
	.set L$set$197,LCFI142-LCFI141
	.long L$set$197
	.byte	0xb
	.byte	0x4
	.set L$set$198,LCFI143-LCFI142
	.long L$set$198
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
	.set L$set$199,LCFI144-LCFI143
	.long L$set$199
	.byte	0xb
	.byte	0x4
	.set L$set$200,LCFI145-LCFI144
	.long L$set$200
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
	.set L$set$201,LCFI146-LCFI145
	.long L$set$201
	.byte	0xb
	.byte	0x4
	.set L$set$202,LCFI147-LCFI146
	.long L$set$202
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
	.set L$set$203,LCFI148-LCFI147
	.long L$set$203
	.byte	0xb
	.byte	0x4
	.set L$set$204,LCFI149-LCFI148
	.long L$set$204
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
	.set L$set$205,LCFI150-LCFI149
	.long L$set$205
	.byte	0xb
	.byte	0x4
	.set L$set$206,LCFI151-LCFI150
	.long L$set$206
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
	.set L$set$207,LCFI152-LCFI151
	.long L$set$207
	.byte	0xb
	.byte	0x4
	.set L$set$208,LCFI153-LCFI152
	.long L$set$208
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
	.set L$set$209,LCFI154-LCFI153
	.long L$set$209
	.byte	0xb
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$210,LEFDE55-LASFDE55
	.long L$set$210
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB29-.
	.set L$set$211,LFE29-LFB29
	.quad L$set$211
	.uleb128 0
	.byte	0x4
	.set L$set$212,LCFI155-LFB29
	.long L$set$212
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$213,LCFI156-LCFI155
	.long L$set$213
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$214,LCFI157-LCFI156
	.long L$set$214
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$215,LCFI158-LCFI157
	.long L$set$215
	.byte	0xb
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$216,LEFDE57-LASFDE57
	.long L$set$216
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB30-.
	.set L$set$217,LFE30-LFB30
	.quad L$set$217
	.uleb128 0
	.byte	0x4
	.set L$set$218,LCFI159-LFB30
	.long L$set$218
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$219,LCFI160-LCFI159
	.long L$set$219
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$220,LCFI161-LCFI160
	.long L$set$220
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$221,LCFI162-LCFI161
	.long L$set$221
	.byte	0xb
	.align	3
LEFDE57:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
