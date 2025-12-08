	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC10:
	.ascii "aegis_execution.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_aegis_execution__bytes_to_u256:
LFB7:
	movi	v31.4s, 0
	stp	x29, x30, [sp, -48]!
LCFI0:
	mov	x29, sp
LCFI1:
	ldr	w1, [x0, 32]
	stp	q31, q31, [x29, 16]
	tbnz	w1, #31, L11
	add	x6, x29, 16
	cmp	w1, 31
	ble	L3
	mov	x5, 0
L5:
	mov	x3, 0
	add	x4, x0, x5
	mov	w1, 56
	.p2align 5,,15
L4:
	ldrb	w2, [x4], 1
	lsl	x2, x2, x1
	sub	w1, w1, #8
	orr	x3, x3, x2
	cmn	w1, #8
	bne	L4
	sub	x1, x6, x5
	add	x5, x5, 8
	str	x3, [x1, 24]
	cmp	x5, 32
	bne	L5
L3:
	ld1	{v30.16b - v31.16b}, [x6]
	st1	{v30.16b - v31.16b}, [x8]
	ldp	x29, x30, [sp], 48
LCFI2:
	ret
L11:
LCFI3:
	adrp	x0, lC10@PAGE
	mov	w1, 89
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE7:
	.align	2
	.p2align 5,,15
_aegis_execution__u256_to_bytes:
LFB6:
	stp	x29, x30, [sp, -32]!
LCFI4:
	mov	x29, sp
LCFI5:
	mov	x1, 4
	str	x19, [sp, 16]
LCFI6:
	mov	x19, x0
	mov	x0, 40
	bl	_system__secondary_stack__ss_allocate
	mov	x1, x0
	mov	w2, 31
	mov	x6, 0
	add	x7, x19, 24
	add	x0, x0, 8
	stp	xzr, xzr, [x1]
	str	w2, [x1, 4]
	stp	xzr, xzr, [x1, 16]
	str	xzr, [x1, 32]
L14:
	neg	x2, x6
	add	x3, x0, x6
	ldr	x5, [x7, x2]
	mov	w2, 56
	.p2align 5,,15
L13:
	lsr	x4, x5, x2
	sub	w2, w2, #8
	strb	w4, [x3], 1
	cmn	w2, #8
	bne	L13
	add	x6, x6, 8
	cmp	x6, 32
	bne	L14
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI7:
	ret
LFE6:
	.align	2
	.p2align 5,,15
_aegis_execution__storage_key_to_bytes:
LFB5:
	stp	x29, x30, [sp, -192]!
LCFI8:
	mov	x29, sp
LCFI9:
	movi	v31.4s, 0
	stp	x19, x20, [sp, 16]
LCFI10:
	add	x20, x29, 96
	stp	x21, x22, [sp, 32]
LCFI11:
	add	x21, x29, 128
	mov	x22, x0
	mov	x0, 40
	ld1	{v28.16b - v29.16b}, [x1]
	mov	x1, 4
	str	x23, [sp, 48]
LCFI12:
	stp	q31, q31, [x21]
	stp	q31, q31, [x21, 32]
	st1	{v28.16b - v29.16b}, [x20]
	bl	_system__secondary_stack__ss_allocate
	mov	w1, 31
	stp	xzr, xzr, [x0]
	mov	x5, 0
	mov	x19, x0
	add	x6, x29, 160
	add	x23, x0, 8
	str	w1, [x0, 4]
	stp	xzr, xzr, [x0, 16]
	str	xzr, [x0, 32]
	ldp	q31, q30, [x22]
	stp	q31, q30, [x21]
L20:
	add	x1, x20, x5
	sub	x2, x6, x5
	ldr	x4, [x1, 24]
	mov	w1, 56
	.p2align 5,,15
L19:
	lsr	x3, x4, x1
	sub	w1, w1, #8
	strb	w3, [x2], 1
	cmn	w1, #8
	bne	L19
	sub	x5, x5, #8
	cmn	x5, #32
	bne	L20
	adrp	x1, lC7@PAGE
	mov	x0, x21
	add	x1, x1, lC7@PAGEOFF;
	add	x2, x29, 64
	bl	_anubis_sha3__sha3_256
	ldp	q30, q31, [x29, 64]
	mov	x1, x19
	mov	x0, x23
	str	q30, [x19, 8]
	str	q31, [x23, 16]
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 192
LCFI13:
	ret
LFE5:
	.const
	.align	2
lC7:
	.word	0
	.word	63
	.text
	.align	2
	.p2align 5,,15
_aegis_execution__account_balance_key:
LFB22:
	stp	x29, x30, [sp, -112]!
LCFI14:
	mov	x29, sp
LCFI15:
	mov	x1, 4
	stp	x19, x20, [sp, 16]
LCFI16:
	mov	x20, x0
	mov	x0, 40
	stp	xzr, xzr, [x29, 72]
	stp	xzr, xzr, [x29, 88]
	str	xzr, [x29, 104]
	bl	_system__secondary_stack__ss_allocate
	adrp	x1, lC11@PAGE
	mov	x19, x0
	mov	w3, 31
	add	x0, x29, 72
	ldr	d31, [x1, #lC11@PAGEOFF]
	adrp	x1, lC8@PAGE
	add	x2, x29, 32
	stp	xzr, xzr, [x19]
	add	x1, x1, lC8@PAGEOFF;
	str	w3, [x19, 4]
	stp	xzr, xzr, [x19, 16]
	str	xzr, [x19, 32]
	str	d31, [x29, 72]
	ldp	q30, q31, [x20]
	stp	q30, q31, [x29, 80]
	bl	_anubis_sha3__sha3_256
	ldp	q30, q31, [x29, 32]
	add	x0, x19, 8
	mov	x1, x19
	str	q30, [x19, 8]
	str	q31, [x0, 16]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI17:
	ret
LFE22:
	.const
	.align	2
lC8:
	.word	0
	.word	39
	.text
	.align	2
	.p2align 5,,15
_aegis_execution__ensure_state_trie:
LFB4:
	stp	x29, x30, [sp, -32]!
LCFI18:
	mov	x29, sp
LCFI19:
	str	x19, [sp, 16]
LCFI20:
	adrp	x19, _aegis_execution__state_trie_valid@PAGE
	ldrb	w0, [x19, #_aegis_execution__state_trie_valid@PAGEOFF]
	cmp	w0, 1
	bhi	L31
	cbz	w0, L32
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI21:
	ret
	.p2align 2,,3
L32:
LCFI22:
	bl	_khepri_mpt__create_trie
	adrp	x3, _aegis_execution__state_trie@PAGE
	ubfx	x2, x0, 32, 8
	str	w0, [x3, #_aegis_execution__state_trie@PAGEOFF]
	cmp	w2, 1
	bhi	L33
	strb	w2, [x19, #_aegis_execution__state_trie_valid@PAGEOFF]
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI23:
	ret
L31:
LCFI24:
	adrp	x0, lC10@PAGE
	mov	w1, 28
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L33:
	adrp	x0, lC10@PAGE
	mov	w1, 30
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__execution_modeH
_aegis_execution__execution_modeH:
LFB2:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L37
	sub	w1, w1, w2
	cmp	w1, 4
	bgt	L38
L37:
	adrp	x1, _execution_modeG.4@PAGE
	mov	x2, 0
	add	x1, x1, _execution_modeG.4@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L38:
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
	adrp	x1, _execution_modeG.4@PAGE
	sxtw	x2, w2
	add	x1, x1, _execution_modeG.4@PAGEOFF;
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__execution_contextIP
_aegis_execution__execution_contextIP:
LFB3:
	adrp	x1, lC12@PAGE
	movi	v25.4s, 0
	mov	x7, 1103806595072
	movk	x7, 0x101, lsl 48
	mov	w6, 16843009
	stp	xzr, xzr, [x0, 176]
	mov	x3, 0
	add	x2, x0, 432
	add	x5, x0, 368
	mov	w4, 1
	ldr	d24, [x1, #lC12@PAGEOFF]
	stp	xzr, xzr, [x0, 192]
	stp	xzr, xzr, [x0, 208]
	stp	xzr, xzr, [x0, 224]
	stp	q25, q25, [x0, 240]
	str	q25, [x0, 272]
	stp	xzr, xzr, [x0, 288]
	stp	xzr, xzr, [x0, 304]
	stp	xzr, xzr, [x0, 320]
	stp	xzr, xzr, [x0, 336]
	str	x7, [x0, 352]
	str	w6, [x0, 360]
	strb	wzr, [x0, 364]
	.p2align 5,,15
L40:
	add	x1, x3, x3, lsl 1
	add	x3, x3, 1
	add	x2, x2, 192
	add	x1, x5, x1, lsl 6
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	stp	xzr, xzr, [x1, 32]
	stp	xzr, xzr, [x1, 48]
	stp	q25, q25, [x2, -192]
	str	q25, [x2, -160]
	stp	xzr, xzr, [x1, 112]
	stp	xzr, xzr, [x1, 128]
	stp	xzr, xzr, [x1, 144]
	stp	xzr, xzr, [x1, 160]
	strh	wzr, [x1, 176]
	strb	wzr, [x1, 178]
	str	d24, [x2, -77]
	strb	w4, [x2, -69]
	strb	wzr, [x1, 188]
	cmp	x3, 1024
	bne	L40
	add	x3, x0, 196608
	add	x4, x0, 196608
	mov	x2, 0
	add	x3, x3, 464
	add	x4, x4, 432
	.p2align 5,,15
L41:
	movi	v26.4s, 0
	add	x1, x2, x2, lsl 3
	add	x2, x2, 1
	add	x1, x4, x1, lsl 3
	mov	v27.16b, v26.16b
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	st1	{v26.16b - v27.16b}, [x3]
	strb	wzr, [x1, 64]
	add	x3, x3, 72
	cmp	x2, 256
	bne	L41
	add	x3, x0, 212992
	mov	x2, 0
	add	x3, x3, 2488
	.p2align 5,,15
L42:
	add	x1, x2, x2, lsl 1
	add	x2, x2, 1
	lsl	x4, x1, 3
	add	x1, x3, x1, lsl 3
	strh	wzr, [x3, x4]
	str	wzr, [x1, 4]
	str	xzr, [x1, 8]
	strb	wzr, [x1, 16]
	cmp	x2, 1024
	bne	L42
	add	x1, x0, 237568
	add	x5, x0, 237568
	mov	x2, 0
	add	x1, x1, 2536
	add	x5, x5, 2496
	.p2align 5,,15
L43:
	movi	v28.4s, 0
	add	x4, x2, x2, lsl 4
	add	x2, x2, 1
	add	x3, x5, x4, lsl 3
	lsl	x4, x4, 3
	mov	v29.16b, v28.16b
	add	x3, x3, 1
	strb	wzr, [x5, x4]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	st1	{v28.16b - v29.16b}, [x1]
	stp	q28, q28, [x1, 32]
	stp	q28, q28, [x1, 64]
	add	x1, x1, 136
	cmp	x2, 4096
	bne	L43
	add	x1, x0, 794624
	add	x5, x0, 794624
	mov	x2, 0
	add	x1, x1, 2560
	add	x5, x5, 2520
	.p2align 5,,15
L44:
	movi	v30.4s, 0
	add	x4, x2, x2, lsl 4
	add	x2, x2, 1
	add	x3, x5, x4, lsl 3
	lsl	x4, x4, 3
	mov	v31.16b, v30.16b
	add	x3, x3, 1
	strb	wzr, [x5, x4]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	st1	{v30.16b - v31.16b}, [x1]
	stp	q30, q30, [x1, 32]
	stp	q30, q30, [x1, 64]
	add	x1, x1, 136
	cmp	x2, 4096
	bne	L44
	add	x0, x0, 1351680
	mov	x2, 0
	add	x0, x0, 2544
	.p2align 5,,15
L45:
	add	x1, x2, x2, lsl 1
	add	x2, x2, 1
	lsl	x3, x1, 3
	add	x1, x0, x1, lsl 3
	strh	wzr, [x0, x3]
	str	wzr, [x1, 4]
	str	xzr, [x1, 8]
	strb	wzr, [x1, 16]
	cmp	x2, 1024
	bne	L45
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__create_context
_aegis_execution__create_context:
LFB8:
	sub	sp, sp, #2656
LCFI25:
	mov	x15, x2
	sub	sp, sp, #1376256
LCFI26:
	mov	x11, x0
	mov	x14, x3
	mov	x13, x4
	stp	x29, x30, [sp]
LCFI27:
	mov	x29, sp
LCFI28:
	mov	x12, x5
	add	x9, x29, 96
	mov	w10, w6
	mov	x0, x9
	str	x19, [sp, 16]
LCFI29:
	mov	x19, x1
	bl	_aegis_execution__execution_contextIP
	ld1	{v28.16b - v29.16b}, [x15]
	add	x3, x29, 128
	add	x1, x29, 160
	strb	wzr, [x29, 256]
	add	x0, x29, 192
	add	x2, x29, 224
	ld1	{v20.16b - v21.16b}, [x14]
	ld1	{v24.16b - v25.16b}, [x13]
	ld1	{v30.16b - v31.16b}, [x12]
	st1	{v28.16b - v29.16b}, [x3]
	ldp	q26, q27, [x11]
	st1	{v20.16b - v21.16b}, [x1]
	st1	{v24.16b - v25.16b}, [x0]
	st1	{v30.16b - v31.16b}, [x2]
	stp	q26, q27, [x29, 96]
	cmp	w10, 3
	bhi	L70
	add	x0, x29, 1376256
	str	wzr, [x0, 2640]
	strb	w10, [x0, 2644]
	tbnz	x19, #63, L71
	mov	w2, -1000
	add	x1, x29, 56
	movi	v31.4s, 0
	mov	w0, 10000
	cmp	w10, 3
	strb	wzr, [x29, 264]
	st1	{v28.16b - v29.16b}, [x1]
	madd	w0, w10, w2, w0
	mov	w1, 16843009
	ldr	q25, [x29, 72]
	stp	x19, xzr, [x29, 40]
	mov	w2, 7000
	mov	x4, 1103806595072
	movk	x4, 0x101, lsl 48
	adrp	x5, lC12@PAGE
	and	w0, w0, 65535
	csel	w0, w0, w2, ne
	mov	x3, 0
	ldr	q24, [x29, 40]
	add	x2, x9, 432
	add	x16, x29, 448
	str	w1, [x29, 456]
	add	x1, x29, 196608
	add	x15, x29, 480
	stp	q26, q27, [x29, 272]
	stp	q26, q27, [x29, 304]
	stp	q31, q31, [x29, 336]
	stp	x19, xzr, [x29, 368]
	add	x14, x29, 560
	add	x13, x29, 592
	add	x12, x29, 640
	add	x11, x29, 642
	add	x10, x29, 652
	mov	w7, 1
	strh	w0, [x29, 88]
	ldr	x0, [x29, 88]
	strh	wzr, [x1, 464]
	str	wzr, [x1, 468]
	add	x1, x29, 196608
	add	x1, x1, 512
	stp	xzr, xzr, [x29, 384]
	str	q24, [x1, -40]
	str	q28, [x1, -24]
	str	q25, [x1, -8]
	add	x1, x29, 196608
	stp	xzr, xzr, [x29, 400]
	str	x0, [x1, 520]
	add	x0, x29, 229376
	stp	xzr, xzr, [x29, 416]
	str	wzr, [x0, 10776]
	add	x0, x29, 1343488
	stp	xzr, xzr, [x29, 432]
	str	wzr, [x0, 10808]
	str	xzr, [x0, 10816]
	add	x0, x29, 1351680
	str	x4, [x29, 448]
	strb	wzr, [x0, 2632]
	add	x0, x29, 212992
	strb	wzr, [x29, 460]
	str	wzr, [x0, 2576]
	add	x0, x29, 786432
	ldr	d30, [x5, #lC12@PAGEOFF]
	str	wzr, [x0, 10784]
	str	xzr, [x0, 10792]
	add	x0, x29, 794624
	strb	wzr, [x0, 2608]
	.p2align 5,,15
L56:
	add	x0, x3, x3, lsl 1
	add	x3, x3, 1
	add	x2, x2, 192
	lsl	x0, x0, 6
	add	x6, x16, x0
	add	x5, x15, x0
	add	x4, x14, x0
	add	x1, x13, x0
	stp	xzr, xzr, [x6, 16]
	stp	xzr, xzr, [x5, 16]
	stp	xzr, xzr, [x6, 32]
	stp	xzr, xzr, [x5, 32]
	stp	q31, q31, [x2, -192]
	str	q31, [x2, -160]
	strh	wzr, [x12, x0]
	strb	wzr, [x11, x0]
	stp	xzr, xzr, [x4, 16]
	stp	xzr, xzr, [x1, 16]
	stp	xzr, xzr, [x4, 32]
	stp	xzr, xzr, [x1, 32]
	str	d30, [x2, -77]
	strb	w7, [x2, -69]
	strb	wzr, [x10, x0]
	cmp	x3, 1024
	bne	L56
	add	x5, x29, 212992
	add	x4, x29, 212992
	add	x3, x29, 212992
	add	x2, x29, 212992
	mov	x1, 0
	add	x5, x5, 2584
	add	x4, x4, 2588
	add	x3, x3, 2592
	add	x2, x2, 2600
	.p2align 5,,15
L57:
	add	x0, x1, x1, lsl 1
	add	x1, x1, 1
	lsl	x0, x0, 3
	strh	wzr, [x5, x0]
	str	wzr, [x4, x0]
	str	xzr, [x3, x0]
	strb	wzr, [x2, x0]
	cmp	x1, 1024
	bne	L57
	add	x5, x29, 1351680
	add	x4, x29, 1351680
	add	x3, x29, 1351680
	add	x2, x29, 1351680
	mov	x1, 0
	add	x5, x5, 2640
	add	x4, x4, 2644
	add	x3, x3, 2648
	add	x2, x2, 2656
	.p2align 5,,15
L58:
	add	x0, x1, x1, lsl 1
	add	x1, x1, 1
	lsl	x0, x0, 3
	strh	wzr, [x5, x0]
	str	wzr, [x4, x0]
	str	xzr, [x3, x0]
	strb	wzr, [x2, x0]
	cmp	x1, 1024
	bne	L58
	add	x0, x29, 794624
	add	x5, x29, 794624
	add	x4, x29, 794624
	mov	x1, 0
	add	x0, x0, 2656
	add	x5, x5, 2616
	add	x4, x4, 2608
	.p2align 5,,15
L59:
	movi	v22.4s, 0
	add	x3, x1, x1, lsl 4
	add	x1, x1, 1
	lsl	x3, x3, 3
	add	x2, x4, x3
	mov	v23.16b, v22.16b
	add	x2, x2, 9
	strb	wzr, [x5, x3]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	st1	{v22.16b - v23.16b}, [x0]
	stp	q22, q22, [x0, 32]
	stp	q22, q22, [x0, 64]
	add	x0, x0, 136
	cmp	x1, 4096
	bne	L59
	mov	v31.16b, v22.16b
	add	x0, x9, 237568
	add	x5, x29, 237568
	add	x4, x29, 237568
	mov	x1, 0
	add	x0, x0, 2536
	add	x5, x5, 2592
	add	x4, x4, 2576
	.p2align 5,,15
L60:
	add	x3, x1, x1, lsl 4
	add	x1, x1, 1
	lsl	x3, x3, 3
	add	x2, x4, x3
	add	x2, x2, 17
	strb	wzr, [x5, x3]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	st1	{v22.16b - v23.16b}, [x0]
	stp	q31, q31, [x0, 32]
	stp	q31, q31, [x0, 64]
	add	x0, x0, 136
	cmp	x1, 4096
	bne	L60
	add	x2, x9, 196608
	add	x5, x29, 196608
	add	x4, x29, 196608
	mov	x0, 0
	add	x2, x2, 464
	add	x5, x5, 512
	add	x4, x4, 592
	.p2align 5,,15
L61:
	add	x1, x0, x0, lsl 3
	add	x0, x0, 1
	add	x3, x5, x1, lsl 3
	lsl	x1, x1, 3
	stp	xzr, xzr, [x3, 16]
	stp	xzr, xzr, [x3, 32]
	st1	{v22.16b - v23.16b}, [x2]
	strb	wzr, [x4, x1]
	add	x2, x2, 72
	cmp	x0, 256
	bne	L61
	mov	x2, 2552
	mov	x1, x9
	mov	x0, x8
	movk	x2, 0x15, lsl 16
	bl	_memcpy
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp]
LCFI30:
	add	sp, sp, 2656
LCFI31:
	add	sp, sp, 1376256
LCFI32:
	ret
L70:
LCFI33:
	adrp	x0, lC10@PAGE
	mov	w1, 126
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L71:
	adrp	x0, lC10@PAGE
	mov	w1, 131
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE8:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__enter_call
_aegis_execution__enter_call:
LFB9:
	adrp	x6, lC12@PAGE
	stp	x29, x30, [sp, -208]!
LCFI34:
	add	x8, x0, 196608
	mov	x29, sp
LCFI35:
	mov	w9, 1
	ldr	d31, [x6, #lC12@PAGEOFF]
	mov	x6, x0
	stp	xzr, xzr, [x29, 128]
	ldrsh	w7, [x8, 368]
	stp	xzr, xzr, [x29, 144]
	stp	xzr, xzr, [x29, 160]
	and	w0, w7, 65535
	stp	xzr, xzr, [x29, 176]
	str	d31, [x29, 195]
	strb	w9, [x29, 203]
	cmp	w0, 1024
	bhi	L80
	cmp	w7, 1024
	beq	L78
	ldp	q27, q26, [x1]
	add	x0, x29, 80
	ld1	{v28.16b - v29.16b}, [x3]
	add	x3, x29, 16
	ldp	q30, q31, [x2]
	stp	q27, q26, [x3]
	st1	{v28.16b - v29.16b}, [x0]
	stp	q30, q31, [x29, 48]
	tbnz	x4, #63, L81
	cmp	w5, 4
	bhi	L82
	cmp	w5, 1
	cset	w10, eq
	beq	L83
L77:
	str	x4, [x29, 112]
	sbfiz	x0, x7, 1, 32
	add	x0, x0, w7, sxtw
	ldp	q20, q22, [x3]
	add	x4, x6, x0, lsl 6
	add	w9, w7, 1
	str	xzr, [x29, 120]
	add	x1, x6, 176
	mov	w0, 1
	strh	w7, [x29, 192]
	add	x2, x4, 368
	ldp	q21, q24, [x3, 32]
	strb	w5, [x29, 194]
	ldr	q23, [x3, 64]
	strb	w10, [x29, 204]
	str	q20, [x4, 368]
	ldp	q26, q25, [x3, 80]
	stp	q22, q21, [x2, 16]
	ldp	q28, q27, [x3, 112]
	stp	q24, q23, [x2, 48]
	ldp	q30, q29, [x3, 144]
	stp	q26, q25, [x2, 80]
	ldr	q31, [x3, 176]
	stp	q28, q27, [x2, 112]
	stp	q30, q29, [x2, 144]
	str	q31, [x4, 544]
	strh	w9, [x8, 368]
	str	q20, [x6, 176]
	stp	q22, q21, [x1, 16]
	stp	q24, q23, [x1, 48]
	stp	q26, q25, [x1, 80]
	stp	q28, q27, [x1, 112]
	stp	q30, q29, [x1, 144]
	str	q31, [x6, 352]
	ldp	x29, x30, [sp], 208
LCFI36:
	ret
	.p2align 2,,3
L83:
LCFI37:
	strb	wzr, [x29, 197]
	strb	wzr, [x29, 199]
	str	wzr, [x29, 200]
	b	L77
	.p2align 2,,3
L78:
	mov	w0, 0
	ldp	x29, x30, [sp], 208
LCFI38:
	ret
L80:
LCFI39:
	adrp	x0, lC10@PAGE
	mov	w1, 257
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L82:
	adrp	x0, lC10@PAGE
	mov	w1, 271
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L81:
	adrp	x0, lC10@PAGE
	mov	w1, 266
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE9:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__exit_call
_aegis_execution__exit_call:
LFB10:
	add	x3, x0, 196608
	stp	x29, x30, [sp, -16]!
LCFI40:
	mov	x29, sp
LCFI41:
	mov	x2, x0
	ldrsh	w1, [x3, 368]
	and	w0, w1, 65535
	cmp	w0, 1024
	bhi	L92
	cbz	w1, L90
	ldr	x0, [x2, 280]
	tbnz	x0, #63, L93
	sub	w4, w1, #1
	sxth	w4, w4
	strh	w4, [x3, 368]
	cbz	w4, L84
	sub	w1, w1, #2
	add	x4, x2, 176
	sbfiz	x3, x1, 1, 16
	add	x1, x3, w1, sxth
	add	x1, x2, x1, lsl 6
	add	x3, x1, 368
	ldr	q30, [x3, 80]
	ldp	q26, q29, [x3, 16]
	ldp	q28, q31, [x3, 48]
	ldr	q27, [x1, 368]
	str	q27, [x2, 176]
	ldr	q27, [x3, 96]
	str	q26, [x2, 192]
	ldr	q26, [x3, 112]
	str	q29, [x2, 208]
	ldr	q29, [x3, 128]
	str	q28, [x2, 224]
	ldr	q28, [x3, 144]
	str	q31, [x2, 240]
	ldr	q31, [x3, 160]
	str	q30, [x2, 256]
	ldr	q30, [x3, 176]
	stp	q27, q26, [x4, 96]
	stp	q29, q28, [x4, 128]
	stp	q31, q30, [x4, 160]
	ldp	x29, x30, [sp], 16
LCFI42:
	ret
	.p2align 2,,3
L90:
LCFI43:
	mov	x0, 0
L84:
	ldp	x29, x30, [sp], 16
LCFI44:
	ret
L92:
LCFI45:
	adrp	x0, lC10@PAGE
	mov	w1, 296
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L93:
	adrp	x0, lC10@PAGE
	mov	w1, 297
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE10:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__current_depth
_aegis_execution__current_depth:
LFB11:
	add	x0, x0, 196608
	ldrsh	w0, [x0, 368]
	and	w1, w0, 65535
	cmp	w1, 1024
	bhi	L99
	ret
L99:
	adrp	x0, lC10@PAGE
	stp	x29, x30, [sp, -16]!
LCFI46:
	mov	w1, 311
	mov	x29, sp
LCFI47:
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE11:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__take_snapshot
_aegis_execution__take_snapshot:
LFB12:
	add	x3, x0, 1376256
	stp	x29, x30, [sp, -16]!
LCFI48:
	mov	x29, sp
LCFI49:
	ldr	w1, [x3, 2544]
	tbnz	w1, #31, L107
	cmp	w1, 1023
	bgt	L105
	add	x2, x0, 1343488
	sxth	w4, w1
	ldr	w7, [x2, 10712]
	tbnz	w7, #31, L108
	add	x2, x0, 196608
	ldr	x6, [x2, 384]
	tbnz	x6, #63, L109
	sbfiz	x2, x1, 1, 32
	add	x2, x2, w1, sxtw
	add	x2, x0, x2, lsl 3
	mov	w5, 24
	add	w8, w1, 1
	add	x10, x2, 1351680
	mov	w9, 1
	umaddl	x1, w1, w5, x0
	add	x0, x2, 1343488
	mov	w5, 1
	strh	w4, [x10, 2544]
	str	w7, [x0, 10740]
	str	x6, [x0, 10744]
	mov	x0, 0
	add	x1, x1, 1351680
	bfi	x0, x4, 0, 16
	add	x1, x1, 2544
	strb	w9, [x1, 16]
	bfi	x0, x5, 16, 8
	str	w8, [x3, 2544]
	ldp	x29, x30, [sp], 16
LCFI50:
	ret
	.p2align 2,,3
L105:
LCFI51:
	mov	w4, 0
	mov	x0, 0
	bfi	x0, x4, 0, 16
	mov	w5, 0
	bfi	x0, x5, 16, 8
	ldp	x29, x30, [sp], 16
LCFI52:
	ret
L107:
LCFI53:
	adrp	x0, lC10@PAGE
	mov	w1, 325
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L108:
	adrp	x0, lC10@PAGE
	mov	w1, 332
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L109:
	adrp	x0, lC10@PAGE
	mov	w1, 333
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE12:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__commit_snapshot
_aegis_execution__commit_snapshot:
LFB13:
	stp	x29, x30, [sp, -16]!
LCFI54:
	and	w2, w1, 65535
	mov	x29, sp
LCFI55:
	cmp	w2, 1024
	bhi	L118
	add	x2, x0, 1376256
	ldr	w2, [x2, 2544]
	tbnz	w2, #31, L119
	cmp	w1, 1024
	ccmp	w1, w2, 0, ne
	blt	L120
	ldp	x29, x30, [sp], 16
LCFI56:
	ret
	.p2align 2,,3
L120:
LCFI57:
	sbfiz	x2, x1, 1, 32
	add	x1, x2, w1, sxtw
	add	x0, x0, x1, lsl 3
	add	x1, x0, 1351680
	strb	wzr, [x1, 2560]
	ldp	x29, x30, [sp], 16
LCFI58:
	ret
L119:
LCFI59:
	adrp	x0, lC10@PAGE
	mov	w1, 350
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L118:
	adrp	x0, lC10@PAGE
	mov	w1, 346
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__rollback_to_snapshot
_aegis_execution__rollback_to_snapshot:
LFB14:
	stp	x29, x30, [sp, -16]!
LCFI60:
	and	w2, w1, 65535
	mov	x29, sp
LCFI61:
	cmp	w2, 1024
	bhi	L135
	add	x3, x0, 1376256
	ldr	w2, [x3, 2544]
	tbnz	w2, #31, L136
	cmp	w1, 1024
	ccmp	w1, w2, 0, ne
	blt	L137
L121:
	ldp	x29, x30, [sp], 16
LCFI62:
	ret
	.p2align 2,,3
L137:
LCFI63:
	mov	w2, 24
	sxtw	x4, w1
	umaddl	x2, w1, w2, x0
	add	x2, x2, 1351680
	add	x2, x2, 2544
	ldrb	w2, [x2, 16]
	cmp	w2, 1
	bhi	L138
	cbz	w2, L121
	add	x4, x4, x4, lsl 1
	add	x4, x0, x4, lsl 3
	add	x4, x4, 1343488
	ldr	w5, [x4, 10740]
	tbnz	w5, #31, L139
	add	x2, x0, 1343488
	ldr	x4, [x4, 10744]
	str	w5, [x2, 10712]
	tbnz	x4, #63, L140
	add	x0, x0, 196608
	str	x4, [x0, 384]
	str	w1, [x3, 2544]
	ldp	x29, x30, [sp], 16
LCFI64:
	ret
L135:
LCFI65:
	adrp	x0, lC10@PAGE
	mov	w1, 359
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L136:
	adrp	x0, lC10@PAGE
	mov	w1, 363
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L138:
	adrp	x0, lC10@PAGE
	mov	w1, 365
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L140:
	adrp	x0, lC10@PAGE
	mov	w1, 368
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L139:
	adrp	x0, lC10@PAGE
	mov	w1, 367
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE14:
	.const
	.align	3
lC13:
	.ascii "aegis_execution.ads"
	.space 1
	.align	3
lC14:
	.ascii "failed precondition from aegis_execution.ads:147"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__use_gas
_aegis_execution__use_gas:
LFB15:
	stp	x29, x30, [sp, -16]!
LCFI66:
	mov	x29, sp
LCFI67:
	tbnz	x1, #63, L145
	mov	x3, 34917
	movk	x3, 0x5d63, lsl 16
	movk	x3, 0x46dc, lsl 32
	movk	x3, 0x3, lsl 48
	cmp	x1, x3
	bgt	L146
	add	x0, x0, 196608
	ldp	x29, x30, [sp], 16
LCFI68:
	add	x0, x0, 376
	b	_aegis_gas__consume_gas_discounted
L145:
LCFI69:
	adrp	x0, lC13@PAGE
	mov	w1, 147
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L146:
	adrp	x0, lC14@PAGE
	adrp	x1, lC6@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC6@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE15:
	.const
	.align	2
lC6:
	.word	1
	.word	48
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__gas_remaining
_aegis_execution__gas_remaining:
LFB16:
	add	x0, x0, 196608
	stp	x29, x30, [sp, -16]!
LCFI70:
	mov	x29, sp
LCFI71:
	add	x0, x0, 376
	bl	_aegis_gas__remaining_gas
	tbnz	x0, #63, L150
	ldp	x29, x30, [sp], 16
LCFI72:
	ret
L150:
LCFI73:
	adrp	x0, lC10@PAGE
	mov	w1, 389
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE16:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__refund
_aegis_execution__refund:
LFB17:
	add	x0, x0, 1343488
	stp	x29, x30, [sp, -16]!
LCFI74:
	mov	x29, sp
LCFI75:
	ldr	x3, [x0, 10720]
	orr	x2, x3, x1
	tbnz	x2, #63, L159
	mov	x2, 9223372036854775807
	sub	x4, x2, x3
	cmp	x1, x4
	bgt	L153
	adds	x2, x3, x1
	bvs	L160
L153:
	str	x2, [x0, 10720]
	ldp	x29, x30, [sp], 16
LCFI76:
	ret
L160:
LCFI77:
	adrp	x0, lC10@PAGE
	mov	w1, 399
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L159:
	adrp	x0, lC10@PAGE
	mov	w1, 398
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE17:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__storage_load
_aegis_execution__storage_load:
LFB18:
	stp	x29, x30, [sp, -176]!
LCFI78:
	mov	x29, sp
LCFI79:
LEHB0:
LEHE0:
	add	x4, x29, 176
	stp	x23, x24, [sp, 48]
LCFI80:
	add	x23, x29, 104
	mov	x8, x23
	stp	x19, x20, [sp, 16]
LCFI81:
	mov	x19, x1
	mov	x20, x2
	stp	x21, x22, [sp, 32]
LCFI82:
	mov	x21, x0
	mov	x22, x3
	str	x4, [x29, 128]
LEHB1:
	bl	_system__secondary_stack__ss_mark
	mov	x1, x20
	mov	x0, x19
	bl	_aegis_execution__storage_key_to_bytes
	ldp	w2, w3, [x1]
	mov	x19, x1
	mov	x24, x0
	cmp	w2, 0
	ccmp	w2, w3, 0, lt
	cset	w20, le
	ble	L179
	add	x0, x21, 196608
	mov	x1, 200
	add	x0, x0, 376
	bl	_aegis_gas__consume_gas_discounted
LEHE1:
	cmp	w0, 1
	bhi	L180
	cbnz	w0, L164
L171:
	movi	v30.4s, 0
	mov	v31.16b, v30.16b
	st1	{v30.16b - v31.16b}, [x22]
L165:
	mov	x0, x23
LEHB2:
	bl	_system__secondary_stack__ss_release
	mov	w0, w20
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
LEHE2:
	ldp	x29, x30, [sp], 176
LCFI83:
	ret
	.p2align 2,,3
L164:
LCFI84:
LEHB3:
	bl	_aegis_execution__ensure_state_trie
	adrp	x0, _aegis_execution__state_trie_valid@PAGE
	ldrb	w0, [x0, #_aegis_execution__state_trie_valid@PAGEOFF]
	cmp	w0, 1
	bhi	L181
	cbz	w0, L171
	adrp	x0, _aegis_execution__state_trie@PAGE
	ldr	w0, [x0, #_aegis_execution__state_trie@PAGEOFF]
	cmp	w0, 255
	bhi	L182
	add	x21, x29, 136
	mov	x1, x24
	mov	x2, x19
	mov	x3, x21
	bl	_khepri_mpt__get
	ubfx	w1, w0, 8, 8
	and	w0, w0, 255
	cmp	w1, 7
	bhi	L183
	cmp	w0, 1
	bhi	L184
	cmp	w1, 0
	cset	w20, eq
	cmp	w20, 0
	ccmp	w0, 0, 4, ne
	cset	w19, ne
	beq	L171
	add	x20, x29, 64
	mov	x0, x21
	mov	x8, x20
	bl	_aegis_execution__bytes_to_u256
	ld1	{v30.16b - v31.16b}, [x20]
	mov	w20, w19
	st1	{v30.16b - v31.16b}, [x22]
	b	L165
L179:
	adrp	x0, lC10@PAGE
	mov	w1, 419
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L180:
	adrp	x0, lC10@PAGE
	mov	w1, 426
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L181:
	adrp	x0, lC10@PAGE
	mov	w1, 434
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L184:
	adrp	x0, lC10@PAGE
	mov	w1, 449
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L183:
	adrp	x0, lC10@PAGE
	mov	w1, 449
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L182:
	adrp	x0, lC10@PAGE
	mov	w1, 442
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE3:
L173:
	mov	x19, x0
	mov	x0, x23
LEHB4:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE4:
LFE18:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
LLSDA18:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE18-LLSDACSB18
LLSDACSB18:
	.uleb128 LEHB0-LFB18
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB18
	.uleb128 LEHE1-LEHB1
	.uleb128 L173-LFB18
	.uleb128 0
	.uleb128 LEHB2-LFB18
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB3-LFB18
	.uleb128 LEHE3-LEHB3
	.uleb128 L173-LFB18
	.uleb128 0
	.uleb128 LEHB4-LFB18
	.uleb128 LEHE4-LEHB4
	.uleb128 0
	.uleb128 0
LLSDACSE18:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__storage_store
_aegis_execution__storage_store:
LFB20:
	stp	x29, x30, [sp, -160]!
LCFI85:
	mov	x29, sp
LCFI86:
LEHB5:
LEHE5:
	add	x4, x29, 160
	stp	x27, x28, [sp, 80]
LCFI87:
	add	x27, x29, 128
	mov	x8, x27
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI88:
	mov	x21, x0
	mov	x22, x1
	stp	x23, x24, [sp, 48]
LCFI89:
	mov	x23, x2
	mov	x24, x3
	stp	x25, x26, [sp, 64]
LCFI90:
	str	x4, [x29, 152]
LEHB6:
	bl	_system__secondary_stack__ss_mark
	mov	x1, x23
	mov	x0, x22
	bl	_aegis_execution__storage_key_to_bytes
	mov	x19, x1
	mov	x25, x0
	ldp	w0, w1, [x1]
	cmp	w0, 0
	ccmp	w0, w1, 0, lt
	ble	L212
	mov	x0, x24
	bl	_aegis_execution__u256_to_bytes
LEHE6:
	mov	x20, x1
	mov	x26, x0
	ldp	w0, w1, [x1]
	cmp	w0, 0
	ccmp	w0, w1, 0, lt
	cset	w28, le
	ble	L213
	ldrb	w0, [x21, 160]
	cmp	w0, 3
	bhi	L214
	cmp	w0, 1
	bne	L215
L190:
	mov	x0, x27
LEHB7:
	bl	_system__secondary_stack__ss_release
	mov	w0, w28
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
LEHE7:
	ldp	x29, x30, [sp], 160
LCFI91:
	ret
	.p2align 2,,3
L215:
LCFI92:
	add	x0, x21, 196608
	mov	x1, 20000
	add	x0, x0, 376
LEHB8:
	bl	_aegis_gas__consume_gas_discounted
	cmp	w0, 1
	bhi	L216
	cbz	w0, L190
	bl	_aegis_execution__ensure_state_trie
	adrp	x0, _aegis_execution__state_trie_valid@PAGE
	ldrb	w0, [x0, #_aegis_execution__state_trie_valid@PAGEOFF]
	cmp	w0, 1
	bhi	L217
	cbz	w0, L190
	adrp	x0, _aegis_execution__state_trie@PAGE
	ldr	w0, [x0, #_aegis_execution__state_trie@PAGEOFF]
	cmp	w0, 255
	bhi	L218
	mov	x3, x26
	mov	x4, x20
	mov	x1, x25
	mov	x2, x19
	bl	_khepri_mpt__put
	add	x6, x21, 1343488
	and	w4, w0, 255
	ldr	w2, [x6, 10712]
	ubfx	w0, w0, 8, 8
	tbnz	w2, #31, L219
	cmp	w4, 1
	bhi	L220
	cmp	w2, 4095
	ccmp	w4, 0, 4, le
	beq	L197
	ldp	q27, q26, [x22]
	sxtw	x3, w2
	sbfiz	x1, x2, 4, 32
	add	x5, x1, x3
	add	x7, x29, 96
	add	x5, x21, x5, lsl 3
	mov	w8, 3
	ld1	{v28.16b - v29.16b}, [x23]
	add	x5, x5, 794624
	ld1	{v30.16b - v31.16b}, [x24]
	stp	q27, q26, [x7]
	strb	w8, [x5, 2520]
	ldp	q26, q27, [x7]
	add	x5, x1, x3
	add	x5, x21, x5, lsl 3
	add	x5, x5, 794624
	add	x5, x5, 2521
	str	q26, [x5]
	str	q27, [x5, 16]
	add	x5, x1, x3
	add	x5, x21, x5, lsl 3
	add	x5, x5, 794624
	add	x5, x5, 2560
	st1	{v28.16b - v29.16b}, [x5]
	add	x5, x1, x3
	add	x5, x21, x5, lsl 3
	add	x5, x5, 786432
	str	xzr, [x5, 10784]
	add	x5, x1, x3
	add	x5, x21, x5, lsl 3
	add	x5, x5, 786432
	str	xzr, [x5, 10792]
	add	x5, x1, x3
	add	x5, x21, x5, lsl 3
	add	x5, x5, 786432
	str	xzr, [x5, 10800]
	add	x5, x1, x3
	add	x5, x21, x5, lsl 3
	add	x5, x5, 786432
	str	xzr, [x5, 10808]
	add	x1, x1, x3
	add	x1, x21, x1, lsl 3
	add	x1, x1, 794624
	add	x1, x1, 2624
	st1	{v30.16b - v31.16b}, [x1]
	add	w1, w2, 1
	str	w1, [x6, 10712]
L197:
	cmp	w0, 7
	bhi	L221
	cmp	w0, 0
	ccmp	w4, 0, 4, eq
	cset	w28, ne
	b	L190
L214:
	adrp	x0, lC10@PAGE
	mov	w1, 474
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L213:
	adrp	x0, lC10@PAGE
	mov	w1, 469
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L212:
	adrp	x0, lC10@PAGE
	mov	w1, 468
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L216:
	adrp	x0, lC10@PAGE
	mov	w1, 481
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L217:
	adrp	x0, lC10@PAGE
	mov	w1, 488
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L218:
	adrp	x0, lC10@PAGE
	mov	w1, 495
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L220:
	adrp	x0, lC10@PAGE
	mov	w1, 503
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L219:
	adrp	x0, lC10@PAGE
	mov	w1, 503
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L221:
	adrp	x0, lC10@PAGE
	mov	w1, 514
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE8:
L200:
	mov	x19, x0
	mov	x0, x27
LEHB9:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE9:
LFE20:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table1:
LLSDA20:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE20-LLSDACSB20
LLSDACSB20:
	.uleb128 LEHB5-LFB20
	.uleb128 LEHE5-LEHB5
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB6-LFB20
	.uleb128 LEHE6-LEHB6
	.uleb128 L200-LFB20
	.uleb128 0
	.uleb128 LEHB7-LFB20
	.uleb128 LEHE7-LEHB7
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB8-LFB20
	.uleb128 LEHE8-LEHB8
	.uleb128 L200-LFB20
	.uleb128 0
	.uleb128 LEHB9-LFB20
	.uleb128 LEHE9-LEHB9
	.uleb128 0
	.uleb128 0
LLSDACSE20:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__get_balance
_aegis_execution__get_balance:
LFB23:
	stp	x29, x30, [sp, -128]!
LCFI93:
	mov	x29, sp
LCFI94:
LEHB10:
LEHE10:
	add	x0, x29, 128
	str	x21, [sp, 32]
LCFI95:
	add	x21, x29, 56
	stp	x19, x20, [sp, 16]
LCFI96:
	mov	x20, x8
	mov	x8, x21
	mov	x19, x1
	str	x0, [x29, 80]
LEHB11:
	bl	_system__secondary_stack__ss_mark
	mov	x0, x19
	bl	_aegis_execution__account_balance_key
	mov	x2, x1
	mov	x1, x0
	ldp	w0, w3, [x2]
	cmp	w0, 0
	ccmp	w0, w3, 0, lt
	ble	L238
	adrp	x0, _aegis_execution__state_trie_valid@PAGE
	ldrb	w0, [x0, #_aegis_execution__state_trie_valid@PAGEOFF]
	cmp	w0, 1
	bhi	L239
	cbz	w0, L230
	adrp	x0, _aegis_execution__state_trie@PAGE
	ldr	w0, [x0, #_aegis_execution__state_trie@PAGEOFF]
	cmp	w0, 255
	bhi	L240
	add	x19, x29, 88
	mov	x3, x19
	bl	_khepri_mpt__get
LEHE11:
	ubfx	w1, w0, 8, 8
	and	w0, w0, 255
	cmp	w1, 7
	bhi	L241
	cmp	w0, 1
	bhi	L242
	cmp	w1, 0
	ccmp	w0, 0, 4, eq
	bne	L243
L230:
	movi	v31.4s, 0
	mov	x0, x21
	stp	q31, q31, [x20]
LEHB12:
	bl	_system__secondary_stack__ss_release
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
LEHE12:
	ldp	x29, x30, [sp], 128
LCFI97:
	ret
	.p2align 2,,3
L243:
LCFI98:
	mov	x8, x20
	mov	x0, x19
LEHB13:
	bl	_aegis_execution__bytes_to_u256
LEHE13:
	mov	x0, x21
LEHB14:
	bl	_system__secondary_stack__ss_release
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
LEHE14:
	ldp	x29, x30, [sp], 128
LCFI99:
	ret
L239:
LCFI100:
	adrp	x0, lC10@PAGE
	mov	w1, 554
	add	x0, x0, lC10@PAGEOFF;
LEHB15:
	bl	___gnat_rcheck_CE_Invalid_Data
L238:
	adrp	x0, lC10@PAGE
	mov	w1, 549
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L240:
	adrp	x0, lC10@PAGE
	mov	w1, 560
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L242:
	adrp	x0, lC10@PAGE
	mov	w1, 567
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L241:
	adrp	x0, lC10@PAGE
	mov	w1, 567
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE15:
L232:
	mov	x19, x0
	mov	x0, x21
LEHB16:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE16:
LFE23:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table2:
LLSDA23:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE23-LLSDACSB23
LLSDACSB23:
	.uleb128 LEHB10-LFB23
	.uleb128 LEHE10-LEHB10
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB11-LFB23
	.uleb128 LEHE11-LEHB11
	.uleb128 L232-LFB23
	.uleb128 0
	.uleb128 LEHB12-LFB23
	.uleb128 LEHE12-LEHB12
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB13-LFB23
	.uleb128 LEHE13-LEHB13
	.uleb128 L232-LFB23
	.uleb128 0
	.uleb128 LEHB14-LFB23
	.uleb128 LEHE14-LEHB14
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB15-LFB23
	.uleb128 LEHE15-LEHB15
	.uleb128 L232-LFB23
	.uleb128 0
	.uleb128 LEHB16-LFB23
	.uleb128 LEHE16-LEHB16
	.uleb128 0
	.uleb128 0
LLSDACSE23:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__transfer_value
_aegis_execution__transfer_value:
LFB25:
	stp	x29, x30, [sp, -400]!
LCFI101:
	mov	x29, sp
LCFI102:
LEHB17:
LEHE17:
	add	x4, x29, 400
	stp	x27, x28, [sp, 80]
LCFI103:
	add	x27, x29, 368
	mov	x8, x27
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI104:
	mov	x21, x0
	mov	x22, x1
	stp	x23, x24, [sp, 48]
LCFI105:
	mov	x23, x2
	mov	x24, x3
	stp	x25, x26, [sp, 64]
LCFI106:
	str	x4, [x29, 392]
LEHB18:
	bl	_system__secondary_stack__ss_mark
	mov	x0, x22
	bl	_aegis_execution__account_balance_key
	mov	x19, x1
	mov	x25, x0
	ldp	w0, w1, [x1]
	cmp	w0, 0
	ccmp	w0, w1, 0, lt
	ble	L270
	mov	x0, x23
	bl	_aegis_execution__account_balance_key
LEHE18:
	mov	x20, x1
	mov	x26, x0
	ldp	w0, w1, [x1]
	cmp	w0, 0
	ccmp	w0, w1, 0, lt
	cset	w28, le
	ble	L271
	ldrb	w0, [x21, 160]
	cmp	w0, 3
	bhi	L272
	cmp	w0, 1
	bne	L273
L249:
	mov	x0, x27
LEHB19:
	bl	_system__secondary_stack__ss_release
	mov	w0, w28
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
LEHE19:
	ldp	x29, x30, [sp], 400
LCFI107:
	ret
	.p2align 2,,3
L273:
LCFI108:
LEHB20:
	bl	_aegis_execution__ensure_state_trie
	adrp	x0, _aegis_execution__state_trie_valid@PAGE
	ldrb	w0, [x0, #_aegis_execution__state_trie_valid@PAGEOFF]
	cmp	w0, 1
	bhi	L274
	cbz	w0, L249
	add	x8, x29, 144
	add	x2, x29, 176
	mov	x1, x22
	mov	x0, x21
	stp	x8, x2, [x29, 128]
	bl	_aegis_execution__get_balance
	ldr	x8, [x29, 128]
	ld1	{v30.16b - v31.16b}, [x8]
	add	x3, x29, 208
	mov	x1, x23
	ldr	x2, [x29, 136]
	mov	x0, x21
	st1	{v30.16b - v31.16b}, [x2]
	stp	x8, x3, [x29, 120]
	bl	_aegis_execution__get_balance
	ldr	x8, [x29, 120]
	ld1	{v30.16b - v31.16b}, [x8]
	ldp	x3, x0, [x29, 128]
	mov	x1, x24
	st1	{v30.16b - v31.16b}, [x3]
	bl	_aegis_u256__less_than
	cmp	w0, 1
	bhi	L275
	cbnz	w0, L249
	add	x0, x29, 240
	mov	x1, x24
	mov	x8, x0
	str	x0, [x29, 120]
	ldr	x0, [x29, 136]
	bl	_aegis_u256__sub_mod
	add	x0, x29, 272
	mov	x1, x24
	mov	x8, x0
	str	x0, [x29, 104]
	ldr	x0, [x29, 128]
	bl	_aegis_u256__add_mod
	adrp	x24, _aegis_execution__state_trie@PAGE
	ldr	w5, [x24, #_aegis_execution__state_trie@PAGEOFF]
	cmp	w5, 255
	bhi	L276
	ldr	x0, [x29, 120]
	str	w5, [x29, 116]
	bl	_aegis_execution__u256_to_bytes
	ldr	w5, [x29, 116]
	mov	x3, x0
	mov	x4, x1
	mov	x2, x19
	mov	x1, x25
	mov	w0, w5
	bl	_khepri_mpt__put
	and	w1, w0, 255
	ubfx	w0, w0, 8, 8
	cmp	w1, 1
	bhi	L277
	cmp	w0, 7
	bhi	L278
	cmp	w0, 0
	eor	w1, w1, 1
	cset	w0, ne
	orr	w0, w0, w1
	tbnz	x0, 0, L249
	ldr	w19, [x24, #_aegis_execution__state_trie@PAGEOFF]
	cmp	w19, 255
	bhi	L279
	ldr	x0, [x29, 104]
	bl	_aegis_execution__u256_to_bytes
	mov	x3, x0
	mov	x4, x1
	mov	x2, x20
	mov	x1, x26
	mov	w0, w19
	bl	_khepri_mpt__put
	and	w1, w0, 255
	ubfx	w0, w0, 8, 8
	cmp	w1, 1
	bhi	L280
	cmp	w0, 7
	bhi	L281
	cmp	w0, 0
	eor	w1, w1, 1
	cset	w0, ne
	orr	w0, w0, w1
	tbnz	x0, 0, L249
	add	x3, x21, 1343488
	ldr	w0, [x3, 10712]
	tbnz	w0, #31, L282
	mov	w1, 2147483647
	cmp	w0, w1
	beq	L283
	cmp	w0, 4094
	bgt	L262
	ldp	q31, q30, [x22]
	sxtw	x4, w0
	sbfiz	x1, x0, 4, 32
	add	x2, x1, x4
	add	x5, x29, 304
	add	x2, x21, x2, lsl 3
	add	x2, x2, 794624
	stp	q31, q30, [x5]
	strb	wzr, [x2, 2520]
	ldp	q30, q31, [x5]
	add	x2, x1, x4
	add	x2, x21, x2, lsl 3
	add	x2, x2, 794624
	add	x2, x2, 2521
	str	q30, [x2]
	str	q31, [x2, 16]
	movi	v30.4s, 0
	add	x2, x1, x4
	add	x2, x21, x2, lsl 3
	add	x2, x2, 794624
	mov	v31.16b, v30.16b
	add	x2, x2, 2560
	st1	{v30.16b - v31.16b}, [x2]
	ldr	x2, [x29, 136]
	ld1	{v28.16b - v29.16b}, [x2]
	add	x2, x1, x4
	add	x2, x21, x2, lsl 3
	add	x2, x2, 794624
	add	x2, x2, 2592
	st1	{v28.16b - v29.16b}, [x2]
	ldr	x2, [x29, 120]
	add	x1, x1, x4
	add	x1, x21, x1, lsl 3
	add	x1, x1, 794624
	add	x1, x1, 2624
	ld1	{v28.16b - v29.16b}, [x2]
	st1	{v28.16b - v29.16b}, [x1]
	add	w2, w0, 1
	add	x5, x29, 336
	sxtw	x4, w2
	sbfiz	x1, x2, 4, 32
	str	w2, [x3, 10712]
	add	x2, x1, x4
	ldp	q29, q28, [x23]
	add	x2, x21, x2, lsl 3
	add	x2, x2, 794624
	stp	q29, q28, [x5]
	strb	wzr, [x2, 2520]
	ldp	q28, q29, [x5]
	add	x2, x1, x4
	add	x2, x21, x2, lsl 3
	add	x2, x2, 794624
	add	x2, x2, 2521
	str	q28, [x2]
	str	q29, [x2, 16]
	add	x2, x1, x4
	add	x2, x21, x2, lsl 3
	add	x2, x2, 794624
	add	x2, x2, 2560
	st1	{v30.16b - v31.16b}, [x2]
	ldr	x2, [x29, 128]
	ld1	{v30.16b - v31.16b}, [x2]
	add	x2, x1, x4
	add	x2, x21, x2, lsl 3
	add	x2, x2, 794624
	add	x2, x2, 2592
	st1	{v30.16b - v31.16b}, [x2]
	ldr	x2, [x29, 104]
	add	x1, x1, x4
	add	x1, x21, x1, lsl 3
	add	x1, x1, 794624
	add	x1, x1, 2624
	ld1	{v30.16b - v31.16b}, [x2]
	st1	{v30.16b - v31.16b}, [x1]
	add	w0, w0, 2
	str	w0, [x3, 10712]
L262:
	mov	w28, 1
	b	L249
L272:
	adrp	x0, lC10@PAGE
	mov	w1, 592
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L271:
	adrp	x0, lC10@PAGE
	mov	w1, 587
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L270:
	adrp	x0, lC10@PAGE
	mov	w1, 586
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L274:
	adrp	x0, lC10@PAGE
	mov	w1, 599
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L275:
	adrp	x0, lC10@PAGE
	mov	w1, 609
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L283:
	adrp	x0, lC10@PAGE
	mov	w1, 647
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L282:
	adrp	x0, lC10@PAGE
	mov	w1, 647
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L277:
	adrp	x0, lC10@PAGE
	mov	w1, 627
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L276:
	adrp	x0, lC10@PAGE
	mov	w1, 620
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L281:
	adrp	x0, lC10@PAGE
	mov	w1, 641
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L280:
	adrp	x0, lC10@PAGE
	mov	w1, 641
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L279:
	adrp	x0, lC10@PAGE
	mov	w1, 634
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L278:
	adrp	x0, lC10@PAGE
	mov	w1, 627
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE20:
L264:
	mov	x19, x0
	mov	x0, x27
LEHB21:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE21:
LFE25:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table3:
LLSDA25:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE25-LLSDACSB25
LLSDACSB25:
	.uleb128 LEHB17-LFB25
	.uleb128 LEHE17-LEHB17
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB18-LFB25
	.uleb128 LEHE18-LEHB18
	.uleb128 L264-LFB25
	.uleb128 0
	.uleb128 LEHB19-LFB25
	.uleb128 LEHE19-LEHB19
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB20-LFB25
	.uleb128 LEHE20-LEHB20
	.uleb128 L264-LFB25
	.uleb128 0
	.uleb128 LEHB21-LFB25
	.uleb128 LEHE21-LEHB21
	.uleb128 0
	.uleb128 0
LLSDACSE25:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__get_caller
_aegis_execution__get_caller:
LFB27:
	ldp	q31, q30, [x0, 176]
	stp	q31, q30, [x8]
	ret
LFE27:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__get_address
_aegis_execution__get_address:
LFB28:
	ldp	q31, q30, [x0, 208]
	stp	q31, q30, [x8]
	ret
LFE28:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__get_call_value
_aegis_execution__get_call_value:
LFB29:
	add	x0, x0, 240
	ld1	{v30.16b - v31.16b}, [x0]
	st1	{v30.16b - v31.16b}, [x8]
	ret
LFE29:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__get_origin
_aegis_execution__get_origin:
LFB30:
	ldp	q31, q30, [x0]
	stp	q31, q30, [x8]
	ret
LFE30:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__get_block_number
_aegis_execution__get_block_number:
LFB31:
	add	x0, x0, 64
	ld1	{v30.16b - v31.16b}, [x0]
	st1	{v30.16b - v31.16b}, [x8]
	ret
LFE31:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__get_timestamp
_aegis_execution__get_timestamp:
LFB32:
	add	x0, x0, 96
	ld1	{v30.16b - v31.16b}, [x0]
	st1	{v30.16b - v31.16b}, [x8]
	ret
LFE32:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__get_chain_id
_aegis_execution__get_chain_id:
LFB33:
	add	x0, x0, 128
	ld1	{v30.16b - v31.16b}, [x0]
	st1	{v30.16b - v31.16b}, [x8]
	ret
LFE33:
	.const
	.align	3
lC16:
	.ascii "failed precondition from aegis_execution.ads:247"
	.align	3
lC17:
	.ascii "aegis_execution.adb:750"
	.align	3
lC18:
	.ascii "Loop_Invariant failed at aegis_execution.adb:771"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__emit_log
_aegis_execution__emit_log:
LFB34:
	mov	x12, 4416
	sub	sp, sp, x12
LCFI109:
	stp	x29, x30, [sp]
LCFI110:
	mov	x29, sp
LCFI111:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	str	x25, [sp, 64]
LCFI112:
	tbnz	w2, #31, L335
	mov	w24, w2
	cmp	w2, 4
	bgt	L336
	add	x23, x29, 160
	mov	x20, x0
	mov	x19, x1
	mov	x2, 4256
	mov	x0, x23
	mov	w1, 0
	mov	x22, x3
	mov	w21, w4
	bl	_memset
	ldrb	w0, [x20, 160]
	cmp	w0, 3
	bhi	L337
	cmp	w0, 1
	beq	L298
	tbnz	w21, #31, L338
	mov	w0, 16960
	movk	w0, 0xf, lsl 16
	cmp	w21, w0
	bgt	L298
	mov	w1, w21
	mov	w0, w24
	bl	_aegis_gas__gas_log
	mov	x1, x0
	tbnz	x0, #63, L339
	mov	x0, 34917
	movk	x0, 0x5d63, lsl 16
	movk	x0, 0x46dc, lsl 32
	movk	x0, 0x3, lsl 48
	cmp	x1, x0
	bgt	L340
	add	x0, x20, 196608
	add	x0, x0, 376
	bl	_aegis_gas__consume_gas_discounted
	cmp	w0, 1
	bhi	L341
	cbz	w0, L298
	ldr	q30, [x20, 208]
	add	x25, x20, 208
	sub	w3, w24, #1
	ldr	q31, [x25, 16]
	stp	q30, q31, [x29, 160]
	cbz	w24, L342
	mov	w6, 0
	add	x7, x23, 32
	mov	w5, 32
	mov	w4, 4256
	b	L304
	.p2align 2,,3
L305:
	add	w6, w6, 1
	add	x7, x7, 32
	cmp	w5, w4
	beq	L343
L304:
	sbfiz	x2, x6, 5, 8
	mov	w1, w5
	add	x0, x19, x2
	add	w5, w5, 32
	ldr	q31, [x19, x2]
	ldr	q30, [x0, 16]
	stp	q31, q30, [x7]
	cmp	w3, w6
	bne	L305
	cmp	w21, 4096
	bgt	L334
	mov	w0, 4256
	sub	w0, w0, w5
	cmp	w0, w21
	bge	L307
L334:
	add	w3, w1, 31
L308:
	add	x19, x29, 96
	mov	x0, x23
	stp	wzr, w3, [x29, 88]
	mov	x2, x19
	add	x1, x29, 88
	bl	_anubis_sha3__sha3_256
	add	x5, x20, 1343488
	ldr	w4, [x5, 10712]
	tbnz	w4, #31, L344
	cmp	w4, 4095
	bgt	L320
	mov	w0, 0
	mov	x3, 0
	mov	x2, x19
	.p2align 5,,15
L319:
	ldrb	w1, [x2], 1
	lsl	x1, x1, x0
	add	w0, w0, 8
	orr	x3, x3, x1
	cmp	w0, 64
	bne	L319
	ldr	q28, [x25, 16]
	mov	x0, 10240
	sbfiz	x1, x4, 4, 32
	movk	x0, 0xc, lsl 16
	add	x1, x1, w4, sxtw
	add	x2, x20, x1, lsl 3
	ldr	q29, [x20, 208]
	add	x6, x0, 528
	mov	w8, 3
	add	x0, x2, x0
	add	x1, x6, x1, lsl 3
	add	x2, x2, 794624
	adrp	x6, lC19@PAGE
	add	x7, x0, 473
	movi	v31.4s, 0
	strb	w8, [x2, 2520]
	add	x20, x20, x1
	add	w4, w4, 1
	ldr	q30, [x6, #lC19@PAGEOFF]
	add	x6, x0, 512
	add	x1, x20, 16
	stp	q29, q28, [x7]
	str	x3, [x0, 512]
	stp	xzr, xzr, [x6, 8]
	str	xzr, [x0, 536]
	str	q31, [x20, 16]
	stp	q31, q30, [x1, 16]
	str	q31, [x20, 64]
	str	w4, [x5, 10712]
L320:
	mov	w0, 1
	b	L296
	.p2align 2,,3
L298:
	mov	w0, 0
L296:
	mov	x12, 4416
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, x12
LCFI113:
	ret
	.p2align 2,,3
L342:
LCFI114:
	mov	w3, 31
	cmp	w21, 4096
	bgt	L308
	mov	w5, 32
L307:
	sub	w3, w21, #1
	cbz	w21, L310
	mov	w0, -1
	mov	w4, 4255
	.p2align 5,,15
L315:
	add	w0, w0, 1
	adds	w1, w0, w5
	bmi	L312
	cmp	w1, w5
	bcc	L312
	cmp	w1, w4
	bhi	L345
	sxth	x2, w0
	ldrb	w2, [x22, x2]
	strb	w2, [x23, w1, sxtw]
	cmp	w3, w0
	bne	L315
	add	w21, w21, w5
	sub	w3, w21, #1
	cmp	w3, w4
	ble	L308
	adrp	x0, lC10@PAGE
	mov	w1, 791
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L343:
	adrp	x0, lC18@PAGE
	adrp	x1, lC6@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC6@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L310:
	sub	w3, w5, #1
	b	L308
L312:
	adrp	x0, lC10@PAGE
	mov	w1, 785
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L337:
	adrp	x0, lC10@PAGE
	mov	w1, 730
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L336:
	adrp	x0, lC16@PAGE
	adrp	x1, lC6@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC6@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L335:
	adrp	x0, lC13@PAGE
	mov	w1, 247
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L345:
	adrp	x0, lC10@PAGE
	mov	w1, 785
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L338:
	adrp	x0, lC10@PAGE
	mov	w1, 737
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L341:
	adrp	x0, lC10@PAGE
	mov	w1, 753
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L339:
	adrp	x0, lC10@PAGE
	mov	w1, 747
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L340:
	adrp	x0, lC17@PAGE
	adrp	x1, lC9@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC9@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L344:
	adrp	x0, lC10@PAGE
	mov	w1, 795
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE34:
	.const
	.align	2
lC9:
	.word	1
	.word	23
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__finalize_success
_aegis_execution__finalize_success:
LFB35:
	add	x1, x0, 196608
	ldr	x1, [x1, 384]
	strb	wzr, [x2]
	tbnz	x1, #63, L351
	mov	w3, 2
	str	x1, [x2, 8]
	stp	xzr, xzr, [x2, 16]
	stp	xzr, xzr, [x2, 32]
	strb	w3, [x0, 168]
	ret
L351:
	adrp	x0, lC10@PAGE
	stp	x29, x30, [sp, -16]!
LCFI115:
	mov	w1, 830
	mov	x29, sp
LCFI116:
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE35:
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__finalize_revert
_aegis_execution__finalize_revert:
LFB36:
	mov	w3, 1
	add	x4, x0, 196608
	ldr	x4, [x4, 384]
	strb	w3, [x2]
	tbnz	x4, #63, L357
	str	x4, [x2, 8]
	mov	w5, 3
	add	x4, x0, 1351680
	ldp	q31, q30, [x1]
	stp	q31, q30, [x2, 16]
	strb	w5, [x0, 168]
	strb	w3, [x4, 2536]
	ret
L357:
	adrp	x0, lC10@PAGE
	stp	x29, x30, [sp, -16]!
LCFI117:
	mov	w1, 842
	mov	x29, sp
LCFI118:
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE36:
	.const
	.align	3
lC20:
	.ascii "failed precondition from aegis_execution.ads:276"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_execution__finalize_failure
_aegis_execution__finalize_failure:
LFB37:
	stp	x29, x30, [sp, -16]!
LCFI119:
	mov	x29, sp
LCFI120:
	cmp	w1, 6
	bhi	L363
	cbz	w1, L364
	add	x3, x0, 196608
	strb	w1, [x2]
	ldr	x1, [x3, 376]
	tbnz	x1, #63, L365
	mov	w4, 4
	add	x3, x0, 1351680
	str	x1, [x2, 8]
	mov	w1, 1
	stp	xzr, xzr, [x2, 16]
	stp	xzr, xzr, [x2, 32]
	strb	w4, [x0, 168]
	strb	w1, [x3, 2536]
	ldp	x29, x30, [sp], 16
LCFI121:
	ret
L363:
LCFI122:
	adrp	x0, lC13@PAGE
	mov	w1, 276
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L365:
	adrp	x0, lC10@PAGE
	mov	w1, 855
	add	x0, x0, lC10@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L364:
	adrp	x0, lC20@PAGE
	adrp	x1, lC6@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC6@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE37:
	.const
	.align	3
_execution_modeG.4:
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	1
	.space 7
	.globl _aegis_execution__execution_modeN
	.align	3
_aegis_execution__execution_modeN:
	.byte	1
	.byte	12
	.byte	23
	.byte	36
	.byte	47
	.space 3
	.globl _aegis_execution__execution_modeS
	.align	3
_aegis_execution__execution_modeS:
	.ascii "MODE_NORMALMODE_STATICMODE_DELEGATEMODE_CREATE"
	.data
_aegis_execution__state_trie_valid:
	.space 1
	.align	2
_aegis_execution__state_trie:
	.space 4
	.globl _aegis_execution_E
	.align	1
_aegis_execution_E:
	.space 2
	.const
	.align	3
lC11:
	.byte	66
	.byte	65
	.byte	76
	.byte	65
	.byte	78
	.byte	67
	.byte	69
	.byte	0
	.align	3
lC12:
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.literal16
	.align	4
lC19:
	.xword	1
	.xword	0
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
	.quad	LFB7-.
	.set L$set$2,LFE7-LFB7
	.quad L$set$2
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$3,LCFI0-LFB7
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
	.quad	LFB6-.
	.set L$set$8,LFE6-LFB6
	.quad L$set$8
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$9,LCFI4-LFB6
	.long L$set$9
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$10,LCFI5-LCFI4
	.long L$set$10
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$11,LCFI6-LCFI5
	.long L$set$11
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$13,LEFDE5-LASFDE5
	.long L$set$13
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB5-.
	.set L$set$14,LFE5-LFB5
	.quad L$set$14
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$15,LCFI8-LFB5
	.long L$set$15
	.byte	0xe
	.uleb128 0xc0
	.byte	0x9d
	.uleb128 0x18
	.byte	0x9e
	.uleb128 0x17
	.byte	0x4
	.set L$set$16,LCFI9-LCFI8
	.long L$set$16
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$17,LCFI10-LCFI9
	.long L$set$17
	.byte	0x93
	.uleb128 0x16
	.byte	0x94
	.uleb128 0x15
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0x95
	.uleb128 0x14
	.byte	0x96
	.uleb128 0x13
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0x97
	.uleb128 0x12
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
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
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$21,LEFDE7-LASFDE7
	.long L$set$21
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB22-.
	.set L$set$22,LFE22-LFB22
	.quad L$set$22
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$23,LCFI14-LFB22
	.long L$set$23
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$24,LCFI15-LCFI14
	.long L$set$24
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$25,LCFI16-LCFI15
	.long L$set$25
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x4
	.set L$set$26,LCFI17-LCFI16
	.long L$set$26
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
	.set L$set$27,LEFDE9-LASFDE9
	.long L$set$27
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB4-.
	.set L$set$28,LFE4-LFB4
	.quad L$set$28
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$29,LCFI18-LFB4
	.long L$set$29
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$30,LCFI19-LCFI18
	.long L$set$30
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$31,LCFI20-LCFI19
	.long L$set$31
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$32,LCFI21-LCFI20
	.long L$set$32
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$33,LCFI22-LCFI21
	.long L$set$33
	.byte	0xb
	.byte	0x4
	.set L$set$34,LCFI23-LCFI22
	.long L$set$34
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$35,LCFI24-LCFI23
	.long L$set$35
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$36,LEFDE11-LASFDE11
	.long L$set$36
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB2-.
	.set L$set$37,LFE2-LFB2
	.quad L$set$37
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$38,LEFDE13-LASFDE13
	.long L$set$38
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB3-.
	.set L$set$39,LFE3-LFB3
	.quad L$set$39
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$40,LEFDE15-LASFDE15
	.long L$set$40
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB8-.
	.set L$set$41,LFE8-LFB8
	.quad L$set$41
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$42,LCFI25-LFB8
	.long L$set$42
	.byte	0xe
	.uleb128 0xa60
	.byte	0x4
	.set L$set$43,LCFI26-LCFI25
	.long L$set$43
	.byte	0xe
	.uleb128 0x150a60
	.byte	0x4
	.set L$set$44,LCFI27-LCFI26
	.long L$set$44
	.byte	0x9d
	.uleb128 0x2a14c
	.byte	0x9e
	.uleb128 0x2a14b
	.byte	0x4
	.set L$set$45,LCFI28-LCFI27
	.long L$set$45
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$46,LCFI29-LCFI28
	.long L$set$46
	.byte	0x93
	.uleb128 0x2a14a
	.byte	0x4
	.set L$set$47,LCFI30-LCFI29
	.long L$set$47
	.byte	0xa
	.byte	0xd3
	.byte	0xdd
	.byte	0xde
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$48,LCFI31-LCFI30
	.long L$set$48
	.byte	0xe
	.uleb128 0x150000
	.byte	0x4
	.set L$set$49,LCFI32-LCFI31
	.long L$set$49
	.byte	0xe
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
	.quad	LFB9-.
	.set L$set$52,LFE9-LFB9
	.quad L$set$52
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$53,LCFI34-LFB9
	.long L$set$53
	.byte	0xe
	.uleb128 0xd0
	.byte	0x9d
	.uleb128 0x1a
	.byte	0x9e
	.uleb128 0x19
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
	.byte	0x4
	.set L$set$57,LCFI38-LCFI37
	.long L$set$57
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$58,LCFI39-LCFI38
	.long L$set$58
	.byte	0xb
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$59,LEFDE19-LASFDE19
	.long L$set$59
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB10-.
	.set L$set$60,LFE10-LFB10
	.quad L$set$60
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$61,LCFI40-LFB10
	.long L$set$61
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$62,LCFI41-LCFI40
	.long L$set$62
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$63,LCFI42-LCFI41
	.long L$set$63
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$64,LCFI43-LCFI42
	.long L$set$64
	.byte	0xb
	.byte	0x4
	.set L$set$65,LCFI44-LCFI43
	.long L$set$65
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$66,LCFI45-LCFI44
	.long L$set$66
	.byte	0xb
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$67,LEFDE21-LASFDE21
	.long L$set$67
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB11-.
	.set L$set$68,LFE11-LFB11
	.quad L$set$68
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$69,LCFI46-LFB11
	.long L$set$69
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$70,LCFI47-LCFI46
	.long L$set$70
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$71,LEFDE23-LASFDE23
	.long L$set$71
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB12-.
	.set L$set$72,LFE12-LFB12
	.quad L$set$72
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$73,LCFI48-LFB12
	.long L$set$73
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$74,LCFI49-LCFI48
	.long L$set$74
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$75,LCFI50-LCFI49
	.long L$set$75
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI51-LCFI50
	.long L$set$76
	.byte	0xb
	.byte	0x4
	.set L$set$77,LCFI52-LCFI51
	.long L$set$77
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$78,LCFI53-LCFI52
	.long L$set$78
	.byte	0xb
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$79,LEFDE25-LASFDE25
	.long L$set$79
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB13-.
	.set L$set$80,LFE13-LFB13
	.quad L$set$80
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$81,LCFI54-LFB13
	.long L$set$81
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$82,LCFI55-LCFI54
	.long L$set$82
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$83,LCFI56-LCFI55
	.long L$set$83
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$84,LCFI57-LCFI56
	.long L$set$84
	.byte	0xb
	.byte	0x4
	.set L$set$85,LCFI58-LCFI57
	.long L$set$85
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$86,LCFI59-LCFI58
	.long L$set$86
	.byte	0xb
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$87,LEFDE27-LASFDE27
	.long L$set$87
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB14-.
	.set L$set$88,LFE14-LFB14
	.quad L$set$88
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$89,LCFI60-LFB14
	.long L$set$89
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$90,LCFI61-LCFI60
	.long L$set$90
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$91,LCFI62-LCFI61
	.long L$set$91
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$92,LCFI63-LCFI62
	.long L$set$92
	.byte	0xb
	.byte	0x4
	.set L$set$93,LCFI64-LCFI63
	.long L$set$93
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$94,LCFI65-LCFI64
	.long L$set$94
	.byte	0xb
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$95,LEFDE29-LASFDE29
	.long L$set$95
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB15-.
	.set L$set$96,LFE15-LFB15
	.quad L$set$96
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$97,LCFI66-LFB15
	.long L$set$97
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$98,LCFI67-LCFI66
	.long L$set$98
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$99,LCFI68-LCFI67
	.long L$set$99
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$100,LCFI69-LCFI68
	.long L$set$100
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$101,LEFDE31-LASFDE31
	.long L$set$101
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB16-.
	.set L$set$102,LFE16-LFB16
	.quad L$set$102
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$103,LCFI70-LFB16
	.long L$set$103
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$104,LCFI71-LCFI70
	.long L$set$104
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$105,LCFI72-LCFI71
	.long L$set$105
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$106,LCFI73-LCFI72
	.long L$set$106
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$107,LEFDE33-LASFDE33
	.long L$set$107
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB17-.
	.set L$set$108,LFE17-LFB17
	.quad L$set$108
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$109,LCFI74-LFB17
	.long L$set$109
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$110,LCFI75-LCFI74
	.long L$set$110
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$111,LCFI76-LCFI75
	.long L$set$111
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$112,LCFI77-LCFI76
	.long L$set$112
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$113,LEFDE35-LASFDE35
	.long L$set$113
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB18-.
	.set L$set$114,LFE18-LFB18
	.quad L$set$114
	.uleb128 0x8
	.quad	LLSDA18-.
	.byte	0x4
	.set L$set$115,LCFI78-LFB18
	.long L$set$115
	.byte	0xe
	.uleb128 0xb0
	.byte	0x9d
	.uleb128 0x16
	.byte	0x9e
	.uleb128 0x15
	.byte	0x4
	.set L$set$116,LCFI79-LCFI78
	.long L$set$116
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$117,LCFI80-LCFI79
	.long L$set$117
	.byte	0x97
	.uleb128 0x10
	.byte	0x98
	.uleb128 0xf
	.byte	0x4
	.set L$set$118,LCFI81-LCFI80
	.long L$set$118
	.byte	0x93
	.uleb128 0x14
	.byte	0x94
	.uleb128 0x13
	.byte	0x4
	.set L$set$119,LCFI82-LCFI81
	.long L$set$119
	.byte	0x95
	.uleb128 0x12
	.byte	0x96
	.uleb128 0x11
	.byte	0x4
	.set L$set$120,LCFI83-LCFI82
	.long L$set$120
	.byte	0xa
	.byte	0xde
	.byte	0xdd
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
	.set L$set$121,LCFI84-LCFI83
	.long L$set$121
	.byte	0xb
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$122,LEFDE37-LASFDE37
	.long L$set$122
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB20-.
	.set L$set$123,LFE20-LFB20
	.quad L$set$123
	.uleb128 0x8
	.quad	LLSDA20-.
	.byte	0x4
	.set L$set$124,LCFI85-LFB20
	.long L$set$124
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$125,LCFI86-LCFI85
	.long L$set$125
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$126,LCFI87-LCFI86
	.long L$set$126
	.byte	0x9b
	.uleb128 0xa
	.byte	0x9c
	.uleb128 0x9
	.byte	0x4
	.set L$set$127,LCFI88-LCFI87
	.long L$set$127
	.byte	0x93
	.uleb128 0x12
	.byte	0x94
	.uleb128 0x11
	.byte	0x95
	.uleb128 0x10
	.byte	0x96
	.uleb128 0xf
	.byte	0x4
	.set L$set$128,LCFI89-LCFI88
	.long L$set$128
	.byte	0x97
	.uleb128 0xe
	.byte	0x98
	.uleb128 0xd
	.byte	0x4
	.set L$set$129,LCFI90-LCFI89
	.long L$set$129
	.byte	0x99
	.uleb128 0xc
	.byte	0x9a
	.uleb128 0xb
	.byte	0x4
	.set L$set$130,LCFI91-LCFI90
	.long L$set$130
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
	.set L$set$131,LCFI92-LCFI91
	.long L$set$131
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$132,LEFDE39-LASFDE39
	.long L$set$132
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB23-.
	.set L$set$133,LFE23-LFB23
	.quad L$set$133
	.uleb128 0x8
	.quad	LLSDA23-.
	.byte	0x4
	.set L$set$134,LCFI93-LFB23
	.long L$set$134
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$135,LCFI94-LCFI93
	.long L$set$135
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$136,LCFI95-LCFI94
	.long L$set$136
	.byte	0x95
	.uleb128 0xc
	.byte	0x4
	.set L$set$137,LCFI96-LCFI95
	.long L$set$137
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x4
	.set L$set$138,LCFI97-LCFI96
	.long L$set$138
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
	.set L$set$139,LCFI98-LCFI97
	.long L$set$139
	.byte	0xb
	.byte	0x4
	.set L$set$140,LCFI99-LCFI98
	.long L$set$140
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
	.set L$set$141,LCFI100-LCFI99
	.long L$set$141
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$142,LEFDE41-LASFDE41
	.long L$set$142
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB25-.
	.set L$set$143,LFE25-LFB25
	.quad L$set$143
	.uleb128 0x8
	.quad	LLSDA25-.
	.byte	0x4
	.set L$set$144,LCFI101-LFB25
	.long L$set$144
	.byte	0xe
	.uleb128 0x190
	.byte	0x9d
	.uleb128 0x32
	.byte	0x9e
	.uleb128 0x31
	.byte	0x4
	.set L$set$145,LCFI102-LCFI101
	.long L$set$145
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$146,LCFI103-LCFI102
	.long L$set$146
	.byte	0x9b
	.uleb128 0x28
	.byte	0x9c
	.uleb128 0x27
	.byte	0x4
	.set L$set$147,LCFI104-LCFI103
	.long L$set$147
	.byte	0x93
	.uleb128 0x30
	.byte	0x94
	.uleb128 0x2f
	.byte	0x95
	.uleb128 0x2e
	.byte	0x96
	.uleb128 0x2d
	.byte	0x4
	.set L$set$148,LCFI105-LCFI104
	.long L$set$148
	.byte	0x97
	.uleb128 0x2c
	.byte	0x98
	.uleb128 0x2b
	.byte	0x4
	.set L$set$149,LCFI106-LCFI105
	.long L$set$149
	.byte	0x99
	.uleb128 0x2a
	.byte	0x9a
	.uleb128 0x29
	.byte	0x4
	.set L$set$150,LCFI107-LCFI106
	.long L$set$150
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
	.set L$set$151,LCFI108-LCFI107
	.long L$set$151
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$152,LEFDE43-LASFDE43
	.long L$set$152
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB27-.
	.set L$set$153,LFE27-LFB27
	.quad L$set$153
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$154,LEFDE45-LASFDE45
	.long L$set$154
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB28-.
	.set L$set$155,LFE28-LFB28
	.quad L$set$155
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$156,LEFDE47-LASFDE47
	.long L$set$156
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB29-.
	.set L$set$157,LFE29-LFB29
	.quad L$set$157
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$158,LEFDE49-LASFDE49
	.long L$set$158
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB30-.
	.set L$set$159,LFE30-LFB30
	.quad L$set$159
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$160,LEFDE51-LASFDE51
	.long L$set$160
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB31-.
	.set L$set$161,LFE31-LFB31
	.quad L$set$161
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$162,LEFDE53-LASFDE53
	.long L$set$162
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB32-.
	.set L$set$163,LFE32-LFB32
	.quad L$set$163
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$164,LEFDE55-LASFDE55
	.long L$set$164
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB33-.
	.set L$set$165,LFE33-LFB33
	.quad L$set$165
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$166,LEFDE57-LASFDE57
	.long L$set$166
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB34-.
	.set L$set$167,LFE34-LFB34
	.quad L$set$167
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$168,LCFI109-LFB34
	.long L$set$168
	.byte	0xe
	.uleb128 0x1140
	.byte	0x4
	.set L$set$169,LCFI110-LCFI109
	.long L$set$169
	.byte	0x9d
	.uleb128 0x228
	.byte	0x9e
	.uleb128 0x227
	.byte	0x4
	.set L$set$170,LCFI111-LCFI110
	.long L$set$170
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$171,LCFI112-LCFI111
	.long L$set$171
	.byte	0x93
	.uleb128 0x226
	.byte	0x94
	.uleb128 0x225
	.byte	0x95
	.uleb128 0x224
	.byte	0x96
	.uleb128 0x223
	.byte	0x97
	.uleb128 0x222
	.byte	0x98
	.uleb128 0x221
	.byte	0x99
	.uleb128 0x220
	.byte	0x4
	.set L$set$172,LCFI113-LCFI112
	.long L$set$172
	.byte	0xa
	.byte	0xd9
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
	.set L$set$173,LCFI114-LCFI113
	.long L$set$173
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$174,LEFDE59-LASFDE59
	.long L$set$174
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB35-.
	.set L$set$175,LFE35-LFB35
	.quad L$set$175
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$176,LCFI115-LFB35
	.long L$set$176
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$177,LCFI116-LCFI115
	.long L$set$177
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$178,LEFDE61-LASFDE61
	.long L$set$178
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB36-.
	.set L$set$179,LFE36-LFB36
	.quad L$set$179
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$180,LCFI117-LFB36
	.long L$set$180
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$181,LCFI118-LCFI117
	.long L$set$181
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$182,LEFDE63-LASFDE63
	.long L$set$182
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB37-.
	.set L$set$183,LFE37-LFB37
	.quad L$set$183
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$184,LCFI119-LFB37
	.long L$set$184
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$185,LCFI120-LCFI119
	.long L$set$185
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$186,LCFI121-LCFI120
	.long L$set$186
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$187,LCFI122-LCFI121
	.long L$set$187
	.byte	0xb
	.align	3
LEFDE63:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
