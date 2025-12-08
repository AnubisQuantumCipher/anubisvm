	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__account_stateIP
_aegis_storage__account_stateIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__change_typeH
_aegis_storage__change_typeH:
LFB3:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L6
	sub	w1, w1, w2
	cmp	w1, 8
	bgt	L7
L6:
	mov	x3, 0
	mov	x0, 0
L4:
	adrp	x2, _change_typeG.16@PAGE
	mov	w1, 43691
	add	x2, x2, _change_typeG.16@PAGEOFF;
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
L7:
	ldrb	w1, [x0, 9]
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
	b	L4
LFE3:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__trie_node_typeH
_aegis_storage__trie_node_typeH:
LFB4:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L11
	sub	w1, w1, w2
	cmp	w1, 4
	bgt	L12
L11:
	adrp	x1, _trie_node_typeG.12@PAGE
	mov	x2, 0
	add	x1, x1, _trie_node_typeG.12@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L12:
	ldrb	w0, [x0, 5]
	mov	w1, 14
	add	w3, w0, w0, lsl 1
	lsl	w4, w0, 3
	sub	w4, w4, w0
	add	w3, w0, w3, lsl 2
	udiv	w0, w4, w1
	udiv	w2, w3, w1
	msub	w0, w0, w1, w4
	sxtw	x0, w0
	msub	w2, w2, w1, w3
	adrp	x1, _trie_node_typeG.12@PAGE
	add	x1, x1, _trie_node_typeG.12@PAGEOFF;
	ldrb	w0, [x1, x0]
	sxtw	x2, w2
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__storage_access_modeH
_aegis_storage__storage_access_modeH:
LFB5:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L16
	sub	w1, w1, w2
	cmp	w1, 7
	bgt	L17
L16:
	adrp	x1, _storage_access_modeG.8@PAGE
	mov	x2, 0
	add	x1, x1, _storage_access_modeG.8@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L17:
	ldrb	w0, [x0, 8]
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
	adrp	x1, _storage_access_modeG.8@PAGE
	sxtw	x2, w2
	add	x1, x1, _storage_access_modeG.8@PAGEOFF;
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__storage_entryIP
_aegis_storage__storage_entryIP:
LFB101:
	ret
LFE101:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__storage_resultH
_aegis_storage__storage_resultH:
LFB7:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L22
	sub	w1, w1, w2
	cmp	w1, 8
	bgt	L23
L22:
	adrp	x1, _storage_resultG.4@PAGE
	mov	x2, 0
	add	x1, x1, _storage_resultG.4@PAGEOFF;
	mov	x0, 0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
	.p2align 2,,3
L23:
	ldrb	w2, [x0, 9]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w0, w2, w2, lsl 1
	add	w2, w2, w2, lsl 3
	umull	x3, w0, w1
	umull	x1, w2, w1
	lsr	x3, x3, 35
	lsr	x1, x1, 35
	add	w5, w3, w3, lsl 2
	add	w4, w1, w1, lsl 2
	add	w3, w3, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w0, w0, w3
	sub	w2, w2, w1
	adrp	x1, _storage_resultG.4@PAGE
	sxtw	x2, w2
	add	x1, x1, _storage_resultG.4@PAGEOFF;
	sxtw	x0, w0
	ldrb	w0, [x1, x0]
	ldrb	w1, [x1, x2]
	add	w0, w0, w1
	and	w0, w0, 3
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__state_changeIP
_aegis_storage__state_changeIP:
LFB8:
	movi	v30.4s, 0
	mov	x1, x0
	add	x2, x0, 40
	strb	wzr, [x1], 1
	stp	xzr, xzr, [x1]
	mov	v31.16b, v30.16b
	stp	xzr, xzr, [x1, 16]
	add	x1, x0, 72
	st1	{v30.16b - v31.16b}, [x2]
	stp	q30, q30, [x1]
	stp	q30, q30, [x1, 32]
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__Tchange_logBIP
_aegis_storage__Tchange_logBIP:
LFB9:
	ldrsh	w3, [x1]
	ldrsh	w6, [x1, 2]
	cmp	w3, w6
	bgt	L25
	mov	w7, 136
	sxtw	x9, w3
	mov	x8, -32
	mov	x4, x9
	smull	x7, w3, w7
	add	x2, x0, 72
	sub	w6, w6, w3
	add	x1, x9, 1
	add	x6, x1, w6, uxth
	sub	x8, x8, x7
	.p2align 5,,15
L27:
	movi	v30.4s, 0
	sub	x1, x4, x9
	add	x5, x2, x7
	add	x1, x1, x1, lsl 4
	add	x5, x5, x8
	add	x4, x4, 1
	add	x3, x0, x1, lsl 3
	lsl	x1, x1, 3
	mov	v31.16b, v30.16b
	add	x3, x3, 1
	strb	wzr, [x0, x1]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	st1	{v30.16b - v31.16b}, [x5]
	stp	q30, q30, [x2]
	stp	q30, q30, [x2, 32]
	add	x2, x2, 136
	cmp	x4, x6
	bne	L27
L25:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__transaction_effectsIP
_aegis_storage__transaction_effectsIP:
LFB10:
	mov	x2, 0
	add	x1, x0, 40
	.p2align 5,,15
L30:
	movi	v30.4s, 0
	add	x4, x2, x2, lsl 4
	add	x2, x2, 1
	add	x3, x0, x4, lsl 3
	lsl	x4, x4, 3
	mov	v31.16b, v30.16b
	add	x3, x3, 1
	strb	wzr, [x0, x4]
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	st1	{v30.16b - v31.16b}, [x1]
	stp	q30, q30, [x1, 32]
	stp	q30, q30, [x1, 64]
	add	x1, x1, 136
	cmp	x2, 4096
	bne	L30
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__Tnibble_pathBIP
_aegis_storage__Tnibble_pathBIP:
LFB11:
	ret
LFE11:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__Tbranch_childrenBIP
_aegis_storage__Tbranch_childrenBIP:
LFB12:
	ret
LFE12:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__trie_nodeD2
_aegis_storage__trie_nodeD2:
LFB13:
	cmp	w0, 1
	cset	w0, ne
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__trie_nodeD3
_aegis_storage__trie_nodeD3:
LFB14:
	cmp	w0, 2
	cset	w0, ne
	ret
LFE14:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__trie_nodeD4
_aegis_storage__trie_nodeD4:
LFB15:
	cmp	w0, 3
	cset	w0, ne
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__trie_nodeEQ
_aegis_storage__trie_nodeEQ:
LFB16:
	mov	x2, x0
	ldrb	w3, [x1]
	ldrb	w0, [x0]
	cmp	w3, w0
	bne	L58
	cmp	w3, 1
	beq	L39
	cmp	w3, 2
	beq	L54
	mov	w0, 1
	cbz	w3, L61
	mov	x0, 4
	b	L41
	.p2align 2,,3
L73:
	cmp	x0, 68
	beq	L68
L41:
	ldrsb	w4, [x2, x0]
	ldrsb	w3, [x1, x0]
	add	x0, x0, 1
	cmp	w4, w3
	beq	L73
	.p2align 5,,15
L58:
	mov	w0, 0
L61:
	ret
	.p2align 2,,3
L39:
	add	x0, x2, 4
	add	x3, x1, 4
	add	x2, x2, 516
	b	L44
	.p2align 2,,3
L75:
	ldr	q31, [x0, 16]
	add	x3, x3, 32
	add	x0, x0, 32
	ldr	q30, [x3, -16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbnz	x4, L58
	cmp	x0, x2
	beq	L74
L44:
	ldr	q31, [x0]
	ldr	q30, [x3]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x4, d31
	cbz	x4, L75
	mov	w0, 0
	b	L61
	.p2align 2,,3
L68:
	ldr	w3, [x1, 68]
	mov	w0, 0
	ldr	w4, [x2, 68]
	cmp	w4, w3
	bne	L61
	ldr	q30, [x1, 72]
	add	x0, x1, 72
	add	x2, x2, 72
	ldr	q31, [x2]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x1, d31
	cbz	x1, L76
L51:
	mov	w0, 1
L52:
	eor	w0, w0, 1
	ret
	.p2align 2,,3
L54:
	mov	x0, 4
	b	L40
	.p2align 2,,3
L77:
	cmp	x0, 68
	beq	L68
L40:
	ldrsb	w4, [x2, x0]
	ldrsb	w3, [x1, x0]
	add	x0, x0, 1
	cmp	w4, w3
	beq	L77
	mov	w0, 0
	b	L61
	.p2align 2,,3
L74:
	add	x1, x1, 516
	ldr	q31, [x0]
	ldr	q30, [x1]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x2, d31
	cbnz	x2, L51
	ldr	q31, [x0, 16]
	ldr	q30, [x1, 16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbz	x0, L52
	mov	w0, 1
	b	L52
	.p2align 2,,3
L76:
	ldr	q30, [x0, 16]
	ldr	q31, [x2, 16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbz	x0, L52
	mov	w0, 1
	b	L52
LFE16:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__trie_nodeIP
_aegis_storage__trie_nodeIP:
LFB20:
	strb	w1, [x0]
	ret
LFE20:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__Tproof_node_dataBIP
_aegis_storage__Tproof_node_dataBIP:
LFB21:
	ret
LFE21:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__proof_nodeIP
_aegis_storage__proof_nodeIP:
LFB103:
	ret
LFE103:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__Tproof_nodes_arrayBIP
_aegis_storage__Tproof_nodes_arrayBIP:
LFB23:
	ret
LFE23:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__merkle_proofIP
_aegis_storage__merkle_proofIP:
LFB105:
	ret
LFE105:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__state_snapshotIP
_aegis_storage__state_snapshotIP:
LFB25:
	strh	wzr, [x0]
	str	wzr, [x0, 4]
	str	xzr, [x0, 8]
	strb	wzr, [x0, 16]
	ret
LFE25:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__Tsnapshot_stackBIP
_aegis_storage__Tsnapshot_stackBIP:
LFB26:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L84
	sxtw	x5, w2
	sub	w1, w1, w2
	add	x4, x5, 1
	mov	x2, x5
	add	x4, x4, x1
	.p2align 5,,15
L86:
	sub	x1, x2, x5
	add	x2, x2, 1
	add	x1, x1, x1, lsl 1
	lsl	x3, x1, 3
	add	x1, x0, x1, lsl 3
	strh	wzr, [x0, x3]
	str	wzr, [x1, 4]
	str	xzr, [x1, 8]
	strb	wzr, [x1, 16]
	cmp	x2, x4
	bne	L86
L84:
	ret
LFE26:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__access_entryIP
_aegis_storage__access_entryIP:
LFB27:
	movi	v30.4s, 0
	add	x1, x0, 32
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	mov	v31.16b, v30.16b
	st1	{v30.16b - v31.16b}, [x1]
	strb	wzr, [x0, 64]
	ret
LFE27:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__Taccess_listBIP
_aegis_storage__Taccess_listBIP:
LFB28:
	ldrsh	w6, [x1]
	ldrsh	w4, [x1, 2]
	cmp	w6, w4
	bgt	L89
	sxtw	x5, w6
	add	x3, x0, 32
	mov	x2, x5
	sub	w4, w4, w6
	add	x1, x5, 1
	add	x4, x1, w4, uxth
	.p2align 5,,15
L91:
	movi	v30.4s, 0
	sub	x1, x2, x5
	add	x2, x2, 1
	add	x1, x1, x1, lsl 3
	add	x1, x0, x1, lsl 3
	mov	v31.16b, v30.16b
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	st1	{v30.16b - v31.16b}, [x3]
	strb	wzr, [x1, 64]
	add	x3, x3, 72
	cmp	x2, x4
	bne	L91
L89:
	ret
LFE28:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__access_setIP
_aegis_storage__access_setIP:
LFB29:
	mov	x2, 0
	add	x3, x0, 32
	.p2align 5,,15
L94:
	movi	v30.4s, 0
	add	x1, x2, x2, lsl 3
	add	x2, x2, 1
	add	x1, x0, x1, lsl 3
	mov	v31.16b, v30.16b
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	st1	{v30.16b - v31.16b}, [x3]
	strb	wzr, [x1, 64]
	add	x3, x3, 72
	cmp	x2, 256
	bne	L94
	ret
LFE29:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__key_to_nibbles
_aegis_storage__key_to_nibbles:
LFB30:
	ldp	q31, q28, [x0]
	movi	v29.16b, 0xf
	ushr	v30.16b, v31.16b, 4
	and	v31.16b, v31.16b, v29.16b
	st2	{v30.16b - v31.16b}, [x8], 32
	and	v31.16b, v28.16b, v29.16b
	ushr	v30.16b, v28.16b, 4
	st2	{v30.16b - v31.16b}, [x8]
	ret
LFE30:
	.const
	.align	3
lC5:
	.ascii "aegis_storage.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__is_empty_account
_aegis_storage__is_empty_account:
LFB31:
	ldp	x4, x1, [x0, 64]
	cbnz	x1, L100
	ldrb	w2, [x0, 144]
	cmp	w2, 1
	bhi	L100
	add	x3, x0, 32
	ldr	x1, [x0, 32]
	eor	w2, w2, 1
	ldp	x6, x5, [x3, 8]
	ldr	x0, [x3, 24]
	orr	x1, x1, x6
	orr	x1, x1, x5
	orr	x1, x1, x0
	cmp	x1, 0
	cset	w1, eq
	cmp	x4, 0
	cset	w0, eq
	and	w0, w0, w1
	and	w0, w0, w2
	ret
L100:
	adrp	x0, lC5@PAGE
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	w1, 29
	mov	x29, sp
LCFI1:
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE31:
	.align	2
	.p2align 5,,15
	.globl _aegis_storage__compute_slot_key
_aegis_storage__compute_slot_key:
LFB33:
	stp	x29, x30, [sp, -160]!
LCFI2:
	mov	x2, x0
	mov	x29, sp
LCFI3:
	add	x6, x29, 32
	add	x0, x29, 96
	mov	x5, 0
	add	x7, x29, 128
	stp	x19, x20, [sp, 16]
LCFI4:
	mov	x20, x8
	ld1	{v28.16b - v29.16b}, [x1]
	stp	xzr, xzr, [x29, 128]
	stp	xzr, xzr, [x29, 144]
	ldp	q31, q30, [x2]
	st1	{v28.16b - v29.16b}, [x6]
	stp	q31, q30, [x0]
L109:
	add	x1, x6, x5
	sub	x2, x7, x5
	ldr	x4, [x1, 24]
	mov	w1, 56
	.p2align 5,,15
L108:
	lsr	x3, x4, x1
	sub	w1, w1, #8
	strb	w3, [x2], 1
	cmn	w1, #8
	bne	L108
	sub	x5, x5, #8
	cmn	x5, #32
	bne	L109
	add	x19, x29, 64
	adrp	x1, lC3@PAGE
	add	x1, x1, lC3@PAGEOFF;
	mov	x2, x19
	bl	_anubis_sha3__keccak_256
	ldp	q31, q30, [x19]
	stp	q31, q30, [x20]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 160
LCFI5:
	ret
LFE33:
	.const
	.align	2
lC3:
	.word	0
	.word	63
	.text
	.const
	.align	3
_storage_resultG.4:
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	2
	.byte	0
	.space 5
	.align	3
_storage_access_modeG.8:
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
	.align	3
_trie_node_typeG.12:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.byte	3
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.space 2
_change_typeG.16:
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	5
	.globl _aegis_storage__storage_resultN
	.align	3
_aegis_storage__storage_resultN:
	.byte	1
	.byte	11
	.byte	28
	.byte	49
	.byte	67
	.space 3
	.globl _aegis_storage__storage_resultS
	.align	3
_aegis_storage__storage_resultS:
	.ascii "STORAGE_OKSTORAGE_NOT_FOUNDSTORAGE_ACCESS_DENIEDSTORAGE_OUT_OF_GAS"
	.globl _aegis_storage__storage_access_modeN
	.align	3
_aegis_storage__storage_access_modeN:
	.byte	1
	.byte	12
	.byte	24
	.byte	35
	.byte	46
	.space 3
	.globl _aegis_storage__storage_access_modeS
	.align	3
_aegis_storage__storage_access_modeS:
	.ascii "ACCESS_READACCESS_WRITEACCESS_COLDACCESS_WARM"
	.globl _aegis_storage__trie_node_typeN
	.align	3
_aegis_storage__trie_node_typeN:
	.byte	1
	.byte	10
	.byte	21
	.byte	35
	.byte	44
	.space 3
	.globl _aegis_storage__trie_node_typeS
	.align	3
_aegis_storage__trie_node_typeS:
	.ascii "NODE_NULLNODE_BRANCHNODE_EXTENSIONNODE_LEAF"
	.globl _aegis_storage__change_typeN
	.align	3
_aegis_storage__change_typeN:
	.byte	1
	.byte	15
	.byte	27
	.byte	38
	.byte	52
	.byte	65
	.byte	79
	.space 1
	.globl _aegis_storage__change_typeS
	.align	3
_aegis_storage__change_typeS:
	.ascii "CHANGE_BALANCECHANGE_NONCECHANGE_CODECHANGE_STORAGECHANGE_CREATECHANGE_DESTROY"
	.globl _aegis_storage__empty_account
	.align	4
_aegis_storage__empty_account:
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.xword	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	1
	.space 14
	.globl _aegis_storage_E
	.data
	.align	1
_aegis_storage_E:
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
	.quad	LFB101-.
	.set L$set$10,LFE101-LFB101
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
	.quad	LFB10-.
	.set L$set$18,LFE10-LFB10
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
	.quad	LFB16-.
	.set L$set$30,LFE16-LFB16
	.quad L$set$30
	.uleb128 0
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$31,LEFDE31-LASFDE31
	.long L$set$31
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB20-.
	.set L$set$32,LFE20-LFB20
	.quad L$set$32
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$33,LEFDE33-LASFDE33
	.long L$set$33
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB21-.
	.set L$set$34,LFE21-LFB21
	.quad L$set$34
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$35,LEFDE35-LASFDE35
	.long L$set$35
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB103-.
	.set L$set$36,LFE103-LFB103
	.quad L$set$36
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$37,LEFDE37-LASFDE37
	.long L$set$37
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB23-.
	.set L$set$38,LFE23-LFB23
	.quad L$set$38
	.uleb128 0
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$39,LEFDE39-LASFDE39
	.long L$set$39
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB105-.
	.set L$set$40,LFE105-LFB105
	.quad L$set$40
	.uleb128 0
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$41,LEFDE41-LASFDE41
	.long L$set$41
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB25-.
	.set L$set$42,LFE25-LFB25
	.quad L$set$42
	.uleb128 0
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$43,LEFDE43-LASFDE43
	.long L$set$43
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB26-.
	.set L$set$44,LFE26-LFB26
	.quad L$set$44
	.uleb128 0
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$45,LEFDE45-LASFDE45
	.long L$set$45
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB27-.
	.set L$set$46,LFE27-LFB27
	.quad L$set$46
	.uleb128 0
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$47,LEFDE47-LASFDE47
	.long L$set$47
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB28-.
	.set L$set$48,LFE28-LFB28
	.quad L$set$48
	.uleb128 0
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$49,LEFDE49-LASFDE49
	.long L$set$49
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB29-.
	.set L$set$50,LFE29-LFB29
	.quad L$set$50
	.uleb128 0
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$51,LEFDE51-LASFDE51
	.long L$set$51
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB30-.
	.set L$set$52,LFE30-LFB30
	.quad L$set$52
	.uleb128 0
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$53,LEFDE53-LASFDE53
	.long L$set$53
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB31-.
	.set L$set$54,LFE31-LFB31
	.quad L$set$54
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI0-LFB31
	.long L$set$55
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$56,LCFI1-LCFI0
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$57,LEFDE55-LASFDE55
	.long L$set$57
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB33-.
	.set L$set$58,LFE33-LFB33
	.quad L$set$58
	.uleb128 0
	.byte	0x4
	.set L$set$59,LCFI2-LFB33
	.long L$set$59
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$60,LCFI3-LCFI2
	.long L$set$60
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$61,LCFI4-LCFI3
	.long L$set$61
	.byte	0x93
	.uleb128 0x12
	.byte	0x94
	.uleb128 0x11
	.byte	0x4
	.set L$set$62,LCFI5-LCFI4
	.long L$set$62
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE55:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
