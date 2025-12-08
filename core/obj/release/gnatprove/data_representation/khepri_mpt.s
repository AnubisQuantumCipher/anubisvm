	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC8:
	.ascii "khepri_mpt.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
_khepri_mpt__allocate_node:
LFB14:
	stp	x29, x30, [sp, -32]!
LCFI0:
	adrp	x4, _khepri_mpt__node_store@PAGE
	mov	x29, sp
LCFI1:
	mov	x5, 16960
	mov	x3, 0
	mov	x1, x0
	add	x4, x4, _khepri_mpt__node_store@PAGEOFF;
	movk	x5, 0xf, lsl 16
	str	x19, [sp, 16]
LCFI2:
	b	L4
	.p2align 2,,3
L12:
	add	x3, x3, 1
	cmp	x3, x5
	beq	L10
L4:
	add	x2, x3, x3, lsl 2
	add	x2, x3, x2, lsl 2
	add	x2, x3, x2, lsl 3
	add	x2, x4, x2, lsl 2
	ldrb	w2, [x2, 672]
	cmp	w2, 1
	bhi	L11
	cbnz	w2, L12
	sbfiz	x2, x3, 2, 32
	add	x2, x2, w3, sxtw
	lsl	x2, x2, 2
	add	x2, x2, w3, sxtw
	lsl	x2, x2, 3
	add	x2, x2, w3, sxtw
	add	x4, x4, x2, lsl 2
	mov	x2, 672
	mov	w19, w3
	mov	x0, x4
	bl	_memcpy
	mov	x4, x0
	mov	w0, 1
	mov	w1, 1
	strb	w0, [x4, 672]
	mov	x0, 0
	bfi	x0, x19, 0, 32
	bfi	x0, x1, 32, 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI3:
	ret
L10:
LCFI4:
	mov	w19, 0
	mov	x0, 0
	bfi	x0, x19, 0, 32
	mov	w1, 0
	bfi	x0, x1, 32, 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI5:
	ret
L11:
LCFI6:
	adrp	x0, lC8@PAGE
	mov	w1, 72
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE14:
	.align	2
	.p2align 5,,15
_khepri_mpt__encode_length:
LFB18:
	stp	x29, x30, [sp, -16]!
LCFI7:
	mov	x29, sp
LCFI8:
	mov	w7, w0
	ldp	w5, w9, [x3]
	adds	w3, w5, w4
	sxtw	x8, w5
	cset	w6, vs
	cmp	w0, 55
	bgt	L16
	cbnz	w6, L45
	cmp	w9, w3
	ccmp	w5, w3, 0, ge
	bgt	L46
	sxtw	x3, w3
	add	w1, w1, w0
	sub	x3, x3, x8
	mov	w5, 2147483647
	add	w0, w4, 1
	strb	w1, [x2, x3]
	cmp	w4, w5
	beq	L47
L13:
	ldp	x29, x30, [sp], 16
LCFI9:
	ret
	.p2align 2,,3
L16:
LCFI10:
	cmp	w0, 255
	bgt	L21
	cbnz	w6, L48
	cmp	w9, w3
	ccmp	w5, w3, 0, ge
	bgt	L49
	sxtw	x0, w3
	add	w1, w1, 56
	sub	x0, x0, x8
	mov	w5, 2147483647
	strb	w1, [x2, x0]
	cmp	w3, w5
	beq	L50
	add	w3, w3, 1
	cmp	w9, w3
	blt	L51
	sxtw	x3, w3
	mov	w1, 2147483646
	sub	x3, x3, x8
	add	w0, w4, 2
	strb	w7, [x2, x3]
	cmp	w4, w1
	blt	L13
	adrp	x0, lC8@PAGE
	mov	w1, 182
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L21:
	mov	w0, 65535
	cmp	w7, w0
	ble	L52
	cbnz	w6, L53
	cmp	w9, w3
	ccmp	w5, w3, 0, ge
	bgt	L54
	sxtw	x0, w3
	add	w1, w1, 58
	sub	x0, x0, x8
	mov	w5, 2147483647
	strb	w1, [x2, x0]
	cmp	w3, w5
	beq	L55
	add	w0, w3, 1
	cmp	w9, w0
	blt	L56
	sxtw	x0, w0
	lsr	w5, w7, 16
	sub	x0, x0, x8
	mov	w1, 2147483646
	strb	w5, [x2, x0]
	cmp	w3, w1
	beq	L57
	add	w0, w3, 2
	cmp	w9, w0
	blt	L58
	sxtw	x0, w0
	lsr	w5, w7, 8
	sub	x0, x0, x8
	sub	w1, w1, #1
	strb	w5, [x2, x0]
	cmp	w3, w1
	beq	L59
	add	w3, w3, 3
	cmp	w9, w3
	blt	L60
	sxtw	x3, w3
	mov	w1, 2147483644
	sub	x3, x3, x8
	add	w0, w4, 4
	strb	w7, [x2, x3]
	cmp	w4, w1
	blt	L13
	adrp	x0, lC8@PAGE
	mov	w1, 198
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L52:
	cbnz	w6, L61
	cmp	w9, w3
	ccmp	w5, w3, 0, ge
	bgt	L62
	sxtw	x0, w3
	add	w1, w1, 57
	sub	x0, x0, x8
	mov	w5, 2147483647
	strb	w1, [x2, x0]
	cmp	w3, w5
	beq	L63
	add	w0, w3, 1
	cmp	w9, w0
	blt	L64
	sxtw	x0, w0
	ubfx	x5, x7, 8, 8
	sub	x0, x0, x8
	mov	w1, 2147483646
	strb	w5, [x2, x0]
	cmp	w3, w1
	beq	L65
	add	w3, w3, 2
	cmp	w9, w3
	blt	L66
	sxtw	x3, w3
	mov	w1, 2147483644
	sub	x3, x3, x8
	add	w0, w4, 3
	strb	w7, [x2, x3]
	cmp	w4, w1
	ble	L13
	adrp	x0, lC8@PAGE
	mov	w1, 189
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L45:
	adrp	x0, lC8@PAGE
	mov	w1, 177
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L46:
	adrp	x0, lC8@PAGE
	mov	w1, 177
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L49:
	adrp	x0, lC8@PAGE
	mov	w1, 180
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L47:
	adrp	x0, lC8@PAGE
	mov	w1, 178
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L62:
	adrp	x0, lC8@PAGE
	mov	w1, 184
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L51:
	adrp	x0, lC8@PAGE
	mov	w1, 181
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L48:
	adrp	x0, lC8@PAGE
	mov	w1, 180
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L50:
	adrp	x0, lC8@PAGE
	mov	w1, 181
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L54:
	adrp	x0, lC8@PAGE
	mov	w1, 191
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L66:
	adrp	x0, lC8@PAGE
	mov	w1, 187
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L65:
	adrp	x0, lC8@PAGE
	mov	w1, 187
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L61:
	adrp	x0, lC8@PAGE
	mov	w1, 184
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L64:
	adrp	x0, lC8@PAGE
	mov	w1, 185
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L63:
	adrp	x0, lC8@PAGE
	mov	w1, 185
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L53:
	adrp	x0, lC8@PAGE
	mov	w1, 191
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L55:
	adrp	x0, lC8@PAGE
	mov	w1, 192
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L57:
	adrp	x0, lC8@PAGE
	mov	w1, 194
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L56:
	adrp	x0, lC8@PAGE
	mov	w1, 192
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L60:
	adrp	x0, lC8@PAGE
	mov	w1, 196
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L59:
	adrp	x0, lC8@PAGE
	mov	w1, 196
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L58:
	adrp	x0, lC8@PAGE
	mov	w1, 194
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE18:
	.align	2
	.p2align 5,,15
_khepri_mpt__encode_string:
LFB19:
	stp	x29, x30, [sp, -64]!
LCFI11:
	mov	x29, sp
LCFI12:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI13:
	mov	x21, x2
	mov	x22, x0
	stp	x23, x24, [sp, 48]
LCFI14:
	mov	x23, x1
	mov	x24, x3
	ldp	w1, w6, [x1]
	ldp	w5, w2, [x3]
	sxtw	x19, w1
	sxtw	x20, w5
	cmp	w1, w6
	beq	L109
	sxtw	x7, w6
	add	x0, x19, 54
	cmp	x0, x7
	blt	L78
L70:
	adds	w3, w5, w4
	bvs	L80
	cmp	w2, w3
	ccmp	w5, w3, 0, ge
	bgt	L110
	cmp	w1, w6
	bgt	L83
	sub	x0, x7, x19
	add	x0, x0, 1
	cmp	x0, 255
	bgt	L111
	sxtw	x1, w3
	sub	w0, w0, #128
	sub	x1, x1, x20
	mov	w8, 2147483647
	strb	w0, [x21, x1]
	cmp	w4, w8
	beq	L85
	sub	x7, x22, x19
	add	w0, w4, 1
	add	x7, x7, 1
	sub	x3, x19, #1
	sxtw	x6, w6
	.p2align 5,,15
L91:
	adds	w1, w5, w0
	bvs	L87
	cmp	w5, w1
	ccmp	w2, w1, 1, le
	blt	L112
	ldrb	w4, [x7, x3]
	sxtw	x1, w1
	sub	x1, x1, x20
	strb	w4, [x21, x1]
	cmp	w0, w8
	beq	L113
	add	x3, x3, 1
	add	w0, w0, 1
	cmp	x6, x3
	bne	L91
L67:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 64
LCFI15:
	ret
	.p2align 2,,3
L78:
LCFI16:
	mov	w0, 0
	cmp	w1, w6
	ble	L71
L92:
	mov	x2, x21
	mov	w1, 183
	mov	x3, x24
	bl	_khepri_mpt__encode_length
	ldp	w1, w2, [x23]
	cmp	w1, w2
	bgt	L67
	ldp	w3, w7, [x24]
	sxtw	x1, w1
	sub	x6, x22, x19
	sub	x4, x1, #1
	add	x6, x6, 1
	sxtw	x2, w2
	mov	w8, 2147483647
	.p2align 5,,15
L100:
	adds	w1, w3, w0
	bvs	L95
	cmp	w3, w1
	bgt	L97
	cmp	w7, w1
	blt	L97
	ldrb	w5, [x6, x4]
	sxtw	x1, w1
	sub	x1, x1, x20
	strb	w5, [x21, x1]
	cmp	w0, w8
	beq	L114
	add	x4, x4, 1
	add	w0, w0, 1
	cmp	x2, x4
	bne	L100
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 64
LCFI17:
	ret
	.p2align 2,,3
L109:
LCFI18:
	ldrb	w3, [x0]
	tbnz	x3, 7, L115
	adds	w0, w5, w4
	bvs	L73
	cmp	w5, w0
	ccmp	w2, w0, 1, le
	blt	L116
	sxtw	x0, w0
	mov	w1, 2147483647
	sub	x0, x0, x20
	strb	w3, [x21, x0]
	cmp	w4, w1
	beq	L117
L76:
	add	w0, w4, 1
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 64
LCFI19:
	ret
	.p2align 2,,3
L83:
LCFI20:
	sxtw	x0, w3
	mov	w2, -128
	sub	x0, x0, x20
	mov	w1, 2147483647
	strb	w2, [x21, x0]
	cmp	w4, w1
	bne	L76
L85:
	adrp	x0, lC8@PAGE
	mov	w1, 216
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L115:
	add	x0, x19, 54
	mov	x7, x19
	cmp	x0, x19
	bge	L70
L71:
	sub	x7, x7, x19
	mov	x1, 2147483647
	add	x0, x7, 1
	cmp	x0, x1
	bls	L92
	adrp	x0, lC8@PAGE
	mov	w1, 223
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L97:
	adrp	x0, lC8@PAGE
	mov	w1, 225
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L112:
	adrp	x0, lC8@PAGE
	mov	w1, 218
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L95:
	adrp	x0, lC8@PAGE
	mov	w1, 225
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L87:
	adrp	x0, lC8@PAGE
	mov	w1, 218
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L113:
	adrp	x0, lC8@PAGE
	mov	w1, 219
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L114:
	adrp	x0, lC8@PAGE
	mov	w1, 226
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L111:
	adrp	x0, lC8@PAGE
	mov	w1, 215
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L73:
	adrp	x0, lC8@PAGE
	mov	w1, 210
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L116:
	adrp	x0, lC8@PAGE
	mov	w1, 210
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L117:
	adrp	x0, lC8@PAGE
	mov	w1, 211
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L80:
	adrp	x0, lC8@PAGE
	mov	w1, 214
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L110:
	adrp	x0, lC8@PAGE
	mov	w1, 214
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE19:
	.align	2
	.p2align 5,,15
_khepri_mpt__node_value_to_bytes:
LFB16:
	stp	x29, x30, [sp, -48]!
LCFI21:
	mov	x29, sp
LCFI22:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI23:
	cbz	w1, L119
	sub	w21, w1, #1
	mov	x20, x0
	sxtw	x0, w21
	mov	w19, w1
	add	x0, x0, 12
	mov	x1, 4
	and	x0, x0, -4
	bl	_system__secondary_stack__ss_allocate
	mov	x1, x0
	cmp	w19, 32
	cset	w7, gt
	uxtw	x4, w19
	sub	x5, x20, #1
	add	x0, x0, 8
	add	x6, x1, 7
	stp	wzr, w21, [x1]
	mov	x2, 1
	b	L120
	.p2align 2,,3
L122:
	cmp	w7, 0
	ccmp	w2, 31, 4, ne
	add	x2, x2, 1
	bgt	L126
L120:
	ldrb	w3, [x5, x2]
	strb	w3, [x6, x2]
	cmp	x2, x4
	bne	L122
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI24:
	ret
	.p2align 2,,3
L119:
LCFI25:
	mov	x1, 4
	mov	x0, 8
	bl	_system__secondary_stack__ss_allocate
	movi	d31, 0xffffffff00000000
	mov	x1, x0
	str	d31, [x0], 8
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI26:
	ret
L126:
LCFI27:
	adrp	x0, lC8@PAGE
	mov	w1, 139
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE16:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__path_entryIP
_khepri_mpt__path_entryIP:
LFB2:
	mov	x0, 0
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__Tpath_stackBIP
_khepri_mpt__Tpath_stackBIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__iteratorIP
_khepri_mpt__iteratorIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__trie_entryIP
_khepri_mpt__trie_entryIP:
LFB5:
	sub	sp, sp, #16
LCFI28:
	stp	wzr, wzr, [sp]
	ldr	x0, [sp]
	strb	wzr, [sp, 8]
	ldr	w1, [sp, 8]
	add	sp, sp, 16
LCFI29:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__Ttrie_arrayBIP
_khepri_mpt__Ttrie_arrayBIP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__node_entryIP
_khepri_mpt__node_entryIP:
LFB82:
	ret
LFE82:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__Tnode_arrayBIP
_khepri_mpt__Tnode_arrayBIP:
LFB8:
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__snapshot_entryIP
_khepri_mpt__snapshot_entryIP:
LFB9:
	sub	sp, sp, #16
LCFI30:
	stp	wzr, wzr, [sp]
	ldr	x0, [sp]
	strb	wzr, [sp, 8]
	ldr	w1, [sp, 8]
	add	sp, sp, 16
LCFI31:
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__Tsnapshot_arrayBIP
_khepri_mpt__Tsnapshot_arrayBIP:
LFB10:
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__keccak_256
_khepri_mpt__keccak_256:
LFB17:
	stp	x29, x30, [sp, -112]!
LCFI32:
	mov	x29, sp
LCFI33:
	stp	x19, x20, [sp, 16]
LCFI34:
	mov	x20, x8
	stp	x21, x22, [sp, 32]
	str	x23, [sp, 48]
LCFI35:
	ldp	w21, w22, [x1]
	cmp	w21, w22
	bgt	L141
	tbnz	w21, #31, L143
	sxtw	x2, w22
	mov	x1, x0
	add	x2, x2, 1
	sub	x2, x2, w21, sxtw
	add	x0, x2, 15
	and	x0, x0, -16
	sub	sp, sp, x0
	mov	x0, sp
	mov	x23, sp
	bl	_memcpy
	b	L139
	.p2align 2,,3
L141:
	add	x23, x29, 64
L139:
	add	x19, x29, 80
	mov	x0, x23
	stp	w21, w22, [x29, 72]
	mov	x2, x19
	add	x1, x29, 72
	bl	_anubis_sha3__keccak_256
	ldp	q31, q30, [x19]
	stp	q31, q30, [x20]
	mov	sp, x29
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 112
LCFI36:
	ret
L143:
LCFI37:
	adrp	x0, lC8@PAGE
	mov	w1, 152
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE17:
	.const
	.align	3
lC9:
	.ascii "failed precondition from khepri_mpt.ads:201"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__encode_node
_khepri_mpt__encode_node:
LFB21:
	sub	sp, sp, #800
LCFI38:
	stp	x29, x30, [sp]
LCFI39:
	mov	x29, sp
LCFI40:
	add	x3, x29, 800
LEHB0:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI41:
	mov	x21, x2
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI42:
	mov	x26, x0
	stp	x27, x28, [sp, 80]
LCFI43:
	str	x3, [x29, 184]
	ldp	w4, w3, [x2]
	sxtw	x22, w4
	sxtw	x2, w3
	add	x0, x22, 530
	cmp	x0, x2
	bge	L240
	mov	x20, x1
	cmp	w4, w3
	mov	w1, 0
	mov	x0, x20
	sub	x2, x2, x22
	csinc	x2, xzr, x2, gt
	bl	_memset
	ldrb	w0, [x26]
	cmp	w0, 3
	bhi	L241
	cmp	w0, 1
	beq	L148
	cmp	w0, 2
	beq	L149
	cbnz	w0, L150
	ldp	w0, w1, [x21]
	cmp	w0, w1
	bgt	L242
	sxtw	x0, w0
	mov	w1, -128
	sub	x0, x0, x22
	mov	w19, 1
	strb	w1, [x20, x0]
L152:
	mov	x0, 0
	mov	x1, 1
	bfi	x0, x19, 0, 32
	bfi	x0, x1, 32, 8
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	add	sp, sp, 800
LCFI44:
	ret
	.p2align 2,,3
L149:
LCFI45:
	ldr	w0, [x26, 68]
	stp	xzr, xzr, [x29, 192]
	stp	xzr, xzr, [x29, 208]
	strb	wzr, [x29, 224]
	tbnz	w0, #31, L243
	tbz	x0, 0, L244
	ldrb	w1, [x26, 4]
	cmp	w1, 15
	bhi	L245
	add	w1, w1, 16
	sub	w4, w0, #1
	asr	w4, w4, 1
	strb	w1, [x29, 192]
	cbz	w4, L173
	add	w4, w4, 2
	add	x2, x26, 5
	mov	x1, 2
	add	x6, x29, 192
	b	L185
	.p2align 2,,3
L183:
	ldrb	w3, [x2, 1]
	cmp	w3, 15
	bhi	L246
	add	w0, w3, w0, lsl 4
	add	x3, x1, x6
	add	x5, x1, 1
	add	x2, x2, 2
	strb	w0, [x3, -1]
	cmp	x4, x5
	beq	L175
	mov	x1, x5
L185:
	ldrb	w0, [x2]
	cmp	w0, 15
	bhi	L247
	cmp	x1, 33
	bne	L183
	adrp	x0, lC8@PAGE
	mov	w1, 332
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L148:
	ldr	w0, [x26, 68]
	stp	xzr, xzr, [x29, 192]
	stp	xzr, xzr, [x29, 208]
	strb	wzr, [x29, 224]
	tbnz	w0, #31, L248
	tbz	x0, 0, L249
	ldrb	w1, [x26, 4]
	cmp	w1, 15
	bhi	L250
	add	w1, w1, 48
	sub	w4, w0, #1
	asr	w4, w4, 1
	strb	w1, [x29, 192]
	cbz	w4, L155
	add	w4, w4, 2
	add	x2, x26, 5
	mov	x1, 2
	add	x6, x29, 192
	b	L167
	.p2align 2,,3
L165:
	ldrb	w3, [x2, 1]
	cmp	w3, 15
	bhi	L251
	add	w0, w3, w0, lsl 4
	add	x3, x1, x6
	add	x5, x1, 1
	add	x2, x2, 2
	strb	w0, [x3, -1]
	cmp	x5, x4
	beq	L157
	mov	x1, x5
L167:
	ldrb	w0, [x2]
	cmp	w0, 15
	bhi	L252
	cmp	x1, 33
	bne	L165
	adrp	x0, lC8@PAGE
	mov	w1, 293
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L150:
	add	x25, x29, 192
	mov	x2, 601
	mov	x0, x25
	mov	w1, 0
	adrp	x24, lC2@PAGE
	adrp	x23, lC3@PAGE
	bl	_memset
	mov	x19, 0
	mov	w4, 0
	add	x27, x26, 108
	add	x24, x24, lC2@PAGEOFF;
	add	x23, x23, lC3@PAGEOFF;
	mov	w28, -128
	b	L198
	.p2align 2,,3
L195:
	cmp	w4, 600
	bgt	L253
	add	x19, x19, 1
	strb	w28, [x25, w4, sxtw]
	add	x27, x27, 33
	add	w4, w4, 1
	cmp	x19, 16
	beq	L254
L198:
	add	x1, x19, x19, lsl 5
	add	x1, x26, x1
	ldrb	w1, [x1, 140]
	cmp	w1, 1
	bhi	L255
	cbz	w1, L195
	mov	x0, x27
	mov	x2, x25
	mov	x3, x23
	mov	x1, x24
	bl	_khepri_mpt__encode_string
	add	x19, x19, 1
	mov	w4, w0
	add	x27, x27, 33
	cmp	x19, 16
	bne	L198
L254:
	ldr	w0, [x26, 104]
	cmp	w0, 0
	blt	L256
	bne	L257
	cmp	w4, 600
	bgt	L258
	mov	w0, -128
	add	w23, w4, 1
	strb	w0, [x25, w4, sxtw]
L202:
	cmp	w23, 55
	bgt	L204
	ldr	w2, [x21]
	cmp	w2, 0
	bgt	L205
	ldr	w4, [x21, 4]
	tbnz	w4, #31, L205
	neg	x0, x22
	sub	w3, w23, #64
	mov	w1, 1
	strb	w3, [x20, x0]
L207:
	sxtw	x0, w1
	mov	x3, -1
	mov	x19, x0
	sub	x0, x0, x22
	add	x0, x0, 1
	add	w1, w1, w23
	mov	w5, 2147483647
	add	x20, x20, x0
	b	L211
	.p2align 2,,3
L260:
	cmp	w19, w4
	bgt	L208
	cmp	x3, 600
	beq	L208
	add	x0, x25, x3
	ldrb	w0, [x0, 1]
	strb	w0, [x20, x3]
	cmp	w19, w5
	beq	L259
	add	w19, w19, 1
	add	x3, x3, 1
	cmp	w19, w1
	beq	L152
L211:
	cmp	w19, w2
	bge	L260
L208:
	adrp	x0, lC8@PAGE
	mov	w1, 390
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L249:
	mov	w1, 32
	asr	w0, w0, 1
	strb	w1, [x29, 192]
	cbz	w0, L155
	add	w2, w0, 1
	add	x3, x26, 4
	mov	x1, 2
	add	x5, x29, 192
	b	L156
	.p2align 2,,3
L158:
	add	x1, x1, 1
	add	x3, x3, 2
L156:
	ldrb	w0, [x3]
	cmp	w0, 15
	bhi	L261
	ldrb	w4, [x3, 1]
	cmp	w4, 15
	bhi	L262
	add	w0, w4, w0, lsl 4
	add	x4, x1, x5
	strb	w0, [x4, -1]
	cmp	x1, x2
	beq	L157
	cmp	x1, 33
	bne	L158
	adrp	x0, lC8@PAGE
	mov	w1, 283
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L244:
	asr	w0, w0, 1
	cbz	w0, L173
	add	x4, x26, 4
	add	w1, w0, 1
	add	w3, w0, 1
	mov	x2, 2
	add	x6, x29, 192
	b	L174
	.p2align 2,,3
L176:
	add	x2, x2, 1
	add	x4, x4, 2
L174:
	ldrb	w0, [x4]
	cmp	w0, 15
	bhi	L263
	ldrb	w5, [x4, 1]
	cmp	w5, 15
	bhi	L264
	add	w0, w5, w0, lsl 4
	add	x5, x2, x6
	strb	w0, [x5, -1]
	cmp	x3, x2
	beq	L175
	cmp	x2, 33
	bne	L176
	adrp	x0, lC8@PAGE
	mov	w1, 322
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LEHE0:
L257:
	add	x19, x29, 136
	str	w4, [x29, 108]
	mov	x8, x19
LEHB1:
	bl	_system__secondary_stack__ss_mark
	ldr	w1, [x26, 104]
	ldr	w4, [x29, 108]
	tbnz	w1, #31, L265
	add	x0, x26, 72
	str	w4, [x29, 108]
	bl	_khepri_mpt__node_value_to_bytes
	adrp	x3, lC3@PAGE
	ldr	w4, [x29, 108]
	mov	x2, x25
	add	x3, x3, lC3@PAGEOFF;
	bl	_khepri_mpt__encode_string
LEHE1:
	mov	w23, w0
	mov	x0, x19
LEHB2:
	bl	_system__secondary_stack__ss_release
	b	L202
L155:
	mov	w1, 1
L157:
	ldr	w0, [x21]
	cmp	w0, 0
	bgt	L168
	ldr	w0, [x21, 4]
	tbnz	w0, #31, L168
	neg	x22, x22
	mov	w0, -62
	sub	w5, w1, #1
	mov	w4, 1
	mov	x2, x20
	mov	x3, x21
	strb	w0, [x20, x22]
	add	x1, x29, 120
	add	x0, x29, 192
	stp	wzr, w5, [x29, 120]
	add	x22, x29, 160
	bl	_khepri_mpt__encode_string
LEHE2:
	mov	x8, x22
	mov	w19, w0
LEHB3:
	bl	_system__secondary_stack__ss_mark
	ldr	w1, [x26, 104]
	tbnz	w1, #31, L266
	add	x0, x26, 72
	bl	_khepri_mpt__node_value_to_bytes
	mov	w4, w19
	mov	x2, x20
	mov	x3, x21
	bl	_khepri_mpt__encode_string
LEHE3:
	mov	w19, w0
	mov	x0, x22
LEHB4:
	bl	_system__secondary_stack__ss_release
	b	L152
L173:
	mov	w1, 1
L175:
	ldr	w0, [x21]
	cmp	w0, 0
	bgt	L186
	ldr	w0, [x21, 4]
	tbnz	w0, #31, L186
	neg	x0, x22
	mov	w6, -62
	sub	w5, w1, #1
	mov	w4, 1
	add	x1, x29, 128
	mov	x2, x20
	strb	w6, [x20, x0]
	mov	x3, x21
	add	x0, x29, 192
	stp	wzr, w5, [x29, 128]
	bl	_khepri_mpt__encode_string
	ldrb	w1, [x26, 140]
	mov	w4, w0
	cmp	w1, 1
	bhi	L267
	cbnz	w1, L268
	ldr	w0, [x21]
	cmp	w0, w4
	bgt	L191
	ldr	w0, [x21, 4]
	cmp	w0, w4
	blt	L191
	sxtw	x0, w4
	mov	w2, -128
	sub	x0, x0, x22
	mov	w1, 2147483647
	add	w19, w4, 1
	strb	w2, [x20, x0]
	cmp	w4, w1
	bne	L152
	adrp	x0, lC8@PAGE
	mov	w1, 347
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L204:
	mov	w4, 0
	mov	x2, x20
	mov	w1, 247
	mov	x3, x21
	mov	w0, w23
	bl	_khepri_mpt__encode_length
	ldp	w2, w4, [x21]
	mov	w1, w0
	b	L207
L268:
	adrp	x1, lC2@PAGE
	mov	x2, x20
	mov	x3, x21
	add	x0, x26, 108
	add	x1, x1, lC2@PAGEOFF;
	bl	_khepri_mpt__encode_string
	mov	w19, w0
	b	L152
L259:
	adrp	x0, lC8@PAGE
	mov	w1, 391
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L255:
	adrp	x0, lC8@PAGE
	mov	w1, 362
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L247:
	adrp	x0, lC8@PAGE
	mov	w1, 331
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L252:
	adrp	x0, lC8@PAGE
	mov	w1, 292
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L251:
	adrp	x0, lC8@PAGE
	mov	w1, 293
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L253:
	adrp	x0, lC8@PAGE
	mov	w1, 366
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L246:
	adrp	x0, lC8@PAGE
	mov	w1, 332
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L263:
	adrp	x0, lC8@PAGE
	mov	w1, 322
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L264:
	adrp	x0, lC8@PAGE
	mov	w1, 323
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE4:
L266:
	adrp	x0, lC8@PAGE
	mov	w1, 304
	add	x0, x0, lC8@PAGEOFF;
LEHB5:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE5:
L168:
	adrp	x0, lC8@PAGE
	mov	w1, 299
	add	x0, x0, lC8@PAGEOFF;
LEHB6:
	bl	___gnat_rcheck_CE_Index_Check
LEHE6:
L265:
	adrp	x0, lC8@PAGE
	mov	w1, 374
	add	x0, x0, lC8@PAGEOFF;
LEHB7:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE7:
L258:
	adrp	x0, lC8@PAGE
	mov	w1, 376
	add	x0, x0, lC8@PAGEOFF;
LEHB8:
	bl	___gnat_rcheck_CE_Index_Check
L241:
	adrp	x0, lC8@PAGE
	mov	w1, 264
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L245:
	adrp	x0, lC8@PAGE
	mov	w1, 327
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L262:
	adrp	x0, lC8@PAGE
	mov	w1, 284
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L240:
	adrp	x0, lC9@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L242:
	adrp	x0, lC8@PAGE
	mov	w1, 267
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L243:
	adrp	x0, lC8@PAGE
	mov	w1, 317
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L256:
	adrp	x0, lC8@PAGE
	mov	w1, 372
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L261:
	adrp	x0, lC8@PAGE
	mov	w1, 283
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L248:
	adrp	x0, lC8@PAGE
	mov	w1, 278
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L250:
	adrp	x0, lC8@PAGE
	mov	w1, 288
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L205:
	adrp	x0, lC8@PAGE
	mov	w1, 382
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L191:
	adrp	x0, lC8@PAGE
	mov	w1, 346
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L267:
	adrp	x0, lC8@PAGE
	mov	w1, 343
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L186:
	adrp	x0, lC8@PAGE
	mov	w1, 338
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L218:
	mov	x1, x0
	mov	x0, x19
	mov	x19, x1
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
L217:
	mov	x19, x0
	mov	x0, x22
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE8:
LFE21:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
LLSDA21:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE21-LLSDACSB21
LLSDACSB21:
	.uleb128 LEHB0-LFB21
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB21
	.uleb128 LEHE1-LEHB1
	.uleb128 L218-LFB21
	.uleb128 0
	.uleb128 LEHB2-LFB21
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB3-LFB21
	.uleb128 LEHE3-LEHB3
	.uleb128 L217-LFB21
	.uleb128 0
	.uleb128 LEHB4-LFB21
	.uleb128 LEHE4-LEHB4
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB5-LFB21
	.uleb128 LEHE5-LEHB5
	.uleb128 L217-LFB21
	.uleb128 0
	.uleb128 LEHB6-LFB21
	.uleb128 LEHE6-LEHB6
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB7-LFB21
	.uleb128 LEHE7-LEHB7
	.uleb128 L218-LFB21
	.uleb128 0
	.uleb128 LEHB8-LFB21
	.uleb128 LEHE8-LEHB8
	.uleb128 0
	.uleb128 0
LLSDACSE21:
	.text
	.const
	.align	2
lC2:
	.word	0
	.word	31
	.align	2
lC3:
	.word	0
	.word	600
	.align	2
lC1:
	.word	1
	.word	43
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__hash_node
_khepri_mpt__hash_node:
LFB20:
	sub	sp, sp, #592
LCFI46:
	mov	x2, 532
	mov	w1, 0
	stp	x29, x30, [sp]
LCFI47:
	mov	x29, sp
LCFI48:
	str	x21, [sp, 32]
LCFI49:
	add	x21, x29, 56
	stp	x19, x20, [sp, 16]
LCFI50:
	mov	x20, x0
	mov	x0, x21
	mov	x19, x8
	bl	_memset
	adrp	x2, lC0@PAGE
	mov	x1, x21
	mov	x0, x20
	add	x2, x2, lC0@PAGEOFF;
	bl	_khepri_mpt__encode_node
	ubfx	x1, x0, 32, 8
	cmp	w1, 1
	bhi	L271
	cmp	w0, 0
	blt	L271
	eor	w1, w1, 1
	cset	w2, eq
	orr	w1, w2, w1
	tbnz	x1, 0, L276
	sub	w2, w0, #1
	cmp	w2, 531
	bgt	L277
	mov	x8, x19
	mov	x0, x21
	stp	wzr, w2, [x29, 48]
	add	x1, x29, 48
	bl	_khepri_mpt__keccak_256
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 592
LCFI51:
	ret
	.p2align 2,,3
L276:
LCFI52:
	stp	xzr, xzr, [x19]
	stp	xzr, xzr, [x19, 16]
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, 592
LCFI53:
	ret
L271:
LCFI54:
	adrp	x0, lC8@PAGE
	mov	w1, 241
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L277:
	adrp	x0, lC8@PAGE
	mov	w1, 245
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE20:
	.const
	.align	2
lC0:
	.word	0
	.word	531
	.text
	.align	2
	.p2align 5,,15
_khepri_mpt__make_leaf:
LFB28:
	sub	sp, sp, #768
LCFI55:
	mov	x3, x2
	mov	x2, 672
	stp	x29, x30, [sp]
LCFI56:
	mov	x29, sp
LCFI57:
	stp	x19, x20, [sp, 16]
LCFI58:
	add	x20, x29, 96
	stp	x21, x22, [sp, 32]
LCFI59:
	mov	x21, x1
	mov	x22, x8
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	stp	x23, x24, [sp, 48]
LCFI60:
	mov	x24, x0
	mov	x0, x20
	ldp	w19, w23, [x3]
	bl	_memcpy
	mov	w1, 1
	ldp	q28, q30, [x24]
	add	x0, x29, 100
	strb	w1, [x29, 96]
	ldp	q29, q31, [x24, 32]
	ldr	w1, [x24, 64]
	str	q28, [x29, 100]
	stp	q30, q29, [x0, 16]
	str	q31, [x29, 148]
	str	w1, [x29, 164]
	cmp	w19, w23
	bgt	L279
	sxtw	x0, w23
	mov	x1, 2147483647
	sub	x0, x0, w19, sxtw
	add	x0, x0, 1
	cmp	x0, x1
	bgt	L295
	cmp	w0, 32
	mov	w1, 32
	csel	w1, w0, w1, le
	str	w1, [x29, 200]
	cmp	w0, 0
	blt	L296
	sub	w1, w1, #1
	mov	x2, 0
	bne	L284
	b	L281
	.p2align 2,,3
L293:
	ldrb	w4, [x21, x2]
	add	x3, x20, x2
	strb	w4, [x3, 72]
	cmp	x1, x2
	beq	L281
	add	x2, x2, 1
	cmp	x2, 32
	beq	L288
L284:
	adds	w3, w19, w2
	bvs	L286
	cmp	w23, w3
	ccmp	w19, w3, 0, ge
	ble	L293
L288:
	adrp	x0, lC8@PAGE
	mov	w1, 554
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L279:
	str	wzr, [x29, 200]
L281:
	add	x19, x29, 64
	mov	x0, x20
	mov	x8, x19
	bl	_khepri_mpt__hash_node
	ldp	q30, q31, [x19]
	mov	w3, 1
	mov	x1, x20
	mov	x0, x22
	mov	x2, 672
	strb	w3, [x29, 764]
	add	x3, x29, 512
	str	q30, [x3, 220]
	str	q31, [x3, 236]
	bl	_memcpy
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 768
LCFI61:
	ret
L286:
LCFI62:
	adrp	x0, lC8@PAGE
	mov	w1, 554
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L295:
	adrp	x0, lC8@PAGE
	mov	w1, 552
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L296:
	adrp	x0, lC8@PAGE
	mov	w1, 553
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE28:
	.align	2
	.p2align 5,,15
_khepri_mpt__make_extension:
LFB30:
	sub	sp, sp, #800
LCFI63:
	mov	x2, 672
	stp	x29, x30, [sp]
LCFI64:
	mov	x29, sp
LCFI65:
	stp	x21, x22, [sp, 32]
LCFI66:
	add	x21, x29, 128
	mov	x22, x1
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	stp	x19, x20, [sp, 16]
LCFI67:
	mov	x19, x0
	mov	x0, x21
	add	x20, x29, 64
	stp	x23, x24, [sp, 48]
LCFI68:
	mov	x23, x8
	mov	w24, 1
	bl	_memcpy
	mov	w2, 2
	add	x1, x29, 132
	mov	x8, x20
	mov	x0, x21
	strb	w2, [x29, 128]
	ldp	q31, q28, [x22]
	strb	w24, [x29, 268]
	ldp	q29, q30, [x19]
	ldr	w2, [x19, 64]
	str	q31, [x29, 236]
	str	q29, [x29, 132]
	ldp	q29, q31, [x19, 32]
	str	q28, [x29, 252]
	stp	q30, q29, [x1, 16]
	str	q31, [x29, 180]
	str	w2, [x29, 196]
	bl	_khepri_mpt__hash_node
	strb	w24, [x29, 796]
	add	x3, x29, 512
	mov	x2, 672
	ldp	q30, q31, [x20]
	mov	x1, x21
	mov	x0, x23
	str	q30, [x3, 252]
	add	x3, x29, 1024
	str	q31, [x3, -244]
	bl	_memcpy
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 800
LCFI69:
	ret
LFE30:
	.align	2
	.p2align 5,,15
_khepri_mpt__delete_from_node:
LFB36:
	sub	sp, sp, #864
LCFI70:
	stp	x29, x30, [sp]
LCFI71:
	mov	x29, sp
LCFI72:
	stp	x19, x20, [sp, 16]
LCFI73:
	mov	w19, w0
	mov	w0, 16959
	movk	w0, 0xf, lsl 16
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI74:
	cmp	w19, w0
	bgt	L301
	sxtw	x23, w19
	sbfiz	x24, x19, 2, 32
	add	x20, x24, x23
	adrp	x25, _khepri_mpt__node_store@PAGE
	add	x20, x23, x20, lsl 2
	add	x0, x25, _khepri_mpt__node_store@PAGEOFF;
	add	x20, x23, x20, lsl 3
	add	x20, x0, x20, lsl 2
	ldrb	w0, [x20, 672]
	cmp	w0, 1
	bhi	L365
	cbz	w0, L301
	add	x26, x29, 192
	mov	x21, x1
	mov	x2, 672
	mov	x0, x26
	mov	x1, x20
	bl	_memcpy
	ldrb	w22, [x29, 192]
	cmp	w22, 3
	bhi	L366
	cmp	w22, 1
	beq	L305
	cmp	w22, 2
	beq	L306
	cbz	w22, L301
	ldr	w3, [x21, 64]
	cmp	w3, 0
	blt	L367
	beq	L368
	ldrb	w0, [x21]
	cmp	w0, 15
	bhi	L369
	add	x2, x29, 332
	ubfiz	x1, x0, 5, 8
	add	x1, x1, w0, uxtw
	ldrb	w0, [x2, x1]
	cmp	w0, 1
	bhi	L370
	cbz	w0, L301
	mov	x0, 0
	mov	w3, 0
	strb	wzr, [x2, x1]
	mov	w4, 2147483647
	.p2align 5,,15
L329:
	add	x1, x0, x0, lsl 5
	ldrb	w1, [x2, x1]
	cmp	w1, 1
	bhi	L326
	cbz	w1, L327
	cmp	w3, w4
	beq	L328
	add	w3, w3, 1
L327:
	add	x0, x0, 1
	cmp	x0, 16
	bne	L329
	ldr	w0, [x29, 296]
	tbnz	w0, #31, L371
	orr	w1, w0, w3
	cbz	w1, L363
	cmp	w0, 0
	ccmp	w3, 1, 0, eq
	bne	L333
L364:
	add	x20, x29, 88
	mov	x0, x26
	mov	x8, x20
	bl	_khepri_mpt__hash_node
	ldr	q30, [x29, 88]
	add	x0, x29, 1024
	ldr	q31, [x29, 104]
	str	q30, [x0, -196]
	str	q31, [x0, -180]
L334:
	add	x0, x24, x23
	add	x25, x25, _khepri_mpt__node_store@PAGEOFF;
	add	x0, x23, x0, lsl 2
	mov	w3, 1
	mov	x1, x26
	mov	x2, 672
	add	x0, x23, x0, lsl 3
	strb	w3, [x29, 860]
	add	x0, x25, x0, lsl 2
	bl	_memcpy
L332:
	mov	w2, 0
	mov	w1, 1
	b	L309
	.p2align 2,,3
L305:
	mov	x1, x21
	add	x0, x29, 196
	bl	_khepri_mpt_types__keys_equal
	cmp	w0, 1
	bhi	L372
	cbnz	w0, L373
	.p2align 5,,15
L301:
	mov	w2, 1
	mov	w1, 0
L309:
	mov	x0, 0
	bfi	x0, x19, 0, 32
	bfi	x0, x1, 32, 8
	bfi	x0, x2, 40, 8
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, 864
LCFI75:
	ret
	.p2align 2,,3
L306:
LCFI76:
	mov	x1, x21
	add	x0, x29, 196
	bl	_khepri_mpt_types__is_prefix
	cmp	w0, 1
	bhi	L374
	cbz	w0, L301
	ldr	w1, [x29, 260]
	tbnz	w1, #31, L375
	mov	x0, x21
	add	x8, x29, 120
	bl	_khepri_mpt_types__remove_prefix
	ldr	w0, [x29, 184]
	tbz	w0, #31, L301
	adrp	x0, lC8@PAGE
	mov	w1, 1067
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L368:
	ldr	w0, [x29, 296]
	cmp	w0, 0
	blt	L376
	beq	L301
	add	x1, x29, 512
	mov	x0, 0
	str	wzr, [x29, 296]
	add	x2, x29, 332
	mov	w4, 2147483647
	stp	xzr, xzr, [x1, -248]
	stp	xzr, xzr, [x1, -232]
	.p2align 5,,15
L319:
	add	x1, x0, x0, lsl 5
	ldrb	w1, [x2, x1]
	cmp	w1, 1
	bhi	L326
	cbz	w1, L317
	cmp	w3, w4
	beq	L328
	add	w3, w3, 1
L317:
	add	x0, x0, 1
	cmp	x0, 16
	bne	L319
	cbz	w3, L363
	cmp	w3, 1
	beq	L364
L333:
	add	x20, x29, 120
	mov	x0, x26
	mov	x8, x20
	bl	_khepri_mpt__hash_node
	ldr	q30, [x29, 120]
	add	x0, x29, 1024
	ldr	q31, [x29, 136]
	str	q30, [x0, -196]
	str	q31, [x0, -180]
	b	L334
	.p2align 2,,3
L373:
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	mov	x2, 672
	mov	x0, x20
	strb	wzr, [x20, 672]
	mov	w19, 2147483647
	bl	_memcpy
	mov	w1, w22
	mov	w2, 0
	b	L309
L365:
	adrp	x0, lC8@PAGE
	mov	w1, 1038
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L363:
	add	x0, x24, x23
	add	x25, x25, _khepri_mpt__node_store@PAGEOFF;
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	add	x0, x23, x0, lsl 2
	mov	x2, 672
	mov	w19, 2147483647
	add	x0, x23, x0, lsl 3
	add	x0, x25, x0, lsl 2
	strb	wzr, [x0, 672]
	bl	_memcpy
	b	L332
L326:
	adrp	x0, lC8@PAGE
	mov	w1, 1006
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L328:
	adrp	x0, lC8@PAGE
	mov	w1, 1007
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L366:
	adrp	x0, lC8@PAGE
	mov	w1, 1045
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L369:
	adrp	x0, lC8@PAGE
	mov	w1, 1113
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L367:
	adrp	x0, lC8@PAGE
	mov	w1, 1081
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L376:
	adrp	x0, lC8@PAGE
	mov	w1, 1083
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L375:
	adrp	x0, lC8@PAGE
	mov	w1, 1065
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L372:
	adrp	x0, lC8@PAGE
	mov	w1, 1050
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L374:
	adrp	x0, lC8@PAGE
	mov	w1, 1060
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L371:
	adrp	x0, lC8@PAGE
	mov	w1, 1124
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L370:
	adrp	x0, lC8@PAGE
	mov	w1, 1115
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE36:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__decode_node
_khepri_mpt__decode_node:
LFB24:
	stp	x29, x30, [sp, -48]!
LCFI77:
	mov	x3, x1
	mov	x29, sp
LCFI78:
	mov	x6, x2
	mov	x2, 672
	stp	x19, x20, [sp, 16]
LCFI79:
	mov	x20, x0
	mov	x0, x6
	str	x21, [sp, 32]
LCFI80:
	ldp	w19, w21, [x3]
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	bl	_memcpy
	cmp	w19, w21
	bgt	L389
	ldrb	w1, [x20]
	mov	x6, x0
	beq	L396
L379:
	cmp	w1, 191
	bls	L391
	cmp	w1, 246
	bls	L399
	add	w0, w1, 9
	tst	w0, 255
	beq	L392
	add	w5, w1, 10
	mov	w3, 0
	mov	x1, 1
	and	x5, x5, 255
	mov	w0, 16777215
	.p2align 5,,15
L388:
	adds	w4, w19, w1
	bvs	L384
	cmp	w21, w4
	ccmp	w19, w4, 0, ge
	bgt	L400
	add	w4, w3, 8388608
	cmp	w4, w0
	bhi	L401
	ldrb	w4, [x20, x1]
	add	x1, x1, 1
	add	w3, w4, w3, lsl 8
	cmp	x1, x5
	bne	L388
	b	L382
	.p2align 2,,3
L389:
	mov	w2, 0
	mov	x0, 0
	bfi	x0, x2, 0, 8
	mov	w1, 6
	bfi	x0, x1, 8, 8
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI81:
	ret
	.p2align 2,,3
L391:
LCFI82:
	mov	w2, 0
	mov	x0, 0
	bfi	x0, x2, 0, 8
	mov	w1, 6
	bfi	x0, x1, 8, 8
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI83:
	ret
	.p2align 2,,3
L396:
LCFI84:
	cmp	w1, 128
	bne	L379
	mov	w0, 0
L380:
	mov	w2, 1
	strb	w0, [x6]
	mov	x0, 0
	bfi	x0, x2, 0, 8
	mov	w1, 0
	bfi	x0, x1, 8, 8
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI85:
	ret
	.p2align 2,,3
L399:
LCFI86:
	add	w1, w1, 64
	and	w3, w1, 255
L382:
	cmp	w3, 40
	mov	w0, 3
	csinc	w0, w0, wzr, ge
	b	L380
	.p2align 2,,3
L392:
	mov	w0, 1
	b	L380
L400:
	adrp	x0, lC8@PAGE
	mov	w1, 446
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L384:
	adrp	x0, lC8@PAGE
	mov	w1, 446
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L401:
	adrp	x0, lC8@PAGE
	mov	w1, 445
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
LFE24:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__create_trie
_khepri_mpt__create_trie:
LFB25:
	adrp	x2, _khepri_mpt__tries@PAGE
	mov	x0, 0
	add	x2, x2, _khepri_mpt__tries@PAGEOFF;
	.p2align 5,,15
L407:
	mov	x1, 1
	cmp	x0, 0
	csel	x0, x0, x1, ne
	add	x1, x0, x0, lsl 1
	add	x1, x2, x1, lsl 2
	ldrb	w1, [x1, 8]
	cmp	w1, 1
	bhi	L414
	cbz	w1, L405
	add	x0, x0, 1
	cmp	x0, 256
	bne	L407
	mov	w1, 0
	mov	x0, 0
	bfi	x0, x1, 0, 32
	mov	w5, 0
	bfi	x0, x5, 32, 8
	ret
	.p2align 2,,3
L405:
	adrp	x1, lC10@PAGE
	mov	w4, 12
	sbfiz	x3, x0, 1, 32
	add	x3, x3, w0, sxtw
	mov	w5, 1
	ldr	d31, [x1, #lC10@PAGEOFF]
	umaddl	x4, w0, w4, x2
	mov	w1, w0
	mov	x0, 0
	lsl	x3, x3, 2
	bfi	x0, x1, 0, 32
	mov	w6, 1
	bfi	x0, x5, 32, 8
	str	d31, [x2, x3]
	strb	w6, [x4, 8]
	ret
L414:
	adrp	x0, lC8@PAGE
	stp	x29, x30, [sp, -16]!
LCFI87:
	mov	w1, 82
	mov	x29, sp
LCFI88:
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__load_trie
_khepri_mpt__load_trie:
LFB26:
	adrp	x2, _khepri_mpt__tries@PAGE
	mov	x0, 0
	add	x2, x2, _khepri_mpt__tries@PAGEOFF;
	.p2align 5,,15
L420:
	mov	x1, 1
	cmp	x0, 0
	csel	x0, x0, x1, ne
	add	x1, x0, x0, lsl 1
	add	x1, x2, x1, lsl 2
	ldrb	w1, [x1, 8]
	cmp	w1, 1
	bhi	L427
	cbz	w1, L418
	add	x0, x0, 1
	cmp	x0, 256
	bne	L420
	mov	w1, 0
	mov	x0, 0
	bfi	x0, x1, 0, 32
	mov	w5, 0
	bfi	x0, x5, 32, 8
	ret
	.p2align 2,,3
L418:
	adrp	x1, lC10@PAGE
	mov	w4, 12
	sbfiz	x3, x0, 1, 32
	add	x3, x3, w0, sxtw
	mov	w5, 1
	ldr	d31, [x1, #lC10@PAGEOFF]
	umaddl	x4, w0, w4, x2
	mov	w1, w0
	mov	x0, 0
	lsl	x3, x3, 2
	bfi	x0, x1, 0, 32
	mov	w6, 1
	bfi	x0, x5, 32, 8
	str	d31, [x2, x3]
	strb	w6, [x4, 8]
	ret
L427:
	adrp	x0, lC8@PAGE
	stp	x29, x30, [sp, -16]!
LCFI89:
	mov	w1, 82
	mov	x29, sp
LCFI90:
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE26:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__destroy_trie
_khepri_mpt__destroy_trie:
LFB27:
	cmp	w0, 255
	bhi	L431
	cbz	w0, L428
	adrp	x1, _khepri_mpt__tries@PAGE
	sbfiz	x2, x0, 1, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_mpt__tries@PAGEOFF;
	lsl	x3, x0, 2
	add	x0, x1, x0, lsl 2
	ldrb	w2, [x0, 8]
	cmp	w2, 1
	bhi	L431
	cbz	w2, L428
	adrp	x2, lC10@PAGE
	strb	wzr, [x0, 8]
	ldr	d31, [x2, #lC10@PAGEOFF]
	str	d31, [x1, x3]
L428:
	ret
L431:
	adrp	x0, lC8@PAGE
	stp	x29, x30, [sp, -16]!
LCFI91:
	mov	w1, 531
	mov	x29, sp
LCFI92:
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE27:
	.const
	.align	3
lC11:
	.ascii "failed precondition from khepri_mpt.ads:85"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__put
_khepri_mpt__put:
LFB32:
	sub	sp, sp, #3152
LCFI93:
	mov	w5, w0
	stp	x29, x30, [sp]
LCFI94:
	mov	x29, sp
LCFI95:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI96:
	ldp	w0, w9, [x2]
	ldpsw	x7, x12, [x4]
	sxtw	x10, w0
	sxtw	x8, w9
	add	x11, x10, 31
	add	x7, x7, 31
	cmp	x7, x12
	ccmp	x11, x8, 1, ge
	blt	L634
	cset	w22, lt
	cmp	w5, 255
	bhi	L444
	cbz	w5, L455
	sxtw	x23, w5
	sbfiz	x19, x5, 1, 32
	adrp	x27, _khepri_mpt__tries@PAGE
	mov	x6, x2
	mov	x20, x4
	add	x2, x19, x23
	add	x4, x27, _khepri_mpt__tries@PAGEOFF;
	add	x2, x4, x2, lsl 2
	ldrb	w2, [x2, 8]
	cmp	w2, 1
	bhi	L444
	cbz	w2, L455
	mov	x21, x3
	cmp	w9, w0
	blt	L585
	sub	x8, x8, x10
	mov	x0, 2147483647
	add	x2, x8, 1
	cmp	x2, x0
	bgt	L635
L447:
	add	x24, x29, 848
	mov	x0, x1
	mov	x8, x24
	mov	x1, x6
	bl	_khepri_mpt_types__bytes_to_nibbles
	ldr	q28, [x29, 848]
	add	x4, x29, 976
	add	x25, x19, x23
	lsl	x25, x25, 2
	add	x26, x27, _khepri_mpt__tries@PAGEOFF;
	ldp	q30, q29, [x24, 16]
	ldr	q31, [x24, 48]
	str	q28, [x29, 976]
	ldr	w0, [x24, 64]
	str	q31, [x4, 48]
	stp	q30, q29, [x4, 16]
	ldr	w28, [x26, x25]
	str	w0, [x4, 64]
	tbnz	w28, #31, L636
	mov	w0, 2147483647
	cmp	w28, w0
	beq	L637
	mov	w0, 16959
	movk	w0, 0xf, lsl 16
	cmp	w28, w0
	bgt	L455
	sxtw	x25, w28
	sbfiz	x0, x28, 2, 32
	add	x1, x0, x25
	adrp	x24, _khepri_mpt__node_store@PAGE
	add	x1, x25, x1, lsl 2
	str	x0, [x29, 160]
	add	x0, x24, _khepri_mpt__node_store@PAGEOFF;
	add	x1, x25, x1, lsl 3
	add	x1, x0, x1, lsl 2
	ldrb	w0, [x1, 672]
	cmp	w0, 1
	bhi	L638
	str	x4, [x29, 168]
	cbz	w0, L455
	ldp	w26, w7, [x20]
	add	x6, x29, 1136
	mov	x2, 672
	mov	x0, x6
	str	w7, [x29, 152]
	bl	_memcpy
	mov	x6, x0
	ldrb	w0, [x29, 1136]
	ldr	x4, [x29, 168]
	ldr	w7, [x29, 152]
	cmp	w0, 3
	bhi	L639
	sxtw	x1, w26
	str	x1, [x29, 168]
	cmp	w0, 1
	beq	L459
	cmp	w0, 2
	beq	L460
	cbnz	w0, L461
	add	x8, x29, 176
	mov	x2, x20
	mov	x0, x4
	mov	x1, x21
	mov	x20, x8
	bl	_khepri_mpt__make_leaf
	add	x4, x29, 2480
	mov	x1, x20
	mov	x0, x4
	mov	x2, 672
	bl	_memcpy
	bl	_khepri_mpt__allocate_node
	mov	x28, x0
	ubfx	x0, x0, 32, 8
	cmp	w0, 1
	bhi	L640
	cbz	w0, L560
	ldr	x0, [x29, 160]
	add	x24, x24, _khepri_mpt__node_store@PAGEOFF;
	mov	x2, 672
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	add	x0, x0, x25
	add	x0, x25, x0, lsl 2
	add	x0, x25, x0, lsl 3
	add	x0, x24, x0, lsl 2
	strb	wzr, [x0, 672]
	bl	_memcpy
L464:
	tbnz	w28, #31, L641
L581:
	add	x0, x27, _khepri_mpt__tries@PAGEOFF;
	add	x1, x19, x23
	add	x2, x0, x1, lsl 2
	str	w28, [x0, x1, lsl 2]
	ldr	w0, [x2, 4]
	tbnz	w0, #31, L642
	mov	w1, 2147483647
	add	w2, w0, 1
	cmp	w0, w1
	bne	L584
	adrp	x0, lC8@PAGE
	mov	w1, 899
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L455:
	mov	w1, 2
L446:
	mov	x0, 0
	bfi	x0, x22, 0, 8
	bfi	x0, x1, 8, 8
	mov	sp, x29
LCFI97:
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	add	sp, sp, 3152
LCFI98:
	ret
	.p2align 2,,3
L585:
LCFI99:
	mov	w2, 0
	b	L447
	.p2align 2,,3
L637:
	add	x8, x29, 2480
	mov	x1, x21
	mov	x2, x20
	mov	x0, x4
	mov	x20, x8
	bl	_khepri_mpt__make_leaf
	mov	x0, x20
	bl	_khepri_mpt__allocate_node
	ubfx	x1, x0, 32, 8
	cmp	w1, 1
	bhi	L643
	cbz	w1, L560
	tbnz	w0, #31, L644
	mov	w2, 1
	str	w0, [x26, x25]
L584:
	add	x3, x27, _khepri_mpt__tries@PAGEOFF;
	add	x5, x19, x23
	add	x5, x3, x5, lsl 2
	mov	w1, 0
	mov	w22, 1
	str	w2, [x5, 4]
	b	L446
	.p2align 2,,3
L460:
	add	x0, x29, 1140
	mov	x1, x4
	stp	x4, x0, [x29, 144]
	bl	_khepri_mpt_types__common_prefix_length
	ldr	x4, [x29, 144]
	tbnz	w0, #31, L645
	ldr	w6, [x29, 1204]
	tbnz	w6, #31, L646
	cmp	w0, 0
	ccmp	w0, w6, 4, eq
	beq	L581
	add	x26, x29, 2480
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	mov	x2, 672
	mov	x0, x26
	str	x4, [x29, 136]
	add	x28, x29, 1808
	str	w6, [x29, 144]
	bl	_memcpy
	mov	w8, 3
	mov	x2, 672
	mov	x0, x28
	mov	x1, x26
	strb	w8, [x29, 2480]
	bl	_memcpy
	ldrb	w2, [x29, 1140]
	ldr	x4, [x29, 136]
	ldr	w6, [x29, 144]
	cmp	w2, 15
	bhi	L647
	cmp	w6, 1
	bgt	L648
	add	x1, x29, 1244
	add	x0, x29, 1104
	add	x3, x29, 1904
	ldr	q31, [x1, 16]
	ldr	q30, [x0, 140]
	ubfiz	x0, x2, 5, 8
	add	x0, x0, w2, uxtw
	add	x0, x3, x0
	ldrb	w1, [x29, 1276]
	str	q31, [x0, 28]
	str	q30, [x0, 12]
	strb	w1, [x0, 44]
L541:
	ldr	w0, [x29, 1040]
	cmp	w0, 0
	blt	L649
	beq	L543
	ldrb	w5, [x29, 976]
	cmp	w5, 15
	bhi	L650
	add	x1, x29, 1056
	mov	x0, x4
	str	x3, [x29, 144]
	mov	x4, x1
	mov	x8, x1
	str	w5, [x29, 152]
	mov	w1, 1
	str	x4, [x29, 168]
	bl	_khepri_mpt_types__remove_prefix
	ldr	x4, [x29, 168]
	mov	x8, x26
	mov	x1, x21
	mov	x2, x20
	mov	x0, x4
	bl	_khepri_mpt__make_leaf
	mov	x0, x26
	bl	_khepri_mpt__allocate_node
	ubfx	x0, x0, 32, 8
	ldr	x3, [x29, 144]
	cmp	w0, 1
	ldr	w5, [x29, 152]
	bhi	L651
	cbz	w0, L560
	add	x0, x29, 3152
	add	x1, x29, 3152
	ldr	q30, [x0, -36]
	ubfiz	x0, x5, 5, 8
	add	x2, x0, w5, uxtw
	add	x3, x3, x2
	add	x0, x29, 1948
	ldr	q31, [x1, -20]
	mov	w1, 1
	str	q30, [x3, 12]
	str	q31, [x3, 28]
	strb	w1, [x0, x2]
L547:
	mov	x8, x26
	mov	x0, x28
	bl	_khepri_mpt__hash_node
	ldr	x0, [x29, 160]
	mov	w1, 1
	add	x24, x24, _khepri_mpt__node_store@PAGEOFF;
	add	x3, x29, 2640
	mov	x2, 672
	ldr	q31, [x26, 16]
	strb	w1, [x29, 2476]
	ldr	q30, [x29, 2480]
	add	x0, x0, x25
	add	x0, x25, x0, lsl 2
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	add	x0, x25, x0, lsl 3
	str	q31, [x3, -180]
	add	x0, x24, x0, lsl 2
	str	q30, [x3, -196]
	strb	wzr, [x0, 672]
	bl	_memcpy
	mov	x0, x28
	bl	_khepri_mpt__allocate_node
	mov	x28, x0
	ubfx	x0, x0, 32, 8
	cmp	w0, 1
	bhi	L652
L559:
	cbnz	w0, L464
	.p2align 5,,15
L560:
	mov	w1, 5
	b	L446
	.p2align 2,,3
L461:
	ldr	w0, [x29, 1040]
	cmp	w0, 0
	blt	L653
	bne	L562
	cmp	w26, w7
	bgt	L563
	sxtw	x0, w7
	sxtw	x2, w26
	sub	x0, x0, x2
	mov	x1, 2147483647
	add	x0, x0, 1
	cmp	x0, x1
	bgt	L654
	cmp	w0, 32
	mov	w4, 32
	csel	w4, w0, w4, le
	str	w4, [x29, 1240]
	cmp	w0, 0
	blt	L655
	sub	w4, w4, #1
	mov	x0, 0
	bne	L568
	b	L565
	.p2align 2,,3
L573:
	add	x0, x0, 1
	cmp	x0, 32
	beq	L572
L568:
	adds	w1, w26, w0
	bvs	L570
	cmp	w26, w1
	ccmp	w7, w1, 1, le
	blt	L572
	ldrb	w2, [x21, x0]
	add	x1, x6, x0
	strb	w2, [x1, 72]
	cmp	x4, x0
	bne	L573
L565:
	add	x20, x29, 2480
	mov	x0, x6
	str	x6, [x29, 168]
	mov	x8, x20
	bl	_khepri_mpt__hash_node
	ldp	x0, x6, [x29, 160]
	add	x3, x29, 1616
	mov	w7, 1
	mov	x2, 672
	strb	w7, [x29, 1804]
	ldr	q30, [x29, 2480]
	add	x4, x0, x25
	add	x0, x24, _khepri_mpt__node_store@PAGEOFF;
	ldr	q31, [x29, 2496]
	add	x4, x25, x4, lsl 2
	mov	x1, x6
	add	x4, x25, x4, lsl 3
	add	x0, x0, x4, lsl 2
	str	q30, [x3, 156]
	str	q31, [x3, 172]
	bl	_memcpy
	b	L581
	.p2align 2,,3
L459:
	add	x0, x29, 1140
	mov	x1, x4
	stp	x6, x4, [x29, 136]
	str	x0, [x29, 152]
	bl	_khepri_mpt_types__common_prefix_length
	ldp	x6, x4, [x29, 136]
	mov	w10, w0
	tbnz	w0, #31, L656
	ldr	w11, [x29, 1040]
	tbnz	w11, #31, L467
	ldr	w8, [x29, 1204]
	tbnz	w8, #31, L467
	cmp	w0, w11
	ccmp	w0, w8, 0, eq
	bne	L468
	add	x26, x29, 2480
	mov	x1, x6
	mov	x0, x26
	mov	x2, 672
	bl	_memcpy
	ldp	w1, w9, [x20]
	cmp	w9, w1
	blt	L469
	sxtw	x2, w9
	sxtw	x0, w1
	sub	x2, x2, x0
	mov	x4, 2147483647
	add	x2, x2, 1
	cmp	x2, x4
	bgt	L657
	cmp	w2, 32
	mov	w8, 32
	csel	w8, w2, w8, le
	str	w8, [x29, 2584]
	cmp	w2, 0
	blt	L658
	ldr	x2, [x29, 168]
	sub	w8, w8, #1
	sub	x6, x0, x2
	mov	x0, 0
	add	x6, x21, x6
	bne	L474
	b	L471
	.p2align 2,,3
L479:
	add	x0, x0, 1
	cmp	x0, 32
	beq	L478
L474:
	adds	w2, w1, w0
	bvs	L476
	cmp	w1, w2
	ccmp	w9, w2, 1, le
	blt	L478
	ldrb	w4, [x6, x0]
	add	x2, x26, x0
	strb	w4, [x2, 72]
	cmp	x0, x8
	bne	L479
L471:
	add	x20, x29, 1808
	mov	x0, x26
	mov	x8, x20
	bl	_khepri_mpt__hash_node
	ldr	x0, [x29, 160]
	mov	w6, 1
	add	x3, x29, 3152
	mov	x1, x26
	mov	x2, 672
	ldr	q30, [x29, 1808]
	strb	w6, [x29, 3148]
	ldr	q31, [x29, 1824]
	add	x4, x0, x25
	add	x0, x24, _khepri_mpt__node_store@PAGEOFF;
	add	x4, x25, x4, lsl 2
	add	x4, x25, x4, lsl 3
	str	q30, [x3, -36]
	add	x0, x0, x4, lsl 2
	str	q31, [x3, -20]
	bl	_memcpy
	b	L581
L468:
	add	x26, x29, 2480
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	mov	x2, 672
	mov	x0, x26
	stp	x6, x4, [x29, 120]
	add	x28, x29, 1808
	str	w8, [x29, 136]
	str	w11, [x29, 144]
	str	w10, [x29, 160]
	bl	_memcpy
	mov	w12, 3
	mov	x0, x28
	mov	x1, x26
	mov	x2, 672
	strb	w12, [x29, 2480]
	bl	_memcpy
	ldr	w8, [x29, 136]
	ldr	w10, [x29, 160]
	ldr	x4, [x29, 128]
	ldr	w11, [x29, 144]
	cmp	w10, w8
	bge	L482
	mov	x0, sp
	cmp	w10, 63
	ldr	x6, [x29, 120]
	str	x0, [x29, 160]
	bhi	L659
	ldr	x0, [x29, 152]
	ldrb	w0, [x0, w10, sxtw]
	str	w0, [x29, 120]
	cmp	w0, 15
	bhi	L660
	add	x8, x29, 1056
	ldr	x0, [x29, 152]
	add	w1, w10, 1
	stp	x6, x4, [x29, 128]
	str	x8, [x29, 144]
	str	w10, [x29, 152]
	bl	_khepri_mpt_types__remove_prefix
	ldr	w0, [x29, 1240]
	ldp	x6, x4, [x29, 128]
	ldr	x8, [x29, 144]
	ldr	w10, [x29, 152]
	tbnz	w0, #31, L661
	sub	w11, w0, #1
	add	x3, x29, 928
	cbz	w0, L486
	sxtw	x2, w0
	mov	w1, 0
	add	x0, x2, 15
	and	x0, x0, -16
	sub	sp, sp, x0
	stp	x6, x8, [x29, 104]
	mov	x0, sp
	str	x4, [x29, 128]
	str	w11, [x29, 136]
	str	w10, [x29, 144]
	str	x0, [x29, 152]
	bl	_memset
	ldr	x4, [x29, 128]
	ldp	x6, x8, [x29, 104]
	ldr	x3, [x29, 152]
	ldr	w11, [x29, 136]
	ldr	w10, [x29, 144]
L486:
	mov	x0, x8
	mov	x1, x3
	stp	x6, x4, [x29, 136]
	mov	x8, x26
	add	x2, x29, 936
	str	w10, [x29, 152]
	str	wzr, [x29, 936]
	str	w11, [x29, 940]
	bl	_khepri_mpt__make_leaf
	ldr	w14, [x29, 1240]
	ldp	x6, x4, [x29, 136]
	ldr	w10, [x29, 152]
	tbnz	w14, #31, L662
	sub	w11, w14, #1
	cbz	w14, L493
	dup	v27.4s, w11
	adrp	x1, lC13@PAGE
	adrp	x0, lC12@PAGE
	ldr	q28, [x1, #lC13@PAGEOFF]
	mvni	v19.4s, 0xf
	movi	v20.4s, 0x4
	movi	v21.4s, 0x10
	movi	v22.4s, 0x8
	movi	v23.4s, 0xc
	ldr	q25, [x0, #lC12@PAGEOFF]
	mov	x0, 0
L492:
	mov	v31.16b, v28.16b
	add	v30.4s, v28.4s, v20.4s
	umov	x12, v25.d[0]
	add	v28.4s, v28.4s, v21.4s
	add	v25.4s, v25.4s, v19.4s
	add	v29.4s, v31.4s, v22.4s
	add	v26.4s, v31.4s, v23.4s
	cmeq	v24.4s, v31.4s, v27.4s
	cmeq	v30.4s, v30.4s, v27.4s
	cmeq	v29.4s, v29.4s, v27.4s
	cmeq	v26.4s, v26.4s, v27.4s
	orr	v30.16b, v30.16b, v24.16b
	orr	v29.16b, v29.16b, v26.16b
	orr	v30.16b, v30.16b, v29.16b
	umaxp	v30.4s, v30.4s, v30.4s
	fmov	x1, d30
	cbz	x1, L495
	fmov	w0, s31
	sub	w12, w12, #1
	sxtw	x1, w0
	add	x8, x1, 1
	add	x8, x8, x12
	b	L494
	.p2align 2,,3
L663:
	add	x1, x1, 1
	cmp	x8, x1
	beq	L491
L494:
	add	x2, x6, x1
	add	x0, x26, x1
	ldrb	w2, [x2, 72]
	strb	w2, [x0, 72]
	cmp	w11, w1
	bne	L663
L493:
	add	x1, x29, 944
	mov	x0, x26
	str	x4, [x29, 128]
	mov	x8, x1
	str	w10, [x29, 136]
	str	x1, [x29, 152]
	str	w14, [x29, 2584]
	bl	_khepri_mpt__hash_node
	ldr	x8, [x29, 152]
	mov	w1, 1
	add	x2, x29, 3116
	mov	x0, x26
	ldr	q30, [x29, 944]
	str	w1, [x29, 144]
	strb	w1, [x29, 3148]
	add	x1, x29, 3152
	ldr	q31, [x8, 16]
	str	x2, [x29, 152]
	str	q30, [x1, -36]
	str	q31, [x2, 16]
	bl	_khepri_mpt__allocate_node
	ubfx	x0, x0, 32, 8
	ldr	x4, [x29, 128]
	cmp	w0, 1
	ldr	x2, [x29, 152]
	ldr	w10, [x29, 136]
	ldr	w1, [x29, 144]
	bhi	L664
	cbnz	w0, L496
	ldr	x0, [x29, 160]
	mov	w1, 5
	mov	sp, x0
	b	L446
L562:
	ldrb	w26, [x29, 976]
	cmp	w26, 15
	bhi	L665
	add	x8, x29, 1808
	mov	x0, x4
	mov	w1, 1
	stp	x6, x8, [x29, 160]
	bl	_khepri_mpt_types__remove_prefix
	ubfiz	x0, x26, 5, 8
	add	x3, x29, 1276
	add	x26, x0, w26, uxtw
	ldrb	w0, [x3, x26]
	ldp	x6, x8, [x29, 160]
	cmp	w0, 1
	bhi	L666
	cbnz	w0, L581
	add	x4, x29, 2480
	mov	x2, x20
	stp	x6, x3, [x29, 160]
	mov	x0, x8
	mov	x1, x21
	mov	x8, x4
	mov	x20, x4
	bl	_khepri_mpt__make_leaf
	mov	x0, x20
	bl	_khepri_mpt__allocate_node
	ubfx	x0, x0, 32, 8
	ldp	x6, x3, [x29, 160]
	cmp	w0, 1
	bhi	L667
	cbz	w0, L560
	add	x0, x29, 3152
	add	x1, x29, 1232
	str	x6, [x29, 168]
	add	x1, x1, x26
	mov	w21, 1
	ldr	q30, [x0, -36]
	add	x20, x29, 1056
	add	x24, x24, _khepri_mpt__node_store@PAGEOFF;
	mov	x8, x20
	strb	w21, [x3, x26]
	ldr	q31, [x0, -20]
	mov	x0, x6
	str	q30, [x1, 12]
	str	q31, [x1, 28]
	bl	_khepri_mpt__hash_node
	ldr	x6, [x29, 168]
	mov	x0, 676
	add	x3, x29, 1616
	mov	x2, 672
	strb	w21, [x29, 1804]
	ldr	q30, [x29, 1056]
	madd	x0, x25, x0, x24
	ldr	q31, [x29, 1072]
	mov	x1, x6
	str	q30, [x3, 156]
	str	q31, [x3, 172]
	bl	_memcpy
	b	L581
L563:
	str	wzr, [x29, 1240]
	b	L565
L482:
	add	x0, x29, 1104
	add	x1, x29, 1208
	add	x2, x29, 2128
	ldr	q30, [x0, 104]
	add	x0, x29, 1880
	ldr	q31, [x1, 16]
	ldr	w1, [x29, 1240]
	str	q30, [x2, -248]
	str	q31, [x0, 16]
	str	w1, [x29, 1912]
	cmp	w10, w11
	bge	L500
	cmp	w10, 63
	bhi	L668
L499:
	ldrb	w3, [x4, w10, sxtw]
	cmp	w3, 15
	bhi	L669
	add	x0, x29, 1056
	add	w1, w10, 1
	str	w3, [x29, 144]
	mov	x5, x0
	mov	x8, x0
	str	w10, [x29, 152]
	mov	x0, x4
	stp	x5, x4, [x29, 160]
	bl	_khepri_mpt_types__remove_prefix
	ldr	x5, [x29, 160]
	mov	x8, x26
	mov	x1, x21
	mov	x2, x20
	mov	x0, x5
	bl	_khepri_mpt__make_leaf
	mov	x0, x26
	bl	_khepri_mpt__allocate_node
	ubfx	x0, x0, 32, 8
	ldr	x4, [x29, 168]
	cmp	w0, 1
	ldr	w3, [x29, 144]
	ldr	w10, [x29, 152]
	bhi	L670
	cbz	w0, L560
	add	x0, x29, 3152
	add	x2, x29, 3152
	ubfiz	x1, x3, 5, 8
	add	x3, x1, w3, uxtw
	ldr	q30, [x0, -36]
	add	x0, x29, 1904
	add	x1, x29, 1948
	add	x0, x0, x3
	ldr	q31, [x2, -20]
	mov	w2, 1
	str	q30, [x0, 12]
	str	q31, [x0, 28]
	strb	w2, [x1, x3]
L505:
	add	x21, x29, 944
	mov	x0, x28
	str	x4, [x29, 160]
	mov	x8, x21
	str	w10, [x29, 168]
	add	x20, x29, 2444
	bl	_khepri_mpt__hash_node
	ldp	q30, q31, [x29, 944]
	mov	w0, 1
	strb	w0, [x29, 2476]
	add	x0, x29, 2640
	ldr	w10, [x29, 168]
	str	q30, [x0, -196]
	str	q31, [x20, 16]
	cbz	w10, L517
	sub	w0, w10, #1
	adrp	x3, lC13@PAGE
	ldr	x4, [x29, 160]
	movi	v31.4s, 0
	adrp	x1, lC14@PAGE
	mvni	v7.4s, 0xf
	dup	v26.4s, w0
	add	x8, x29, 1056
	ldr	q29, [x3, #lC13@PAGEOFF]
	mov	x6, x8
	movi	v16.4s, 0x4
	movi	v17.4s, 0x8
	movi	v18.4s, 0xc
	movi	v19.4s, 0x10
	ldr	q23, [x1, #lC14@PAGEOFF]
	movi	v20.16b, 0xf
	add	x1, x29, 1120
	mov	x2, x4
	str	q31, [x29, 1056]
	stp	q31, q31, [x8, 16]
	str	q31, [x8, 48]
	str	w10, [x29, 1120]
L518:
	ldr	q22, [x2]
	mov	v21.16b, v23.16b
	add	v25.4s, v29.4s, v16.4s
	mov	v30.16b, v29.16b
	add	v28.4s, v29.4s, v17.4s
	add	v24.4s, v29.4s, v18.4s
	mov	v31.16b, v29.16b
	add	v23.4s, v23.4s, v7.4s
	add	v29.4s, v29.4s, v19.4s
	cmhi	v27.16b, v22.16b, v20.16b
	umaxp	v27.4s, v27.4s, v27.4s
	fmov	x3, d27
	cbnz	x3, L525
	cmeq	v28.4s, v26.4s, v28.4s
	cmeq	v24.4s, v26.4s, v24.4s
	cmeq	v27.4s, v26.4s, v25.4s
	cmeq	v30.4s, v26.4s, v30.4s
	orr	v28.16b, v28.16b, v24.16b
	orr	v30.16b, v30.16b, v27.16b
	orr	v30.16b, v30.16b, v28.16b
	umaxp	v30.4s, v30.4s, v30.4s
	fmov	x3, d30
	cbnz	x3, L525
	add	x2, x2, 16
	str	q22, [x6], 16
	cmp	x1, x6
	bne	L518
L521:
	adrp	x0, lC8@PAGE
	mov	w1, 700
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L469:
	str	wzr, [x29, 2584]
	b	L471
L495:
	add	x2, x6, x0
	add	x1, x26, x0
	ldr	q31, [x2, 72]
	str	q31, [x1, 72]
	cbnz	x0, L491
	mov	x0, 16
	b	L492
L496:
	add	x0, x29, 3152
	ldr	w3, [x29, 120]
	ldr	q30, [x0, -36]
	add	x0, x29, 1904
	ldr	q31, [x2, 16]
	ubfiz	x6, x3, 5, 8
	add	x3, x6, w3, uxtw
	add	x0, x0, x3
	add	x6, x29, 1948
	str	q30, [x0, 12]
	str	q31, [x0, 28]
	strb	w1, [x6, x3]
	ldr	x0, [x29, 160]
	mov	sp, x0
	ldr	w0, [x29, 1040]
	tbnz	w0, #31, L498
	cmp	w10, w0
	blt	L499
L500:
	ldp	w2, w12, [x20]
	cmp	w12, w2
	blt	L506
	sxtw	x0, w12
	mov	x1, 2147483647
	sub	x0, x0, w2, sxtw
	add	x0, x0, 1
	sxtw	x11, w2
	cmp	x0, x1
	bgt	L671
	cmp	w0, 32
	mov	w8, 32
	csel	w8, w0, w8, le
	str	w8, [x29, 1912]
	cmp	w0, 0
	blt	L672
	beq	L505
	ldr	x0, [x29, 168]
	mov	x1, 0
	sub	w8, w8, #1
	sub	x11, x11, x0
	add	x11, x21, x11
	b	L510
	.p2align 2,,3
L625:
	ldrb	w6, [x11, x1]
	add	x0, x28, x1
	strb	w6, [x0, 72]
	cmp	x1, x8
	beq	L505
	add	x1, x1, 1
	cmp	x1, 32
	beq	L514
L510:
	adds	w0, w2, w1
	bvs	L512
	cmp	w12, w0
	ccmp	w2, w0, 0, ge
	ble	L625
L514:
	adrp	x0, lC8@PAGE
	mov	w1, 687
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L525:
	fmov	w1, s31
	sub	w10, w0, w1
	fmov	w0, s21
	sxtw	x2, w1
	add	x6, x2, 1
	add	x2, x2, 2
	add	x10, x10, x6
	sub	w1, w0, #1
	fmov	w0, s31
	add	x11, x2, x1
	add	w0, w0, 1
	sxtw	x0, w0
	b	L522
	.p2align 2,,3
L523:
	add	x0, x0, 1
	cmp	x11, x0
	beq	L521
L522:
	add	x2, x4, x0
	ldrb	w2, [x2, -1]
	cmp	w2, 15
	bhi	L673
	add	x6, x8, x0
	strb	w2, [x6, -1]
	cmp	x0, x10
	bne	L523
	mov	x0, x28
	str	x8, [x29, 168]
	bl	_khepri_mpt__allocate_node
	ubfx	x0, x0, 32, 8
	ldr	x8, [x29, 168]
	cmp	w0, 1
	bhi	L674
	cbz	w0, L560
	add	x2, x29, 176
	mov	x1, x20
	mov	x0, x8
	mov	x8, x2
	mov	x20, x2
	add	x24, x24, _khepri_mpt__node_store@PAGEOFF;
	bl	_khepri_mpt__make_extension
	mov	x1, x20
	mov	x2, 672
	mov	x0, x26
	bl	_memcpy
	mov	x0, 676
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	mov	x2, 672
	madd	x0, x25, x0, x24
	strb	wzr, [x0, 672]
	bl	_memcpy
	mov	x0, x26
	bl	_khepri_mpt__allocate_node
	mov	x28, x0
	ubfx	x0, x0, 32, 8
	cmp	w0, 1
	bls	L559
	adrp	x0, lC8@PAGE
	mov	w1, 712
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L648:
	add	x1, x29, 1056
	ldr	x0, [x29, 152]
	mov	x3, x1
	mov	x8, x1
	str	x4, [x29, 136]
	mov	w1, 1
	str	w2, [x29, 144]
	str	x3, [x29, 152]
	bl	_khepri_mpt_types__remove_prefix
	ldr	x3, [x29, 152]
	mov	x8, x26
	add	x1, x29, 1244
	mov	x0, x3
	bl	_khepri_mpt__make_extension
	mov	x0, x26
	bl	_khepri_mpt__allocate_node
	ubfx	x0, x0, 32, 8
	ldr	x4, [x29, 136]
	cmp	w0, 1
	ldr	w2, [x29, 144]
	bhi	L675
	cbz	w0, L560
	add	x0, x29, 3152
	add	x5, x29, 3152
	add	x3, x29, 1904
	add	x1, x29, 1948
	ldr	q30, [x0, -36]
	ubfiz	x0, x2, 5, 8
	add	x0, x0, w2, uxtw
	add	x2, x3, x0
	mov	w6, 1
	ldr	q31, [x5, -20]
	str	q30, [x2, 12]
	str	q31, [x2, 28]
	strb	w6, [x1, x0]
	b	L541
L543:
	ldp	w1, w10, [x20]
	cmp	w10, w1
	blt	L548
	sxtw	x0, w10
	mov	x2, 2147483647
	sub	x0, x0, w1, sxtw
	add	x0, x0, 1
	sxtw	x8, w1
	cmp	x0, x2
	bgt	L676
	cmp	w0, 32
	mov	w6, 32
	csel	w6, w0, w6, le
	str	w6, [x29, 1912]
	cmp	w0, 0
	blt	L677
	beq	L547
	ldr	x2, [x29, 168]
	mov	x0, 0
	sub	w6, w6, #1
	sub	x8, x8, x2
	add	x8, x21, x8
	b	L552
	.p2align 2,,3
L629:
	ldrb	w4, [x8, x0]
	add	x2, x28, x0
	strb	w4, [x2, 72]
	cmp	x6, x0
	beq	L547
	add	x0, x0, 1
	cmp	x0, 32
	beq	L556
L552:
	adds	w2, w1, w0
	bvs	L554
	cmp	w10, w2
	ccmp	w1, w2, 0, ge
	ble	L629
L556:
	adrp	x0, lC8@PAGE
	mov	w1, 783
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L506:
	str	wzr, [x29, 1912]
	b	L505
L517:
	add	x24, x24, _khepri_mpt__node_store@PAGEOFF;
	mov	x0, 676
	adrp	x1, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x1, [x1, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	mov	x2, 672
	madd	x0, x25, x0, x24
	strb	wzr, [x0, 672]
	bl	_memcpy
	mov	x0, x28
	bl	_khepri_mpt__allocate_node
	mov	x28, x0
	ubfx	x0, x0, 32, 8
	cmp	w0, 1
	bls	L559
	adrp	x0, lC8@PAGE
	mov	w1, 721
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L548:
	str	wzr, [x29, 1912]
	b	L547
L444:
	adrp	x0, lC8@PAGE
	mov	w1, 865
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L634:
	adrp	x0, lC11@PAGE
	adrp	x1, lC5@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC5@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L572:
	adrp	x0, lC8@PAGE
	mov	w1, 807
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L478:
	adrp	x0, lC8@PAGE
	mov	w1, 631
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L570:
	adrp	x0, lC8@PAGE
	mov	w1, 807
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L476:
	adrp	x0, lC8@PAGE
	mov	w1, 631
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L636:
	adrp	x0, lC8@PAGE
	mov	w1, 874
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L635:
	adrp	x0, lC8@PAGE
	mov	w1, 871
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L676:
	adrp	x0, lC8@PAGE
	mov	w1, 781
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L658:
	adrp	x0, lC8@PAGE
	mov	w1, 630
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L467:
	adrp	x0, lC8@PAGE
	mov	w1, 626
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L656:
	adrp	x0, lC8@PAGE
	mov	w1, 624
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L645:
	adrp	x0, lC8@PAGE
	mov	w1, 731
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L647:
	adrp	x0, lC8@PAGE
	mov	w1, 744
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L646:
	adrp	x0, lC8@PAGE
	mov	w1, 733
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L649:
	adrp	x0, lC8@PAGE
	mov	w1, 766
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L674:
	adrp	x0, lC8@PAGE
	mov	w1, 704
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L652:
	adrp	x0, lC8@PAGE
	mov	w1, 790
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L651:
	adrp	x0, lC8@PAGE
	mov	w1, 773
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L650:
	adrp	x0, lC8@PAGE
	mov	w1, 768
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L498:
	adrp	x0, lC8@PAGE
	mov	w1, 670
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L642:
	adrp	x0, lC8@PAGE
	mov	w1, 899
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L641:
	adrp	x0, lC8@PAGE
	mov	w1, 898
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L657:
	adrp	x0, lC8@PAGE
	mov	w1, 629
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L639:
	adrp	x0, lC8@PAGE
	mov	w1, 611
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L638:
	adrp	x0, lC8@PAGE
	mov	w1, 604
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L643:
	adrp	x0, lC8@PAGE
	mov	w1, 877
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L644:
	adrp	x0, lC8@PAGE
	mov	w1, 881
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L673:
	adrp	x0, lC8@PAGE
	mov	w1, 700
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L668:
	adrp	x0, lC8@PAGE
	mov	w1, 672
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L670:
	adrp	x0, lC8@PAGE
	mov	w1, 677
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L669:
	adrp	x0, lC8@PAGE
	mov	w1, 672
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L655:
	adrp	x0, lC8@PAGE
	mov	w1, 806
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L654:
	adrp	x0, lC8@PAGE
	mov	w1, 805
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L664:
	adrp	x0, lC8@PAGE
	mov	w1, 658
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L675:
	adrp	x0, lC8@PAGE
	mov	w1, 753
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L653:
	adrp	x0, lC8@PAGE
	mov	w1, 803
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L640:
	adrp	x0, lC8@PAGE
	mov	w1, 616
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L554:
	adrp	x0, lC8@PAGE
	mov	w1, 783
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L677:
	adrp	x0, lC8@PAGE
	mov	w1, 782
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L512:
	adrp	x0, lC8@PAGE
	mov	w1, 687
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L672:
	adrp	x0, lC8@PAGE
	mov	w1, 686
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L671:
	adrp	x0, lC8@PAGE
	mov	w1, 685
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L659:
	adrp	x0, lC8@PAGE
	mov	w1, 646
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L491:
	adrp	x0, lC8@PAGE
	mov	w1, 652
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L661:
	adrp	x0, lC8@PAGE
	mov	w1, 648
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L660:
	adrp	x0, lC8@PAGE
	mov	w1, 646
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L662:
	adrp	x0, lC8@PAGE
	mov	w1, 651
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L667:
	adrp	x0, lC8@PAGE
	mov	w1, 825
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L666:
	adrp	x0, lC8@PAGE
	mov	w1, 819
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L665:
	adrp	x0, lC8@PAGE
	mov	w1, 816
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE32:
	.const
	.align	2
lC5:
	.word	1
	.word	42
	.text
	.const
	.align	3
lC15:
	.ascii "failed precondition from khepri_mpt.ads:97"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__get
_khepri_mpt__get:
LFB33:
	sub	sp, sp, #896
LCFI100:
	mov	w4, w0
	stp	x29, x30, [sp]
LCFI101:
	mov	x29, sp
LCFI102:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
LCFI103:
	ldp	w5, w6, [x2]
	sxtw	x7, w5
	sxtw	x0, w6
	add	x8, x7, 31
	cmp	x8, x0
	blt	L724
	stp	xzr, xzr, [x3]
	mov	x21, x3
	stp	xzr, xzr, [x3, 16]
	str	wzr, [x3, 32]
	cmp	w4, 255
	bhi	L682
	cbz	w4, L707
	sxtw	x22, w4
	sbfiz	x19, x4, 1, 32
	adrp	x20, _khepri_mpt__tries@PAGE
	add	x4, x19, x22
	mov	x9, x2
	add	x2, x20, _khepri_mpt__tries@PAGEOFF;
	lsl	x3, x4, 2
	add	x4, x2, x4, lsl 2
	ldrb	w4, [x4, 8]
	cmp	w4, 1
	bhi	L682
	cbz	w4, L708
	ldr	w2, [x2, x3]
	tbnz	w2, #31, L725
	mov	w3, 2147483647
	cmp	w2, w3
	beq	L701
	cmp	w6, w5
	blt	L709
	sub	x0, x0, x7
	mov	x2, 2147483647
	add	x0, x0, 1
	cmp	x0, x2
	bgt	L686
	mov	w2, w0
L685:
	add	x23, x29, 64
	mov	x0, x1
	mov	x8, x23
	mov	x1, x9
	bl	_khepri_mpt_types__bytes_to_nibbles
	ldr	q28, [x29, 64]
	add	x24, x29, 152
	add	x20, x20, _khepri_mpt__tries@PAGEOFF;
	add	x4, x19, x22
	ldp	q30, q29, [x23, 16]
	ldr	q31, [x23, 48]
	str	q28, [x29, 152]
	ldr	w1, [x23, 64]
	str	q31, [x24, 48]
	stp	q30, q29, [x24, 16]
	ldr	w0, [x20, x4, lsl 2]
	str	w1, [x24, 64]
	tbnz	w0, #31, L726
	mov	w1, 16959
	movk	w1, 0xf, lsl 16
	cmp	w0, w1
	bgt	L701
	sxtw	x2, w0
	add	x1, x2, w0, sxtw 2
	adrp	x0, _khepri_mpt__node_store@PAGE
	add	x1, x2, x1, lsl 2
	add	x0, x0, _khepri_mpt__node_store@PAGEOFF;
	add	x1, x2, x1, lsl 3
	add	x1, x0, x1, lsl 2
	ldrb	w0, [x1, 672]
	cmp	w0, 1
	bhi	L727
	cbz	w0, L701
	mov	x2, 672
	add	x0, x29, 224
	bl	_memcpy
	ldrb	w0, [x29, 224]
	cmp	w0, 3
	bhi	L728
	cmp	w0, 1
	beq	L691
	cmp	w0, 2
	beq	L692
	cbz	w0, L701
	ldr	w0, [x29, 216]
	cmp	w0, 0
	blt	L729
	bne	L703
	ldr	w0, [x29, 328]
	cmp	w0, 0
	blt	L730
	beq	L701
L696:
	add	x0, x29, 296
	add	x1, x29, 512
	mov	w4, 1
	ldr	q31, [x1, -216]
	mov	w1, 0
	ldr	q30, [x0, 16]
	ldr	w0, [x29, 328]
	stp	q31, q30, [x21]
	str	w0, [x21, 32]
	b	L681
	.p2align 2,,3
L707:
	mov	w1, 2
	mov	w4, 0
L681:
	mov	x0, 0
	bfi	x0, x4, 0, 8
	bfi	x0, x1, 8, 8
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	add	sp, sp, 896
LCFI104:
	ret
	.p2align 2,,3
L691:
LCFI105:
	mov	x1, x24
	add	x0, x29, 228
	bl	_khepri_mpt_types__keys_equal
	cmp	w0, 1
	bhi	L731
	cbnz	w0, L696
	.p2align 5,,15
L701:
	mov	w1, 1
	mov	w4, 0
	b	L681
	.p2align 2,,3
L708:
	mov	w1, 2
	b	L681
	.p2align 2,,3
L709:
	mov	w2, 0
	b	L685
	.p2align 2,,3
L692:
	mov	x1, x24
	add	x0, x29, 228
	bl	_khepri_mpt_types__is_prefix
	cmp	w0, 1
	bhi	L732
	cbz	w0, L701
	ldr	w1, [x29, 292]
	tbnz	w1, #31, L733
	mov	x8, x23
	mov	x0, x24
	bl	_khepri_mpt_types__remove_prefix
	b	L701
L703:
	ldrb	w0, [x29, 152]
	cmp	w0, 15
	bhi	L734
	add	x1, x29, 364
	ubfiz	x2, x0, 5, 8
	add	x0, x2, w0, uxtw
	ldrb	w0, [x1, x0]
	cmp	w0, 1
	bhi	L735
	cbz	w0, L701
	mov	x8, x23
	mov	x0, x24
	mov	w1, 1
	bl	_khepri_mpt_types__remove_prefix
	b	L701
L682:
	adrp	x0, lC8@PAGE
	mov	w1, 921
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L724:
	adrp	x0, lC15@PAGE
	adrp	x1, lC5@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC5@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L726:
	adrp	x0, lC8@PAGE
	mov	w1, 932
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L725:
	adrp	x0, lC8@PAGE
	mov	w1, 926
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L728:
	adrp	x0, lC8@PAGE
	mov	w1, 939
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L730:
	adrp	x0, lC8@PAGE
	mov	w1, 968
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L727:
	adrp	x0, lC8@PAGE
	mov	w1, 935
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L686:
	adrp	x0, lC8@PAGE
	mov	w1, 931
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L735:
	adrp	x0, lC8@PAGE
	mov	w1, 981
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L734:
	adrp	x0, lC8@PAGE
	mov	w1, 979
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L729:
	adrp	x0, lC8@PAGE
	mov	w1, 966
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L731:
	adrp	x0, lC8@PAGE
	mov	w1, 945
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L733:
	adrp	x0, lC8@PAGE
	mov	w1, 957
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L732:
	adrp	x0, lC8@PAGE
	mov	w1, 955
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE33:
	.const
	.align	3
lC16:
	.ascii "failed precondition from khepri_mpt.ads:107"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__delete
_khepri_mpt__delete:
LFB37:
	stp	x29, x30, [sp, -208]!
LCFI106:
	mov	x29, sp
LCFI107:
	mov	x4, x2
	mov	w3, w0
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI108:
	ldp	w5, w6, [x2]
	sxtw	x7, w5
	sxtw	x0, w6
	add	x2, x7, 31
	cmp	x2, x0
	blt	L757
	cmp	w3, 255
	bhi	L740
	cbz	w3, L749
	sxtw	x22, w3
	sbfiz	x19, x3, 1, 32
	adrp	x20, _khepri_mpt__tries@PAGE
	add	x2, x19, x22
	add	x3, x20, _khepri_mpt__tries@PAGEOFF;
	lsl	x8, x2, 2
	add	x2, x3, x2, lsl 2
	ldrb	w2, [x2, 8]
	cmp	w2, 1
	bhi	L740
	cbz	w2, L750
	ldr	w2, [x3, x8]
	tbnz	w2, #31, L758
	mov	w3, 2147483647
	cmp	w2, w3
	beq	L751
	cmp	w6, w5
	blt	L752
	sub	x0, x0, x7
	mov	x3, 2147483647
	add	x2, x0, 1
	cmp	x2, x3
	bgt	L759
L742:
	add	x21, x29, 48
	mov	x0, x1
	mov	x8, x21
	mov	x1, x4
	bl	_khepri_mpt_types__bytes_to_nibbles
	ldr	q28, [x29, 48]
	add	x1, x29, 136
	add	x3, x19, x22
	add	x20, x20, _khepri_mpt__tries@PAGEOFF;
	lsl	x22, x3, 2
	ldp	q30, q29, [x21, 16]
	add	x19, x20, x3, lsl 2
	ldr	q31, [x21, 48]
	str	q28, [x29, 136]
	ldr	w2, [x21, 64]
	str	q31, [x1, 48]
	stp	q30, q29, [x1, 16]
	ldr	w0, [x20, x22]
	str	w2, [x1, 64]
	tbnz	w0, #31, L760
	bl	_khepri_mpt__delete_from_node
	ubfx	x2, x0, 32, 8
	ubfx	x1, x0, 40, 8
	cmp	w2, 1
	bhi	L761
	cbz	w2, L739
	tbnz	w0, #31, L762
	ldr	w2, [x19, 4]
	str	w0, [x20, x22]
	cmp	w2, 0
	blt	L763
	beq	L748
	sub	w2, w2, #1
	str	w2, [x19, 4]
L748:
	mov	w2, 1
	b	L739
	.p2align 2,,3
L749:
	mov	w1, 2
	mov	w2, 0
L739:
	mov	x0, 0
	bfi	x0, x2, 0, 8
	bfi	x0, x1, 8, 8
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 208
LCFI109:
	ret
	.p2align 2,,3
L750:
LCFI110:
	mov	x0, 0
	mov	w1, 2
	bfi	x0, x2, 0, 8
	bfi	x0, x1, 8, 8
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 208
LCFI111:
	ret
	.p2align 2,,3
L751:
LCFI112:
	mov	w2, 0
	mov	w1, 1
	b	L739
	.p2align 2,,3
L752:
	mov	w2, 0
	b	L742
L740:
	adrp	x0, lC8@PAGE
	mov	w1, 1160
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L757:
	adrp	x0, lC16@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L758:
	adrp	x0, lC8@PAGE
	mov	w1, 1165
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L761:
	adrp	x0, lC8@PAGE
	mov	w1, 1182
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L760:
	adrp	x0, lC8@PAGE
	mov	w1, 1175
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L759:
	adrp	x0, lC8@PAGE
	mov	w1, 1171
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L763:
	adrp	x0, lC8@PAGE
	mov	w1, 1189
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L762:
	adrp	x0, lC8@PAGE
	mov	w1, 1183
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE37:
	.const
	.align	3
lC17:
	.ascii "failed precondition from khepri_mpt.ads:116"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__contains
_khepri_mpt__contains:
LFB38:
	stp	x29, x30, [sp, -64]!
LCFI113:
	mov	x29, sp
LCFI114:
	ldpsw	x4, x6, [x2]
	add	x3, x4, 31
	cmp	x3, x6
	blt	L769
	cmp	w0, 255
	bhi	L770
	add	x3, x29, 24
	bl	_khepri_mpt__get
	and	w0, w0, 255
	cmp	w0, 1
	bhi	L771
	ldp	x29, x30, [sp], 64
LCFI115:
	ret
L769:
LCFI116:
	adrp	x0, lC17@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L771:
	adrp	x0, lC8@PAGE
	mov	w1, 1210
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L770:
	adrp	x0, lC8@PAGE
	mov	w1, 1209
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE38:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__root_hash
_khepri_mpt__root_hash:
LFB39:
	stp	x29, x30, [sp, -16]!
LCFI117:
	mov	x29, sp
LCFI118:
	cmp	w0, 255
	bhi	L775
	mov	x1, x8
	cbz	w0, L774
	adrp	x2, _khepri_mpt__tries@PAGE
	sbfiz	x3, x0, 1, 32
	add	x0, x3, w0, sxtw
	add	x2, x2, _khepri_mpt__tries@PAGEOFF;
	lsl	x3, x0, 2
	add	x0, x2, x0, lsl 2
	ldrb	w0, [x0, 8]
	cmp	w0, 1
	bhi	L775
	cbz	w0, L774
	ldr	w0, [x2, x3]
	tbnz	w0, #31, L792
	mov	w2, 2147483647
	cmp	w0, w2
	beq	L793
	mov	w2, 16959
	movk	w2, 0xf, lsl 16
	cmp	w0, w2
	bgt	L774
	sxtw	x3, w0
	sbfiz	x0, x0, 2, 32
	add	x2, x0, x3
	adrp	x5, _khepri_mpt__node_store@PAGE
	add	x2, x3, x2, lsl 2
	add	x4, x5, _khepri_mpt__node_store@PAGEOFF;
	add	x2, x3, x2, lsl 3
	add	x2, x4, x2, lsl 2
	ldrb	w4, [x2, 672]
	cmp	w4, 1
	bhi	L794
	cbz	w4, L774
	ldrb	w2, [x2, 668]
	cmp	w2, 1
	bhi	L795
	cbnz	w2, L796
	.p2align 5,,15
L774:
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
L772:
	ldp	x29, x30, [sp], 16
LCFI119:
	ret
	.p2align 2,,3
L793:
LCFI120:
	adrp	x0, lC6@PAGE
	adrp	x1, lC7@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC7@PAGEOFF;
	bl	_khepri_mpt__keccak_256
	b	L772
	.p2align 2,,3
L796:
	add	x0, x0, x3
	add	x5, x5, _khepri_mpt__node_store@PAGEOFF;
	add	x0, x3, x0, lsl 2
	add	x0, x3, x0, lsl 3
	add	x0, x5, x0, lsl 2
	add	x0, x0, 636
	ldp	q31, q30, [x0]
	stp	q31, q30, [x8]
	b	L772
L775:
	adrp	x0, lC8@PAGE
	mov	w1, 1219
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L792:
	adrp	x0, lC8@PAGE
	mov	w1, 1223
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L794:
	adrp	x0, lC8@PAGE
	mov	w1, 1229
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L795:
	adrp	x0, lC8@PAGE
	mov	w1, 1230
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE39:
	.const
lC6:
	.byte	-128
	.align	2
lC7:
	.word	0
	.word	0
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__is_empty
_khepri_mpt__is_empty:
LFB40:
	stp	x29, x30, [sp, -16]!
LCFI121:
	mov	x29, sp
LCFI122:
	cmp	w0, 255
	bhi	L800
	cbz	w0, L803
	adrp	x1, _khepri_mpt__tries@PAGE
	sbfiz	x2, x0, 1, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_mpt__tries@PAGEOFF;
	lsl	x2, x0, 2
	add	x0, x1, x0, lsl 2
	ldrb	w0, [x0, 8]
	cmp	w0, 1
	bhi	L800
	cbz	w0, L803
	ldr	w0, [x1, x2]
	tbnz	w0, #31, L805
	mov	w1, 2147483647
	cmp	w0, w1
	cset	w0, eq
	ldp	x29, x30, [sp], 16
LCFI123:
	ret
	.p2align 2,,3
L803:
LCFI124:
	mov	w0, 1
	ldp	x29, x30, [sp], 16
LCFI125:
	ret
L800:
LCFI126:
	adrp	x0, lC8@PAGE
	mov	w1, 1244
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L805:
	adrp	x0, lC8@PAGE
	mov	w1, 1247
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE40:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__node_count
_khepri_mpt__node_count:
LFB41:
	stp	x29, x30, [sp, -16]!
LCFI127:
	mov	x29, sp
LCFI128:
	cmp	w0, 255
	bhi	L809
	cbz	w0, L811
	adrp	x1, _khepri_mpt__tries@PAGE
	sbfiz	x2, x0, 1, 32
	add	x0, x2, w0, sxtw
	add	x1, x1, _khepri_mpt__tries@PAGEOFF;
	add	x0, x1, x0, lsl 2
	ldrb	w1, [x0, 8]
	cmp	w1, 1
	bhi	L809
	cbz	w1, L811
	ldr	w0, [x0, 4]
	tbz	w0, #31, L806
	adrp	x0, lC8@PAGE
	mov	w1, 1259
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L811:
	mov	w0, 0
L806:
	ldp	x29, x30, [sp], 16
LCFI129:
	ret
L809:
LCFI130:
	adrp	x0, lC8@PAGE
	mov	w1, 1256
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE41:
	.const
	.align	3
lC18:
	.ascii "failed precondition from khepri_mpt.ads:150"
	.text
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__generate_proof
_khepri_mpt__generate_proof:
LFB42:
	sub	sp, sp, #1472
LCFI131:
	stp	x29, x30, [sp]
LCFI132:
	mov	x29, sp
LCFI133:
	stp	x19, x20, [sp, 16]
LCFI134:
	mov	x20, x3
	stp	x21, x22, [sp, 32]
LCFI135:
	mov	w21, w0
	stp	x23, x24, [sp, 48]
LCFI136:
	mov	x23, x2
	mov	x24, x1
	stp	x25, x26, [sp, 64]
	str	x27, [sp, 80]
LCFI137:
	ldp	w1, w2, [x2]
	sxtw	x3, w1
	sxtw	x0, w2
	add	x4, x3, 31
	cmp	x4, x0
	blt	L876
	cmp	w2, w1
	bge	L877
	mov	w22, 0
L815:
	mov	x19, 0
	mov	x3, x20
	.p2align 5,,15
L817:
	mov	x0, x3
	mov	x2, 532
	mov	w1, 0
	bl	_memset
	add	x4, x19, x19, lsl 4
	add	x3, x0, 536
	lsl	x4, x4, 2
	sub	x4, x4, x19
	add	x19, x19, 1
	add	x4, x20, x4, lsl 3
	str	wzr, [x4, 532]
	cmp	x19, 64
	bne	L817
	add	x25, x20, 32768
	add	x19, x29, 96
	mov	x0, 34816
	mov	w2, w22
	add	x26, x20, x0
	mov	x1, x23
	str	wzr, [x25, 1536]
	mov	x0, x24
	mov	x8, x19
	bl	_khepri_mpt_types__bytes_to_nibbles
	ldp	q28, q31, [x19, 16]
	sub	x22, x26, #508
	sub	x0, x26, #440
	ldr	q29, [x29, 96]
	ldr	q30, [x19, 48]
	stp	q29, q28, [x22]
	ldr	w1, [x19, 64]
	stp	q31, q30, [x22, 32]
	str	w1, [x22, 64]
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	str	wzr, [x0, 32]
	strb	wzr, [x25, 1644]
	cmp	w21, 255
	bhi	L820
	cbz	w21, L854
	sxtw	x27, w21
	adrp	x22, _khepri_mpt__tries@PAGE
	sbfiz	x21, x21, 1, 32
	add	x1, x22, _khepri_mpt__tries@PAGEOFF;
	add	x0, x21, x27
	lsl	x3, x0, 2
	add	x0, x1, x0, lsl 2
	ldrb	w2, [x0, 8]
	cmp	w2, 1
	bhi	L820
	cbz	w2, L855
	ldr	w0, [x1, x3]
	tbnz	w0, #31, L878
	mov	w1, 2147483647
	cmp	w0, w1
	beq	L856
	ldp	w1, w0, [x23]
	cmp	w0, w1
	blt	L857
	sxtw	x2, w0
	mov	x0, 2147483647
	sub	x2, x2, w1, sxtw
	add	x2, x2, 1
	cmp	x2, x0
	bgt	L879
L823:
	mov	x1, x23
	mov	x0, x24
	mov	x8, x19
	add	x23, x29, 184
	bl	_khepri_mpt_types__bytes_to_nibbles
	ldr	q28, [x29, 96]
	add	x22, x22, _khepri_mpt__tries@PAGEOFF;
	add	x21, x21, x27
	ldp	q30, q29, [x19, 16]
	ldr	q31, [x19, 48]
	str	q28, [x29, 184]
	ldr	w1, [x19, 64]
	str	q31, [x23, 48]
	stp	q30, q29, [x23, 16]
	ldr	w0, [x22, x21, lsl 2]
	str	w1, [x23, 64]
	tbnz	w0, #31, L880
	mov	w1, 16959
	movk	w1, 0xf, lsl 16
	cmp	w0, w1
	bgt	L856
	sxtw	x2, w0
	add	x1, x2, w0, sxtw 2
	adrp	x0, _khepri_mpt__node_store@PAGE
	add	x1, x2, x1, lsl 2
	add	x0, x0, _khepri_mpt__node_store@PAGEOFF;
	add	x1, x2, x1, lsl 3
	add	x1, x0, x1, lsl 2
	ldrb	w2, [x1, 672]
	cmp	w2, 1
	bhi	L881
	mov	w0, 0
	cbnz	w2, L882
L822:
	mov	w1, 0
	mov	w2, 1
	str	w0, [x25, 1536]
	b	L819
L877:
	sub	x22, x0, x3
	mov	x0, 2147483647
	add	x22, x22, 1
	cmp	x22, x0
	ble	L815
	adrp	x0, lC8@PAGE
	mov	w1, 1283
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L854:
	mov	w1, 2
	mov	w2, 0
L819:
	mov	x0, 0
	bfi	x0, x2, 0, 8
	bfi	x0, x1, 8, 8
	ldr	x27, [sp, 80]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	add	sp, sp, 1472
LCFI138:
	ret
L855:
LCFI139:
	mov	w1, 2
	b	L819
L856:
	mov	w0, 0
	b	L822
L857:
	mov	w2, 0
	b	L823
L882:
	add	x3, x29, 800
	mov	x2, 672
	mov	x0, x3
	add	x21, x29, 256
	bl	_memcpy
	adrp	x2, lC0@PAGE
	mov	x1, x21
	add	x2, x2, lC0@PAGEOFF;
	bl	_khepri_mpt__encode_node
	ubfx	x2, x0, 32, 8
	mov	x3, x0
	cmp	w2, 1
	bhi	L883
	cbz	w2, L855
	cmp	w0, 0
	blt	L884
	beq	L836
	uxtw	x4, w0
	sub	x2, x20, #1
	mov	x0, 1
	b	L834
	.p2align 2,,3
L835:
	add	x0, x0, 1
	cmp	x0, 533
	beq	L885
L834:
	add	x1, x21, x0
	ldrb	w1, [x1, -1]
	strb	w1, [x2, x0]
	cmp	x4, x0
	bne	L835
L836:
	ldrb	w21, [x29, 800]
	str	w3, [x20, 532]
	cmp	w21, 3
	bhi	L886
	cmp	w21, 1
	beq	L837
	cmp	w21, 2
	beq	L838
	cbz	w21, L844
	ldr	w0, [x29, 248]
	cmp	w0, 0
	blt	L887
	bne	L848
	ldr	w0, [x29, 904]
	cmp	w0, 0
	blt	L888
	mov	w1, 0
	beq	L850
	add	x0, x29, 872
	add	x1, x29, 1024
	sub	x26, x26, #440
	ldr	q31, [x1, -152]
	mov	w1, 1
	ldr	q30, [x0, 16]
	ldr	w0, [x29, 904]
	stp	q31, q30, [x26]
	str	w0, [x26, 32]
L850:
	mov	w0, 1
	strb	w1, [x25, 1644]
	str	w0, [x25, 1536]
L840:
	mov	w1, 0
	mov	w2, 1
	b	L819
L844:
	mov	w0, 1
	strb	wzr, [x25, 1644]
	str	w0, [x25, 1536]
	b	L840
L838:
	mov	x1, x23
	add	x0, x29, 804
	bl	_khepri_mpt_types__is_prefix
	cmp	w0, 1
	bhi	L889
	cbz	w0, L844
	ldr	w1, [x29, 868]
	tbnz	w1, #31, L890
	mov	x0, x23
	mov	x8, x19
	bl	_khepri_mpt_types__remove_prefix
	mov	w0, 1
	b	L822
L837:
	mov	x1, x23
	add	x0, x29, 804
	bl	_khepri_mpt_types__keys_equal
	cmp	w0, 1
	bhi	L891
	cbz	w0, L842
	add	x1, x29, 872
	add	x0, x29, 1024
	sub	x26, x26, #440
	ldr	q31, [x0, -152]
	mov	w0, w21
	ldr	q30, [x1, 16]
	ldr	w1, [x29, 904]
	stp	q31, q30, [x26]
	str	w1, [x26, 32]
L842:
	mov	w1, 1
	strb	w0, [x25, 1644]
	str	w1, [x25, 1536]
	b	L840
L891:
	adrp	x0, lC8@PAGE
	mov	w1, 1338
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L888:
	adrp	x0, lC8@PAGE
	mov	w1, 1367
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L848:
	ldrb	w0, [x29, 184]
	cmp	w0, 15
	bhi	L892
	add	x1, x29, 940
	ubfiz	x2, x0, 5, 8
	add	x0, x2, w0, uxtw
	ldrb	w0, [x1, x0]
	cmp	w0, 1
	bhi	L893
	cbz	w0, L844
	mov	x0, x23
	mov	x8, x19
	mov	w1, 1
	bl	_khepri_mpt_types__remove_prefix
	mov	w0, 1
	b	L822
L887:
	adrp	x0, lC8@PAGE
	mov	w1, 1365
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L893:
	adrp	x0, lC8@PAGE
	mov	w1, 1383
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L892:
	adrp	x0, lC8@PAGE
	mov	w1, 1380
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L884:
	adrp	x0, lC8@PAGE
	mov	w1, 1322
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L883:
	adrp	x0, lC8@PAGE
	mov	w1, 1316
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L886:
	adrp	x0, lC8@PAGE
	mov	w1, 1328
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L890:
	adrp	x0, lC8@PAGE
	mov	w1, 1352
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L889:
	adrp	x0, lC8@PAGE
	mov	w1, 1350
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L876:
	adrp	x0, lC18@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L820:
	adrp	x0, lC8@PAGE
	mov	w1, 1290
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L878:
	adrp	x0, lC8@PAGE
	mov	w1, 1296
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L880:
	adrp	x0, lC8@PAGE
	mov	w1, 1304
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L879:
	adrp	x0, lC8@PAGE
	mov	w1, 1303
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L881:
	adrp	x0, lC8@PAGE
	mov	w1, 1308
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L885:
	adrp	x0, lC8@PAGE
	mov	w1, 1323
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE42:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__verify_proof
_khepri_mpt__verify_proof:
LFB43:
	sub	sp, sp, #688
LCFI140:
	mov	x2, 532
	stp	x29, x30, [sp]
LCFI141:
	mov	x29, sp
LCFI142:
	stp	x19, x20, [sp, 16]
LCFI143:
	add	x19, x29, 152
	mov	x20, x1
	mov	w1, 0
	stp	x21, x22, [sp, 32]
LCFI144:
	mov	x21, x0
	mov	x0, x19
	add	x22, x20, 32768
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI145:
	bl	_memset
	ldr	w0, [x22, 1536]
	cmp	w0, 0
	blt	L934
	beq	L935
	ldr	w3, [x20, 532]
	tbnz	w3, #31, L936
	cmp	w3, 532
	ccmp	w3, 0, 4, le
	beq	L902
	sub	w5, w3, #1
	mov	x0, 1
	uxtw	x3, w3
	sub	x4, x20, #1
	b	L903
	.p2align 2,,3
L904:
	add	x0, x0, 1
	cmp	x0, 533
	beq	L937
L903:
	ldrb	w2, [x4, x0]
	add	x1, x19, x0
	strb	w2, [x1, -1]
	cmp	x0, x3
	bne	L904
	add	x28, x29, 120
	mov	x0, x19
	stp	wzr, w5, [x29, 104]
	mov	x8, x28
	add	x1, x29, 104
	bl	_khepri_mpt__keccak_256
	ldr	q31, [x21]
	ldr	q30, [x29, 120]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L902
	ldr	q31, [x21, 16]
	ldr	q30, [x28, 16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L902
	ldr	w0, [x22, 1536]
	tbnz	w0, #31, L938
	sub	w23, w0, #1
	cbz	w0, L921
	mov	x27, 0
	add	x26, x20, 1
	mov	w25, -1
	add	x24, x29, 112
	.p2align 5,,15
L920:
	add	w25, w25, 1
	cmp	w25, 63
	bgt	L939
L909:
	add	x0, x27, x27, lsl 4
	lsl	x0, x0, 2
	sub	x0, x0, x27
	add	x0, x20, x0, lsl 3
	ldr	w5, [x0, 532]
	cmp	w5, 0
	blt	L940
	beq	L902
	sub	w6, w5, #1
	mov	x2, -1
	sub	w5, w5, #1
	.p2align 5,,15
L916:
	cmp	w25, 63
	bgt	L914
	cmp	x2, 531
	beq	L913
	ldrb	w4, [x26, x2]
	add	x3, x19, x2
	add	x2, x2, 1
	strb	w4, [x3, 1]
	cmp	x2, x5
	bne	L916
L915:
	mov	x8, x28
	mov	x0, x19
	stp	wzr, w6, [x29, 112]
	mov	x1, x24
	bl	_khepri_mpt__keccak_256
	cbnz	w25, L917
	ldr	q31, [x21]
	ldr	q30, [x28]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L902
	ldr	q31, [x21, 16]
	ldr	q30, [x28, 16]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L902
	.p2align 5,,15
L917:
	add	x27, x27, 1
	add	x26, x26, 536
	cmp	w23, w25
	bne	L920
	b	L921
	.p2align 2,,3
L902:
	mov	w2, 0
	mov	w1, 3
L900:
	mov	x0, 0
	bfi	x0, x2, 0, 8
	bfi	x0, x1, 8, 8
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	add	sp, sp, 688
LCFI146:
	ret
L935:
LCFI147:
	adrp	x0, lC6@PAGE
	adrp	x1, lC7@PAGE
	add	x19, x29, 120
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC7@PAGEOFF;
	mov	x8, x19
	bl	_khepri_mpt__keccak_256
	ldr	q31, [x21]
	ldr	q30, [x29, 120]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L897
	ldr	q31, [x21, 16]
	ldr	q30, [x29, 136]
	eor	v31.16b, v31.16b, v30.16b
	umaxp	v31.4s, v31.4s, v31.4s
	fmov	x0, d31
	cbnz	x0, L897
L921:
	mov	w1, 0
	mov	w2, 1
	b	L900
L897:
	mov	w1, 3
	mov	w2, 0
	b	L900
L914:
	ldr	w0, [x22, 1536]
	cmp	w0, 64
	bgt	L913
	cmp	x2, 531
	beq	L913
	ldrb	w1, [x26, x2]
	add	x0, x19, x2
	add	x2, x2, 1
	strb	w1, [x0, 1]
	cmp	x2, x5
	bne	L914
	b	L915
	.p2align 2,,3
L913:
	adrp	x0, lC8@PAGE
	mov	w1, 1479
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L937:
	adrp	x0, lC8@PAGE
	mov	w1, 1448
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L940:
	adrp	x0, lC8@PAGE
	mov	w1, 1469
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L939:
	ldr	w0, [x22, 1536]
	cmp	w0, 64
	ble	L909
	adrp	x0, lC8@PAGE
	mov	w1, 1469
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L938:
	adrp	x0, lC8@PAGE
	mov	w1, 1468
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L936:
	adrp	x0, lC8@PAGE
	mov	w1, 1440
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L934:
	adrp	x0, lC8@PAGE
	mov	w1, 1423
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE43:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__create_snapshot
_khepri_mpt__create_snapshot:
LFB44:
	adrp	x3, _khepri_mpt__snapshot_store@PAGE
	stp	x29, x30, [sp, -16]!
LCFI148:
	mov	x1, 0
	mov	x29, sp
LCFI149:
	mov	w4, w0
	add	x3, x3, _khepri_mpt__snapshot_store@PAGEOFF;
	b	L944
	.p2align 2,,3
L958:
	add	x1, x1, 1
	cmp	x1, 100
	beq	L956
L944:
	add	x2, x1, x1, lsl 1
	add	x2, x3, x2, lsl 2
	ldrb	w2, [x2, 8]
	cmp	w2, 1
	bhi	L957
	cbnz	w2, L958
	cmp	w4, 255
	bhi	L946
	cbz	w4, L945
	adrp	x5, _khepri_mpt__tries@PAGE
	sbfiz	x0, x4, 1, 32
	add	x0, x0, w4, sxtw
	add	x5, x5, _khepri_mpt__tries@PAGEOFF;
	lsl	x6, x0, 2
	add	x0, x5, x0, lsl 2
	ldrb	w2, [x0, 8]
	cmp	w2, 1
	bhi	L946
	cbnz	w2, L959
	mov	w4, 0
L945:
	mov	x0, 0
	bfi	x0, x4, 0, 32
	bfi	x0, x2, 32, 8
	ldp	x29, x30, [sp], 16
LCFI150:
	ret
L956:
LCFI151:
	mov	w4, 0
	mov	x0, 0
	bfi	x0, x4, 0, 32
	mov	w2, 0
	bfi	x0, x2, 32, 8
	ldp	x29, x30, [sp], 16
LCFI152:
	ret
L959:
LCFI153:
	sbfiz	x0, x1, 1, 32
	add	x0, x0, w1, sxtw
	ldr	w6, [x5, x6]
	add	x5, x3, x0, lsl 2
	str	w4, [x3, x0, lsl 2]
	mov	w4, w1
	mov	x0, 0
	mov	w2, 1
	bfi	x0, x4, 0, 32
	mov	w7, 1
	bfi	x0, x2, 32, 8
	str	w6, [x5, 4]
	strb	w7, [x5, 8]
	ldp	x29, x30, [sp], 16
LCFI154:
	ret
L957:
LCFI155:
	adrp	x0, lC8@PAGE
	mov	w1, 92
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L946:
	adrp	x0, lC8@PAGE
	mov	w1, 1521
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE44:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__restore_snapshot
_khepri_mpt__restore_snapshot:
LFB45:
	stp	x29, x30, [sp, -16]!
LCFI156:
	mov	x29, sp
LCFI157:
	tbnz	w1, #31, L970
	cmp	w1, 99
	bgt	L966
	adrp	x3, _khepri_mpt__snapshot_store@PAGE
	sbfiz	x2, x1, 1, 32
	add	x1, x2, w1, sxtw
	add	x3, x3, _khepri_mpt__snapshot_store@PAGEOFF;
	lsl	x4, x1, 2
	add	x1, x3, x1, lsl 2
	ldrb	w2, [x1, 8]
	cmp	w2, 1
	bhi	L971
	cbz	w2, L967
	ldr	w4, [x3, x4]
	orr	w2, w4, w0
	cmp	w2, 255
	bhi	L972
	mov	w3, 2
	mov	w2, 0
	cmp	w4, w0
	bne	L962
	ldr	w1, [x1, 4]
	tbnz	w1, #31, L973
	adrp	x0, _khepri_mpt__tries@PAGE
	sbfiz	x2, x4, 1, 32
	add	x4, x2, w4, sxtw
	add	x0, x0, _khepri_mpt__tries@PAGEOFF;
	mov	w3, 0
	mov	w2, 1
	str	w1, [x0, x4, lsl 2]
	b	L962
	.p2align 2,,3
L966:
	mov	w3, 2
	mov	w2, 0
L962:
	mov	x0, 0
	bfi	x0, x2, 0, 8
	bfi	x0, x3, 8, 8
	ldp	x29, x30, [sp], 16
LCFI158:
	ret
	.p2align 2,,3
L967:
LCFI159:
	mov	x0, 0
	mov	w3, 2
	bfi	x0, x2, 0, 8
	bfi	x0, x3, 8, 8
	ldp	x29, x30, [sp], 16
LCFI160:
	ret
L970:
LCFI161:
	adrp	x0, lC8@PAGE
	mov	w1, 1548
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L971:
	adrp	x0, lC8@PAGE
	mov	w1, 1549
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L972:
	adrp	x0, lC8@PAGE
	mov	w1, 1555
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L973:
	adrp	x0, lC8@PAGE
	mov	w1, 1560
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE45:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__discard_snapshot
_khepri_mpt__discard_snapshot:
LFB46:
	stp	x29, x30, [sp, -16]!
LCFI162:
	mov	x29, sp
LCFI163:
	tbnz	w1, #31, L983
	cmp	w1, 99
	bgt	L974
	adrp	x2, _khepri_mpt__snapshot_store@PAGE
	sbfiz	x3, x1, 1, 32
	add	x1, x3, w1, sxtw
	add	x2, x2, _khepri_mpt__snapshot_store@PAGEOFF;
	lsl	x4, x1, 2
	add	x1, x2, x1, lsl 2
	ldrb	w3, [x1, 8]
	cmp	w3, 1
	bhi	L984
	cbz	w3, L974
	ldr	w2, [x2, x4]
	orr	w3, w2, w0
	cmp	w3, 255
	bhi	L985
	cmp	w2, w0
	beq	L986
L974:
	ldp	x29, x30, [sp], 16
LCFI164:
	ret
	.p2align 2,,3
L986:
LCFI165:
	strb	wzr, [x1, 8]
	b	L974
L983:
	adrp	x0, lC8@PAGE
	mov	w1, 1569
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L984:
	adrp	x0, lC8@PAGE
	mov	w1, 1570
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L985:
	adrp	x0, lC8@PAGE
	mov	w1, 1571
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE46:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__begin_iteration
_khepri_mpt__begin_iteration:
LFB47:
	stp	x29, x30, [sp, -32]!
LCFI166:
	mov	x29, sp
LCFI167:
	stp	x19, x20, [sp, 16]
LCFI168:
	cmp	w0, 255
	bhi	L1001
	mov	x19, x1
	mov	w20, w0
	bl	_khepri_mpt__is_empty
	movi	v31.4s, 0
	mov	x2, x19
	add	x3, x19, 516
	str	w20, [x2], 4
	.p2align 5,,15
L989:
	str	q31, [x2], 16
	cmp	x2, x3
	bne	L989
	movi	v31.4s, 0
	add	x1, x19, 520
	str	wzr, [x19, 516]
	str	wzr, [x19, 584]
	cmp	w20, 0
	eor	w2, w0, 1
	cset	w3, ne
	stp	q31, q31, [x1]
	stp	q31, q31, [x1, 32]
	strb	w0, [x19, 588]
	tst	w3, w2
	bne	L1002
L987:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI169:
	ret
L1002:
LCFI170:
	adrp	x0, _khepri_mpt__tries@PAGE
	sbfiz	x1, x20, 1, 32
	add	x20, x1, w20, sxtw
	add	x0, x0, _khepri_mpt__tries@PAGEOFF;
	lsl	x2, x20, 2
	add	x20, x0, x20, lsl 2
	ldrb	w1, [x20, 8]
	cmp	w1, 1
	bhi	L1003
	cbz	w1, L987
	ldr	w0, [x0, x2]
	tbnz	w0, #31, L1004
	mov	w1, 16959
	movk	w1, 0xf, lsl 16
	cmp	w0, w1
	bgt	L987
	mov	w1, 1
	stp	w0, wzr, [x19, 4]
	str	w1, [x19, 516]
	b	L987
L1004:
	adrp	x0, lC8@PAGE
	mov	w1, 1600
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1001:
	adrp	x0, lC8@PAGE
	mov	w1, 1586
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1003:
	adrp	x0, lC8@PAGE
	mov	w1, 1599
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE47:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt__next
_khepri_mpt__next:
LFB48:
	sub	sp, sp, #800
LCFI171:
	movi	v31.4s, 0
	stp	x29, x30, [sp]
LCFI172:
	mov	x29, sp
LCFI173:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI174:
	str	x2, [x29, 112]
	stp	q31, q31, [x1]
	stp	q31, q31, [x1, 32]
	str	wzr, [x1, 64]
	ldr	w19, [x0, 516]
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	str	wzr, [x2, 32]
	tbnz	w19, #31, L1007
	mov	x24, x0
	ldrb	w0, [x0, 588]
	cmp	w0, 1
	bhi	L1007
	cmp	w19, 0
	ccmp	w0, 0, 0, ne
	cset	w0, ne
	str	w0, [x29, 108]
	bne	L1062
	adrp	x7, _khepri_mpt__node_store@PAGE
	mov	w6, 16959
	mov	x28, x1
	movk	w6, 0xf, lsl 16
	add	x22, x7, _khepri_mpt__node_store@PAGEOFF;
	.p2align 5,,15
L1008:
	cmp	w19, 64
	ccmp	w19, 0, 4, le
	cset	w23, eq
	beq	L1063
	sub	w19, w19, #1
	sbfiz	x25, x19, 3, 32
	add	x27, x24, x25
	ldr	w20, [x27, 4]
	tbnz	w20, #31, L1102
	cmp	w20, w6
	bgt	L1018
	sxtw	x26, w20
	sbfiz	x3, x20, 2, 32
	add	x21, x3, x26
	add	x21, x26, x21, lsl 2
	add	x21, x26, x21, lsl 3
	add	x21, x22, x21, lsl 2
	ldrb	w0, [x21, 672]
	cmp	w0, 1
	bhi	L1103
	str	x3, [x29, 120]
	cbz	w0, L1018
	add	x5, x29, 128
	mov	x1, x21
	mov	x0, x5
	mov	x2, 672
	bl	_memcpy
	mov	w1, 676
	mov	w6, 16959
	ldr	x3, [x29, 120]
	mov	x5, x0
	movk	w6, 0xf, lsl 16
	umull	x20, w20, w1
	ldr	w0, [x21, 68]
	ldrb	w1, [x22, x20]
	cmp	w1, 3
	bhi	L1104
	cmp	w1, 1
	beq	L1016
	cmp	w1, 2
	beq	L1017
	cbz	w1, L1018
	ldr	w1, [x27, 8]
	tbnz	w1, #31, L1105
	add	x0, x3, x26
	add	x0, x26, x0, lsl 2
	add	x0, x26, x0, lsl 3
	add	x0, x22, x0, lsl 2
	ldr	w0, [x0, 104]
	tbnz	w0, #31, L1106
	cmp	w1, 0
	ccmp	w0, 0, 4, eq
	beq	L1052
	add	x0, x24, 520
	ldr	q31, [x29, 200]
	add	x25, x24, x25
	mov	w2, 1
	ldp	q27, q26, [x0]
	ldp	q29, q28, [x0, 32]
	ldr	q30, [x29, 216]
	ldr	w3, [x0, 64]
	stp	q27, q26, [x28]
	ldr	w0, [x29, 232]
	stp	q29, q28, [x28, 32]
	ldr	x1, [x29, 112]
	str	w3, [x28, 64]
	stp	q31, q30, [x1]
	str	w0, [x1, 32]
	ldr	w0, [x24, 516]
	str	w2, [x25, 8]
	tbz	w0, #31, L1009
L1053:
	adrp	x0, lC8@PAGE
	mov	w1, 1629
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
	.p2align 2,,3
L1016:
	add	x1, x24, 520
	ldr	w2, [x24, 584]
	ldp	q29, q28, [x1]
	ldp	q31, q30, [x1, 32]
	str	w2, [x28, 64]
	stp	q29, q28, [x28]
	stp	q31, q30, [x28, 32]
	cmp	w0, 0
	blt	L1107
	bne	L1108
L1022:
	ldr	q31, [x29, 200]
	ldr	q30, [x29, 216]
	ldr	x2, [x29, 112]
	ldr	w1, [x29, 232]
	ldr	w0, [x24, 516]
	stp	q31, q30, [x2]
	str	w1, [x2, 32]
	tbnz	w0, #31, L1109
	sub	w1, w0, #1
	cbz	w0, L1035
	mov	w0, 1
	str	w1, [x24, 516]
	tbnz	x0, 0, L1009
L1062:
	mov	w0, 1
	mov	w1, 1
	str	w1, [x29, 108]
	strb	w0, [x24, 588]
L1009:
	ldr	w0, [x29, 108]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	add	sp, sp, 800
LCFI175:
	ret
	.p2align 2,,3
L1017:
LCFI176:
	cmp	w0, 0
	blt	L1110
	beq	L1018
	ldr	w3, [x24, 584]
	uxtw	x0, w0
	tbnz	w3, #31, L1039
	mov	x2, 0
	b	L1047
	.p2align 2,,3
L1040:
	cmp	x2, 64
	beq	L1111
	add	x1, x5, x2
	ldrb	w1, [x1, 4]
	cmp	w1, 15
	bhi	L1112
	add	x4, x24, w3, sxtw
	add	x2, x2, 1
	strb	w1, [x4, 520]
	add	w3, w3, 1
	mov	w23, 1
	cmp	x2, x0
	beq	L1048
L1047:
	cmp	w3, 63
	ble	L1040
	.p2align 5,,15
L1041:
	add	x2, x2, 1
	cmp	x2, x0
	bne	L1041
	cbz	w23, L1018
L1048:
	str	w3, [x24, 584]
	.p2align 5,,15
L1018:
	str	w19, [x24, 516]
	b	L1008
	.p2align 2,,3
L1052:
	cbz	w1, L1064
	sub	w0, w1, #1
	cmp	w0, 15
	bgt	L1018
	sub	w0, w1, #2
	b	L1055
	.p2align 2,,3
L1108:
	uxtw	x0, w0
	tbnz	w2, #31, L1023
	mov	x3, 0
	b	L1031
	.p2align 2,,3
L1024:
	cmp	x3, 64
	beq	L1113
	add	x1, x5, x3
	ldrb	w1, [x1, 4]
	cmp	w1, 15
	bhi	L1114
	mov	w23, 1
	add	x3, x3, 1
	strb	w1, [x28, w2, sxtw]
	add	w2, w2, w23
	cmp	x0, x3
	beq	L1032
L1031:
	cmp	w2, 63
	ble	L1024
	.p2align 5,,15
L1025:
	add	x3, x3, 1
	cmp	x0, x3
	bne	L1025
	cbz	w23, L1022
L1032:
	str	w2, [x28, 64]
	b	L1022
	.p2align 2,,3
L1064:
	mov	w0, -1
L1055:
	add	w3, w0, 1
	add	x5, x29, 268
	sxtw	x1, w3
	b	L1061
	.p2align 2,,3
L1057:
	add	x1, x1, 1
	cmp	w3, 15
	beq	L1018
	add	w3, w3, 1
L1061:
	add	x2, x1, x1, lsl 5
	mov	w4, w0
	mov	w0, w3
	ldrb	w2, [x5, x2]
	cmp	w2, 1
	bhi	L1115
	cbz	w2, L1057
	ldr	w0, [x24, 584]
	tbnz	w0, #31, L1116
	cmp	w0, 63
	bgt	L1059
	add	w1, w0, 1
	add	x0, x24, w0, sxtw
	strb	w3, [x0, 520]
	str	w1, [x24, 584]
L1059:
	add	x25, x24, x25
	add	w4, w4, 3
	ldr	w19, [x24, 516]
	str	w4, [x25, 8]
	tbz	w19, #31, L1008
	b	L1053
L1063:
	ldr	w0, [x29, 108]
	tbz	x0, 0, L1062
	b	L1009
	.p2align 2,,3
L1102:
	adrp	x0, lC8@PAGE
	mov	w1, 1633
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1039:
	adrp	x0, lC8@PAGE
	mov	w1, 1667
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1023:
	adrp	x0, lC8@PAGE
	mov	w1, 1652
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1103:
	adrp	x0, lC8@PAGE
	mov	w1, 1635
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1114:
	cbz	w23, L1030
	str	w2, [x28, 64]
L1030:
	adrp	x0, lC8@PAGE
	mov	w1, 1653
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1111:
	cbz	w23, L1044
	str	w3, [x24, 584]
L1044:
	adrp	x0, lC8@PAGE
	mov	w1, 1668
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L1112:
	cbz	w23, L1046
	str	w3, [x24, 584]
L1046:
	adrp	x0, lC8@PAGE
	mov	w1, 1668
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1113:
	cbz	w23, L1028
	str	w2, [x28, 64]
L1028:
	adrp	x0, lC8@PAGE
	mov	w1, 1653
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L1104:
	adrp	x0, lC8@PAGE
	mov	w1, 1641
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1116:
	adrp	x0, lC8@PAGE
	mov	w1, 1697
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1107:
	adrp	x0, lC8@PAGE
	mov	w1, 1651
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1109:
	adrp	x0, lC8@PAGE
	mov	w1, 1661
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1007:
	adrp	x0, lC8@PAGE
	mov	w1, 1622
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1115:
	adrp	x0, lC8@PAGE
	mov	w1, 1695
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1106:
	adrp	x0, lC8@PAGE
	mov	w1, 1680
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1035:
	adrp	x0, lC8@PAGE
	mov	w1, 1661
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L1105:
	adrp	x0, lC8@PAGE
	mov	w1, 1677
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L1110:
	adrp	x0, lC8@PAGE
	mov	w1, 1666
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE48:
	.align	2
	.p2align 5,,15
	.globl _khepri_mpt___elabb
_khepri_mpt___elabb:
LFB0:
	stp	x29, x30, [sp, -48]!
LCFI177:
	mov	x29, sp
LCFI178:
	adrp	x3, _khepri_mpt__node_store@PAGE
	add	x3, x3, _khepri_mpt__node_store@PAGEOFF;
	stp	x21, x22, [sp, 32]
LCFI179:
	mov	x21, 16960
	movk	x21, 0xf, lsl 16
	adrp	x22, _khepri_mpt_types__empty_node@GOTPAGE
	ldr	x22, [x22, _khepri_mpt_types__empty_node@GOTPAGEOFF]
	stp	x19, x20, [sp, 16]
LCFI180:
	mov	x19, 0
	mov	x20, x3
	.p2align 5,,15
L1118:
	mov	x0, x3
	mov	x2, 672
	mov	x1, x22
	bl	_memcpy
	mov	x3, x0
	add	x0, x19, x19, lsl 2
	add	x3, x3, 676
	add	x0, x19, x0, lsl 2
	add	x0, x19, x0, lsl 3
	add	x19, x19, 1
	add	x0, x20, x0, lsl 2
	strb	wzr, [x0, 672]
	cmp	x19, x21
	bne	L1118
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI181:
	ret
LFE0:
	.data
	.align	4
_khepri_mpt__snapshot_store:
	.space 1200
	.zerofill __DATA,__bss,_khepri_mpt__node_store,676000000,4
	.data
	.align	2
_khepri_mpt__tries:
	.space 3072
	.globl _khepri_mpt__null_trie
	.const
	.align	2
_khepri_mpt__null_trie:
	.space 4
	.globl _khepri_mpt_E
	.data
	.align	1
_khepri_mpt_E:
	.space 2
	.const
	.align	3
lC10:
	.word	2147483647
	.word	0
	.literal16
	.align	4
lC12:
	.word	32
	.word	31
	.word	30
	.word	29
	.align	4
lC13:
	.word	0
	.word	1
	.word	2
	.word	3
	.align	4
lC14:
	.word	64
	.word	63
	.word	62
	.word	61
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
	.quad	LFB14-.
	.set L$set$2,LFE14-LFB14
	.quad L$set$2
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$3,LCFI0-LFB14
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
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
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
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI6-LCFI5
	.long L$set$9
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$10,LEFDE3-LASFDE3
	.long L$set$10
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB18-.
	.set L$set$11,LFE18-LFB18
	.quad L$set$11
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$12,LCFI7-LFB18
	.long L$set$12
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0xa
	.byte	0xde
	.byte	0xdd
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
	.quad	LFB19-.
	.set L$set$17,LFE19-LFB19
	.quad L$set$17
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$18,LCFI11-LFB19
	.long L$set$18
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0x93
	.uleb128 0x6
	.byte	0x94
	.uleb128 0x5
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0x97
	.uleb128 0x2
	.byte	0x98
	.uleb128 0x1
	.byte	0x4
	.set L$set$22,LCFI15-LCFI14
	.long L$set$22
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
	.set L$set$23,LCFI16-LCFI15
	.long L$set$23
	.byte	0xb
	.byte	0x4
	.set L$set$24,LCFI17-LCFI16
	.long L$set$24
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
	.set L$set$25,LCFI18-LCFI17
	.long L$set$25
	.byte	0xb
	.byte	0x4
	.set L$set$26,LCFI19-LCFI18
	.long L$set$26
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
	.set L$set$27,LCFI20-LCFI19
	.long L$set$27
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$28,LEFDE7-LASFDE7
	.long L$set$28
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB16-.
	.set L$set$29,LFE16-LFB16
	.quad L$set$29
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$30,LCFI21-LFB16
	.long L$set$30
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$31,LCFI22-LCFI21
	.long L$set$31
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$32,LCFI23-LCFI22
	.long L$set$32
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$33,LCFI24-LCFI23
	.long L$set$33
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
	.set L$set$34,LCFI25-LCFI24
	.long L$set$34
	.byte	0xb
	.byte	0x4
	.set L$set$35,LCFI26-LCFI25
	.long L$set$35
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
	.set L$set$36,LCFI27-LCFI26
	.long L$set$36
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$37,LEFDE9-LASFDE9
	.long L$set$37
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB2-.
	.set L$set$38,LFE2-LFB2
	.quad L$set$38
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$39,LEFDE11-LASFDE11
	.long L$set$39
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB3-.
	.set L$set$40,LFE3-LFB3
	.quad L$set$40
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$41,LEFDE13-LASFDE13
	.long L$set$41
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB4-.
	.set L$set$42,LFE4-LFB4
	.quad L$set$42
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$43,LEFDE15-LASFDE15
	.long L$set$43
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB5-.
	.set L$set$44,LFE5-LFB5
	.quad L$set$44
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$45,LCFI28-LFB5
	.long L$set$45
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$46,LCFI29-LCFI28
	.long L$set$46
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$47,LEFDE17-LASFDE17
	.long L$set$47
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB6-.
	.set L$set$48,LFE6-LFB6
	.quad L$set$48
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$49,LEFDE19-LASFDE19
	.long L$set$49
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB82-.
	.set L$set$50,LFE82-LFB82
	.quad L$set$50
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$51,LEFDE21-LASFDE21
	.long L$set$51
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB8-.
	.set L$set$52,LFE8-LFB8
	.quad L$set$52
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$53,LEFDE23-LASFDE23
	.long L$set$53
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB9-.
	.set L$set$54,LFE9-LFB9
	.quad L$set$54
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$55,LCFI30-LFB9
	.long L$set$55
	.byte	0xe
	.uleb128 0x10
	.byte	0x4
	.set L$set$56,LCFI31-LCFI30
	.long L$set$56
	.byte	0xe
	.uleb128 0
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$57,LEFDE25-LASFDE25
	.long L$set$57
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB10-.
	.set L$set$58,LFE10-LFB10
	.quad L$set$58
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$59,LEFDE27-LASFDE27
	.long L$set$59
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB17-.
	.set L$set$60,LFE17-LFB17
	.quad L$set$60
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$61,LCFI32-LFB17
	.long L$set$61
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$62,LCFI33-LCFI32
	.long L$set$62
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$63,LCFI34-LCFI33
	.long L$set$63
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x4
	.set L$set$64,LCFI35-LCFI34
	.long L$set$64
	.byte	0x95
	.uleb128 0xa
	.byte	0x96
	.uleb128 0x9
	.byte	0x97
	.uleb128 0x8
	.byte	0x4
	.set L$set$65,LCFI36-LCFI35
	.long L$set$65
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
	.set L$set$66,LCFI37-LCFI36
	.long L$set$66
	.byte	0xb
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$67,LEFDE29-LASFDE29
	.long L$set$67
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB21-.
	.set L$set$68,LFE21-LFB21
	.quad L$set$68
	.uleb128 0x8
	.quad	LLSDA21-.
	.byte	0x4
	.set L$set$69,LCFI38-LFB21
	.long L$set$69
	.byte	0xe
	.uleb128 0x320
	.byte	0x4
	.set L$set$70,LCFI39-LCFI38
	.long L$set$70
	.byte	0x9d
	.uleb128 0x64
	.byte	0x9e
	.uleb128 0x63
	.byte	0x4
	.set L$set$71,LCFI40-LCFI39
	.long L$set$71
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$72,LCFI41-LCFI40
	.long L$set$72
	.byte	0x93
	.uleb128 0x62
	.byte	0x94
	.uleb128 0x61
	.byte	0x95
	.uleb128 0x60
	.byte	0x96
	.uleb128 0x5f
	.byte	0x4
	.set L$set$73,LCFI42-LCFI41
	.long L$set$73
	.byte	0x97
	.uleb128 0x5e
	.byte	0x98
	.uleb128 0x5d
	.byte	0x99
	.uleb128 0x5c
	.byte	0x9a
	.uleb128 0x5b
	.byte	0x4
	.set L$set$74,LCFI43-LCFI42
	.long L$set$74
	.byte	0x9b
	.uleb128 0x5a
	.byte	0x9c
	.uleb128 0x59
	.byte	0x4
	.set L$set$75,LCFI44-LCFI43
	.long L$set$75
	.byte	0xa
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
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$76,LCFI45-LCFI44
	.long L$set$76
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$77,LEFDE31-LASFDE31
	.long L$set$77
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB20-.
	.set L$set$78,LFE20-LFB20
	.quad L$set$78
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$79,LCFI46-LFB20
	.long L$set$79
	.byte	0xe
	.uleb128 0x250
	.byte	0x4
	.set L$set$80,LCFI47-LCFI46
	.long L$set$80
	.byte	0x9d
	.uleb128 0x4a
	.byte	0x9e
	.uleb128 0x49
	.byte	0x4
	.set L$set$81,LCFI48-LCFI47
	.long L$set$81
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$82,LCFI49-LCFI48
	.long L$set$82
	.byte	0x95
	.uleb128 0x46
	.byte	0x4
	.set L$set$83,LCFI50-LCFI49
	.long L$set$83
	.byte	0x93
	.uleb128 0x48
	.byte	0x94
	.uleb128 0x47
	.byte	0x4
	.set L$set$84,LCFI51-LCFI50
	.long L$set$84
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
	.set L$set$85,LCFI52-LCFI51
	.long L$set$85
	.byte	0xb
	.byte	0x4
	.set L$set$86,LCFI53-LCFI52
	.long L$set$86
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
	.set L$set$87,LCFI54-LCFI53
	.long L$set$87
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$88,LEFDE33-LASFDE33
	.long L$set$88
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB28-.
	.set L$set$89,LFE28-LFB28
	.quad L$set$89
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$90,LCFI55-LFB28
	.long L$set$90
	.byte	0xe
	.uleb128 0x300
	.byte	0x4
	.set L$set$91,LCFI56-LCFI55
	.long L$set$91
	.byte	0x9d
	.uleb128 0x60
	.byte	0x9e
	.uleb128 0x5f
	.byte	0x4
	.set L$set$92,LCFI57-LCFI56
	.long L$set$92
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$93,LCFI58-LCFI57
	.long L$set$93
	.byte	0x93
	.uleb128 0x5e
	.byte	0x94
	.uleb128 0x5d
	.byte	0x4
	.set L$set$94,LCFI59-LCFI58
	.long L$set$94
	.byte	0x95
	.uleb128 0x5c
	.byte	0x96
	.uleb128 0x5b
	.byte	0x4
	.set L$set$95,LCFI60-LCFI59
	.long L$set$95
	.byte	0x97
	.uleb128 0x5a
	.byte	0x98
	.uleb128 0x59
	.byte	0x4
	.set L$set$96,LCFI61-LCFI60
	.long L$set$96
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
	.set L$set$97,LCFI62-LCFI61
	.long L$set$97
	.byte	0xb
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$98,LEFDE35-LASFDE35
	.long L$set$98
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB30-.
	.set L$set$99,LFE30-LFB30
	.quad L$set$99
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$100,LCFI63-LFB30
	.long L$set$100
	.byte	0xe
	.uleb128 0x320
	.byte	0x4
	.set L$set$101,LCFI64-LCFI63
	.long L$set$101
	.byte	0x9d
	.uleb128 0x64
	.byte	0x9e
	.uleb128 0x63
	.byte	0x4
	.set L$set$102,LCFI65-LCFI64
	.long L$set$102
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$103,LCFI66-LCFI65
	.long L$set$103
	.byte	0x95
	.uleb128 0x60
	.byte	0x96
	.uleb128 0x5f
	.byte	0x4
	.set L$set$104,LCFI67-LCFI66
	.long L$set$104
	.byte	0x93
	.uleb128 0x62
	.byte	0x94
	.uleb128 0x61
	.byte	0x4
	.set L$set$105,LCFI68-LCFI67
	.long L$set$105
	.byte	0x97
	.uleb128 0x5e
	.byte	0x98
	.uleb128 0x5d
	.byte	0x4
	.set L$set$106,LCFI69-LCFI68
	.long L$set$106
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
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$107,LEFDE37-LASFDE37
	.long L$set$107
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB36-.
	.set L$set$108,LFE36-LFB36
	.quad L$set$108
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$109,LCFI70-LFB36
	.long L$set$109
	.byte	0xe
	.uleb128 0x360
	.byte	0x4
	.set L$set$110,LCFI71-LCFI70
	.long L$set$110
	.byte	0x9d
	.uleb128 0x6c
	.byte	0x9e
	.uleb128 0x6b
	.byte	0x4
	.set L$set$111,LCFI72-LCFI71
	.long L$set$111
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$112,LCFI73-LCFI72
	.long L$set$112
	.byte	0x93
	.uleb128 0x6a
	.byte	0x94
	.uleb128 0x69
	.byte	0x4
	.set L$set$113,LCFI74-LCFI73
	.long L$set$113
	.byte	0x95
	.uleb128 0x68
	.byte	0x96
	.uleb128 0x67
	.byte	0x97
	.uleb128 0x66
	.byte	0x98
	.uleb128 0x65
	.byte	0x99
	.uleb128 0x64
	.byte	0x9a
	.uleb128 0x63
	.byte	0x4
	.set L$set$114,LCFI75-LCFI74
	.long L$set$114
	.byte	0xa
	.byte	0xd9
	.byte	0xda
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
	.set L$set$115,LCFI76-LCFI75
	.long L$set$115
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$116,LEFDE39-LASFDE39
	.long L$set$116
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB24-.
	.set L$set$117,LFE24-LFB24
	.quad L$set$117
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$118,LCFI77-LFB24
	.long L$set$118
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$119,LCFI78-LCFI77
	.long L$set$119
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$120,LCFI79-LCFI78
	.long L$set$120
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$121,LCFI80-LCFI79
	.long L$set$121
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$122,LCFI81-LCFI80
	.long L$set$122
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
	.set L$set$123,LCFI82-LCFI81
	.long L$set$123
	.byte	0xb
	.byte	0x4
	.set L$set$124,LCFI83-LCFI82
	.long L$set$124
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
	.set L$set$125,LCFI84-LCFI83
	.long L$set$125
	.byte	0xb
	.byte	0x4
	.set L$set$126,LCFI85-LCFI84
	.long L$set$126
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
	.set L$set$127,LCFI86-LCFI85
	.long L$set$127
	.byte	0xb
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$128,LEFDE41-LASFDE41
	.long L$set$128
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB25-.
	.set L$set$129,LFE25-LFB25
	.quad L$set$129
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$130,LCFI87-LFB25
	.long L$set$130
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$131,LCFI88-LCFI87
	.long L$set$131
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$132,LEFDE43-LASFDE43
	.long L$set$132
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB26-.
	.set L$set$133,LFE26-LFB26
	.quad L$set$133
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$134,LCFI89-LFB26
	.long L$set$134
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$135,LCFI90-LCFI89
	.long L$set$135
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$136,LEFDE45-LASFDE45
	.long L$set$136
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB27-.
	.set L$set$137,LFE27-LFB27
	.quad L$set$137
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$138,LCFI91-LFB27
	.long L$set$138
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$139,LCFI92-LCFI91
	.long L$set$139
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$140,LEFDE47-LASFDE47
	.long L$set$140
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB32-.
	.set L$set$141,LFE32-LFB32
	.quad L$set$141
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$142,LCFI93-LFB32
	.long L$set$142
	.byte	0xe
	.uleb128 0xc50
	.byte	0x4
	.set L$set$143,LCFI94-LCFI93
	.long L$set$143
	.byte	0x9d
	.uleb128 0x18a
	.byte	0x9e
	.uleb128 0x189
	.byte	0x4
	.set L$set$144,LCFI95-LCFI94
	.long L$set$144
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$145,LCFI96-LCFI95
	.long L$set$145
	.byte	0x93
	.uleb128 0x188
	.byte	0x94
	.uleb128 0x187
	.byte	0x95
	.uleb128 0x186
	.byte	0x96
	.uleb128 0x185
	.byte	0x97
	.uleb128 0x184
	.byte	0x98
	.uleb128 0x183
	.byte	0x99
	.uleb128 0x182
	.byte	0x9a
	.uleb128 0x181
	.byte	0x9b
	.uleb128 0x180
	.byte	0x9c
	.uleb128 0x17f
	.byte	0x4
	.set L$set$146,LCFI97-LCFI96
	.long L$set$146
	.byte	0xa
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$147,LCFI98-LCFI97
	.long L$set$147
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
	.byte	0xdd
	.byte	0xde
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$148,LCFI99-LCFI98
	.long L$set$148
	.byte	0xb
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$149,LEFDE49-LASFDE49
	.long L$set$149
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB33-.
	.set L$set$150,LFE33-LFB33
	.quad L$set$150
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$151,LCFI100-LFB33
	.long L$set$151
	.byte	0xe
	.uleb128 0x380
	.byte	0x4
	.set L$set$152,LCFI101-LCFI100
	.long L$set$152
	.byte	0x9d
	.uleb128 0x70
	.byte	0x9e
	.uleb128 0x6f
	.byte	0x4
	.set L$set$153,LCFI102-LCFI101
	.long L$set$153
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$154,LCFI103-LCFI102
	.long L$set$154
	.byte	0x93
	.uleb128 0x6e
	.byte	0x94
	.uleb128 0x6d
	.byte	0x95
	.uleb128 0x6c
	.byte	0x96
	.uleb128 0x6b
	.byte	0x97
	.uleb128 0x6a
	.byte	0x98
	.uleb128 0x69
	.byte	0x4
	.set L$set$155,LCFI104-LCFI103
	.long L$set$155
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
	.set L$set$156,LCFI105-LCFI104
	.long L$set$156
	.byte	0xb
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$157,LEFDE51-LASFDE51
	.long L$set$157
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB37-.
	.set L$set$158,LFE37-LFB37
	.quad L$set$158
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$159,LCFI106-LFB37
	.long L$set$159
	.byte	0xe
	.uleb128 0xd0
	.byte	0x9d
	.uleb128 0x1a
	.byte	0x9e
	.uleb128 0x19
	.byte	0x4
	.set L$set$160,LCFI107-LCFI106
	.long L$set$160
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$161,LCFI108-LCFI107
	.long L$set$161
	.byte	0x93
	.uleb128 0x18
	.byte	0x94
	.uleb128 0x17
	.byte	0x95
	.uleb128 0x16
	.byte	0x96
	.uleb128 0x15
	.byte	0x4
	.set L$set$162,LCFI109-LCFI108
	.long L$set$162
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
	.set L$set$163,LCFI110-LCFI109
	.long L$set$163
	.byte	0xb
	.byte	0x4
	.set L$set$164,LCFI111-LCFI110
	.long L$set$164
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
	.set L$set$165,LCFI112-LCFI111
	.long L$set$165
	.byte	0xb
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$166,LEFDE53-LASFDE53
	.long L$set$166
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB38-.
	.set L$set$167,LFE38-LFB38
	.quad L$set$167
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$168,LCFI113-LFB38
	.long L$set$168
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$169,LCFI114-LCFI113
	.long L$set$169
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$170,LCFI115-LCFI114
	.long L$set$170
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$171,LCFI116-LCFI115
	.long L$set$171
	.byte	0xb
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$172,LEFDE55-LASFDE55
	.long L$set$172
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB39-.
	.set L$set$173,LFE39-LFB39
	.quad L$set$173
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$174,LCFI117-LFB39
	.long L$set$174
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$175,LCFI118-LCFI117
	.long L$set$175
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$176,LCFI119-LCFI118
	.long L$set$176
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$177,LCFI120-LCFI119
	.long L$set$177
	.byte	0xb
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$178,LEFDE57-LASFDE57
	.long L$set$178
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB40-.
	.set L$set$179,LFE40-LFB40
	.quad L$set$179
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$180,LCFI121-LFB40
	.long L$set$180
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$181,LCFI122-LCFI121
	.long L$set$181
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$182,LCFI123-LCFI122
	.long L$set$182
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$183,LCFI124-LCFI123
	.long L$set$183
	.byte	0xb
	.byte	0x4
	.set L$set$184,LCFI125-LCFI124
	.long L$set$184
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$185,LCFI126-LCFI125
	.long L$set$185
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$186,LEFDE59-LASFDE59
	.long L$set$186
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB41-.
	.set L$set$187,LFE41-LFB41
	.quad L$set$187
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$188,LCFI127-LFB41
	.long L$set$188
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$189,LCFI128-LCFI127
	.long L$set$189
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$190,LCFI129-LCFI128
	.long L$set$190
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$191,LCFI130-LCFI129
	.long L$set$191
	.byte	0xb
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$192,LEFDE61-LASFDE61
	.long L$set$192
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB42-.
	.set L$set$193,LFE42-LFB42
	.quad L$set$193
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$194,LCFI131-LFB42
	.long L$set$194
	.byte	0xe
	.uleb128 0x5c0
	.byte	0x4
	.set L$set$195,LCFI132-LCFI131
	.long L$set$195
	.byte	0x9d
	.uleb128 0xb8
	.byte	0x9e
	.uleb128 0xb7
	.byte	0x4
	.set L$set$196,LCFI133-LCFI132
	.long L$set$196
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$197,LCFI134-LCFI133
	.long L$set$197
	.byte	0x93
	.uleb128 0xb6
	.byte	0x94
	.uleb128 0xb5
	.byte	0x4
	.set L$set$198,LCFI135-LCFI134
	.long L$set$198
	.byte	0x95
	.uleb128 0xb4
	.byte	0x96
	.uleb128 0xb3
	.byte	0x4
	.set L$set$199,LCFI136-LCFI135
	.long L$set$199
	.byte	0x97
	.uleb128 0xb2
	.byte	0x98
	.uleb128 0xb1
	.byte	0x4
	.set L$set$200,LCFI137-LCFI136
	.long L$set$200
	.byte	0x99
	.uleb128 0xb0
	.byte	0x9a
	.uleb128 0xaf
	.byte	0x9b
	.uleb128 0xae
	.byte	0x4
	.set L$set$201,LCFI138-LCFI137
	.long L$set$201
	.byte	0xa
	.byte	0xdb
	.byte	0xd9
	.byte	0xda
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
	.set L$set$202,LCFI139-LCFI138
	.long L$set$202
	.byte	0xb
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$203,LEFDE63-LASFDE63
	.long L$set$203
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB43-.
	.set L$set$204,LFE43-LFB43
	.quad L$set$204
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$205,LCFI140-LFB43
	.long L$set$205
	.byte	0xe
	.uleb128 0x2b0
	.byte	0x4
	.set L$set$206,LCFI141-LCFI140
	.long L$set$206
	.byte	0x9d
	.uleb128 0x56
	.byte	0x9e
	.uleb128 0x55
	.byte	0x4
	.set L$set$207,LCFI142-LCFI141
	.long L$set$207
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$208,LCFI143-LCFI142
	.long L$set$208
	.byte	0x93
	.uleb128 0x54
	.byte	0x94
	.uleb128 0x53
	.byte	0x4
	.set L$set$209,LCFI144-LCFI143
	.long L$set$209
	.byte	0x95
	.uleb128 0x52
	.byte	0x96
	.uleb128 0x51
	.byte	0x4
	.set L$set$210,LCFI145-LCFI144
	.long L$set$210
	.byte	0x97
	.uleb128 0x50
	.byte	0x98
	.uleb128 0x4f
	.byte	0x99
	.uleb128 0x4e
	.byte	0x9a
	.uleb128 0x4d
	.byte	0x9b
	.uleb128 0x4c
	.byte	0x9c
	.uleb128 0x4b
	.byte	0x4
	.set L$set$211,LCFI146-LCFI145
	.long L$set$211
	.byte	0xa
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
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$212,LCFI147-LCFI146
	.long L$set$212
	.byte	0xb
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$213,LEFDE65-LASFDE65
	.long L$set$213
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB44-.
	.set L$set$214,LFE44-LFB44
	.quad L$set$214
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$215,LCFI148-LFB44
	.long L$set$215
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$216,LCFI149-LCFI148
	.long L$set$216
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$217,LCFI150-LCFI149
	.long L$set$217
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$218,LCFI151-LCFI150
	.long L$set$218
	.byte	0xb
	.byte	0x4
	.set L$set$219,LCFI152-LCFI151
	.long L$set$219
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$220,LCFI153-LCFI152
	.long L$set$220
	.byte	0xb
	.byte	0x4
	.set L$set$221,LCFI154-LCFI153
	.long L$set$221
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$222,LCFI155-LCFI154
	.long L$set$222
	.byte	0xb
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$223,LEFDE67-LASFDE67
	.long L$set$223
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB45-.
	.set L$set$224,LFE45-LFB45
	.quad L$set$224
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$225,LCFI156-LFB45
	.long L$set$225
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$226,LCFI157-LCFI156
	.long L$set$226
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$227,LCFI158-LCFI157
	.long L$set$227
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$228,LCFI159-LCFI158
	.long L$set$228
	.byte	0xb
	.byte	0x4
	.set L$set$229,LCFI160-LCFI159
	.long L$set$229
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$230,LCFI161-LCFI160
	.long L$set$230
	.byte	0xb
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$231,LEFDE69-LASFDE69
	.long L$set$231
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB46-.
	.set L$set$232,LFE46-LFB46
	.quad L$set$232
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$233,LCFI162-LFB46
	.long L$set$233
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$234,LCFI163-LCFI162
	.long L$set$234
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$235,LCFI164-LCFI163
	.long L$set$235
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$236,LCFI165-LCFI164
	.long L$set$236
	.byte	0xb
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$237,LEFDE71-LASFDE71
	.long L$set$237
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB47-.
	.set L$set$238,LFE47-LFB47
	.quad L$set$238
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$239,LCFI166-LFB47
	.long L$set$239
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$240,LCFI167-LCFI166
	.long L$set$240
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$241,LCFI168-LCFI167
	.long L$set$241
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$242,LCFI169-LCFI168
	.long L$set$242
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$243,LCFI170-LCFI169
	.long L$set$243
	.byte	0xb
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$244,LEFDE73-LASFDE73
	.long L$set$244
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB48-.
	.set L$set$245,LFE48-LFB48
	.quad L$set$245
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$246,LCFI171-LFB48
	.long L$set$246
	.byte	0xe
	.uleb128 0x320
	.byte	0x4
	.set L$set$247,LCFI172-LCFI171
	.long L$set$247
	.byte	0x9d
	.uleb128 0x64
	.byte	0x9e
	.uleb128 0x63
	.byte	0x4
	.set L$set$248,LCFI173-LCFI172
	.long L$set$248
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$249,LCFI174-LCFI173
	.long L$set$249
	.byte	0x93
	.uleb128 0x62
	.byte	0x94
	.uleb128 0x61
	.byte	0x95
	.uleb128 0x60
	.byte	0x96
	.uleb128 0x5f
	.byte	0x97
	.uleb128 0x5e
	.byte	0x98
	.uleb128 0x5d
	.byte	0x99
	.uleb128 0x5c
	.byte	0x9a
	.uleb128 0x5b
	.byte	0x9b
	.uleb128 0x5a
	.byte	0x9c
	.uleb128 0x59
	.byte	0x4
	.set L$set$250,LCFI175-LCFI174
	.long L$set$250
	.byte	0xa
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
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$251,LCFI176-LCFI175
	.long L$set$251
	.byte	0xb
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$252,LEFDE75-LASFDE75
	.long L$set$252
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB0-.
	.set L$set$253,LFE0-LFB0
	.quad L$set$253
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$254,LCFI177-LFB0
	.long L$set$254
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$255,LCFI178-LCFI177
	.long L$set$255
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$256,LCFI179-LCFI178
	.long L$set$256
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$257,LCFI180-LCFI179
	.long L$set$257
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$258,LCFI181-LCFI180
	.long L$set$258
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
LEFDE75:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
