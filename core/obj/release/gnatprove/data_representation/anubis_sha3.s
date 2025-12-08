	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC8:
	.ascii "failed precondition from anubis_sha3.ads:150"
	.align	3
lC9:
	.ascii "anubis_sha3.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__bytes_to_lane
_anubis_sha3__bytes_to_lane:
LFB2:
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	x29, sp
LCFI1:
	mov	x4, x0
	ldp	w5, w6, [x1]
	sxtw	x0, w5
	add	x0, x0, 7
	cmp	x0, w6, sxtw
	bne	L8
	mov	x1, 0
	mov	x0, 0
	.p2align 5,,15
L4:
	add	w2, w5, w1
	cmp	w6, w2
	blt	L9
	ldrb	w2, [x4, x1]
	lsl	w3, w1, 3
	add	x1, x1, 1
	lsl	x2, x2, x3
	orr	x0, x0, x2
	cmp	x1, 8
	bne	L4
	ldp	x29, x30, [sp], 16
LCFI2:
	ret
L9:
LCFI3:
	adrp	x0, lC9@PAGE
	mov	w1, 17
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L8:
	adrp	x0, lC8@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE2:
	.const
	.align	2
lC0:
	.word	1
	.word	44
	.text
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_sha3.ads:155"
	.align	3
lC11:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:35"
	.align	3
lC12:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:36"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__lane_to_bytes
_anubis_sha3__lane_to_bytes:
LFB3:
	stp	x29, x30, [sp, -64]!
LCFI4:
	mov	x29, sp
LCFI5:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI6:
	mov	x21, x0
	str	x23, [sp, 48]
LCFI7:
	ldp	w19, w20, [x2]
	sxtw	x23, w19
	add	x0, x23, 7
	cmp	x0, w20, sxtw
	bne	L22
	tbnz	w19, #31, L23
	tbnz	w20, #31, L24
	mov	x4, x1
	mov	x22, x2
	cmp	w19, w20
	mov	w1, 0
	cset	x2, le
	mov	x0, x4
	lsl	x2, x2, 3
	bl	_memset
	mov	x4, x0
	ldp	w0, w5, [x22]
	mov	x3, 0
	sxtw	x1, w0
	sub	x1, x1, x23
	add	x1, x4, x1
	.p2align 5,,15
L18:
	cmp	w19, w0
	bne	L25
	cmp	w20, w5
	bne	L26
	lsl	w2, w3, 3
	add	w4, w19, w3
	lsr	x2, x21, x2
	cmp	w4, w20
	bgt	L27
	strb	w2, [x1, x3]
	add	x3, x3, 1
	cmp	x3, 8
	bne	L18
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 64
LCFI8:
	ret
L27:
LCFI9:
	adrp	x0, lC9@PAGE
	mov	w1, 41
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L26:
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L25:
	adrp	x0, lC11@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L24:
	adrp	x0, lC9@PAGE
	mov	w1, 29
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L23:
	adrp	x0, lC9@PAGE
	mov	w1, 28
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L22:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE3:
	.const
	.align	2
lC1:
	.word	1
	.word	43
	.text
	.const
	.align	3
lC13:
	.ascii "anubis_sha3.ads"
	.space 1
	.align	3
lC14:
	.ascii "failed precondition from anubis_sha3.ads:128"
	.align	3
lC15:
	.ascii "failed precondition from anubis_sha3.ads:129"
	.align	3
lC16:
	.ascii "failed precondition from anubis_sha3.ads:130"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__absorb_block
_anubis_sha3__absorb_block:
LFB5:
	stp	x29, x30, [sp, -128]!
LCFI10:
	mov	x29, sp
LCFI11:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
LCFI12:
	mov	x25, x0
	stp	x27, x28, [sp, 80]
LCFI13:
	ldp	w4, w0, [x2]
	cmp	w3, 0
	ble	L48
	mov	x26, x1
	sub	w1, w3, #8
	cmp	w1, 192
	bhi	L31
	tst	x3, 7
	bne	L31
	sxtw	x22, w4
	mov	x20, x2
	sxtw	x1, w0
	add	x2, x22, 6
	cmp	x2, x1
	bge	L33
	add	x2, x22, 199
	cmp	x1, x2
	bgt	L33
	cmp	w4, w0
	bgt	L34
	sub	x1, x1, x22
	mov	x2, 2147483647
	add	x1, x1, 1
	cmp	x1, x2
	bhi	L49
	cmp	w3, w1
	bne	L34
	add	x1, x29, 120
	asr	w3, w3, 3
	mov	w27, -1
	mov	w23, 52429
	str	x1, [x29, 104]
	add	x1, x29, 112
	add	w21, w3, w27
	movk	w23, 0xcccc, lsl 16
	mov	w24, 2147483640
	str	x1, [x29, 96]
	b	L43
	.p2align 2,,3
L50:
	ldp	w4, w0, [x20]
L43:
	add	w27, w27, 1
	add	w2, w4, w27, lsl 3
	lsl	w3, w27, 3
	umull	x19, w27, w23
	add	w1, w2, 6
	lsr	x19, x19, 34
	add	w7, w19, w19, lsl 2
	sxtw	x28, w19
	sub	w19, w27, w7
	cmp	w1, w0
	bge	L42
	adds	w4, w4, w3
	bvs	L41
	cmp	w2, w24
	bgt	L41
	add	w4, w2, 7
	tbnz	w2, #31, L42
	sxtw	x3, w2
	stp	w2, w4, [x29, 112]
	sub	x2, x3, x22
	ldp	x1, x0, [x29, 96]
	ldr	x2, [x2, x26]
	str	x2, [x29, 120]
	bl	_anubis_sha3__bytes_to_lane
	sbfiz	x1, x19, 2, 32
	add	x19, x1, w19, sxtw
	add	x19, x19, x28
	ldr	x1, [x25, x19, lsl 3]
	eor	x0, x0, x1
	str	x0, [x25, x19, lsl 3]
	cmp	w27, w21
	bne	L50
	mov	x0, x25
	bl	_anubis_keccak__keccak_f
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 128
LCFI14:
	ret
L34:
LCFI15:
	adrp	x0, lC16@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L42:
	adrp	x0, lC9@PAGE
	mov	w1, 60
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L41:
	adrp	x0, lC9@PAGE
	mov	w1, 60
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L48:
	adrp	x0, lC13@PAGE
	mov	w1, 128
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L33:
	adrp	x0, lC15@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L31:
	adrp	x0, lC14@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L49:
	adrp	x0, lC13@PAGE
	mov	w1, 130
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE5:
	.const
	.align	3
lC17:
	.ascii "failed precondition from anubis_sha3.ads:140"
	.align	3
lC18:
	.ascii "failed precondition from anubis_sha3.ads:141"
	.align	3
lC19:
	.ascii "failed precondition from anubis_sha3.ads:142"
	.align	3
lC20:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:86"
	.align	3
lC21:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:87"
	.align	3
lC22:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:98"
	.align	3
lC23:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:99"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__squeeze_block
_anubis_sha3__squeeze_block:
LFB6:
	stp	x29, x30, [sp, -128]!
LCFI16:
	mov	x29, sp
LCFI17:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI18:
	mov	x22, x1
	stp	x23, x24, [sp, 48]
LCFI19:
	mov	x24, x0
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI20:
	ldp	w19, w26, [x2]
	sxtw	x21, w19
	sxtw	x0, w26
	add	x1, x21, 6
	cmp	x1, x0
	bge	L53
	add	x1, x21, 199
	cmp	x0, x1
	bgt	L53
	cmp	w3, 0
	ble	L79
	sub	w1, w3, #8
	cmp	w1, 192
	bhi	L56
	tst	x3, 7
	bne	L56
	cmp	w19, w26
	bgt	L57
	sub	x1, x0, x21
	mov	x27, x2
	add	x1, x1, 1
	mov	x2, 2147483647
	cmp	x1, x2
	bhi	L80
	cmp	w3, w1
	bne	L57
	asr	w20, w3, 3
	tbnz	w19, #31, L81
	tbnz	w26, #31, L82
	add	x0, x0, 1
	mov	w1, 0
	sub	x2, x0, x21
	mov	x0, x22
	bl	_memset
	ldr	w7, [x27]
	adrp	x0, lC3@PAGE
	mov	w25, 52429
	add	x0, x0, lC3@PAGEOFF;
	mov	x28, 0
	add	x23, x29, 120
	movk	w25, 0xcccc, lsl 16
	str	x0, [x29, 104]
	.p2align 5,,15
L74:
	cmp	w19, w7
	bne	L83
	ldr	w0, [x27, 4]
	cmp	w26, w0
	bne	L84
	umull	x0, w28, w25
	ldr	x2, [x29, 104]
	mov	x1, x23
	lsr	x0, x0, 34
	add	w3, w0, w0, lsl 2
	sxtw	x0, w0
	sub	w3, w28, w3
	sbfiz	x4, x3, 2, 32
	add	x3, x4, w3, sxtw
	add	x3, x3, x0
	ldr	x0, [x24, x3, lsl 3]
	bl	_anubis_sha3__lane_to_bytes
	ldp	w7, w2, [x27]
	lsl	w5, w28, 3
	mov	x4, -1
	sxtw	x6, w7
	add	x6, x6, w28, uxtw 3
	sub	x6, x6, x21
	add	x6, x6, 1
	add	x6, x22, x6
	.p2align 5,,15
L73:
	add	w3, w4, 1
	cmp	w19, w7
	bne	L85
	cmp	w26, w2
	bne	L86
	adds	w0, w19, w5
	bvs	L71
	adds	w3, w3, w0
	bvs	L71
	cmp	w26, w3
	ccmp	w19, w3, 0, ge
	bgt	L87
	add	x0, x23, x4
	ldrb	w0, [x0, 1]
	strb	w0, [x6, x4]
	add	x4, x4, 1
	cmp	x4, 7
	bne	L73
	add	x28, x28, 1
	cmp	x28, x20
	bne	L74
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 128
LCFI21:
	ret
L87:
LCFI22:
	adrp	x0, lC9@PAGE
	mov	w1, 100
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L71:
	adrp	x0, lC9@PAGE
	mov	w1, 100
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L57:
	adrp	x0, lC19@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC19@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L85:
	adrp	x0, lC22@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC22@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L86:
	adrp	x0, lC23@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC23@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L83:
	adrp	x0, lC20@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC20@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L84:
	adrp	x0, lC21@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC21@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L53:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L56:
	adrp	x0, lC18@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L79:
	adrp	x0, lC13@PAGE
	mov	w1, 141
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L80:
	adrp	x0, lC13@PAGE
	mov	w1, 142
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L81:
	adrp	x0, lC9@PAGE
	mov	w1, 78
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L82:
	adrp	x0, lC9@PAGE
	mov	w1, 79
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE6:
	.const
	.align	2
lC3:
	.word	0
	.word	7
	.text
	.const
	.align	3
lC24:
	.ascii "failed precondition from anubis_sha3.adb:115"
	.align	3
lC25:
	.ascii "failed precondition from anubis_sha3.adb:116"
	.align	3
lC26:
	.ascii "failed precondition from anubis_sha3.adb:117"
	.align	3
lC27:
	.ascii "failed precondition from anubis_sha3.adb:118"
	.align	3
lC28:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:129"
	.align	3
lC29:
	.ascii "Loop_Variant failed at anubis_sha3.adb:131"
	.align	3
lC30:
	.ascii "Loop_Variant failed at anubis_sha3.adb:180"
	.text
	.align	2
	.p2align 5,,15
_anubis_sha3__sha3_sponge:
LFB8:
	stp	x29, x30, [sp, -400]!
LCFI23:
	mov	x29, sp
LCFI24:
	mov	w9, 2147483647
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
LCFI25:
	mov	x23, x0
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI26:
	mov	x27, x1
	str	x3, [x29, 120]
	ldp	w8, w1, [x3]
	str	w5, [x29, 128]
	ldp	w26, w0, [x27]
	cmp	w1, w9
	beq	L179
	cmp	w0, w9
	beq	L180
	cmp	w8, w1
	bgt	L181
	sxtw	x21, w8
	subs	w8, w1, w8
	cset	w0, vs
	str	w0, [x29, 132]
	bvs	L182
	mov	w20, w4
	cmp	w8, w4
	bge	L183
	movi	v31.4s, 0
	add	x3, x29, 200
	add	x4, x29, 232
	str	xzr, [x29, 392]
	sxtw	x1, w1
	mov	x19, x2
	str	x3, [x29, 144]
	add	x2, x1, 1
	mov	x0, x19
	mov	w1, 0
	sub	x2, x2, x21
	stp	q31, q31, [x3]
	stp	q31, q31, [x4]
	add	x4, x29, 264
	add	x3, x29, 360
	stp	q31, q31, [x4]
	add	x4, x29, 296
	stp	q31, q31, [x4]
	add	x4, x29, 328
	stp	q31, q31, [x4]
	stp	q31, q31, [x3]
	bl	_memset
	ldp	w5, w9, [x27]
	cmp	w5, w9
	bgt	L96
	add	x0, x29, 168
	sxtw	x26, w26
	mov	w28, 0
	mov	w1, 0
	str	x0, [x29, 136]
	.p2align 5,,15
L117:
	sxtw	x22, w9
	mov	x0, 2147483647
	sub	x22, x22, w5, sxtw
	add	x22, x22, 1
	cmp	x22, x0
	bgt	L97
	mov	w0, w22
	cmp	w20, w22
	bgt	L173
	tbnz	w28, #31, L184
	sub	w2, w22, w20
	cmp	w2, w28
	blt	L101
	cmp	w28, w22
	bgt	L185
	sub	w22, w22, w28
	cbz	w1, L103
	cmp	w24, w22
	ble	L186
L103:
	mov	x0, sp
	adds	w24, w5, w28
	str	x0, [x29, 152]
	bvs	L105
	tbnz	w24, #31, L187
	adds	w12, w24, w20
	bvs	L109
	sub	w25, w12, #1
	cmp	w24, w25
	bgt	L188
	cmp	w9, w25
	ccmp	w5, w24, 0, ge
	bgt	L189
	sxtw	x1, w24
	mov	x0, 1
	sub	x2, x0, x1
	add	x2, x2, w25, sxtw
L112:
	add	x0, x2, 15
	sub	x1, x1, x26
	and	x0, x0, -16
	add	x1, x23, x1
	sub	sp, sp, x0
	mov	x0, sp
	bl	_memcpy
	ldp	x2, x0, [x29, 136]
	mov	w3, w20
	mov	x1, sp
	stp	w24, w25, [x29, 168]
	bl	_anubis_sha3__absorb_block
	adds	w10, w20, w28
	cset	w0, vs
	mov	w28, w10
	cbnz	w0, L190
	ldr	x1, [x29, 152]
	ldp	w5, w9, [x27]
	mov	sp, x1
	mov	w1, 1
	cmp	w9, w5
	bge	L160
L173:
	mov	x1, sp
	str	x1, [x29, 152]
	tbnz	w28, #31, L191
L120:
	subs	w24, w0, w28
	bvs	L192
	stp	w24, w5, [x29, 112]
	str	w9, [x29, 136]
	tbnz	w24, #31, L124
	sxtw	x25, w20
	mov	w1, 0
	add	x22, x25, 15
	mov	x2, x25
	and	x0, x22, -16
	sub	w27, w20, #1
	sub	sp, sp, x0
	mov	x0, sp
	str	x0, [x29, 104]
	bl	_memset
	ldr	x4, [x29, 104]
	ldr	w11, [x29, 112]
	cbz	w24, L119
	ldr	w5, [x29, 116]
	adds	w10, w5, w28
	bvs	L127
	ldr	w9, [x29, 136]
	uxtw	x12, w24
	mov	w1, w10
	mov	x0, 1
	sub	x2, sp, #1
	b	L132
	.p2align 2,,3
L134:
	cmp	w27, w0
	blt	L133
	adds	w1, w10, w0
	add	x0, x0, 1
	bvs	L127
L132:
	cmp	w5, w1
	ccmp	w9, w1, 1, le
	blt	L133
	sxtw	x1, w1
	sub	x1, x1, x26
	ldrb	w1, [x23, x1]
	strb	w1, [x2, x0]
	cmp	x0, x12
	bne	L134
	cmp	w27, w24
	bge	L119
	adrp	x0, lC9@PAGE
	mov	w1, 155
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L96:
	mov	x0, sp
	sxtw	x25, w20
	add	x22, x25, 15
	mov	x2, x25
	str	x0, [x29, 152]
	and	x0, x22, -16
	mov	w1, 0
	sub	sp, sp, x0
	sub	w27, w20, #1
	mov	x0, sp
	str	x0, [x29, 136]
	bl	_memset
	ldr	x4, [x29, 136]
	mov	w11, 0
L119:
	sub	x5, x25, #1
	mov	x1, x4
	ldrb	w0, [x29, 128]
	mov	w3, w20
	add	x2, x29, 176
	strb	w0, [x4, w11, sxtw]
	ldrb	w7, [x4, x5]
	ldr	x0, [x29, 144]
	orr	w7, w7, -128
	strb	w7, [x4, x5]
	stp	wzr, w27, [x29, 176]
	bl	_anubis_sha3__absorb_block
	ldr	x0, [x29, 152]
	mov	sp, x0
	ldr	x0, [x29, 120]
	ldp	w1, w0, [x0]
	cmp	w0, w1
	bge	L193
L135:
	and	x22, x22, -16
	mov	x21, sp
	ldr	x0, [x29, 144]
	sub	sp, sp, x22
	mov	w3, w20
	stp	wzr, w27, [x29, 184]
	mov	x1, sp
	add	x2, x29, 184
	bl	_anubis_sha3__squeeze_block
	ldr	x0, [x29, 120]
	mov	x22, sp
	ldp	w1, w0, [x0]
	cmp	w0, w1
	blt	L161
	sxtw	x0, w0
	mov	x2, 2147483647
	sub	x0, x0, w1, sxtw
	add	x0, x0, 1
	cmp	x0, x2
	bgt	L140
	cmp	w0, w20
	bgt	L140
	bic	w2, w0, w0, asr #31
	sxtw	x2, w2
	cmp	x2, x0
	beq	L138
	adrp	x0, lC9@PAGE
	mov	w1, 170
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Length_Check
	.p2align 2,,3
L188:
	sxtw	x1, w24
	mov	x2, 0
	b	L112
	.p2align 2,,3
L160:
	mov	w24, w22
	b	L117
L193:
	sxtw	x0, w0
	mov	x24, 2147483647
	sub	x0, x0, w1, sxtw
	add	x0, x0, 1
	cmp	x0, x24
	bgt	L194
	add	x1, x29, 192
	mov	w28, w0
	str	x1, [x29, 136]
	and	x1, x22, -16
	str	x1, [x29, 152]
	cmp	w0, w20
	ble	L135
	.p2align 5,,15
L137:
	ldr	x0, [x29, 152]
	mov	x22, sp
	cmp	w28, w20
	mov	w3, w20
	csel	w23, w28, w20, le
	sub	sp, sp, x0
	ldp	x2, x0, [x29, 136]
	mov	x1, sp
	mov	x26, sp
	stp	wzr, w27, [x29, 192]
	bl	_anubis_sha3__squeeze_block
	ldr	x0, [x29, 120]
	uxtw	x11, w23
	ldp	w10, w9, [x0]
	ldr	w0, [x29, 132]
	adds	w5, w10, w0
	cset	w0, vs
	bvs	L144
	mov	x1, 0
	b	L148
	.p2align 2,,3
L195:
	mov	w0, w1
L148:
	adds	w0, w0, w5
	bvs	L144
	cmp	w10, w0
	bgt	L145
	cmp	w9, w0
	blt	L145
	cmp	x25, x1
	beq	L145
	ldrb	w2, [x26, x1]
	sxtw	x0, w0
	add	x1, x1, 1
	sub	x0, x0, x21
	strb	w2, [x19, x0]
	cmp	x11, x1
	bne	L195
	ldr	w0, [x29, 132]
	adds	w23, w23, w0
	bmi	L150
	cmp	w23, w0
	bcc	L150
	sxtw	x0, w9
	sub	x0, x0, w10, sxtw
	add	x0, x0, 1
	cmp	x0, x24
	bgt	L196
	str	w23, [x29, 132]
	tbnz	w23, #31, L197
	cmp	w23, w0
	bge	L154
	ldr	x0, [x29, 144]
	bl	_anubis_keccak__keccak_f
L154:
	ldr	x0, [x29, 120]
	mov	sp, x22
	ldp	w1, w0, [x0]
	cmp	w0, w1
	bge	L198
L88:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 400
LCFI27:
	ret
	.p2align 2,,3
L198:
LCFI28:
	sxtw	x0, w0
	sub	x0, x0, w1, sxtw
	add	x0, x0, 1
	cmp	x0, x24
	bgt	L155
	cmp	w23, w0
	bge	L88
	sub	w0, w0, w23
	cmp	w28, w0
	ble	L156
	mov	w28, w0
	b	L137
L145:
	adrp	x0, lC9@PAGE
	mov	w1, 188
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L101:
	mov	x1, sp
	str	x1, [x29, 152]
	b	L120
L161:
	mov	x2, 0
L138:
	mov	x1, x22
	mov	x0, x19
	bl	_memcpy
	mov	sp, x21
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 400
LCFI29:
	ret
L144:
LCFI30:
	adrp	x0, lC9@PAGE
	mov	w1, 188
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L133:
	adrp	x0, lC9@PAGE
	mov	w1, 150
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L184:
	adrp	x0, lC9@PAGE
	mov	w1, 128
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L105:
	adrp	x0, lC9@PAGE
	mov	w1, 133
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L109:
	adrp	x0, lC9@PAGE
	mov	w1, 134
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L150:
	adrp	x0, lC9@PAGE
	mov	w1, 190
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L127:
	adrp	x0, lC9@PAGE
	mov	w1, 150
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L140:
	adrp	x0, lC9@PAGE
	mov	w1, 170
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
	.p2align 2,,3
L190:
	adrp	x0, lC9@PAGE
	mov	w1, 138
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L182:
	adrp	x0, lC9@PAGE
	mov	w1, 118
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L156:
	adrp	x0, lC30@PAGE
	adrp	x1, lC5@PAGE
	add	x0, x0, lC30@PAGEOFF;
	add	x1, x1, lC5@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L183:
	adrp	x0, lC27@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC27@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L191:
	adrp	x0, lC9@PAGE
	mov	w1, 144
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L192:
	adrp	x0, lC9@PAGE
	mov	w1, 144
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L124:
	adrp	x0, lC9@PAGE
	mov	w1, 144
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L194:
	adrp	x0, lC9@PAGE
	mov	w1, 164
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L181:
	adrp	x0, lC26@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC26@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L180:
	adrp	x0, lC25@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC25@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L189:
	adrp	x0, lC9@PAGE
	mov	w1, 135
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L179:
	adrp	x0, lC24@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC24@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L196:
	adrp	x0, lC9@PAGE
	mov	w1, 191
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L197:
	adrp	x0, lC9@PAGE
	mov	w1, 191
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L185:
	adrp	x0, lC28@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC28@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L186:
	adrp	x0, lC29@PAGE
	adrp	x1, lC5@PAGE
	add	x0, x0, lC29@PAGEOFF;
	add	x1, x1, lC5@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L97:
	adrp	x0, lC9@PAGE
	mov	w1, 128
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L187:
	adrp	x0, lC9@PAGE
	mov	w1, 133
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L155:
	adrp	x0, lC9@PAGE
	mov	w1, 177
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE8:
	.const
	.align	2
lC5:
	.word	1
	.word	42
	.text
	.const
	.align	3
lC31:
	.ascii "failed precondition from anubis_sha3.adb:247"
	.align	3
lC32:
	.ascii "failed precondition from anubis_sha3.adb:248"
	.align	3
lC33:
	.ascii "failed precondition from anubis_sha3.adb:249"
	.align	3
lC34:
	.ascii "failed precondition from anubis_sha3.adb:250"
	.align	3
lC35:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:264"
	.align	3
lC36:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:266"
	.align	3
lC37:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:267"
	.align	3
lC38:
	.ascii "Loop_Variant failed at anubis_sha3.adb:268"
	.align	3
lC39:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:303"
	.align	3
lC40:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:304"
	.align	3
lC41:
	.ascii "Loop_Variant failed at anubis_sha3.adb:305"
	.align	3
lC42:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:317"
	.align	3
lC43:
	.ascii "Loop_Invariant failed at anubis_sha3.adb:318"
	.text
	.align	2
	.p2align 5,,15
_anubis_sha3__shake_sponge.constprop.0:
LFB30:
	stp	x29, x30, [sp, -400]!
LCFI31:
	mov	x29, sp
LCFI32:
	mov	x10, x0
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI33:
	mov	x27, x3
	mov	x3, x1
	str	x1, [x29, 120]
	mov	w1, 2147483647
	ldp	w19, w23, [x27]
	str	w5, [x29, 132]
	ldp	w28, w0, [x3]
	cmp	w23, w1
	beq	L297
	cmp	w0, w1
	beq	L298
	cmp	w19, w23
	bgt	L299
	ldr	w0, [x29, 132]
	cmp	w0, 0
	ble	L300
	subs	w0, w23, w19
	cset	w20, vs
	bvs	L207
	cmp	w0, w1
	beq	L207
	ldr	w1, [x29, 132]
	add	w0, w0, 1
	cmp	w1, w0
	bne	L301
	movi	v31.4s, 0
	add	x0, x29, 200
	add	x1, x29, 232
	str	xzr, [x29, 392]
	str	x0, [x29, 168]
	stp	q31, q31, [x0]
	stp	q31, q31, [x1]
	add	x1, x29, 264
	add	x0, x29, 360
	stp	q31, q31, [x1]
	add	x1, x29, 296
	stp	q31, q31, [x1]
	add	x1, x29, 328
	stp	q31, q31, [x1]
	stp	q31, q31, [x0]
	tbnz	w19, #31, L302
	tbnz	w23, #31, L303
	mov	x24, x2
	sxtw	x2, w23
	str	x10, [x29, 160]
	sxtw	x25, w19
	add	x2, x2, 1
	mov	w1, 0
	mov	x0, x24
	sub	x2, x2, x25
	mov	w22, w4
	bl	_memset
	ldr	x0, [x29, 120]
	ldp	w8, w12, [x0]
	cmp	w12, w8
	blt	L211
	ldr	x10, [x29, 160]
	add	x1, x29, 192
	sxtw	x28, w28
	mov	w5, 0
	mov	w0, 0
	str	x1, [x29, 112]
	.p2align 5,,15
L234:
	sxtw	x21, w12
	mov	x1, 2147483647
	sub	x21, x21, w8, sxtw
	add	x21, x21, 1
	cmp	x21, x1
	bgt	L212
	mov	w13, w21
	cmp	w22, w21
	bgt	L290
	tbnz	w5, #31, L304
	sub	w1, w21, w22
	cmp	w1, w5
	blt	L216
	cmp	w5, w21
	bgt	L305
	ldr	w1, [x27]
	cmp	w19, w1
	bne	L306
	ldr	w1, [x27, 4]
	cmp	w23, w1
	bne	L307
	sub	w1, w21, w5
	str	w1, [x29, 136]
	cbz	w0, L220
	cmp	w1, w26
	bge	L308
L220:
	mov	x0, sp
	adds	w21, w5, w8
	str	x0, [x29, 160]
	bvs	L222
	tbnz	w21, #31, L309
	adds	w14, w22, w21
	bvs	L226
	sub	w26, w14, #1
	cmp	w26, w21
	blt	L310
	cmp	w21, w8
	ccmp	w12, w26, 1, ge
	blt	L311
	sxtw	x1, w21
	mov	x0, 1
	sub	x2, x0, x1
	add	x2, x2, w26, sxtw
L229:
	add	x0, x2, 15
	sub	x1, x1, x28
	and	x0, x0, -16
	add	x1, x10, x1
	sub	sp, sp, x0
	str	w5, [x29, 144]
	mov	x0, sp
	str	x10, [x29, 152]
	bl	_memcpy
	ldr	x2, [x29, 112]
	mov	w3, w22
	mov	x1, sp
	stp	w21, w26, [x29, 192]
	ldr	x0, [x29, 168]
	bl	_anubis_sha3__absorb_block
	ldr	w5, [x29, 144]
	ldr	x10, [x29, 152]
	adds	w5, w22, w5
	cset	w13, vs
	cbnz	w13, L312
	ldr	x1, [x29, 120]
	ldr	x0, [x29, 160]
	ldp	w8, w12, [x1]
	mov	sp, x0
	mov	w0, 1
	cmp	w12, w8
	bge	L278
L290:
	mov	x0, sp
	str	x0, [x29, 160]
	tbnz	w5, #31, L313
L237:
	subs	w13, w13, w5
	bvs	L314
	str	w13, [x29, 100]
	str	x10, [x29, 112]
	str	w5, [x29, 120]
	str	w13, [x29, 128]
	str	w8, [x29, 136]
	str	w12, [x29, 144]
	tbnz	w13, #31, L241
	sxtw	x26, w22
	mov	w1, 0
	add	x0, x26, 15
	mov	x2, x26
	sub	w21, w22, #1
	str	x0, [x29, 152]
	and	x0, x0, -16
	sub	sp, sp, x0
	mov	x0, sp
	str	x0, [x29, 104]
	bl	_memset
	ldr	w13, [x29, 128]
	ldr	x4, [x29, 104]
	ldr	w6, [x29, 100]
	cbz	w13, L236
	ldr	w5, [x29, 120]
	ldr	w8, [x29, 136]
	adds	w5, w5, w8
	bvs	L244
	ldr	x10, [x29, 112]
	uxtw	x14, w13
	mov	w0, w5
	mov	x1, 1
	sub	x2, sp, #1
	ldr	w12, [x29, 144]
	b	L249
	.p2align 2,,3
L251:
	cmp	w21, w1
	blt	L250
	adds	w0, w5, w1
	add	x1, x1, 1
	bvs	L244
L249:
	cmp	w8, w0
	ccmp	w12, w0, 1, le
	blt	L250
	sxtw	x0, w0
	sub	x0, x0, x28
	ldrb	w0, [x10, x0]
	strb	w0, [x2, x1]
	cmp	x1, x14
	bne	L251
	cmp	w21, w13
	bge	L236
	adrp	x0, lC9@PAGE
	mov	w1, 291
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L211:
	mov	x0, sp
	sxtw	x26, w22
	mov	x2, x26
	mov	w1, 0
	str	x0, [x29, 160]
	add	x0, x26, 15
	sub	w21, w22, #1
	str	x0, [x29, 152]
	and	x0, x0, -16
	sub	sp, sp, x0
	mov	x0, sp
	str	x0, [x29, 144]
	bl	_memset
	ldr	x4, [x29, 144]
	mov	w6, 0
L236:
	mov	w0, 31
	sub	x5, x26, #1
	mov	x1, x4
	mov	w3, w22
	strb	w0, [x4, w6, sxtw]
	add	x2, x29, 184
	ldrb	w8, [x4, x5]
	ldr	x0, [x29, 168]
	orr	w8, w8, -128
	strb	w8, [x4, x5]
	stp	wzr, w21, [x29, 184]
	bl	_anubis_sha3__absorb_block
	ldr	x1, [x29, 160]
	ldr	w0, [x27]
	mov	sp, x1
	cmp	w19, w0
	bne	L274
	ldr	w0, [x27, 4]
	ldr	w5, [x29, 132]
	cmp	w23, w0
	bne	L275
	ldr	x0, [x29, 152]
	and	x0, x0, -16
	str	x0, [x29, 136]
	add	x0, x29, 176
	str	x0, [x29, 120]
	.p2align 5,,15
L253:
	mov	x0, sp
	cmp	w22, w5
	csel	w8, w22, w5, le
	str	x0, [x29, 144]
	ldr	x0, [x29, 136]
	sub	sp, sp, x0
	mov	x28, sp
	tbnz	w8, #31, L315
	ldr	x2, [x29, 120]
	mov	w3, w22
	mov	x1, sp
	str	w5, [x29, 152]
	ldr	x0, [x29, 168]
	str	w8, [x29, 160]
	stp	wzr, w21, [x29, 176]
	bl	_anubis_sha3__squeeze_block
	ldr	w8, [x29, 160]
	ldr	w5, [x29, 152]
	cbnz	w8, L255
L271:
	adds	w8, w20, w8
	mov	w20, w8
	bvs	L316
	tbnz	w8, #31, L317
	ldr	w0, [x29, 132]
	cmp	w0, w8
	bgt	L318
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 400
LCFI34:
	ret
	.p2align 2,,3
L278:
LCFI35:
	ldr	w26, [x29, 136]
	b	L234
	.p2align 2,,3
L310:
	sxtw	x1, w21
	mov	x2, 0
	b	L229
	.p2align 2,,3
L318:
	ldr	x0, [x29, 168]
	str	w5, [x29, 160]
	bl	_anubis_keccak__keccak_f
	ldr	x1, [x29, 144]
	ldr	w0, [x27]
	mov	sp, x1
	cmp	w19, w0
	bne	L274
	ldr	w0, [x27, 4]
	cmp	w23, w0
	bne	L275
	ldr	w0, [x29, 132]
	ldr	w5, [x29, 160]
	sub	w0, w0, w20
	cmp	w0, w5
	bge	L276
	mov	w5, w0
	b	L253
	.p2align 2,,3
L255:
	ldp	w10, w11, [x27]
	mov	x1, 0
	uxtw	x12, w8
	b	L270
	.p2align 2,,3
L268:
	cmp	x1, x26
	beq	L269
	ldrb	w2, [x28, x1]
	sxtw	x0, w0
	add	x1, x1, 1
	sub	x0, x0, x25
	strb	w2, [x24, x0]
	cmp	x1, x12
	beq	L271
L270:
	cmp	w19, w10
	bne	L319
	cmp	w23, w11
	bne	L320
	adds	w2, w20, w19
	bvs	L267
	adds	w0, w1, w2
	bvs	L267
	cmp	w19, w0
	ccmp	w23, w0, 1, le
	bge	L268
L269:
	adrp	x0, lC9@PAGE
	mov	w1, 319
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L216:
	mov	x0, sp
	str	x0, [x29, 160]
	b	L237
L267:
	adrp	x0, lC9@PAGE
	mov	w1, 319
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L320:
	adrp	x0, lC43@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC43@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L319:
	adrp	x0, lC42@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC42@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L250:
	adrp	x0, lC9@PAGE
	mov	w1, 287
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L222:
	adrp	x0, lC9@PAGE
	mov	w1, 270
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L226:
	adrp	x0, lC9@PAGE
	mov	w1, 271
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L244:
	adrp	x0, lC9@PAGE
	mov	w1, 287
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L309:
	adrp	x0, lC9@PAGE
	mov	w1, 270
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L275:
	adrp	x0, lC40@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC40@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L315:
	adrp	x0, lC9@PAGE
	mov	w1, 310
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L311:
	adrp	x0, lC9@PAGE
	mov	w1, 272
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L241:
	adrp	x0, lC9@PAGE
	mov	w1, 281
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L298:
	adrp	x0, lC32@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC32@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L297:
	adrp	x0, lC31@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC31@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L305:
	adrp	x0, lC35@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC35@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L306:
	adrp	x0, lC36@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC36@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L313:
	adrp	x0, lC9@PAGE
	mov	w1, 281
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L314:
	adrp	x0, lC9@PAGE
	mov	w1, 281
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
	.p2align 2,,3
L316:
	adrp	x0, lC9@PAGE
	mov	w1, 322
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L317:
	adrp	x0, lC9@PAGE
	mov	w1, 325
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L312:
	adrp	x0, lC9@PAGE
	mov	w1, 275
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L274:
	adrp	x0, lC39@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC39@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L304:
	adrp	x0, lC9@PAGE
	mov	w1, 263
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L212:
	adrp	x0, lC9@PAGE
	mov	w1, 263
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L307:
	adrp	x0, lC37@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC37@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L308:
	adrp	x0, lC38@PAGE
	adrp	x1, lC5@PAGE
	add	x0, x0, lC38@PAGEOFF;
	add	x1, x1, lC5@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L276:
	adrp	x0, lC41@PAGE
	adrp	x1, lC5@PAGE
	add	x0, x0, lC41@PAGEOFF;
	add	x1, x1, lC5@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L301:
	adrp	x0, lC34@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC34@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L207:
	adrp	x0, lC9@PAGE
	mov	w1, 250
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L300:
	adrp	x0, lC9@PAGE
	mov	w1, 250
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L299:
	adrp	x0, lC33@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC33@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L303:
	adrp	x0, lC9@PAGE
	mov	w1, 256
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L302:
	adrp	x0, lC9@PAGE
	mov	w1, 255
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE30:
	.const
	.align	3
lC44:
	.ascii "failed precondition from anubis_sha3.ads:63"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__sha3_256
_anubis_sha3__sha3_256:
LFB9:
	ldr	w4, [x1, 4]
	mov	w3, 2147483647
	cmp	w4, w3
	beq	L326
	adrp	x3, lC4@PAGE
	mov	w5, 6
	add	x3, x3, lC4@PAGEOFF;
	mov	w4, 136
	b	_anubis_sha3__sha3_sponge
L326:
	adrp	x0, lC44@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI36:
	add	x0, x0, lC44@PAGEOFF;
	mov	x29, sp
LCFI37:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE9:
	.const
	.align	2
lC4:
	.word	0
	.word	31
	.text
	.const
	.align	3
lC45:
	.ascii "failed precondition from anubis_sha3.ads:72"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__sha3_384
_anubis_sha3__sha3_384:
LFB11:
	ldr	w4, [x1, 4]
	mov	w3, 2147483647
	cmp	w4, w3
	beq	L332
	adrp	x3, lC6@PAGE
	mov	w5, 6
	add	x3, x3, lC6@PAGEOFF;
	mov	w4, 104
	b	_anubis_sha3__sha3_sponge
L332:
	adrp	x0, lC45@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI38:
	add	x0, x0, lC45@PAGEOFF;
	mov	x29, sp
LCFI39:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE11:
	.const
	.align	2
lC6:
	.word	0
	.word	47
	.text
	.const
	.align	3
lC46:
	.ascii "failed precondition from anubis_sha3.ads:81"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__sha3_512
_anubis_sha3__sha3_512:
LFB13:
	ldr	w4, [x1, 4]
	mov	w3, 2147483647
	cmp	w4, w3
	beq	L338
	adrp	x3, lC7@PAGE
	mov	w5, 6
	add	x3, x3, lC7@PAGEOFF;
	mov	w4, 72
	b	_anubis_sha3__sha3_sponge
L338:
	adrp	x0, lC46@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI40:
	add	x0, x0, lC46@PAGEOFF;
	mov	x29, sp
LCFI41:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE13:
	.const
	.align	2
lC7:
	.word	0
	.word	63
	.text
	.const
	.align	3
lC47:
	.ascii "failed precondition from anubis_sha3.ads:92"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__keccak_256
_anubis_sha3__keccak_256:
LFB15:
	ldr	w4, [x1, 4]
	mov	w3, 2147483647
	cmp	w4, w3
	beq	L344
	adrp	x3, lC4@PAGE
	mov	w5, 1
	add	x3, x3, lC4@PAGEOFF;
	mov	w4, 136
	b	_anubis_sha3__sha3_sponge
L344:
	adrp	x0, lC47@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI42:
	add	x0, x0, lC47@PAGEOFF;
	mov	x29, sp
LCFI43:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE15:
	.const
	.align	3
lC48:
	.ascii "failed precondition from anubis_sha3.ads:103"
	.align	3
lC49:
	.ascii "failed precondition from anubis_sha3.ads:104"
	.align	3
lC50:
	.ascii "failed postcondition from anubis_sha3.ads:105"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__shake128
_anubis_sha3__shake128:
LFB18:
	stp	x29, x30, [sp, -48]!
LCFI44:
	mov	x29, sp
LCFI45:
	mov	w5, w4
	mov	w6, 2147483647
	stp	x19, x20, [sp, 16]
	ldr	w4, [x1, 4]
	str	x21, [sp, 32]
LCFI46:
	ldp	w21, w20, [x3]
	cmp	w20, w6
	beq	L347
	cmp	w4, w6
	beq	L347
	cmp	w20, w21
	blt	L348
	sxtw	x4, w20
	mov	x6, 2147483647
	sub	x4, x4, w21, sxtw
	add	x4, x4, 1
	cmp	x4, x6
	bgt	L356
	cmp	w5, 0
	ble	L350
	cmp	w5, w4
	bne	L351
	mov	w4, 65535
	cmp	w5, w4
	bgt	L351
	mov	w4, 168
	mov	x19, x3
	bl	_anubis_sha3__shake_sponge.constprop.0
	ldp	w1, w0, [x19]
	cmp	w1, w21
	ccmp	w0, w20, 0, eq
	bne	L357
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI47:
	ret
	.p2align 2,,3
L348:
LCFI48:
	cmp	w5, 0
	ble	L350
L351:
	adrp	x0, lC49@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC49@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L347:
	adrp	x0, lC48@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC48@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L350:
	adrp	x0, lC13@PAGE
	mov	w1, 104
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L357:
	adrp	x0, lC50@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC50@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L356:
	adrp	x0, lC13@PAGE
	mov	w1, 104
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE18:
	.const
	.align	2
lC2:
	.word	1
	.word	45
	.text
	.const
	.align	3
lC51:
	.ascii "failed precondition from anubis_sha3.ads:116"
	.align	3
lC52:
	.ascii "failed precondition from anubis_sha3.ads:117"
	.align	3
lC53:
	.ascii "failed postcondition from anubis_sha3.ads:118"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_sha3__shake256
_anubis_sha3__shake256:
LFB20:
	stp	x29, x30, [sp, -48]!
LCFI49:
	mov	x29, sp
LCFI50:
	mov	w5, w4
	mov	w6, 2147483647
	stp	x19, x20, [sp, 16]
	ldr	w4, [x1, 4]
	str	x21, [sp, 32]
LCFI51:
	ldp	w21, w20, [x3]
	cmp	w20, w6
	beq	L360
	cmp	w4, w6
	beq	L360
	cmp	w20, w21
	blt	L361
	sxtw	x4, w20
	mov	x6, 2147483647
	sub	x4, x4, w21, sxtw
	add	x4, x4, 1
	cmp	x4, x6
	bgt	L369
	cmp	w5, 0
	ble	L363
	cmp	w5, w4
	bne	L364
	mov	w4, 65535
	cmp	w5, w4
	bgt	L364
	mov	w4, 136
	mov	x19, x3
	bl	_anubis_sha3__shake_sponge.constprop.0
	ldp	w1, w0, [x19]
	cmp	w1, w21
	ccmp	w0, w20, 0, eq
	bne	L370
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI52:
	ret
	.p2align 2,,3
L361:
LCFI53:
	cmp	w5, 0
	ble	L363
L364:
	adrp	x0, lC52@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC52@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L360:
	adrp	x0, lC51@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC51@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L363:
	adrp	x0, lC13@PAGE
	mov	w1, 117
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L370:
	adrp	x0, lC53@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC53@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L369:
	adrp	x0, lC13@PAGE
	mov	w1, 117
	add	x0, x0, lC13@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE20:
	.globl _anubis_sha3__keccak_suffix
	.const
_anubis_sha3__keccak_suffix:
	.byte	1
	.globl _anubis_sha3__shake_suffix
_anubis_sha3__shake_suffix:
	.byte	31
	.globl _anubis_sha3__sha3_suffix
_anubis_sha3__sha3_suffix:
	.byte	6
	.globl _anubis_sha3_E
	.data
	.align	1
_anubis_sha3_E:
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
	.quad	LFB3-.
	.set L$set$8,LFE3-LFB3
	.quad L$set$8
	.uleb128 0
	.byte	0x4
	.set L$set$9,LCFI4-LFB3
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
	.byte	0x95
	.uleb128 0x4
	.byte	0x96
	.uleb128 0x3
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0x97
	.uleb128 0x2
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
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
	.quad	LFB5-.
	.set L$set$16,LFE5-LFB5
	.quad L$set$16
	.uleb128 0
	.byte	0x4
	.set L$set$17,LCFI10-LFB5
	.long L$set$17
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$18,LCFI11-LCFI10
	.long L$set$18
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$19,LCFI12-LCFI11
	.long L$set$19
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
	.byte	0x98
	.uleb128 0x9
	.byte	0x99
	.uleb128 0x8
	.byte	0x9a
	.uleb128 0x7
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0x9b
	.uleb128 0x6
	.byte	0x9c
	.uleb128 0x5
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
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
	.set L$set$22,LCFI15-LCFI14
	.long L$set$22
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$23,LEFDE7-LASFDE7
	.long L$set$23
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB6-.
	.set L$set$24,LFE6-LFB6
	.quad L$set$24
	.uleb128 0
	.byte	0x4
	.set L$set$25,LCFI16-LFB6
	.long L$set$25
	.byte	0xe
	.uleb128 0x80
	.byte	0x9d
	.uleb128 0x10
	.byte	0x9e
	.uleb128 0xf
	.byte	0x4
	.set L$set$26,LCFI17-LCFI16
	.long L$set$26
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$27,LCFI18-LCFI17
	.long L$set$27
	.byte	0x93
	.uleb128 0xe
	.byte	0x94
	.uleb128 0xd
	.byte	0x95
	.uleb128 0xc
	.byte	0x96
	.uleb128 0xb
	.byte	0x4
	.set L$set$28,LCFI19-LCFI18
	.long L$set$28
	.byte	0x97
	.uleb128 0xa
	.byte	0x98
	.uleb128 0x9
	.byte	0x4
	.set L$set$29,LCFI20-LCFI19
	.long L$set$29
	.byte	0x99
	.uleb128 0x8
	.byte	0x9a
	.uleb128 0x7
	.byte	0x9b
	.uleb128 0x6
	.byte	0x9c
	.uleb128 0x5
	.byte	0x4
	.set L$set$30,LCFI21-LCFI20
	.long L$set$30
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
	.set L$set$31,LCFI22-LCFI21
	.long L$set$31
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$32,LEFDE9-LASFDE9
	.long L$set$32
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB8-.
	.set L$set$33,LFE8-LFB8
	.quad L$set$33
	.uleb128 0
	.byte	0x4
	.set L$set$34,LCFI23-LFB8
	.long L$set$34
	.byte	0xe
	.uleb128 0x190
	.byte	0x9d
	.uleb128 0x32
	.byte	0x9e
	.uleb128 0x31
	.byte	0x4
	.set L$set$35,LCFI24-LCFI23
	.long L$set$35
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$36,LCFI25-LCFI24
	.long L$set$36
	.byte	0x93
	.uleb128 0x30
	.byte	0x94
	.uleb128 0x2f
	.byte	0x95
	.uleb128 0x2e
	.byte	0x96
	.uleb128 0x2d
	.byte	0x97
	.uleb128 0x2c
	.byte	0x98
	.uleb128 0x2b
	.byte	0x4
	.set L$set$37,LCFI26-LCFI25
	.long L$set$37
	.byte	0x99
	.uleb128 0x2a
	.byte	0x9a
	.uleb128 0x29
	.byte	0x9b
	.uleb128 0x28
	.byte	0x9c
	.uleb128 0x27
	.byte	0x4
	.set L$set$38,LCFI27-LCFI26
	.long L$set$38
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
	.set L$set$39,LCFI28-LCFI27
	.long L$set$39
	.byte	0xb
	.byte	0x4
	.set L$set$40,LCFI29-LCFI28
	.long L$set$40
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
	.set L$set$41,LCFI30-LCFI29
	.long L$set$41
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$42,LEFDE11-LASFDE11
	.long L$set$42
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB30-.
	.set L$set$43,LFE30-LFB30
	.quad L$set$43
	.uleb128 0
	.byte	0x4
	.set L$set$44,LCFI31-LFB30
	.long L$set$44
	.byte	0xe
	.uleb128 0x190
	.byte	0x9d
	.uleb128 0x32
	.byte	0x9e
	.uleb128 0x31
	.byte	0x4
	.set L$set$45,LCFI32-LCFI31
	.long L$set$45
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$46,LCFI33-LCFI32
	.long L$set$46
	.byte	0x93
	.uleb128 0x30
	.byte	0x94
	.uleb128 0x2f
	.byte	0x95
	.uleb128 0x2e
	.byte	0x96
	.uleb128 0x2d
	.byte	0x97
	.uleb128 0x2c
	.byte	0x98
	.uleb128 0x2b
	.byte	0x99
	.uleb128 0x2a
	.byte	0x9a
	.uleb128 0x29
	.byte	0x9b
	.uleb128 0x28
	.byte	0x9c
	.uleb128 0x27
	.byte	0x4
	.set L$set$47,LCFI34-LCFI33
	.long L$set$47
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
	.set L$set$48,LCFI35-LCFI34
	.long L$set$48
	.byte	0xb
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$49,LEFDE13-LASFDE13
	.long L$set$49
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB9-.
	.set L$set$50,LFE9-LFB9
	.quad L$set$50
	.uleb128 0
	.byte	0x4
	.set L$set$51,LCFI36-LFB9
	.long L$set$51
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$52,LCFI37-LCFI36
	.long L$set$52
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$53,LEFDE15-LASFDE15
	.long L$set$53
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB11-.
	.set L$set$54,LFE11-LFB11
	.quad L$set$54
	.uleb128 0
	.byte	0x4
	.set L$set$55,LCFI38-LFB11
	.long L$set$55
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$56,LCFI39-LCFI38
	.long L$set$56
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE15:
LSFDE17:
	.set L$set$57,LEFDE17-LASFDE17
	.long L$set$57
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB13-.
	.set L$set$58,LFE13-LFB13
	.quad L$set$58
	.uleb128 0
	.byte	0x4
	.set L$set$59,LCFI40-LFB13
	.long L$set$59
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$60,LCFI41-LCFI40
	.long L$set$60
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$61,LEFDE19-LASFDE19
	.long L$set$61
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB15-.
	.set L$set$62,LFE15-LFB15
	.quad L$set$62
	.uleb128 0
	.byte	0x4
	.set L$set$63,LCFI42-LFB15
	.long L$set$63
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$64,LCFI43-LCFI42
	.long L$set$64
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$65,LEFDE21-LASFDE21
	.long L$set$65
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB18-.
	.set L$set$66,LFE18-LFB18
	.quad L$set$66
	.uleb128 0
	.byte	0x4
	.set L$set$67,LCFI44-LFB18
	.long L$set$67
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$68,LCFI45-LCFI44
	.long L$set$68
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$69,LCFI46-LCFI45
	.long L$set$69
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$70,LCFI47-LCFI46
	.long L$set$70
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
	.set L$set$71,LCFI48-LCFI47
	.long L$set$71
	.byte	0xb
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$72,LEFDE23-LASFDE23
	.long L$set$72
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB20-.
	.set L$set$73,LFE20-LFB20
	.quad L$set$73
	.uleb128 0
	.byte	0x4
	.set L$set$74,LCFI49-LFB20
	.long L$set$74
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$75,LCFI50-LCFI49
	.long L$set$75
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$76,LCFI51-LCFI50
	.long L$set$76
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x4
	.set L$set$77,LCFI52-LCFI51
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
	.set L$set$78,LCFI53-LCFI52
	.long L$set$78
	.byte	0xb
	.align	3
LEFDE23:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
