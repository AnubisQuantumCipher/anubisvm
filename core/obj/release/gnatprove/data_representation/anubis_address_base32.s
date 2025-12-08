	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC6:
	.ascii "Static_Predicate failed at anubis_address_base32.adb:8"
	.text
	.align	2
	.p2align 5,,15
_anubis_address_base32__encode_5bits.part.0:
LFB30:
	adrp	x0, lC6@PAGE
	adrp	x1, lC1@PAGE
	stp	x29, x30, [sp, -16]!
LCFI0:
	add	x0, x0, lC6@PAGEOFF;
	mov	x29, sp
LCFI1:
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE30:
	.const
	.align	2
lC1:
	.word	1
	.word	54
	.text
	.const
	.align	3
lC7:
	.ascii "failed postcondition from anubis_address_base32.ads:100"
	.text
	.align	2
	.p2align 5,,15
_anubis_address_base32__decode_char.part.0:
LFB31:
	adrp	x0, lC7@PAGE
	adrp	x1, lC2@PAGE
	stp	x29, x30, [sp, -16]!
LCFI2:
	add	x0, x0, lC7@PAGEOFF;
	mov	x29, sp
LCFI3:
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE31:
	.const
	.align	2
lC2:
	.word	1
	.word	55
	.text
	.const
	.align	3
lC8:
	.ascii "anubis_address_base32.adb"
	.space 1
	.align	3
lC9:
	.ascii "Static_Predicate failed at anubis_address_base32.adb:10"
	.text
	.align	2
	.p2align 5,,15
_anubis_address_base32__encode_5bits___wrapped_statements.0:
LFB3:
	stp	x29, x30, [sp, -16]!
LCFI4:
	mov	x29, sp
LCFI5:
	ldrb	w1, [x16]
	cmp	w1, 31
	bhi	L13
	adrp	x0, _anubis_address_base32__encode_table@PAGE
	add	x0, x0, _anubis_address_base32__encode_table@PAGEOFF;
	ldrsb	w0, [x0, w1, uxtw]
	cmp	w0, 57
	ble	L14
	sub	w1, w0, #97
	and	w1, w1, 255
	cmp	w1, 25
	bhi	L10
	mov	x2, 46847
	movk	x2, 0x3ef, lsl 16
	lsr	x1, x2, x1
	tbz	x1, 0, L10
L9:
	ldp	x29, x30, [sp], 16
LCFI6:
	ret
	.p2align 2,,3
L14:
LCFI7:
	cmp	w0, 47
	bgt	L9
L10:
	adrp	x0, lC9@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L13:
	adrp	x0, lC8@PAGE
	mov	w1, 10
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LFE3:
	.const
	.align	3
lC10:
	.ascii "failed precondition from anubis_address_base32.ads:93"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_base32__encode_5bits
_anubis_address_base32__encode_5bits:
LFB2:
	stp	x29, x30, [sp, -32]!
LCFI8:
	mov	x29, sp
LCFI9:
	add	x1, x29, 32
	strb	w0, [x29, 16]
	str	x1, [x29, 24]
	cmp	w0, 31
	bhi	L19
	add	x16, x29, 16
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L17
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L20
L17:
	ldp	x29, x30, [sp], 32
LCFI10:
	ret
	.p2align 2,,3
L20:
LCFI11:
	bl	_anubis_address_base32__encode_5bits.part.0
L19:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE2:
	.const
	.align	2
lC0:
	.word	1
	.word	53
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_base32__decode_char
_anubis_address_base32__decode_char:
LFB4:
	sub	w0, w0, #48
	and	w0, w0, 255
	cmp	w0, 74
	bhi	L23
	adrp	x1, _CSWTCH.75@PAGE
	add	x1, x1, _CSWTCH.75@PAGEOFF;
	ldrb	w0, [x1, w0, uxtw]
	sub	w1, w0, #32
	and	w1, w1, 255
	cmp	w1, 222
	bls	L27
	ret
	.p2align 2,,3
L23:
	mov	w0, 255
	ret
L27:
	stp	x29, x30, [sp, -16]!
LCFI12:
	mov	x29, sp
LCFI13:
	bl	_anubis_address_base32__decode_char.part.0
LFE4:
	.const
	.align	3
lC11:
	.ascii "Loop_Invariant failed at anubis_address_base32.adb:68"
	.align	3
lC12:
	.ascii "failed postcondition from anubis_address_base32.ads:35"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_base32__encode_account_id
_anubis_address_base32__encode_account_id:
LFB6:
	stp	x29, x30, [sp, -192]!
LCFI14:
	mov	x29, sp
LCFI15:
	stp	x23, x24, [sp, 48]
LCFI16:
	mov	x24, 46847
	mov	x23, x0
	movk	x24, 0x3ef, lsl 16
	stp	x19, x20, [sp, 16]
LCFI17:
	mov	x20, x1
	stp	x21, x22, [sp, 32]
LCFI18:
	mov	w22, 0
	add	x21, x29, 112
	stp	x25, x26, [sp, 64]
LCFI19:
	mov	x26, x1
	mov	w25, 0
	stp	x27, x28, [sp, 80]
LCFI20:
	stp	x0, x1, [x29, 96]
	.p2align 5,,15
L29:
	mov	x27, x26
	mov	w28, 35
	ldrb	w4, [x23, 1]
	ldrb	w19, [x23]
	ldrb	w2, [x23, 3]
	lsl	x4, x4, 24
	ldrb	w0, [x23, 2]
	orr	x19, x4, x19, lsl 32
	ldrb	w4, [x23, 4]
	lsl	x2, x2, 8
	orr	x0, x2, x0, lsl 16
	orr	x19, x19, x4
	orr	x19, x19, x0
	.p2align 5,,15
L31:
	lsr	x0, x19, x28
	add	x1, x29, 192
	and	x0, x0, 31
	mov	x16, x21
	str	x1, [x29, 120]
	strb	w0, [x29, 112]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L30
	sub	w2, w0, #97
	lsr	x2, x24, x2
	tbz	x2, 0, L59
L30:
	sub	w28, w28, #5
	strb	w0, [x27], 1
	cmn	w28, #5
	bne	L31
	add	w25, w25, 8
	add	w1, w22, 5
	cmp	w25, 48
	beq	L60
	mov	w0, 52429
	add	x26, x26, 8
	movk	w0, 0xcccc, lsl 16
	add	x23, x23, 5
	umull	x0, w22, w0
	lsr	x0, x0, 34
	add	w0, w0, 1
	cmp	w25, w0, lsl 3
	bne	L61
	mov	w22, w1
	b	L29
	.p2align 2,,3
L59:
	adrp	x0, lC6@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L60:
	ldr	x0, [x29, 96]
	add	x16, x29, 128
	ldrh	w19, [x0, 30]
	add	x0, x29, 192
	str	x0, [x29, 136]
	rev16	w19, w19
	ubfx	x0, x19, 11, 5
	and	x19, x19, 65535
	strb	w0, [x29, 128]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L33
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L35
L33:
	ldr	x2, [x29, 104]
	ubfx	x1, x19, 6, 5
	add	x16, x29, 144
	strb	w1, [x29, 144]
	strb	w0, [x2, 48]
	add	x0, x29, 192
	str	x0, [x29, 152]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L34
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L35
L34:
	ldr	x2, [x29, 104]
	ubfx	x1, x19, 1, 5
	add	x16, x29, 160
	strb	w1, [x29, 160]
	strb	w0, [x2, 49]
	add	x0, x29, 192
	str	x0, [x29, 168]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L36
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L35
L36:
	ldr	x1, [x29, 104]
	ubfiz	x19, x19, 4, 1
	add	x16, x29, 176
	strb	w19, [x29, 176]
	strb	w0, [x1, 50]
	add	x0, x29, 192
	str	x0, [x29, 184]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L37
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L35
L37:
	ldr	x3, [x29, 104]
	mov	x2, 46847
	movk	x2, 0x3ef, lsl 16
	add	x1, x3, 52
	strb	w0, [x3, 51]
	b	L42
	.p2align 2,,3
L38:
	sub	w0, w0, #97
	and	w0, w0, 255
	cmp	w0, 25
	bhi	L40
	lsr	x0, x2, x0
	tbz	x0, 0, L40
L39:
	add	x20, x20, 1
	cmp	x1, x20
	beq	L62
L42:
	ldrsb	w0, [x20]
	cmp	w0, 57
	bgt	L38
	cmp	w0, 47
	bgt	L39
L40:
	adrp	x0, lC12@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L35:
	bl	_anubis_address_base32__encode_5bits.part.0
L62:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 192
LCFI21:
	ret
L61:
LCFI22:
	adrp	x0, lC11@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE6:
	.const
	.align	3
lC13:
	.ascii "failed postcondition from anubis_address_base32.ads:44"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_base32__encode_checksum
_anubis_address_base32__encode_checksum:
LFB8:
	stp	x29, x30, [sp, -160]!
LCFI23:
	mov	x29, sp
LCFI24:
	and	w1, w0, 65280
	add	x2, x29, 160
	add	x16, x29, 56
	str	x19, [sp, 16]
LCFI25:
	mov	w19, w0
	ubfiz	w19, w19, 16, 8
	str	x0, [x29, 40]
	ubfx	x0, x0, 16, 8
	orr	w19, w19, w1
	str	x2, [x29, 64]
	lsr	w1, w19, 19
	orr	w19, w19, w0
	strb	w1, [x29, 56]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L64
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L66
L64:
	ubfx	x1, x19, 14, 5
	add	x2, x29, 160
	strb	w0, [x29, 144]
	add	x16, x29, 72
	strb	w1, [x29, 72]
	str	x2, [x29, 80]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L65
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L66
L65:
	ubfx	x1, x19, 9, 5
	add	x2, x29, 160
	strb	w0, [x29, 145]
	add	x16, x29, 88
	strb	w1, [x29, 88]
	str	x2, [x29, 96]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L67
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L66
L67:
	ubfx	x1, x19, 4, 5
	add	x2, x29, 160
	strb	w0, [x29, 146]
	add	x16, x29, 104
	strb	w1, [x29, 104]
	str	x2, [x29, 112]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L68
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L66
L68:
	ubfiz	w19, w19, 1, 4
	add	x1, x29, 160
	strb	w0, [x29, 147]
	add	x16, x29, 120
	strb	w19, [x29, 120]
	str	x1, [x29, 128]
	bl	_anubis_address_base32__encode_5bits___wrapped_statements.0
	cmp	w0, 57
	ble	L69
	mov	x1, 46847
	sub	w2, w0, #97
	movk	x1, 0x3ef, lsl 16
	lsr	x1, x1, x2
	tbz	x1, 0, L66
L69:
	mov	x3, 46847
	mov	x1, 0
	strb	w0, [x29, 148]
	add	x2, x29, 144
	movk	x3, 0x3ef, lsl 16
L74:
	ldrsb	w0, [x2, x1]
	cmp	w0, 57
	ble	L92
	sub	w0, w0, #97
	and	w0, w0, 255
	cmp	w0, 25
	bhi	L72
	lsr	x0, x3, x0
	tbz	x0, 0, L72
L71:
	add	x1, x1, 1
	cmp	x1, 5
	bne	L74
	ldr	w3, [x29, 144]
	mov	x0, 0
	ldrb	w1, [x2, 4]
	bfi	x0, x3, 0, 32
	bfi	x0, x1, 32, 8
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 160
LCFI26:
	ret
	.p2align 2,,3
L66:
LCFI27:
	bl	_anubis_address_base32__encode_5bits.part.0
	.p2align 2,,3
L92:
	cmp	w0, 47
	bgt	L71
L72:
	adrp	x0, lC13@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC13@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE8:
	.const
	.align	3
lC14:
	.ascii "Loop_Invariant failed at anubis_address_base32.adb:140"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_base32__decode_account_id
_anubis_address_base32__decode_account_id:
LFB10:
	ldrsb	w2, [x0]
	adrp	x12, _CSWTCH.75@PAGE
	mov	w7, 0
	mov	w10, 0
	mov	x8, x1
	mov	x4, x0
	mov	x11, x0
	add	x9, x12, _CSWTCH.75@PAGEOFF;
	mov	x5, 0
	mov	w3, 35
	sub	w2, w2, #48
	and	w2, w2, 255
	cmp	w2, 74
	bhi	L121
	stp	x29, x30, [sp, -16]!
LCFI28:
	mov	x29, sp
LCFI29:
L95:
	ldrb	w2, [x9, w2, uxtw]
	sub	w6, w2, #32
	and	w6, w6, 255
	cmp	w6, 222
	bls	L122
	cmp	w2, 255
	beq	L98
	lsl	x2, x2, x3
	sub	w3, w3, #5
	add	x4, x4, 1
	orr	x5, x5, x2
	cmn	w3, #5
	beq	L123
L99:
	ldrsb	w2, [x4]
	sub	w2, w2, #48
	and	w2, w2, 255
	cmp	w2, 74
	bls	L95
L98:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI30:
	ret
	.p2align 2,,3
L121:
	mov	w0, 0
	ret
	.p2align 2,,3
L123:
LCFI31:
	lsr	x4, x5, 32
	lsr	x3, x5, 24
	strb	w5, [x8, 4]
	lsr	x2, x5, 16
	lsr	x5, x5, 8
	add	w10, w10, 5
	strb	w4, [x8]
	add	w6, w7, 8
	strb	w3, [x8, 1]
	strb	w2, [x8, 2]
	strb	w5, [x8, 3]
	cmp	w10, 30
	beq	L124
	asr	w7, w7, 3
	add	x8, x8, 5
	add	w7, w7, 1
	add	x4, x11, 8
	add	w7, w7, w7, lsl 2
	cmp	w7, w10
	bne	L125
	mov	w7, w6
	mov	x11, x4
	mov	x5, 0
	mov	w3, 35
	b	L99
	.p2align 2,,3
L124:
	mov	x6, 0
	add	x12, x12, _CSWTCH.75@PAGEOFF;
	add	x4, x0, 48
	mov	w3, 11
L102:
	ldrsb	w2, [x4]
	sub	w2, w2, #48
	and	w2, w2, 255
	cmp	w2, 74
	bhi	L98
	ldrb	w2, [x12, w2, uxtw]
	sub	w5, w2, #32
	and	w5, w5, 255
	cmp	w5, 222
	bls	L103
	cmp	w2, 255
	beq	L98
	lsl	x2, x2, x3
	sub	w3, w3, #5
	add	x4, x4, 1
	orr	x6, x6, x2
	cmn	w3, #4
	bne	L102
	ldrsb	w0, [x0, 51]
	sub	w0, w0, #48
	and	w0, w0, 255
	cmp	w0, 74
	bhi	L98
	ldrb	w0, [x12, w0, uxtw]
	sub	w2, w0, #32
	and	w2, w2, 255
	cmp	w2, 222
	bls	L103
	cmp	w0, 255
	beq	L98
	ubfx	x2, x0, 4, 4
	mov	w0, 1
	orr	x2, x2, x6
	rev16	w2, w2
	strh	w2, [x1, 30]
	ldp	x29, x30, [sp], 16
LCFI32:
	ret
L122:
LCFI33:
	adrp	x0, lC7@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L125:
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L103:
	bl	_anubis_address_base32__decode_char.part.0
LFE10:
	.align	2
	.p2align 5,,15
	.globl _anubis_address_base32__decode_checksum
_anubis_address_base32__decode_checksum:
LFB11:
	stp	x29, x30, [sp, -32]!
LCFI34:
	adrp	x5, _CSWTCH.75@PAGE
	mov	x29, sp
LCFI35:
	mov	w6, 0
	mov	w7, 0
	mov	w8, 0
	mov	w3, 0
	add	x5, x5, _CSWTCH.75@PAGEOFF;
	add	x2, x29, 24
	str	x0, [x29, 24]
	mov	w1, 19
L131:
	ldrsb	w0, [x2]
	sub	w0, w0, #48
	and	w0, w0, 255
	cmp	w0, 74
	bhi	L130
	ldrb	w0, [x5, w0, uxtw]
	sub	w4, w0, #32
	and	w4, w4, 255
	cmp	w4, 222
	bls	L132
	cmp	w0, 255
	beq	L130
	lsl	w0, w0, w1
	sub	w1, w1, #5
	orr	w3, w3, w0
	add	x2, x2, 1
	cmn	w1, #1
	bne	L131
	ldrsb	w0, [x29, 28]
	sub	w0, w0, #48
	and	w0, w0, 255
	cmp	w0, 74
	bhi	L130
	ldrb	w0, [x5, w0, uxtw]
	sub	w1, w0, #32
	and	w1, w1, 255
	cmp	w1, 222
	bls	L132
	cmp	w0, 255
	beq	L130
	orr	w0, w3, w0, lsr 1
	ubfx	x8, x3, 16, 8
	ubfx	x7, x3, 8, 8
	mov	w1, 1
	and	w6, w0, 255
	mov	x0, 0
	bfi	x0, x8, 0, 8
	bfi	x0, x7, 8, 8
	bfi	x0, x6, 16, 8
	bfi	x0, x1, 24, 8
	ldp	x29, x30, [sp], 32
LCFI36:
	ret
	.p2align 2,,3
L130:
LCFI37:
	mov	x0, 0
	mov	w1, 0
	bfi	x0, x8, 0, 8
	bfi	x0, x7, 8, 8
	bfi	x0, x6, 16, 8
	bfi	x0, x1, 24, 8
	ldp	x29, x30, [sp], 32
LCFI38:
	ret
L132:
LCFI39:
	bl	_anubis_address_base32__decode_char.part.0
LFE11:
	.const
	.align	3
lC15:
	.ascii "Loop_Invariant failed at anubis_address_base32.adb:265"
	.align	3
lC16:
	.ascii "failed postcondition from anubis_address_base32.ads:72"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_base32__chunk_payload
_anubis_address_base32__chunk_payload:
LFB12:
	stp	x29, x30, [sp, -16]!
LCFI40:
	mov	w5, 0
	mov	x29, sp
LCFI41:
	mov	w3, 0
	mov	x4, x0
	mov	x2, x1
	mov	w6, 45
	b	L142
	.p2align 2,,3
L143:
	add	x4, x4, 8
	add	x2, x2, 9
L142:
	ldr	d31, [x4]
	add	w3, w3, 8
	add	w5, w5, 9
	strb	w6, [x2, 8]
	str	d31, [x2]
	cmp	w3, 48
	bne	L143
	cmp	w5, 54
	bne	L144
	ldrsb	w5, [x1, 26]
	mov	w2, 45
	ldrsb	w4, [x1, 35]
	ldrsb	w3, [x1, 53]
	cmp	w5, w2
	ldrsb	w6, [x1, 8]
	ccmp	w4, w2, 0, eq
	ldrsb	w5, [x1, 17]
	ccmp	w3, w2, 0, eq
	cset	w3, ne
	ldrb	w4, [x0, 48]
	cmp	w6, w2
	ldrb	w6, [x0, 49]
	ccmp	w5, w2, 0, eq
	ldrb	w5, [x0, 50]
	strb	w4, [x1, 54]
	ldrb	w4, [x0, 51]
	strb	w6, [x1, 55]
	ldrsb	w0, [x1, 44]
	strb	w5, [x1, 56]
	strb	w4, [x1, 57]
	ccmp	w0, w2, 0, eq
	cset	w0, ne
	orr	w0, w3, w0
	cbnz	w0, L149
	ldp	x29, x30, [sp], 16
LCFI42:
	ret
L144:
LCFI43:
	adrp	x0, lC15@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC15@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L149:
	adrp	x0, lC16@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC16@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LFE12:
	.const
	.align	3
lC17:
	.ascii "Static_Predicate failed at anubis_address_base32.adb:292"
	.align	3
lC18:
	.ascii "Static_Predicate failed at anubis_address_base32.adb:310"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address_base32__unchunk_payload
_anubis_address_base32__unchunk_payload:
LFB14:
	mov	x7, 46847
	mov	w8, 0
	stp	x29, x30, [sp, -16]!
LCFI44:
	mov	x5, x0
	mov	x6, x1
	movk	x7, 0x3ef, lsl 16
	mov	x29, sp
LCFI45:
	.p2align 5,,15
L151:
	mov	x2, 0
	b	L156
	.p2align 2,,3
L152:
	sub	w3, w4, #97
	and	w3, w3, 255
	cmp	w3, 25
	bhi	L154
	lsr	x3, x7, x3
	tbz	x3, 0, L154
L153:
	strb	w4, [x6, x2]
	add	x2, x2, 1
	cmp	x2, 8
	beq	L172
L156:
	ldrsb	w4, [x5, x2]
	cmp	w4, 57
	bgt	L152
	cmp	w4, 47
	bgt	L153
L154:
	adrp	x0, lC17@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L172:
	ldrsb	w2, [x5, 8]
	cmp	w2, 45
	bne	L165
	add	w8, w8, 9
	cmp	w8, 54
	beq	L173
	add	x5, x5, 9
	add	x6, x6, 8
	b	L151
	.p2align 2,,3
L173:
	mov	x5, 46847
	mov	x2, 0
	add	x4, x0, 54
	add	x3, x1, 48
	movk	x5, 0x3ef, lsl 16
L164:
	ldrsb	w1, [x4, x2]
	cmp	w1, 57
	ble	L174
	sub	w0, w1, #97
	and	w0, w0, 255
	cmp	w0, 25
	bhi	L162
	lsr	x0, x5, x0
	tbz	x0, 0, L162
L161:
	strb	w1, [x3, x2]
	add	x2, x2, 1
	cmp	x2, 4
	bne	L164
	mov	w0, 1
L158:
	ldp	x29, x30, [sp], 16
LCFI46:
	ret
	.p2align 2,,3
L174:
LCFI47:
	cmp	w1, 47
	bgt	L161
L162:
	adrp	x0, lC18@PAGE
	adrp	x1, lC4@PAGE
	add	x0, x0, lC18@PAGEOFF;
	add	x1, x1, lC4@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L165:
	mov	w0, 0
	b	L158
LFE14:
	.const
	.align	2
lC4:
	.word	1
	.word	56
	.text
	.const
_CSWTCH.75:
	.byte	0
	.byte	1
	.byte	2
	.byte	3
	.byte	4
	.byte	5
	.byte	6
	.byte	7
	.byte	8
	.byte	9
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	10
	.byte	11
	.byte	12
	.byte	13
	.byte	14
	.byte	15
	.byte	16
	.byte	17
	.byte	-1
	.byte	18
	.byte	19
	.byte	-1
	.byte	20
	.byte	21
	.byte	-1
	.byte	22
	.byte	23
	.byte	24
	.byte	25
	.byte	26
	.byte	-1
	.byte	27
	.byte	28
	.byte	29
	.byte	30
	.byte	31
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	-1
	.byte	10
	.byte	11
	.byte	12
	.byte	13
	.byte	14
	.byte	15
	.byte	16
	.byte	17
	.byte	-1
	.byte	18
	.byte	19
	.byte	-1
	.byte	20
	.byte	21
	.byte	-1
	.byte	22
	.byte	23
	.byte	24
	.byte	25
	.byte	26
	.byte	-1
	.byte	27
	.byte	28
	.byte	29
	.byte	30
	.byte	31
	.globl _anubis_address_base32__encode_table
	.align	3
_anubis_address_base32__encode_table:
	.ascii "0123456789abcdefghjkmnpqrstvwxyz"
	.globl _anubis_address_base32_E
	.data
	.align	1
_anubis_address_base32_E:
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
	.quad	LFB30-.
	.set L$set$2,LFE30-LFB30
	.quad L$set$2
	.uleb128 0
	.byte	0x4
	.set L$set$3,LCFI0-LFB30
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
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$5,LEFDE3-LASFDE3
	.long L$set$5
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB31-.
	.set L$set$6,LFE31-LFB31
	.quad L$set$6
	.uleb128 0
	.byte	0x4
	.set L$set$7,LCFI2-LFB31
	.long L$set$7
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$8,LCFI3-LCFI2
	.long L$set$8
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$9,LEFDE5-LASFDE5
	.long L$set$9
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB3-.
	.set L$set$10,LFE3-LFB3
	.quad L$set$10
	.uleb128 0
	.byte	0x4
	.set L$set$11,LCFI4-LFB3
	.long L$set$11
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI6-LCFI5
	.long L$set$13
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$14,LCFI7-LCFI6
	.long L$set$14
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$15,LEFDE7-LASFDE7
	.long L$set$15
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB2-.
	.set L$set$16,LFE2-LFB2
	.quad L$set$16
	.uleb128 0
	.byte	0x4
	.set L$set$17,LCFI8-LFB2
	.long L$set$17
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$18,LCFI9-LCFI8
	.long L$set$18
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$19,LCFI10-LCFI9
	.long L$set$19
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$20,LCFI11-LCFI10
	.long L$set$20
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$21,LEFDE9-LASFDE9
	.long L$set$21
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB4-.
	.set L$set$22,LFE4-LFB4
	.quad L$set$22
	.uleb128 0
	.byte	0x4
	.set L$set$23,LCFI12-LFB4
	.long L$set$23
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$24,LCFI13-LCFI12
	.long L$set$24
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$25,LEFDE11-LASFDE11
	.long L$set$25
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB6-.
	.set L$set$26,LFE6-LFB6
	.quad L$set$26
	.uleb128 0
	.byte	0x4
	.set L$set$27,LCFI14-LFB6
	.long L$set$27
	.byte	0xe
	.uleb128 0xc0
	.byte	0x9d
	.uleb128 0x18
	.byte	0x9e
	.uleb128 0x17
	.byte	0x4
	.set L$set$28,LCFI15-LCFI14
	.long L$set$28
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$29,LCFI16-LCFI15
	.long L$set$29
	.byte	0x97
	.uleb128 0x12
	.byte	0x98
	.uleb128 0x11
	.byte	0x4
	.set L$set$30,LCFI17-LCFI16
	.long L$set$30
	.byte	0x93
	.uleb128 0x16
	.byte	0x94
	.uleb128 0x15
	.byte	0x4
	.set L$set$31,LCFI18-LCFI17
	.long L$set$31
	.byte	0x95
	.uleb128 0x14
	.byte	0x96
	.uleb128 0x13
	.byte	0x4
	.set L$set$32,LCFI19-LCFI18
	.long L$set$32
	.byte	0x99
	.uleb128 0x10
	.byte	0x9a
	.uleb128 0xf
	.byte	0x4
	.set L$set$33,LCFI20-LCFI19
	.long L$set$33
	.byte	0x9b
	.uleb128 0xe
	.byte	0x9c
	.uleb128 0xd
	.byte	0x4
	.set L$set$34,LCFI21-LCFI20
	.long L$set$34
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
	.set L$set$35,LCFI22-LCFI21
	.long L$set$35
	.byte	0xb
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$36,LEFDE13-LASFDE13
	.long L$set$36
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB8-.
	.set L$set$37,LFE8-LFB8
	.quad L$set$37
	.uleb128 0
	.byte	0x4
	.set L$set$38,LCFI23-LFB8
	.long L$set$38
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$39,LCFI24-LCFI23
	.long L$set$39
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$40,LCFI25-LCFI24
	.long L$set$40
	.byte	0x93
	.uleb128 0x12
	.byte	0x4
	.set L$set$41,LCFI26-LCFI25
	.long L$set$41
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$42,LCFI27-LCFI26
	.long L$set$42
	.byte	0xb
	.align	3
LEFDE13:
LSFDE15:
	.set L$set$43,LEFDE15-LASFDE15
	.long L$set$43
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB10-.
	.set L$set$44,LFE10-LFB10
	.quad L$set$44
	.uleb128 0
	.byte	0x4
	.set L$set$45,LCFI28-LFB10
	.long L$set$45
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$46,LCFI29-LCFI28
	.long L$set$46
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$47,LCFI30-LCFI29
	.long L$set$47
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$48,LCFI31-LCFI30
	.long L$set$48
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
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
	.quad	LFB11-.
	.set L$set$52,LFE11-LFB11
	.quad L$set$52
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI34-LFB11
	.long L$set$53
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
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
	.quad	LFB12-.
	.set L$set$60,LFE12-LFB12
	.quad L$set$60
	.uleb128 0
	.byte	0x4
	.set L$set$61,LCFI40-LFB12
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
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$65,LEFDE21-LASFDE21
	.long L$set$65
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB14-.
	.set L$set$66,LFE14-LFB14
	.quad L$set$66
	.uleb128 0
	.byte	0x4
	.set L$set$67,LCFI44-LFB14
	.long L$set$67
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$68,LCFI45-LCFI44
	.long L$set$68
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$69,LCFI46-LCFI45
	.long L$set$69
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$70,LCFI47-LCFI46
	.long L$set$70
	.byte	0xb
	.align	3
LEFDE21:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
