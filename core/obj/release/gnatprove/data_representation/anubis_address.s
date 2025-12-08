	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.const
	.align	3
lC4:
	.ascii "anubis_address.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address__create_address
_anubis_address__create_address:
LFB2:
	stp	x29, x30, [sp, -96]!
LCFI0:
	mov	x29, sp
LCFI1:
	stp	x19, x20, [sp, 16]
LCFI2:
	mov	w19, w1
	stp	x21, x22, [sp, 32]
LCFI3:
	cmp	w1, 3
	bhi	L6
	mov	w21, w0
	mov	x1, x2
	mov	w0, w19
	add	x2, x29, 58
	mov	x20, x8
	add	x22, x29, 56
	bl	_anubis_address_derive__derive_account_id
	cmp	w21, 4
	bhi	L7
	strb	w21, [x29, 56]
	mov	w0, 1
	strb	w19, [x29, 57]
	ldp	q31, q30, [x22]
	strb	w0, [x29, 90]
	ldr	w0, [x22, 31]
	stp	q31, q30, [x20]
	str	w0, [x20, 31]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 96
LCFI4:
	ret
L6:
LCFI5:
	adrp	x0, lC4@PAGE
	mov	w1, 19
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L7:
	adrp	x0, lC4@PAGE
	mov	w1, 22
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE2:
	.const
	.align	3
lC5:
	.ascii "anubis_address.ads"
	.space 1
	.align	3
lC6:
	.ascii "failed precondition from anubis_address.ads:61"
	.align	3
lC7:
	.ascii "Loop_Invariant failed at anubis_address.adb:80"
	.align	3
lC8:
	.ascii "Loop_Invariant failed at anubis_address.adb:81"
	.align	3
lC9:
	.ascii "Loop_Invariant failed at anubis_address.adb:100"
	.align	3
lC10:
	.ascii "anubis_address.adb:106"
	.align	3
lC11:
	.ascii "anubis_address.adb:112"
	.align	3
lC12:
	.ascii "Loop_Invariant failed at anubis_address.adb:116"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address__format_address
_anubis_address__format_address:
LFB4:
	stp	x29, x30, [sp, -288]!
LCFI6:
	mov	x29, sp
LCFI7:
LEHB0:
LEHE0:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI8:
	mov	x21, x0
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	ldrb	w0, [x0, 34]
	str	x27, [sp, 80]
LCFI9:
	cmp	w0, 1
	bhi	L36
	cbz	w0, L37
	add	x20, x29, 136
	add	x0, x29, 288
	mov	x8, x20
	mov	x25, x1
	str	x0, [x29, 160]
LEHB1:
	bl	_system__secondary_stack__ss_mark
	ldrb	w0, [x21]
	cmp	w0, 4
	bhi	L38
	bl	_anubis_address_types__network_string
	ldp	w19, w23, [x1]
	mov	x26, x0
	and	w0, w23, w23, asr #31
	cmp	w19, w0
	ble	L39
	ldrb	w0, [x21, 1]
	cmp	w0, 3
	bhi	L40
	bl	_anubis_address_types__entity_char
LEHE1:
	movi	v31.16b, 0x20
	add	x27, x29, 168
	mov	w22, w0
	mov	x1, x27
	add	x0, x21, 2
LEHB2:
	stp	q31, q31, [x25]
	stp	q31, q31, [x25, 32]
	str	q31, [x25, 64]
	str	h31, [x25, 80]
LEHE2:
LEHB3:
	bl	_anubis_address_base32__encode_account_id
	ldrb	w0, [x21]
	cmp	w0, 4
	bhi	L41
	ldrb	w1, [x21, 1]
	cmp	w1, 3
	bhi	L42
	mov	x2, x27
	bl	_anubis_address_checksum__compute_checksum
	sxtw	x0, w0
	ubfx	x2, x0, 8, 8
	ubfx	x1, x0, 16, 8
	strb	w0, [x29, 104]
	strb	w2, [x29, 105]
	ldrh	w0, [x29, 104]
	strb	w1, [x29, 114]
	strb	w1, [x29, 106]
	strh	w0, [x29, 112]
	ldr	x0, [x29, 112]
	bl	_anubis_address_base32__encode_checksum
	ubfx	x1, x0, 8, 8
	lsr	w3, w0, 24
	strb	w0, [x29, 120]
	ubfx	x4, x0, 16, 8
	ubfx	x2, x0, 32, 8
	strb	w1, [x29, 121]
	add	x21, x29, 224
	mov	x0, x27
	strb	w4, [x29, 122]
	mov	x1, x21
	add	x24, x29, 128
	strb	w3, [x29, 123]
	ldr	w3, [x29, 120]
	strb	w2, [x24, 4]
	strb	w2, [x29, 124]
	str	w3, [x29, 128]
	bl	_anubis_address_base32__chunk_payload
LEHE3:
	adrp	x0, lC13@PAGE
	ldr	d31, [x0, #lC13@PAGEOFF]
LEHB4:
	str	d31, [x25]
LEHE4:
	cmp	w19, w23
	ble	L43
	mov	x0, 9
	mov	w2, w0
L16:
	add	x0, x25, x0
	mov	w1, 58
LEHB5:
	strb	w1, [x0, -1]
	add	w0, w2, 1
	add	x0, x25, w0, sxtw
	strb	w22, [x0, -1]
	add	w0, w2, 2
	add	x0, x25, w0, sxtw
	strb	w1, [x0, -1]
	sxtw	x5, w2
	add	w0, w2, 3
	sub	x3, x21, x5
	sxtw	x0, w0
	add	x5, x5, 61
	sub	x3, x3, #3
	sub	x4, x25, #1
	b	L22
	.p2align 2,,3
L29:
	mov	x0, x2
L22:
	cmp	w0, 78
	ccmp	w0, 14, 4, ne
	ble	L44
	ldrsb	w2, [x3, x0]
	strb	w2, [x4, x0]
	add	x2, x0, 1
	cmp	x2, x5
	bne	L29
	add	w1, w0, 1
	mov	w2, 78
	cmp	w1, 72
	ccmp	w1, w2, 4, gt
	beq	L45
	mov	w2, 45
	add	x1, x25, w1, sxtw
	strb	w2, [x1, -1]
	add	w1, w0, 2
	mov	w2, 79
	cmp	w1, 73
	ccmp	w1, w2, 4, gt
	beq	L46
	sxtw	x0, w0
	sxtw	x1, w1
	sub	x24, x24, x0
	add	x19, x0, 6
	sub	x0, x24, #2
L25:
	ldrsb	w2, [x0, x1]
	strb	w2, [x4, x1]
	cmp	x19, x1
	beq	L47
	add	x1, x1, 1
	cmp	w1, 83
	bne	L25
	adrp	x0, lC12@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC12@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
	.p2align 2,,3
L43:
	sxtw	x3, w19
	sxtw	x5, w23
	sub	x1, x25, x3
	sub	x0, x26, x3
	add	x1, x1, 8
	sub	x3, x3, #1
	mov	w2, 9
	mov	w6, 2147483640
	.p2align 5,,15
L20:
	add	x3, x3, 1
	cmp	w3, w6
	bge	L48
	add	w4, w3, 8
	cmp	w2, w4
	bne	L49
	cmp	w2, 17
	beq	L50
	ldrsb	w4, [x0, x3]
	strb	w4, [x1, x3]
LEHE5:
	add	w2, w2, 1
	cmp	x3, x5
	bne	L20
	sxtw	x0, w2
	b	L16
	.p2align 2,,3
L47:
	mov	x0, x20
LEHB6:
	bl	_system__secondary_stack__ss_release
	mov	w0, w19
	ldr	x27, [sp, 80]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
LEHE6:
	ldp	x29, x30, [sp], 288
LCFI10:
	ret
L44:
LCFI11:
	adrp	x0, lC9@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC9@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
LEHB7:
	bl	_system__assertions__raise_assert_failure
L48:
	adrp	x0, lC4@PAGE
	mov	w1, 80
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L49:
	adrp	x0, lC7@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC7@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L50:
	adrp	x0, lC8@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC8@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L42:
	adrp	x0, lC4@PAGE
	mov	w1, 57
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L41:
	adrp	x0, lC4@PAGE
	mov	w1, 57
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE7:
L36:
	adrp	x0, lC5@PAGE
	mov	w1, 61
	add	x0, x0, lC5@PAGEOFF;
LEHB8:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE8:
L40:
	adrp	x0, lC4@PAGE
	mov	w1, 41
	add	x0, x0, lC4@PAGEOFF;
LEHB9:
	bl	___gnat_rcheck_CE_Invalid_Data
L39:
	adrp	x0, lC4@PAGE
	mov	w1, 40
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L38:
	adrp	x0, lC4@PAGE
	mov	w1, 40
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE9:
L37:
	adrp	x0, lC6@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC6@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
LEHB10:
	bl	_system__assertions__raise_assert_failure
LEHE10:
L45:
	adrp	x0, lC10@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
LEHB11:
	bl	_system__assertions__raise_assert_failure
L46:
	adrp	x0, lC11@PAGE
	adrp	x1, lC2@PAGE
	add	x0, x0, lC11@PAGEOFF;
	add	x1, x1, lC2@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
LEHE11:
L30:
	mov	x19, x0
	mov	x0, x20
LEHB12:
	bl	_system__secondary_stack__ss_release
	mov	x0, x19
	bl	__Unwind_Resume
LEHE12:
LFE4:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
LLSDA4:
	.byte	0xff
	.byte	0xff
	.byte	0x1
	.uleb128 LLSDACSE4-LLSDACSB4
LLSDACSB4:
	.uleb128 LEHB0-LFB4
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB4
	.uleb128 LEHE1-LEHB1
	.uleb128 L30-LFB4
	.uleb128 0
	.uleb128 LEHB2-LFB4
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB3-LFB4
	.uleb128 LEHE3-LEHB3
	.uleb128 L30-LFB4
	.uleb128 0
	.uleb128 LEHB4-LFB4
	.uleb128 LEHE4-LEHB4
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB5-LFB4
	.uleb128 LEHE5-LEHB5
	.uleb128 L30-LFB4
	.uleb128 0
	.uleb128 LEHB6-LFB4
	.uleb128 LEHE6-LEHB6
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB7-LFB4
	.uleb128 LEHE7-LEHB7
	.uleb128 L30-LFB4
	.uleb128 0
	.uleb128 LEHB8-LFB4
	.uleb128 LEHE8-LEHB8
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB9-LFB4
	.uleb128 LEHE9-LEHB9
	.uleb128 L30-LFB4
	.uleb128 0
	.uleb128 LEHB10-LFB4
	.uleb128 LEHE10-LEHB10
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB11-LFB4
	.uleb128 LEHE11-LEHB11
	.uleb128 L30-LFB4
	.uleb128 0
	.uleb128 LEHB12-LFB4
	.uleb128 LEHE12-LEHB12
	.uleb128 0
	.uleb128 0
LLSDACSE4:
	.text
	.const
	.align	2
lC0:
	.word	1
	.word	47
	.align	2
lC1:
	.word	1
	.word	46
	.align	2
lC2:
	.word	1
	.word	22
	.text
	.const
	.align	3
lC14:
	.ascii "failed precondition from anubis_address.ads:72"
	.align	3
lC16:
	.ascii "mldsa87"
	.align	3
lC17:
	.ascii "Loop_Invariant failed at anubis_address.adb:194"
	.align	3
lC18:
	.ascii "dev"
	.align	3
lC19:
	.ascii "lab"
	.align	3
lC20:
	.ascii "staging"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address__parse_address
_anubis_address__parse_address:
LFB7:
	stp	x29, x30, [sp, -288]!
LCFI12:
	mov	x29, sp
LCFI13:
	mov	w3, 65434
	movk	w3, 0x7fff, lsl 16
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
	stp	x23, x24, [sp, 48]
	stp	x25, x26, [sp, 64]
	stp	x27, x28, [sp, 80]
LCFI14:
	ldp	w27, w5, [x1]
	cmp	w27, 0
	ccmp	w5, w3, 0, gt
	bgt	L131
	add	x23, x2, 2
	mov	x21, x1
	strb	wzr, [x2]
	adrp	x1, lC15@PAGE
	strb	wzr, [x2, 1]
	mov	x20, x2
	add	x1, x1, lC15@PAGEOFF;
	stp	xzr, xzr, [x23]
	add	x25, x29, 144
	sxtw	x24, w27
	mov	x22, x0
	add	x0, x24, 76
	stp	xzr, xzr, [x23, 16]
	strb	wzr, [x2, 34]
	ldr	w2, [x1]
	ldrb	w1, [x1, 4]
	str	w2, [x29, 144]
	strb	w1, [x25, 4]
	cmp	x0, w5, sxtw
	bge	L51
	mov	x3, x22
	mov	w19, w27
	cmp	w27, w5
	ble	L57
	b	L51
	.p2align 2,,3
L133:
	add	w19, w19, 1
	cmp	w5, w19
	blt	L51
	add	x3, x3, 1
	cmp	w27, w19
	bgt	L132
L57:
	ldrsb	w4, [x3]
	cmp	w4, 58
	bne	L133
	cmp	w5, w19
	blt	L51
	sub	w26, w19, #1
	mov	x28, sp
	cmp	w27, w26
	bgt	L61
	mov	x2, 1
	mov	x1, x22
	sub	x2, x2, x24
	add	x2, x2, w26, sxtw
	add	x0, x2, 15
	and	x0, x0, -16
	sub	w26, w26, w27
	sub	sp, sp, x0
	mov	x0, sp
	bl	_memcpy
	cmp	w26, 6
	bne	L61
	ldr	w1, [sp]
	mov	w0, 27757
	movk	w0, 0x7364, lsl 16
	cmp	w1, w0
	beq	L134
L61:
	mov	sp, x28
L51:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x25, x26, [sp, 64]
	ldp	x27, x28, [sp, 80]
	ldp	x29, x30, [sp], 288
LCFI15:
	ret
L134:
LCFI16:
	ldrh	w1, [sp, 4]
	mov	w0, 14433
	cmp	w1, w0
	bne	L61
	ldrb	w0, [sp, 6]
	cmp	w0, 55
	bne	L61
	ldr	w6, [x21, 4]
	add	w26, w19, 1
	mov	sp, x28
	cmp	w26, w6
	bgt	L51
	sxtw	x9, w26
	ldr	w7, [x21]
	sub	x0, x22, x24
	mov	x3, x9
	mov	w2, 2147483647
	b	L67
	.p2align 2,,3
L138:
	cmp	w3, w2
	beq	L135
	add	x3, x3, 1
	cmp	w6, w3
	blt	L136
L67:
	mov	w4, w3
	cmp	w7, w3
	bgt	L137
	ldrsb	w1, [x0, x3]
	cmp	w1, 58
	bne	L138
L68:
	cmp	w3, w6
	bgt	L51
	mov	w0, 2147483647
	cmp	w3, w0
	beq	L139
	add	w27, w3, 1
	cmp	w27, w0
	beq	L140
	add	w0, w3, 2
	cmp	w0, w6
	bgt	L51
	cmp	w7, w0
	bgt	L141
	sxtw	x0, w0
	sub	x0, x0, x24
	ldrsb	w0, [x22, x0]
	cmp	w0, 58
	bne	L51
	mov	w0, 2147483646
	cmp	w27, w0
	beq	L142
	sub	w0, w0, #57
	add	w1, w3, 3
	cmp	w1, w0
	bgt	L143
	add	w0, w3, 60
	cmp	w0, w6
	bgt	L51
	sxtw	x10, w3
	mov	x2, -1
	sub	x10, x10, x24
	add	x28, x29, 224
	add	x10, x22, x10
	add	x5, x10, 4
	.p2align 5,,15
L75:
	cmp	w1, w6
	ccmp	w7, w1, 0, le
	bgt	L144
	ldrsb	w4, [x5, x2]
	add	x0, x28, x2
	add	x2, x2, 1
	add	w1, w1, 1
	strb	w4, [x0, 1]
	cmp	x2, 57
	bne	L75
	add	w0, w3, 61
	cmp	w0, w6
	bgt	L51
	cmp	w7, w0
	bgt	L145
	sxtw	x1, w0
	sub	x1, x1, x24
	ldrsb	w1, [x22, x1]
	cmp	w1, 45
	bne	L51
	mov	w1, 2147483647
	cmp	w0, w1
	beq	L146
	add	w0, w3, 62
	mov	w1, 2147483644
	cmp	w0, w1
	bge	L147
	add	w1, w3, 66
	cmp	w1, w6
	bgt	L51
	add	x10, x10, 63
	mov	x1, -1
L80:
	cmp	w7, w0
	ccmp	w6, w0, 1, le
	blt	L148
	ldrsb	w4, [x10, x1]
	add	x2, x25, x1
	add	x1, x1, 1
	add	w0, w0, 1
	strb	w4, [x2, 1]
	cmp	x1, 4
	bne	L80
	mov	x5, 46847
	mov	x4, -1
	movk	x5, 0x3ef, lsl 16
	add	x2, x29, 160
L85:
	add	x0, x25, x4
	ldrsb	w1, [x0, 1]
	cmp	w1, 57
	ble	L149
	sub	w0, w1, #97
	and	w0, w0, 255
	cmp	w0, 25
	bhi	L51
	lsr	x0, x5, x0
	tbz	x0, 0, L51
L82:
	add	x0, x4, x2
	add	x4, x4, 1
	strb	w1, [x0, 1]
	cmp	x4, 4
	bne	L85
	mov	x0, sp
	sub	w25, w3, #1
	str	x0, [x29, 112]
	cmp	w26, w25
	bgt	L86
	cmp	w7, w26
	bgt	L150
	sxtw	x2, w25
	sub	x2, x2, w19, sxtw
	add	x0, x2, 15
	and	x0, x0, -16
	sub	x1, x9, x24
	sub	sp, sp, x0
	add	x1, x22, x1
	str	x4, [x29, 104]
	mov	x0, sp
	sub	w25, w25, w26
	bl	_memcpy
	cmp	w25, 3
	ldr	x4, [x29, 104]
	bne	L88
	ldr	w0, [sp]
	mov	w1, 24941
	movk	w1, 0x6e69, lsl 16
	cmp	w0, w1
	beq	L151
	mov	w1, 25972
	movk	w1, 0x7473, lsl 16
	cmp	w0, w1
	beq	L152
L86:
	ldr	x0, [x29, 112]
	mov	sp, x0
	b	L51
	.p2align 2,,3
L136:
	add	w3, w4, 1
	b	L68
L149:
	cmp	w1, 47
	ble	L51
	b	L82
L132:
	adrp	x0, lC4@PAGE
	mov	w1, 168
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L88:
	cmp	w25, 2
	beq	L153
	str	x4, [x29, 104]
	cmp	w25, 6
	bne	L86
	adrp	x1, lC20@PAGE
	mov	x2, 7
	add	x1, x1, lC20@PAGEOFF;
	mov	x0, sp
	bl	_memcmp
	ldr	x4, [x29, 104]
	cbnz	w0, L86
L89:
	strb	w4, [x20]
	ldr	x0, [x29, 112]
	mov	sp, x0
	ldr	w0, [x21]
	cmp	w0, w27
	bgt	L93
	ldr	w0, [x21, 4]
	cmp	w0, w27
	blt	L93
	sxtw	x0, w27
	sub	x0, x0, x24
	ldrsb	w0, [x22, x0]
	cmp	w0, 117
	beq	L107
	bgt	L96
	cmp	w0, 99
	beq	L108
	cmp	w0, 115
	bne	L51
	mov	w1, 3
L95:
	add	x19, x29, 168
	mov	x0, x28
	strb	w1, [x20, 1]
	mov	x1, x19
	bl	_anubis_address_base32__unchunk_payload
	cmp	w0, 1
	bhi	L154
	cbz	w0, L51
	mov	x1, x23
	mov	x0, x19
	bl	_anubis_address_base32__decode_account_id
	cmp	w0, 1
	bhi	L155
	cbz	w0, L51
	ldr	x0, [x29, 160]
	bl	_anubis_address_base32__decode_checksum
	ubfx	x2, x0, 16, 8
	lsr	w1, w0, 24
	str	x0, [x29, 120]
	strh	w0, [x29, 136]
	strb	w2, [x29, 138]
	cmp	w1, 1
	bhi	L156
	cbz	w1, L51
	ldrb	w0, [x20]
	cmp	w0, 4
	bhi	L101
	ldrb	w1, [x20, 1]
	cmp	w1, 3
	bhi	L101
	ldr	x3, [x29, 136]
	mov	x2, x19
	bl	_anubis_address_checksum__verify_checksum
	cmp	w0, 1
	bhi	L101
	cbz	w0, L51
	mov	w0, 1
	strb	w0, [x20, 34]
	b	L51
L137:
	adrp	x0, lC4@PAGE
	mov	w1, 192
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L135:
	adrp	x0, lC17@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC17@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L144:
	adrp	x0, lC4@PAGE
	mov	w1, 220
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L96:
	cmp	w0, 118
	bne	L51
	mov	w1, 2
	b	L95
L153:
	adrp	x1, lC18@PAGE
	mov	x2, 3
	add	x1, x1, lC18@PAGEOFF;
	mov	x0, sp
	bl	_memcmp
	cbnz	w0, L157
	mov	w4, w25
	b	L89
L157:
	adrp	x1, lC19@PAGE
	mov	x2, 3
	add	x1, x1, lC19@PAGEOFF;
	mov	x0, sp
	bl	_memcmp
	cbnz	w0, L86
	mov	w4, 3
	b	L89
L148:
	adrp	x0, lC4@PAGE
	mov	w1, 240
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L145:
	adrp	x0, lC4@PAGE
	mov	w1, 226
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L143:
	adrp	x0, lC4@PAGE
	mov	w1, 215
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L142:
	adrp	x0, lC4@PAGE
	mov	w1, 211
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L107:
	mov	w1, 0
	b	L95
L93:
	adrp	x0, lC4@PAGE
	mov	w1, 272
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L147:
	adrp	x0, lC4@PAGE
	mov	w1, 234
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L146:
	adrp	x0, lC4@PAGE
	mov	w1, 230
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L156:
	adrp	x0, lC4@PAGE
	mov	w1, 293
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L155:
	adrp	x0, lC4@PAGE
	mov	w1, 287
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L154:
	adrp	x0, lC4@PAGE
	mov	w1, 282
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L108:
	mov	w1, 1
	b	L95
L131:
	adrp	x0, lC14@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC14@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L150:
	adrp	x0, lC4@PAGE
	mov	w1, 254
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L152:
	mov	w4, 1
	b	L89
L151:
	mov	w4, 0
	b	L89
L141:
	adrp	x0, lC4@PAGE
	mov	w1, 207
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L140:
	adrp	x0, lC4@PAGE
	mov	w1, 207
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L139:
	adrp	x0, lC4@PAGE
	mov	w1, 203
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L101:
	adrp	x0, lC4@PAGE
	mov	w1, 298
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE7:
	.const
	.align	3
lC15:
	.ascii "     "
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address__validate_address
_anubis_address__validate_address:
LFB8:
	stp	x29, x30, [sp, -112]!
LCFI17:
	mov	x29, sp
LCFI18:
	stp	x19, x20, [sp, 16]
LCFI19:
	mov	x19, x0
	ldrb	w0, [x0, 34]
	cmp	w0, 1
	bhi	L169
	cbnz	w0, L170
L160:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI20:
	ret
	.p2align 2,,3
L170:
LCFI21:
	add	x20, x29, 56
	add	x0, x19, 2
	mov	x1, x20
	bl	_anubis_address_base32__encode_account_id
	ldrb	w0, [x19]
	cmp	w0, 4
	bhi	L162
	ldrb	w1, [x19, 1]
	cmp	w1, 3
	bhi	L162
	mov	x2, x20
	bl	_anubis_address_checksum__compute_checksum
	sxtw	x1, w0
	ldrb	w0, [x19]
	ubfx	x3, x1, 8, 8
	ubfx	x2, x1, 16, 8
	strb	w1, [x29, 40]
	strb	w3, [x29, 41]
	ldrh	w1, [x29, 40]
	strb	w2, [x29, 50]
	strh	w1, [x29, 48]
	cmp	w0, 4
	bhi	L164
	ldrb	w1, [x19, 1]
	cmp	w1, 3
	bhi	L164
	ldr	x3, [x29, 48]
	mov	x2, x20
	bl	_anubis_address_checksum__verify_checksum
	cmp	w0, 1
	bls	L160
L164:
	adrp	x0, lC4@PAGE
	mov	w1, 324
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L169:
	adrp	x0, lC4@PAGE
	mov	w1, 316
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L162:
	adrp	x0, lC4@PAGE
	mov	w1, 322
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE8:
	.const
	.align	3
lC21:
	.ascii "failed precondition from anubis_address.ads:87"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address__get_chunked_payload
_anubis_address__get_chunked_payload:
LFB9:
	stp	x29, x30, [sp, -192]!
LCFI22:
	mov	x29, sp
LCFI23:
	stp	x19, x20, [sp, 16]
LCFI24:
	mov	x20, x0
	stp	x21, x22, [sp, 32]
	ldrb	w0, [x0, 34]
	str	x23, [sp, 48]
LCFI25:
	ldp	w4, w3, [x2]
	cmp	w0, 1
	bhi	L184
	sxtw	x23, w4
	mov	x21, x2
	mov	x19, x1
	sxtw	x2, w3
	add	x1, x23, 56
	eor	w0, w0, 1
	cmp	x1, x2
	cset	w1, ge
	orr	w0, w1, w0
	tbnz	x0, 0, L185
	cmp	w4, w3
	mov	w1, 32
	mov	x0, x19
	add	x22, x29, 72
	sub	x2, x2, x23
	csinc	x2, xzr, x2, gt
	bl	_memset
	add	x0, x20, 2
	mov	x1, x22
	bl	_anubis_address_base32__encode_account_id
	add	x20, x29, 128
	mov	x0, x22
	mov	x1, x20
	bl	_anubis_address_base32__chunk_payload
	ldp	w2, w1, [x21]
	mov	x3, -1
	sxtw	x0, w2
	sub	x0, x0, x23
	add	x0, x0, 1
	add	x4, x19, x0
	.p2align 5,,15
L180:
	add	w0, w3, 1
	adds	w0, w0, w2
	bvs	L176
	cmp	w2, w0
	bgt	L179
	cmp	w0, w1
	bgt	L179
	add	x0, x20, x3
	ldrsb	w0, [x0, 1]
	strb	w0, [x4, x3]
	add	x3, x3, 1
	cmp	x3, 57
	bne	L180
	mov	w0, 58
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 192
LCFI26:
	ret
L179:
LCFI27:
	adrp	x0, lC4@PAGE
	mov	w1, 344
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L176:
	adrp	x0, lC4@PAGE
	mov	w1, 344
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L185:
	adrp	x0, lC21@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC21@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L184:
	adrp	x0, lC5@PAGE
	mov	w1, 87
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE9:
	.const
	.align	3
lC22:
	.ascii "failed precondition from anubis_address.ads:97"
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_address__get_checksum_string
_anubis_address__get_checksum_string:
LFB11:
	stp	x29, x30, [sp, -160]!
LCFI28:
	mov	x29, sp
LCFI29:
	stp	x19, x20, [sp, 16]
	stp	x21, x22, [sp, 32]
LCFI30:
	mov	x21, x0
	ldrb	w0, [x0, 34]
	str	x23, [sp, 48]
LCFI31:
	ldp	w4, w3, [x2]
	cmp	w0, 1
	bhi	L201
	sxtw	x22, w4
	mov	x20, x2
	mov	x19, x1
	sxtw	x2, w3
	add	x1, x22, 3
	eor	w0, w0, 1
	cmp	x1, x2
	cset	w1, ge
	orr	w0, w1, w0
	tbnz	x0, 0, L202
	cmp	w4, w3
	mov	w1, 32
	mov	x0, x19
	sub	x2, x2, x22
	csinc	x2, xzr, x2, gt
	add	x23, x29, 104
	bl	_memset
	add	x0, x21, 2
	mov	x1, x23
	bl	_anubis_address_base32__encode_account_id
	ldrb	w0, [x21]
	cmp	w0, 4
	bhi	L191
	ldrb	w1, [x21, 1]
	cmp	w1, 3
	bhi	L191
	mov	x2, x23
	bl	_anubis_address_checksum__compute_checksum
	sxtw	x0, w0
	ubfx	x2, x0, 8, 8
	ubfx	x1, x0, 16, 8
	strb	w0, [x29, 72]
	strb	w2, [x29, 73]
	ldrh	w0, [x29, 72]
	strb	w1, [x29, 82]
	strb	w1, [x29, 74]
	strh	w0, [x29, 80]
	ldr	x0, [x29, 80]
	bl	_anubis_address_base32__encode_checksum
	ldp	w4, w6, [x20]
	ubfx	x1, x0, 8, 8
	lsr	w2, w0, 24
	ubfx	x7, x0, 16, 8
	strb	w0, [x29, 88]
	add	x5, x29, 96
	strb	w1, [x29, 89]
	ubfx	x0, x0, 32, 8
	mov	x3, -1
	strb	w7, [x29, 90]
	strb	w2, [x29, 91]
	sxtw	x1, w4
	ldr	w2, [x29, 88]
	sub	x1, x1, x22
	strb	w0, [x5, 4]
	add	x1, x1, 1
	strb	w0, [x29, 92]
	add	x1, x19, x1
	str	w2, [x29, 96]
L197:
	add	w2, w3, 1
	adds	w2, w2, w4
	bvs	L193
	cmp	w4, w2
	bgt	L196
	cmp	w2, w6
	bgt	L196
	add	x2, x5, x3
	ldrsb	w2, [x2, 1]
	strb	w2, [x1, x3]
	add	x3, x3, 1
	cmp	x3, 4
	bne	L197
	mov	w0, 5
	ldr	x23, [sp, 48]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 160
LCFI32:
	ret
L196:
LCFI33:
	adrp	x0, lC4@PAGE
	mov	w1, 369
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L193:
	adrp	x0, lC4@PAGE
	mov	w1, 369
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L191:
	adrp	x0, lC4@PAGE
	mov	w1, 365
	add	x0, x0, lC4@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L202:
	adrp	x0, lC22@PAGE
	adrp	x1, lC1@PAGE
	add	x0, x0, lC22@PAGEOFF;
	add	x1, x1, lC1@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L201:
	adrp	x0, lC5@PAGE
	mov	w1, 97
	add	x0, x0, lC5@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE11:
	.globl _anubis_address_E
	.data
	.align	1
_anubis_address_E:
	.space 2
	.const
	.align	3
lC13:
	.byte	109
	.byte	108
	.byte	100
	.byte	115
	.byte	97
	.byte	56
	.byte	55
	.byte	58
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
	.quad	LFB2-.
	.set L$set$2,LFE2-LFB2
	.quad L$set$2
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$3,LCFI0-LFB2
	.long L$set$3
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$5,LCFI2-LCFI1
	.long L$set$5
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$6,LCFI3-LCFI2
	.long L$set$6
	.byte	0x95
	.uleb128 0x8
	.byte	0x96
	.uleb128 0x7
	.byte	0x4
	.set L$set$7,LCFI4-LCFI3
	.long L$set$7
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
	.set L$set$8,LCFI5-LCFI4
	.long L$set$8
	.byte	0xb
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$9,LEFDE3-LASFDE3
	.long L$set$9
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB4-.
	.set L$set$10,LFE4-LFB4
	.quad L$set$10
	.uleb128 0x8
	.quad	LLSDA4-.
	.byte	0x4
	.set L$set$11,LCFI6-LFB4
	.long L$set$11
	.byte	0xe
	.uleb128 0x120
	.byte	0x9d
	.uleb128 0x24
	.byte	0x9e
	.uleb128 0x23
	.byte	0x4
	.set L$set$12,LCFI7-LCFI6
	.long L$set$12
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$13,LCFI8-LCFI7
	.long L$set$13
	.byte	0x93
	.uleb128 0x22
	.byte	0x94
	.uleb128 0x21
	.byte	0x95
	.uleb128 0x20
	.byte	0x96
	.uleb128 0x1f
	.byte	0x4
	.set L$set$14,LCFI9-LCFI8
	.long L$set$14
	.byte	0x97
	.uleb128 0x1e
	.byte	0x98
	.uleb128 0x1d
	.byte	0x99
	.uleb128 0x1c
	.byte	0x9a
	.uleb128 0x1b
	.byte	0x9b
	.uleb128 0x1a
	.byte	0x4
	.set L$set$15,LCFI10-LCFI9
	.long L$set$15
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xdb
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
	.set L$set$16,LCFI11-LCFI10
	.long L$set$16
	.byte	0xb
	.align	3
LEFDE3:
LSFDE5:
	.set L$set$17,LEFDE5-LASFDE5
	.long L$set$17
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB7-.
	.set L$set$18,LFE7-LFB7
	.quad L$set$18
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$19,LCFI12-LFB7
	.long L$set$19
	.byte	0xe
	.uleb128 0x120
	.byte	0x9d
	.uleb128 0x24
	.byte	0x9e
	.uleb128 0x23
	.byte	0x4
	.set L$set$20,LCFI13-LCFI12
	.long L$set$20
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$21,LCFI14-LCFI13
	.long L$set$21
	.byte	0x93
	.uleb128 0x22
	.byte	0x94
	.uleb128 0x21
	.byte	0x95
	.uleb128 0x20
	.byte	0x96
	.uleb128 0x1f
	.byte	0x97
	.uleb128 0x1e
	.byte	0x98
	.uleb128 0x1d
	.byte	0x99
	.uleb128 0x1c
	.byte	0x9a
	.uleb128 0x1b
	.byte	0x9b
	.uleb128 0x1a
	.byte	0x9c
	.uleb128 0x19
	.byte	0x4
	.set L$set$22,LCFI15-LCFI14
	.long L$set$22
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
	.set L$set$23,LCFI16-LCFI15
	.long L$set$23
	.byte	0xb
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$24,LEFDE7-LASFDE7
	.long L$set$24
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB8-.
	.set L$set$25,LFE8-LFB8
	.quad L$set$25
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$26,LCFI17-LFB8
	.long L$set$26
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$27,LCFI18-LCFI17
	.long L$set$27
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$28,LCFI19-LCFI18
	.long L$set$28
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x4
	.set L$set$29,LCFI20-LCFI19
	.long L$set$29
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$30,LCFI21-LCFI20
	.long L$set$30
	.byte	0xb
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$31,LEFDE9-LASFDE9
	.long L$set$31
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB9-.
	.set L$set$32,LFE9-LFB9
	.quad L$set$32
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$33,LCFI22-LFB9
	.long L$set$33
	.byte	0xe
	.uleb128 0xc0
	.byte	0x9d
	.uleb128 0x18
	.byte	0x9e
	.uleb128 0x17
	.byte	0x4
	.set L$set$34,LCFI23-LCFI22
	.long L$set$34
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$35,LCFI24-LCFI23
	.long L$set$35
	.byte	0x93
	.uleb128 0x16
	.byte	0x94
	.uleb128 0x15
	.byte	0x4
	.set L$set$36,LCFI25-LCFI24
	.long L$set$36
	.byte	0x95
	.uleb128 0x14
	.byte	0x96
	.uleb128 0x13
	.byte	0x97
	.uleb128 0x12
	.byte	0x4
	.set L$set$37,LCFI26-LCFI25
	.long L$set$37
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
	.set L$set$38,LCFI27-LCFI26
	.long L$set$38
	.byte	0xb
	.align	3
LEFDE9:
LSFDE11:
	.set L$set$39,LEFDE11-LASFDE11
	.long L$set$39
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB11-.
	.set L$set$40,LFE11-LFB11
	.quad L$set$40
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$41,LCFI28-LFB11
	.long L$set$41
	.byte	0xe
	.uleb128 0xa0
	.byte	0x9d
	.uleb128 0x14
	.byte	0x9e
	.uleb128 0x13
	.byte	0x4
	.set L$set$42,LCFI29-LCFI28
	.long L$set$42
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$43,LCFI30-LCFI29
	.long L$set$43
	.byte	0x93
	.uleb128 0x12
	.byte	0x94
	.uleb128 0x11
	.byte	0x95
	.uleb128 0x10
	.byte	0x96
	.uleb128 0xf
	.byte	0x4
	.set L$set$44,LCFI31-LCFI30
	.long L$set$44
	.byte	0x97
	.uleb128 0xe
	.byte	0x4
	.set L$set$45,LCFI32-LCFI31
	.long L$set$45
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
	.set L$set$46,LCFI33-LCFI32
	.long L$set$46
	.byte	0xb
	.align	3
LEFDE11:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
