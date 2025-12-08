	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__byte_arrayIP
_aegis_ffi__byte_arrayIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__Thash256BIP
_aegis_ffi__Thash256BIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__Thash512BIP
_aegis_ffi__Thash512BIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__Tu256_limbsBIP
_aegis_ffi__Tu256_limbsBIP:
LFB5:
	ret
LFE5:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__u256IP
_aegis_ffi__u256IP:
LFB6:
	ret
LFE6:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__vm_contextIP
_aegis_ffi__vm_contextIP:
LFB7:
	stp	xzr, xzr, [x0, 176]
	str	xzr, [x0, 192]
	strh	wzr, [x0, 200]
	ret
LFE7:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__exec_contextIP
_aegis_ffi__exec_contextIP:
LFB128:
	ret
LFE128:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__state_entryIP
_aegis_ffi__state_entryIP:
LFB9:
	add	x1, x0, 32768
	str	wzr, [x0, 256]
	str	wzr, [x1, 260]
	strb	wzr, [x1, 264]
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__Tstate_tableBIP
_aegis_ffi__Tstate_tableBIP:
LFB10:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L10
	sxtw	x5, w2
	sub	w1, w1, w2
	add	x4, x5, 1
	mov	x2, x5
	add	x4, x4, x1
	mov	x6, 33036
	.p2align 5,,15
L12:
	sub	x1, x2, x5
	add	x2, x2, 1
	madd	x1, x1, x6, x0
	add	x3, x1, 32768
	str	wzr, [x1, 256]
	str	wzr, [x3, 260]
	strb	wzr, [x3, 264]
	cmp	x2, x4
	bne	L12
L10:
	ret
LFE10:
	.align	2
	.p2align 5,,15
	.globl _aegis_ffi__Tbytecode_bufferBIP
_aegis_ffi__Tbytecode_bufferBIP:
LFB11:
	ret
LFE11:
	.const
	.align	3
lC9:
	.ascii "aegis_ffi.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_init
_aegis_init:
LFB12:
	adrp	x1, _aegis_ffi__library_initialized@PAGE
	ldrb	w0, [x1, #_aegis_ffi__library_initialized@PAGEOFF]
	cmp	w0, 1
	bhi	L26
	cbnz	w0, L17
	mov	w0, 1
	strb	w0, [x1, #_aegis_ffi__library_initialized@PAGEOFF]
L17:
	mov	w0, 0
LEHB0:
	ret
L26:
	adrp	x0, lC9@PAGE
	stp	x29, x30, [sp, -32]!
LCFI0:
	mov	w1, 55
	mov	x29, sp
LCFI1:
	add	x0, x0, lC9@PAGEOFF;
LEHE0:
	str	x19, [sp, 16]
LCFI2:
LEHB1:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE1:
L21:
	mov	x19, x0
	cmp	x1, 1
	bne	L27
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB2:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI3:
	ret
L27:
LCFI4:
	bl	__Unwind_Resume
LEHE2:
LFE12:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table0:
	.align	2
LLSDA12:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT12-LLSDATTD12
LLSDATTD12:
	.byte	0x1
	.uleb128 LLSDACSE12-LLSDACSB12
LLSDACSB12:
	.uleb128 LEHB0-LFB12
	.uleb128 LEHE0-LEHB0
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB1-LFB12
	.uleb128 LEHE1-LEHB1
	.uleb128 L21-LFB12
	.uleb128 0x1
	.uleb128 LEHB2-LFB12
	.uleb128 LEHE2-LEHB2
	.uleb128 0
	.uleb128 0
LLSDACSE12:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr0:
	.long	___gnat_others_value@GOT-L_got_pcr0
LLSDATT12:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_cleanup
_aegis_cleanup:
LFB13:
	adrp	x1, _aegis_ffi__library_initialized@PAGE
	ldrb	w0, [x1, #_aegis_ffi__library_initialized@PAGEOFF]
	cmp	w0, 1
	bhi	L37
	cbz	w0, L28
	strb	wzr, [x1, #_aegis_ffi__library_initialized@PAGEOFF]
L28:
	ret
L37:
	adrp	x0, lC9@PAGE
	stp	x29, x30, [sp, -16]!
LCFI5:
	mov	w1, 71
	mov	x29, sp
LCFI6:
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE13:
	.const
	.align	3
lC10:
	.ascii "1.0.0"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_version
_aegis_version:
LFB14:
	adrp	x0, lC10@PAGE
	adrp	x1, lC0@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC0@PAGEOFF;
	b	_interfaces__c__strings__new_string
LFE14:
	.const
	.align	2
lC0:
	.word	1
	.word	5
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_mldsa87_keygen
_aegis_mldsa87_keygen:
LFB15:
	cmp	x0, 0
	ccmp	x1, 0, 4, ne
	beq	L45
	stp	x29, x30, [sp, -32]!
LCFI7:
	mov	x29, sp
LCFI8:
LEHB3:
LEHE3:
	mov	x3, x0
	mov	x0, x2
	str	x19, [sp, 16]
LCFI9:
	cbz	x2, L51
	mov	x2, x1
	mov	x1, x3
LEHB4:
	bl	_anubis_mldsa__keygen
LEHE4:
L42:
	mov	w0, 0
L39:
LEHB5:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI10:
	ret
	.p2align 2,,3
L45:
	mov	w0, 1
LEHE5:
	ret
	.p2align 2,,3
L51:
LCFI11:
	adrp	x0, _zero_seed.12@PAGE
	mov	x2, x1
	add	x0, x0, _zero_seed.12@PAGEOFF;
	mov	x1, x3
LEHB6:
	bl	_anubis_mldsa__keygen
LEHE6:
	b	L42
L46:
	mov	x19, x0
	cmp	x1, 1
	bne	L52
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB7:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L39
L52:
	bl	__Unwind_Resume
LEHE7:
LFE15:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table1:
	.align	2
LLSDA15:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT15-LLSDATTD15
LLSDATTD15:
	.byte	0x1
	.uleb128 LLSDACSE15-LLSDACSB15
LLSDACSB15:
	.uleb128 LEHB3-LFB15
	.uleb128 LEHE3-LEHB3
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB4-LFB15
	.uleb128 LEHE4-LEHB4
	.uleb128 L46-LFB15
	.uleb128 0x1
	.uleb128 LEHB5-LFB15
	.uleb128 LEHE5-LEHB5
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB6-LFB15
	.uleb128 LEHE6-LEHB6
	.uleb128 L46-LFB15
	.uleb128 0x1
	.uleb128 LEHB7-LFB15
	.uleb128 LEHE7-LEHB7
	.uleb128 0
	.uleb128 0
LLSDACSE15:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr1:
	.long	___gnat_others_value@GOT-L_got_pcr1
LLSDATT15:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_mldsa87_sign
_aegis_mldsa87_sign:
LFB16:
	cmp	x3, 0
	stp	x29, x30, [sp, -80]!
LCFI12:
	mov	x29, sp
LCFI13:
	ccmp	x2, 0, 0, ne
LEHB8:
	ccmp	x1, 0, 4, ne
	ccmp	x4, 0, 4, ne
	stp	x19, x20, [sp, 16]
LCFI14:
	mov	x20, x0
	cset	w0, eq
	cmp	x20, 0
	stp	x21, x22, [sp, 32]
LCFI15:
	mov	x21, x1
	mov	x1, 1073741824
	ccmp	x3, x1, 2, ne
	cset	w1, hi
	orr	w0, w0, w1
	stp	x23, x24, [sp, 48]
LCFI16:
	cbz	w0, L71
	mov	w0, 1
LEHE8:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 80
LCFI17:
	ret
	.p2align 2,,3
L71:
LCFI18:
	mov	x19, x4
	mov	x22, sp
	sub	w23, w3, #1
	cbz	w3, L64
	add	x0, x3, 15
	mov	x5, x2
	and	x0, x0, -16
	sub	sp, sp, x0
	mov	x24, sp
	cbz	x2, L72
	mov	x0, 0
	ldp	w1, w0, [x0]
	cmp	w1, 0
	ccmp	w23, w0, 0, le
	bgt	L73
	mov	x2, x3
	mov	x0, sp
	sub	x1, x5, w1, sxtw
	bl	_memcpy
L55:
	adrp	x3, _zero_random.11@PAGE
	mov	x4, x20
	stp	wzr, w23, [x29, 72]
	mov	x1, x24
	mov	x0, x19
	add	x3, x3, _zero_random.11@PAGEOFF;
	add	x2, x29, 72
LEHB9:
	bl	_anubis_mldsa__sign
LEHE9:
LEHB10:
LEHE10:
	mov	sp, x22
	cmp	w0, 1
	bhi	L74
	cbz	w0, L59
	mov	x0, 4627
LEHB11:
	str	x0, [x21]
LEHE11:
	mov	w0, 0
LEHB12:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 80
LCFI19:
	ret
L66:
LCFI20:
	mov	x19, x0
	mov	x0, x1
L61:
	cmp	x0, 1
	mov	x0, x19
	bne	L75
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
	bl	___gnat_end_handler_v1
L59:
	mov	w0, 255
LEHE12:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 80
LCFI21:
	ret
	.p2align 2,,3
L64:
LCFI22:
	add	x24, x29, 64
	b	L55
L74:
	adrp	x0, lC9@PAGE
	mov	w1, 176
	add	x0, x0, lC9@PAGEOFF;
LEHB13:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE13:
L73:
	adrp	x0, lC9@PAGE
	mov	w1, 171
	add	x0, x0, lC9@PAGEOFF;
LEHB14:
	bl	___gnat_rcheck_CE_Range_Check
L72:
	adrp	x0, lC9@PAGE
	mov	w1, 171
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
LEHE14:
L65:
	mov	x19, x0
LEHB15:
	mov	x0, x1
	mov	sp, x22
	b	L61
L75:
	bl	__Unwind_Resume
LEHE15:
LFE16:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table2:
	.align	2
LLSDA16:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT16-LLSDATTD16
LLSDATTD16:
	.byte	0x1
	.uleb128 LLSDACSE16-LLSDACSB16
LLSDACSB16:
	.uleb128 LEHB8-LFB16
	.uleb128 LEHE8-LEHB8
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB9-LFB16
	.uleb128 LEHE9-LEHB9
	.uleb128 L65-LFB16
	.uleb128 0x3
	.uleb128 LEHB10-LFB16
	.uleb128 LEHE10-LEHB10
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB11-LFB16
	.uleb128 LEHE11-LEHB11
	.uleb128 L66-LFB16
	.uleb128 0x1
	.uleb128 LEHB12-LFB16
	.uleb128 LEHE12-LEHB12
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB13-LFB16
	.uleb128 LEHE13-LEHB13
	.uleb128 L66-LFB16
	.uleb128 0x1
	.uleb128 LEHB14-LFB16
	.uleb128 LEHE14-LEHB14
	.uleb128 L65-LFB16
	.uleb128 0x3
	.uleb128 LEHB15-LFB16
	.uleb128 LEHE15-LEHB15
	.uleb128 0
	.uleb128 0
LLSDACSE16:
	.byte	0x1
	.byte	0
	.byte	0
	.byte	0x7d
	.align	2
L_got_pcr2:
	.long	___gnat_others_value@GOT-L_got_pcr2
LLSDATT16:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_mldsa87_verify
_aegis_mldsa87_verify:
LFB17:
	cmp	x3, 0
	stp	x29, x30, [sp, -80]!
LCFI23:
	mov	x7, x2
	ccmp	x2, 0, 0, ne
	mov	x29, sp
LCFI24:
LEHB16:
LEHE16:
	ccmp	x5, 0, 4, ne
	cset	w2, eq
	cmp	x0, 0
	stp	x19, x20, [sp, 16]
	ccmp	x4, 0, 4, ne
	stp	x21, x22, [sp, 32]
LCFI25:
	mov	x21, x0
	cset	w0, eq
	orr	w2, w2, w0
	stp	x23, x24, [sp, 48]
LCFI26:
	cbnz	w2, L81
	mov	x0, 4627
	mov	x19, x5
	cmp	x1, x0
	bne	L95
	mov	x0, 1073741824
	cmp	x3, x0
	bhi	L96
	mov	x20, x4
	mov	x22, sp
	sub	w23, w3, #1
	cbz	w3, L90
	add	x0, x3, 15
	and	x0, x0, -16
	sub	sp, sp, x0
	mov	x24, sp
	cbz	x7, L97
	mov	x0, 0
	ldp	w1, w0, [x0]
	cmp	w1, 0
	ccmp	w23, w0, 0, le
	bgt	L98
	mov	x2, x3
	mov	x0, sp
	sub	x1, x7, w1, sxtw
	bl	_memcpy
L82:
	mov	x3, x21
	mov	x1, x24
	stp	wzr, w23, [x29, 72]
	mov	x0, x20
	add	x2, x29, 72
LEHB17:
	bl	_anubis_mldsa__verify
	cmp	w0, 1
	bhi	L99
	and	w0, w0, 1
	strb	w0, [x19]
LEHE17:
LEHB18:
	mov	w0, 0
	mov	sp, x22
LEHE18:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 80
LCFI27:
	ret
	.p2align 2,,3
L95:
LCFI28:
LEHB19:
	strb	wzr, [x5]
LEHE19:
	mov	w0, 2
L76:
LEHB20:
LEHE20:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 80
LCFI29:
	ret
	.p2align 2,,3
L96:
LCFI30:
LEHB21:
	strb	wzr, [x5]
LEHE21:
L81:
	mov	w0, 1
LEHB22:
LEHE22:
	mov	sp, x29
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
	ldp	x29, x30, [sp], 80
LCFI31:
	ret
	.p2align 2,,3
L90:
LCFI32:
	add	x24, x29, 64
	b	L82
L98:
	adrp	x0, lC9@PAGE
	mov	w1, 232
	add	x0, x0, lC9@PAGEOFF;
LEHB23:
	bl	___gnat_rcheck_CE_Range_Check
L97:
	adrp	x0, lC9@PAGE
	mov	w1, 232
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L99:
	adrp	x0, lC9@PAGE
	mov	w1, 234
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE23:
L91:
	mov	x19, x0
	mov	x0, x1
L87:
	cmp	x0, 1
	mov	x0, x19
	bne	L100
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB24:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L76
L92:
	mov	x19, x0
	mov	x0, x1
	mov	sp, x22
	b	L87
L100:
	bl	__Unwind_Resume
LEHE24:
LFE17:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table3:
	.align	2
LLSDA17:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT17-LLSDATTD17
LLSDATTD17:
	.byte	0x1
	.uleb128 LLSDACSE17-LLSDACSB17
LLSDACSB17:
	.uleb128 LEHB16-LFB17
	.uleb128 LEHE16-LEHB16
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB17-LFB17
	.uleb128 LEHE17-LEHB17
	.uleb128 L92-LFB17
	.uleb128 0x3
	.uleb128 LEHB18-LFB17
	.uleb128 LEHE18-LEHB18
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB19-LFB17
	.uleb128 LEHE19-LEHB19
	.uleb128 L91-LFB17
	.uleb128 0x1
	.uleb128 LEHB20-LFB17
	.uleb128 LEHE20-LEHB20
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB21-LFB17
	.uleb128 LEHE21-LEHB21
	.uleb128 L91-LFB17
	.uleb128 0x1
	.uleb128 LEHB22-LFB17
	.uleb128 LEHE22-LEHB22
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB23-LFB17
	.uleb128 LEHE23-LEHB23
	.uleb128 L92-LFB17
	.uleb128 0x3
	.uleb128 LEHB24-LFB17
	.uleb128 LEHE24-LEHB24
	.uleb128 0
	.uleb128 0
LLSDACSE17:
	.byte	0x1
	.byte	0
	.byte	0
	.byte	0x7d
	.align	2
L_got_pcr3:
	.long	___gnat_others_value@GOT-L_got_pcr3
LLSDATT17:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_mlkem1024_keygen
_aegis_mlkem1024_keygen:
LFB18:
	cmp	x0, 0
	ccmp	x1, 0, 4, ne
	beq	L107
	stp	x29, x30, [sp, -32]!
LCFI33:
	mov	x29, sp
LCFI34:
LEHB25:
LEHE25:
	mov	x4, x0
	mov	x3, x1
	mov	x0, x2
	str	x19, [sp, 16]
LCFI35:
	cbz	x2, L113
	adrp	x1, _zero_z.8@PAGE
	mov	x2, x4
	add	x1, x1, _zero_z.8@PAGEOFF;
LEHB26:
	bl	_anubis_mlkem__keygen
LEHE26:
L104:
	mov	w0, 0
L101:
LEHB27:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI36:
	ret
	.p2align 2,,3
L107:
	mov	w0, 1
LEHE27:
	ret
	.p2align 2,,3
L113:
LCFI37:
	adrp	x1, _zero_z.9@PAGE
	adrp	x0, _zero_d.10@PAGE
	mov	x2, x4
	add	x1, x1, _zero_z.9@PAGEOFF;
	add	x0, x0, _zero_d.10@PAGEOFF;
LEHB28:
	bl	_anubis_mlkem__keygen
LEHE28:
	b	L104
L108:
	mov	x19, x0
	cmp	x1, 1
	bne	L114
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB29:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L101
L114:
	bl	__Unwind_Resume
LEHE29:
LFE18:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table4:
	.align	2
LLSDA18:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT18-LLSDATTD18
LLSDATTD18:
	.byte	0x1
	.uleb128 LLSDACSE18-LLSDACSB18
LLSDACSB18:
	.uleb128 LEHB25-LFB18
	.uleb128 LEHE25-LEHB25
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB26-LFB18
	.uleb128 LEHE26-LEHB26
	.uleb128 L108-LFB18
	.uleb128 0x1
	.uleb128 LEHB27-LFB18
	.uleb128 LEHE27-LEHB27
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB28-LFB18
	.uleb128 LEHE28-LEHB28
	.uleb128 L108-LFB18
	.uleb128 0x1
	.uleb128 LEHB29-LFB18
	.uleb128 LEHE29-LEHB29
	.uleb128 0
	.uleb128 0
LLSDACSE18:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr4:
	.long	___gnat_others_value@GOT-L_got_pcr4
LLSDATT18:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_mlkem1024_encaps
_aegis_mlkem1024_encaps:
LFB19:
	cmp	x0, 0
	ccmp	x1, 0, 4, ne
	ccmp	x2, 0, 4, ne
	beq	L121
	stp	x29, x30, [sp, -32]!
LCFI38:
	mov	x29, sp
LCFI39:
LEHB30:
LEHE30:
	mov	x5, x0
	mov	x4, x1
	mov	x0, x2
	mov	x1, x3
	str	x19, [sp, 16]
LCFI40:
	cbz	x3, L127
	mov	x3, x5
	mov	x2, x4
LEHB31:
	bl	_anubis_mlkem__encaps
LEHE31:
L118:
	mov	w0, 0
L115:
LEHB32:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI41:
	ret
	.p2align 2,,3
L121:
	mov	w0, 1
LEHE32:
	ret
	.p2align 2,,3
L127:
LCFI42:
	adrp	x1, _zero_m.7@PAGE
	mov	x3, x5
	mov	x2, x4
	add	x1, x1, _zero_m.7@PAGEOFF;
LEHB33:
	bl	_anubis_mlkem__encaps
LEHE33:
	b	L118
L122:
	mov	x19, x0
	cmp	x1, 1
	bne	L128
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB34:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L115
L128:
	bl	__Unwind_Resume
LEHE34:
LFE19:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table5:
	.align	2
LLSDA19:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT19-LLSDATTD19
LLSDATTD19:
	.byte	0x1
	.uleb128 LLSDACSE19-LLSDACSB19
LLSDACSB19:
	.uleb128 LEHB30-LFB19
	.uleb128 LEHE30-LEHB30
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB31-LFB19
	.uleb128 LEHE31-LEHB31
	.uleb128 L122-LFB19
	.uleb128 0x1
	.uleb128 LEHB32-LFB19
	.uleb128 LEHE32-LEHB32
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB33-LFB19
	.uleb128 LEHE33-LEHB33
	.uleb128 L122-LFB19
	.uleb128 0x1
	.uleb128 LEHB34-LFB19
	.uleb128 LEHE34-LEHB34
	.uleb128 0
	.uleb128 0
LLSDACSE19:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr5:
	.long	___gnat_others_value@GOT-L_got_pcr5
LLSDATT19:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_mlkem1024_decaps
_aegis_mlkem1024_decaps:
LFB20:
	cmp	x0, 0
	ccmp	x1, 0, 4, ne
	ccmp	x2, 0, 4, ne
	bne	L139
	mov	w0, 1
LEHB35:
	ret
	.p2align 2,,3
L139:
	mov	x3, x0
	stp	x29, x30, [sp, -32]!
LCFI43:
	mov	x0, x2
	mov	x29, sp
LCFI44:
	mov	x2, x3
LEHE35:
	str	x19, [sp, 16]
LCFI45:
LEHB36:
	bl	_anubis_mlkem__decaps
LEHE36:
	mov	w0, 0
L129:
LEHB37:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI46:
	ret
L134:
LCFI47:
	mov	x19, x0
	cmp	x1, 1
	bne	L140
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L129
L140:
	bl	__Unwind_Resume
LEHE37:
LFE20:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table6:
	.align	2
LLSDA20:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT20-LLSDATTD20
LLSDATTD20:
	.byte	0x1
	.uleb128 LLSDACSE20-LLSDACSB20
LLSDACSB20:
	.uleb128 LEHB35-LFB20
	.uleb128 LEHE35-LEHB35
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB36-LFB20
	.uleb128 LEHE36-LEHB36
	.uleb128 L134-LFB20
	.uleb128 0x1
	.uleb128 LEHB37-LFB20
	.uleb128 LEHE37-LEHB37
	.uleb128 0
	.uleb128 0
LLSDACSE20:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr6:
	.long	___gnat_others_value@GOT-L_got_pcr6
LLSDATT20:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_sha3_256
_aegis_sha3_256:
LFB21:
	cbz	x0, L149
	stp	x29, x30, [sp, -80]!
LCFI48:
	mov	x29, sp
LCFI49:
LEHB38:
LEHE38:
	stp	x19, x20, [sp, 16]
LCFI50:
	mov	x19, x0
	cbz	x2, L157
	cbz	x1, L150
	mov	x0, 2147483647
	cmp	x2, x0
	bhi	L158
	sub	w3, w2, #1
	add	x20, x29, 48
	mov	x0, x1
	mov	x2, x20
	add	x1, x29, 40
	stp	wzr, w3, [x29, 40]
LEHB39:
	bl	_anubis_sha3__sha3_256
L146:
	mov	x2, -1
	.p2align 5,,15
L144:
	add	x1, x20, x2
	add	x2, x2, 1
	ldrb	w1, [x1, 1]
	strb	w1, [x19, x2]
LEHE39:
	cmp	x2, 31
	bne	L144
	mov	w0, 0
L141:
LEHB40:
	ldp	x19, x20, [sp, 16]
LEHE40:
	ldp	x29, x30, [sp], 80
LCFI51:
	ret
	.p2align 2,,3
L157:
LCFI52:
	adrp	x1, lC1@PAGE
	add	x20, x29, 48
	add	x1, x1, lC1@PAGEOFF;
	mov	x2, x20
	add	x0, x29, 32
LEHB41:
	bl	_anubis_sha3__sha3_256
LEHE41:
	b	L146
L150:
	mov	w0, 1
	b	L141
L149:
LCFI53:
	mov	w0, 1
LEHB42:
LEHE42:
	ret
L158:
LCFI54:
	adrp	x0, lC9@PAGE
	mov	w1, 404
	add	x0, x0, lC9@PAGEOFF;
LEHB43:
	bl	___gnat_rcheck_CE_Range_Check
LEHE43:
L151:
	mov	x19, x0
	cmp	x1, 1
	bne	L159
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB44:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L141
L159:
	bl	__Unwind_Resume
LEHE44:
LFE21:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table7:
	.align	2
LLSDA21:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT21-LLSDATTD21
LLSDATTD21:
	.byte	0x1
	.uleb128 LLSDACSE21-LLSDACSB21
LLSDACSB21:
	.uleb128 LEHB38-LFB21
	.uleb128 LEHE38-LEHB38
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB39-LFB21
	.uleb128 LEHE39-LEHB39
	.uleb128 L151-LFB21
	.uleb128 0x1
	.uleb128 LEHB40-LFB21
	.uleb128 LEHE40-LEHB40
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB41-LFB21
	.uleb128 LEHE41-LEHB41
	.uleb128 L151-LFB21
	.uleb128 0x1
	.uleb128 LEHB42-LFB21
	.uleb128 LEHE42-LEHB42
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB43-LFB21
	.uleb128 LEHE43-LEHB43
	.uleb128 L151-LFB21
	.uleb128 0x1
	.uleb128 LEHB44-LFB21
	.uleb128 LEHE44-LEHB44
	.uleb128 0
	.uleb128 0
LLSDACSE21:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr7:
	.long	___gnat_others_value@GOT-L_got_pcr7
LLSDATT21:
	.text
	.const
	.align	2
lC1:
	.word	0
	.word	-1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_sha3_512
_aegis_sha3_512:
LFB22:
	cbz	x0, L168
	stp	x29, x30, [sp, -112]!
LCFI55:
	mov	x29, sp
LCFI56:
LEHB45:
LEHE45:
	stp	x19, x20, [sp, 16]
LCFI57:
	mov	x19, x0
	cbz	x2, L176
	cbz	x1, L169
	mov	x0, 2147483647
	cmp	x2, x0
	bhi	L177
	sub	w3, w2, #1
	add	x20, x29, 48
	mov	x0, x1
	mov	x2, x20
	add	x1, x29, 40
	stp	wzr, w3, [x29, 40]
LEHB46:
	bl	_anubis_sha3__sha3_512
L165:
	mov	x2, -1
	.p2align 5,,15
L163:
	add	x1, x20, x2
	add	x2, x2, 1
	ldrb	w1, [x1, 1]
	strb	w1, [x19, x2]
LEHE46:
	cmp	x2, 63
	bne	L163
	mov	w0, 0
L160:
LEHB47:
	ldp	x19, x20, [sp, 16]
LEHE47:
	ldp	x29, x30, [sp], 112
LCFI58:
	ret
L176:
LCFI59:
	adrp	x1, lC1@PAGE
	add	x20, x29, 48
	add	x1, x1, lC1@PAGEOFF;
	mov	x2, x20
	add	x0, x29, 32
LEHB48:
	bl	_anubis_sha3__sha3_512
LEHE48:
	b	L165
L169:
	mov	w0, 1
	b	L160
L168:
LCFI60:
	mov	w0, 1
LEHB49:
LEHE49:
	ret
L177:
LCFI61:
	adrp	x0, lC9@PAGE
	mov	w1, 446
	add	x0, x0, lC9@PAGEOFF;
LEHB50:
	bl	___gnat_rcheck_CE_Range_Check
LEHE50:
L170:
	mov	x19, x0
	cmp	x1, 1
	bne	L178
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB51:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L160
L178:
	bl	__Unwind_Resume
LEHE51:
LFE22:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table8:
	.align	2
LLSDA22:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT22-LLSDATTD22
LLSDATTD22:
	.byte	0x1
	.uleb128 LLSDACSE22-LLSDACSB22
LLSDACSB22:
	.uleb128 LEHB45-LFB22
	.uleb128 LEHE45-LEHB45
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB46-LFB22
	.uleb128 LEHE46-LEHB46
	.uleb128 L170-LFB22
	.uleb128 0x1
	.uleb128 LEHB47-LFB22
	.uleb128 LEHE47-LEHB47
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB48-LFB22
	.uleb128 LEHE48-LEHB48
	.uleb128 L170-LFB22
	.uleb128 0x1
	.uleb128 LEHB49-LFB22
	.uleb128 LEHE49-LEHB49
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB50-LFB22
	.uleb128 LEHE50-LEHB50
	.uleb128 L170-LFB22
	.uleb128 0x1
	.uleb128 LEHB51-LFB22
	.uleb128 LEHE51-LEHB51
	.uleb128 0
	.uleb128 0
LLSDACSE22:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr8:
	.long	___gnat_others_value@GOT-L_got_pcr8
LLSDATT22:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_keccak256
_aegis_keccak256:
LFB23:
	cbz	x0, L187
	stp	x29, x30, [sp, -80]!
LCFI62:
	mov	x29, sp
LCFI63:
LEHB52:
LEHE52:
	stp	x19, x20, [sp, 16]
LCFI64:
	mov	x19, x0
	cbz	x2, L195
	cbz	x1, L188
	mov	x0, 2147483647
	cmp	x2, x0
	bhi	L196
	sub	w3, w2, #1
	add	x20, x29, 48
	mov	x0, x1
	mov	x2, x20
	add	x1, x29, 40
	stp	wzr, w3, [x29, 40]
LEHB53:
	bl	_anubis_sha3__keccak_256
L184:
	mov	x2, -1
	.p2align 5,,15
L182:
	add	x1, x20, x2
	add	x2, x2, 1
	ldrb	w1, [x1, 1]
	strb	w1, [x19, x2]
LEHE53:
	cmp	x2, 31
	bne	L182
	mov	w0, 0
L179:
LEHB54:
	ldp	x19, x20, [sp, 16]
LEHE54:
	ldp	x29, x30, [sp], 80
LCFI65:
	ret
	.p2align 2,,3
L195:
LCFI66:
	adrp	x1, lC1@PAGE
	add	x20, x29, 48
	add	x1, x1, lC1@PAGEOFF;
	mov	x2, x20
	add	x0, x29, 32
LEHB55:
	bl	_anubis_sha3__keccak_256
LEHE55:
	b	L184
L188:
	mov	w0, 1
	b	L179
L187:
LCFI67:
	mov	w0, 1
LEHB56:
LEHE56:
	ret
L196:
LCFI68:
	adrp	x0, lC9@PAGE
	mov	w1, 490
	add	x0, x0, lC9@PAGEOFF;
LEHB57:
	bl	___gnat_rcheck_CE_Range_Check
LEHE57:
L189:
	mov	x19, x0
	cmp	x1, 1
	bne	L197
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB58:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L179
L197:
	bl	__Unwind_Resume
LEHE58:
LFE23:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table9:
	.align	2
LLSDA23:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT23-LLSDATTD23
LLSDATTD23:
	.byte	0x1
	.uleb128 LLSDACSE23-LLSDACSB23
LLSDACSB23:
	.uleb128 LEHB52-LFB23
	.uleb128 LEHE52-LEHB52
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB53-LFB23
	.uleb128 LEHE53-LEHB53
	.uleb128 L189-LFB23
	.uleb128 0x1
	.uleb128 LEHB54-LFB23
	.uleb128 LEHE54-LEHB54
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB55-LFB23
	.uleb128 LEHE55-LEHB55
	.uleb128 L189-LFB23
	.uleb128 0x1
	.uleb128 LEHB56-LFB23
	.uleb128 LEHE56-LEHB56
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB57-LFB23
	.uleb128 LEHE57-LEHB57
	.uleb128 L189-LFB23
	.uleb128 0x1
	.uleb128 LEHB58-LFB23
	.uleb128 LEHE58-LEHB58
	.uleb128 0
	.uleb128 0
LLSDACSE23:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr9:
	.long	___gnat_others_value@GOT-L_got_pcr9
LLSDATT23:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_shake128
_aegis_shake128:
LFB24:
	cmp	x0, 0
	mov	x6, x0
	sub	x4, x1, #1
	mov	x0, 65534
	ccmp	x4, x0, 2, ne
	bls	L212
L206:
	mov	w0, 1
LEHB59:
	ret
	.p2align 2,,3
L212:
	cmp	x3, 0
	mov	x0, x2
	ccmp	x2, 0, 0, ne
	beq	L206
	stp	x29, x30, [sp, -64]!
LCFI69:
	mov	x29, sp
LCFI70:
LEHE59:
	sub	w7, w1, #1
	str	x19, [sp, 16]
LCFI71:
	cbz	x3, L213
	mov	x2, 2147483647
	cmp	x3, x2
	bhi	L214
	sub	w5, w3, #1
	mov	w4, w1
	stp	wzr, w7, [x29, 56]
	mov	x2, x6
	add	x3, x29, 56
	add	x1, x29, 48
	stp	wzr, w5, [x29, 48]
LEHB60:
	bl	_anubis_sha3__shake128
LEHE60:
L202:
	mov	w0, 0
L198:
LEHB61:
	ldr	x19, [sp, 16]
LEHE61:
	ldp	x29, x30, [sp], 64
LCFI72:
	ret
	.p2align 2,,3
L213:
LCFI73:
	adrp	x0, lC1@PAGE
	mov	w4, w1
	stp	wzr, w7, [x29, 40]
	add	x1, x0, lC1@PAGEOFF;
	mov	x2, x6
	add	x3, x29, 40
	add	x0, x29, 32
LEHB62:
	bl	_anubis_sha3__shake128
	b	L202
L214:
	adrp	x0, lC9@PAGE
	mov	w1, 538
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LEHE62:
L207:
	mov	x19, x0
	cmp	x1, 1
	bne	L215
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB63:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L198
L215:
	bl	__Unwind_Resume
LEHE63:
LFE24:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table10:
	.align	2
LLSDA24:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT24-LLSDATTD24
LLSDATTD24:
	.byte	0x1
	.uleb128 LLSDACSE24-LLSDACSB24
LLSDACSB24:
	.uleb128 LEHB59-LFB24
	.uleb128 LEHE59-LEHB59
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB60-LFB24
	.uleb128 LEHE60-LEHB60
	.uleb128 L207-LFB24
	.uleb128 0x1
	.uleb128 LEHB61-LFB24
	.uleb128 LEHE61-LEHB61
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB62-LFB24
	.uleb128 LEHE62-LEHB62
	.uleb128 L207-LFB24
	.uleb128 0x1
	.uleb128 LEHB63-LFB24
	.uleb128 LEHE63-LEHB63
	.uleb128 0
	.uleb128 0
LLSDACSE24:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr10:
	.long	___gnat_others_value@GOT-L_got_pcr10
LLSDATT24:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_shake256
_aegis_shake256:
LFB25:
	cmp	x0, 0
	mov	x6, x0
	sub	x4, x1, #1
	mov	x0, 65534
	ccmp	x4, x0, 2, ne
	bls	L230
L224:
	mov	w0, 1
LEHB64:
	ret
	.p2align 2,,3
L230:
	cmp	x3, 0
	mov	x0, x2
	ccmp	x2, 0, 0, ne
	beq	L224
	stp	x29, x30, [sp, -64]!
LCFI74:
	mov	x29, sp
LCFI75:
LEHE64:
	sub	w7, w1, #1
	str	x19, [sp, 16]
LCFI76:
	cbz	x3, L231
	mov	x2, 2147483647
	cmp	x3, x2
	bhi	L232
	sub	w5, w3, #1
	mov	w4, w1
	stp	wzr, w7, [x29, 56]
	mov	x2, x6
	add	x3, x29, 56
	add	x1, x29, 48
	stp	wzr, w5, [x29, 48]
LEHB65:
	bl	_anubis_sha3__shake256
LEHE65:
L220:
	mov	w0, 0
L216:
LEHB66:
	ldr	x19, [sp, 16]
LEHE66:
	ldp	x29, x30, [sp], 64
LCFI77:
	ret
	.p2align 2,,3
L231:
LCFI78:
	adrp	x0, lC1@PAGE
	mov	w4, w1
	stp	wzr, w7, [x29, 40]
	add	x1, x0, lC1@PAGEOFF;
	mov	x2, x6
	add	x3, x29, 40
	add	x0, x29, 32
LEHB67:
	bl	_anubis_sha3__shake256
	b	L220
L232:
	adrp	x0, lC9@PAGE
	mov	w1, 581
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LEHE67:
L225:
	mov	x19, x0
	cmp	x1, 1
	bne	L233
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB68:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L216
L233:
	bl	__Unwind_Resume
LEHE68:
LFE25:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table11:
	.align	2
LLSDA25:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT25-LLSDATTD25
LLSDATTD25:
	.byte	0x1
	.uleb128 LLSDACSE25-LLSDACSB25
LLSDACSB25:
	.uleb128 LEHB64-LFB25
	.uleb128 LEHE64-LEHB64
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB65-LFB25
	.uleb128 LEHE65-LEHB65
	.uleb128 L225-LFB25
	.uleb128 0x1
	.uleb128 LEHB66-LFB25
	.uleb128 LEHE66-LEHB66
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB67-LFB25
	.uleb128 LEHE67-LEHB67
	.uleb128 L225-LFB25
	.uleb128 0x1
	.uleb128 LEHB68-LFB25
	.uleb128 LEHE68-LEHB68
	.uleb128 0
	.uleb128 0
LLSDACSE25:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr11:
	.long	___gnat_others_value@GOT-L_got_pcr11
LLSDATT25:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_derive_address
_aegis_derive_address:
LFB26:
	cmp	x0, 0
	ccmp	x1, 0, 4, ne
	beq	L243
	sub	w0, w2, #99
	cmp	w0, 19
	bhi	L243
	mov	x2, 1
	movk	x2, 0xd, lsl 16
	lsr	x2, x2, x0
	and	x0, x2, 1
	tbz	x2, 0, L243
	sub	sp, sp, #2656
LCFI79:
	adrp	x5, _domain_prefix.6@PAGE
	add	x5, x5, _domain_prefix.6@PAGEOFF;
	stp	x29, x30, [sp]
LCFI80:
	mov	x29, sp
LCFI81:
	add	x4, x29, 40
LEHB69:
LEHE69:
	str	x19, [sp, 16]
LCFI82:
	.p2align 5,,15
L236:
	add	x2, x0, x5
LEHB70:
	ldrsb	w3, [x2, -1]
	add	x2, x4, x0
	strb	w3, [x2, -1]
	add	x0, x0, 1
	cmp	x0, 18
	bne	L236
	mov	x0, 0
	.p2align 5,,15
L237:
	ldrb	w3, [x1, x0]
	add	x2, x4, x0
	strb	w3, [x2, 18]
	add	x0, x0, 1
	cmp	x0, 2591
	bne	L237
	adrp	x0, lC9@PAGE
	mov	w1, 647
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LEHE70:
	.p2align 2,,3
L243:
LCFI83:
	mov	w0, 1
LEHB71:
	ret
L244:
LCFI84:
	mov	x19, x0
	cmp	x1, 1
	bne	L252
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
	bl	___gnat_end_handler_v1
	mov	w0, 255
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp]
	add	sp, sp, 2656
LCFI85:
	ret
L252:
LCFI86:
	bl	__Unwind_Resume
LEHE71:
LFE26:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table12:
	.align	2
LLSDA26:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT26-LLSDATTD26
LLSDATTD26:
	.byte	0x1
	.uleb128 LLSDACSE26-LLSDACSB26
LLSDACSB26:
	.uleb128 LEHB69-LFB26
	.uleb128 LEHE69-LEHB69
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB70-LFB26
	.uleb128 LEHE70-LEHB70
	.uleb128 L244-LFB26
	.uleb128 0x1
	.uleb128 LEHB71-LFB26
	.uleb128 LEHE71-LEHB71
	.uleb128 0
	.uleb128 0
LLSDACSE26:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr12:
	.long	___gnat_others_value@GOT-L_got_pcr12
LLSDATT26:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_create
_aegis_vm_create:
LFB27:
	cbz	x0, L258
	stp	x29, x30, [sp, -32]!
LCFI87:
	mov	x29, sp
LCFI88:
LEHB72:
LEHE72:
	stp	x19, x20, [sp, 16]
LCFI89:
	mov	x20, x0
	mov	x0, 208
LEHB73:
	bl	___gnat_malloc
LEHE73:
	mov	x19, x0
LEHB74:
	stp	xzr, xzr, [x0, 176]
	str	xzr, [x0, 192]
	strh	wzr, [x0, 200]
LEHE74:
LEHB75:
	str	xzr, [x0, 152]
	str	xzr, [x0, 160]
	str	wzr, [x0, 168]
	mov	x0, 524288
	bl	___gnat_malloc
	str	x0, [x19, 176]
	str	xzr, [x19, 184]
	mov	x0, 12288
	movk	x0, 0x204, lsl 16
	bl	___gnat_malloc
LEHE75:
	mov	x1, 0
	mov	x4, 33036
	.p2align 5,,15
L255:
	madd	x3, x1, x4, x0
	add	x1, x1, 1
	add	x2, x3, 32768
LEHB76:
	str	wzr, [x3, 256]
	str	wzr, [x2, 260]
	strb	wzr, [x2, 264]
LEHE76:
	cmp	x1, 1024
	bne	L255
LEHB77:
	str	x0, [x19, 192]
	mov	w0, 1
	strh	w0, [x19, 200]
	str	x19, [x20]
LEHE77:
	mov	w0, 0
L253:
LEHB78:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI90:
	ret
	.p2align 2,,3
L258:
	mov	w0, 1
	ret
L259:
LCFI91:
	mov	x19, x0
	cmp	x1, 1
	bne	L265
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
	bl	___gnat_end_handler_v1
	mov	w0, 5
	b	L253
L265:
	bl	__Unwind_Resume
LEHE78:
LFE27:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table13:
	.align	2
LLSDA27:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT27-LLSDATTD27
LLSDATTD27:
	.byte	0x1
	.uleb128 LLSDACSE27-LLSDACSB27
LLSDACSB27:
	.uleb128 LEHB72-LFB27
	.uleb128 LEHE72-LEHB72
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB73-LFB27
	.uleb128 LEHE73-LEHB73
	.uleb128 L259-LFB27
	.uleb128 0x1
	.uleb128 LEHB74-LFB27
	.uleb128 LEHE74-LEHB74
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB75-LFB27
	.uleb128 LEHE75-LEHB75
	.uleb128 L259-LFB27
	.uleb128 0x1
	.uleb128 LEHB76-LFB27
	.uleb128 LEHE76-LEHB76
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB77-LFB27
	.uleb128 LEHE77-LEHB77
	.uleb128 L259-LFB27
	.uleb128 0x1
	.uleb128 LEHB78-LFB27
	.uleb128 LEHE78-LEHB78
	.uleb128 0
	.uleb128 0
LLSDACSE27:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr13:
	.long	___gnat_others_value@GOT-L_got_pcr13
LLSDATT27:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_create_with_options
_aegis_vm_create_with_options:
LFB28:
	cbz	x0, L271
	stp	x29, x30, [sp, -48]!
LCFI92:
	mov	x29, sp
LCFI93:
LEHB79:
LEHE79:
	stp	x19, x20, [sp, 16]
LCFI94:
	mov	x20, x0
	mov	x0, 208
	stp	x21, x22, [sp, 32]
LCFI95:
	mov	x22, x1
	mov	w21, w2
LEHB80:
	bl	___gnat_malloc
LEHE80:
	mov	x19, x0
LEHB81:
	stp	xzr, xzr, [x0, 176]
	str	xzr, [x0, 192]
	strh	wzr, [x0, 200]
LEHE81:
LEHB82:
	str	x22, [x0, 152]
	str	xzr, [x0, 160]
	str	w21, [x0, 168]
	mov	x0, 524288
	bl	___gnat_malloc
	str	x0, [x19, 176]
	str	xzr, [x19, 184]
	mov	x0, 12288
	movk	x0, 0x204, lsl 16
	bl	___gnat_malloc
LEHE82:
	mov	x1, 0
	mov	x4, 33036
	.p2align 5,,15
L268:
	madd	x3, x1, x4, x0
	add	x1, x1, 1
	add	x2, x3, 32768
LEHB83:
	str	wzr, [x3, 256]
	str	wzr, [x2, 260]
	strb	wzr, [x2, 264]
LEHE83:
	cmp	x1, 1024
	bne	L268
LEHB84:
	str	x0, [x19, 192]
	mov	w0, 1
	strh	w0, [x19, 200]
	str	x19, [x20]
LEHE84:
	mov	w0, 0
L266:
LEHB85:
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x29, x30, [sp], 48
LCFI96:
	ret
	.p2align 2,,3
L271:
	mov	w0, 1
	ret
L272:
LCFI97:
	mov	x19, x0
	cmp	x1, 1
	bne	L278
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
	bl	___gnat_end_handler_v1
	mov	w0, 5
	b	L266
L278:
	bl	__Unwind_Resume
LEHE85:
LFE28:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table14:
	.align	2
LLSDA28:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT28-LLSDATTD28
LLSDATTD28:
	.byte	0x1
	.uleb128 LLSDACSE28-LLSDACSB28
LLSDACSB28:
	.uleb128 LEHB79-LFB28
	.uleb128 LEHE79-LEHB79
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB80-LFB28
	.uleb128 LEHE80-LEHB80
	.uleb128 L272-LFB28
	.uleb128 0x1
	.uleb128 LEHB81-LFB28
	.uleb128 LEHE81-LEHB81
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB82-LFB28
	.uleb128 LEHE82-LEHB82
	.uleb128 L272-LFB28
	.uleb128 0x1
	.uleb128 LEHB83-LFB28
	.uleb128 LEHE83-LEHB83
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB84-LFB28
	.uleb128 LEHE84-LEHB84
	.uleb128 L272-LFB28
	.uleb128 0x1
	.uleb128 LEHB85-LFB28
	.uleb128 LEHE85-LEHB85
	.uleb128 0
	.uleb128 0
LLSDACSE28:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr14:
	.long	___gnat_others_value@GOT-L_got_pcr14
LLSDATT28:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_set_context
_aegis_vm_set_context:
LFB29:
	cbz	x0, L290
	stp	x29, x30, [sp, -32]!
LCFI98:
	mov	x29, sp
LCFI99:
LEHB86:
LEHE86:
	str	x19, [sp, 16]
LCFI100:
LEHB87:
	ldrb	w8, [x0, 200]
LEHE87:
	cmp	w8, 1
	bhi	L307
	cbz	w8, L291
	cbz	x1, L282
LEHB88:
	ldp	q31, q30, [x1]
	stp	q31, q30, [x0]
L282:
	cbz	x2, L283
	ldp	q31, q30, [x2]
	stp	q31, q30, [x0, 32]
L283:
	cbz	x3, L284
	ldp	q31, q30, [x3]
	stp	q31, q30, [x0, 64]
LEHE88:
L284:
	cbz	x4, L287
	add	x1, x0, 96
LEHB89:
	ld1	{v30.16b - v31.16b}, [x4]
	st1	{v30.16b - v31.16b}, [x1]
L287:
	str	x5, [x0, 128]
	str	x6, [x0, 136]
	str	x7, [x0, 144]
LEHE89:
	mov	w0, 0
L279:
LEHB90:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI101:
	ret
	.p2align 2,,3
L291:
LCFI102:
	mov	w0, 6
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI103:
	ret
	.p2align 2,,3
L290:
	mov	w0, 6
LEHE90:
	ret
L307:
LCFI104:
	adrp	x0, lC9@PAGE
	mov	w1, 738
	add	x0, x0, lC9@PAGEOFF;
LEHB91:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE91:
L292:
	mov	x19, x0
	cmp	x1, 1
	bne	L308
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB92:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L279
L308:
	bl	__Unwind_Resume
LEHE92:
LFE29:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table15:
	.align	2
LLSDA29:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT29-LLSDATTD29
LLSDATTD29:
	.byte	0x1
	.uleb128 LLSDACSE29-LLSDACSB29
LLSDACSB29:
	.uleb128 LEHB86-LFB29
	.uleb128 LEHE86-LEHB86
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB87-LFB29
	.uleb128 LEHE87-LEHB87
	.uleb128 L292-LFB29
	.uleb128 0x1
	.uleb128 LEHB88-LFB29
	.uleb128 LEHE88-LEHB88
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB89-LFB29
	.uleb128 LEHE89-LEHB89
	.uleb128 L292-LFB29
	.uleb128 0x1
	.uleb128 LEHB90-LFB29
	.uleb128 LEHE90-LEHB90
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB91-LFB29
	.uleb128 LEHE91-LEHB91
	.uleb128 L292-LFB29
	.uleb128 0x1
	.uleb128 LEHB92-LFB29
	.uleb128 LEHE92-LEHB92
	.uleb128 0
	.uleb128 0
LLSDACSE29:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr15:
	.long	___gnat_others_value@GOT-L_got_pcr15
LLSDATT29:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_execute
_aegis_vm_execute:
LFB30:
	sub	sp, sp, #1152
LCFI105:
	adrp	x1, lC11@PAGE
	movi	v25.4s, 0
	sub	sp, sp, #2756608
LCFI106:
	mov	x3, 1103806595072
	movk	x3, 0x101, lsl 48
	mov	x2, 0
	stp	x29, x30, [sp]
LCFI107:
	mov	x29, sp
LCFI108:
LEHB93:
	add	x4, x29, 1376256
	add	x13, x29, 1376256
	add	x4, x4, 3072
	add	x12, x29, 1376256
	ldr	d24, [x1, #lC11@PAGEOFF]
	add	x1, x29, 1376256
	add	x11, x29, 1376256
	add	x1, x1, 3072
	stp	x19, x20, [sp, 16]
LCFI109:
	mov	x20, x0
	add	x0, x29, 2752512
	add	x19, x29, 1376256
	add	x19, x19, 2688
	add	x10, x29, 1376256
	stp	x21, x22, [sp, 32]
	add	x9, x29, 1376256
	add	x8, x29, 1376256
LCFI110:
	mov	x21, x7
	mov	x22, x6
	add	x13, x13, 2720
	add	x12, x12, 2800
	stp	x23, x24, [sp, 48]
	add	x11, x11, 2832
	add	x10, x10, 3232
	add	x9, x9, 3234
	add	x8, x8, 3244
	mov	w7, 1
LCFI111:
	ldr	x23, [x0, 5248]
	add	x0, x19, 432
	str	x25, [sp, 64]
LCFI112:
	stp	xzr, xzr, [x1, -208]
	mov	w1, 16843009
	stp	xzr, xzr, [x4, -192]
	stp	xzr, xzr, [x4, -176]
	stp	xzr, xzr, [x4, -160]
	add	x4, x29, 1376256
	str	q25, [x4, 2928]
	str	q25, [x4, 2944]
	str	q25, [x4, 2960]
	add	x4, x29, 1376256
	add	x4, x4, 3072
	stp	xzr, xzr, [x4, -96]
	stp	xzr, xzr, [x4, -80]
	stp	xzr, xzr, [x4, -64]
	stp	xzr, xzr, [x4, -48]
	add	x4, x29, 1376256
	str	x3, [x4, 3040]
	str	w1, [x4, 3048]
	strb	wzr, [x4, 3052]
	.p2align 5,,15
L310:
	add	x1, x2, x2, lsl 1
	add	x2, x2, 1
	add	x0, x0, 192
	lsl	x1, x1, 6
	add	x6, x19, x1
	add	x5, x13, x1
	add	x4, x12, x1
	add	x3, x11, x1
	stp	xzr, xzr, [x6, 368]
	stp	xzr, xzr, [x5, 368]
	stp	xzr, xzr, [x6, 384]
	stp	xzr, xzr, [x5, 384]
	stp	q25, q25, [x0, -192]
	str	q25, [x0, -160]
	strh	wzr, [x10, x1]
	strb	wzr, [x9, x1]
	stp	xzr, xzr, [x4, 368]
	stp	xzr, xzr, [x3, 368]
	stp	xzr, xzr, [x4, 384]
	stp	xzr, xzr, [x3, 384]
	str	d24, [x0, -77]
	strb	w7, [x0, -69]
	strb	wzr, [x8, x1]
	cmp	x2, 1024
	bne	L310
	add	x3, x19, 196608
	add	x4, x29, 1572864
	mov	x0, 0
	add	x3, x3, 464
	add	x4, x4, 3184
	.p2align 5,,15
L311:
	movi	v26.4s, 0
	add	x1, x0, x0, lsl 3
	add	x0, x0, 1
	add	x2, x19, x1, lsl 3
	lsl	x1, x1, 3
	mov	v27.16b, v26.16b
	add	x2, x2, 196608
	stp	xzr, xzr, [x2, 432]
	stp	xzr, xzr, [x2, 448]
	st1	{v26.16b - v27.16b}, [x3]
	strb	wzr, [x4, x1]
	add	x3, x3, 72
	cmp	x0, 256
	bne	L311
	add	x5, x29, 1593344
	add	x4, x29, 1593344
	add	x3, x29, 1593344
	add	x2, x29, 1593344
	mov	x1, 0
	add	x5, x5, 1080
	add	x4, x4, 1084
	add	x3, x3, 1088
	add	x2, x2, 1096
	.p2align 5,,15
L312:
	add	x0, x1, x1, lsl 1
	add	x1, x1, 1
	lsl	x0, x0, 3
	strh	wzr, [x5, x0]
	str	wzr, [x4, x0]
	str	xzr, [x3, x0]
	strb	wzr, [x2, x0]
	cmp	x1, 1024
	bne	L312
	add	x0, x19, 237568
	add	x4, x29, 1617920
	mov	x1, 0
	add	x0, x0, 2536
	add	x4, x4, 1088
	.p2align 5,,15
L313:
	movi	v28.4s, 0
	add	x3, x1, x1, lsl 4
	add	x1, x1, 1
	lsl	x3, x3, 3
	add	x2, x19, x3
	mov	v29.16b, v28.16b
	add	x2, x2, 237568
	strb	wzr, [x4, x3]
	add	x2, x2, 2497
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	st1	{v28.16b - v29.16b}, [x0]
	stp	q28, q28, [x0, 32]
	stp	q28, q28, [x0, 64]
	add	x0, x0, 136
	cmp	x1, 4096
	bne	L313
	add	x0, x29, 2174976
	add	x4, x29, 2174976
	mov	x1, 0
	add	x0, x0, 1152
	add	x4, x4, 1112
	.p2align 5,,15
L314:
	movi	v30.4s, 0
	add	x3, x1, x1, lsl 4
	add	x1, x1, 1
	lsl	x3, x3, 3
	add	x2, x19, x3
	mov	v31.16b, v30.16b
	add	x2, x2, 794624
	strb	wzr, [x4, x3]
	add	x2, x2, 2521
	stp	xzr, xzr, [x2]
	stp	xzr, xzr, [x2, 16]
	st1	{v30.16b - v31.16b}, [x0]
	stp	q30, q30, [x0, 32]
	stp	q30, q30, [x0, 64]
	add	x0, x0, 136
	cmp	x1, 4096
	bne	L314
	add	x5, x29, 2732032
	add	x4, x29, 2732032
	add	x3, x29, 2732032
	add	x2, x29, 2732032
	mov	x0, 0
	add	x5, x5, 1136
	add	x4, x4, 1140
	add	x3, x3, 1144
	add	x2, x2, 1152
	.p2align 5,,15
L315:
	add	x1, x0, x0, lsl 1
	add	x0, x0, 1
	lsl	x1, x1, 3
	strh	wzr, [x5, x1]
	str	wzr, [x4, x1]
	str	xzr, [x3, x1]
	strb	wzr, [x2, x1]
LEHE93:
	cmp	x0, 1024
	bne	L315
	cbz	x20, L333
LEHB94:
	ldrb	w0, [x20, 200]
LEHE94:
	cmp	w0, 1
	bhi	L348
	cbz	w0, L333
	cmp	x21, 0
	ccmp	x23, 0, 4, ne
	ccmp	x22, 0, 4, ne
	beq	L334
LEHB95:
	ldr	x1, [x20, 152]
	tbnz	x1, #63, L349
	ldr	w0, [x20, 168]
	sub	w3, w0, #1
	add	x24, x29, 1376256
	cmp	w3, 3
	mov	x2, -1
	csel	w0, w0, wzr, cc
	add	x4, x20, 1
	add	x24, x24, 2640
	and	w6, w0, 255
	.p2align 5,,15
L320:
	ldrb	w3, [x4, x2]
	add	x0, x24, x2
	strb	w3, [x0, 1]
	add	x2, x2, 1
	cmp	x2, 31
	bne	L320
	adrp	x5, _aegis_vm_types__u256_zero@GOTPAGE
	ldr	x5, [x5, _aegis_vm_types__u256_zero@GOTPAGEOFF]
	add	x25, x29, 80
	mov	x0, x24
	mov	x8, x25
	mov	x4, x5
	mov	x3, x5
	mov	x2, x5
	bl	_aegis_execution__create_context
	mov	x2, 2552
	mov	x1, x25
	movk	x2, 0x15, lsl 16
	mov	x0, x19
	bl	_memcpy
	mov	x0, x19
	mov	x1, 21000
	bl	_aegis_execution__use_gas
	cmp	w0, 1
	bhi	L350
	cbz	w0, L351
	adrp	x1, _aegis_contract__empty_return@GOTPAGE
	ldr	x1, [x1, _aegis_contract__empty_return@GOTPAGEOFF]
	mov	x2, x24
	mov	x0, x19
	bl	_aegis_execution__finalize_success
	str	xzr, [x22]
	mov	x0, x19
	bl	_aegis_execution__gas_remaining
	tbnz	x0, #63, L352
	str	x0, [x21]
	ldr	x1, [x20, 152]
	sub	x0, x1, x0
	str	x0, [x21]
	str	wzr, [x23]
LEHE95:
	mov	w0, 0
L309:
LEHB96:
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
LCFI113:
	add	sp, sp, 1152
LCFI114:
	add	sp, sp, 2756608
LCFI115:
	ret
	.p2align 2,,3
L333:
LCFI116:
	mov	w0, 6
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
LCFI117:
	add	sp, sp, 1152
LCFI118:
	add	sp, sp, 2756608
LCFI119:
	ret
	.p2align 2,,3
L334:
LCFI120:
	mov	w0, 1
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
LCFI121:
LEHE96:
	add	sp, sp, 1152
LCFI122:
	add	sp, sp, 2756608
LCFI123:
	ret
	.p2align 2,,3
L351:
LCFI124:
LEHB97:
	ldr	x0, [x20, 152]
	str	x0, [x21]
	mov	w0, 2
	str	w0, [x23]
LEHE97:
	mov	w0, 4
LEHB98:
	ldr	x25, [sp, 64]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
	ldp	x23, x24, [sp, 48]
LCFI125:
LEHE98:
	add	sp, sp, 1152
LCFI126:
	add	sp, sp, 2756608
LCFI127:
	ret
L350:
LCFI128:
	adrp	x0, lC9@PAGE
	mov	w1, 853
	add	x0, x0, lC9@PAGEOFF;
LEHB99:
	bl	___gnat_rcheck_CE_Invalid_Data
L352:
	adrp	x0, lC9@PAGE
	mov	w1, 870
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE99:
L348:
	adrp	x0, lC9@PAGE
	mov	w1, 830
	add	x0, x0, lC9@PAGEOFF;
LEHB100:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE100:
L349:
	adrp	x0, lC9@PAGE
	mov	w1, 841
	add	x0, x0, lC9@PAGEOFF;
LEHB101:
	bl	___gnat_rcheck_CE_Range_Check
LEHE101:
L338:
	mov	x19, x0
	cmp	x1, 1
	bne	L331
	bl	___gnat_begin_handler_v1
	mov	x20, x0
	cbz	x23, L327
L326:
	mov	w0, 6
LEHB102:
	str	w0, [x23]
LEHE102:
L327:
	mov	x1, x20
	mov	x0, x19
	mov	x2, 0
LEHB103:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L309
L336:
	mov	x19, x0
	cmp	x1, 1
	bne	L331
	bl	___gnat_begin_handler_v1
	mov	x20, x0
	b	L326
L331:
	mov	x0, x19
	bl	__Unwind_Resume
L337:
	mov	x1, x20
	mov	x20, x0
	mov	x2, x20
	mov	x0, x19
	bl	___gnat_end_handler_v1
	mov	x0, x20
	bl	__Unwind_Resume
LEHE103:
LFE30:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table16:
	.align	2
LLSDA30:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT30-LLSDATTD30
LLSDATTD30:
	.byte	0x1
	.uleb128 LLSDACSE30-LLSDACSB30
LLSDACSB30:
	.uleb128 LEHB93-LFB30
	.uleb128 LEHE93-LEHB93
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB94-LFB30
	.uleb128 LEHE94-LEHB94
	.uleb128 L338-LFB30
	.uleb128 0x1
	.uleb128 LEHB95-LFB30
	.uleb128 LEHE95-LEHB95
	.uleb128 L336-LFB30
	.uleb128 0x1
	.uleb128 LEHB96-LFB30
	.uleb128 LEHE96-LEHB96
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB97-LFB30
	.uleb128 LEHE97-LEHB97
	.uleb128 L336-LFB30
	.uleb128 0x1
	.uleb128 LEHB98-LFB30
	.uleb128 LEHE98-LEHB98
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB99-LFB30
	.uleb128 LEHE99-LEHB99
	.uleb128 L336-LFB30
	.uleb128 0x1
	.uleb128 LEHB100-LFB30
	.uleb128 LEHE100-LEHB100
	.uleb128 L338-LFB30
	.uleb128 0x1
	.uleb128 LEHB101-LFB30
	.uleb128 LEHE101-LEHB101
	.uleb128 L336-LFB30
	.uleb128 0x1
	.uleb128 LEHB102-LFB30
	.uleb128 LEHE102-LEHB102
	.uleb128 L337-LFB30
	.uleb128 0
	.uleb128 LEHB103-LFB30
	.uleb128 LEHE103-LEHB103
	.uleb128 0
	.uleb128 0
LLSDACSE30:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr16:
	.long	___gnat_others_value@GOT-L_got_pcr16
LLSDATT30:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_load_code
_aegis_vm_load_code:
LFB33:
	cbz	x0, L360
	stp	x29, x30, [sp, -32]!
LCFI129:
	mov	x29, sp
LCFI130:
LEHB104:
LEHE104:
	str	x19, [sp, 16]
LCFI131:
LEHB105:
	ldrb	w3, [x0, 200]
	cmp	w3, 1
	bhi	L370
	cbz	w3, L361
	cmp	x1, 0
	ccmp	x2, 0, 4, ne
	beq	L362
	cmp	x2, 524288
	bhi	L363
	mov	x3, 0
	and	x6, x2, 4294967295
	.p2align 5,,15
L357:
	ldr	x4, [x0, 176]
	cbz	x4, L371
	ldrb	w5, [x1, x3]
	strb	w5, [x4, x3]
	add	x3, x3, 1
	cmp	x6, x3
	bne	L357
	str	x2, [x0, 184]
	mov	w1, 1
	strb	w1, [x0, 201]
LEHE105:
	mov	w0, 0
L353:
LEHB106:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI132:
	ret
	.p2align 2,,3
L362:
LCFI133:
	mov	w0, 1
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI134:
	ret
	.p2align 2,,3
L361:
LCFI135:
	mov	w0, 6
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI136:
	ret
	.p2align 2,,3
L360:
	mov	w0, 6
	ret
	.p2align 2,,3
L363:
LCFI137:
	mov	w0, 11
LEHE106:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI138:
	ret
L371:
LCFI139:
	adrp	x0, lC9@PAGE
	mov	w1, 908
	add	x0, x0, lC9@PAGEOFF;
LEHB107:
	bl	___gnat_rcheck_CE_Access_Check
L370:
	adrp	x0, lC9@PAGE
	mov	w1, 890
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE107:
L364:
	mov	x19, x0
	cmp	x1, 1
	bne	L372
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB108:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L353
L372:
	bl	__Unwind_Resume
LEHE108:
LFE33:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table17:
	.align	2
LLSDA33:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT33-LLSDATTD33
LLSDATTD33:
	.byte	0x1
	.uleb128 LLSDACSE33-LLSDACSB33
LLSDACSB33:
	.uleb128 LEHB104-LFB33
	.uleb128 LEHE104-LEHB104
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB105-LFB33
	.uleb128 LEHE105-LEHB105
	.uleb128 L364-LFB33
	.uleb128 0x1
	.uleb128 LEHB106-LFB33
	.uleb128 LEHE106-LEHB106
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB107-LFB33
	.uleb128 LEHE107-LEHB107
	.uleb128 L364-LFB33
	.uleb128 0x1
	.uleb128 LEHB108-LFB33
	.uleb128 LEHE108-LEHB108
	.uleb128 0
	.uleb128 0
LLSDACSE33:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr17:
	.long	___gnat_others_value@GOT-L_got_pcr17
LLSDATT33:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_execute_with_context
_aegis_vm_execute_with_context:
LFB34:
	sub	sp, sp, #1120
LCFI140:
	adrp	x2, lC11@PAGE
	movi	v25.4s, 0
	sub	sp, sp, #2756608
LCFI141:
	mov	x4, 0
	mov	w8, 1
	stp	x29, x30, [sp]
LCFI142:
	mov	x29, sp
LCFI143:
LEHB109:
	add	x5, x29, 1376256
	add	x14, x29, 1376256
	add	x5, x5, 3072
	add	x13, x29, 1376256
	ldr	d24, [x2, #lC11@PAGEOFF]
	add	x2, x29, 1376256
	add	x12, x29, 1376256
	add	x2, x2, 3072
	stp	x19, x20, [sp, 16]
LCFI144:
	add	x19, x29, 1376256
	add	x19, x19, 2656
	add	x11, x29, 1376256
	add	x10, x29, 1376256
	add	x9, x29, 1376256
	stp	x21, x22, [sp, 32]
LCFI145:
	mov	x20, x1
	add	x3, x19, 432
	mov	w1, 16843009
	add	x14, x14, 2688
	add	x13, x13, 2768
	add	x12, x12, 2800
	stp	xzr, xzr, [x2, -240]
	mov	x2, 1103806595072
	add	x11, x11, 3200
	movk	x2, 0x101, lsl 48
	add	x10, x10, 3202
	add	x9, x9, 3212
	stp	xzr, xzr, [x5, -224]
	stp	xzr, xzr, [x5, -208]
	stp	xzr, xzr, [x5, -192]
	add	x5, x29, 1376256
	str	q25, [x5, 2896]
	str	q25, [x5, 2912]
	str	q25, [x5, 2928]
	add	x5, x29, 1376256
	add	x5, x5, 3072
	stp	xzr, xzr, [x5, -128]
	stp	xzr, xzr, [x5, -112]
	stp	xzr, xzr, [x5, -96]
	stp	xzr, xzr, [x5, -80]
	add	x5, x29, 1376256
	str	x2, [x5, 3008]
	str	w1, [x5, 3016]
	strb	wzr, [x5, 3020]
	.p2align 5,,15
L374:
	add	x2, x4, x4, lsl 1
	add	x4, x4, 1
	add	x3, x3, 192
	lsl	x2, x2, 6
	add	x7, x19, x2
	add	x6, x14, x2
	add	x5, x13, x2
	add	x1, x12, x2
	stp	xzr, xzr, [x7, 368]
	stp	xzr, xzr, [x6, 368]
	stp	xzr, xzr, [x7, 384]
	stp	xzr, xzr, [x6, 384]
	stp	q25, q25, [x3, -192]
	str	q25, [x3, -160]
	strh	wzr, [x11, x2]
	strb	wzr, [x10, x2]
	stp	xzr, xzr, [x5, 368]
	stp	xzr, xzr, [x1, 368]
	stp	xzr, xzr, [x5, 384]
	stp	xzr, xzr, [x1, 384]
	str	d24, [x3, -77]
	strb	w8, [x3, -69]
	strb	wzr, [x9, x2]
	cmp	x4, 1024
	bne	L374
	add	x4, x19, 196608
	add	x5, x29, 1572864
	mov	x1, 0
	add	x4, x4, 464
	add	x5, x5, 3152
	.p2align 5,,15
L375:
	movi	v26.4s, 0
	add	x2, x1, x1, lsl 3
	add	x1, x1, 1
	add	x3, x19, x2, lsl 3
	lsl	x2, x2, 3
	mov	v27.16b, v26.16b
	add	x3, x3, 196608
	stp	xzr, xzr, [x3, 432]
	stp	xzr, xzr, [x3, 448]
	st1	{v26.16b - v27.16b}, [x4]
	strb	wzr, [x5, x2]
	add	x4, x4, 72
	cmp	x1, 256
	bne	L375
	add	x6, x29, 1593344
	add	x5, x29, 1593344
	add	x4, x29, 1593344
	add	x3, x29, 1593344
	mov	x1, 0
	add	x6, x6, 1048
	add	x5, x5, 1052
	add	x4, x4, 1056
	add	x3, x3, 1064
	.p2align 5,,15
L376:
	add	x2, x1, x1, lsl 1
	add	x1, x1, 1
	lsl	x2, x2, 3
	strh	wzr, [x6, x2]
	str	wzr, [x5, x2]
	str	xzr, [x4, x2]
	strb	wzr, [x3, x2]
	cmp	x1, 1024
	bne	L376
	add	x1, x19, 237568
	add	x5, x29, 1617920
	mov	x2, 0
	add	x1, x1, 2536
	add	x5, x5, 1056
	.p2align 5,,15
L377:
	movi	v28.4s, 0
	add	x4, x2, x2, lsl 4
	add	x2, x2, 1
	lsl	x4, x4, 3
	add	x3, x19, x4
	mov	v29.16b, v28.16b
	add	x3, x3, 237568
	strb	wzr, [x5, x4]
	add	x3, x3, 2497
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	st1	{v28.16b - v29.16b}, [x1]
	stp	q28, q28, [x1, 32]
	stp	q28, q28, [x1, 64]
	add	x1, x1, 136
	cmp	x2, 4096
	bne	L377
	add	x1, x29, 2174976
	add	x5, x29, 2174976
	mov	x2, 0
	add	x1, x1, 1120
	add	x5, x5, 1080
	.p2align 5,,15
L378:
	movi	v30.4s, 0
	add	x4, x2, x2, lsl 4
	add	x2, x2, 1
	lsl	x4, x4, 3
	add	x3, x19, x4
	mov	v31.16b, v30.16b
	add	x3, x3, 794624
	strb	wzr, [x5, x4]
	add	x3, x3, 2521
	stp	xzr, xzr, [x3]
	stp	xzr, xzr, [x3, 16]
	st1	{v30.16b - v31.16b}, [x1]
	stp	q30, q30, [x1, 32]
	stp	q30, q30, [x1, 64]
	add	x1, x1, 136
	cmp	x2, 4096
	bne	L378
	add	x6, x29, 2732032
	add	x5, x29, 2732032
	add	x4, x29, 2732032
	add	x3, x29, 2732032
	mov	x1, 0
	add	x6, x6, 1104
	add	x5, x5, 1108
	add	x4, x4, 1112
	add	x3, x3, 1120
	.p2align 5,,15
L379:
	add	x2, x1, x1, lsl 1
	add	x1, x1, 1
	lsl	x2, x2, 3
	strh	wzr, [x6, x2]
	str	wzr, [x5, x2]
	str	xzr, [x4, x2]
	strb	wzr, [x3, x2]
LEHE109:
	cmp	x1, 1024
	bne	L379
	cbz	x0, L393
LEHB110:
	ldrb	w1, [x0, 200]
	cmp	w1, 1
	bhi	L407
	cbz	w1, L393
	ldrb	w1, [x0, 201]
	cmp	w1, 1
	bhi	L408
	cbz	w1, L393
	cbz	x20, L395
	add	x1, x29, 2752512
	ldr	x1, [x1, 5224]
	cmp	x1, 0
	add	x1, x29, 2752512
	ldr	x1, [x1, 5240]
	ccmp	x1, 0, 4, ne
	add	x1, x29, 2752512
	ldr	x1, [x1, 5248]
	ccmp	x1, 0, 4, ne
	beq	L395
	ldr	x1, [x20, 64]
	str	x1, [x0, 152]
	ldr	w2, [x20, 104]
	str	w2, [x0, 168]
	ldr	q30, [x20]
	ldr	q31, [x20, 16]
	str	q30, [x0]
	str	q31, [x0, 16]
	add	x3, x0, 32
	add	x4, x20, 32
	ldr	q30, [x20, 32]
	ldr	q31, [x4, 16]
	str	q30, [x0, 32]
	str	q31, [x3, 16]
	tbnz	x1, #63, L409
	sub	w3, w2, #1
	add	x21, x29, 1376256
	cmp	w3, 3
	mov	x0, -1
	csel	w2, w2, wzr, cc
	add	x4, x20, 1
	add	x21, x21, 2608
	and	w6, w2, 255
	.p2align 5,,15
L385:
	ldrb	w3, [x4, x0]
	add	x2, x21, x0
	strb	w3, [x2, 1]
	add	x0, x0, 1
	cmp	x0, 31
	bne	L385
	adrp	x5, _aegis_vm_types__u256_zero@GOTPAGE
	ldr	x5, [x5, _aegis_vm_types__u256_zero@GOTPAGEOFF]
	add	x22, x29, 48
	mov	x0, x21
	mov	x8, x22
	mov	x4, x5
	mov	x3, x5
	mov	x2, x5
	bl	_aegis_execution__create_context
	mov	x2, 2552
	mov	x1, x22
	movk	x2, 0x15, lsl 16
	mov	x0, x19
	bl	_memcpy
	mov	x0, x19
	mov	x1, 21000
	bl	_aegis_execution__use_gas
	cmp	w0, 1
	bhi	L410
	cbz	w0, L411
	adrp	x1, _aegis_contract__empty_return@GOTPAGE
	ldr	x1, [x1, _aegis_contract__empty_return@GOTPAGEOFF]
	mov	x2, x21
	mov	x0, x19
	bl	_aegis_execution__finalize_success
	add	x0, x29, 2752512
	ldr	x0, [x0, 5224]
	str	xzr, [x0]
	add	x0, x29, 2752512
	ldr	x0, [x0, 5240]
	str	xzr, [x0]
	mov	x0, x19
	bl	_aegis_execution__gas_remaining
	tbnz	x0, #63, L412
	ldr	x1, [x20, 64]
	sub	x0, x1, x0
	add	x1, x29, 2752512
	ldr	x1, [x1, 5248]
	str	x0, [x1]
LEHE110:
	mov	w0, 0
LEHB111:
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
LCFI146:
	add	sp, sp, 1120
LCFI147:
	add	sp, sp, 2756608
LCFI148:
	ret
	.p2align 2,,3
L395:
LCFI149:
	mov	w0, 1
L373:
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
LCFI150:
	add	sp, sp, 1120
LCFI151:
	add	sp, sp, 2756608
LCFI152:
	ret
	.p2align 2,,3
L393:
LCFI153:
	mov	w0, 6
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
LCFI154:
LEHE111:
	add	sp, sp, 1120
LCFI155:
	add	sp, sp, 2756608
LCFI156:
	ret
	.p2align 2,,3
L411:
LCFI157:
LEHB112:
	ldr	x0, [x20, 64]
	add	x1, x29, 2752512
	ldr	x1, [x1, 5248]
	str	x0, [x1]
LEHE112:
	mov	w0, 4
LEHB113:
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	ldp	x21, x22, [sp, 32]
LCFI158:
LEHE113:
	add	sp, sp, 1120
LCFI159:
	add	sp, sp, 2756608
LCFI160:
	ret
L412:
LCFI161:
	adrp	x0, lC9@PAGE
	mov	w1, 1021
	add	x0, x0, lC9@PAGEOFF;
LEHB114:
	bl	___gnat_rcheck_CE_Invalid_Data
L408:
	adrp	x0, lC9@PAGE
	mov	w1, 969
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L407:
	adrp	x0, lC9@PAGE
	mov	w1, 965
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L409:
	adrp	x0, lC9@PAGE
	mov	w1, 990
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L410:
	adrp	x0, lC9@PAGE
	mov	w1, 1002
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE114:
L397:
	mov	x19, x0
	cmp	x1, 1
	bne	L413
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB115:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L373
L413:
	bl	__Unwind_Resume
LEHE115:
LFE34:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table18:
	.align	2
LLSDA34:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT34-LLSDATTD34
LLSDATTD34:
	.byte	0x1
	.uleb128 LLSDACSE34-LLSDACSB34
LLSDACSB34:
	.uleb128 LEHB109-LFB34
	.uleb128 LEHE109-LEHB109
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB110-LFB34
	.uleb128 LEHE110-LEHB110
	.uleb128 L397-LFB34
	.uleb128 0x1
	.uleb128 LEHB111-LFB34
	.uleb128 LEHE111-LEHB111
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB112-LFB34
	.uleb128 LEHE112-LEHB112
	.uleb128 L397-LFB34
	.uleb128 0x1
	.uleb128 LEHB113-LFB34
	.uleb128 LEHE113-LEHB113
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB114-LFB34
	.uleb128 LEHE114-LEHB114
	.uleb128 L397-LFB34
	.uleb128 0x1
	.uleb128 LEHB115-LFB34
	.uleb128 LEHE115-LEHB115
	.uleb128 0
	.uleb128 0
LLSDACSE34:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr18:
	.long	___gnat_others_value@GOT-L_got_pcr18
LLSDATT34:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_set_state
_aegis_vm_set_state:
LFB37:
	cbz	x0, L439
	stp	x29, x30, [sp, -32]!
LCFI162:
	mov	x29, sp
LCFI163:
LEHB116:
LEHE116:
	mov	x5, x0
	str	x19, [sp, 16]
LCFI164:
LEHB117:
	ldrb	w0, [x0, 200]
LEHE117:
	cmp	w0, 1
	bhi	L458
	cbz	w0, L440
	sub	x6, x2, #1
	mov	w0, 1
	cmp	x6, 255
	ccmp	x1, 0, 4, ls
	bne	L459
L414:
LEHB118:
LEHE118:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI165:
	ret
	.p2align 2,,3
L459:
LCFI166:
LEHB119:
	ldr	x10, [x5, 192]
	cbz	x10, L460
	mov	x0, 0
	mov	x7, x10
	mov	x8, 33036
	b	L427
	.p2align 2,,3
L419:
	add	x0, x0, 1
	add	x7, x7, x8
	cmp	x0, 1024
	beq	L461
L427:
	madd	x5, x0, x8, x10
	add	x5, x5, 32768
	ldrb	w5, [x5, 264]
	cmp	w5, 1
	bhi	L462
	cbz	w5, L419
	madd	x5, x0, x8, x10
	ldr	w6, [x5, 256]
	tbnz	w6, #31, L463
	cmp	w6, w2
	bne	L419
	mov	x5, 0
	uxtw	x9, w6
	.p2align 5,,15
L421:
	ldrb	w6, [x1, x5]
	and	w6, w6, 255
	ldrb	w11, [x7, x5]
	cmp	w6, w11
	bne	L419
	add	x5, x5, 1
	cmp	x5, x9
	bne	L421
	cmp	x3, 0
	ccmp	x4, 0, 4, ne
	beq	L464
	mov	x1, 2147483647
	cmp	x4, x1
	bhi	L465
	mov	w5, 33036
	mov	x1, 0
	sub	w6, w4, #1
	mov	w8, 32767
	umaddl	x5, w0, w5, x10
	cmp	w4, 32768
	cset	w7, gt
	add	x5, x5, 260
	.p2align 5,,15
L424:
	ldrb	w2, [x3, x1]
	strb	w2, [x5, x1]
	cmp	x1, x6
	beq	L466
	add	w2, w1, 1
	cmp	w7, 0
	ccmp	w2, w8, 4, ne
	add	x1, x1, 1
	ble	L424
	adrp	x0, lC9@PAGE
	mov	w1, 1081
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
	.p2align 2,,3
L461:
	mov	x0, 0
	mov	x6, 33036
	b	L436
	.p2align 2,,3
L429:
	add	x0, x0, 1
	cmp	x0, 1024
	beq	L467
L436:
	madd	x5, x0, x6, x10
	add	x5, x5, 32768
	ldrb	w5, [x5, 264]
	cmp	w5, 1
	bhi	L468
	cbnz	w5, L429
	mov	w6, 33036
	mov	x5, 0
	and	x8, x2, 4294967295
	umaddl	x6, w0, w6, x10
	.p2align 5,,15
L430:
	ldrb	w7, [x1, x5]
	strb	w7, [x6, x5]
	add	x5, x5, 1
	cmp	x8, x5
	bne	L430
	mov	x1, 33036
	madd	x1, x0, x1, x10
	str	w2, [x1, 256]
	cmp	x3, 0
	ccmp	x4, 0, 4, ne
	beq	L431
	mov	x1, 2147483647
	cmp	x4, x1
	bhi	L469
	add	x2, x6, 260
	mov	x1, 0
	sub	w6, w4, #1
	mov	w8, 32767
	cmp	w4, 32768
	cset	w7, gt
	.p2align 5,,15
L433:
	ldrb	w5, [x3, x1]
	strb	w5, [x2, x1]
	cmp	x1, x6
	beq	L470
	add	w5, w1, 1
	cmp	w7, 0
	ccmp	w5, w8, 4, ne
	add	x1, x1, 1
	ble	L433
	adrp	x0, lC9@PAGE
	mov	w1, 1112
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LEHE119:
	.p2align 2,,3
L440:
	mov	w0, 6
LEHB120:
LEHE120:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI167:
	ret
L470:
LCFI168:
	mov	x1, 33036
	madd	x1, x0, x1, x10
	add	x1, x1, 32768
LEHB121:
	str	w4, [x1, 260]
L435:
	mov	x1, 33036
	mov	w2, 1
	madd	x0, x0, x1, x10
	add	x0, x0, 32768
	strb	w2, [x0, 264]
LEHE121:
	mov	w0, 0
L471:
LEHB122:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI169:
	ret
L439:
	mov	w0, 6
LEHE122:
	ret
L431:
LCFI170:
	mov	x1, 33036
	madd	x1, x0, x1, x10
	add	x1, x1, 32768
LEHB123:
	str	wzr, [x1, 260]
	b	L435
L464:
	mov	x1, 33036
	madd	x1, x0, x1, x10
	add	x1, x1, 32768
	strb	wzr, [x1, 264]
	mov	x1, 33036
	madd	x1, x0, x1, x10
	str	wzr, [x1, 256]
	mov	x1, 33036
	madd	x0, x0, x1, x10
	add	x0, x0, 32768
	str	wzr, [x0, 260]
	mov	w0, 0
	b	L471
L467:
	mov	w0, 5
	b	L414
L466:
	mov	x1, 33036
	madd	x0, x0, x1, x10
	add	x0, x0, 32768
	str	w4, [x0, 260]
	mov	w0, 0
	b	L471
L462:
	adrp	x0, lC9@PAGE
	mov	w1, 1052
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L463:
	adrp	x0, lC9@PAGE
	mov	w1, 1054
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L468:
	adrp	x0, lC9@PAGE
	mov	w1, 1095
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L460:
	adrp	x0, lC9@PAGE
	mov	w1, 1051
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L458:
	adrp	x0, lC9@PAGE
	mov	w1, 1038
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L469:
	adrp	x0, lC9@PAGE
	mov	w1, 1108
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L465:
	adrp	x0, lC9@PAGE
	mov	w1, 1077
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LEHE123:
L442:
	mov	x19, x0
	cmp	x1, 1
	bne	L472
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB124:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L414
L472:
	bl	__Unwind_Resume
LEHE124:
LFE37:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table19:
	.align	2
LLSDA37:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT37-LLSDATTD37
LLSDATTD37:
	.byte	0x1
	.uleb128 LLSDACSE37-LLSDACSB37
LLSDACSB37:
	.uleb128 LEHB116-LFB37
	.uleb128 LEHE116-LEHB116
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB117-LFB37
	.uleb128 LEHE117-LEHB117
	.uleb128 L442-LFB37
	.uleb128 0x1
	.uleb128 LEHB118-LFB37
	.uleb128 LEHE118-LEHB118
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB119-LFB37
	.uleb128 LEHE119-LEHB119
	.uleb128 L442-LFB37
	.uleb128 0x1
	.uleb128 LEHB120-LFB37
	.uleb128 LEHE120-LEHB120
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB121-LFB37
	.uleb128 LEHE121-LEHB121
	.uleb128 L442-LFB37
	.uleb128 0x1
	.uleb128 LEHB122-LFB37
	.uleb128 LEHE122-LEHB122
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB123-LFB37
	.uleb128 LEHE123-LEHB123
	.uleb128 L442-LFB37
	.uleb128 0x1
	.uleb128 LEHB124-LFB37
	.uleb128 LEHE124-LEHB124
	.uleb128 0
	.uleb128 0
LLSDACSE37:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr19:
	.long	___gnat_others_value@GOT-L_got_pcr19
LLSDATT37:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_get_state
_aegis_vm_get_state:
LFB38:
	cbz	x0, L494
	stp	x29, x30, [sp, -32]!
LCFI171:
	mov	x29, sp
LCFI172:
LEHB125:
LEHE125:
	str	x19, [sp, 16]
LCFI173:
LEHB126:
	ldrb	w5, [x0, 200]
	cmp	w5, 1
	bhi	L512
	cbz	w5, L495
	cmp	x2, 0
	ccmp	x4, 0, 4, ne
	ccmp	x1, 0, 4, ne
	beq	L496
	ldr	x12, [x0, 192]
	cbz	x12, L513
	mov	x6, 0
	mov	x8, x12
	mov	x10, 33036
	mov	x14, 2147483647
	b	L490
	.p2align 2,,3
L478:
	add	x6, x6, 1
	add	x8, x8, x10
	cmp	x6, 1024
	beq	L514
L490:
	madd	x5, x6, x10, x12
	add	x5, x5, 32768
	ldrb	w5, [x5, 264]
	cmp	w5, 1
	bhi	L515
	cbz	w5, L478
	cmp	x2, x14
	bhi	L516
	madd	x5, x6, x10, x12
	ldr	w5, [x5, 256]
	tbnz	w5, #31, L517
	cmp	w5, w2
	bne	L478
	cmp	w5, 256
	sub	w9, w5, #1
	cset	w13, gt
	mov	x5, 0
	.p2align 5,,15
L481:
	ldrb	w7, [x1, x5]
	and	w7, w7, 255
	ldrb	w11, [x8, x5]
	cmp	w7, w11
	bne	L478
	cmp	x5, x9
	beq	L518
	add	w7, w5, 1
	add	x5, x5, 1
	cmp	w7, 255
	ccmp	w13, 0, 4, gt
	beq	L481
	adrp	x0, lC9@PAGE
	mov	w1, 1164
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
LEHE126:
	.p2align 2,,3
L496:
	mov	w0, 1
L473:
LEHB127:
LEHE127:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI174:
	ret
L514:
LCFI175:
LEHB128:
	str	xzr, [x4]
LEHE128:
	mov	w0, 0
L524:
LEHB129:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI176:
	ret
L495:
LCFI177:
	mov	w0, 6
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI178:
	ret
L494:
	mov	w0, 6
LEHE129:
	ret
	.p2align 2,,3
L518:
LCFI179:
	mov	x1, 33036
	madd	x1, x6, x1, x12
	add	x1, x1, 32768
LEHB130:
	ldr	w5, [x1, 260]
	tbnz	w5, #31, L519
	cmp	x3, 0
	ccmp	w5, 0, 4, ne
	beq	L484
	mov	x7, 33036
	mov	x2, 0
	uxtw	x5, w5
	mul	x7, x6, x7
	.p2align 5,,15
L487:
	ldr	x1, [x0, 192]
	cbz	x1, L520
	cmp	x2, 32768
	beq	L521
	add	x1, x1, x7
	add	x1, x1, x2
	ldrb	w1, [x1, 260]
	strb	w1, [x3, x2]
	add	x2, x2, 1
	cmp	x2, x5
	bne	L487
L484:
	ldr	x1, [x0, 192]
	cbz	x1, L522
	mov	x0, 33036
	madd	x0, x6, x0, x1
	add	x0, x0, 32768
	ldr	w0, [x0, 260]
	tbnz	w0, #31, L523
	sxtw	x0, w0
	str	x0, [x4]
	mov	w0, 0
	b	L524
L515:
	adrp	x0, lC9@PAGE
	mov	w1, 1155
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L517:
	adrp	x0, lC9@PAGE
	mov	w1, 1156
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L516:
	adrp	x0, lC9@PAGE
	mov	w1, 1156
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
L521:
	adrp	x0, lC9@PAGE
	mov	w1, 1180
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Index_Check
L520:
	adrp	x0, lC9@PAGE
	mov	w1, 1180
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L523:
	adrp	x0, lC9@PAGE
	mov	w1, 1184
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L522:
	adrp	x0, lC9@PAGE
	mov	w1, 1184
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L519:
	adrp	x0, lC9@PAGE
	mov	w1, 1173
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L513:
	adrp	x0, lC9@PAGE
	mov	w1, 1154
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Access_Check
L512:
	adrp	x0, lC9@PAGE
	mov	w1, 1141
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE130:
L497:
	mov	x19, x0
	cmp	x1, 1
	bne	L525
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB131:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L473
L525:
	bl	__Unwind_Resume
LEHE131:
LFE38:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table20:
	.align	2
LLSDA38:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT38-LLSDATTD38
LLSDATTD38:
	.byte	0x1
	.uleb128 LLSDACSE38-LLSDACSB38
LLSDACSB38:
	.uleb128 LEHB125-LFB38
	.uleb128 LEHE125-LEHB125
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB126-LFB38
	.uleb128 LEHE126-LEHB126
	.uleb128 L497-LFB38
	.uleb128 0x1
	.uleb128 LEHB127-LFB38
	.uleb128 LEHE127-LEHB127
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB128-LFB38
	.uleb128 LEHE128-LEHB128
	.uleb128 L497-LFB38
	.uleb128 0x1
	.uleb128 LEHB129-LFB38
	.uleb128 LEHE129-LEHB129
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB130-LFB38
	.uleb128 LEHE130-LEHB130
	.uleb128 L497-LFB38
	.uleb128 0x1
	.uleb128 LEHB131-LFB38
	.uleb128 LEHE131-LEHB131
	.uleb128 0
	.uleb128 0
LLSDACSE38:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr20:
	.long	___gnat_others_value@GOT-L_got_pcr20
LLSDATT38:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_reset
_aegis_vm_reset:
LFB39:
	cbz	x0, L532
	stp	x29, x30, [sp, -32]!
LCFI180:
	mov	x29, sp
LCFI181:
LEHB132:
LEHE132:
	str	x19, [sp, 16]
LCFI182:
LEHB133:
	ldrb	w1, [x0, 200]
	cmp	w1, 1
	bhi	L542
	cbz	w1, L533
	str	xzr, [x0, 184]
	strb	wzr, [x0, 201]
	ldr	x3, [x0, 192]
	mov	x1, 0
	mov	x4, 33036
	cbz	x3, L543
	.p2align 5,,15
L529:
	madd	x2, x1, x4, x3
	add	x2, x2, 32768
	strb	wzr, [x2, 264]
	madd	x2, x1, x4, x3
	str	wzr, [x2, 256]
	add	x2, x2, 32768
	str	wzr, [x2, 260]
	add	x1, x1, 1
	cmp	x1, 1024
	bne	L529
	str	xzr, [x0, 160]
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	add	x1, x0, 32
	stp	xzr, xzr, [x0, 32]
	stp	xzr, xzr, [x1, 16]
	add	x1, x0, 64
	stp	xzr, xzr, [x0, 64]
	stp	xzr, xzr, [x1, 16]
	str	xzr, [x0, 96]
	str	xzr, [x0, 104]
	str	xzr, [x0, 112]
	str	xzr, [x0, 120]
LEHE133:
	mov	w0, 0
L526:
LEHB134:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI183:
	ret
L533:
LCFI184:
	mov	w0, 6
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI185:
	ret
L532:
	mov	w0, 6
LEHE134:
	ret
L543:
LCFI186:
	adrp	x0, lC9@PAGE
	mov	w1, 1211
	add	x0, x0, lC9@PAGEOFF;
LEHB135:
	bl	___gnat_rcheck_CE_Access_Check
L542:
	adrp	x0, lC9@PAGE
	mov	w1, 1202
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE135:
L535:
	mov	x19, x0
	cmp	x1, 1
	bne	L544
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB136:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L526
L544:
	bl	__Unwind_Resume
LEHE136:
LFE39:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table21:
	.align	2
LLSDA39:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT39-LLSDATTD39
LLSDATTD39:
	.byte	0x1
	.uleb128 LLSDACSE39-LLSDACSB39
LLSDACSB39:
	.uleb128 LEHB132-LFB39
	.uleb128 LEHE132-LEHB132
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB133-LFB39
	.uleb128 LEHE133-LEHB133
	.uleb128 L535-LFB39
	.uleb128 0x1
	.uleb128 LEHB134-LFB39
	.uleb128 LEHE134-LEHB134
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB135-LFB39
	.uleb128 LEHE135-LEHB135
	.uleb128 L535-LFB39
	.uleb128 0x1
	.uleb128 LEHB136-LFB39
	.uleb128 LEHE136-LEHB136
	.uleb128 0
	.uleb128 0
LLSDACSE39:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr21:
	.long	___gnat_others_value@GOT-L_got_pcr21
LLSDATT39:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_gas_remaining
_aegis_vm_gas_remaining:
LFB40:
	cbz	x0, L552
	stp	x29, x30, [sp, -32]!
LCFI187:
	mov	x29, sp
LCFI188:
LEHB137:
LEHE137:
	str	x19, [sp, 16]
LCFI189:
LEHB138:
	ldrb	w2, [x0, 200]
	cmp	w2, 1
	bhi	L560
	cbz	w2, L553
	cbz	x1, L554
	ldr	x2, [x0, 160]
	ldr	x0, [x0, 152]
	cmp	x2, x0
	bls	L548
	str	xzr, [x1]
LEHE138:
L549:
	mov	w0, 0
L545:
LEHB139:
LEHE139:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI190:
	ret
	.p2align 2,,3
L548:
LCFI191:
	sub	x0, x0, x2
LEHB140:
	str	x0, [x1]
LEHE140:
	b	L549
	.p2align 2,,3
L553:
	mov	w0, 6
LEHB141:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI192:
	ret
	.p2align 2,,3
L554:
LCFI193:
	mov	w0, 1
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI194:
	ret
	.p2align 2,,3
L552:
	mov	w0, 6
LEHE141:
	ret
L560:
LCFI195:
	adrp	x0, lC9@PAGE
	mov	w1, 1237
	add	x0, x0, lC9@PAGEOFF;
LEHB142:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE142:
L555:
	mov	x19, x0
	cmp	x1, 1
	bne	L561
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB143:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L545
L561:
	bl	__Unwind_Resume
LEHE143:
LFE40:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table22:
	.align	2
LLSDA40:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT40-LLSDATTD40
LLSDATTD40:
	.byte	0x1
	.uleb128 LLSDACSE40-LLSDACSB40
LLSDACSB40:
	.uleb128 LEHB137-LFB40
	.uleb128 LEHE137-LEHB137
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB138-LFB40
	.uleb128 LEHE138-LEHB138
	.uleb128 L555-LFB40
	.uleb128 0x1
	.uleb128 LEHB139-LFB40
	.uleb128 LEHE139-LEHB139
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB140-LFB40
	.uleb128 LEHE140-LEHB140
	.uleb128 L555-LFB40
	.uleb128 0x1
	.uleb128 LEHB141-LFB40
	.uleb128 LEHE141-LEHB141
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB142-LFB40
	.uleb128 LEHE142-LEHB142
	.uleb128 L555-LFB40
	.uleb128 0x1
	.uleb128 LEHB143-LFB40
	.uleb128 LEHE143-LEHB143
	.uleb128 0
	.uleb128 0
LLSDACSE40:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr22:
	.long	___gnat_others_value@GOT-L_got_pcr22
LLSDATT40:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_vm_destroy
_aegis_vm_destroy:
LFB41:
	cbz	x0, L570
	stp	x29, x30, [sp, -32]!
LCFI196:
	mov	x29, sp
LCFI197:
LEHB144:
LEHE144:
	str	x19, [sp, 16]
LCFI198:
LEHB145:
	ldr	x1, [x0, 176]
	cbz	x1, L564
	add	x2, x1, 524288
	.p2align 5,,15
L565:
	strb	wzr, [x1]
	add	x1, x1, 1
	cmp	x1, x2
	bne	L565
L564:
	ldr	x3, [x0, 192]
	cbz	x3, L566
	mov	x1, 0
	mov	x4, 33036
	.p2align 5,,15
L567:
	madd	x2, x1, x4, x3
	add	x2, x2, 32768
	strb	wzr, [x2, 264]
	madd	x2, x1, x4, x3
	str	wzr, [x2, 256]
	add	x2, x2, 32768
	str	wzr, [x2, 260]
	add	x1, x1, 1
	cmp	x1, 1024
	bne	L567
L566:
	stp	xzr, xzr, [x0]
	stp	xzr, xzr, [x0, 16]
	add	x1, x0, 32
	stp	xzr, xzr, [x0, 32]
	stp	xzr, xzr, [x1, 16]
	add	x1, x0, 64
	stp	xzr, xzr, [x0, 64]
	stp	xzr, xzr, [x1, 16]
	str	xzr, [x0, 96]
	str	xzr, [x0, 104]
	str	xzr, [x0, 112]
	str	xzr, [x0, 120]
	str	xzr, [x0, 128]
	str	xzr, [x0, 136]
	str	xzr, [x0, 144]
	str	xzr, [x0, 152]
	str	xzr, [x0, 160]
	str	xzr, [x0, 184]
	strh	wzr, [x0, 200]
	bl	___gnat_free
LEHE145:
	mov	w0, 0
L562:
LEHB146:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI199:
	ret
L570:
	mov	w0, 1
	ret
L571:
LCFI200:
	mov	x19, x0
	cmp	x1, 1
	bne	L584
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L562
L584:
	bl	__Unwind_Resume
LEHE146:
LFE41:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table23:
	.align	2
LLSDA41:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT41-LLSDATTD41
LLSDATTD41:
	.byte	0x1
	.uleb128 LLSDACSE41-LLSDACSB41
LLSDACSB41:
	.uleb128 LEHB144-LFB41
	.uleb128 LEHE144-LEHB144
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB145-LFB41
	.uleb128 LEHE145-LEHB145
	.uleb128 L571-LFB41
	.uleb128 0x1
	.uleb128 LEHB146-LFB41
	.uleb128 LEHE146-LEHB146
	.uleb128 0
	.uleb128 0
LLSDACSE41:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr23:
	.long	___gnat_others_value@GOT-L_got_pcr23
LLSDATT41:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_validate_bytecode
_aegis_validate_bytecode:
LFB42:
	cmp	x0, 0
	mov	w0, 1
	ccmp	x1, 0, 4, ne
	beq	L585
	cmp	x1, 524288
	mov	w0, 11
	csel	w0, w0, wzr, hi
L585:
	ret
LFE42:
	.align	2
	.p2align 5,,15
	.globl _aegis_estimate_gas
_aegis_estimate_gas:
LFB43:
	cmp	x1, 0
	ccmp	x6, 0, 4, ne
	ccmp	x0, 0, 4, ne
	beq	L593
	mov	x0, 21000
	stp	x29, x30, [sp, -32]!
LCFI201:
	mov	x29, sp
LCFI202:
	add	x1, x0, x1, lsl 4
LEHB147:
LEHE147:
	str	x19, [sp, 16]
LCFI203:
LEHB148:
	str	x1, [x6]
LEHE148:
	mov	w0, 0
L589:
LEHB149:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI204:
	ret
	.p2align 2,,3
L593:
	mov	w0, 1
	ret
L594:
LCFI205:
	mov	x19, x0
	cmp	x1, 1
	bne	L599
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L589
L599:
	bl	__Unwind_Resume
LEHE149:
LFE43:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table24:
	.align	2
LLSDA43:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT43-LLSDATTD43
LLSDATTD43:
	.byte	0x1
	.uleb128 LLSDACSE43-LLSDACSB43
LLSDACSB43:
	.uleb128 LEHB147-LFB43
	.uleb128 LEHE147-LEHB147
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB148-LFB43
	.uleb128 LEHE148-LEHB148
	.uleb128 L594-LFB43
	.uleb128 0x1
	.uleb128 LEHB149-LFB43
	.uleb128 LEHE149-LEHB149
	.uleb128 0
	.uleb128 0
LLSDACSE43:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr24:
	.long	___gnat_others_value@GOT-L_got_pcr24
LLSDATT43:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_storage_load
_aegis_storage_load:
LFB44:
	mov	x4, x0
	cbz	x0, L610
	stp	x29, x30, [sp, -112]!
LCFI206:
	mov	x29, sp
LCFI207:
LEHB150:
LEHE150:
	mov	x0, x1
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI208:
LEHB151:
	ldrb	w1, [x4, 200]
	cmp	w1, 1
	bhi	L626
	cbz	w1, L611
	cmp	x2, 0
	mov	x19, x3
	ccmp	x3, 0, 4, ne
	ccmp	x0, 0, 4, ne
	beq	L612
	ldr	x1, [x2]
	str	x1, [x29, 48]
	ldr	x1, [x2, 8]
	add	x20, x29, 48
	str	x1, [x20, 8]
	ldr	x1, [x2, 16]
	str	x1, [x20, 16]
	ldr	x1, [x2, 24]
	str	x1, [x20, 24]
	bl	_khepri_storage_trie__find_storage
	mov	w2, 10000
	cmp	w0, w2
	bhi	L627
	cbnz	w0, L604
L625:
	str	xzr, [x19]
	str	xzr, [x19, 8]
	str	xzr, [x19, 16]
	str	xzr, [x19, 24]
LEHE151:
L607:
	mov	w0, 0
L600:
LEHB152:
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI209:
	ret
	.p2align 2,,3
L612:
LCFI210:
	mov	w0, 1
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
LEHE152:
	ldp	x29, x30, [sp], 112
LCFI211:
	ret
	.p2align 2,,3
L604:
LCFI212:
	add	x21, x29, 80
	mov	x1, x20
	mov	x2, x21
LEHB153:
	bl	_khepri_storage_trie__sload
	and	w0, w0, 255
	cmp	w0, 1
	bhi	L628
	cbz	w0, L625
	ldr	x0, [x29, 80]
	str	x0, [x19]
	ldr	x0, [x21, 8]
	str	x0, [x19, 8]
	ldr	x0, [x21, 16]
	str	x0, [x19, 16]
	ldr	x0, [x21, 24]
	str	x0, [x19, 24]
LEHE153:
	b	L607
	.p2align 2,,3
L611:
	mov	w0, 6
LEHB154:
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI213:
	ret
	.p2align 2,,3
L610:
	mov	w0, 6
LEHE154:
	ret
L626:
LCFI214:
	adrp	x0, lC9@PAGE
	mov	w1, 1374
	add	x0, x0, lC9@PAGEOFF;
LEHB155:
	bl	___gnat_rcheck_CE_Invalid_Data
L627:
	adrp	x0, lC9@PAGE
	mov	w1, 1412
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L628:
	adrp	x0, lC9@PAGE
	mov	w1, 1427
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE155:
L613:
	mov	x19, x0
	cmp	x1, 1
	bne	L629
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB156:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L600
L629:
	bl	__Unwind_Resume
LEHE156:
LFE44:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table25:
	.align	2
LLSDA44:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT44-LLSDATTD44
LLSDATTD44:
	.byte	0x1
	.uleb128 LLSDACSE44-LLSDACSB44
LLSDACSB44:
	.uleb128 LEHB150-LFB44
	.uleb128 LEHE150-LEHB150
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB151-LFB44
	.uleb128 LEHE151-LEHB151
	.uleb128 L613-LFB44
	.uleb128 0x1
	.uleb128 LEHB152-LFB44
	.uleb128 LEHE152-LEHB152
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB153-LFB44
	.uleb128 LEHE153-LEHB153
	.uleb128 L613-LFB44
	.uleb128 0x1
	.uleb128 LEHB154-LFB44
	.uleb128 LEHE154-LEHB154
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB155-LFB44
	.uleb128 LEHE155-LEHB155
	.uleb128 L613-LFB44
	.uleb128 0x1
	.uleb128 LEHB156-LFB44
	.uleb128 LEHE156-LEHB156
	.uleb128 0
	.uleb128 0
LLSDACSE44:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr25:
	.long	___gnat_others_value@GOT-L_got_pcr25
LLSDATT44:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_storage_store
_aegis_storage_store:
LFB45:
	cbz	x0, L642
	stp	x29, x30, [sp, -112]!
LCFI215:
	mov	x29, sp
LCFI216:
LEHB157:
LEHE157:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI217:
LEHB158:
	ldrb	w0, [x0, 200]
	cmp	w0, 1
	bhi	L657
	cbz	w0, L643
	cmp	x2, 0
	mov	x19, x1
	ccmp	x3, 0, 4, ne
	ccmp	x1, 0, 4, ne
	beq	L644
	mov	x5, -1
	add	x20, x29, 48
	add	x21, x29, 80
L633:
	mov	x4, x5
	add	x5, x5, 1
	ldr	x1, [x2, x5, lsl 3]
	add	x0, x20, x4, lsl 3
	lsl	x4, x4, 3
	str	x1, [x0, 8]
	ldr	x0, [x3, x5, lsl 3]
	add	x4, x21, x4
	str	x0, [x4, 8]
	cmp	x5, 3
	bne	L633
	mov	x0, x19
	bl	_khepri_storage_trie__find_storage
	mov	w1, 10000
	cmp	w0, w1
	bhi	L658
	cbz	w0, L659
L635:
	mov	x2, x21
	mov	x1, x20
	bl	_khepri_storage_trie__sstore
LEHE158:
	and	w0, w0, 255
	cmp	w0, 1
	bhi	L660
	cbz	w0, L641
	mov	w0, 0
L630:
LEHB159:
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI218:
	ret
	.p2align 2,,3
L644:
LCFI219:
	mov	w0, 1
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 112
LCFI220:
	ret
	.p2align 2,,3
L643:
LCFI221:
	mov	w0, 6
	ldr	x21, [sp, 32]
	ldp	x19, x20, [sp, 16]
LEHE159:
	ldp	x29, x30, [sp], 112
LCFI222:
	ret
	.p2align 2,,3
L659:
LCFI223:
	mov	x0, x19
LEHB160:
	bl	_khepri_storage_trie__create_storage
	ubfx	x2, x0, 32, 8
	cmp	w2, 1
	bhi	L661
	cbz	w2, L641
	mov	w2, 10000
	cmp	w0, w2
	bls	L635
	adrp	x0, lC9@PAGE
	mov	w1, 1508
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE160:
	.p2align 2,,3
L642:
LCFI224:
	mov	w0, 6
LEHB161:
LEHE161:
	ret
L657:
LCFI225:
	adrp	x0, lC9@PAGE
	mov	w1, 1452
	add	x0, x0, lC9@PAGEOFF;
LEHB162:
	bl	___gnat_rcheck_CE_Invalid_Data
L660:
	adrp	x0, lC9@PAGE
	mov	w1, 1515
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L658:
	adrp	x0, lC9@PAGE
	mov	w1, 1491
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L661:
	adrp	x0, lC9@PAGE
	mov	w1, 1501
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE162:
L645:
	mov	x19, x0
	cmp	x1, 1
	bne	L662
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB163:
	bl	___gnat_end_handler_v1
L641:
	mov	w0, 255
	b	L630
L662:
	bl	__Unwind_Resume
LEHE163:
LFE45:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table26:
	.align	2
LLSDA45:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT45-LLSDATTD45
LLSDATTD45:
	.byte	0x1
	.uleb128 LLSDACSE45-LLSDACSB45
LLSDACSB45:
	.uleb128 LEHB157-LFB45
	.uleb128 LEHE157-LEHB157
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB158-LFB45
	.uleb128 LEHE158-LEHB158
	.uleb128 L645-LFB45
	.uleb128 0x1
	.uleb128 LEHB159-LFB45
	.uleb128 LEHE159-LEHB159
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB160-LFB45
	.uleb128 LEHE160-LEHB160
	.uleb128 L645-LFB45
	.uleb128 0x1
	.uleb128 LEHB161-LFB45
	.uleb128 LEHE161-LEHB161
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB162-LFB45
	.uleb128 LEHE162-LEHB162
	.uleb128 L645-LFB45
	.uleb128 0x1
	.uleb128 LEHB163-LFB45
	.uleb128 LEHE163-LEHB163
	.uleb128 0
	.uleb128 0
LLSDACSE45:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr26:
	.long	___gnat_others_value@GOT-L_got_pcr26
LLSDATT45:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_get_state_root
_aegis_get_state_root:
LFB46:
	cbz	x0, L673
	stp	x29, x30, [sp, -96]!
LCFI226:
	mov	x29, sp
LCFI227:
LEHB164:
LEHE164:
	stp	x19, x20, [sp, 16]
LCFI228:
	mov	x19, x1
LEHB165:
	ldrb	w1, [x0, 200]
	cmp	w1, 1
	bhi	L683
	cbz	w1, L674
	cbz	x19, L675
	add	x5, x0, 65
	mov	x2, -1
	add	x0, x29, 32
	.p2align 5,,15
L666:
	ldrb	w4, [x5, x2]
	add	x3, x0, x2
	strb	w4, [x3, 1]
	add	x2, x2, 1
	cmp	x2, 31
	bne	L666
	bl	_khepri_storage_trie__find_storage
	mov	w2, 10000
	cmp	w0, w2
	bhi	L684
	cbnz	w0, L668
	stp	xzr, xzr, [x19]
	stp	xzr, xzr, [x19, 16]
LEHE165:
L669:
	mov	w0, 0
L663:
LEHB166:
	ldp	x19, x20, [sp, 16]
LEHE166:
	ldp	x29, x30, [sp], 96
LCFI229:
	ret
	.p2align 2,,3
L668:
LCFI230:
	add	x20, x29, 64
	mov	x8, x20
LEHB167:
	bl	_khepri_storage_trie__get_root
	mov	x2, -1
	.p2align 5,,15
L670:
	add	x0, x20, x2
	add	x2, x2, 1
	ldrb	w0, [x0, 1]
	strb	w0, [x19, x2]
LEHE167:
	cmp	x2, 31
	bne	L670
	b	L669
	.p2align 2,,3
L674:
	mov	w0, 6
LEHB168:
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI231:
	ret
L675:
LCFI232:
	mov	w0, 1
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI233:
	ret
L673:
	mov	w0, 6
LEHE168:
	ret
L684:
LCFI234:
	adrp	x0, lC9@PAGE
	mov	w1, 1557
	add	x0, x0, lC9@PAGEOFF;
LEHB169:
	bl	___gnat_rcheck_CE_Invalid_Data
L683:
	adrp	x0, lC9@PAGE
	mov	w1, 1535
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE169:
L676:
	mov	x19, x0
	cmp	x1, 1
	bne	L685
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB170:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L663
L685:
	bl	__Unwind_Resume
LEHE170:
LFE46:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table27:
	.align	2
LLSDA46:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT46-LLSDATTD46
LLSDATTD46:
	.byte	0x1
	.uleb128 LLSDACSE46-LLSDACSB46
LLSDACSB46:
	.uleb128 LEHB164-LFB46
	.uleb128 LEHE164-LEHB164
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB165-LFB46
	.uleb128 LEHE165-LEHB165
	.uleb128 L676-LFB46
	.uleb128 0x1
	.uleb128 LEHB166-LFB46
	.uleb128 LEHE166-LEHB166
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB167-LFB46
	.uleb128 LEHE167-LEHB167
	.uleb128 L676-LFB46
	.uleb128 0x1
	.uleb128 LEHB168-LFB46
	.uleb128 LEHE168-LEHB168
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB169-LFB46
	.uleb128 LEHE169-LEHB169
	.uleb128 L676-LFB46
	.uleb128 0x1
	.uleb128 LEHB170-LFB46
	.uleb128 LEHE170-LEHB170
	.uleb128 0
	.uleb128 0
LLSDACSE46:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr27:
	.long	___gnat_others_value@GOT-L_got_pcr27
LLSDATT46:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_get_storage_proof
_aegis_get_storage_proof:
LFB47:
	mov	x5, x0
	cbz	x0, L699
	mov	x12, 34480
	mov	x0, x1
	sub	sp, sp, x12
LCFI235:
	stp	x29, x30, [sp]
LCFI236:
	mov	x29, sp
LCFI237:
LEHB171:
LEHE171:
	stp	x19, x20, [sp, 16]
LCFI238:
LEHB172:
	ldrb	w1, [x5, 200]
	cmp	w1, 1
	bhi	L709
	cbz	w1, L700
	cmp	x2, 0
	mov	x19, x4
	ccmp	x4, 0, 4, ne
	ccmp	x0, 0, 4, ne
	beq	L701
	ldr	x1, [x2]
	mov	x20, x3
	str	x1, [x29, 32]
	ldr	x3, [x2, 8]
	sub	x1, x29, #336
	str	x3, [x1, 376]
	ldr	x3, [x2, 16]
	str	x3, [x1, 384]
	ldr	x2, [x2, 24]
	str	x2, [x1, 392]
	bl	_khepri_storage_trie__find_storage
	mov	w2, 10000
	cmp	w0, w2
	bhi	L710
	cbnz	w0, L690
L708:
	str	xzr, [x19]
LEHE172:
L695:
	mov	w0, 0
L686:
LEHB173:
	mov	x12, 34480
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, x12
LCFI239:
	ret
	.p2align 2,,3
L701:
LCFI240:
	mov	w0, 1
	mov	x12, 34480
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
LEHE173:
	add	sp, sp, x12
LCFI241:
	ret
	.p2align 2,,3
L690:
LCFI242:
	add	x2, x29, 64
	add	x1, x29, 32
LEHB174:
	bl	_khepri_storage_trie__generate_storage_proof
	cmp	w0, 1
	bhi	L711
	cmp	x20, 0
	ccmp	w0, 0, 4, ne
	beq	L708
	add	x0, x29, 32768
	ldrb	w0, [x0, 1708]
	cmp	w0, 1
	bhi	L712
	cbnz	w0, L713
	strb	wzr, [x20]
L696:
	mov	x0, 1
	str	x0, [x19]
LEHE174:
	b	L695
	.p2align 2,,3
L700:
	mov	w0, 6
LEHB175:
	mov	x12, 34480
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, x12
LCFI243:
	ret
	.p2align 2,,3
L699:
	mov	w0, 6
LEHE175:
	ret
	.p2align 2,,3
L713:
LCFI244:
	mov	w0, 1
LEHB176:
	strb	w0, [x20]
	b	L696
L709:
	adrp	x0, lC9@PAGE
	mov	w1, 1588
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L710:
	adrp	x0, lC9@PAGE
	mov	w1, 1620
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L712:
	adrp	x0, lC9@PAGE
	mov	w1, 1642
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L711:
	adrp	x0, lC9@PAGE
	mov	w1, 1634
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE176:
L702:
	mov	x19, x0
	cmp	x1, 1
	bne	L714
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB177:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L686
L714:
	bl	__Unwind_Resume
LEHE177:
LFE47:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table28:
	.align	2
LLSDA47:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT47-LLSDATTD47
LLSDATTD47:
	.byte	0x1
	.uleb128 LLSDACSE47-LLSDACSB47
LLSDACSB47:
	.uleb128 LEHB171-LFB47
	.uleb128 LEHE171-LEHB171
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB172-LFB47
	.uleb128 LEHE172-LEHB172
	.uleb128 L702-LFB47
	.uleb128 0x1
	.uleb128 LEHB173-LFB47
	.uleb128 LEHE173-LEHB173
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB174-LFB47
	.uleb128 LEHE174-LEHB174
	.uleb128 L702-LFB47
	.uleb128 0x1
	.uleb128 LEHB175-LFB47
	.uleb128 LEHE175-LEHB175
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB176-LFB47
	.uleb128 LEHE176-LEHB176
	.uleb128 L702-LFB47
	.uleb128 0x1
	.uleb128 LEHB177-LFB47
	.uleb128 LEHE177-LEHB177
	.uleb128 0
	.uleb128 0
LLSDACSE47:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr28:
	.long	___gnat_others_value@GOT-L_got_pcr28
LLSDATT47:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_verify_storage_proof
_aegis_verify_storage_proof:
LFB48:
	cmp	x3, 0
	ccmp	x6, 0, 4, ne
	ccmp	x2, 0, 4, ne
	ccmp	x0, 0, 4, ne
	beq	L724
	cmp	x5, 0
	ccmp	x4, 0, 0, ne
	beq	L724
	mov	x12, 34560
	mov	x8, x0
	sub	sp, sp, x12
LCFI245:
	stp	x29, x30, [sp]
LCFI246:
	mov	x29, sp
LCFI247:
LEHB178:
LEHE178:
	stp	x19, x20, [sp, 16]
	str	x21, [sp, 32]
LCFI248:
LEHB179:
	ldr	x0, [x2]
	str	x0, [x29, 48]
	ldr	x0, [x2, 8]
	sub	x21, x29, #256
	str	x0, [x21, 312]
	ldr	x0, [x2, 16]
	str	x0, [x21, 320]
	ldr	x2, [x2, 24]
	mov	x19, x3
	mov	x20, x6
	str	x2, [x21, 328]
	mov	x1, -1
	add	x0, x29, 80
	.p2align 5,,15
L717:
	mov	x2, x1
	add	x1, x1, 1
	ldrb	w7, [x8, x1]
	add	x2, x0, x2
	strb	w7, [x2, 1]
	cmp	x1, 31
	bne	L717
	add	x2, x29, 32768
	mov	w1, 1
	str	wzr, [x2, 1680]
	strb	w1, [x2, 1788]
	cbz	x5, L718
	ldrb	w1, [x4]
	and	w1, w1, 255
	cmp	w1, 1
	cset	w1, eq
	strb	w1, [x2, 1788]
L718:
	add	x3, x29, 112
	add	x2, x29, 144
	add	x1, x29, 48
	bl	_khepri_storage_trie__verify_storage_proof
	cmp	w0, 1
	bhi	L735
	cbz	w0, L725
	ldr	x0, [x29, 112]
	str	x0, [x19]
	ldr	x0, [x21, 376]
	str	x0, [x19, 8]
	ldr	x0, [x21, 384]
	str	x0, [x19, 16]
	ldr	x0, [x21, 392]
	str	x0, [x19, 24]
	mov	w0, 1
L720:
	strb	w0, [x20]
LEHE179:
	mov	w0, 0
L715:
LEHB180:
	mov	x12, 34560
	ldr	x21, [sp, 32]
	ldp	x29, x30, [sp]
	ldp	x19, x20, [sp, 16]
	add	sp, sp, x12
LCFI249:
	ret
	.p2align 2,,3
L724:
	mov	w0, 1
LEHE180:
	ret
	.p2align 2,,3
L725:
LCFI250:
	mov	w0, 0
	b	L720
L735:
	adrp	x0, lC9@PAGE
	mov	w1, 1734
	add	x0, x0, lC9@PAGEOFF;
LEHB181:
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE181:
L726:
	mov	x19, x0
	cmp	x1, 1
	bne	L736
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB182:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L715
L736:
	bl	__Unwind_Resume
LEHE182:
LFE48:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table29:
	.align	2
LLSDA48:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT48-LLSDATTD48
LLSDATTD48:
	.byte	0x1
	.uleb128 LLSDACSE48-LLSDACSB48
LLSDACSB48:
	.uleb128 LEHB178-LFB48
	.uleb128 LEHE178-LEHB178
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB179-LFB48
	.uleb128 LEHE179-LEHB179
	.uleb128 L726-LFB48
	.uleb128 0x1
	.uleb128 LEHB180-LFB48
	.uleb128 LEHE180-LEHB180
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB181-LFB48
	.uleb128 LEHE181-LEHB181
	.uleb128 L726-LFB48
	.uleb128 0x1
	.uleb128 LEHB182-LFB48
	.uleb128 LEHE182-LEHB182
	.uleb128 0
	.uleb128 0
LLSDACSE48:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr29:
	.long	___gnat_others_value@GOT-L_got_pcr29
LLSDATT48:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_get_cert_level
_aegis_get_cert_level:
LFB49:
	cbz	x0, L745
	stp	x29, x30, [sp, -96]!
LCFI251:
	mov	x29, sp
LCFI252:
LEHB183:
LEHE183:
	str	x19, [sp, 16]
LCFI253:
LEHB184:
	ldrb	w0, [x0, 200]
	cmp	w0, 1
	bhi	L755
	cbz	w0, L746
	cmp	x1, 0
	mov	x19, x2
	ccmp	x2, 0, 4, ne
	beq	L747
	mov	x0, -1
	add	x5, x29, 64
	.p2align 5,,15
L740:
	mov	x3, x0
	add	x0, x0, 1
	ldrb	w4, [x1, x0]
	add	x3, x5, x3
	strb	w4, [x3, 1]
	cmp	x0, 31
	bne	L740
	ldp	q31, q30, [x5]
	add	x0, x29, 32
	stp	q31, q30, [x0]
	bl	_khepri_registry__get_level
	cmp	w0, 4
	bhi	L756
	beq	L748
	adrp	x1, _CSWTCH.230@PAGE
	add	x1, x1, _CSWTCH.230@PAGEOFF;
	ldr	w0, [x1, w0, uxtw 2]
L742:
	str	w0, [x19]
LEHE184:
	mov	w0, 0
L737:
LEHB185:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI254:
	ret
	.p2align 2,,3
L747:
LCFI255:
	mov	w0, 1
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI256:
	ret
	.p2align 2,,3
L746:
LCFI257:
	mov	w0, 6
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 96
LCFI258:
	ret
	.p2align 2,,3
L748:
LCFI259:
	mov	w0, 3
	b	L742
L745:
LCFI260:
	mov	w0, 6
LEHE185:
	ret
L755:
LCFI261:
	adrp	x0, lC9@PAGE
	mov	w1, 1791
	add	x0, x0, lC9@PAGEOFF;
LEHB186:
	bl	___gnat_rcheck_CE_Invalid_Data
L756:
	adrp	x0, lC9@PAGE
	mov	w1, 1801
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LEHE186:
L749:
	mov	x19, x0
	cmp	x1, 1
	bne	L757
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
LEHB187:
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L737
L757:
	bl	__Unwind_Resume
LEHE187:
LFE49:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table30:
	.align	2
LLSDA49:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT49-LLSDATTD49
LLSDATTD49:
	.byte	0x1
	.uleb128 LLSDACSE49-LLSDACSB49
LLSDACSB49:
	.uleb128 LEHB183-LFB49
	.uleb128 LEHE183-LEHB183
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB184-LFB49
	.uleb128 LEHE184-LEHB184
	.uleb128 L749-LFB49
	.uleb128 0x1
	.uleb128 LEHB185-LFB49
	.uleb128 LEHE185-LEHB185
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB186-LFB49
	.uleb128 LEHE186-LEHB186
	.uleb128 L749-LFB49
	.uleb128 0x1
	.uleb128 LEHB187-LFB49
	.uleb128 LEHE187-LEHB187
	.uleb128 0
	.uleb128 0
LLSDACSE49:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr30:
	.long	___gnat_others_value@GOT-L_got_pcr30
LLSDATT49:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_apply_gas_discount
_aegis_apply_gas_discount:
LFB52:
	cbz	x2, L763
	mov	x3, 100
	mov	w6, 10
	stp	x29, x30, [sp, -32]!
LCFI262:
	sub	w5, w1, #1
	mov	x4, 62915
	umsubl	x1, w1, w6, x3
	cmp	w5, 3
	csel	x1, x1, x3, cc
	mul	x0, x0, x1
	movk	x4, 0x5c28, lsl 16
	mov	x29, sp
LCFI263:
	movk	x4, 0xc28f, lsl 32
LEHB188:
LEHE188:
	movk	x4, 0x28f5, lsl 48
	str	x19, [sp, 16]
LCFI264:
	lsr	x0, x0, 2
	umulh	x0, x0, x4
	lsr	x0, x0, 2
LEHB189:
	str	x0, [x2]
LEHE189:
	mov	w0, 0
L758:
LEHB190:
	ldr	x19, [sp, 16]
	ldp	x29, x30, [sp], 32
LCFI265:
	ret
	.p2align 2,,3
L763:
	mov	w0, 1
	ret
L765:
LCFI266:
	mov	x19, x0
	cmp	x1, 1
	bne	L770
	bl	___gnat_begin_handler_v1
	mov	x1, x0
	mov	x2, 0
	mov	x0, x19
	bl	___gnat_end_handler_v1
	mov	w0, 255
	b	L758
L770:
	bl	__Unwind_Resume
LEHE190:
LFE52:
	.section __TEXT,__gcc_except_tab
	.p2align	2
GCC_except_table31:
	.align	2
LLSDA52:
	.byte	0xff
	.byte	0x9b
	.uleb128 LLSDATT52-LLSDATTD52
LLSDATTD52:
	.byte	0x1
	.uleb128 LLSDACSE52-LLSDACSB52
LLSDACSB52:
	.uleb128 LEHB188-LFB52
	.uleb128 LEHE188-LEHB188
	.uleb128 0
	.uleb128 0
	.uleb128 LEHB189-LFB52
	.uleb128 LEHE189-LEHB189
	.uleb128 L765-LFB52
	.uleb128 0x1
	.uleb128 LEHB190-LFB52
	.uleb128 LEHE190-LEHB190
	.uleb128 0
	.uleb128 0
LLSDACSE52:
	.byte	0x1
	.byte	0
	.align	2
L_got_pcr31:
	.long	___gnat_others_value@GOT-L_got_pcr31
LLSDATT52:
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_secure_zero
_aegis_secure_zero:
LFB53:
	mov	x2, 2147483647
	cmp	x1, x2
	bhi	L779
	cmp	x0, 0
	ccmp	x1, 0, 4, ne
	beq	L771
	add	w2, w1, 1
	mov	x1, 1
	.p2align 5,,15
L774:
	add	x3, x0, x1
	add	x1, x1, 1
	strb	wzr, [x3, -1]
	cmp	x1, x2
	bne	L774
L771:
	ret
L779:
	adrp	x0, lC9@PAGE
	stp	x29, x30, [sp, -16]!
LCFI267:
	mov	w1, 1847
	mov	x29, sp
LCFI268:
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE53:
	.align	2
	.p2align 5,,15
	.globl _aegis_constant_time_compare
_aegis_constant_time_compare:
LFB54:
	mov	x3, 2147483647
	cmp	x2, x3
	bhi	L789
	cmp	x0, 0
	mov	x7, x0
	ccmp	x1, 0, 4, ne
	mov	w0, 1
	ccmp	x2, 0, 4, ne
	beq	L780
	mov	w4, 0
	add	w5, w2, w0
	mov	x3, 1
	.p2align 5,,15
L783:
	add	x6, x7, x3
	add	x2, x1, x3
	ldrb	w6, [x6, -1]
	add	x3, x3, 1
	ldrb	w2, [x2, -1]
	eor	w2, w2, w6
	and	w2, w2, 255
	orr	w4, w4, w2
	cmp	x5, x3
	bne	L783
	cmp	w4, 0
	cset	w0, ne
L780:
	ret
L789:
	adrp	x0, lC9@PAGE
	stp	x29, x30, [sp, -16]!
LCFI269:
	mov	w1, 1866
	mov	x29, sp
LCFI270:
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Range_Check
LFE54:
	.const
	.align	2
_CSWTCH.230:
	.word	0
	.word	0
	.word	1
	.word	2
	.align	3
_domain_prefix.6:
	.ascii "aegis-v1-mldsa87-"
_zero_m.7:
	.space 32
_zero_z.8:
	.space 32
_zero_z.9:
	.space 32
_zero_d.10:
	.space 32
_zero_random.11:
	.space 32
_zero_seed.12:
	.space 32
	.data
_aegis_ffi__library_initialized:
	.space 1
	.globl _aegis_ffi_E
	.align	1
_aegis_ffi_E:
	.space 2
	.const
	.align	3
lC11:
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
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
L_got_pcr32:
	.long	___gnat_personality_v0@GOT-L_got_pcr32
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
	.uleb128 0x8
	.quad	0
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
	.uleb128 0x8
	.quad	0
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
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$9,LEFDE9-LASFDE9
	.long L$set$9
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB6-.
	.set L$set$10,LFE6-LFB6
	.quad L$set$10
	.uleb128 0x8
	.quad	0
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
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE11:
LSFDE13:
	.set L$set$13,LEFDE13-LASFDE13
	.long L$set$13
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB128-.
	.set L$set$14,LFE128-LFB128
	.quad L$set$14
	.uleb128 0x8
	.quad	0
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
	.uleb128 0x8
	.quad	0
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
	.uleb128 0x8
	.quad	0
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
	.uleb128 0x8
	.quad	0
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
	.uleb128 0x8
	.quad	LLSDA12-.
	.byte	0x4
	.set L$set$23,LCFI0-LFB12
	.long L$set$23
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$24,LCFI1-LCFI0
	.long L$set$24
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$25,LCFI2-LCFI1
	.long L$set$25
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$26,LCFI3-LCFI2
	.long L$set$26
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$27,LCFI4-LCFI3
	.long L$set$27
	.byte	0xb
	.align	3
LEFDE21:
LSFDE23:
	.set L$set$28,LEFDE23-LASFDE23
	.long L$set$28
LASFDE23:
	.long	LASFDE23-EH_frame1
	.quad	LFB13-.
	.set L$set$29,LFE13-LFB13
	.quad L$set$29
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$30,LCFI5-LFB13
	.long L$set$30
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$31,LCFI6-LCFI5
	.long L$set$31
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE23:
LSFDE25:
	.set L$set$32,LEFDE25-LASFDE25
	.long L$set$32
LASFDE25:
	.long	LASFDE25-EH_frame1
	.quad	LFB14-.
	.set L$set$33,LFE14-LFB14
	.quad L$set$33
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE25:
LSFDE27:
	.set L$set$34,LEFDE27-LASFDE27
	.long L$set$34
LASFDE27:
	.long	LASFDE27-EH_frame1
	.quad	LFB15-.
	.set L$set$35,LFE15-LFB15
	.quad L$set$35
	.uleb128 0x8
	.quad	LLSDA15-.
	.byte	0x4
	.set L$set$36,LCFI7-LFB15
	.long L$set$36
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$37,LCFI8-LCFI7
	.long L$set$37
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$38,LCFI9-LCFI8
	.long L$set$38
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$39,LCFI10-LCFI9
	.long L$set$39
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$40,LCFI11-LCFI10
	.long L$set$40
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE27:
LSFDE29:
	.set L$set$41,LEFDE29-LASFDE29
	.long L$set$41
LASFDE29:
	.long	LASFDE29-EH_frame1
	.quad	LFB16-.
	.set L$set$42,LFE16-LFB16
	.quad L$set$42
	.uleb128 0x8
	.quad	LLSDA16-.
	.byte	0x4
	.set L$set$43,LCFI12-LFB16
	.long L$set$43
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$44,LCFI13-LCFI12
	.long L$set$44
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$45,LCFI14-LCFI13
	.long L$set$45
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$46,LCFI15-LCFI14
	.long L$set$46
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x4
	.set L$set$47,LCFI16-LCFI15
	.long L$set$47
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x4
	.set L$set$48,LCFI17-LCFI16
	.long L$set$48
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
	.set L$set$49,LCFI18-LCFI17
	.long L$set$49
	.byte	0xb
	.byte	0x4
	.set L$set$50,LCFI19-LCFI18
	.long L$set$50
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
	.set L$set$51,LCFI20-LCFI19
	.long L$set$51
	.byte	0xb
	.byte	0x4
	.set L$set$52,LCFI21-LCFI20
	.long L$set$52
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
	.set L$set$53,LCFI22-LCFI21
	.long L$set$53
	.byte	0xb
	.align	3
LEFDE29:
LSFDE31:
	.set L$set$54,LEFDE31-LASFDE31
	.long L$set$54
LASFDE31:
	.long	LASFDE31-EH_frame1
	.quad	LFB17-.
	.set L$set$55,LFE17-LFB17
	.quad L$set$55
	.uleb128 0x8
	.quad	LLSDA17-.
	.byte	0x4
	.set L$set$56,LCFI23-LFB17
	.long L$set$56
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$57,LCFI24-LCFI23
	.long L$set$57
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$58,LCFI25-LCFI24
	.long L$set$58
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x95
	.uleb128 0x6
	.byte	0x96
	.uleb128 0x5
	.byte	0x4
	.set L$set$59,LCFI26-LCFI25
	.long L$set$59
	.byte	0x97
	.uleb128 0x4
	.byte	0x98
	.uleb128 0x3
	.byte	0x4
	.set L$set$60,LCFI27-LCFI26
	.long L$set$60
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
	.set L$set$61,LCFI28-LCFI27
	.long L$set$61
	.byte	0xb
	.byte	0x4
	.set L$set$62,LCFI29-LCFI28
	.long L$set$62
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
	.set L$set$63,LCFI30-LCFI29
	.long L$set$63
	.byte	0xb
	.byte	0x4
	.set L$set$64,LCFI31-LCFI30
	.long L$set$64
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
	.set L$set$65,LCFI32-LCFI31
	.long L$set$65
	.byte	0xb
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$66,LEFDE33-LASFDE33
	.long L$set$66
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB18-.
	.set L$set$67,LFE18-LFB18
	.quad L$set$67
	.uleb128 0x8
	.quad	LLSDA18-.
	.byte	0x4
	.set L$set$68,LCFI33-LFB18
	.long L$set$68
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$69,LCFI34-LCFI33
	.long L$set$69
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$70,LCFI35-LCFI34
	.long L$set$70
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$71,LCFI36-LCFI35
	.long L$set$71
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$72,LCFI37-LCFI36
	.long L$set$72
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$73,LEFDE35-LASFDE35
	.long L$set$73
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB19-.
	.set L$set$74,LFE19-LFB19
	.quad L$set$74
	.uleb128 0x8
	.quad	LLSDA19-.
	.byte	0x4
	.set L$set$75,LCFI38-LFB19
	.long L$set$75
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$76,LCFI39-LCFI38
	.long L$set$76
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$77,LCFI40-LCFI39
	.long L$set$77
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$78,LCFI41-LCFI40
	.long L$set$78
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$79,LCFI42-LCFI41
	.long L$set$79
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$80,LEFDE37-LASFDE37
	.long L$set$80
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB20-.
	.set L$set$81,LFE20-LFB20
	.quad L$set$81
	.uleb128 0x8
	.quad	LLSDA20-.
	.byte	0x4
	.set L$set$82,LCFI43-LFB20
	.long L$set$82
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$83,LCFI44-LCFI43
	.long L$set$83
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$84,LCFI45-LCFI44
	.long L$set$84
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$85,LCFI46-LCFI45
	.long L$set$85
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$86,LCFI47-LCFI46
	.long L$set$86
	.byte	0xb
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$87,LEFDE39-LASFDE39
	.long L$set$87
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB21-.
	.set L$set$88,LFE21-LFB21
	.quad L$set$88
	.uleb128 0x8
	.quad	LLSDA21-.
	.byte	0x4
	.set L$set$89,LCFI48-LFB21
	.long L$set$89
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$90,LCFI49-LCFI48
	.long L$set$90
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$91,LCFI50-LCFI49
	.long L$set$91
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$92,LCFI51-LCFI50
	.long L$set$92
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$93,LCFI52-LCFI51
	.long L$set$93
	.byte	0xb
	.byte	0x4
	.set L$set$94,LCFI53-LCFI52
	.long L$set$94
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0x4
	.set L$set$95,LCFI54-LCFI53
	.long L$set$95
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x50
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$96,LEFDE41-LASFDE41
	.long L$set$96
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB22-.
	.set L$set$97,LFE22-LFB22
	.quad L$set$97
	.uleb128 0x8
	.quad	LLSDA22-.
	.byte	0x4
	.set L$set$98,LCFI55-LFB22
	.long L$set$98
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$99,LCFI56-LCFI55
	.long L$set$99
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$100,LCFI57-LCFI56
	.long L$set$100
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x4
	.set L$set$101,LCFI58-LCFI57
	.long L$set$101
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$102,LCFI59-LCFI58
	.long L$set$102
	.byte	0xb
	.byte	0x4
	.set L$set$103,LCFI60-LCFI59
	.long L$set$103
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0x4
	.set L$set$104,LCFI61-LCFI60
	.long L$set$104
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x70
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$105,LEFDE43-LASFDE43
	.long L$set$105
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB23-.
	.set L$set$106,LFE23-LFB23
	.quad L$set$106
	.uleb128 0x8
	.quad	LLSDA23-.
	.byte	0x4
	.set L$set$107,LCFI62-LFB23
	.long L$set$107
	.byte	0xe
	.uleb128 0x50
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.byte	0x4
	.set L$set$108,LCFI63-LCFI62
	.long L$set$108
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$109,LCFI64-LCFI63
	.long L$set$109
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x4
	.set L$set$110,LCFI65-LCFI64
	.long L$set$110
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$111,LCFI66-LCFI65
	.long L$set$111
	.byte	0xb
	.byte	0x4
	.set L$set$112,LCFI67-LCFI66
	.long L$set$112
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0x4
	.set L$set$113,LCFI68-LCFI67
	.long L$set$113
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x50
	.byte	0x93
	.uleb128 0x8
	.byte	0x94
	.uleb128 0x7
	.byte	0x9d
	.uleb128 0xa
	.byte	0x9e
	.uleb128 0x9
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$114,LEFDE45-LASFDE45
	.long L$set$114
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB24-.
	.set L$set$115,LFE24-LFB24
	.quad L$set$115
	.uleb128 0x8
	.quad	LLSDA24-.
	.byte	0x4
	.set L$set$116,LCFI69-LFB24
	.long L$set$116
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$117,LCFI70-LCFI69
	.long L$set$117
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$118,LCFI71-LCFI70
	.long L$set$118
	.byte	0x93
	.uleb128 0x6
	.byte	0x4
	.set L$set$119,LCFI72-LCFI71
	.long L$set$119
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$120,LCFI73-LCFI72
	.long L$set$120
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$121,LEFDE47-LASFDE47
	.long L$set$121
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB25-.
	.set L$set$122,LFE25-LFB25
	.quad L$set$122
	.uleb128 0x8
	.quad	LLSDA25-.
	.byte	0x4
	.set L$set$123,LCFI74-LFB25
	.long L$set$123
	.byte	0xe
	.uleb128 0x40
	.byte	0x9d
	.uleb128 0x8
	.byte	0x9e
	.uleb128 0x7
	.byte	0x4
	.set L$set$124,LCFI75-LCFI74
	.long L$set$124
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$125,LCFI76-LCFI75
	.long L$set$125
	.byte	0x93
	.uleb128 0x6
	.byte	0x4
	.set L$set$126,LCFI77-LCFI76
	.long L$set$126
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$127,LCFI78-LCFI77
	.long L$set$127
	.byte	0xb
	.align	3
LEFDE47:
LSFDE49:
	.set L$set$128,LEFDE49-LASFDE49
	.long L$set$128
LASFDE49:
	.long	LASFDE49-EH_frame1
	.quad	LFB26-.
	.set L$set$129,LFE26-LFB26
	.quad L$set$129
	.uleb128 0x8
	.quad	LLSDA26-.
	.byte	0x4
	.set L$set$130,LCFI79-LFB26
	.long L$set$130
	.byte	0xe
	.uleb128 0xa60
	.byte	0x4
	.set L$set$131,LCFI80-LCFI79
	.long L$set$131
	.byte	0x9d
	.uleb128 0x14c
	.byte	0x9e
	.uleb128 0x14b
	.byte	0x4
	.set L$set$132,LCFI81-LCFI80
	.long L$set$132
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$133,LCFI82-LCFI81
	.long L$set$133
	.byte	0x93
	.uleb128 0x14a
	.byte	0x4
	.set L$set$134,LCFI83-LCFI82
	.long L$set$134
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xd3
	.byte	0xdd
	.byte	0xde
	.byte	0x4
	.set L$set$135,LCFI84-LCFI83
	.long L$set$135
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0xa60
	.byte	0x93
	.uleb128 0x14a
	.byte	0x9d
	.uleb128 0x14c
	.byte	0x9e
	.uleb128 0x14b
	.byte	0x4
	.set L$set$136,LCFI85-LCFI84
	.long L$set$136
	.byte	0xa
	.byte	0xd3
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$137,LCFI86-LCFI85
	.long L$set$137
	.byte	0xb
	.align	3
LEFDE49:
LSFDE51:
	.set L$set$138,LEFDE51-LASFDE51
	.long L$set$138
LASFDE51:
	.long	LASFDE51-EH_frame1
	.quad	LFB27-.
	.set L$set$139,LFE27-LFB27
	.quad L$set$139
	.uleb128 0x8
	.quad	LLSDA27-.
	.byte	0x4
	.set L$set$140,LCFI87-LFB27
	.long L$set$140
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$141,LCFI88-LCFI87
	.long L$set$141
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$142,LCFI89-LCFI88
	.long L$set$142
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x4
	.set L$set$143,LCFI90-LCFI89
	.long L$set$143
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$144,LCFI91-LCFI90
	.long L$set$144
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x94
	.uleb128 0x1
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE51:
LSFDE53:
	.set L$set$145,LEFDE53-LASFDE53
	.long L$set$145
LASFDE53:
	.long	LASFDE53-EH_frame1
	.quad	LFB28-.
	.set L$set$146,LFE28-LFB28
	.quad L$set$146
	.uleb128 0x8
	.quad	LLSDA28-.
	.byte	0x4
	.set L$set$147,LCFI92-LFB28
	.long L$set$147
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$148,LCFI93-LCFI92
	.long L$set$148
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$149,LCFI94-LCFI93
	.long L$set$149
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$150,LCFI95-LCFI94
	.long L$set$150
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x4
	.set L$set$151,LCFI96-LCFI95
	.long L$set$151
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
	.set L$set$152,LCFI97-LCFI96
	.long L$set$152
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x30
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x95
	.uleb128 0x2
	.byte	0x96
	.uleb128 0x1
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.align	3
LEFDE53:
LSFDE55:
	.set L$set$153,LEFDE55-LASFDE55
	.long L$set$153
LASFDE55:
	.long	LASFDE55-EH_frame1
	.quad	LFB29-.
	.set L$set$154,LFE29-LFB29
	.quad L$set$154
	.uleb128 0x8
	.quad	LLSDA29-.
	.byte	0x4
	.set L$set$155,LCFI98-LFB29
	.long L$set$155
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$156,LCFI99-LCFI98
	.long L$set$156
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$157,LCFI100-LCFI99
	.long L$set$157
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$158,LCFI101-LCFI100
	.long L$set$158
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$159,LCFI102-LCFI101
	.long L$set$159
	.byte	0xb
	.byte	0x4
	.set L$set$160,LCFI103-LCFI102
	.long L$set$160
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$161,LCFI104-LCFI103
	.long L$set$161
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE55:
LSFDE57:
	.set L$set$162,LEFDE57-LASFDE57
	.long L$set$162
LASFDE57:
	.long	LASFDE57-EH_frame1
	.quad	LFB30-.
	.set L$set$163,LFE30-LFB30
	.quad L$set$163
	.uleb128 0x8
	.quad	LLSDA30-.
	.byte	0x4
	.set L$set$164,LCFI105-LFB30
	.long L$set$164
	.byte	0xe
	.uleb128 0x480
	.byte	0x4
	.set L$set$165,LCFI106-LCFI105
	.long L$set$165
	.byte	0xe
	.uleb128 0x2a1480
	.byte	0x4
	.set L$set$166,LCFI107-LCFI106
	.long L$set$166
	.byte	0x9d
	.uleb128 0x54290
	.byte	0x9e
	.uleb128 0x5428f
	.byte	0x4
	.set L$set$167,LCFI108-LCFI107
	.long L$set$167
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$168,LCFI109-LCFI108
	.long L$set$168
	.byte	0x93
	.uleb128 0x5428e
	.byte	0x94
	.uleb128 0x5428d
	.byte	0x4
	.set L$set$169,LCFI110-LCFI109
	.long L$set$169
	.byte	0x95
	.uleb128 0x5428c
	.byte	0x96
	.uleb128 0x5428b
	.byte	0x4
	.set L$set$170,LCFI111-LCFI110
	.long L$set$170
	.byte	0x97
	.uleb128 0x5428a
	.byte	0x98
	.uleb128 0x54289
	.byte	0x4
	.set L$set$171,LCFI112-LCFI111
	.long L$set$171
	.byte	0x99
	.uleb128 0x54288
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
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$173,LCFI114-LCFI113
	.long L$set$173
	.byte	0xe
	.uleb128 0x2a1000
	.byte	0x4
	.set L$set$174,LCFI115-LCFI114
	.long L$set$174
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$175,LCFI116-LCFI115
	.long L$set$175
	.byte	0xb
	.byte	0x4
	.set L$set$176,LCFI117-LCFI116
	.long L$set$176
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
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$177,LCFI118-LCFI117
	.long L$set$177
	.byte	0xe
	.uleb128 0x2a1000
	.byte	0x4
	.set L$set$178,LCFI119-LCFI118
	.long L$set$178
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$179,LCFI120-LCFI119
	.long L$set$179
	.byte	0xb
	.byte	0x4
	.set L$set$180,LCFI121-LCFI120
	.long L$set$180
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
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$181,LCFI122-LCFI121
	.long L$set$181
	.byte	0xe
	.uleb128 0x2a1000
	.byte	0x4
	.set L$set$182,LCFI123-LCFI122
	.long L$set$182
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$183,LCFI124-LCFI123
	.long L$set$183
	.byte	0xb
	.byte	0x4
	.set L$set$184,LCFI125-LCFI124
	.long L$set$184
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
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$185,LCFI126-LCFI125
	.long L$set$185
	.byte	0xe
	.uleb128 0x2a1000
	.byte	0x4
	.set L$set$186,LCFI127-LCFI126
	.long L$set$186
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$187,LCFI128-LCFI127
	.long L$set$187
	.byte	0xb
	.align	3
LEFDE57:
LSFDE59:
	.set L$set$188,LEFDE59-LASFDE59
	.long L$set$188
LASFDE59:
	.long	LASFDE59-EH_frame1
	.quad	LFB33-.
	.set L$set$189,LFE33-LFB33
	.quad L$set$189
	.uleb128 0x8
	.quad	LLSDA33-.
	.byte	0x4
	.set L$set$190,LCFI129-LFB33
	.long L$set$190
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$191,LCFI130-LCFI129
	.long L$set$191
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$192,LCFI131-LCFI130
	.long L$set$192
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$193,LCFI132-LCFI131
	.long L$set$193
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$194,LCFI133-LCFI132
	.long L$set$194
	.byte	0xb
	.byte	0x4
	.set L$set$195,LCFI134-LCFI133
	.long L$set$195
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$196,LCFI135-LCFI134
	.long L$set$196
	.byte	0xb
	.byte	0x4
	.set L$set$197,LCFI136-LCFI135
	.long L$set$197
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$198,LCFI137-LCFI136
	.long L$set$198
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$199,LCFI138-LCFI137
	.long L$set$199
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$200,LCFI139-LCFI138
	.long L$set$200
	.byte	0xb
	.align	3
LEFDE59:
LSFDE61:
	.set L$set$201,LEFDE61-LASFDE61
	.long L$set$201
LASFDE61:
	.long	LASFDE61-EH_frame1
	.quad	LFB34-.
	.set L$set$202,LFE34-LFB34
	.quad L$set$202
	.uleb128 0x8
	.quad	LLSDA34-.
	.byte	0x4
	.set L$set$203,LCFI140-LFB34
	.long L$set$203
	.byte	0xe
	.uleb128 0x460
	.byte	0x4
	.set L$set$204,LCFI141-LCFI140
	.long L$set$204
	.byte	0xe
	.uleb128 0x2a1460
	.byte	0x4
	.set L$set$205,LCFI142-LCFI141
	.long L$set$205
	.byte	0x9d
	.uleb128 0x5428c
	.byte	0x9e
	.uleb128 0x5428b
	.byte	0x4
	.set L$set$206,LCFI143-LCFI142
	.long L$set$206
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$207,LCFI144-LCFI143
	.long L$set$207
	.byte	0x93
	.uleb128 0x5428a
	.byte	0x94
	.uleb128 0x54289
	.byte	0x4
	.set L$set$208,LCFI145-LCFI144
	.long L$set$208
	.byte	0x95
	.uleb128 0x54288
	.byte	0x96
	.uleb128 0x54287
	.byte	0x4
	.set L$set$209,LCFI146-LCFI145
	.long L$set$209
	.byte	0xa
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$210,LCFI147-LCFI146
	.long L$set$210
	.byte	0xe
	.uleb128 0x2a1000
	.byte	0x4
	.set L$set$211,LCFI148-LCFI147
	.long L$set$211
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$212,LCFI149-LCFI148
	.long L$set$212
	.byte	0xb
	.byte	0x4
	.set L$set$213,LCFI150-LCFI149
	.long L$set$213
	.byte	0xa
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$214,LCFI151-LCFI150
	.long L$set$214
	.byte	0xe
	.uleb128 0x2a1000
	.byte	0x4
	.set L$set$215,LCFI152-LCFI151
	.long L$set$215
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$216,LCFI153-LCFI152
	.long L$set$216
	.byte	0xb
	.byte	0x4
	.set L$set$217,LCFI154-LCFI153
	.long L$set$217
	.byte	0xa
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$218,LCFI155-LCFI154
	.long L$set$218
	.byte	0xe
	.uleb128 0x2a1000
	.byte	0x4
	.set L$set$219,LCFI156-LCFI155
	.long L$set$219
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$220,LCFI157-LCFI156
	.long L$set$220
	.byte	0xb
	.byte	0x4
	.set L$set$221,LCFI158-LCFI157
	.long L$set$221
	.byte	0xa
	.byte	0xd5
	.byte	0xd6
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xd
	.uleb128 0x1f
	.byte	0x4
	.set L$set$222,LCFI159-LCFI158
	.long L$set$222
	.byte	0xe
	.uleb128 0x2a1000
	.byte	0x4
	.set L$set$223,LCFI160-LCFI159
	.long L$set$223
	.byte	0xe
	.uleb128 0
	.byte	0x4
	.set L$set$224,LCFI161-LCFI160
	.long L$set$224
	.byte	0xb
	.align	3
LEFDE61:
LSFDE63:
	.set L$set$225,LEFDE63-LASFDE63
	.long L$set$225
LASFDE63:
	.long	LASFDE63-EH_frame1
	.quad	LFB37-.
	.set L$set$226,LFE37-LFB37
	.quad L$set$226
	.uleb128 0x8
	.quad	LLSDA37-.
	.byte	0x4
	.set L$set$227,LCFI162-LFB37
	.long L$set$227
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$228,LCFI163-LCFI162
	.long L$set$228
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$229,LCFI164-LCFI163
	.long L$set$229
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$230,LCFI165-LCFI164
	.long L$set$230
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$231,LCFI166-LCFI165
	.long L$set$231
	.byte	0xb
	.byte	0x4
	.set L$set$232,LCFI167-LCFI166
	.long L$set$232
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$233,LCFI168-LCFI167
	.long L$set$233
	.byte	0xb
	.byte	0x4
	.set L$set$234,LCFI169-LCFI168
	.long L$set$234
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$235,LCFI170-LCFI169
	.long L$set$235
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE63:
LSFDE65:
	.set L$set$236,LEFDE65-LASFDE65
	.long L$set$236
LASFDE65:
	.long	LASFDE65-EH_frame1
	.quad	LFB38-.
	.set L$set$237,LFE38-LFB38
	.quad L$set$237
	.uleb128 0x8
	.quad	LLSDA38-.
	.byte	0x4
	.set L$set$238,LCFI171-LFB38
	.long L$set$238
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$239,LCFI172-LCFI171
	.long L$set$239
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$240,LCFI173-LCFI172
	.long L$set$240
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$241,LCFI174-LCFI173
	.long L$set$241
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$242,LCFI175-LCFI174
	.long L$set$242
	.byte	0xb
	.byte	0x4
	.set L$set$243,LCFI176-LCFI175
	.long L$set$243
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$244,LCFI177-LCFI176
	.long L$set$244
	.byte	0xb
	.byte	0x4
	.set L$set$245,LCFI178-LCFI177
	.long L$set$245
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$246,LCFI179-LCFI178
	.long L$set$246
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE65:
LSFDE67:
	.set L$set$247,LEFDE67-LASFDE67
	.long L$set$247
LASFDE67:
	.long	LASFDE67-EH_frame1
	.quad	LFB39-.
	.set L$set$248,LFE39-LFB39
	.quad L$set$248
	.uleb128 0x8
	.quad	LLSDA39-.
	.byte	0x4
	.set L$set$249,LCFI180-LFB39
	.long L$set$249
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$250,LCFI181-LCFI180
	.long L$set$250
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$251,LCFI182-LCFI181
	.long L$set$251
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$252,LCFI183-LCFI182
	.long L$set$252
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$253,LCFI184-LCFI183
	.long L$set$253
	.byte	0xb
	.byte	0x4
	.set L$set$254,LCFI185-LCFI184
	.long L$set$254
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$255,LCFI186-LCFI185
	.long L$set$255
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE67:
LSFDE69:
	.set L$set$256,LEFDE69-LASFDE69
	.long L$set$256
LASFDE69:
	.long	LASFDE69-EH_frame1
	.quad	LFB40-.
	.set L$set$257,LFE40-LFB40
	.quad L$set$257
	.uleb128 0x8
	.quad	LLSDA40-.
	.byte	0x4
	.set L$set$258,LCFI187-LFB40
	.long L$set$258
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$259,LCFI188-LCFI187
	.long L$set$259
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$260,LCFI189-LCFI188
	.long L$set$260
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$261,LCFI190-LCFI189
	.long L$set$261
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$262,LCFI191-LCFI190
	.long L$set$262
	.byte	0xb
	.byte	0x4
	.set L$set$263,LCFI192-LCFI191
	.long L$set$263
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$264,LCFI193-LCFI192
	.long L$set$264
	.byte	0xb
	.byte	0x4
	.set L$set$265,LCFI194-LCFI193
	.long L$set$265
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$266,LCFI195-LCFI194
	.long L$set$266
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE69:
LSFDE71:
	.set L$set$267,LEFDE71-LASFDE71
	.long L$set$267
LASFDE71:
	.long	LASFDE71-EH_frame1
	.quad	LFB41-.
	.set L$set$268,LFE41-LFB41
	.quad L$set$268
	.uleb128 0x8
	.quad	LLSDA41-.
	.byte	0x4
	.set L$set$269,LCFI196-LFB41
	.long L$set$269
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$270,LCFI197-LCFI196
	.long L$set$270
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$271,LCFI198-LCFI197
	.long L$set$271
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$272,LCFI199-LCFI198
	.long L$set$272
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$273,LCFI200-LCFI199
	.long L$set$273
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE71:
LSFDE73:
	.set L$set$274,LEFDE73-LASFDE73
	.long L$set$274
LASFDE73:
	.long	LASFDE73-EH_frame1
	.quad	LFB42-.
	.set L$set$275,LFE42-LFB42
	.quad L$set$275
	.uleb128 0x8
	.quad	0
	.align	3
LEFDE73:
LSFDE75:
	.set L$set$276,LEFDE75-LASFDE75
	.long L$set$276
LASFDE75:
	.long	LASFDE75-EH_frame1
	.quad	LFB43-.
	.set L$set$277,LFE43-LFB43
	.quad L$set$277
	.uleb128 0x8
	.quad	LLSDA43-.
	.byte	0x4
	.set L$set$278,LCFI201-LFB43
	.long L$set$278
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$279,LCFI202-LCFI201
	.long L$set$279
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$280,LCFI203-LCFI202
	.long L$set$280
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$281,LCFI204-LCFI203
	.long L$set$281
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$282,LCFI205-LCFI204
	.long L$set$282
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE75:
LSFDE77:
	.set L$set$283,LEFDE77-LASFDE77
	.long L$set$283
LASFDE77:
	.long	LASFDE77-EH_frame1
	.quad	LFB44-.
	.set L$set$284,LFE44-LFB44
	.quad L$set$284
	.uleb128 0x8
	.quad	LLSDA44-.
	.byte	0x4
	.set L$set$285,LCFI206-LFB44
	.long L$set$285
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$286,LCFI207-LCFI206
	.long L$set$286
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$287,LCFI208-LCFI207
	.long L$set$287
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x4
	.set L$set$288,LCFI209-LCFI208
	.long L$set$288
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
	.set L$set$289,LCFI210-LCFI209
	.long L$set$289
	.byte	0xb
	.byte	0x4
	.set L$set$290,LCFI211-LCFI210
	.long L$set$290
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
	.set L$set$291,LCFI212-LCFI211
	.long L$set$291
	.byte	0xb
	.byte	0x4
	.set L$set$292,LCFI213-LCFI212
	.long L$set$292
	.byte	0xde
	.byte	0xdd
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$293,LCFI214-LCFI213
	.long L$set$293
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x70
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.align	3
LEFDE77:
LSFDE79:
	.set L$set$294,LEFDE79-LASFDE79
	.long L$set$294
LASFDE79:
	.long	LASFDE79-EH_frame1
	.quad	LFB45-.
	.set L$set$295,LFE45-LFB45
	.quad L$set$295
	.uleb128 0x8
	.quad	LLSDA45-.
	.byte	0x4
	.set L$set$296,LCFI215-LFB45
	.long L$set$296
	.byte	0xe
	.uleb128 0x70
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.byte	0x4
	.set L$set$297,LCFI216-LCFI215
	.long L$set$297
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$298,LCFI217-LCFI216
	.long L$set$298
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x4
	.set L$set$299,LCFI218-LCFI217
	.long L$set$299
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
	.set L$set$300,LCFI219-LCFI218
	.long L$set$300
	.byte	0xb
	.byte	0x4
	.set L$set$301,LCFI220-LCFI219
	.long L$set$301
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
	.set L$set$302,LCFI221-LCFI220
	.long L$set$302
	.byte	0xb
	.byte	0x4
	.set L$set$303,LCFI222-LCFI221
	.long L$set$303
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
	.set L$set$304,LCFI223-LCFI222
	.long L$set$304
	.byte	0xb
	.byte	0x4
	.set L$set$305,LCFI224-LCFI223
	.long L$set$305
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xd3
	.byte	0xd4
	.byte	0xd5
	.byte	0xdd
	.byte	0xde
	.byte	0x4
	.set L$set$306,LCFI225-LCFI224
	.long L$set$306
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x70
	.byte	0x93
	.uleb128 0xc
	.byte	0x94
	.uleb128 0xb
	.byte	0x95
	.uleb128 0xa
	.byte	0x9d
	.uleb128 0xe
	.byte	0x9e
	.uleb128 0xd
	.align	3
LEFDE79:
LSFDE81:
	.set L$set$307,LEFDE81-LASFDE81
	.long L$set$307
LASFDE81:
	.long	LASFDE81-EH_frame1
	.quad	LFB46-.
	.set L$set$308,LFE46-LFB46
	.quad L$set$308
	.uleb128 0x8
	.quad	LLSDA46-.
	.byte	0x4
	.set L$set$309,LCFI226-LFB46
	.long L$set$309
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$310,LCFI227-LCFI226
	.long L$set$310
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$311,LCFI228-LCFI227
	.long L$set$311
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x4
	.set L$set$312,LCFI229-LCFI228
	.long L$set$312
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$313,LCFI230-LCFI229
	.long L$set$313
	.byte	0xb
	.byte	0x4
	.set L$set$314,LCFI231-LCFI230
	.long L$set$314
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$315,LCFI232-LCFI231
	.long L$set$315
	.byte	0xb
	.byte	0x4
	.set L$set$316,LCFI233-LCFI232
	.long L$set$316
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$317,LCFI234-LCFI233
	.long L$set$317
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x60
	.byte	0x93
	.uleb128 0xa
	.byte	0x94
	.uleb128 0x9
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.align	3
LEFDE81:
LSFDE83:
	.set L$set$318,LEFDE83-LASFDE83
	.long L$set$318
LASFDE83:
	.long	LASFDE83-EH_frame1
	.quad	LFB47-.
	.set L$set$319,LFE47-LFB47
	.quad L$set$319
	.uleb128 0x8
	.quad	LLSDA47-.
	.byte	0x4
	.set L$set$320,LCFI235-LFB47
	.long L$set$320
	.byte	0xe
	.uleb128 0x86b0
	.byte	0x4
	.set L$set$321,LCFI236-LCFI235
	.long L$set$321
	.byte	0x9d
	.uleb128 0x10d6
	.byte	0x9e
	.uleb128 0x10d5
	.byte	0x4
	.set L$set$322,LCFI237-LCFI236
	.long L$set$322
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$323,LCFI238-LCFI237
	.long L$set$323
	.byte	0x93
	.uleb128 0x10d4
	.byte	0x94
	.uleb128 0x10d3
	.byte	0x4
	.set L$set$324,LCFI239-LCFI238
	.long L$set$324
	.byte	0xa
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$325,LCFI240-LCFI239
	.long L$set$325
	.byte	0xb
	.byte	0x4
	.set L$set$326,LCFI241-LCFI240
	.long L$set$326
	.byte	0xa
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$327,LCFI242-LCFI241
	.long L$set$327
	.byte	0xb
	.byte	0x4
	.set L$set$328,LCFI243-LCFI242
	.long L$set$328
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$329,LCFI244-LCFI243
	.long L$set$329
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x86b0
	.byte	0x93
	.uleb128 0x10d4
	.byte	0x94
	.uleb128 0x10d3
	.byte	0x9d
	.uleb128 0x10d6
	.byte	0x9e
	.uleb128 0x10d5
	.align	3
LEFDE83:
LSFDE85:
	.set L$set$330,LEFDE85-LASFDE85
	.long L$set$330
LASFDE85:
	.long	LASFDE85-EH_frame1
	.quad	LFB48-.
	.set L$set$331,LFE48-LFB48
	.quad L$set$331
	.uleb128 0x8
	.quad	LLSDA48-.
	.byte	0x4
	.set L$set$332,LCFI245-LFB48
	.long L$set$332
	.byte	0xe
	.uleb128 0x8700
	.byte	0x4
	.set L$set$333,LCFI246-LCFI245
	.long L$set$333
	.byte	0x9d
	.uleb128 0x10e0
	.byte	0x9e
	.uleb128 0x10df
	.byte	0x4
	.set L$set$334,LCFI247-LCFI246
	.long L$set$334
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$335,LCFI248-LCFI247
	.long L$set$335
	.byte	0x93
	.uleb128 0x10de
	.byte	0x94
	.uleb128 0x10dd
	.byte	0x95
	.uleb128 0x10dc
	.byte	0x4
	.set L$set$336,LCFI249-LCFI248
	.long L$set$336
	.byte	0xd5
	.byte	0xd3
	.byte	0xd4
	.byte	0xdd
	.byte	0xde
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$337,LCFI250-LCFI249
	.long L$set$337
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x8700
	.byte	0x93
	.uleb128 0x10de
	.byte	0x94
	.uleb128 0x10dd
	.byte	0x95
	.uleb128 0x10dc
	.byte	0x9d
	.uleb128 0x10e0
	.byte	0x9e
	.uleb128 0x10df
	.align	3
LEFDE85:
LSFDE87:
	.set L$set$338,LEFDE87-LASFDE87
	.long L$set$338
LASFDE87:
	.long	LASFDE87-EH_frame1
	.quad	LFB49-.
	.set L$set$339,LFE49-LFB49
	.quad L$set$339
	.uleb128 0x8
	.quad	LLSDA49-.
	.byte	0x4
	.set L$set$340,LCFI251-LFB49
	.long L$set$340
	.byte	0xe
	.uleb128 0x60
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.byte	0x4
	.set L$set$341,LCFI252-LCFI251
	.long L$set$341
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$342,LCFI253-LCFI252
	.long L$set$342
	.byte	0x93
	.uleb128 0xa
	.byte	0x4
	.set L$set$343,LCFI254-LCFI253
	.long L$set$343
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$344,LCFI255-LCFI254
	.long L$set$344
	.byte	0xb
	.byte	0x4
	.set L$set$345,LCFI256-LCFI255
	.long L$set$345
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$346,LCFI257-LCFI256
	.long L$set$346
	.byte	0xb
	.byte	0x4
	.set L$set$347,LCFI258-LCFI257
	.long L$set$347
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$348,LCFI259-LCFI258
	.long L$set$348
	.byte	0xb
	.byte	0x4
	.set L$set$349,LCFI260-LCFI259
	.long L$set$349
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0xd3
	.byte	0xdd
	.byte	0xde
	.byte	0x4
	.set L$set$350,LCFI261-LCFI260
	.long L$set$350
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x60
	.byte	0x93
	.uleb128 0xa
	.byte	0x9d
	.uleb128 0xc
	.byte	0x9e
	.uleb128 0xb
	.align	3
LEFDE87:
LSFDE89:
	.set L$set$351,LEFDE89-LASFDE89
	.long L$set$351
LASFDE89:
	.long	LASFDE89-EH_frame1
	.quad	LFB52-.
	.set L$set$352,LFE52-LFB52
	.quad L$set$352
	.uleb128 0x8
	.quad	LLSDA52-.
	.byte	0x4
	.set L$set$353,LCFI262-LFB52
	.long L$set$353
	.byte	0xe
	.uleb128 0x20
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.byte	0x4
	.set L$set$354,LCFI263-LCFI262
	.long L$set$354
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$355,LCFI264-LCFI263
	.long L$set$355
	.byte	0x93
	.uleb128 0x2
	.byte	0x4
	.set L$set$356,LCFI265-LCFI264
	.long L$set$356
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$357,LCFI266-LCFI265
	.long L$set$357
	.byte	0xc
	.uleb128 0x1d
	.uleb128 0x20
	.byte	0x93
	.uleb128 0x2
	.byte	0x9d
	.uleb128 0x4
	.byte	0x9e
	.uleb128 0x3
	.align	3
LEFDE89:
LSFDE91:
	.set L$set$358,LEFDE91-LASFDE91
	.long L$set$358
LASFDE91:
	.long	LASFDE91-EH_frame1
	.quad	LFB53-.
	.set L$set$359,LFE53-LFB53
	.quad L$set$359
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$360,LCFI267-LFB53
	.long L$set$360
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$361,LCFI268-LCFI267
	.long L$set$361
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE91:
LSFDE93:
	.set L$set$362,LEFDE93-LASFDE93
	.long L$set$362
LASFDE93:
	.long	LASFDE93-EH_frame1
	.quad	LFB54-.
	.set L$set$363,LFE54-LFB54
	.quad L$set$363
	.uleb128 0x8
	.quad	0
	.byte	0x4
	.set L$set$364,LCFI269-LFB54
	.long L$set$364
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$365,LCFI270-LCFI269
	.long L$set$365
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE93:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
