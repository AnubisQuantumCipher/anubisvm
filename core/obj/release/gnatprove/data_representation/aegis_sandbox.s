	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__Treturn_data_bufferBIP
_aegis_sandbox__Treturn_data_bufferBIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__return_dataIP
_aegis_sandbox__return_dataIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__syscall_numberH
_aegis_sandbox__syscall_numberH:
LFB4:
	ldp	w1, w7, [x1]
	adrp	x10, _syscall_numberP.12@PAGE
	adrp	x12, _syscall_numberT1.11@PAGE
	adrp	x11, _syscall_numberT2.10@PAGE
	mov	x5, 0
	mov	w2, 0
	mov	w4, 0
	add	x10, x10, _syscall_numberP.12@PAGEOFF;
	add	x12, x12, _syscall_numberT1.11@PAGEOFF;
	add	x11, x11, _syscall_numberT2.10@PAGEOFF;
	mov	w8, 63
	sub	w9, w1, #1
	sxtw	x13, w1
	sxtw	x9, w9
	cmp	w1, w7
	sub	w7, w7, w1
	csinc	w7, wzr, w7, gt
L7:
	ldr	w3, [x10, x5, lsl 2]
	add	x1, x9, w3, sxtw
	sub	x1, x1, x13
	cmp	w7, w3
	blt	L6
	ldrb	w6, [x0, x1]
	ldrb	w3, [x12, x5]
	ldrb	w1, [x11, x5]
	add	x5, x5, 1
	madd	w3, w3, w6, w4
	madd	w1, w1, w6, w2
	sdiv	w2, w3, w8
	sdiv	w6, w1, w8
	lsl	w4, w2, 6
	sub	w4, w4, w2
	sub	w4, w3, w4
	lsl	w2, w6, 6
	sub	w2, w2, w6
	sub	w2, w1, w2
	cmp	x5, 5
	bne	L7
L6:
	adrp	x3, _syscall_numberG.9@PAGE
	mov	w0, 31
	add	x3, x3, _syscall_numberG.9@PAGEOFF;
	ldrb	w2, [x3, w2, sxtw]
	ldrb	w1, [x3, w4, sxtw]
	add	w1, w1, w2
	udiv	w2, w1, w0
	lsl	w0, w2, 5
	sub	w0, w0, w2
	sub	w0, w1, w0
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__syscall_resultH
_aegis_sandbox__syscall_resultH:
LFB5:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L13
	sub	w1, w1, w2
	cmp	w1, 8
	bgt	L14
L13:
	mov	x3, 0
	mov	x0, 0
L11:
	adrp	x2, _syscall_resultG.5@PAGE
	mov	w1, 52429
	add	x2, x2, _syscall_resultG.5@PAGEOFF;
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
L14:
	ldrb	w3, [x0, 9]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w0, w3, w3, lsl 1
	add	w3, w3, w3, lsl 3
	umull	x2, w0, w1
	umull	x1, w3, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	add	w5, w2, w2, lsl 2
	add	w4, w1, w1, lsl 2
	add	w2, w2, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w0, w0, w2
	sub	w3, w3, w1
	sxtw	x0, w0
	sxtw	x3, w3
	b	L11
LFE5:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__sandbox_statusH
_aegis_sandbox__sandbox_statusH:
LFB6:
	ldp	w2, w1, [x1]
	cmp	w2, w1
	bgt	L18
	sub	w1, w1, w2
	cmp	w1, 9
	bgt	L19
L18:
	mov	x3, 0
	mov	x0, 0
L16:
	adrp	x2, _sandbox_statusG.1@PAGE
	mov	w1, 52429
	add	x2, x2, _sandbox_statusG.1@PAGEOFF;
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
L19:
	ldrb	w3, [x0, 10]
	mov	w1, 35747
	movk	w1, 0xba2e, lsl 16
	add	w0, w3, w3, lsl 1
	add	w3, w3, w3, lsl 3
	umull	x2, w0, w1
	umull	x1, w3, w1
	lsr	x2, x2, 35
	lsr	x1, x1, 35
	add	w5, w2, w2, lsl 2
	add	w4, w1, w1, lsl 2
	add	w2, w2, w5, lsl 1
	add	w1, w1, w4, lsl 1
	sub	w0, w0, w2
	sub	w3, w3, w1
	sxtw	x0, w0
	sxtw	x3, w3
	b	L16
LFE6:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__call_frameIP
_aegis_sandbox__call_frameIP:
LFB7:
	adrp	x1, lC0@PAGE
	movi	v31.4s, 0
	stp	xzr, xzr, [x0]
	add	x1, x1, lC0@PAGEOFF;
	ldr	x2, [x1]
	stp	xzr, xzr, [x0, 16]
	ldrb	w1, [x1, 8]
	stp	xzr, xzr, [x0, 32]
	stp	xzr, xzr, [x0, 48]
	stp	q31, q31, [x0, 64]
	str	q31, [x0, 96]
	stp	xzr, xzr, [x0, 112]
	stp	xzr, xzr, [x0, 128]
	stp	xzr, xzr, [x0, 144]
	stp	xzr, xzr, [x0, 160]
	strh	wzr, [x0, 176]
	strb	wzr, [x0, 178]
	str	x2, [x0, 179]
	strb	w1, [x0, 187]
	strb	wzr, [x0, 188]
	ret
LFE7:
	.const
lC0:
	.byte	0
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.byte	1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__Tcall_stackBIP
_aegis_sandbox__Tcall_stackBIP:
LFB8:
	ldrsh	w7, [x1]
	ldrsh	w4, [x1, 2]
	cmp	w7, w4
	bgt	L21
	movi	v31.4s, 0
	sxtw	x5, w7
	adrp	x1, lC7@PAGE
	mov	x3, x5
	add	x2, x0, 64
	mov	w6, 1
	ldr	d30, [x1, #lC7@PAGEOFF]
	sub	w4, w4, w7
	add	x1, x5, 1
	add	x4, x1, w4, uxth
	.p2align 5,,15
L23:
	sub	x1, x3, x5
	add	x3, x3, 1
	add	x1, x1, x1, lsl 1
	add	x2, x2, 192
	add	x1, x0, x1, lsl 6
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	stp	xzr, xzr, [x1, 32]
	stp	xzr, xzr, [x1, 48]
	stp	q31, q31, [x2, -192]
	str	q31, [x2, -160]
	stp	xzr, xzr, [x1, 112]
	stp	xzr, xzr, [x1, 128]
	stp	xzr, xzr, [x1, 144]
	stp	xzr, xzr, [x1, 160]
	strh	wzr, [x1, 176]
	strb	wzr, [x1, 178]
	str	d30, [x2, -77]
	strb	w6, [x2, -69]
	strb	wzr, [x1, 188]
	cmp	x4, x3
	bne	L23
L21:
	ret
LFE8:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__sandbox_contextIP
_aegis_sandbox__sandbox_contextIP:
LFB9:
	adrp	x1, lC7@PAGE
	movi	v27.4s, 0
	mov	x7, 1103806595072
	movk	x7, 0x101, lsl 48
	mov	w6, 16843009
	stp	xzr, xzr, [x0, 8]
	mov	x3, 0
	add	x2, x0, 264
	add	x5, x0, 200
	mov	w4, 1
	ldr	d26, [x1, #lC7@PAGEOFF]
	stp	xzr, xzr, [x0, 24]
	stp	xzr, xzr, [x0, 40]
	stp	xzr, xzr, [x0, 56]
	str	q27, [x0, 72]
	str	q27, [x0, 88]
	str	q27, [x0, 104]
	stp	xzr, xzr, [x0, 120]
	stp	xzr, xzr, [x0, 136]
	stp	xzr, xzr, [x0, 152]
	stp	xzr, xzr, [x0, 168]
	str	x7, [x0, 184]
	str	w6, [x0, 192]
	strb	wzr, [x0, 196]
	.p2align 5,,15
L26:
	add	x1, x3, x3, lsl 1
	add	x3, x3, 1
	add	x2, x2, 192
	add	x1, x5, x1, lsl 6
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	stp	xzr, xzr, [x1, 32]
	stp	xzr, xzr, [x1, 48]
	stp	q27, q27, [x2, -192]
	str	q27, [x2, -160]
	stp	xzr, xzr, [x1, 112]
	stp	xzr, xzr, [x1, 128]
	stp	xzr, xzr, [x1, 144]
	stp	xzr, xzr, [x1, 160]
	strh	wzr, [x1, 176]
	strb	wzr, [x1, 178]
	str	d26, [x2, -77]
	strb	w4, [x2, -69]
	strb	wzr, [x1, 188]
	cmp	x3, 1024
	bne	L26
	add	x3, x0, 196608
	add	x4, x0, 196608
	mov	x2, 0
	add	x3, x3, 296
	add	x4, x4, 264
	.p2align 5,,15
L27:
	movi	v28.4s, 0
	add	x1, x2, x2, lsl 3
	add	x2, x2, 1
	add	x1, x4, x1, lsl 3
	mov	v29.16b, v28.16b
	stp	xzr, xzr, [x1]
	stp	xzr, xzr, [x1, 16]
	st1	{v28.16b - v29.16b}, [x3]
	strb	wzr, [x1, 64]
	add	x3, x3, 72
	cmp	x2, 256
	bne	L27
	add	x3, x0, 212992
	mov	x2, 0
	add	x3, x3, 2320
	.p2align 5,,15
L28:
	add	x1, x2, x2, lsl 1
	add	x2, x2, 1
	lsl	x4, x1, 3
	add	x1, x3, x1, lsl 3
	strh	wzr, [x3, x4]
	str	wzr, [x1, 4]
	str	xzr, [x1, 8]
	strb	wzr, [x1, 16]
	cmp	x2, 1024
	bne	L28
	add	x1, x0, 237568
	add	x0, x0, 237568
	mov	x2, 0
	add	x1, x1, 2368
	add	x0, x0, 2328
	.p2align 5,,15
L29:
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
	bne	L29
	ret
LFE9:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__block_infoIP
_aegis_sandbox__block_infoIP:
LFB95:
	ret
LFE95:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__transaction_infoIP
_aegis_sandbox__transaction_infoIP:
LFB97:
	ret
LFE97:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__execution_environmentIP
_aegis_sandbox__execution_environmentIP:
LFB99:
	ret
LFE99:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__memory_access_recordIP
_aegis_sandbox__memory_access_recordIP:
LFB13:
	mov	x0, 0
	mov	x1, 0
	ret
LFE13:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__memory_expansion_resultIP
_aegis_sandbox__memory_expansion_resultIP:
LFB101:
	ret
LFE101:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__Tlog_data_bufferBIP
_aegis_sandbox__Tlog_data_bufferBIP:
LFB15:
	ret
LFE15:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__Tlog_topicsBIP
_aegis_sandbox__Tlog_topicsBIP:
LFB16:
	ret
LFE16:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__log_entryIP
_aegis_sandbox__log_entryIP:
LFB103:
	ret
LFE103:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__Tlog_listBIP
_aegis_sandbox__Tlog_listBIP:
LFB18:
	ret
LFE18:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__transaction_logsIP
_aegis_sandbox__transaction_logsIP:
LFB105:
	ret
LFE105:
	.const
	.align	3
lC8:
	.ascii "aegis_sandbox.adb"
	.space 1
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__has_capability
_aegis_sandbox__has_capability:
LFB20:
	cmp	w1, 8
	bhi	L46
	ldrb	w0, [x0, w1, uxtw]
	cmp	w0, 1
	bhi	L46
	ret
L46:
	adrp	x0, lC8@PAGE
	stp	x29, x30, [sp, -16]!
LCFI0:
	mov	w1, 16
	mov	x29, sp
LCFI1:
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE20:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__is_static_safe
_aegis_sandbox__is_static_safe:
LFB22:
	cmp	w0, 30
	bhi	L56
	mov	x1, 65533
	cmp	w0, 25
	movk	x1, 0x1c5, lsl 16
	lsr	x0, x1, x0
	and	w0, w0, 1
	csel	w0, w0, wzr, cc
	ret
L56:
	adrp	x0, lC8@PAGE
	stp	x29, x30, [sp, -16]!
LCFI2:
	mov	w1, 22
	mov	x29, sp
LCFI3:
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE22:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__required_capabilities
_aegis_sandbox__required_capabilities:
LFB23:
	stp	x29, x30, [sp, -48]!
LCFI4:
	mov	x29, sp
LCFI5:
	stp	xzr, xzr, [x29, 32]
	cmp	w0, 30
	bhi	L60
	adrp	x6, _CSWTCH.142@PAGE
	adrp	x5, _CSWTCH.143@PAGE
	adrp	x4, _CSWTCH.144@PAGE
	adrp	x3, _CSWTCH.145@PAGE
	ldrb	w1, [x29, 40]
	adrp	x2, _CSWTCH.146@PAGE
	add	x6, x6, _CSWTCH.142@PAGEOFF;
	add	x2, x2, _CSWTCH.146@PAGEOFF;
	add	x5, x5, _CSWTCH.143@PAGEOFF;
	ldrb	w6, [x6, w0, uxtw]
	add	x4, x4, _CSWTCH.144@PAGEOFF;
	add	x3, x3, _CSWTCH.145@PAGEOFF;
	ldrb	w5, [x5, w0, uxtw]
	ldrb	w4, [x4, w0, uxtw]
	strb	w6, [x29, 33]
	ldrb	w3, [x3, w0, uxtw]
	strb	w5, [x29, 34]
	ldrb	w0, [x2, w0, uxtw]
	strb	w4, [x29, 35]
	strb	w3, [x29, 37]
	strb	w0, [x29, 38]
	ldr	x0, [x29, 32]
	ldp	x29, x30, [sp], 48
LCFI6:
	ret
L60:
LCFI7:
	adrp	x0, lC8@PAGE
	mov	w1, 40
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE23:
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__validate_syscall
_aegis_sandbox__validate_syscall:
LFB24:
	stp	x29, x30, [sp, -48]!
LCFI8:
	mov	x29, sp
LCFI9:
	stp	x19, x20, [sp, 16]
LCFI10:
	cmp	w1, 30
	bhi	L82
	mov	x19, x0
	mov	w0, w1
	mov	w20, w1
	bl	_aegis_sandbox__required_capabilities
	ubfx	x2, x0, 8, 8
	ldrb	w3, [x19, 196]
	ubfx	x7, x0, 16, 8
	lsr	w6, w0, 24
	ubfx	x5, x0, 32, 8
	strb	w1, [x29, 40]
	ubfx	x4, x0, 40, 8
	ubfx	x1, x0, 48, 8
	strb	w2, [x29, 33]
	lsr	x2, x0, 56
	strb	w0, [x29, 32]
	strb	w7, [x29, 34]
	strb	w6, [x29, 35]
	strb	w5, [x29, 36]
	strb	w4, [x29, 37]
	strb	w1, [x29, 38]
	strb	w2, [x29, 39]
	cmp	w3, 1
	bhi	L83
	mov	x2, -1
	cbnz	w3, L84
L64:
	add	x3, x29, 32
	add	x0, x19, 188
	.p2align 5,,15
L69:
	add	x1, x3, x2
	ldrb	w1, [x1, 1]
	cmp	w1, 1
	bhi	L85
	cbz	w1, L67
	ldrb	w1, [x0, x2]
	cmp	w1, 1
	bhi	L86
	cbz	w1, L75
L67:
	add	x2, x2, 1
	cmp	x2, 8
	bne	L69
	add	x19, x19, 196608
	ldr	x1, [x19, 216]
	tbnz	x1, #63, L71
	ldr	x0, [x19, 208]
	tbnz	x0, #63, L71
	cmp	x1, x0
	cset	w0, ge
	lsl	w0, w0, 1
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI11:
	ret
	.p2align 2,,3
L84:
LCFI12:
	cmp	w20, 24
	bhi	L75
	mov	x0, 65533
	movk	x0, 0x1c5, lsl 16
	lsr	x0, x0, x20
	tbnz	x0, 0, L64
L75:
	mov	w0, 1
	ldp	x19, x20, [sp, 16]
	ldp	x29, x30, [sp], 48
LCFI13:
	ret
L85:
LCFI14:
	adrp	x0, lC8@PAGE
	mov	w1, 77
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L86:
	adrp	x0, lC8@PAGE
	mov	w1, 78
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L71:
	adrp	x0, lC8@PAGE
	mov	w1, 84
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L83:
	adrp	x0, lC8@PAGE
	mov	w1, 70
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L82:
	adrp	x0, lC8@PAGE
	mov	w1, 67
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE24:
	.const
	.align	3
lC9:
	.ascii "aegis_sandbox.ads"
	.space 1
	.align	3
lC10:
	.ascii "failed precondition from aegis_sandbox.ads:305"
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox__validate_memory_access
_aegis_sandbox__validate_memory_access:
LFB25:
	stp	x29, x30, [sp, -16]!
LCFI15:
	orr	w4, w2, w1
	mov	x29, sp
LCFI16:
	tbnz	w4, #31, L99
	mov	w4, 2147483647
	sub	w4, w4, w2
	cmp	w1, w4
	bgt	L100
	add	x0, x0, 196608
	ldr	w0, [x0, 204]
	tbnz	w0, #31, L101
	adds	w2, w2, w1
	bvs	L92
	cmp	w0, w2
	blt	L102
	cmp	w3, 2
	bhi	L103
	cset	w0, ne
	ldp	x29, x30, [sp], 16
LCFI17:
	ret
	.p2align 2,,3
L102:
LCFI18:
	mov	w0, 0
	ldp	x29, x30, [sp], 16
LCFI19:
	ret
L99:
LCFI20:
	adrp	x0, lC9@PAGE
	mov	w1, 305
	add	x0, x0, lC9@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L92:
	adrp	x0, lC8@PAGE
	mov	w1, 99
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Overflow_Check
L101:
	adrp	x0, lC8@PAGE
	mov	w1, 99
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
L100:
	adrp	x0, lC10@PAGE
	adrp	x1, lC6@PAGE
	add	x0, x0, lC10@PAGEOFF;
	add	x1, x1, lC6@PAGEOFF;
	bl	_system__assertions__raise_assert_failure
L103:
	adrp	x0, lC8@PAGE
	mov	w1, 104
	add	x0, x0, lC8@PAGEOFF;
	bl	___gnat_rcheck_CE_Invalid_Data
LFE25:
	.const
	.align	2
lC6:
	.word	1
	.word	46
	.text
	.align	2
	.p2align 5,,15
	.globl _aegis_sandbox___elabs
_aegis_sandbox___elabs:
LFB1:
	adrp	x3, _aegis_sandbox__empty_return_data@PAGE
	stp	x29, x30, [sp, -16]!
LCFI21:
	mov	x2, 24576
	add	x3, x3, _aegis_sandbox__empty_return_data@PAGEOFF;
	mov	x29, sp
LCFI22:
	mov	w1, 0
	mov	x0, x3
	bl	_memset
	mov	x3, x0
	add	x0, x0, 16384
	add	x3, x3, 24576
	str	wzr, [x0, 8192]
	strb	wzr, [x3, 4]
	ldp	x29, x30, [sp], 16
LCFI23:
	ret
LFE1:
	.const
_CSWTCH.146:
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
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
_CSWTCH.145:
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
	.byte	1
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
_CSWTCH.144:
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
	.byte	1
	.byte	1
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
_CSWTCH.143:
	.byte	0
	.byte	1
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
	.byte	1
	.byte	1
	.byte	1
	.byte	1
_CSWTCH.142:
	.byte	1
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
	.align	3
_sandbox_statusG.1:
	.byte	0
	.byte	4
	.byte	0
	.byte	0
	.byte	0
	.byte	3
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	2
	.space 5
	.align	3
_syscall_resultG.5:
	.byte	0
	.byte	3
	.byte	0
	.byte	3
	.byte	0
	.byte	0
	.byte	2
	.byte	0
	.byte	0
	.byte	1
	.byte	0
	.space 5
_syscall_numberG.9:
	.byte	0
	.byte	4
	.byte	0
	.byte	26
	.byte	13
	.byte	16
	.byte	3
	.byte	0
	.byte	17
	.byte	16
	.byte	9
	.byte	23
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	0
	.byte	6
	.byte	0
	.byte	1
	.byte	0
	.byte	0
	.byte	4
	.byte	10
	.byte	0
	.byte	0
	.byte	19
	.byte	0
	.byte	0
	.byte	14
	.byte	0
	.byte	3
	.byte	25
	.byte	0
	.byte	0
	.byte	26
	.byte	0
	.byte	23
	.byte	0
	.byte	16
	.byte	26
	.byte	0
	.byte	0
	.byte	0
	.byte	7
	.byte	23
	.byte	24
	.byte	0
	.byte	27
	.byte	0
	.byte	0
	.byte	0
	.byte	12
	.byte	0
	.byte	16
	.byte	0
	.byte	3
	.byte	0
	.byte	25
	.byte	15
	.align	3
_syscall_numberT2.10:
	.byte	25
	.byte	46
	.byte	9
	.byte	7
	.byte	31
	.space 3
	.align	3
_syscall_numberT1.11:
	.byte	49
	.byte	6
	.byte	44
	.byte	19
	.byte	6
	.space 3
	.align	2
_syscall_numberP.12:
	.word	5
	.word	8
	.word	9
	.word	11
	.word	13
	.globl _aegis_sandbox__memory_opN
	.align	2
_aegis_sandbox__memory_opN:
	.byte	1
	.byte	9
	.byte	18
	.byte	26
	.globl _aegis_sandbox__memory_opS
	.align	3
_aegis_sandbox__memory_opS:
	.ascii "MEM_READMEM_WRITEMEM_COPY"
	.globl _aegis_sandbox__sandbox_statusN
	.align	3
_aegis_sandbox__sandbox_statusN:
	.byte	1
	.byte	16
	.byte	31
	.byte	47
	.byte	63
	.byte	76
	.space 2
	.globl _aegis_sandbox__sandbox_statusS
	.align	3
_aegis_sandbox__sandbox_statusS:
	.ascii "SANDBOX_RUNNINGSANDBOX_STOPPEDSANDBOX_RETURNEDSANDBOX_REVERTEDSANDBOX_ERROR"
	.globl _aegis_sandbox__syscall_resultN
	.align	3
_aegis_sandbox__syscall_resultN:
	.byte	1
	.byte	11
	.byte	25
	.byte	43
	.byte	56
	.byte	71
	.space 2
	.globl _aegis_sandbox__syscall_resultS
	.align	3
_aegis_sandbox__syscall_resultS:
	.ascii "SYSCALL_OKSYSCALL_DENIEDSYSCALL_OUT_OF_GASSYSCALL_ERRORSYSCALL_INVALID"
	.globl _aegis_sandbox__syscall_numberN
	.align	1
_aegis_sandbox__syscall_numberN:
	.hword	1
	.hword	10
	.hword	20
	.hword	28
	.hword	41
	.hword	57
	.hword	73
	.hword	83
	.hword	94
	.hword	107
	.hword	119
	.hword	135
	.hword	150
	.hword	163
	.hword	175
	.hword	191
	.hword	202
	.hword	217
	.hword	225
	.hword	239
	.hword	255
	.hword	265
	.hword	276
	.hword	286
	.hword	296
	.hword	304
	.hword	320
	.hword	328
	.hword	336
	.hword	344
	.hword	352
	.hword	360
	.globl _aegis_sandbox__syscall_numberS
	.align	3
_aegis_sandbox__syscall_numberS:
	.ascii "SYS_SLOADSYS_SSTORESYS_SHA3SYS_KECCAK256SYS_MLDSA_VERIFYSYS_MLKEM_DECAPSSYS_CALLERSYS_ADDRESSSYS_CALLVALUESYS_CALLDATASYS_CALLDATASIZESYS_BLOCKNUMBERSYS_TIMESTAMPSYS_GASPRICESYS_GASREMAININGSYS_BALANCESYS_SELFBALANCESYS_CALLSYS_STATICCALLSYS_DELEGATECALLSYS_CREATESYS_CREATE2SYS_RETURNSYS_REVERTSYS_STOPSYS_SELFDESTRUCTSYS_LOG0SYS_LOG1SYS_LOG2SYS_LOG3SYS_LOG4"
	.globl _aegis_sandbox__empty_return_data
	.zerofill __DATA,__common,_aegis_sandbox__empty_return_data,24584,2
	.globl _aegis_sandbox_E
	.data
	.align	1
_aegis_sandbox_E:
	.space 2
	.const
	.align	3
lC7:
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
	.quad	LFB6-.
	.set L$set$10,LFE6-LFB6
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
	.quad	LFB95-.
	.set L$set$18,LFE95-LFB95
	.quad L$set$18
	.uleb128 0
	.align	3
LEFDE17:
LSFDE19:
	.set L$set$19,LEFDE19-LASFDE19
	.long L$set$19
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB97-.
	.set L$set$20,LFE97-LFB97
	.quad L$set$20
	.uleb128 0
	.align	3
LEFDE19:
LSFDE21:
	.set L$set$21,LEFDE21-LASFDE21
	.long L$set$21
LASFDE21:
	.long	LASFDE21-EH_frame1
	.quad	LFB99-.
	.set L$set$22,LFE99-LFB99
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
	.quad	LFB101-.
	.set L$set$26,LFE101-LFB101
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
	.quad	LFB103-.
	.set L$set$32,LFE103-LFB103
	.quad L$set$32
	.uleb128 0
	.align	3
LEFDE31:
LSFDE33:
	.set L$set$33,LEFDE33-LASFDE33
	.long L$set$33
LASFDE33:
	.long	LASFDE33-EH_frame1
	.quad	LFB18-.
	.set L$set$34,LFE18-LFB18
	.quad L$set$34
	.uleb128 0
	.align	3
LEFDE33:
LSFDE35:
	.set L$set$35,LEFDE35-LASFDE35
	.long L$set$35
LASFDE35:
	.long	LASFDE35-EH_frame1
	.quad	LFB105-.
	.set L$set$36,LFE105-LFB105
	.quad L$set$36
	.uleb128 0
	.align	3
LEFDE35:
LSFDE37:
	.set L$set$37,LEFDE37-LASFDE37
	.long L$set$37
LASFDE37:
	.long	LASFDE37-EH_frame1
	.quad	LFB20-.
	.set L$set$38,LFE20-LFB20
	.quad L$set$38
	.uleb128 0
	.byte	0x4
	.set L$set$39,LCFI0-LFB20
	.long L$set$39
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$40,LCFI1-LCFI0
	.long L$set$40
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE37:
LSFDE39:
	.set L$set$41,LEFDE39-LASFDE39
	.long L$set$41
LASFDE39:
	.long	LASFDE39-EH_frame1
	.quad	LFB22-.
	.set L$set$42,LFE22-LFB22
	.quad L$set$42
	.uleb128 0
	.byte	0x4
	.set L$set$43,LCFI2-LFB22
	.long L$set$43
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$44,LCFI3-LCFI2
	.long L$set$44
	.byte	0xd
	.uleb128 0x1d
	.align	3
LEFDE39:
LSFDE41:
	.set L$set$45,LEFDE41-LASFDE41
	.long L$set$45
LASFDE41:
	.long	LASFDE41-EH_frame1
	.quad	LFB23-.
	.set L$set$46,LFE23-LFB23
	.quad L$set$46
	.uleb128 0
	.byte	0x4
	.set L$set$47,LCFI4-LFB23
	.long L$set$47
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$48,LCFI5-LCFI4
	.long L$set$48
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$49,LCFI6-LCFI5
	.long L$set$49
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$50,LCFI7-LCFI6
	.long L$set$50
	.byte	0xb
	.align	3
LEFDE41:
LSFDE43:
	.set L$set$51,LEFDE43-LASFDE43
	.long L$set$51
LASFDE43:
	.long	LASFDE43-EH_frame1
	.quad	LFB24-.
	.set L$set$52,LFE24-LFB24
	.quad L$set$52
	.uleb128 0
	.byte	0x4
	.set L$set$53,LCFI8-LFB24
	.long L$set$53
	.byte	0xe
	.uleb128 0x30
	.byte	0x9d
	.uleb128 0x6
	.byte	0x9e
	.uleb128 0x5
	.byte	0x4
	.set L$set$54,LCFI9-LCFI8
	.long L$set$54
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$55,LCFI10-LCFI9
	.long L$set$55
	.byte	0x93
	.uleb128 0x4
	.byte	0x94
	.uleb128 0x3
	.byte	0x4
	.set L$set$56,LCFI11-LCFI10
	.long L$set$56
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$57,LCFI12-LCFI11
	.long L$set$57
	.byte	0xb
	.byte	0x4
	.set L$set$58,LCFI13-LCFI12
	.long L$set$58
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xd3
	.byte	0xd4
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$59,LCFI14-LCFI13
	.long L$set$59
	.byte	0xb
	.align	3
LEFDE43:
LSFDE45:
	.set L$set$60,LEFDE45-LASFDE45
	.long L$set$60
LASFDE45:
	.long	LASFDE45-EH_frame1
	.quad	LFB25-.
	.set L$set$61,LFE25-LFB25
	.quad L$set$61
	.uleb128 0
	.byte	0x4
	.set L$set$62,LCFI15-LFB25
	.long L$set$62
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$63,LCFI16-LCFI15
	.long L$set$63
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$64,LCFI17-LCFI16
	.long L$set$64
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$65,LCFI18-LCFI17
	.long L$set$65
	.byte	0xb
	.byte	0x4
	.set L$set$66,LCFI19-LCFI18
	.long L$set$66
	.byte	0xa
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.byte	0x4
	.set L$set$67,LCFI20-LCFI19
	.long L$set$67
	.byte	0xb
	.align	3
LEFDE45:
LSFDE47:
	.set L$set$68,LEFDE47-LASFDE47
	.long L$set$68
LASFDE47:
	.long	LASFDE47-EH_frame1
	.quad	LFB1-.
	.set L$set$69,LFE1-LFB1
	.quad L$set$69
	.uleb128 0
	.byte	0x4
	.set L$set$70,LCFI21-LFB1
	.long L$set$70
	.byte	0xe
	.uleb128 0x10
	.byte	0x9d
	.uleb128 0x2
	.byte	0x9e
	.uleb128 0x1
	.byte	0x4
	.set L$set$71,LCFI22-LCFI21
	.long L$set$71
	.byte	0xd
	.uleb128 0x1d
	.byte	0x4
	.set L$set$72,LCFI23-LCFI22
	.long L$set$72
	.byte	0xde
	.byte	0xdd
	.byte	0xc
	.uleb128 0x1f
	.uleb128 0
	.align	3
LEFDE47:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
