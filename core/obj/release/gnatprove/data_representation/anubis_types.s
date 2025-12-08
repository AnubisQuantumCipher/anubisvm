	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_types__byte_arrayIP
_anubis_types__byte_arrayIP:
LFB1:
	ret
LFE1:
	.align	2
	.p2align 5,,15
	.globl _anubis_types__Tword256BIP
_anubis_types__Tword256BIP:
LFB2:
	ret
LFE2:
	.globl _anubis_types__zero_hash
	.const
_anubis_types__zero_hash:
	.space 32
	.globl _anubis_types__zero_word64
	.align	3
_anubis_types__zero_word64:
	.space 8
	.globl _anubis_types__zero_word32
	.align	2
_anubis_types__zero_word32:
	.space 4
	.globl _anubis_types__zero_byte
_anubis_types__zero_byte:
	.space 1
	.globl _anubis_types_E
	.data
	.align	1
_anubis_types_E:
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
	.quad	LFB1-.
	.set L$set$2,LFE1-LFB1
	.quad L$set$2
	.uleb128 0
	.align	3
LEFDE1:
LSFDE3:
	.set L$set$3,LEFDE3-LASFDE3
	.long L$set$3
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB2-.
	.set L$set$4,LFE2-LFB2
	.quad L$set$4
	.uleb128 0
	.align	3
LEFDE3:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
