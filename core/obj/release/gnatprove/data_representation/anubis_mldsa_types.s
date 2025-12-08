	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_types__TpolynomialBIP
_anubis_mldsa_types__TpolynomialBIP:
LFB1:
	ret
LFE1:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_types__Tpoly_vector_kBIP
_anubis_mldsa_types__Tpoly_vector_kBIP:
LFB2:
	ret
LFE2:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_types__Tpoly_vector_lBIP
_anubis_mldsa_types__Tpoly_vector_lBIP:
LFB3:
	ret
LFE3:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_types__Tpoly_matrixBIP
_anubis_mldsa_types__Tpoly_matrixBIP:
LFB4:
	ret
LFE4:
	.align	2
	.p2align 5,,15
	.globl _anubis_mldsa_types__seeds_recordIP
_anubis_mldsa_types__seeds_recordIP:
LFB5:
	ret
LFE5:
	.globl _anubis_mldsa_types_E
	.data
	.align	1
_anubis_mldsa_types_E:
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
LSFDE5:
	.set L$set$5,LEFDE5-LASFDE5
	.long L$set$5
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB3-.
	.set L$set$6,LFE3-LFB3
	.quad L$set$6
	.uleb128 0
	.align	3
LEFDE5:
LSFDE7:
	.set L$set$7,LEFDE7-LASFDE7
	.long L$set$7
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB4-.
	.set L$set$8,LFE4-LFB4
	.quad L$set$8
	.uleb128 0
	.align	3
LEFDE7:
LSFDE9:
	.set L$set$9,LEFDE9-LASFDE9
	.long L$set$9
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB5-.
	.set L$set$10,LFE5-LFB5
	.quad L$set$10
	.uleb128 0
	.align	3
LEFDE9:
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
