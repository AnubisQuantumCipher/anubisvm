	.arch armv8.4-a+fp16+sb+ssbs
	.build_version macos,  16, 0
	.text
	.globl _anubis_config__target_proof_level
	.const
	.align	2
_anubis_config__target_proof_level:
	.ascii "Gold"
	.globl _anubis_config_E
	.data
	.align	1
_anubis_config_E:
	.space 2
	.ident	"GCC: (GNU) 14.1.0"
	.subsections_via_symbols
