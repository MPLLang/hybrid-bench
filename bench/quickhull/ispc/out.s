	.text
	.file	"max_dist_pt.ispc"
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function max_dist_pt_8___un_3C_und_3E_un_3C_uni_3E_unIundundundund
.LCPI0_0:
	.long	8                               # 0x8
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3, 0x0
.LCPI0_1:
	.quad	0xfff0000000000000              # double -Inf
	.text
	.globl	max_dist_pt_8___un_3C_und_3E_un_3C_uni_3E_unIundundundund
	.p2align	4, 0x90
	.type	max_dist_pt_8___un_3C_und_3E_un_3C_uni_3E_unIundundundund,@function
max_dist_pt_8___un_3C_und_3E_un_3C_uni_3E_unIundundundund: # @max_dist_pt_8___un_3C_und_3E_un_3C_uni_3E_unIundundundund
# %bb.0:
	shll	$2, %edx
	movslq	%edx, %rax
	vpslld	$4, (%rsi,%rax), %ymm5
	vxorpd	%xmm6, %xmm6, %xmm6
	kxnorw	%k0, %k0, %k1
	vgatherdpd	(%rdi,%ymm5), %zmm6 {%k1}
	vpsllw	$7, %xmm4, %xmm4
	vpmovb2m	%xmm4, %k1
	kxnorw	%k0, %k0, %k2
	vpord	.LCPI0_0(%rip){1to8}, %ymm5, %ymm4
	vxorpd	%xmm5, %xmm5, %xmm5
	vgatherdpd	(%rdi,%ymm4), %zmm5 {%k2}
	vbroadcastsd	%xmm0, %zmm4
	vbroadcastsd	%xmm1, %zmm7
	vsubpd	%zmm4, %zmm6, %zmm4
	vsubpd	%zmm7, %zmm5, %zmm5
	vsubsd	%xmm0, %xmm2, %xmm0
	vbroadcastsd	%xmm0, %zmm0
	vsubsd	%xmm1, %xmm3, %xmm1
	vbroadcastsd	%xmm1, %zmm1
	vmulpd	%zmm4, %zmm1, %zmm1
	vfmsub231pd	%zmm0, %zmm5, %zmm1     # zmm1 = (zmm5 * zmm0) - zmm1
	vbroadcastsd	.LCPI0_1(%rip), %zmm0   # zmm0 = [-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf,-Inf]
	vblendmpd	%zmm1, %zmm0, %zmm2 {%k1}
	kshiftrw	$8, %k1, %k0
	knotb	%k0, %k1
	vmovapd	%zmm0, %zmm0 {%k1} {z}
	vmaxpd	%zmm2, %zmm0, %zmm0
	vextractf64x4	$1, %zmm0, %ymm2
	vmaxpd	%zmm0, %zmm2, %zmm0
	vextractf128	$1, %ymm0, %xmm2
	vmaxpd	%xmm0, %xmm2, %xmm0
	vshufpd	$1, %xmm0, %xmm0, %xmm2         # xmm2 = xmm0[1,0]
	vmaxsd	%xmm0, %xmm2, %xmm0
	vbroadcastsd	%xmm0, %zmm0
	vcmpeqpd	%zmm0, %zmm1, %k0
	kortestw	%k0, %k0
	kmovw	%k0, %ecx
	movl	$-1, %eax
	cmovnel	%ecx, %eax
	vzeroupper
	retq
.Lfunc_end0:
	.size	max_dist_pt_8___un_3C_und_3E_un_3C_uni_3E_unIundundundund, .Lfunc_end0-max_dist_pt_8___un_3C_und_3E_un_3C_uni_3E_unIundundundund
                                        # -- End function
	.section	.rodata.cst4,"aM",@progbits,4
	.p2align	2, 0x0                          # -- Begin function max_dist_pt_8
.LCPI1_0:
	.long	8                               # 0x8
	.text
	.globl	max_dist_pt_8
	.p2align	4, 0x90
	.type	max_dist_pt_8,@function
max_dist_pt_8:                          # @max_dist_pt_8
# %bb.0:
	shll	$2, %edx
	movslq	%edx, %rax
	vpslld	$4, (%rsi,%rax), %ymm4
	kxnorw	%k0, %k0, %k1
	vxorpd	%xmm5, %xmm5, %xmm5
	vgatherdpd	(%rdi,%ymm4), %zmm5 {%k1}
	kxnorw	%k0, %k0, %k1
	vpord	.LCPI1_0(%rip){1to8}, %ymm4, %ymm4
	vxorpd	%xmm6, %xmm6, %xmm6
	vgatherdpd	(%rdi,%ymm4), %zmm6 {%k1}
	vbroadcastsd	%xmm0, %zmm4
	vbroadcastsd	%xmm1, %zmm7
	vxorpd	%xmm8, %xmm8, %xmm8
	vsubpd	%zmm4, %zmm5, %zmm4
	vsubpd	%zmm7, %zmm6, %zmm5
	vsubsd	%xmm0, %xmm2, %xmm0
	vbroadcastsd	%xmm0, %zmm0
	vsubsd	%xmm1, %xmm3, %xmm1
	vbroadcastsd	%xmm1, %zmm1
	vmulpd	%zmm4, %zmm1, %zmm1
	vfmsub231pd	%zmm0, %zmm5, %zmm1     # zmm1 = (zmm5 * zmm0) - zmm1
	vmaxpd	%zmm1, %zmm8, %zmm0
	vextractf64x4	$1, %zmm0, %ymm2
	vmaxpd	%zmm0, %zmm2, %zmm0
	vextractf128	$1, %ymm0, %xmm2
	vmaxpd	%xmm0, %xmm2, %xmm0
	vshufpd	$1, %xmm0, %xmm0, %xmm2         # xmm2 = xmm0[1,0]
	vmaxsd	%xmm0, %xmm2, %xmm0
	vbroadcastsd	%xmm0, %zmm0
	vcmpeqpd	%zmm0, %zmm1, %k0
	kortestw	%k0, %k0
	kmovw	%k0, %ecx
	movl	$-1, %eax
	cmovnel	%ecx, %eax
	vzeroupper
	retq
.Lfunc_end1:
	.size	max_dist_pt_8, .Lfunc_end1-max_dist_pt_8
                                        # -- End function
	.ident	"Intel(r) Implicit SPMD Program Compiler (Intel(r) ISPC), 1.24.0 (build commit d394222aef59e475 @ 20240523, LLVM 17.0.6)"
	.ident	"LLVM version 17.0.6 (/usr/local/src/ispc/build/llvm-source/clang 6009708b4367171ccdbf4b5905cb6a803753fe18)"
	.section	".note.GNU-stack","",@progbits
