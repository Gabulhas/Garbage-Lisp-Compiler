	.file	"runtime.c"
# GNU C17 (GCC) version 11.2.0 (x86_64-pc-linux-gnu)
#	compiled by GNU C version 11.2.0, GMP version 6.2.1, MPFR version 4.1.0-p13, MPC version 1.2.1, isl version isl-0.24-GMP

# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed: -mtune=generic -march=x86-64
	.text
	.section	.rodata
.LC0:
	.string	"%d"
	.text
	.globl	inputInt
	.type	inputInt, @function
inputInt:
.LFB0:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
# ./c_runtime_and_stdlib/src/runtime.c:3: int inputInt() {
	movq	%fs:40, %rax	# MEM[(<address-space-1> long unsigned int *)40B], tmp87
	movq	%rax, -8(%rbp)	# tmp87, D.2356
	xorl	%eax, %eax	# tmp87
# ./c_runtime_and_stdlib/src/runtime.c:5:     scanf("%d", &result);
	leaq	-12(%rbp), %rax	#, tmp84
	movq	%rax, %rsi	# tmp84,
	leaq	.LC0(%rip), %rax	#, tmp85
	movq	%rax, %rdi	# tmp85,
	movl	$0, %eax	#,
	call	__isoc99_scanf@PLT	#
# ./c_runtime_and_stdlib/src/runtime.c:6:     return result;
	movl	-12(%rbp), %eax	# result, _3
# ./c_runtime_and_stdlib/src/runtime.c:7: }
	movq	-8(%rbp), %rdx	# D.2356, tmp88
	subq	%fs:40, %rdx	# MEM[(<address-space-1> long unsigned int *)40B], tmp88
	je	.L3	#,
	call	__stack_chk_fail@PLT	#
.L3:
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE0:
	.size	inputInt, .-inputInt
	.section	.rodata
.LC1:
	.string	"%d\n"
	.text
	.globl	printInt
	.type	printInt, @function
printInt:
.LFB1:
	.cfi_startproc
	pushq	%rbp	#
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	movq	%rsp, %rbp	#,
	.cfi_def_cfa_register 6
	subq	$16, %rsp	#,
	movl	%edi, -4(%rbp)	# v, v
# ./c_runtime_and_stdlib/src/runtime.c:10:     printf("%d\n", v);
	movl	-4(%rbp), %eax	# v, tmp82
	movl	%eax, %esi	# tmp82,
	leaq	.LC1(%rip), %rax	#, tmp83
	movq	%rax, %rdi	# tmp83,
	movl	$0, %eax	#,
	call	printf@PLT	#
# ./c_runtime_and_stdlib/src/runtime.c:11: }
	nop	
	leave	
	.cfi_def_cfa 7, 8
	ret	
	.cfi_endproc
.LFE1:
	.size	printInt, .-printInt
	.ident	"GCC: (GNU) 11.2.0"
	.section	.note.GNU-stack,"",@progbits
