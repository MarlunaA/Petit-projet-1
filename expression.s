	.text
	.globl	main
main:
	movsd .F1, %xmm0
	subq $8, %rsp
	movsd %xmm0, 0(%rsp)
	movsd 0(%rsp), %xmm0
	xorpd %xmm1, %xmm1
	subsd %xmm0, %xmm1
	movsd %xmm1, 0(%rsp)
	movsd .F0, %xmm0
	subq $8, %rsp
	movsd %xmm0, 0(%rsp)
	movsd 0(%rsp), %xmm0
	addq $8, %rsp
	movsd 0(%rsp), %xmm1
	addq $8, %rsp
	addsd %xmm1, %xmm0
	subq $8, %rsp
	movsd %xmm0, 0(%rsp)
	movsd 0(%rsp), %xmm0
	addq $8, %rsp
	call print_float
	ret

    print_float:
          movq $S_float, %rdi
          movq $1, %rax
          call printf
          ret
          	.data
S_float:
	.string "%f\n"
.F0:
  .double 3.
.F1:
  .double 5.
