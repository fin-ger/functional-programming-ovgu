	.globl	kacke
	.type	kacke, @function
kacke:
        pushq   %rbp
        movq    %rsp, %rbp
	movl	$1, %eax
        popq    %rbp
	ret

