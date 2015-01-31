	.intel_syntax noprefix
	.section .rodata
.hello:
	.string "Hello"
.world:
	.string "World!\n"
.show_i:
	.string "show_i: %i\n"
.yougave:
	.string "You gave %i argument(s), the first: '%s'\n"
.write_error:
	.string "error on write"
.factis:
	.string "fact(%u)=%u\n"
.overflow:
	.string "arithmetic overflow"
	.text
	.globl main
.fact_iter:
	# fact_iter(A=n,B=total)
	cmp eax, 0
	jbe G3_else
	# imul is 32-bit only, even with 64-bit arguments!
	imul ebx, eax
	jc G5_then
	dec eax
	jmp .fact_iter
	jmp G6_cont
G5_then:
	push OFFSET FLAT:.overflow
	push 0
	jmp error
G6_cont:
	jmp G4_cont
G3_else:
	mov eax, ebx
	ret
G4_cont:
fact:
	# fact(n)
	mov eax, [esp+4]
	mov ebx, 1
	jmp .fact_iter
main:
	# Save program arguments: argc, argv
	# Output strings in various ways, using an invoke macro that
	#  works on Linux both on x86 and x86-64
	push OFFSET FLAT:.hello
	call strlen
	add esp, 4
	push eax
	push OFFSET FLAT:.hello
	push 1
	call write
	add esp, 12
	# Check for errors
	cmp eax, 0
	jae G2_cont
	push OFFSET FLAT:.write_error
	call perror
	add esp, 4
	mov eax, 1
	ret
G2_cont:
	# Omit error checking now
	push 32
	call putchar
	add esp, 4
	push OFFSET FLAT:.world
	call puts
	add esp, 4
	# Use the program arguments
	mov ecx, [esp+4]
	mov edx, [esp+8]
	# exclude program name from number of arguments
	dec ecx
	push [edx+4]
	push ecx
	push OFFSET FLAT:.yougave
	call printf
	add esp, 12
	mov ecx, [esp+4]
	cmp ecx, 1
	jbe G1_cont
	mov edx, [esp+8]
	push [edx+4]
	call atol
	add esp, 4
	push eax
	mov ebx, eax
	push ebx
	push OFFSET FLAT:.show_i
	call printf
	add esp, 8
	# C=n
	pop ecx
	push ecx
	push ecx
	call fact
	add esp, 4
	# B=fact(n)
	mov ebx, eax
	pop ecx
	push ebx
	push ecx
	push OFFSET FLAT:.factis
	call printf
	add esp, 12
G1_cont:
	mov eax, 0
	ret
error:
	push [esp+4]
	call puts
	add esp, 4
	push 1
	call _exit
	add esp, 4
