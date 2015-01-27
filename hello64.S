	.intel_syntax noprefix
	.section .rodata
.hello:
	.string "Hello"
.world:
	.string "World!\n"
.write_error:
	.string "error on write"
	.text
	.globl main
main:
	# Output strings in various ways, using an invoke macro that
	#  works on Linux both on x86 and x86-64
	mov rdi, OFFSET FLAT:.hello
	call strlen
	mov rdi, 1
	mov rsi, OFFSET FLAT:.hello
	mov rdx, rax
	call write
	# Check for errors
	cmp rax, 0
	jae G1_cont
	mov rdi, OFFSET FLAT:.write_error
	call perror
	mov rax, 1
	ret
G1_cont:
	# Omit error checking now
	mov rdi, 32
	call putchar
	mov rdi, OFFSET FLAT:.world
	call puts
	mov rax, 0
	ret
