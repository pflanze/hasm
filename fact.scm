
(.section .rodata)

.hello: (.string "Hello")
.world: (.string "World!\n")

.show_i: (.string "show_i: %i\n")
.yougave: (.string "You gave %i argument(s), the first: '%s'\n")
.write_error: (.string "error on write")
.factis: (.string "fact(%u)=%u\n")

.overflow: (.string "arithmetic overflow")

(.text)
(.globl main)

.fact_iter:
"fact_iter(A=n,B=total)"
(cmp (A) 0)
(if (likely >)
    (begin
      "imul is 32-bit only, even with 64-bit arguments!"
      (imul (B) (A))
      (if (unlikely c)
	  (goto error .overflow)
	  (begin
	    (dec (A))
	    (jmp .fact_iter))))
    (begin
      (mov (A) (B))
      (ret)))

fact:
"fact(n)"
(mov (A) (arg 0))
(mov (B) 1)
(jmp .fact_iter)


main:
"Save program arguments: argc, argv"
(saveargs
 ((0 argc)
  (1 argv))

 "Output strings in various ways, using an invoke macro that
 works on Linux both on x86 and x86-64"

 (invoke strlen .hello)
 (invoke write 1 .hello (A))
 "Check for errors"
 (cmp (size_t ax) 0)
 (if <
     (begin
       (invoke perror .write_error)
       (mov (A) 1)
       (ret)))

 "Omit error checking now"
 (invoke putchar (eval (.integer #\space)))
 (invoke puts .world)


 "Use the program arguments"
 (begin

   (mov (C) (argc))
   (mov (D) (argv))

   "exclude program name from number of arguments"
   (dec (C))

   (AMD64 ;; HACK, make part of invoke somehow
    "abi-0.99.pdf page 20: %al is used as hidden argument to specify
the number of vector registers used"
    (mov al 0))
   (invoke printf .yougave (C) (ref (D) (words.bytes 1)))

   (mov (C) (argc))

   (cmp (C) 1)
   (if >
       (begin
	 (mov (D) (argv))
	 (invoke atol (ref (D) (words.bytes 1)))
	 (push (A))
	 (mov (B) (A))
	 (AMD64 (mov al 0))
	 (invoke printf .show_i (B))
	 "C=n"
	 (pop (C))
	 (push (C))
	 (invoke fact (C))
	 "B=fact(n)"
	 (mov (B) (A))
	 (pop (C))
	 (AMD64 (mov al 0))
	 (invoke printf .factis (C) (B)))))

 (mov (A) 0)
 (ret))



error:
(invoke puts (arg 0))
(invoke _exit 1)
