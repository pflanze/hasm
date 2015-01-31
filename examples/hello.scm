
(.section .rodata)

.hello: (.string "Hello")
.world: (.string "World!\n")
.write_error: (.string "error on write")

(.text)
(.globl main)

main:
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

(mov (A) 0)
(ret)
