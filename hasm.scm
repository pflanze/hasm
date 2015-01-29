;; XX lib
(def. (symbol.ref v i)
  (string-ref (symbol->string v) i))

(def (skip-false l)
     (drop-while false? l))

(TEST
 > (skip-false '(#f #f #t))
 (#t)
 > (skip-false '(#f #f 5 #t))
 (5 #t))

(def (skip-false-from-end l)
     (reverse (skip-false (reverse l))))

(TEST
 > (skip-false-from-end '(#f #f 5 #t #f #f))
 (#f #f 5 #t))

;; ==============================================================
;; Intel 32-bit assembly syntax (intel style)

;; === 32 vs. 64 bit
;; But still allow it to be assembled as 64-bit binaries, too?:

(def asm-bits-system
     (string.number
	  (xbacktick
	   "bash" "-c"
	   "file `which perl`|perl -wne 'm/ELF (\\d+)-bit/ or die; print $1,\"\\n\"'")))

(defparameter asm-bits asm-bits-system)

(def (asm-wordsize-bytes)
     (xcase (asm-bits) ((32) 4) ((64) 8)))

(def (asm-mode-64?)
     (xcase (asm-bits) ((32) #f) ((64) #t)))

(def (asm-mode-system?)
     (= (asm-bits) asm-bits-system))

(def (word? v)
     ((xcase (asm-bits) ((32) uint32?) ((64) uint64?)) v))

(def (sword? v)
     ((xcase (asm-bits) ((32) int32?) ((64) int64?)) v))

;; === Labels

;; a gensym for labels

(def *asm-label-seqno* 0)

(def (init-asm-label-seqno!)
     (set! *asm-label-seqno* 0))

(def (genlabel name)
     (let* ((seqno (inc! *asm-label-seqno*))
	    ;; neither ":" nor "-" are allowed in labels
	    (sym (source.symbol-append "G" (.string seqno) "_" name)))
       sym))

(defmacro (with-asm-genlabel var . body)
  `(let ((,var (genlabel ',var)))
     ,@body))

(defmacro (with-asm-genlabels vars . body)
  (fold-right (lambda (var tail)
		`(with-asm-genlabel ,var ,tail))
	      `(begin ,@body)
	      (source-code vars)))

;; === Registers

(defenum reg64
  rax rcx rdx rbx rsp rbp rsi rdi
  r0  r1  r2  r3  r4  r5  r6  r7
  ;; ---
  r8 r9 r10 r11 r12 r13 r14 r15
  xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7)

(defenum reg32
  ;; acc counter data base
  eax ecx edx ebx
  ;; sp sbp source dest
  esp ebp  esi edi
  ;; http://stackoverflow.com/questions/1753602/registers-for-x86-64-processors
  ;; x64:
  r8d r9d r10d r11d r12d r13d r14d r15d
  ;; these, too?
  r0d r1d r2d r3d r4d r5d r6d r7d)

(defenum reg16
  ax cx dx bx sp bp si di
  ;; http://stackoverflow.com/questions/1753602/registers-for-x86-64-processors
  ;; x64:
  r8w r9w r10w r11w r12w r13w r14w r15w
  ;; these, too?
  r0w r1w r2w r3w r4w r5w r6w r7w)

(defenum reg8
  ah al  ch cl  dh dl  bh bl
  ;; http://stackoverflow.com/questions/1753602/registers-for-x86-64-processors
  ;; x64 only? ?
  sil dil bpl spl
  ;; of course x64 only:
  r8b r9b r10b r11b r12b r13b r14b r15b
  ;; these, too?
  r0b r1b r2b r3b r4b r5b r6b r7b
  ;; XXX: 'h' versions, too??
  )

(def reg?
     (either reg64? reg32? reg16? reg8?))

;; width of storage in bytes
(def. (reg8.asm-width v) 1)
(def. (reg16.asm-width v) 2)
(def. (reg32.asm-width v) 4)
;;(def. (reg64.asm-width v) 8)

;; labels as addresses, XX always ok?
(def. (symbol.asm-width v) (asm-wordsize-bytes))
;; and generic numbers passed as arguments
(def. (integer.asm-width v) (asm-wordsize-bytes))
;; and indirect references, also when passed as arguments, right?
(def. (asm-ref.asm-width v) (asm-wordsize-bytes))

(class asm-labelreference (struct #(symbol? name))
       (method (asm-string v)
	       (string-append "OFFSET FLAT:"
			      (.string (.name v)))))

(class asm-ref (struct #(reg? source)
		       #!optional
		       #((maybe integer?) offset))
       (method (asm-string v)
	       (string-append "["
			      (.string (.source v))
			      (cond ((.offset v)
				     => (lambda (off)
					  (string-append
					   (if (negative? off)
					       ""
					       "+")
					   (.string off))))
				    (else
				     ""))
			      "]")))

(def asm-label? symbol?) ;; well, versus keyword as definition and
			 ;; symbol for reference, but first-class
			 ;; reference is |asm-labelreference|


(class asm-comment (struct #(string? value))
       (method (asm-print v)
	       (for-each (lambda (line)
			   (print "\t# " line "\n"))
			 (string-split (.value v) #\newline))))

(insert-result-of
 (def asm-ops
      `( ;; pseudo-ops
	(.section #(symbol? name))
	(.string #(string? value))
	(.text)
	(.globl #(symbol? name))
	;; ops
	(push #((either reg?
			asm-labelreference?
			word?
			sword?
			asm-ref? ;; XX does it do that? usage was my error
			) source))
	(pop #(reg? target))
	(mov target source)
	(sub target source)
	(add target source)
	(mul target) ;; odd instruction, yes.
	(imul target #!optional source aux)
	(div target source)
	(call jumptarget)
	(leave)
	;; http://pdos.csail.mit.edu/6.828/2004/readings/i386/s03_05.htm
	;; "RET instructions may optionally specify an immediate
	;; operand. By adding this constant to the new top-of-stack
	;; pointer, RET effectively removes any arguments that the
	;; calling program pushed on the stack before the execution of
	;; the CALL instruction."
	(ret #!optional #((maybe integer?) dropbytes))

	(jmp #((either asm-label? natural0?) jumptarget))
	(int #(natural0? number)) ;; XX stricter range
	(int3)
	(dec #(reg? target)) ;; or more..
	(inc #(reg? target))
	(xor #(reg? target) source)

	(cmp source1 source2)
	(test source1 source2)

	;; http://pdos.csail.mit.edu/6.828/2004/readings/i386/s03_05.htm
	;; 80386 Programmer's Reference Manual -- Section 3.5.html

	;; Unsigned Conditional Transfers
	(ja #(asm-label? jumptarget)) (jnbe #(asm-label? jumptarget))
	(jae #(asm-label? jumptarget)) (jnb #(asm-label? jumptarget))
	(jb #(asm-label? jumptarget)) (jnae #(asm-label? jumptarget))
	(jbe #(asm-label? jumptarget)) (jna #(asm-label? jumptarget))
	
	(jc #(asm-label? jumptarget))
	(jnc #(asm-label? jumptarget))
	;; ZF
	(je #(asm-label? jumptarget)) (jz #(asm-label? jumptarget))
	(jne #(asm-label? jumptarget)) (jnz #(asm-label? jumptarget))

	(jnp #(asm-label? jumptarget)) (jpo #(asm-label? jumptarget))
	(jp #(asm-label? jumptarget)) (jpe #(asm-label? jumptarget))

	;; Signed Conditional Transfers
	;; TODO
	))
 `(class asm-op
	 (def (asm-op-name-substring str)
	      (substring str 4 (.length str)))
	  
	 (method (asm-print v)
		 (let-pair ((name vals) (vector.list v))
			   (print "\t"
				  (asm-op-name-substring (symbol.string name)))
			   (let ((vals* (skip-false-from-end vals)))
			     (if (pair? vals*)
				 (print " "
					(list-join (map .asm-string vals*)
						   ", "))))
			   (print "\n")))
	 ,@(map (lambda (op)
		  (let-pair ((name fields) op)
			    `(subclass ,(source.symbol-append
					 "asm-" name)
				       (struct ,@fields))))
		asm-ops)))

(TEST
 > (with-output-to-string (& (.asm-print (asm-add ebx 4))))
 "\tadd ebx, 4\n")


(compile-time ;;XX hm
 (define-if-not-defined *asm-macros* '())

 (def (set-asm-macro! name expander)
      (push! *asm-macros* (cons name expander)))

 (defmacro (def-asm-macro name+args . body)
   (assert* pair? name+args
	    (lambda (name+args)
	      (let-pair ((name args) name+args)
			(assert* symbol? name
				 (lambda (name)
				   (set-asm-macro!
				    name
				    (eval `(lambda ,(cons `CTX args)
					     ,@body))))))))))

(def (asm-macro-expand CTX code)
     ;; expand until no expansion left, but don't expand
     ;; subforms. Returns (values CTX* code*), where CTX* should be
     ;; used to expand code*'s subforms.
     (if (pair? code)
	 (let-pair
	  ((name args) code)
	  (cond ((assq name CTX)
		 => (lambda-pair ((_name expander))
			    (letv ((CTX res)
				   ;; XX forever errors
				   (apply expander CTX args))
				  (asm-macro-expand CTX res))))
		(else
		 ;; do not expand args here
		 (values CTX code))))
	 (values CTX code)))

(def (asm-macro-expand-all CTX code)
     ;; expand until no expansion left, and does expand
     ;; subforms. Returns the expanded code.
     (letv ((CTX* code) (asm-macro-expand CTX code))
	   (if (pair? code)
	       (improper-map
		(C asm-macro-expand-all CTX* _)
		code)
	       code)))


;; === ASM macros

;; evaluate Scheme code
(def-asm-macro (eval scheme-expr)
  (values CTX (eval scheme-expr)))


(def amd64-abi-argument-registers
     '(rdi  rsi  rdx  rcx  r8   r9
       xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7))


(def (goto-invoke-expand CTX cfunction args tail-call?)
     (let ((args (map (C asm-macro-expand-all CTX _) args)))
       (values
	CTX
	(if (asm-mode-64?)
	    `(begin
	       ,@(F (stream-map (lambda (arg reg)
				  ;; XXX: check/reorder not to overwrite regs!
				  `(mov ,reg ,arg))
				args
				amd64-abi-argument-registers))
	       (,(if tail-call? `jmp `call) ,cfunction))
	    (let ((allocsize (sum (map .asm-width args))))
	      `(begin
		 ,@(map (lambda (arg)
			  `(push ,arg))
			(reverse args))
		 ,(if tail-call?
		      `(begin
			 ;; push fake return address; well ugh; that's
			 ;; part of my unfinished TCO ABI...
			 (push 0)
			 (jmp ,cfunction)
			 ;; XXX trust that jumped-to function will
			 ;; drop stack arguments: not C ABI conform!
			 )
		      `(begin
			 (call ,cfunction)
			 (add esp ,allocsize)))))))))

(def-asm-macro (invoke cfunction . args)
  (goto-invoke-expand CTX cfunction args #f))

;; "tail-call", or continuation-dropping, version of invoke
(def-asm-macro (goto cfunction . args)
  (goto-invoke-expand CTX cfunction args #t))

(def-asm-macro (arg n)
  (values
   CTX
   (if (asm-mode-64?)
       (list-ref amd64-abi-argument-registers n)
       `(ref esp (words.bytes ,(inc n))))))


;; Saving program arguments: On x86, they are already on the stack,
;; thus no saving needed; on AMD64, write them to the 'red area' of
;; the stack (i.e. the area after the stack pointer that's reserved
;; for temporary storage, not updating the stack pointer), but we need
;; to shadow |call| (and |goto|, XXX todo), so that the SP will be
;; updated for sub-calls.

;; Introduces a local (lexical) `(savedarg n) where n is the same n as
;; in `(saveargs (n...) ..) or `(arg n).

;; Also introduces aliases for all given names in `(saveargs ((n
;; name)...)  ..).

;; XX todo: allow access to saved args of outer saveargs scope (allow
;; useful nesting of |saveargs|)

(def-asm-macro (saveargs argnumber+names . body)
  (def (argnumber v)
       (mcase v
	      (`(`argnumber `name) argnumber)
	      (else v)))
  (def (maybe-name v)
       (mcase v
	      (`(`argnumber `name) name)
	      (else #f)))
  (let* ((argnumber->savedpos
	  (let ((m (map/iota (lambda (argnumber+name savedpos)
			       (cons (argnumber argnumber+name) savedpos))
			     argnumber+names)))
	    (C number-alist-ref m _)))
	 (let-alias-with-body
	  `(let-alias ,(filter
			identity
			(map (lambda (argnumber+name)
			       (cond ((maybe-name argnumber+name)
				      => (lambda (name)
					   `(,name (savedarg
						    ,(argnumber argnumber+name)))))
				     (else #f)))
			     argnumber+names))
		      ,@body)))
    (if (asm-mode-64?)
	(let* ((numargs (length (source-code argnumber+names)))
	       (prev-num-savedargs (cond ((assq '$num-savedargs CTX) => cdr) (else 0)))
	       (num-savedargs (+ prev-num-savedargs numargs)))
	  (values
	   (cons*
	    (cons '$num-savedargs num-savedargs)
	    (cons 'savedarg
		  (lambda (CTX argnumber)
		    (values CTX
			    `(ref rsp (words.bytes
				       ,(- (+ 1
					      (argnumber->savedpos argnumber)
					      prev-num-savedargs)))))))
	    (cons 'call
		  (lambda (CTX . args)
		    (values
		     CTX
		     `(begin
			;; nice, this kind of comment can actually be
			;; output by macros; ok, macros that output
			;; "text" could do the same
			"(update SP to reflect values saved to the red area)"
			(sub rsp ,(* 8 num-savedargs))
			;; output object, not syntax, to avoid expansion
			;; loop
			,(apply asm-call args)
			(add rsp ,(* 8 num-savedargs))))))
	    CTX)
	   `(begin
	      ,@(map/iota (lambda (argnumber+name savedpos)
			    `(mov (ref rsp (words.bytes ,(- (inc savedpos))))
				  ,(list-ref amd64-abi-argument-registers
					     (argnumber argnumber+name))))
			  argnumber+names)
	      ,let-alias-with-body)))
	(values
	 (cons (cons 'savedarg
		     (lambda (CTX argnumber)
		       (values CTX
			       `(ref esp (words.bytes
					  ,(inc (argnumber->savedpos argnumber)))))))
	       CTX)
	 let-alias-with-body))))


(def-asm-macro (size_t reg)
  (values
   CTX
   (assert* reg? reg
	    (lambda (reg)
	      (symbol-append (if (asm-mode-64?) 'r 'e) reg)))))
;; and XX alias, make simpler
(def-asm-macro (word reg)
  (values
   CTX
   (assert* reg? reg
	    (lambda (reg)
	      (symbol-append (if (asm-mode-64?) 'r 'e) reg)))))
;; ah and shorter:
(def-asm-macro (A)
  (values CTX
	  (if (asm-mode-64?) 'rax 'eax)))

(def-asm-macro (B) (values CTX (if (asm-mode-64?) 'rbx 'ebx)))
(def-asm-macro (C) (values CTX (if (asm-mode-64?) 'rcx 'ecx)))
(def-asm-macro (D) (values CTX (if (asm-mode-64?) 'rdx 'edx)))

(def-asm-macro (words.bytes n)
  (values
   CTX
   (* n (asm-wordsize-bytes))))


(def-asm-macro (AMD64 . body)
  (values
   CTX
   (if (asm-mode-64?)
       `(begin ,@body)
       `(begin))))


(def asm-branch
     ;; comparison to branch instruction, and its negation
     '((> ja <=)
       (>= jae <)
       (< jb >=)
       (<= jbe >)
       (c jc !c)
       (= je !=)
       (z jz !z)
       (!c jnc c)
       (!= jne =)
       (!z jnz z)
       ;; todo jnp jpo, jp jpe
       ))

(def jump-ops
     (append (map cadr asm-branch) '(call jmp)))

(def (asm-branch-for tst negate?)
     (cond ((assq tst asm-branch)
	    => (applying (lambda (_tst opname negated-tst)
			   (if negate?
			       (asm-branch-for negated-tst #f)
			       opname))))
	   (error "unknown branch test:" tst)))

(def (asm-macro:if-expand tst unlikely? yes maybe-no)
     (if maybe-no
	 ;; two branches
	 (let ((no maybe-no))
	   (if unlikely?
	       (with-asm-genlabels
		(then cont)
		`(begin
		   (,(asm-branch-for tst #f) ,then)
		   ,no
		   (jmp ,cont)
		   ,(.keyword then)
		   ,yes
		   ,(.keyword cont)))

	       (with-asm-genlabels
		(else cont)
		`(begin
		   (,(asm-branch-for tst #t) ,else)
		   ,yes
		   (jmp ,cont)
		   ,(.keyword else)
		   ,no
		   ,(.keyword cont)))))

	 ;; one branch
	 (with-asm-genlabels
	  (cont)
	  `(begin
	     (,(asm-branch-for tst #t) ,cont)
	     ,yes
	     ,(.keyword cont)))))


(def-asm-macro (if tst yes #!optional no)
  ;; tst can be wrapped in a |likely| or |unlikely| form
  (values
   CTX
   (mcase tst
	  (`(likely `e)
	   (asm-macro:if-expand e #f yes no))
	  (`(unlikely `e)
	   (asm-macro:if-expand e #t yes no))
	  (else
	   (asm-macro:if-expand tst #f yes no)))))


(def-asm-macro (ref source #!optional offset)
  ;; return first-class value, OK??? wow why does that work?
  (values CTX
	  (asm-ref (asm-macro-expand-all CTX source)
		   (asm-macro-expand-all CTX offset))))

;; = aliases =

(def-asm-macro (let-alias binds . body)
  ;; create macros for all the bindings that return the syntax that
  ;; they specify
  (let ((CTX* (fold-right (lambda (bind CTX)
			    (mcase bind
				   (`(`name `code)
				    (cons (cons name (lambda (CTX)
						       (values CTX code)))
					  CTX))))
			  CTX
			  binds)))
    (values CTX* `(begin ,@body))))


;; = debugging =

(def-asm-macro (step)
  (values
   CTX
   ;; XX something better?
   `(begin "enter the debugger"
	   ;; <vulture-> technically you should be using int3 not int
	   ;; 3.  the main difference being int3 is always the single
	   ;; byte CC and some assemblers may assemble "int 3" as
	   ;; either CC or CD 03 where, for compatibility, it's best
	   ;; to be CC
	   (int3))))



;; === the assembler main functions & procedure =============================

(def (asm-begin? form)
     (and (pair? form)
	  (eq? (car form) 'begin)))

(def (asm CTX form tail)
     (letv ((CTX* form) (asm-macro-expand CTX form))
	   (cond
	    ((asm-op? form)
	     ;; used by macros to output already-expanded bare code to
	     ;; avoid expansion loops
	     (cons form tail))
	    ((keyword? form)
	     (cons form tail))
	    ((asm-begin? form)
	     (fold-right (C asm CTX* _ _) tail (cdr form)))
	    ((pair? form)
	     (let ((form (map (C asm-macro-expand-all CTX* _) form)))
	       (let-pair
		((name args) form)
		;; objectify:
		;; evil eval?
		(let ((op (eval (source.symbol-append
				 'asm-
				 name)))
		      (args* (map (lambda (arg)
				    (cond ((reg? arg)
					   arg)
					  ((symbol? arg)
					   ;; if it's an instruction
					   ;; then it's a label. But,
					   ;; don't take addresses
					   ;; from labels as
					   ;; first-class values if
					   ;; it's for a jump
					   ;; instruction.
					   (if (or (eq? (symbol.ref name 0) #\.)
						   (memq name jump-ops))
					       arg
					       (asm-labelreference arg)))
					  (else
					   ;; XX always ok?
					   arg)))
				  args)))
		  (cons (apply op args*)
			tail)))))
	    ((string? form)
	     (cons (asm-comment form) tail))
	    (else
	     (error "not a pair, label or comment string:" form)))))

(def. (keyword.asm-print v)
  (println v))

(def. (pair.asm-print v)
  (.asm-print (car v))
  (.asm-print (cdr v)))

(def. number.asm-string
  object->string)

(def. string.asm-string
  object->string)

(def. symbol.asm-string
  symbol.string)


(def (asm-read-all p)
     (cons `begin (read-all p)))

(def (assemble from-path
	       #!key
	       (bits (asm-bits))
	       (to (string-append (strip-suffix from-path) ".S")))
     (parameterize
      ((asm-bits bits))
      (init-asm-label-seqno!) ;; XXX beware of multi-file compilations!
      (with-output-to-file
	  to
	(&
	 (let* ((forms (call-with-input-file from-path asm-read-all))
		(as (asm *asm-macros* forms '())))
	   (display "\t.intel_syntax noprefix\n")
	   (for-each .asm-print as))))))

(def (assemble* path
		#!key
		(bits (asm-bits))
		(to (string-append (strip-suffix path) ".S")))
     (parameterize
      ((asm-bits bits))
      (assemble path
		bits: bits
		to: to)
      (let ((to* (strip-suffix to)))
	(apply xsystem `("gcc"
			 ,@(if (asm-mode-system?)
			       (list)
			       (list (string-append "-m" (.string (asm-bits)))))
			 ,(string-append to* ".S")
			 "-o" ,to*)))))

(def (asrun path #!key (bits (asm-bits)))
     (assemble* path bits: bits)
     (xsystem (path-expand (strip-suffix path))))

(def (assemble-suite bits)
     (def (path base)
	  (string-append base (.string bits) ".S"))
     (assemble* "hello.scm"
		to: (path "hello")
		bits: bits)
     (assemble* "fact.scm"
		to: (path "fact")
		bits: bits))

;; (assemble-suite 32)
;; (assemble-suite 64)
