
(def (assemble-example path bits)
     (assemble* path
		to: (string-append (strip-suffix path) (.string bits) ".S")
		bits: bits))

(def (assemble-examples-bits bits)
     (assemble-example "examples/hello.scm" bits)
     (assemble-example "examples/fact.scm" bits))

(def (assemble-examples)
     (assemble-examples-bits 32)
     (assemble-examples-bits 64))

