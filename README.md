# Hack Intel assembly with macros implemented in Scheme

This uses S-expressions to represent assembly language, and allows to
write macros in Scheme (macro expanders written in Scheme that output
assembly code).


## Features

* supports both x86 and x86_64 (AMD64), 

* uses Intel syntax 

* various useful macros?

* accessible guts :)


## Missing features

* support for other systems than Linux

* does not (yet) optimize away dead code or unused labels introduced by
  macros

* only some Intel instructions are supported yet (extend `asm-ops` in
  hasm.scm!)

* should be made more modular (split hasm.scm, allow macros as user
  library)


## Setup

### Install Scheme system

Get x86_64 binary tgz from [here](http://christianjaeger.ch/binaries/gambc-v4.5.3-x86_64.tgz).

    $ cd ~
    $ wget http://christianjaeger.ch/binaries/gambc-v4.5.3-x86_64.tgz
    $ sha256sum gambc-v4.5.3-x86_64.tgz
    3bdc1c213cc6c790d37bafb16eef38925e401850ea9938bd7d1b2e6f98354c3d
    $ tar xf gambc-v4.5.3-x86_64.tgz

This will unpack into `~/install/gambc/`. Add `~/install/gambc/bin/`
to your `PATH` environment variable.

Or compile Gambit-C from upstream (but that doesn't carry a patch to
suppress undefined symbol warnings):

    $ ./configure --enable-single-host --prefix="$HOME/install"
    $ make install

### Get libraries

This works for both x86_64 and x86 code. If you want to use
`assemble*` or `asrun` to compile to x86 binaries, you need to install
32-bit support libraries; e.g. on Debian:

    $ sudo apt-get install libc6-dev-i386 libc6-i386

Also, need to get the files in the submodule:

    $ git submodule update

### Run it

    $ gsc -:tE,dar,t8,f8,-8

On first run, this will take some time to compile the dependencies in
`lib`.

    > (assemble "foo.scm") ;; create foo.S
    > (assemble* "foo.scm") ;; create foo.S and compile that to foo
    > (asrun "foo.scm") ;; create foo.S, compile to foo, and run it

To compile to 32|64 bits instead of host system bit size:

    > (assemble* "foo.scm" bits: 32)

### Configure GDB

    $ cat > ~/.gdbinit
    set disassembly-flavor intel
    set history filename ~/.gdbhistory
    set history save on

(See http://www.delorie.com/gnu/docs/gdb/gdb_183.html re history.)
