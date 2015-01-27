* does not yet optimize away dead code or unused labels introduced by
  macros


## Setup

    $ cat > ~/.gdbinit
    set disassembly-flavor intel
    set history filename ~/.gdbhistory
    set history save on

(See http://www.delorie.com/gnu/docs/gdb/gdb_183.html re history.)
