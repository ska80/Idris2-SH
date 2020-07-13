module Idris.IDEMode.REPL.FFI

%foreign "C:fdopen,libc 6"
export
prim__fdopen : Int -> String -> PrimIO AnyPtr
