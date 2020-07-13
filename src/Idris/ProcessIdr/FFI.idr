module Idris.ProcessIdr.FFI

%foreign "scheme:collect"
export
prim__gc : Int -> PrimIO ()
