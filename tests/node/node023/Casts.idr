%foreign "javascript:lambda: (x) => console.log('ok')"
prim__DoubleOk : Double -> PrimIO ()

%foreign "javascript:lambda: (x) => console.log('ok')"
prim__FloatOk : Float -> PrimIO ()

main : HasIO io => io ()
main = do
    primIO $ prim__DoubleOk 0
    primIO $ prim__DoubleOk 99
    primIO $ prim__DoubleOk 100
    primIO $ prim__DoubleOk (-1)
    primIO $ prim__DoubleOk 1234567890

    primIO $ prim__FloatOk 0
    primIO $ prim__FloatOk 99
    primIO $ prim__FloatOk 100
    primIO $ prim__FloatOk (-1)
    primIO $ prim__FloatOk 1234567890
