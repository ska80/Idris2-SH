module TestFloats

put : Float -> IO ()
put = putStrLn . show

main : IO ()
main = do
    put $ expf 1f
    put $ logf 1f

    put $ sinf 1f
    put $ cosf 1f
    put $ tanf 1f
    put $ asinf 1f
    put $ acosf 1f
    put $ atanf 1f

    put $ sqrtf 2f

    put $ floorf 1.5f
    put $ ceilingf 1.5f
