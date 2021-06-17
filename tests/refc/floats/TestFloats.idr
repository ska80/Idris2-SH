module TestFloats

put : Float -> IO ()
put = putStrLn . show

main : IO ()
main = do
    put $ expf 1
    put $ logf 1

    put $ sinf 1
    put $ cosf 1
    put $ tanf 1
    put $ asinf 1
    put $ acosf 1
    put $ atanf 1

    put $ sqrtf 2

    put $ floorf 1.5
    put $ ceilingf 1.5
