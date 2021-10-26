module TestFloats

put : Float -> IO ()
put = putStrLn . show

main : IO ()
main = do
    put $ cast $ exp 1
    put $ cast $ log 1

    put $ cast $ sin 1
    put $ cast $ cos 1
    put $ cast $ tan 1
    put $ cast $ asin 1
    put $ cast $ acos 1
    put $ cast $ atan 1

    put $ cast $ sqrt 2

    put $ cast $ floor 1.5
    put $ cast $ ceiling 1.5
