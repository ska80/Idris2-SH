import Data.So

%floatLit fromFloat

public export
interface FromFloat ty where
  fromFloat : Float -> ty

%allow_overloads fromFloat


record Newtype where
  constructor
  MkNewtype
  wrapped : Float

FromFloat Newtype where
  fromFloat = MkNewtype

Show Newtype where
  showPrec p (MkNewtype v) = showCon p "MkNewtype" $ showArg v


record InUnit where
  constructor MkInUnit
  value      : Float
  0 inBounds : So (0 <= value && value <= 1)

Show InUnit where
  showPrec p (MkInUnit v _) = showCon p "MkInUnit" $ showArg v ++ " _"

namespace InUnit
  public export
  fromFloat :  (v : Float)
             -> {auto 0 prf : So (0 <= v && v <= 1)}
             -> InUnit
  fromFloat v = MkInUnit v prf


main : IO ()
main = do printLn $ the InUnit 0.25
          printLn $ the Newtype 123.456