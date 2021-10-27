module Prelude.Float

import Builtin
import Prelude.Num
import Prelude.Cast

%default total

-------------------------------
-- FLOATING-POINT PRIMITIVES --
-------------------------------

public export
interface Num fp => Neg fp => Fractional fp => FloatingPointPrimitives fp where
  constructor MkFloatingPointPrimitives

  exp : fp -> fp

  log : fp -> fp

  pow : fp -> fp -> fp
  pow x y = exp (y * log x)

  sin : fp -> fp

  cos : fp -> fp

  tan : fp -> fp

  asin : fp -> fp

  acos : fp -> fp

  atan : fp -> fp

  sinh : fp -> fp
  sinh x = (exp x - exp (-x)) / 2

  cosh : fp -> fp
  cosh x = (exp x + exp (-x)) / 2

  tanh : fp -> fp
  tanh x = sinh x / cosh x

  sqrt : fp -> fp

  floor : fp -> fp

  ceiling : fp -> fp


-- Float

public export
FloatingPointPrimitives Float where
  exp x = prim__floatExp x

  log x = prim__floatLog x

  sin x = prim__floatSin x

  cos x = prim__floatCos x

  tan x = prim__floatTan x

  asin x = prim__floatASin x

  acos x = prim__floatACos x

  atan x = prim__floatATan x

  sqrt x = prim__floatSqrt x

  floor x = prim__floatFloor x

  ceiling x = prim__floatCeiling x


-- Double

public export
FloatingPointPrimitives Double where
  exp x = prim__doubleExp x

  log x = prim__doubleLog x

  sin x = prim__doubleSin x

  cos x = prim__doubleCos x

  tan x = prim__doubleTan x

  asin x = prim__doubleASin x

  acos x = prim__doubleACos x

  atan x = prim__doubleATan x

  sqrt x = prim__doubleSqrt x

  floor x = prim__doubleFloor x

  ceiling x = prim__doubleCeiling x


-- Constants

public export
pi : Double
pi = 3.14159265358979323846

public export
fpi : Float
fpi = 3.14159265358979323846

public export
euler : Double
euler = 2.7182818284590452354

public export
feuler : Float
feuler = 2.7182818284590452354
