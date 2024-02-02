module Prelude.Float

import Builtin
import Prelude.Num

%default total

-------------------------------
-- Floating-point primitives --
-------------------------------

namespace Float32
  -- FIXME: (floats)
  -- public export
  -- pi : Float32
  -- pi = cast Float64.pi

  -- public export
  -- euler : Float32
  -- euler = cast Float64.euler

  public export
  exp : Float32 -> Float32
  exp x = prim__float32Exp x

  public export
  log : Float32 -> Float32
  log x = prim__float32Log x

  public export
  pow : Float32 -> Float32 -> Float32
  pow x y = prim__float32Pow x y

  public export
  sin : Float32 -> Float32
  sin x = prim__float32Sin x

  public export
  cos : Float32 -> Float32
  cos x = prim__float32Cos x

  public export
  tan : Float32 -> Float32
  tan x = prim__float32Tan x

  public export
  asin : Float32 -> Float32
  asin x = prim__float32ASin x

  public export
  acos : Float32 -> Float32
  acos x = prim__float32ACos x

  public export
  atan : Float32 -> Float32
  atan x = prim__float32ATan x

  public export
  sinh : Float32 -> Float32
  sinh x = (exp x - exp (-x)) / 2

  public export
  cosh : Float32 -> Float32
  cosh x = (exp x + exp (-x)) / 2

  public export
  tanh : Float32 -> Float32
  tanh x = sinh x / cosh x

  public export
  sqrt : Float32 -> Float32
  sqrt x = prim__float32Sqrt x

  public export
  floor : Float32 -> Float32
  floor x = prim__float32Floor x

  public export
  ceiling : Float32 -> Float32
  ceiling x = prim__float32Ceiling x

namespace Float64
  -- FIXME: (floats)
  -- public export
  -- pi : Float64
  -- pi = 3.14159265358979323846

  -- public export
  -- euler : Float64
  -- euler = 2.7182818284590452354

  public export
  exp : Float64 -> Float64
  exp x = prim__float64Exp x

  public export
  log : Float64 -> Float64
  log x = prim__float64Log x

  public export
  pow : Float64 -> Float64 -> Float64
  pow x y = prim__float64Pow x y

  public export
  sin : Float64 -> Float64
  sin x = prim__float64Sin x

  public export
  cos : Float64 -> Float64
  cos x = prim__float64Cos x

  public export
  tan : Float64 -> Float64
  tan x = prim__float64Tan x

  public export
  asin : Float64 -> Float64
  asin x = prim__float64ASin x

  public export
  acos : Float64 -> Float64
  acos x = prim__float64ACos x

  public export
  atan : Float64 -> Float64
  atan x = prim__float64ATan x

  public export
  sinh : Float64 -> Float64
  sinh x = (exp x - exp (-x)) / 2

  public export
  cosh : Float64 -> Float64
  cosh x = (exp x + exp (-x)) / 2

  public export
  tanh : Float64 -> Float64
  tanh x = sinh x / cosh x

  public export
  sqrt : Float64 -> Float64
  sqrt x = prim__float64Sqrt x

  public export
  floor : Float64 -> Float64
  floor x = prim__float64Floor x

  public export
  ceiling : Float64 -> Float64
  ceiling x = prim__float64Ceiling x
