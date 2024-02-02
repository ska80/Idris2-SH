module Core.SchemeEval.Builtins

import Core.Context
import Core.SchemeEval.ToScheme
import Core.TT

import Data.Vect
import Libraries.Utils.Scheme

-- Integers are wrapped, so unwrap then wrap again
add : Maybe IntKind -> SchemeObj Write -> SchemeObj Write -> SchemeObj Write
add (Just (Signed (P n))) x y = Apply (Var "ct-s+") [x, y, toScheme (n-1)]
add (Just (Unsigned n)) x y = Apply (Var "ct-u+") [x, y, toScheme n]
add _ x y = Apply (Var "ct+") [x, y]

sub : Maybe IntKind -> SchemeObj Write -> SchemeObj Write -> SchemeObj Write
sub (Just (Signed (P n))) x y = Apply (Var "ct-s-") [x, y, toScheme (n-1)]
sub (Just (Unsigned n)) x y = Apply (Var "ct-u-") [x, y, toScheme n]
sub _ x y = Apply (Var "ct-") [x, y]

mul : Maybe IntKind -> SchemeObj Write -> SchemeObj Write -> SchemeObj Write
mul (Just (Signed (P n))) x y = Apply (Var "ct-s*") [x, y, toScheme (n-1)]
mul (Just (Unsigned n)) x y = Apply (Var "ct-u*") [x, y, toScheme n]
mul _ x y = Apply (Var "ct*") [x, y]

div : Maybe IntKind -> SchemeObj Write -> SchemeObj Write -> SchemeObj Write
div (Just (Signed Unlimited)) x y = Apply (Var "ct/") [x, y]
div (Just (Signed (P n))) x y = Apply (Var "ct-s/") [x, y, toScheme (n-1)]
div (Just (Unsigned n)) x y = Apply (Var "ct-u/") [x, y, toScheme n]
div _ x y = Apply (Var "ct/") [x, y]

mod : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
mod x y = Apply (Var "ct-mod") [x, y]

shl : Maybe IntKind -> SchemeObj Write -> SchemeObj Write -> SchemeObj Write
shl (Just (Signed (P n))) x y = Apply (Var "ct-bits-shl-signed") [x, y, toScheme (n-1)]
shl (Just (Unsigned n)) x y = Apply (Var "ct-bits-shl") [x, y, toScheme n]
shl _ x y = Apply (Var "ct-shl") [x, y]

shr : Maybe IntKind -> SchemeObj Write -> SchemeObj Write -> SchemeObj Write
shr _ x y = Apply (Var "ct-shr") [x, y]

-- Floats don't need wrapping, since there's only one double type
-- FIXME: single-precision floating-point operations
addF32 : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
addF32 x y = Apply (Var "+") [x, y]

subF32 : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
subF32 x y = Apply (Var "-") [x, y]

mulF32 : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
mulF32 x y = Apply (Var "*") [x, y]

divF32 : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
divF32 x y = Apply (Var "/") [x, y]

-- FIXME: double-precision floating-point operations
addF64 : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
addF64 x y = Apply (Var "+") [x, y]

subF64 : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
subF64 x y = Apply (Var "-") [x, y]

mulF64 : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
mulF64 x y = Apply (Var "*") [x, y]

divF64 : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
divF64 x y = Apply (Var "/") [x, y]

-- Check necessary arguments are in canonical form before applying the
-- operator, otherwise return the blocked form
-- Current assumption is that all primitives that we can evaluate at
-- compile time work on constants, if they do anything in Scheme at all.
canonical : SchemeObj Write ->
            Vect n (SchemeObj Write) -> SchemeObj Write -> SchemeObj Write
canonical blk [] body = body
canonical blk (n :: ns) body
   = If (Apply (Var "ct-isConstant") [n]) (canonical blk ns body) blk

-- Return blocked application if a partial operator is given an input
-- on which it's undefined
testPartial : SchemeObj Write -> SchemeObj Write -> SchemeObj Write
testPartial blk res
    = Let "p-0" res $
          (If (Apply (Var "ct-isConstant") [Var "p-0"])
              (Var "p-0")
              blk)

unaryOp : SchemeObj Write -> String ->
          SchemeObj Write -> SchemeObj Write
unaryOp blk op x = canonical blk [x] $ Apply (Var op) [x]

binOp : SchemeObj Write -> String ->
        SchemeObj Write -> SchemeObj Write -> SchemeObj Write
binOp blk op x y = canonical blk [x, y] $ Apply (Var op) [x, y]

ternaryOp : SchemeObj Write -> String ->
            SchemeObj Write -> SchemeObj Write -> SchemeObj Write ->
            SchemeObj Write
ternaryOp blk op x y z = canonical blk [x, y, z] $ Apply (Var op) [x, y, z]

int : SchemeObj Write -> SchemeObj Write
int obj = Vector (-100) [obj]

int8 : SchemeObj Write -> SchemeObj Write
int8 obj = Vector (-101) [obj]

int16 : SchemeObj Write -> SchemeObj Write
int16 obj = Vector (-102) [obj]

int32 : SchemeObj Write -> SchemeObj Write
int32 obj = Vector (-103) [obj]

int64 : SchemeObj Write -> SchemeObj Write
int64 obj = Vector (-104) [obj]

integer : SchemeObj Write -> SchemeObj Write
integer obj = Vector (-105) [obj]

bits8 : SchemeObj Write -> SchemeObj Write
bits8 obj = Vector (-106) [obj]

bits16 : SchemeObj Write -> SchemeObj Write
bits16 obj = Vector (-107) [obj]

bits32 : SchemeObj Write -> SchemeObj Write
bits32 obj = Vector (-108) [obj]

bits64 : SchemeObj Write -> SchemeObj Write
bits64 obj = Vector (-109) [obj]

wrap : IntKind -> SchemeObj Write -> SchemeObj Write
wrap (Signed Unlimited) = integer
wrap (Signed (P 8)) = int8
wrap (Signed (P 16)) = int16
wrap (Signed (P 32)) = int32
wrap (Signed (P 64)) = int64
wrap (Unsigned 8) = bits8
wrap (Unsigned 16) = bits16
wrap (Unsigned 32) = bits32
wrap (Unsigned 64) = bits64
wrap _ = integer

-- Result has to be wrapped in Int, which is Vector (-100)
boolOp : SchemeObj Write -> String ->
         SchemeObj Write -> SchemeObj Write -> SchemeObj Write
boolOp blk op x y
    = canonical blk [x, y] $
         int $
             Apply (Var "or")
                [Apply (Var "and") [Apply (Var op) [x, y],
                                    IntegerVal 1],
                 IntegerVal 0]

applyIntCast : IntKind -> IntKind -> SchemeObj Write -> SchemeObj Write
applyIntCast _ (Signed Unlimited) x = x
applyIntCast (Signed m) k@(Signed (P n)) x
    = if P n >= m
         then x
         else wrap k $ Apply (Var "ct-cast-signed") [x, toScheme (n - 1)]
applyIntCast (Unsigned m) k@(Signed (P n)) x
    = if n > m
         then x
         else wrap k $ Apply (Var "ct-cast-signed") [x, toScheme (n - 1)]
applyIntCast (Signed _) k@(Unsigned n) x
    = wrap k $ Apply (Var "ct-cast-unsigned") [x, toScheme n]
applyIntCast (Unsigned m) (Unsigned n) x
    = if n >= m
         then x
         else Apply (Var "ct-cast-unsigned") [x, toScheme n]

applyCast : SchemeObj Write ->
            PrimType -> PrimType ->
            SchemeObj Write -> SchemeObj Write
applyCast blk CharType to x
    = canonical blk [x] $
        case intKind to of
           Nothing =>
              case to of
                   StringType => Apply (Var "string") [x]
                   _ => blk
           Just (Signed Unlimited) => integer $ Apply (Var "char->integer") [x]
           Just k@(Signed (P n)) => wrap k $ Apply (Var "ct-cast-char-boundedInt") [x, toScheme (n - 1)]
           Just k@(Unsigned n) => wrap k $ Apply (Var "ct-cast-char-boundedUInt") [x, toScheme n]
applyCast blk from CharType x
    = canonical blk [x] $
        case intKind from of
           Nothing => blk
           Just k => Apply (Var "ct-cast-int-char") [x]
applyCast blk StringType to x
    = canonical blk [x] $
        case intKind to of
           Nothing => case to of
                           Float64Type => Apply (Var "ct-cast-string-double") [x]
                           _ => blk
           Just (Signed Unlimited) => integer $ Apply (Var "ct-cast-string-int") [x]
           Just k@(Signed (P n)) => wrap k $ Apply (Var "ct-cast-string-boundedInt") [x, toScheme (n - 1)]
           Just k@(Unsigned n) => wrap k $ Apply (Var "ct-cast-string-boundedUInt") [x, toScheme n]
applyCast blk from StringType x
    = canonical blk [x] $
        case intKind from of
           Nothing => case from of
                           Float64Type => Apply (Var "number->string") [x]
                           _ => blk
           Just k => Apply (Var "ct-cast-number-string") [x]
applyCast blk Float64Type to x
    = canonical blk [x] $
        case intKind to of
           Nothing => case to of
                           StringType => Apply (Var "number->string") [x]
                           _ => blk
           Just (Signed Unlimited) => integer $ Apply (Var "ct-exact-truncate") [x]
           Just k@(Signed (P n)) => wrap k $ Apply (Var "ct-exact-truncate-boundedInt") [x, toScheme (n - 1)]
           Just k@(Unsigned n) => wrap k $ Apply (Var "ct-exact-truncate-boundedUInt") [x, toScheme n]
applyCast blk from Float64Type x
    = canonical blk [x] $
        case intKind from of
           Nothing => case from of
                           StringType => Apply (Var "ct-cast-string-double") [x]
                           _ => blk
           Just k => Apply (Var "ct-int-double") [x]
applyCast blk from to x
    = canonical blk [x] $
        case (intKind from, intKind to) of
           (Just f, Just t) => applyIntCast f t x
           _ => blk

applyOp : SchemeObj Write -> -- if we don't have arguments in canonical form
          PrimFn n -> Vect n (SchemeObj Write) ->
          SchemeObj Write
applyOp blk (Add Float32Type) [x, y] = binOp blk "+" x y
applyOp blk (Sub Float32Type) [x, y] = binOp blk "-" x y
applyOp blk (Mul Float32Type) [x, y] = binOp blk "*" x y
applyOp blk (Div Float32Type) [x, y] = binOp blk "/" x y
applyOp blk (Neg Float32Type) [x] = unaryOp blk "-" x

applyOp blk (Add Float64Type) [x, y] = binOp blk "+" x y
applyOp blk (Sub Float64Type) [x, y] = binOp blk "-" x y
applyOp blk (Mul Float64Type) [x, y] = binOp blk "*" x y
applyOp blk (Div Float64Type) [x, y] = binOp blk "/" x y
applyOp blk (Neg Float64Type) [x] = unaryOp blk "-" x

applyOp blk (Add ty) [x, y] = canonical blk [x, y] $ add (intKind ty) x y
applyOp blk (Sub ty) [x, y] = canonical blk [x, y] $ sub (intKind ty) x y
applyOp blk (Mul ty) [x, y] = canonical blk [x, y] $ mul (intKind ty) x y
applyOp blk (Div ty) [x, y] = canonical blk [x, y] $ div (intKind ty) x y
applyOp blk (Mod ty) [x, y] = canonical blk [x, y] $ mod x y
applyOp blk (Neg ty) [x] = canonical blk [x] $ Apply (Var "ct-neg") [x]
applyOp blk (ShiftL ty) [x, y] = canonical blk [x, y] $ shl (intKind ty) x y
applyOp blk (ShiftR ty) [x, y] = canonical blk [x, y] $ shr (intKind ty) x y
applyOp blk (BAnd ty) [x, y] = binOp blk "ct-and" x y
applyOp blk (BOr ty) [x, y] = binOp blk "ct-or" x y
applyOp blk (BXOr ty) [x, y] = binOp blk "ct-xor" x y
applyOp blk (LT CharType) [x, y] = boolOp blk "char<?" x y
applyOp blk (LTE CharType) [x, y] = boolOp blk "char<=?" x y
applyOp blk (EQ CharType) [x, y] = boolOp blk "char=?" x y
applyOp blk (GTE CharType) [x, y] = boolOp blk "char>=?" x y
applyOp blk (GT CharType) [x, y] = boolOp blk "char>?" x y
applyOp blk (LT StringType) [x, y] = boolOp blk "string<?" x y
applyOp blk (LTE StringType) [x, y] = boolOp blk "string<=?" x y
applyOp blk (EQ StringType) [x, y] = boolOp blk "string=?" x y
applyOp blk (GTE StringType) [x, y] = boolOp blk "string>=?" x y
applyOp blk (GT StringType) [x, y] = boolOp blk "string>?" x y

applyOp blk (LT Float32Type) [x, y] = boolOp blk "<" x y
applyOp blk (LTE Float32Type) [x, y] = boolOp blk "<=" x y
applyOp blk (EQ Float32Type) [x, y] = boolOp blk "=" x y
applyOp blk (GTE Float32Type) [x, y] = boolOp blk ">=" x y
applyOp blk (GT Float32Type) [x, y] = boolOp blk ">" x y

applyOp blk (LT Float64Type) [x, y] = boolOp blk "<" x y
applyOp blk (LTE Float64Type) [x, y] = boolOp blk "<=" x y
applyOp blk (EQ Float64Type) [x, y] = boolOp blk "=" x y
applyOp blk (GTE Float64Type) [x, y] = boolOp blk ">=" x y
applyOp blk (GT Float64Type) [x, y] = boolOp blk ">" x y

applyOp blk (LT ty) [x, y] = boolOp blk "ct<" x y
applyOp blk (LTE ty) [x, y] = boolOp blk "ct<=" x y
applyOp blk (EQ ty) [x, y] = boolOp blk "ct=" x y
applyOp blk (GTE ty) [x, y] = boolOp blk "ct>=" x y
applyOp blk (GT ty) [x, y] = boolOp blk "ct>" x y
applyOp blk StrLength [x]
    = canonical blk [x] $ Vector (-100) [Apply (Var "string-length") [x]]
applyOp blk StrHead [x]
    = canonical blk [x] $ Apply (Var "string-ref")
                                [x, IntegerVal 0]
applyOp blk StrTail [x]
    = canonical blk [x] $ Apply (Var "substring")
                                [x, IntegerVal 1,
                                 Apply (Var "string-length") [x]]
applyOp blk StrIndex [x, y]
    = canonical blk [x, y] $ testPartial blk $
         Apply (Var "ct-string-ref") [x, y]
applyOp blk StrCons [x, y]
    = canonical blk [x, y] $ Apply (Var "ct-string-cons") [x, y]
applyOp blk StrAppend [x, y]
    = canonical blk [x, y] $ Apply (Var "string-append") [x, y]
applyOp blk StrReverse [x]
    = canonical blk [x] $ Apply (Var "ct-string-reverse") [x]
applyOp blk StrSubstr [x, y, z]
    = canonical blk [x, y, z] $ Apply (Var "ct-string-substr") [x]

applyOp blk Float32Exp [x] = unaryOp blk "flexp" x
applyOp blk Float32Log [x] = unaryOp blk "fllog" x
applyOp blk Float32Pow [x, y] = binOp blk "expt" x y
applyOp blk Float32Sin [x] = unaryOp blk "flsin" x
applyOp blk Float32Cos [x] = unaryOp blk "flcos" x
applyOp blk Float32Tan [x] = unaryOp blk "fltan" x
applyOp blk Float32ASin [x] = unaryOp blk "flasin" x
applyOp blk Float32ACos [x] = unaryOp blk "flacos" x
applyOp blk Float32ATan [x] = unaryOp blk "flatan" x
applyOp blk Float32Sqrt [x] = unaryOp blk "flsqrt" x
applyOp blk Float32Floor [x] = unaryOp blk "flfloor" x
applyOp blk Float32Ceiling [x] = unaryOp blk "flceiling" x

applyOp blk Float64Exp [x] = unaryOp blk "flexp" x
applyOp blk Float64Log [x] = unaryOp blk "fllog" x
applyOp blk Float64Pow [x, y] = binOp blk "expt" x y
applyOp blk Float64Sin [x] = unaryOp blk "flsin" x
applyOp blk Float64Cos [x] = unaryOp blk "flcos" x
applyOp blk Float64Tan [x] = unaryOp blk "fltan" x
applyOp blk Float64ASin [x] = unaryOp blk "flasin" x
applyOp blk Float64ACos [x] = unaryOp blk "flacos" x
applyOp blk Float64ATan [x] = unaryOp blk "flatan" x
applyOp blk Float64Sqrt [x] = unaryOp blk "flsqrt" x
applyOp blk Float64Floor [x] = unaryOp blk "flfloor" x
applyOp blk Float64Ceiling [x] = unaryOp blk "flceiling" x

applyOp blk (Cast from to) [x] = applyCast blk from to x
applyOp blk BelieveMe [_, _, x] = x
applyOp blk Crash [_, msg] = blk

mkArgList : Int -> (n : Nat) -> Vect n String
mkArgList i Z = []
mkArgList i (S k) = ("x-" ++ show i) :: mkArgList (i + 1) k

export
compileBuiltin : {farity : Nat} ->
                Name -> PrimFn farity -> SchemeObj Write
compileBuiltin nm fn
    = let args = mkArgList 0 farity in
          bindArgs args [] args
  where
    makeBlockedApp : Vect n String -> SchemeObj Write
    makeBlockedApp args = Vector (-2) [toScheme nm, vars args]
      where
        vars : forall n . Vect n String -> SchemeObj Write
        vars [] = Null
        vars (x :: xs) = Cons (Var x) (vars xs)

    bindArgs : Vect n String -> Vect n' String ->
               Vect farity String -> SchemeObj Write
    bindArgs [] done args = applyOp (makeBlockedApp args) fn (map Var args)
    bindArgs (x :: xs) done args
        = Vector (-9) [makeBlockedApp (reverse done),
                       Lambda [x] (bindArgs xs (x :: done) args)]
