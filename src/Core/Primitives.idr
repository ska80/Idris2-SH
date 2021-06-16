module Core.Primitives

import Core.Core
import Core.Context
import Core.TT
import Core.Value
import Libraries.Utils.String

import Data.Vect

%default covering

public export
record Prim where
  constructor MkPrim
  {arity : Nat}
  fn : PrimFn arity
  type : ClosedTerm
  totality : Totality

binOp : (Constant -> Constant -> Maybe Constant) ->
        {vars : _} -> Vect 2 (NF vars) -> Maybe (NF vars)
binOp fn [NPrimVal fc x, NPrimVal _ y]
    = map (NPrimVal fc) (fn x y)
binOp _ _ = Nothing

unaryOp : (Constant -> Maybe Constant) ->
          {vars : _} -> Vect 1 (NF vars) -> Maybe (NF vars)
unaryOp fn [NPrimVal fc x]
    = map (NPrimVal fc) (fn x)
unaryOp _ _ = Nothing

castString : Vect 1 (NF vars) -> Maybe (NF vars)
castString [NPrimVal fc (I i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (I8 i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (I16 i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (I32 i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (I64 i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (BI i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (B8 i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (B16 i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (B32 i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (B64 i)] = Just (NPrimVal fc (Str (show i)))
castString [NPrimVal fc (Ch i)] = Just (NPrimVal fc (Str (stripQuotes (show i))))
castString [NPrimVal fc (Db i)] = Just (NPrimVal fc (Str (show i)))
castString _ = Nothing

castInteger : Vect 1 (NF vars) -> Maybe (NF vars)
castInteger [NPrimVal fc (I i)] = Just (NPrimVal fc (BI (cast i)))
castInteger [NPrimVal fc (I8 i)] = Just (NPrimVal fc (BI i))
castInteger [NPrimVal fc (I16 i)] = Just (NPrimVal fc (BI i))
castInteger [NPrimVal fc (I32 i)] = Just (NPrimVal fc (BI i))
castInteger [NPrimVal fc (I64 i)] = Just (NPrimVal fc (BI i))
castInteger [NPrimVal fc (B8 i)] = Just (NPrimVal fc (BI (cast i)))
castInteger [NPrimVal fc (B16 i)] = Just (NPrimVal fc (BI (cast i)))
castInteger [NPrimVal fc (B32 i)] = Just (NPrimVal fc (BI (cast i)))
castInteger [NPrimVal fc (B64 i)] = Just (NPrimVal fc (BI i))
castInteger [NPrimVal fc (Ch i)] = Just (NPrimVal fc (BI (cast (cast {to=Int} i))))
castInteger [NPrimVal fc (Db i)] = Just (NPrimVal fc (BI (cast i)))
castInteger [NPrimVal fc (Str i)] = Just (NPrimVal fc (BI (cast i)))
castInteger _ = Nothing

castInt : Vect 1 (NF vars) -> Maybe (NF vars)
castInt [NPrimVal fc (I8 i)] = Just (NPrimVal fc (I (fromInteger i)))
castInt [NPrimVal fc (I16 i)] = Just (NPrimVal fc (I (fromInteger i)))
castInt [NPrimVal fc (I32 i)] = Just (NPrimVal fc (I (fromInteger i)))
castInt [NPrimVal fc (I64 i)] = Just (NPrimVal fc (I (fromInteger i)))
castInt [NPrimVal fc (BI i)] = Just (NPrimVal fc (I (fromInteger i)))
castInt [NPrimVal fc (B8 i)] = Just (NPrimVal fc (I i))
castInt [NPrimVal fc (B16 i)] = Just (NPrimVal fc (I i))
castInt [NPrimVal fc (B32 i)] = Just (NPrimVal fc (I i))
castInt [NPrimVal fc (B64 i)] = Just (NPrimVal fc (I (fromInteger i)))
castInt [NPrimVal fc (Db i)] = Just (NPrimVal fc (I (cast i)))
castInt [NPrimVal fc (Ch i)] = Just (NPrimVal fc (I (cast i)))
castInt [NPrimVal fc (Str i)] = Just (NPrimVal fc (I (cast i)))
castInt _ = Nothing

b8max : Int
b8max = 0x100

b16max : Int
b16max = 0x10000

b32max : Int
b32max = 0x100000000

b64max : Integer
b64max = 18446744073709551616 -- 0x10000000000000000

bitCastWrap : (i : Integer) -> (max : Integer) -> Integer
bitCastWrap i max
    = if i >= 0 -- oops, we don't have `rem` yet!
        then i `mod` max
        else max + i `mod` max

int8max : Integer
int8max = 0x80

int16max : Integer
int16max = 0x8000

int32max : Integer
int32max = 0x80000000

int64max : Integer
int64max = 0x8000000000000000

intCastWrap : (i : Integer) -> (max : Integer) -> Integer
intCastWrap i max =
  let max2 = 2*max
      i2   = i `mod` max2
      i3   = if i2 < 0 then i2 + max2 else i2
   in if i3 >= max then i3 - max2 else i3

int8CastWrap : (i : Integer) -> Integer
int8CastWrap i = intCastWrap i int8max

int16CastWrap : (i : Integer) -> Integer
int16CastWrap i = intCastWrap i int16max

int32CastWrap : (i : Integer) -> Integer
int32CastWrap i = intCastWrap i int32max

int64CastWrap : (i : Integer) -> Integer
int64CastWrap i = intCastWrap i int64max

constantIntegerValue : Constant -> Maybe Integer
constantIntegerValue (I i)   = Just $ cast i
constantIntegerValue (I8 i)   = Just i
constantIntegerValue (I16 i)   = Just i
constantIntegerValue (I32 i)   = Just i
constantIntegerValue (I64 i)   = Just i
constantIntegerValue (BI i)  = Just i
constantIntegerValue (B8 i)  = Just $ cast i
constantIntegerValue (B16 i) = Just $ cast i
constantIntegerValue (B32 i) = Just $ cast i
constantIntegerValue (B64 i) = Just i
constantIntegerValue _       = Nothing

castBits8 : Vect 1 (NF vars) -> Maybe (NF vars)
castBits8 [NPrimVal fc constant] = do
    value <- constantIntegerValue constant
    let wrapped = bitCastWrap value (cast b8max)
    pure (NPrimVal fc (B8 (cast wrapped)))
castBits8 _ = Nothing

castBits16 : Vect 1 (NF vars) -> Maybe (NF vars)
castBits16 [NPrimVal fc constant] = do
    value <- constantIntegerValue constant
    let wrapped = bitCastWrap value (cast b16max)
    pure (NPrimVal fc (B16 (cast wrapped)))
castBits16 _ = Nothing

castBits32 : Vect 1 (NF vars) -> Maybe (NF vars)
castBits32 [NPrimVal fc constant] = do
    value <- constantIntegerValue constant
    let wrapped = bitCastWrap value (cast b32max)
    pure (NPrimVal fc (B32 (cast wrapped)))
castBits32 _ = Nothing

castBits64 : Vect 1 (NF vars) -> Maybe (NF vars)
castBits64 [NPrimVal fc constant] = do
    value <- constantIntegerValue constant
    let wrapped = bitCastWrap value b64max
    pure (NPrimVal fc (B64 wrapped))
castBits64 _ = Nothing

castInt8 : Vect 1 (NF vars) -> Maybe (NF vars)
castInt8 [NPrimVal fc constant] = do
    value <- constantIntegerValue constant
    let wrapped = int8CastWrap value
    pure (NPrimVal fc (I8 wrapped))
castInt8 _ = Nothing

castInt16 : Vect 1 (NF vars) -> Maybe (NF vars)
castInt16 [NPrimVal fc constant] = do
    value <- constantIntegerValue constant
    let wrapped = int16CastWrap value
    pure (NPrimVal fc (I16 wrapped))
castInt16 _ = Nothing

castInt32 : Vect 1 (NF vars) -> Maybe (NF vars)
castInt32 [NPrimVal fc constant] = do
    value <- constantIntegerValue constant
    let wrapped = int32CastWrap value
    pure (NPrimVal fc (I32 wrapped))
castInt32 _ = Nothing

castInt64 : Vect 1 (NF vars) -> Maybe (NF vars)
castInt64 [NPrimVal fc constant] = do
    value <- constantIntegerValue constant
    let wrapped = int64CastWrap value
    pure (NPrimVal fc (I64 wrapped))
castInt64 _ = Nothing

castFloat : Vect 1 (NF vars) -> Maybe (NF vars)
castFloat [NPrimVal fc (I i)] = Just (NPrimVal fc (Fl (cast i)))
castFloat [NPrimVal fc (I8 i)] = Just (NPrimVal fc (Fl (cast i)))
castFloat [NPrimVal fc (I16 i)] = Just (NPrimVal fc (Fl (cast i)))
castFloat [NPrimVal fc (I32 i)] = Just (NPrimVal fc (Fl (cast i)))
castFloat [NPrimVal fc (I64 i)] = Just (NPrimVal fc (Fl (cast i)))
castFloat [NPrimVal fc (BI i)] = Just (NPrimVal fc (Fl (cast i)))
castFloat [NPrimVal fc (Str i)] = Just (NPrimVal fc (Fl (cast i)))
castFloat [NPrimVal fc (Db d)] = Just (NPrimVal fc (Fl (cast d)))
castFloat _ = Nothing

castDouble : Vect 1 (NF vars) -> Maybe (NF vars)
castDouble [NPrimVal fc (I i)] = Just (NPrimVal fc (Db (cast i)))
castDouble [NPrimVal fc (I8 i)] = Just (NPrimVal fc (Db (cast i)))
castDouble [NPrimVal fc (I16 i)] = Just (NPrimVal fc (Db (cast i)))
castDouble [NPrimVal fc (I32 i)] = Just (NPrimVal fc (Db (cast i)))
castDouble [NPrimVal fc (I64 i)] = Just (NPrimVal fc (Db (cast i)))
castDouble [NPrimVal fc (BI i)] = Just (NPrimVal fc (Db (cast i)))
castDouble [NPrimVal fc (Str i)] = Just (NPrimVal fc (Db (cast i)))
castDouble [NPrimVal fc (Fl f)] = Just (NPrimVal fc (Db (cast f)))
castDouble _ = Nothing

castChar : Vect 1 (NF vars) -> Maybe (NF vars)
castChar [NPrimVal fc (I i)] = Just (NPrimVal fc (Ch (cast i)))
castChar _ = Nothing

strLength : Vect 1 (NF vars) -> Maybe (NF vars)
strLength [NPrimVal fc (Str s)] = Just (NPrimVal fc (I (cast (length s))))
strLength _ = Nothing

strHead : Vect 1 (NF vars) -> Maybe (NF vars)
strHead [NPrimVal fc (Str "")] = Nothing
strHead [NPrimVal fc (Str str)]
    = Just (NPrimVal fc (Ch (assert_total (prim__strHead str))))
strHead _ = Nothing

strTail : Vect 1 (NF vars) -> Maybe (NF vars)
strTail [NPrimVal fc (Str "")] = Nothing
strTail [NPrimVal fc (Str str)]
    = Just (NPrimVal fc (Str (assert_total (prim__strTail str))))
strTail _ = Nothing

strIndex : Vect 2 (NF vars) -> Maybe (NF vars)
strIndex [NPrimVal fc (Str str), NPrimVal _ (I i)]
    = if i >= 0 && integerToNat (cast i) < length str
         then Just (NPrimVal fc (Ch (assert_total (prim__strIndex str i))))
         else Nothing
strIndex _ = Nothing

strCons : Vect 2 (NF vars) -> Maybe (NF vars)
strCons [NPrimVal fc (Ch x), NPrimVal _ (Str y)]
    = Just (NPrimVal fc (Str (strCons x y)))
strCons _ = Nothing

strAppend : Vect 2 (NF vars) -> Maybe (NF vars)
strAppend [NPrimVal fc (Str x), NPrimVal _ (Str y)]
    = Just (NPrimVal fc (Str (x ++ y)))
strAppend _ = Nothing

strReverse : Vect 1 (NF vars) -> Maybe (NF vars)
strReverse [NPrimVal fc (Str x)]
    = Just (NPrimVal fc (Str (reverse x)))
strReverse _ = Nothing

strSubstr : Vect 3 (NF vars) -> Maybe (NF vars)
strSubstr [NPrimVal fc (I start), NPrimVal _ (I len), NPrimVal _ (Str str)]
    = Just (NPrimVal fc (Str (prim__strSubstr start len str)))
strSubstr _ = Nothing


add : Constant -> Constant -> Maybe Constant
add (BI x) (BI y) = pure $ BI (x + y)
add (I x) (I y) = pure $ I (x + y)
add (I8 x) (I8 y) = pure $ I8 (int8CastWrap $ x + y)
add (I16 x) (I16 y) = pure $ I16 (int16CastWrap $ x + y)
add (I32 x) (I32 y) = pure $ I32 (int32CastWrap $ x + y)
add (I64 x) (I64 y) = pure $ I64 (int64CastWrap $ x + y)
add (B8 x) (B8 y) = pure $ B8 $ (x + y) `mod` b8max
add (B16 x) (B16 y) = pure $ B16 $ (x + y) `mod` b16max
add (B32 x) (B32 y) = pure $ B32 $ (x + y) `mod` b32max
add (B64 x) (B64 y) = pure $ B64 $ (x + y) `mod` b64max
add (Ch x) (Ch y) = pure $ Ch (cast (cast {to=Int} x + cast y))
add (Db x) (Db y) = pure $ Db (x + y)
add _ _ = Nothing

sub : Constant -> Constant -> Maybe Constant
sub (BI x) (BI y) = pure $ BI (x - y)
sub (I x) (I y) = pure $ I (x - y)
sub (I8 x) (I8 y) = pure $ I8 (int8CastWrap $ x - y)
sub (I16 x) (I16 y) = pure $ I16 (int16CastWrap $ x - y)
sub (I32 x) (I32 y) = pure $ I32 (int32CastWrap $ x - y)
sub (I64 x) (I64 y) = pure $ I64 (int64CastWrap $ x - y)
sub (Ch x) (Ch y) = pure $ Ch (cast (cast {to=Int} x - cast y))
sub (Db x) (Db y) = pure $ Db (x - y)
sub _ _ = Nothing

mul : Constant -> Constant -> Maybe Constant
mul (BI x) (BI y) = pure $ BI (x * y)
mul (B8 x) (B8 y) = pure $ B8 $ (x * y) `mod` b8max
mul (B16 x) (B16 y) = pure $ B16 $ (x * y) `mod` b16max
mul (B32 x) (B32 y) = pure $ B32 $ (x * y) `mod` b32max
mul (B64 x) (B64 y) = pure $ B64 $ (x * y) `mod` b64max
mul (I x) (I y) = pure $ I (x * y)
mul (I8 x) (I8 y) = pure $ I8 (int8CastWrap $ x * y)
mul (I16 x) (I16 y) = pure $ I16 (int16CastWrap $ x * y)
mul (I32 x) (I32 y) = pure $ I32 (int32CastWrap $ x * y)
mul (I64 x) (I64 y) = pure $ I64 (int64CastWrap $ x * y)
mul (Db x) (Db y) = pure $ Db (x * y)
mul _ _ = Nothing

div : Constant -> Constant -> Maybe Constant
div (BI x) (BI 0) = Nothing
div (BI x) (BI y) = pure $ BI (assert_total (x `div` y))
div (I x) (I 0) = Nothing
div (I x) (I y) = pure $ I (assert_total (x `div` y))
div (I8 x) (I8 0) = Nothing
div (I8 x) (I8 y) = pure $ I8 (int8CastWrap $ assert_total (x `div` y))
div (I16 x) (I16 0) = Nothing
div (I16 x) (I16 y) = pure $ I16 (int16CastWrap $ assert_total (x `div` y))
div (I32 x) (I32 0) = Nothing
div (I32 x) (I32 y) = pure $ I32 (int32CastWrap $ assert_total (x `div` y))
div (I64 x) (I64 0) = Nothing
div (I64 x) (I64 y) = pure $ I64 (int64CastWrap $ assert_total (x `div` y))
div (Db x) (Db y) = pure $ Db (x / y)
div _ _ = Nothing

mod : Constant -> Constant -> Maybe Constant
mod (BI x) (BI 0) = Nothing
mod (BI x) (BI y) = pure $ BI (assert_total (x `mod` y))
mod (I x) (I 0) = Nothing
mod (I x) (I y) = pure $ I (assert_total (x `mod` y))
mod (I8 x) (I8 0) = Nothing
mod (I8 x) (I8 y) = pure $ I8 (int8CastWrap $ assert_total (x `mod` y))
mod (I16 x) (I16 0) = Nothing
mod (I16 x) (I16 y) = pure $ I16 (int16CastWrap $ assert_total (x `mod` y))
mod (I32 x) (I32 0) = Nothing
mod (I32 x) (I32 y) = pure $ I32 (int32CastWrap $ assert_total (x `mod` y))
mod (I64 x) (I64 0) = Nothing
mod (I64 x) (I64 y) = pure $ I64 (int64CastWrap $ assert_total (x `mod` y))
mod _ _ = Nothing

-- Make sure a signed integer stays within bounds after a
-- bitwise shift operation. If, after the shift, the highest bit
-- is set, treat the result as a negative number, computing its
-- twos complement, otherwise treat it as a positive number,
-- truncating all bits above the highest one.
signedShift : (i : Integer) -> (max : Integer) -> Integer
signedShift i max =
  if prim__and_Integer i max == 0
     then prim__and_Integer i (max - 1) -- treat as positive number: highest bit unset
     else prim__or_Integer i (-max)     -- treat as negative number: highest bit set

shiftl : Constant -> Constant -> Maybe Constant
shiftl (I x) (I y) = pure $ I (prim__shl_Int x y)
shiftl (I8 x) (I8 y) = pure $ I8 (signedShift (prim__shl_Integer x y) int8max)
shiftl (I16 x) (I16 y) = pure $ I16 (signedShift (prim__shl_Integer x y) int16max)
shiftl (I32 x) (I32 y) = pure $ I32 (signedShift (prim__shl_Integer x y) int32max)
shiftl (I64 x) (I64 y) = pure $ I64 (signedShift (prim__shl_Integer x y) int64max)
shiftl (BI x) (BI y) = pure $ BI (prim__shl_Integer x y)
shiftl (B8 x) (B8 y) = pure $ B8 $ (prim__shl_Int x y) `mod` b8max
shiftl (B16 x) (B16 y) = pure $ B16 $ (prim__shl_Int x y) `mod` b16max
shiftl (B32 x) (B32 y) = pure $ B32 $ (prim__shl_Int x y) `mod` b32max
shiftl (B64 x) (B64 y) = pure $ B64 $ (prim__shl_Integer x y) `mod` b64max
shiftl _ _ = Nothing

shiftr : Constant -> Constant -> Maybe Constant
shiftr (I x) (I y) = pure $ I (prim__shr_Int x y)
shiftr (I8 x) (I8 y) = pure $ I8 (signedShift (prim__shr_Integer x y) int8max)
shiftr (I16 x) (I16 y) = pure $ I16 (signedShift (prim__shr_Integer x y) int16max)
shiftr (I32 x) (I32 y) = pure $ I32 (signedShift (prim__shr_Integer x y) int32max)
shiftr (I64 x) (I64 y) = pure $ I64 (signedShift (prim__shr_Integer x y) int64max)
shiftr (B8 x) (B8 y) = pure $ B8 $ (prim__shr_Int x y)
shiftr (B16 x) (B16 y) = pure $ B16 $ (prim__shr_Int x y)
shiftr (B32 x) (B32 y) = pure $ B32 $ (prim__shr_Int x y)
shiftr (B64 x) (B64 y) = pure $ B64 $ (prim__shr_Integer x y)
shiftr _ _ = Nothing

band : Constant -> Constant -> Maybe Constant
band (I x) (I y) = pure $ I (prim__and_Int x y)
band (I8 x) (I8 y) = pure $ I8 (prim__and_Integer x y)
band (I16 x) (I16 y) = pure $ I16 (prim__and_Integer x y)
band (I32 x) (I32 y) = pure $ I32 (prim__and_Integer x y)
band (I64 x) (I64 y) = pure $ I64 (prim__and_Integer x y)
band (BI x) (BI y) = pure $ BI (prim__and_Integer x y)
band (B8 x) (B8 y) = pure $ B8 (prim__and_Int x y)
band (B16 x) (B16 y) = pure $ B16 (prim__and_Int x y)
band (B32 x) (B32 y) = pure $ B32 (prim__and_Int x y)
band (B64 x) (B64 y) = pure $ B64 (prim__and_Integer x y)
band _ _ = Nothing

bor : Constant -> Constant -> Maybe Constant
bor (I x) (I y) = pure $ I (prim__or_Int x y)
bor (I8 x) (I8 y) = pure $ I8 (prim__or_Integer x y)
bor (I16 x) (I16 y) = pure $ I16 (prim__or_Integer x y)
bor (I32 x) (I32 y) = pure $ I32 (prim__or_Integer x y)
bor (I64 x) (I64 y) = pure $ I64 (prim__or_Integer x y)
bor (BI x) (BI y) = pure $ BI (prim__or_Integer x y)
bor (B8 x) (B8 y) = pure $ B8 (prim__or_Int x y)
bor (B16 x) (B16 y) = pure $ B16 (prim__or_Int x y)
bor (B32 x) (B32 y) = pure $ B32 (prim__or_Int x y)
bor (B64 x) (B64 y) = pure $ B64 (prim__or_Integer x y)
bor _ _ = Nothing

-- TODO: Add implementations for
--       Bits64, Integer, Int8, Int16, Int32, and Int64
bxor : Constant -> Constant -> Maybe Constant
bxor (I x) (I y) = pure $ I (prim__xor_Int x y)
bxor (B8 x) (B8 y) = pure $ B8 (prim__xor_Int x y)
bxor (B16 x) (B16 y) = pure $ B16 (prim__xor_Int x y)
bxor (B32 x) (B32 y) = pure $ B32 (prim__xor_Int x y)
bxor _ _ = Nothing

neg : Constant -> Maybe Constant
neg (BI x) = pure $ BI (-x)
neg (I x) = pure $ I (-x)
neg (I8 x) = pure . I8 $ int8CastWrap (-x)
neg (I16 x) = pure . I16 $ int16CastWrap (-x)
neg (I32 x) = pure . I32 $ int32CastWrap (-x)
neg (I64 x) = pure . I64 $ int64CastWrap (-x)
neg (Db x) = pure $ Db (-x)
neg _ = Nothing

toInt : Bool -> Constant
toInt True = I 1
toInt False = I 0

lt : Constant -> Constant -> Maybe Constant
lt (I x) (I y) = pure $ toInt (x < y)
lt (I8 x) (I8 y) = pure $ toInt (x < y)
lt (I16 x) (I16 y) = pure $ toInt (x < y)
lt (I32 x) (I32 y) = pure $ toInt (x < y)
lt (I64 x) (I64 y) = pure $ toInt (x < y)
lt (BI x) (BI y) = pure $ toInt (x < y)
lt (B8 x) (B8 y) = pure $ toInt (x < y)
lt (B16 x) (B16 y) = pure $ toInt (x < y)
lt (B32 x) (B32 y) = pure $ toInt (x < y)
lt (B64 x) (B64 y) = pure $ toInt (x < y)
lt (Str x) (Str y) = pure $ toInt (x < y)
lt (Ch x) (Ch y) = pure $ toInt (x < y)
lt (Db x) (Db y) = pure $ toInt (x < y)
lt _ _ = Nothing

lte : Constant -> Constant -> Maybe Constant
lte (I x) (I y) = pure $ toInt (x <= y)
lte (I8 x) (I8 y) = pure $ toInt (x <= y)
lte (I16 x) (I16 y) = pure $ toInt (x <= y)
lte (I32 x) (I32 y) = pure $ toInt (x <= y)
lte (I64 x) (I64 y) = pure $ toInt (x <= y)
lte (BI x) (BI y) = pure $ toInt (x <= y)
lte (B8 x) (B8 y) = pure $ toInt (x <= y)
lte (B16 x) (B16 y) = pure $ toInt (x <= y)
lte (B32 x) (B32 y) = pure $ toInt (x <= y)
lte (B64 x) (B64 y) = pure $ toInt (x <= y)
lte (Str x) (Str y) = pure $ toInt (x <= y)
lte (Ch x) (Ch y) = pure $ toInt (x <= y)
lte (Db x) (Db y) = pure $ toInt (x <= y)
lte _ _ = Nothing

eq : Constant -> Constant -> Maybe Constant
eq (I x) (I y) = pure $ toInt (x == y)
eq (I8 x) (I8 y) = pure $ toInt (x == y)
eq (I16 x) (I16 y) = pure $ toInt (x == y)
eq (I32 x) (I32 y) = pure $ toInt (x == y)
eq (I64 x) (I64 y) = pure $ toInt (x == y)
eq (BI x) (BI y) = pure $ toInt (x == y)
eq (B8 x) (B8 y) = pure $ toInt (x == y)
eq (B16 x) (B16 y) = pure $ toInt (x == y)
eq (B32 x) (B32 y) = pure $ toInt (x == y)
eq (B64 x) (B64 y) = pure $ toInt (x == y)
eq (Str x) (Str y) = pure $ toInt (x == y)
eq (Ch x) (Ch y) = pure $ toInt (x == y)
eq (Db x) (Db y) = pure $ toInt (x == y)
eq _ _ = Nothing

gte : Constant -> Constant -> Maybe Constant
gte (I x) (I y) = pure $ toInt (x >= y)
gte (I8 x) (I8 y) = pure $ toInt (x >= y)
gte (I16 x) (I16 y) = pure $ toInt (x >= y)
gte (I32 x) (I32 y) = pure $ toInt (x >= y)
gte (I64 x) (I64 y) = pure $ toInt (x >= y)
gte (BI x) (BI y) = pure $ toInt (x >= y)
gte (B8 x) (B8 y) = pure $ toInt (x >= y)
gte (B16 x) (B16 y) = pure $ toInt (x >= y)
gte (B32 x) (B32 y) = pure $ toInt (x >= y)
gte (B64 x) (B64 y) = pure $ toInt (x >= y)
gte (Str x) (Str y) = pure $ toInt (x >= y)
gte (Ch x) (Ch y) = pure $ toInt (x >= y)
gte (Db x) (Db y) = pure $ toInt (x >= y)
gte _ _ = Nothing

gt : Constant -> Constant -> Maybe Constant
gt (I x) (I y) = pure $ toInt (x > y)
gt (I8 x) (I8 y) = pure $ toInt (x > y)
gt (I16 x) (I16 y) = pure $ toInt (x > y)
gt (I32 x) (I32 y) = pure $ toInt (x > y)
gt (I64 x) (I64 y) = pure $ toInt (x > y)
gt (BI x) (BI y) = pure $ toInt (x > y)
gt (B8 x) (B8 y) = pure $ toInt (x > y)
gt (B16 x) (B16 y) = pure $ toInt (x > y)
gt (B32 x) (B32 y) = pure $ toInt (x > y)
gt (B64 x) (B64 y) = pure $ toInt (x > y)
gt (Str x) (Str y) = pure $ toInt (x > y)
gt (Ch x) (Ch y) = pure $ toInt (x > y)
gt (Db x) (Db y) = pure $ toInt (x > y)
gt _ _ = Nothing

doubleOp : (Double -> Double) -> Vect 1 (NF vars) -> Maybe (NF vars)
doubleOp f [NPrimVal fc (Db x)] = Just (NPrimVal fc (Db (f x)))
doubleOp f _ = Nothing

doubleExp : Vect 1 (NF vars) -> Maybe (NF vars)
doubleExp = doubleOp exp

doubleLog : Vect 1 (NF vars) -> Maybe (NF vars)
doubleLog = doubleOp log

doubleSin : Vect 1 (NF vars) -> Maybe (NF vars)
doubleSin = doubleOp sin

doubleCos : Vect 1 (NF vars) -> Maybe (NF vars)
doubleCos = doubleOp cos

doubleTan : Vect 1 (NF vars) -> Maybe (NF vars)
doubleTan = doubleOp tan

doubleASin : Vect 1 (NF vars) -> Maybe (NF vars)
doubleASin = doubleOp asin

doubleACos : Vect 1 (NF vars) -> Maybe (NF vars)
doubleACos = doubleOp acos

doubleATan : Vect 1 (NF vars) -> Maybe (NF vars)
doubleATan = doubleOp atan

doubleSqrt : Vect 1 (NF vars) -> Maybe (NF vars)
doubleSqrt = doubleOp sqrt

doubleFloor : Vect 1 (NF vars) -> Maybe (NF vars)
doubleFloor = doubleOp floor

doubleCeiling : Vect 1 (NF vars) -> Maybe (NF vars)
doubleCeiling = doubleOp ceiling

-- Only reduce for concrete values
believeMe : Vect 3 (NF vars) -> Maybe (NF vars)
believeMe [_, _, val@(NDCon _ _ _ _ _)] = Just val
believeMe [_, _, val@(NTCon _ _ _ _ _)] = Just val
believeMe [_, _, val@(NPrimVal _ _)] = Just val
believeMe [_, _, NType fc] = Just (NType fc)
believeMe [_, _, val] = Nothing

constTy : Constant -> Constant -> Constant -> ClosedTerm
constTy a b c
    = let arr = fnType emptyFC in
    PrimVal emptyFC a `arr` (PrimVal emptyFC b `arr` PrimVal emptyFC c)

constTy3 : Constant -> Constant -> Constant -> Constant -> ClosedTerm
constTy3 a b c d
    = let arr = fnType emptyFC in
    PrimVal emptyFC a `arr`
         (PrimVal emptyFC b `arr`
             (PrimVal emptyFC c `arr` PrimVal emptyFC d))

predTy : Constant -> Constant -> ClosedTerm
predTy a b = let arr = fnType emptyFC in
             PrimVal emptyFC a `arr` PrimVal emptyFC b

arithTy : Constant -> ClosedTerm
arithTy t = constTy t t t

cmpTy : Constant -> ClosedTerm
cmpTy t = constTy t t IntType

floatTy : ClosedTerm
floatTy = predTy FloatType FloatType

doubleTy : ClosedTerm
doubleTy = predTy DoubleType DoubleType

believeMeTy : ClosedTerm
believeMeTy
    = Bind emptyFC (UN "a") (Pi emptyFC erased Explicit (TType emptyFC)) $
      Bind emptyFC (UN "b") (Pi emptyFC erased Explicit (TType emptyFC)) $
      Bind emptyFC (UN "x") (Pi emptyFC top Explicit (Local emptyFC Nothing _ (Later First))) $
      Local emptyFC Nothing _ (Later First)

crashTy : ClosedTerm
crashTy
    = Bind emptyFC (UN "a") (Pi emptyFC erased Explicit (TType emptyFC)) $
      Bind emptyFC (UN "msg") (Pi emptyFC top Explicit (PrimVal emptyFC StringType)) $
      Local emptyFC Nothing _ (Later First)

castTo : Constant -> Vect 1 (NF vars) -> Maybe (NF vars)
castTo IntType = castInt
castTo Int8Type = castInt8
castTo Int16Type = castInt16
castTo Int32Type = castInt32
castTo Int64Type = castInt64
castTo IntegerType = castInteger
castTo Bits8Type = castBits8
castTo Bits16Type = castBits16
castTo Bits32Type = castBits32
castTo Bits64Type = castBits64
castTo StringType = castString
castTo CharType = castChar
castTo FloatType = castFloat
castTo DoubleType = castDouble
castTo _ = const Nothing

export
getOp : {0 arity : Nat} -> PrimFn arity ->
        {vars : List Name} -> Vect arity (NF vars) -> Maybe (NF vars)
getOp (Add ty) = binOp add
getOp (Sub ty) = binOp sub
getOp (Mul ty) = binOp mul
getOp (Div ty) = binOp div
getOp (Mod ty) = binOp mod
getOp (Neg ty) = unaryOp neg
getOp (ShiftL ty) = binOp shiftl
getOp (ShiftR ty) = binOp shiftr

getOp (BAnd ty) = binOp band
getOp (BOr ty) = binOp bor
getOp (BXOr ty) = binOp bxor

getOp (LT ty) = binOp lt
getOp (LTE ty) = binOp lte
getOp (EQ ty) = binOp eq
getOp (GTE ty) = binOp gte
getOp (GT ty) = binOp gt

getOp StrLength = strLength
getOp StrHead = strHead
getOp StrTail = strTail
getOp StrIndex = strIndex
getOp StrCons = strCons
getOp StrAppend = strAppend
getOp StrReverse = strReverse
getOp StrSubstr = strSubstr

getOp DoubleExp = doubleExp
getOp DoubleLog = doubleLog
getOp DoubleSin = doubleSin
getOp DoubleCos = doubleCos
getOp DoubleTan = doubleTan
getOp DoubleASin = doubleASin
getOp DoubleACos = doubleACos
getOp DoubleATan = doubleATan
getOp DoubleSqrt = doubleSqrt
getOp DoubleFloor = doubleFloor
getOp DoubleCeiling = doubleCeiling

getOp (Cast _ y) = castTo y
getOp BelieveMe = believeMe

getOp _ = const Nothing

prim : String -> Name
prim str = UN $ "prim__" ++ str

export
opName : {0 arity : Nat} -> PrimFn arity -> Name
opName (Add ty) = prim $ "add_" ++ show ty
opName (Sub ty) = prim $ "sub_" ++ show ty
opName (Mul ty) = prim $ "mul_" ++ show ty
opName (Div ty) = prim $ "div_" ++ show ty
opName (Mod ty) = prim $ "mod_" ++ show ty
opName (Neg ty) = prim $ "negate_" ++ show ty
opName (ShiftL ty) = prim $ "shl_" ++ show ty
opName (ShiftR ty) = prim $ "shr_" ++ show ty
opName (BAnd ty) = prim $ "and_" ++ show ty
opName (BOr ty) = prim $ "or_" ++ show ty
opName (BXOr ty) = prim $ "xor_" ++ show ty
opName (LT ty) = prim $ "lt_" ++ show ty
opName (LTE ty) = prim $ "lte_" ++ show ty
opName (EQ ty) = prim $ "eq_" ++ show ty
opName (GTE ty) = prim $ "gte_" ++ show ty
opName (GT ty) = prim $ "gt_" ++ show ty
opName StrLength = prim "strLength"
opName StrHead = prim "strHead"
opName StrTail = prim "strTail"
opName StrIndex = prim "strIndex"
opName StrCons = prim "strCons"
opName StrAppend = prim "strAppend"
opName StrReverse = prim "strReverse"
opName StrSubstr = prim "strSubstr"
opName FloatExp = prim "floatExp"
opName FloatLog = prim "floatLog"
opName FloatSin = prim "floatSin"
opName FloatCos = prim "floatCos"
opName FloatTan = prim "floatTan"
opName FloatASin = prim "floatASin"
opName FloatACos = prim "floatACos"
opName FloatATan = prim "floatATan"
opName FloatSqrt = prim "floatSqrt"
opName FloatFloor = prim "floatFloor"
opName FloatCeiling = prim "floatCeiling"
opName DoubleExp = prim "doubleExp"
opName DoubleLog = prim "doubleLog"
opName DoubleSin = prim "doubleSin"
opName DoubleCos = prim "doubleCos"
opName DoubleTan = prim "doubleTan"
opName DoubleASin = prim "doubleASin"
opName DoubleACos = prim "doubleACos"
opName DoubleATan = prim "doubleATan"
opName DoubleSqrt = prim "doubleSqrt"
opName DoubleFloor = prim "doubleFloor"
opName DoubleCeiling = prim "doubleCeiling"
opName (Cast x y) = prim $ "cast_" ++ show x ++ show y
opName BelieveMe = prim $ "believe_me"
opName Crash = prim $ "crash"

integralTypes : List Constant
integralTypes = [ IntType
                , Int8Type
                , Int16Type
                , Int32Type
                , Int64Type
                , IntegerType
                , Bits8Type
                , Bits16Type
                , Bits32Type
                , Bits64Type
                ]

numTypes : List Constant
numTypes = integralTypes ++ [FloatType, DoubleType]

primTypes : List Constant
primTypes = numTypes ++ [StringType, CharType]

export
allPrimitives : List Prim
allPrimitives =
    map (\t => MkPrim (Add t) (arithTy t) isTotal)     numTypes ++
    map (\t => MkPrim (Sub t) (arithTy t) isTotal)     numTypes ++
    map (\t => MkPrim (Mul t) (arithTy t) isTotal)     numTypes ++
    map (\t => MkPrim (Neg t) (predTy t t) isTotal)    numTypes ++
    map (\t => MkPrim (Div t) (arithTy t) notCovering) numTypes ++
    map (\t => MkPrim (Mod t) (arithTy t) notCovering) integralTypes ++

    map (\t => MkPrim (ShiftL t) (arithTy t) isTotal)  integralTypes ++
    map (\t => MkPrim (ShiftR t) (arithTy t) isTotal)  integralTypes ++
    map (\t => MkPrim (BAnd t) (arithTy t) isTotal)    integralTypes ++
    map (\t => MkPrim (BOr t) (arithTy t) isTotal)     integralTypes ++
    map (\t => MkPrim (BXOr t) (arithTy t) isTotal)    integralTypes ++

    map (\t => MkPrim (LT t) (cmpTy t) isTotal)  primTypes ++
    map (\t => MkPrim (LTE t) (cmpTy t) isTotal) primTypes ++
    map (\t => MkPrim (EQ t) (cmpTy t) isTotal)  primTypes ++
    map (\t => MkPrim (GTE t) (cmpTy t) isTotal) primTypes ++
    map (\t => MkPrim (GT t) (cmpTy t) isTotal)  primTypes ++

    [MkPrim StrLength (predTy StringType IntType) isTotal,
     MkPrim StrHead (predTy StringType CharType) notCovering,
     MkPrim StrTail (predTy StringType StringType) notCovering,
     MkPrim StrIndex (constTy StringType IntType CharType) notCovering,
     MkPrim StrCons (constTy CharType StringType StringType) isTotal,
     MkPrim StrAppend (arithTy StringType) isTotal,
     MkPrim StrReverse (predTy StringType StringType) isTotal,
     MkPrim StrSubstr (constTy3 IntType IntType StringType StringType) isTotal,
     MkPrim BelieveMe believeMeTy isTotal,
     MkPrim Crash crashTy notCovering] ++

    [MkPrim FloatExp floatTy isTotal,
     MkPrim FloatLog floatTy isTotal,
     MkPrim FloatSin floatTy isTotal,
     MkPrim FloatCos floatTy isTotal,
     MkPrim FloatTan floatTy isTotal,
     MkPrim FloatASin floatTy isTotal,
     MkPrim FloatACos floatTy isTotal,
     MkPrim FloatATan floatTy isTotal,
     MkPrim FloatSqrt floatTy isTotal,
     MkPrim FloatFloor floatTy isTotal,
     MkPrim FloatCeiling floatTy isTotal] ++

    [MkPrim DoubleExp doubleTy isTotal,
     MkPrim DoubleLog doubleTy isTotal,
     MkPrim DoubleSin doubleTy isTotal,
     MkPrim DoubleCos doubleTy isTotal,
     MkPrim DoubleTan doubleTy isTotal,
     MkPrim DoubleASin doubleTy isTotal,
     MkPrim DoubleACos doubleTy isTotal,
     MkPrim DoubleATan doubleTy isTotal,
     MkPrim DoubleSqrt doubleTy isTotal,
     MkPrim DoubleFloor doubleTy isTotal,
     MkPrim DoubleCeiling doubleTy isTotal] ++

    -- support all combinations of primitive casts with the following
    -- exceptions: String -> Char, Double -> Char, Char -> Double
    [ MkPrim (Cast t1 t2) (predTy t1 t2) isTotal
    | t1 <- primTypes
    , t2 <- primTypes
    , t1 /= t2                         &&
      (t1,t2) /= (StringType,CharType) &&
      (t1,t2) /= (FloatType,CharType)  &&
      (t1,t2) /= (CharType,FloatType) &&
      (t1,t2) /= (DoubleType,CharType) &&
      (t1,t2) /= (CharType,DoubleType)
    ]
