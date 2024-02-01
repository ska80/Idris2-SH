module Core.TT.Primitive

import Core.Name

import Data.String
import Data.Vect

import Decidable.Equality

import Idris.Pretty.Annotations

import Libraries.Data.Ordering.Extra
import Libraries.Data.String.Extra -- compatibility
import Libraries.Text.PrettyPrint.Prettyprinter

%default total

public export
data PrimType
    = IntType
    | Int8Type
    | Int16Type
    | Int32Type
    | Int64Type
    | IntegerType
    | Bits8Type
    | Bits16Type
    | Bits32Type
    | Bits64Type
    | StringType
    | CharType
    | Float32Type
    | Float64Type
    | WorldType

%name PrimType pty

public export
data Constant
    = I   Int
    | I8  Int8
    | I16 Int16
    | I32 Int32
    | I64 Int64
    | BI  Integer
    | B8  Bits8
    | B16 Bits16
    | B32 Bits32
    | B64 Bits64
    | Str String
    | Ch  Char
    | F32 Float32
    | F64 Float64
    | PrT PrimType
    | WorldVal

%name Constant cst

export
isConstantType : Name -> Maybe PrimType
isConstantType (UN (Basic n)) = case n of
  "Int"     => Just IntType
  "Int8"    => Just Int8Type
  "Int16"   => Just Int16Type
  "Int32"   => Just Int32Type
  "Int64"   => Just Int64Type
  "Integer" => Just IntegerType
  "Bits8"   => Just Bits8Type
  "Bits16"  => Just Bits16Type
  "Bits32"  => Just Bits32Type
  "Bits64"  => Just Bits64Type
  "String"  => Just StringType
  "Char"    => Just CharType
  "Float32" => Just Float32Type
  "Float64" => Just Float64Type
  "%World"  => Just WorldType
  _ => Nothing
isConstantType _ = Nothing

export
isPrimType : Constant -> Bool
isPrimType (PrT _) = True
isPrimType _       = False

export
primTypeEq : (x, y : PrimType) -> Maybe (x = y)
primTypeEq IntType IntType = Just Refl
primTypeEq Int8Type Int8Type = Just Refl
primTypeEq Int16Type Int16Type = Just Refl
primTypeEq Int32Type Int32Type = Just Refl
primTypeEq Int64Type Int64Type = Just Refl
primTypeEq IntegerType IntegerType = Just Refl
primTypeEq StringType StringType = Just Refl
primTypeEq CharType CharType = Just Refl
primTypeEq Float32Type Float32Type = Just Refl
primTypeEq Float64Type Float64Type = Just Refl
primTypeEq WorldType WorldType = Just Refl
primTypeEq _ _ = Nothing

export
constantEq : (x, y : Constant) -> Maybe (x = y)
constantEq (I x) (I y) = case decEq x y of
                              Yes Refl => Just Refl
                              No contra => Nothing
constantEq (I8 x) (I8 y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (I16 x) (I16 y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (I32 x) (I32 y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (I64 x) (I64 y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (B8 x) (B8 y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (B16 x) (B16 y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (B32 x) (B32 y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (B64 x) (B64 y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (BI x) (BI y) = case decEq x y of
                                Yes Refl => Just Refl
                                No contra => Nothing
constantEq (Str x) (Str y) = case decEq x y of
                                  Yes Refl => Just Refl
                                  No contra => Nothing
constantEq (Ch x) (Ch y) = case decEq x y of
                                Yes Refl => Just Refl
                                No contra => Nothing
constantEq (F32 x) (F32 y) = Nothing -- no DecEq for Float32!
constantEq (F64 x) (F64 y) = Nothing -- no DecEq for Float64!
constantEq (PrT x) (PrT y) = (\xy => cong PrT xy) <$> primTypeEq x y
constantEq WorldVal WorldVal = Just Refl

constantEq _ _ = Nothing

export
Show PrimType where
  show IntType = "Int"
  show Int8Type = "Int8"
  show Int16Type = "Int16"
  show Int32Type = "Int32"
  show Int64Type = "Int64"
  show IntegerType = "Integer"
  show Bits8Type = "Bits8"
  show Bits16Type = "Bits16"
  show Bits32Type = "Bits32"
  show Bits64Type = "Bits64"
  show StringType = "String"
  show CharType = "Char"
  show Float32Type = "Float32"
  show Float64Type = "Float64"
  show WorldType = "%World"

export
Show Constant where
  show (I x) = show x
  show (I8 x) = show x
  show (I16 x) = show x
  show (I32 x) = show x
  show (I64 x) = show x
  show (BI x) = show x
  show (B8 x) = show x
  show (B16 x) = show x
  show (B32 x) = show x
  show (B64 x) = show x
  show (Str x) = show x
  show (Ch x) = show x
  show (F32 x) = show x
  show (F64 x) = show x
  show (PrT x) = show x
  show WorldVal = "%MkWorld"

Pretty IdrisSyntax PrimType where
  pretty c = annotate (TCon Nothing) $ case c of
    IntType => "Int"
    Int8Type => "Int8"
    Int16Type => "Int16"
    Int32Type => "Int32"
    Int64Type => "Int64"
    IntegerType => "Integer"
    Bits8Type => "Bits8"
    Bits16Type => "Bits16"
    Bits32Type => "Bits32"
    Bits64Type => "Bits64"
    StringType => "String"
    CharType => "Char"
    Float32Type => "Float32"
    Float64Type => "Float64"
    WorldType => "%World"

export
Pretty IdrisSyntax Constant where
  pretty (PrT x) = pretty x
  pretty v = annotate (DCon Nothing) $ pretty0 $ show v


export
Eq PrimType where
  IntType == IntType = True
  Int8Type == Int8Type = True
  Int16Type == Int16Type = True
  Int32Type == Int32Type = True
  Int64Type == Int64Type = True
  IntegerType == IntegerType = True
  Bits8Type == Bits8Type = True
  Bits16Type == Bits16Type = True
  Bits32Type == Bits32Type = True
  Bits64Type == Bits64Type = True
  StringType == StringType = True
  CharType == CharType = True
  Float32Type == Float32Type = True
  Float64Type == Float64Type = True
  WorldType == WorldType = True
  _ == _ = False

export
Eq Constant where
  (I x) == (I y) = x == y
  (I8 x) == (I8 y) = x == y
  (I16 x) == (I16 y) = x == y
  (I32 x) == (I32 y) = x == y
  (I64 x) == (I64 y) = x == y
  (BI x) == (BI y) = x == y
  (B8 x) == (B8 y) = x == y
  (B16 x) == (B16 y) = x == y
  (B32 x) == (B32 y) = x == y
  (B64 x) == (B64 y) = x == y
  (Str x) == (Str y) = x == y
  (Ch x) == (Ch y) = x == y
  (F32 x) == (F32 y) = x == y
  (F64 x) == (F64 y) = x == y
  (PrT x) == (PrT y) = x == y
  WorldVal == WorldVal = True
  _ == _ = False

export
Ord PrimType where
  compare = compare `on` tag
    where
      tag : PrimType -> Int
      tag IntType     = 1
      tag Int8Type    = 2
      tag Int16Type   = 3
      tag Int32Type   = 4
      tag Int64Type   = 5
      tag IntegerType = 6
      tag Bits8Type   = 7
      tag Bits16Type  = 8
      tag Bits32Type  = 9
      tag Bits64Type  = 10
      tag StringType  = 11
      tag CharType    = 12
      tag Float32Type = 13
      tag Float64Type = 14
      tag WorldType   = 15

export
Ord Constant where
    I x `compare` I y = compare x y
    I8 x `compare` I8 y = compare x y
    I16 x `compare` I16 y = compare x y
    I32 x `compare` I32 y = compare x y
    I64 x `compare` I64 y = compare x y
    BI x `compare` BI y = compare x y
    B8 x `compare` B8 y = compare x y
    B16 x `compare` B16 y = compare x y
    B32 x `compare` B32 y = compare x y
    B64 x `compare` B64 y = compare x y
    Str x `compare` Str y = compare x y
    Ch x `compare` Ch y = compare x y
    F32 x `compare` F32 y = compare x y
    F64 x `compare` F64 y = compare x y
    PrT x `compare` PrT y = compare x y
    compare x y = compare (tag x) (tag y)
      where
        tag : Constant -> Int
        tag (I _) = 0
        tag (I8 _) = 1
        tag (I16 _) = 2
        tag (I32 _) = 3
        tag (I64 _) = 4
        tag (BI _) = 5
        tag (B8 _) = 6
        tag (B16 _) = 7
        tag (B32 _) = 8
        tag (B64 _) = 9
        tag (Str _) = 10
        tag (Ch _) = 11
        tag (F32 _) = 12
        tag (F64 _) = 13
        tag (PrT _) = 14
        tag WorldVal = 15

-- for typecase
export
primTypeTag : PrimType -> Int
-- 1 = ->, 2 = Type
primTypeTag IntType = 3
primTypeTag IntegerType = 4
primTypeTag Bits8Type = 5
primTypeTag Bits16Type = 6
primTypeTag Bits32Type = 7
primTypeTag Bits64Type = 8
primTypeTag StringType = 9
primTypeTag CharType = 10
primTypeTag Float32Type = 11
primTypeTag Float64Type = 12
primTypeTag WorldType = 13
primTypeTag Int8Type = 14
primTypeTag Int16Type = 15
primTypeTag Int32Type = 16
primTypeTag Int64Type = 17

||| Precision of integral types.
public export
data Precision = P Int | Unlimited

%name Precision prec

export
Eq Precision where
  (P m) == (P n)         = m == n
  Unlimited == Unlimited = True
  _         == _         = False

export
Ord Precision where
  compare (P m) (P n)         = compare m n
  compare Unlimited Unlimited = EQ
  compare Unlimited _         = GT
  compare _         Unlimited = LT

-- so far, we only support limited precision
-- unsigned integers
public export
data IntKind = Signed Precision | Unsigned Int

public export
intKind : PrimType -> Maybe IntKind
intKind IntegerType = Just $ Signed Unlimited
intKind Int8Type    = Just . Signed   $ P 8
intKind Int16Type   = Just . Signed   $ P 16
intKind Int32Type   = Just . Signed   $ P 32
intKind Int64Type   = Just . Signed   $ P 64
intKind IntType     = Just . Signed   $ P 64
intKind Bits8Type   = Just $ Unsigned 8
intKind Bits16Type  = Just $ Unsigned 16
intKind Bits32Type  = Just $ Unsigned 32
intKind Bits64Type  = Just $ Unsigned 64
intKind _           = Nothing

public export
precision : IntKind -> Precision
precision (Signed p)   = p
precision (Unsigned p) = P p

-- All the internal operators, parameterised by their arity
public export
data PrimFn : Nat -> Type where
     Add : (ty : PrimType) -> PrimFn 2
     Sub : (ty : PrimType) -> PrimFn 2
     Mul : (ty : PrimType) -> PrimFn 2
     Div : (ty : PrimType) -> PrimFn 2
     Mod : (ty : PrimType) -> PrimFn 2
     Neg : (ty : PrimType) -> PrimFn 1
     ShiftL : (ty : PrimType) -> PrimFn 2
     ShiftR : (ty : PrimType) -> PrimFn 2

     BAnd : (ty : PrimType) -> PrimFn 2
     BOr : (ty : PrimType) -> PrimFn 2
     BXOr : (ty : PrimType) -> PrimFn 2

     LT  : (ty : PrimType) -> PrimFn 2
     LTE : (ty : PrimType) -> PrimFn 2
     EQ  : (ty : PrimType) -> PrimFn 2
     GTE : (ty : PrimType) -> PrimFn 2
     GT  : (ty : PrimType) -> PrimFn 2

     StrLength : PrimFn 1
     StrHead : PrimFn 1
     StrTail : PrimFn 1
     StrIndex : PrimFn 2
     StrCons : PrimFn 2
     StrAppend : PrimFn 2
     StrReverse : PrimFn 1
     StrSubstr : PrimFn 3

     Float32Exp : PrimFn 1
     Float32Log : PrimFn 1
     Float32Pow : PrimFn 2
     Float32Sin : PrimFn 1
     Float32Cos : PrimFn 1
     Float32Tan : PrimFn 1
     Float32ASin : PrimFn 1
     Float32ACos : PrimFn 1
     Float32ATan : PrimFn 1
     Float32Sqrt : PrimFn 1
     Float32Floor : PrimFn 1
     Float32Ceiling : PrimFn 1

     Float64Exp : PrimFn 1
     Float64Log : PrimFn 1
     Float64Pow : PrimFn 2
     Float64Sin : PrimFn 1
     Float64Cos : PrimFn 1
     Float64Tan : PrimFn 1
     Float64ASin : PrimFn 1
     Float64ACos : PrimFn 1
     Float64ATan : PrimFn 1
     Float64Sqrt : PrimFn 1
     Float64Floor : PrimFn 1
     Float64Ceiling : PrimFn 1

     Cast : PrimType -> PrimType -> PrimFn 1
     BelieveMe : PrimFn 3
     Crash : PrimFn 2

%name PrimFn f

export
Show (PrimFn arity) where
  show (Add ty) = "+" ++ show ty
  show (Sub ty) = "-" ++ show ty
  show (Mul ty) = "*" ++ show ty
  show (Div ty) = "/" ++ show ty
  show (Mod ty) = "%" ++ show ty
  show (Neg ty) = "neg " ++ show ty
  show (ShiftL ty) = "shl " ++ show ty
  show (ShiftR ty) = "shr " ++ show ty
  show (BAnd ty) = "and " ++ show ty
  show (BOr ty) = "or " ++ show ty
  show (BXOr ty) = "xor " ++ show ty
  show (LT ty) = "<" ++ show ty
  show (LTE ty) = "<=" ++ show ty
  show (EQ ty) = "==" ++ show ty
  show (GTE ty) = ">=" ++ show ty
  show (GT ty) = ">" ++ show ty
  show StrLength = "op_strlen"
  show StrHead = "op_strhead"
  show StrTail = "op_strtail"
  show StrIndex = "op_strindex"
  show StrCons = "op_strcons"
  show StrAppend = "++"
  show StrReverse = "op_strrev"
  show StrSubstr = "op_strsubstr"

  show Float32Exp = "op_float32Exp"
  show Float32Log = "op_float32Log"
  show Float32Pow = "op_float32Pow"
  show Float32Sin = "op_float32Sin"
  show Float32Cos = "op_float32Cos"
  show Float32Tan = "op_float32Tan"
  show Float32ASin = "op_float32ASin"
  show Float32ACos = "op_float32ACos"
  show Float32ATan = "op_float32ATan"
  show Float32Sqrt = "op_float32Sqrt"
  show Float32Floor = "op_float32Floor"
  show Float32Ceiling = "op_float32Ceiling"

  show Float64Exp = "op_float64Exp"
  show Float64Log = "op_float64Log"
  show Float64Pow = "op_float64Pow"
  show Float64Sin = "op_float64Sin"
  show Float64Cos = "op_float64Cos"
  show Float64Tan = "op_float64Tan"
  show Float64ASin = "op_float64ASin"
  show Float64ACos = "op_float64ACos"
  show Float64ATan = "op_float64ATan"
  show Float64Sqrt = "op_float64Sqrt"
  show Float64Floor = "op_float64Floor"
  show Float64Ceiling = "op_float64Ceiling"

  show (Cast x y) = "cast-" ++ show x ++ "-" ++ show y
  show BelieveMe = "believe_me"
  show Crash = "crash"

export
prettyOp : PrimFn arity -> Vect arity (Doc IdrisSyntax) -> Doc IdrisSyntax
prettyOp op@(Add ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "+" <++> v2
prettyOp op@(Sub ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "-" <++> v2
prettyOp op@(Mul ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "*" <++> v2
prettyOp op@(Div ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "`div`" <++> v2
prettyOp op@(Mod ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "`mod`" <++> v2
prettyOp op@(Neg ty) [v] = annotate (Fun $ UN $ Basic $ show op) "-" <++> v
prettyOp op@(ShiftL ty) [v1,v2] = annotate (Fun $ UN $ Basic $ show op) "shiftl" <++> v1 <++> v2
prettyOp op@(ShiftR ty) [v1,v2] = annotate (Fun $ UN $ Basic $ show op) "shiftr" <++> v1 <++> v2
prettyOp op@(BAnd ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "&&" <++> v2
prettyOp op@(BOr ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "||" <++> v2
prettyOp op@(BXOr ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "`xor`" <++> v2
prettyOp op@(LT ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "<" <++> v2
prettyOp op@(LTE ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "<=" <++> v2
prettyOp op@(EQ ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "==" <++> v2
prettyOp op@(GTE ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) ">=" <++> v2
prettyOp op@(GT ty) [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) ">" <++> v2
prettyOp op@StrLength [v] = annotate (Fun $ UN $ Basic $ show op) "length" <++> v
prettyOp op@StrHead [v] = annotate (Fun $ UN $ Basic $ show op) "head" <++> v
prettyOp op@StrTail [v] = annotate (Fun $ UN $ Basic $ show op) "tail" <++> v
prettyOp op@StrIndex [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "[" <+> v2 <+> annotate (Fun $ UN $ Basic $ show op) "]"
prettyOp op@StrCons [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "::" <++> v2
prettyOp op@StrAppend [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "++" <++> v2
prettyOp op@StrReverse [v] = annotate (Fun $ UN $ Basic $ show op) "reverse" <++> v
prettyOp op@StrSubstr [v1,v2,v3] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "[" <+> v2 <+> annotate (Fun $ UN $ Basic $ show op) "," <++> v3 <+> annotate (Fun $ UN $ Basic $ show op) "]"

prettyOp op@Float32Exp [v] = annotate (Fun $ UN $ Basic $ show op) "exp" <++> v
prettyOp op@Float32Log [v] = annotate (Fun $ UN $ Basic $ show op) "log" <++> v
prettyOp op@Float32Pow [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "`pow`" <++> v2
prettyOp op@Float32Sin [v] = annotate (Fun $ UN $ Basic $ show op) "sin" <++> v
prettyOp op@Float32Cos [v] = annotate (Fun $ UN $ Basic $ show op) "cos" <++> v
prettyOp op@Float32Tan [v] = annotate (Fun $ UN $ Basic $ show op) "tan" <++> v
prettyOp op@Float32ASin [v] = annotate (Fun $ UN $ Basic $ show op) "asin" <++> v
prettyOp op@Float32ACos [v] = annotate (Fun $ UN $ Basic $ show op) "acos" <++> v
prettyOp op@Float32ATan [v] = annotate (Fun $ UN $ Basic $ show op) "atan" <++> v
prettyOp op@Float32Sqrt [v] = annotate (Fun $ UN $ Basic $ show op) "sqrt" <++> v
prettyOp op@Float32Floor [v] = annotate (Fun $ UN $ Basic $ show op) "floor" <++> v
prettyOp op@Float32Ceiling [v] = annotate (Fun $ UN $ Basic $ show op) "ceiling" <++> v

prettyOp op@Float64Exp [v] = annotate (Fun $ UN $ Basic $ show op) "exp" <++> v
prettyOp op@Float64Log [v] = annotate (Fun $ UN $ Basic $ show op) "log" <++> v
prettyOp op@Float64Pow [v1,v2] = v1 <++> annotate (Fun $ UN $ Basic $ show op) "`pow`" <++> v2
prettyOp op@Float64Sin [v] = annotate (Fun $ UN $ Basic $ show op) "sin" <++> v
prettyOp op@Float64Cos [v] = annotate (Fun $ UN $ Basic $ show op) "cos" <++> v
prettyOp op@Float64Tan [v] = annotate (Fun $ UN $ Basic $ show op) "tan" <++> v
prettyOp op@Float64ASin [v] = annotate (Fun $ UN $ Basic $ show op) "asin" <++> v
prettyOp op@Float64ACos [v] = annotate (Fun $ UN $ Basic $ show op) "acos" <++> v
prettyOp op@Float64ATan [v] = annotate (Fun $ UN $ Basic $ show op) "atan" <++> v
prettyOp op@Float64Sqrt [v] = annotate (Fun $ UN $ Basic $ show op) "sqrt" <++> v
prettyOp op@Float64Floor [v] = annotate (Fun $ UN $ Basic $ show op) "floor" <++> v
prettyOp op@Float64Ceiling [v] = annotate (Fun $ UN $ Basic $ show op) "ceiling" <++> v

prettyOp op@(Cast x y) [v] = annotate (Fun $ UN $ Basic $ show op) "[" <+> pretty x <++> annotate (Fun $ UN $ Basic $ show op) "->" <++> pretty y <+> annotate (Fun $ UN $ Basic $ show op) "]" <++> v
prettyOp op@BelieveMe [v1,v2,v3] = annotate (Fun $ UN $ Basic $ show op) "believe_me" <++> v1 <++> v2 <++> v3
prettyOp op@Crash [v1,v2] = annotate (Fun $ UN $ Basic $ show op) "crash" <++> v1 <++> v2

export
primFnEq : PrimFn a1 -> PrimFn a2 -> Maybe (a1 = a2)
primFnEq (Add t1) (Add t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (Sub t1) (Sub t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (Mul t1) (Mul t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (Div t1) (Div t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (Mod t1) (Mod t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (Neg t1) (Neg t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (ShiftL t1) (ShiftL t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (ShiftR t1) (ShiftR t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (BAnd t1) (BAnd t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (BOr t1) (BOr t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (BXOr t1) (BXOr t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (LT t1) (LT t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (LTE t1) (LTE t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (EQ t1) (EQ t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (GTE t1) (GTE t2) = if t1 == t2 then Just Refl else Nothing
primFnEq (GT t1) (GT t2) = if t1 == t2 then Just Refl else Nothing
primFnEq StrLength StrLength = Just Refl
primFnEq StrHead StrHead = Just Refl
primFnEq StrTail StrTail = Just Refl
primFnEq StrIndex StrIndex = Just Refl
primFnEq StrCons StrCons = Just Refl
primFnEq StrAppend StrAppend = Just Refl
primFnEq StrReverse StrReverse = Just Refl
primFnEq StrSubstr StrSubstr = Just Refl

primFnEq Float32Exp Float32Exp = Just Refl
primFnEq Float32Log Float32Log = Just Refl
primFnEq Float32Pow Float32Pow = Just Refl
primFnEq Float32Sin Float32Sin = Just Refl
primFnEq Float32Cos Float32Cos = Just Refl
primFnEq Float32Tan Float32Tan = Just Refl
primFnEq Float32ASin Float32ASin = Just Refl
primFnEq Float32ACos Float32ACos = Just Refl
primFnEq Float32ATan Float32ATan = Just Refl
primFnEq Float32Sqrt Float32Sqrt = Just Refl
primFnEq Float32Floor Float32Floor = Just Refl
primFnEq Float32Ceiling Float32Ceiling = Just Refl

primFnEq Float64Exp Float64Exp = Just Refl
primFnEq Float64Log Float64Log = Just Refl
primFnEq Float64Pow Float64Pow = Just Refl
primFnEq Float64Sin Float64Sin = Just Refl
primFnEq Float64Cos Float64Cos = Just Refl
primFnEq Float64Tan Float64Tan = Just Refl
primFnEq Float64ASin Float64ASin = Just Refl
primFnEq Float64ACos Float64ACos = Just Refl
primFnEq Float64ATan Float64ATan = Just Refl
primFnEq Float64Sqrt Float64Sqrt = Just Refl
primFnEq Float64Floor Float64Floor = Just Refl
primFnEq Float64Ceiling Float64Ceiling = Just Refl

primFnEq (Cast f1 t1) (Cast f2 t2) = if f1 == f2 && t1 == t2 then Just Refl else Nothing
primFnEq BelieveMe BelieveMe = Just Refl
primFnEq Crash Crash = Just Refl
primFnEq _ _ = Nothing

export
primFnCmp : PrimFn a1 -> PrimFn a2 -> Ordering
primFnCmp (Add t1) (Add t2) = compare t1 t2
primFnCmp (Sub t1) (Sub t2) = compare t1 t2
primFnCmp (Mul t1) (Mul t2) = compare t1 t2
primFnCmp (Div t1) (Div t2) = compare t1 t2
primFnCmp (Mod t1) (Mod t2) = compare t1 t2
primFnCmp (Neg t1) (Neg t2) = compare t1 t2
primFnCmp (ShiftL t1) (ShiftL t2) = compare t1 t2
primFnCmp (ShiftR t1) (ShiftR t2) = compare t1 t2
primFnCmp (BAnd t1) (BAnd t2) = compare t1 t2
primFnCmp (BOr t1) (BOr t2) = compare t1 t2
primFnCmp (BXOr t1) (BXOr t2) = compare t1 t2
primFnCmp (LT t1) (LT t2) = compare t1 t2
primFnCmp (LTE t1) (LTE t2) = compare t1 t2
primFnCmp (EQ t1) (EQ t2) = compare t1 t2
primFnCmp (GTE t1) (GTE t2) = compare t1 t2
primFnCmp (GT t1) (GT t2) = compare t1 t2
primFnCmp (Cast f1 t1) (Cast f2 t2) = compare f1 f2 `thenCmp` compare t1 t2
primFnCmp f1 f2 = compare (tag f1) (tag f2)
  where
    tag : forall ar. PrimFn ar -> Int
    tag (Add _) = 0
    tag (Sub _) = 1
    tag (Mul _) = 2
    tag (Div _) = 3
    tag (Mod _) = 4
    tag (Neg _) = 5
    tag (ShiftL _) = 6
    tag (ShiftR _) = 7
    tag (BAnd _) = 8
    tag (BOr _) = 9
    tag (BXOr _) = 10
    tag (LT _) = 11
    tag (LTE _) = 12
    tag (EQ _) = 13
    tag (GTE _) = 14
    tag (GT _) = 15
    tag StrLength = 16
    tag StrHead = 17
    tag StrTail = 18
    tag StrIndex = 19
    tag StrCons = 20
    tag StrAppend = 21
    tag StrReverse = 22
    tag StrSubstr = 23

    tag Float32Exp = 24
    tag Float32Log = 25
    tag Float32Pow = 26
    tag Float32Sin = 27
    tag Float32Cos = 28
    tag Float32Tan = 29
    tag Float32ASin = 30
    tag Float32ACos = 31
    tag Float32ATan = 32
    tag Float32Sqrt = 33
    tag Float32Floor = 34
    tag Float32Ceiling = 35

    tag Float64Exp = 36
    tag Float64Log = 37
    tag Float64Pow = 38
    tag Float64Sin = 39
    tag Float64Cos = 40
    tag Float64Tan = 41
    tag Float64ASin = 42
    tag Float64ACos = 43
    tag Float64ATan = 44
    tag Float64Sqrt = 45
    tag Float64Floor = 46
    tag Float64Ceiling = 47

    tag (Cast _ _) = 48
    tag BelieveMe = 49
    tag Crash = 50
