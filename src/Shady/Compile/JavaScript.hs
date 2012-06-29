{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables, TypeOperators #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.Compile.JavaScript
-- Copyright   :  (c) Sean Seefried 2012
-- License     :  AGPLv3
--
-- Maintainer  :  sean.seefried@gmail.com
-- Stability   :  experimental
--
-- Compile Shady language to JavaScript.
--
-- The generated code requires a companion library @shady-op.js@.
-- This library defines a JavaScript \"module\" called @ShadyOp@
-- which contains a number of curried functions implementing
-- Shady language constructs.
--
----------------------------------------------------------------------
module Shady.Compile.JavaScript (
  compile
) where

--
-- A Note on the Implementation
-- ----------------------------
--
-- The companion library @shady-op.js@ is integral to the simplicity of
-- the implementation of 'compile'. Each of the operators of the Shady
-- language can be implemented as a curried JavaScript function.
--
-- If JavaScript were not a language with higher-order functions the
-- implementation would have had take into account the arity of operators
-- and ensure that they were fully applied to expressions before
-- translating them to a JavaScript function invocation.
--
-- Fortunately, we don't have to worry about this. If Haskell
-- type checks a given Shady expression we can be guaranteed that
-- a correct JavaScript expression will be produced.
--
import Language.JavaScript.AST
import Shady.Language.Exp
import Shady.Language.Operator
import Data.Foldable
import Text.PrettyPrint.Leijen.DocExpr

-- FIXME: Remove: Imports for tests
import Control.Applicative hiding ((<*))
import Language.JavaScript.Pretty
import Text.PrettyPrint.Leijen
import Data.Boolean ((<*))
import Shady.Complex

--
-- FIXME: Interesting question. How do we prevent compilation of type 'E (Sampler n)'
-- We cannot create a new class, say 'HasJSType', as there are constraints of
-- class 'HasType' (not 'HasJSType') inside the constructors of the 'E a' data type,
-- But we would need to add constraints for class 'HasJSType' in all the functions below.
-- For now we throw an error if someone uses the 'Texture' operator (the only constructor
-- in the 'Op' type to include a reference to the 'Sampler n' type.)
--
-- Otherwise perhaps we implement 'Texture' somehow?
--

--
-- | Compile takes a Shady expression and converts it to a JavaScript function of
--   equivalent functionality. In JavaScript vectors are represented as arrays, and pairs
--   are represented as objects with two members, @fst@ and @snd@, corresponding to the
--   first and second components of the pair.
--
--   e.g. (with 'Shady.Language.Exp' and 'Language.JavaScript.Pretty' imported)
--
--
--   @pretty . compile . toE $ (vec2 (1 :: E (Vec1 Float)) 2, vec3 (4 :: E (Vec1 Float)) 5 6)@
--
--   evaluates to
--
--  @ShadyOp.pair([1.0, 2.0])([4.0, 5.0, 6.0])@
--
--  which when evaluated produces the same object as the following JavaScript literal:
--
--  @{ fst: [1,2], snd: [4,5,6]}@
--
compile :: HasType a => E a -> JSExpression
compile e = case e of
  Op op     -> compileOp op
  Var v     -> toVar v
  f :^ a    -> JSExpressionInvocation (compile f) (JSInvocation [compile a])
  Lam v' e' -> compileLam v' e'

compileLam :: (HasType a, HasType b) => V a -> E b -> JSExpression
compileLam v e = JSExpressionLiteral $ JSLiteralFunction $
                 JSFunctionLiteral Nothing [toJSName v] $ JSFunctionBody [] [return e]
  where
    return e = JSStatementDisruptive . JSDSReturn . JSReturnStatement . Just . compile $ e

compileOp :: forall a. HasType a => Op a -> JSExpression
compileOp op = case op of
  Lit l      -> literalToJSExpression l
  UniformV (VectorT n _) -> JSExpressionInvocation
                              (shadyOp "uniformV")
                              (JSInvocation [JSExpressionLiteral . JSLiteralNumber .
                               JSNumber . fromIntegral $ natToZ n])
  Swizzle is -> compileSwizzle is
  op' -> shadyOp $ case op' of
           And       -> "and"
           Or        -> "or"
           EqualV _  -> "equalV"
           Equal     -> "equal"
           Lt _      -> "lt"
           Le _      -> "le"
           Min       -> "min"
           Max       -> "max"
           Add       -> "add"
           Sub       -> "sub"
           Mul       -> "mul"
           Quot      -> "quot"
           Rem       -> "rem"
           Div       -> "div"
           Mod       -> "mod"
           Divide    -> "divide"
           Dot       -> "dot"
           Not       -> "not"
           Negate    -> "negate"
           Recip     -> "recip"
           Abs       -> "abs"
           Sqrt      -> "sqrt"
           Exp       -> "exp"
           Log       -> "log"
           Sin       -> "sin"
           Cos       -> "cos"
           Asin      -> "asin"
           Acos      -> "acos"
           Atan      -> "atan"
           Asinh     -> error "compileOp: Asinh not implemented"
           Acosh     -> error "compileOp: Acosh not implemented"
           Atanh     -> error "compileOp: Atanh not implemented"
           Truncate  -> "truncate"
           Round     -> "round"
           Ceiling   -> "ceiling"
           Floor     -> "floor"
           FMod      -> "fmod"
           VVec2     -> "vvec2"
           VVec3     -> "vvec3"
           VVec4     -> "vvec4"
           AllV      -> "allV"
           AnyV      -> "anyV"
           Unit      -> "unit"
           Pair      -> "pair"
           Fst       -> "fst"
           Snd       -> "snd"
           If        -> "ifOp"
           Cat _ _ _ -> "cat"
           Scale     -> "scale"
           Texture _ -> error "compileOp: Texture constructor not handled in JavaScript compiler"

literalToJSExpression :: forall a. HasType a => a -> JSExpression
literalToJSExpression v = case (typeT :: Type a) of
  VecT _    -> JSExpressionLiteral $ vecToJSLiteral v
  UnitT     -> JSExpressionLiteral $ JSLiteralBool False -- represent unit as 'false' in JavaScript
  t :*:  t' -> pair v
  _ :->: _ -> error "literalToJSExpression: function literals not handled"
  where
    pair (a,b) = JSExpressionInvocation
                   (JSExpressionInvocation
                     (shadyOp "pair")
                     (JSInvocation [literalToJSExpression a]))
                   (JSInvocation [literalToJSExpression b])

compileSwizzle :: forall n m. Vec n (Index m) -> JSExpression
compileSwizzle is = JSExpressionInvocation (shadyOp "swizzle")
                      (JSInvocation [JSExpressionLiteral indices])
  where
    is' :: [Index m]
    is' = toList is
    is'' :: [Int]
    is'' = map indexToZ is'
    indices :: JSLiteral
    indices = JSLiteralArray . JSArrayLiteral .
              map (JSExpressionLiteral . JSLiteralNumber .
                   JSNumber . fromIntegral) $ is''

compileLit :: RealFrac a => Op (Vec n a) -> JSExpression
compileLit (Lit v) =
  JSExpressionLiteral . JSLiteralArray . JSArrayLiteral .
  map (JSExpressionLiteral . JSLiteralNumber . JSNumber . fromRational . toRational) .
  toList $ v

shadyOp :: String -> JSExpression
shadyOp name = JSExpressionRefinement (toVar' "ShadyOp") (JSProperty . toJSName' $ name)

vecToJSLiteral :: forall n a. (IsNat n, IsScalar a) =>  Vec n a -> JSLiteral
vecToJSLiteral v = JSLiteralArray . JSArrayLiteral . map (JSExpressionLiteral . litFun) $
                   toList v
  where
    litFun :: IsScalar a => a -> JSLiteral
    litFun =  case (scalarT :: ScalarT a) of
      Bool  -> JSLiteralBool
      Int   -> JSLiteralNumber . JSNumber . fromIntegral
      Float -> JSLiteralNumber . JSNumber . fromRational . toRational

toVar' :: String -> JSExpression
toVar' v = JSExpressionName . toJSName' $ v

toVar :: V a -> JSExpression
toVar = toVar' . varName

-- FIXME: Convert our names to valid JavaScript names.
toJSName' :: String  -> JSName
toJSName' v = fromRight . jsName $ v
  where
    fromRight :: Either a b -> b
    fromRight (Right b) = b
    fromRight (Left a)  = error "Wasn't expecting left"

toJSName :: V a -> JSName
toJSName = toJSName' . varName

indexToZ :: Index m -> Int
indexToZ (Index pf _) = pfToZ pf
  where
    pfToZ :: (n :<: m) -> Int
    pfToZ ZLess      = 0
    pfToZ (SLess pf) = 1 + pfToZ pf

------------------------------

-- Tests

testLit1 = (pretty . compile) (lit $ (pure 1,pure 2)
              :: E (Vec (S (S Z)) Int, Vec (S (S (S (S Z)))) Float))

testExp1 = pretty . compile $ (pairE (1 + sin 2 + 3) 2 :: E (Vec1 Float, Vec (S (S Z)) Float))

-- A simple image
testExp2 = pretty . compile . toE $ e
  where e :: ComplexE Float -> BoolE
        e (x :+ y) = x*x + y*y <* 20.0

testExp3 = pretty . compile . toE $ e
  where e :: E (Vec1 Int) -> E (Vec1 Int)
        e x = x `div` 3