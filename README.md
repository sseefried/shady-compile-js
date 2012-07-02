# Shady to JavaScript

## Introduction

Shady is a language for writing 3D effects in. This package compiles Shady programs to
JavaScript.

## How it works

The module `Shady.Compile.JavaScript` exports a single function

```haskell
compile :: HasType a => E a -> JSExpression
```

Roughly the types that satisfy the `HasType` constraint are vectors of arbitrary length containing
`Float`s,`Int`s or `Bool`s and nested pairs of such vectors.

In JavaScript the types are represented as follows:

* vectors are arrays
* `Float`s and `Int`s are numbers. In JavaScript all numbers are 64-bit floating point values.
   Indexes into arrays must be whole numbers. i.e. satisfy the property `Math.floor(n) === n`
* Pairs are represented as objects with two properties `fst` and `snd`.

## Companion library: `shady-op.js`

The output `compile` function relies on a companion library, `shady-op.js` which exports an
object `ShadyOp` containing a number of properties of function value. 

e.g. `ShadyOp.pair` is used to pair two values together.

Ensure that `shady-op.js` is loaded and in scope whenever using the output of the Haskell `compile` function.

## Sample program

```haskell
import Shady.Language.Exp
import Shady.Compile.JavaScript 
import Language.JavaScript.Pretty

js :: String
js = show . pretty . compile . toE $ (vec2 (1 :: E (Vec1 Float)) 2, vec3 (4 :: E (Vec1 Float)) 5 6)

main = putStrLn js
```

evaluates to:

```javascript
ShadyOp.pair([1.0, 2.0])([4.0, 5.0, 6.0])
```

which is equivalent to the literal

```javascript
{ fst: [1.0, 2.0], snd: [4.0, 5.0, 6.0] }
```