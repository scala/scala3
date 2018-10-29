---
layout: doc-page
title: "Witness as a Modifier"
---

There are some uses of implicits that cannot be expressed directly by witness definitions. In particular, we are lacking analogues of abstract implicit definitions and implicit aliases. The gap can be filled by allowing `witness` to be used as a modifier for objects, vals and defs, with the same meaning as the `implicit` modifier. Arguably, `witness` is a more suitable name for these kinds of definitions than `implicit`.

## Abstract and Alias Witnesses

As an example for an abstract witness consider the following fragment that's derived from Scala's Tasty extractor framework:
```scala
trait TastyAPI {
  type Symbol
  trait SymDeco {
    def name(this sym: Symbol): Name
    def tpe(this sym: Symbol): Type
  }
  witness def symDeco: SymDeco
}
```
An example of a witness alias would be an implementation of `symDeco` in terms of some internal compiler structure:
```scala
trait TastyImpl extends TastyAPI {
  witness val symDeco: SymDeco = compilerSymOps
}
```
As is the case for `implicit` value definitions, the result type of a witness value is mandatory unless the definition occurs as a statement in a block.

## Translation Scheme

Witness-as-a-modifier can be seen as a more low-level and general syntax layer than witness definitions. The latter can be translated into the former.

The translation rule for monomorphic unconditional witnesses is straightforward:
```
        witness <id> for <parents> <body>
  -->
        witness object <id> extends <parents> <body>
```
The translation rule for parameterized or conditional witnesses is more involved:
```
        witness <id> [<tparams>] with <params> for <parents> <body>
  -->
        class <id'> [<tparams>].( <witness-params> ) extends <parents> <body>
        witness def id [<tparams>].( <witness-params> ): <result-type> =
          new <id'> [<tparam-refs>]
  where
        <id'>              is a fresh, compiler generated name
        <witness-params>   is <params> with every parameter prefixed by `witness`
        <result-type>      is derived from <parents> by dropping all value parameters
        <tparam-refs>      is a list of type identifiers referring to each type parameter in <tparams>
```

## Syntax

```
LocalModifier   ::=  ...
                  |  witness
```