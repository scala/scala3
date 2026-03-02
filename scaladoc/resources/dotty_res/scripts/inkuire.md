# Inkuire

Inkuire is a Hoogle-like search engine for Scala 3 (and Kotlin).

# Usage

To include Inkuire in scaladoc one should add `-Ygenerate-inkuire` flag. This will allow the usage of Inkuire for generated Scaladoc with project sources and available sources from external-mappings.

The Inkuire worker works in Scaladoc searchbar. It is triggered once an input containing `=>` is detected in searchbar. There is 1s debounce on searches with Inkuire.

## Generated Files

When including `-Ygenerate-inkuire` flag database for project sources and a config should be generated. Which are namely files: `inkuire-db.json` and `scripts/inkuire-config.json`. Config file includes addresses of possible inkuire-db files. There always is at least one - generated one. But also links for external mappings are addes on relative path `../inkuire-db.json`.

`inkuire-db.json` contains a json with Inkuire engine's representation of:
- types (or rather classes and objects for now)
- functions
- implicit conversions


## Code

The source code is available [here](https://github.com/VirtusLab/Inkuire).

Important parts are:
- engineCommon - provides most of the logic for search engine
- engineJs - provides the way of using Inkuire as a Web Worker

## Integration with Scaladoc

Since Inkuire has quite a lot of dependencies it's sources cannot be easily integrated into Scaladoc.
That is why Inkuire is included as a resource, namely `inkuire.js` file and loaded as Web Worker.

Web worker accepts String messages. Each message should be a requested signature.
Web worker can send different messages:
- engine_ready - sent after database has been loaded
- new_query - sent once a new signature is accepted
- query_ended(`msg`) - After processing a single signature has finished. `msg` is optional and contains an error to be displayed.
- json of format [`ResultFormat`](https://github.com/VirtusLab/Inkuire/blob/68d1e0bb2732deda714de9cfca3fe45f75fb5239/engineCommon/shared/src/main/scala/org/virtuslab/inkuire/engine/common/model/OutputFormat.scala#L12) - resulting function found with some additional information, like package location and documentation link.

## Input format

Signature format accepted by Inkuire is pretty much a Scala curried function. With some minor changes:
- Types with names as single letters or single letters with digits are considered by default type variables. But other type variables can be declared with [polymorphic function types syntax](https://nightly.scala-lang.org/docs/reference/new-types/polymorphic-function-types.html).
- `_` is treated as sort of wildcard, so matches to any type on any position. So searching for any one-argument function from `Int` can be done like this: `Int => _`.

Some example signatures with expected (not exclusive)results:
- `Seq[Int] => (Int => Long) => Seq[Long]` -> `IterableOps.map`
- `(A, B) => A` -> `Product2._1`
- `Set[Long] => Long => Boolean` -> `SetOps.contains`
- `BigDecimal => Byte` -> `ScalaNumericAnyConversions.toByte`
- `Int => Long => Int` -> `Function.const`
- `String => Int => Char` -> `StringOps.apply`

## Signature processing

Some improvements done on engine side:
- first argument can be interpreted as a receiver. More specifically there is no difference between a function on type `A` and a function with the first parameter as `A` (provided other parameters and return type are the same).
- permutating arguments. This may have to be dropped/limited in the future. But for now every permutation of arguments can be matched, so for example: `Int => String => String` matches `Function.const`, even though the arguments are switched.