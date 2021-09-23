---
layout: singlepage-overview
scala3: true
title: "Dropped: Weak Conformance - More Details"
---

To simplify the underlying type theory, Scala 3 drops the notion of
[*weak conformance*](https://www.scala-lang.org/files/archive/spec/2.13/03-types.html#weak-conformance)
altogether. Instead, it provides more flexibility when
assigning a type to a constant expression. The new rule is:

 - *If* a list of expressions `Es` appears as one of

     - the elements of a vararg parameter, or
     - the alternatives of an if-then-else or match expression, or
     - the body and catch results of a try expression,

- *and* all expressions have primitive numeric types, but they do not
   all have the same type,

- *then* the following is attempted:

     - the expressions `Es` are partitioned into `Int` constants on the
       one hand, and all other expressions on the other hand,
     - if all the other expressions have the same numeric type `T`
       (which can be one of `Byte`, `Short`, `Char`, `Int`, `Long`, `Float`,
       `Double`), possibly after widening, and if none of the `Int`
       literals would incur a loss of precision when converted to `T`,
       then they are thus converted (the other expressions are left
       unchanged regardless),
     - otherwise, the expressions `Es` are used unchanged.

    A loss of precision occurs for
    - an `Int -> Float` conversion of a constant
    `c` if `c.toFloat.toInt != c`
    - an `Int -> Byte` conversion of a constant
    `c` if `c.toByte.toInt != c`,
    - an `Int -> Short` conversion of a constant
    `c` if `c.toShort.toInt != c`.

### Examples

```scala
inline val b = 33
def f(): Int = b + 1
Array(b, 33, 5.5)      : Array[Double] // b is an inline val
Array(f(), 33, 5.5)    : Array[AnyVal] // f() is not a constant
Array(5, 11L)          : Array[Long]
Array(5, 11L, 5.5)     : Array[AnyVal] // Long and Double found
Array(1.0f, 2)         : Array[Float]
Array(1.0f, 1234567890): Array[AnyVal] // loss of precision
Array(b, 33, 'a')      : Array[Char]
Array(5.toByte, 11)    : Array[Byte]
```
