---
layout: doc-page
title: Dropped: Weak Conformance - More Details
---

To simplify the underlying type theory, Dotty drops the notion of weak
conformance altogether. Instead, it provides more flexibility when
assigning a type to a constant expression. The new rule is:

 - If a list of expressions `Es` appears as one of

     - the elements of a vararg parameter, or
     - the alternatives of an if-then-else or match expression, or
     - the body and catch results of a try expression,


   and all expressions have primitive numeric types, but they do not
   all have the same type, then the following is attempted:
   
     - the expressions `Es` are partitioned into `Int` literals on the
       one hand, and all other expressions on the other hand
     - if all the other expressions have the same numeric type `T`
       (which can be one of `Byte`, `Short`, `Int`, `Long`, `Float`,
       `Double`), possibly after widening, and if none of the `Int`
       literals would incur a loss of precision when converted to `T`,
       then they are thus converted (the other expressions are left
       unchanged regardless)
     - otherwise, the expressions `Es` are used unchanged

    A loss of precision occurs for an `Int -> Float` conversion of a constant
    `c` if `c.toFloat.toInt != c`. For an `Int -> Byte` conversion it occurs
    if `c.toByte.toInt != c`. For an `Int -> Short` conversion, it occurs
    if `c.toShort.toInt != c`.

__Examples:__

    inline val b = 33
    def f(): Int = b + 1
    List(b, 33, 5.5)      : List[Double] // b is an inline val
    List(f(), 33, 5.5)    : List[AnyVal] // f() is not a constant
    List(5, 11L)          : List[Long]
    List(5, 11L, 5.5)     : List[AnyVal] // Long and Double found
    List(1.0f, 2)         : List[Float]
    List(1.0f, 1234567890): List[AnyVal] // loss of precision
    List(b, 33, 'a')      : List[AnyVal] // Char is not a numeric
    List(5.toByte, 11)    : List[Byte]
