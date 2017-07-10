---
layout: doc-page
title: Dropped: Weak Conformance
---

In some situations, Scala used a {\em weak conformance} relation when
testing type compatibility or computing the least upper bound of a set
of types.  The principal motivation behind weak conformance was to
make as expression like this have type `List[Double]`:

    List(1.0, math.sqrt(3.0), 0, -3.3) // : List[Double]

It's "obvious" that this should be a `List[Double]`. However, without
some special provision, the least upper bound of the lists's element
types `(Double, Double, Int, Double)` would be `AnyVal`, hence the list
expression would be given type `List[AnyVal]`.

A less obvious example is the following one, which was also typed as a
`List[Double]`, using the weak conformance relation.

    val n: Int = 3
    val c: Char = 'X'
    val n: Double = math.sqrt(3.0)
    List(n, c, d) // used to be: List[Double], now: List[AnyVal]

Here, it is less clear why the type should be widened to
`List[Double]`, a `List[AnyVal` seems to be an equally valid -- and
more principled -- choice.

To simplify the underlying type theory, Dotty drops the notion of weak
conformance altogether. Instead, it provides more flexibility when
assigning a type to a constant expression. The rule is:

 - If a list of expressions `Es` appears as one of

     - the elements of a vararg parameter, or
     - the alternatives of an if-then-else or match expression, or
     - the body and catch results of a try expression,

   and all expressions have primitive numeric types, but they do not
   all have the same type, then the following is attempted: Every
   constant expression `E` in `Es` is widened to the least primitive
   numeric value type equal to or above the types of all expressions in `Es`,
   if that can be done without a loss of precision. Here
   _above_ and _least_ are interpreted according to the ordering given
   below.


                  Double
                 /      \
               Long    Float
                 \     /
                   Int
                 /    \
              Short   Char
                |
              Byte

    A loss of precision occurs for an `Int -> Float` conversion of a constant
    `c` if `c.toFloat.toInt != c`. For a `Long -> Double` conversion it occurs
    if `c.toDouble.toLong != c`.

    If these widenings lead to all widened expressions having the same type,
    we use the widened expressions instead of `Es`, otherwise we use `Es` unchanged.

__Examples:__

    inline val b = 33
    def f(): Int = b + 1
    List(b, 33, 'a')      : List[Int]
    List(b, 33, 'a', f()) : List[Int]
    List(1.0f, 'a', 0)    : List[Float]
    List(1.0f, 1L)        : List[Double]
    List(1.0f, 1L, f())   : List[AnyVal]
    List(1.0f, 1234567890): List[AnyVal]

The expression on the second-to-last line has type `List[AnyVal]`,
since widenings only affect constants. Hence, `1.0f` and `1L` are
widened to `Double`, but `f()` still has type `Int`. The elements
don't agree on a type after widening, hence the elements are left
unchanged.

The expression on the last line has type `List[AnyVal]` because
`1234567890` cannot be converted to a `Float` without a loss of
precision.


