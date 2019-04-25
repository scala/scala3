---
layout: doc-page
title: "Extension Methods"
---

Extension methods allow one to add methods to a type after the type is defined. Example:

```scala
case class Circle(x: Double, y: Double, radius: Double)

def (c: Circle) circumference: Double = c.radius * math.Pi * 2
```

Like regular methods, extension methods can be invoked with infix `.`:

```scala
  val circle = Circle(0, 0, 1)
  circle.circumference
```

### Translation of Extension Methods

Extension methods are methods that have a parameter clause in front of the defined
identifier. They translate to methods where the leading parameter section is moved
to after the defined identifier. So, the definition of `circumference` above translates
to the plain method, and can also be invoked as such:
```scala
def circumference(c: Circle): Double = c.radius * math.Pi * 2

assert(circle.circumference == circumference(circle))
```

### Translation of Calls to Extension Methods

When is an extension method applicable? There are two possibilities.

 - An extension method is applicable if it is visible under a simple name, by being defined
   or inherited or imported in a scope enclosing the application.
 - An extension method is applicable if it is a member of some evidence value at the point of the application.

As an example, consider an extension method `longestStrings` on `String` defined in a trait `StringSeqOps`.

```scala
trait StringSeqOps {
  def (xs: Seq[String]) longestStrings = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}
```
We can make the extension method available by defining evidence for `StringSeqOps`, like this:
```scala
evidence ops1 for StringSeqOps
```
Then
```scala
List("here", "is", "a", "list").longestStrings
```
is legal everywhere `ops1` is available as evidence. Alternatively, we can define `longestStrings` as a member of a normal object. But then the method has to be brought into scope to be usable as an extension method.

```scala
object ops2 extends StringSeqOps
import ops2.longestStrings
List("here", "is", "a", "list").longestStrings
```
The precise rules for resolving a selection to an extension method are as follows.

Assume a selection `e.m[Ts]` where `m` is not a member of `e`, where the type arguments `[Ts]` are optional,
and where `T` is the expected type. The following two rewritings are tried in order:

 1. The selection is rewritten to `m[Ts](e)`.
 2. If the first rewriting does not typecheck with expected type `T`, and there is evidence `i`
    in either the current scope or in the evidence scope of `T`, and `i` defines an extension
    method named `m`, then selection is expanded to `i.m[Ts](e)`.
    This second rewriting is attempted at the time where the compiler also tries an implicit conversion
    from `T` to a type containing `m`. If there is more than one way of rewriting, an ambiguity error results.

So `circle.circumference` translates to `CircleOps.circumference(circle)`, provided
`circle` has type `Circle` and `CircleOps` is an eligible evidence value (i.e. it is visible at the point of call or it is defined in the companion object of `Circle`).

### Evidence for Extension Methods

Evidence that defines extension methods can also be defined without a `for` clause. E.g.,

```scala
evidence StringOps {
  def (xs: Seq[String]) longestStrings: Seq[String] = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}

evidence {
  def (xs: List[T]) second[T] = xs.tail.head
}
```
If such an evidence is anonymous (as in the second example above), its name is synthesized from the name
of the first defined extension method.

### Operators

The extension method syntax also applies to the definition of operators.
In each case the definition syntax mirrors the way the operator is applied.
Examples:
```scala
  def (x: String) < (y: String) = ...
  def (x: Elem) +: (xs: Seq[Elem]) = ...

  "ab" + "c"
  1 +: List(2, 3)
```
The two definitions above translate to
```scala
  def < (x: String)(y: String) = ...
  def +: (xs: Seq[Elem])(x: Elem) = ...
```
Note that swap of the two parameters `x` and `xs` when translating
the right-binding operator `+:` to an extension method. This is analogous
to the implementation of right binding operators as normal methods.

### Generic Extensions

The `StringSeqOps` examples extended a specific instance of a generic type. It is also possible to extend a generic type by adding type parameters to an extension method. Examples:

```scala
def (xs: List[T]) second [T] =
  xs.tail.head

def (xs: List[List[T]]) flattened [T] =
  xs.foldLeft[List[T]](Nil)(_ ++ _)

def (x: T) + [T : Numeric](y: T): T =
  the[Numeric[T]].plus(x, y)
```

As usual, type parameters of the extension method follow the defined method name. Nevertheless, such type parameters can already be used in the preceding parameter clause.


### Syntax

The required syntax extension just adds one clause for extension methods relative
to the [current syntax](https://github.com/lampepfl/dotty/blob/master/docs/docs/internals/syntax.md).
```
DefSig            ::=  ...
                    |  ‘(’ DefParam ‘)’ [nl] id [DefTypeParamClause] DefParamClauses
```




