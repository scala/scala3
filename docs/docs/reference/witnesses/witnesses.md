---
layout: doc-page
title: "Witnesses"
---

Witnesses provide a concise and uniform syntax for defining implicit values. Example:

```scala
trait Ord[T] {
  def compareTo(this x: T)(y: T): Int
  def < (this x: T)(y: T) = x.compareTo(y) < 0
  def > (this x: T)(y: T) = x.compareTo(y) > 0
}

witness IntOrd for Ord[Int] {
  def compareTo(this x: Int)(y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

witness ListOrd[T: Ord] for Ord[List[T]] {
  def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = x.compareTo(y)
      if (fst != 0) fst else xs1.compareTo(ys1)
  }
}
```

Witness can be seen as shorthands for implicit definitions. The winesses above could also have been formulated as implicits as follows:
```scala
implicit object IntOrd extends Ord[Int] {
  def compareTo(this x: Int)(y: Int) =
    if (x < y) -1 else if (x > y) +1 else 0
}

class ListOrd[T: Ord] extends Ord[List[T]] {
  def compareTo(this xs: List[T])(ys: List[T]): Int = (xs, ys) match {
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => +1
    case (x :: xs1, y :: ys1) =>
      val fst = x.compareTo(y)
      if (fst != 0) fst else xs1.compareTo(ys1)
  }
}
implicit def ListOrd[T: Ord]: Ord[List[T]] = new ListOrd[T]
```
In fact, a plausible compilation strategy would map the witnesses given above to exactly these implicit definitions.

Implicit definitions are kept for the moment but should be be deprecated eventually. As we will see, the only kind of implicit definitions that canot be emulated by witnesses are implicit conversions. It's interesting that the `implicit` modifier would be relegated at some point to its original meaning of defining an implicit conversion (implicit parameters and definitions came later in Scala's evolution).

Why prefer witnesses over implicit definitions? Their definitions are shorter, more uniform, and they focus on intent rather than mechanism: I.e. we define a _witness for_ a type, instead of an _implicit object_ that happens to _extend_ a type. Likewise, the `ListOrd` witness is shorter and clearer than the class/implicit def combo that emulates it.

## Witnesses for Extension Methods

Witnesses can also be defined without a `for` clause. A typical application is to use a witness to define some extension methods. Examples:

```scala
witness StringOps {
  def longestStrings(this xs: Seq[String]): Seq[String] = {
    val maxLength = xs.map(_.length).max
    xs.filter(_.length == maxLength)
  }
}

witness ListOps {
  def second[T](this xs: List[T]) = xs.tail.head
}
```
Witnesses like these translate directly to `implicit` objects.

## Anonymous Witnesses

The name of a witness definition can be left out. Examples:
```scala
witness for Ord[Int] { ... }
witness [T: Ord] for Ord[List[T]] { ... }

witness {
  def second[T](this xs: List[T]) = xs.tail.head
}
```
If the name of a witness is missing, the compiler will synthesize a name from
the type in the for clause, or, if that is missing, from the first defined
extension method. Details remain to be specified.

## Conditional Witnesses

A witness can depend on another witness being defined. For instance:
```scala
trait Convertible[From, To] {
  def convert (this x: From): To
}

witness [From, To] with c: Convertible[From, To] for Convertible[List[From], List[To]] {
  def convert (this x: ListFrom]): List[To] = x.map(c.convert)
}
```

The `with` clause in a witness defines required witnesses. The witness for `Convertible[List[From], List[To]]` above is defined only if a witness for `Convertible[From, To]` exists.
`with` clauses translate to implicit parameters if implicit defs. Here is the expansion of the anonmous witness above as an implicit def (the example demonstrates well the reduction
in boilerplate that witness syntax can achieve):
```scala
class Convertible_List_List_witness[From, To](implicit c: Convertible[From, To])
extends Convertible[List[From], List[To]] {
  def convert (this x: List[From]): List[To] = x.map(c.convert)
}
implicit def Convertible_List_List_witness[From, To](implicit c: Convertible[From, To])
  : Convertible[List[From], List[To]] =
  new Convertible_List_List_witness[From, To]
```
Context bounds in witness definitions also translate to implicit parameters, and therefore they can be represented alternatively as with clauses. For instance, here is an equivalent definition of the `ListOrd` witness:
```scala
witness ListOrd[T] with ord: Ord[T] for List[Ord[T]] { ... }
```
An underscore ‘_’ can be used as the name of a required witness, if that witness does not
need to be referred to directly. For instance, the last `ListOrd` witness could also have been written like this:
```scala
witness ListOrd[T] with _: Ord[T] for List[Ord[T]] { ... }
```

## Abstract and Alias Witnesses

Like implicit definitions, witnesses can be abstract. An abstract witness is characterized by not having a body after the `for` clause. Example:
```scala
trait TastyAPI {
  type Symbol
  trait SymDeco {
    def name(this sym: Symbol): Name
    def tpe(this sym: Symbol): Type
  }
  witness symDeco for SymDeco
}
```
Abstract witnesses always have a `for` clause. They cannot be anonymous.

Witnesses can also be defined as aliases of other values. Example:
```scala
witness symDeco for SymDeco = compilerSymOps
```
As another example, if one had already defined classes `IntOrd` and `ListOrd`, witnesses for them could be defined as follows:
```scala
class IntOrd extends Ord[Int] { ... }
class ListOrd[T: Ord] extends Ord[List[T]] { ... }

witness for Ord[Int] = new IntOrd
witness [T: Ord] for Ord[List[T]] = new ListOrd[T]
```
The `for` clause in an alias witness is mandatory unless the witness definition is unconditional and occurs as a statement in a block. This corresponds to the same restriction for implicit vals in Scala 3.

Abstract witnesses translate to abstract implicit methods. Alias witnesses translate to implicit defs if they are conditional or to implicit vals otherwise. For instance, the witnesses defined so far in this section translate to:
```scala
implicit def symDeco: SymDeco

implicit val symDeco: SymDeco = compilerSymOps

implicit val Ord_Int_witness: Ord[Int] = new IntOrd
implicit def Ord_List_witness(implicit ev: Ord[T]): Ord[List[T]] = new ListOrd[T]
```
The `lazy` modifier is applicable to unconditional alias witnesses. If present, the translated implicit val is lazy. For instance,
```scala
lazy witness for Ord[Int] = new IntOrd
```
would translate to
```scala
lazy implicit val Ord_Int_witness: Ord[Int] = new IntOrd
```
