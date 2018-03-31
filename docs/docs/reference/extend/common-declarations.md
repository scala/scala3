---
layout: doc-page
title: "Common Declarations"
---

Typeclass traits can have `common` declarations. These are a way to
abstract over values that exist only once for each implementing type.
At first glance, `common` declarations resemble `static` definitions in a language like Java,
but they are more general since they can be inherited.

As an example, consider the following trait `Text` with an implementation class `FlatText`.

```scala
trait Text extends TypeClass {
  def length: Int
  def apply(idx: Int): Char
  def concat(txt: This): This
  def toStr: String

  common def fromString(str: String): This
  common def fromStrings(strs: String*): This =
    ("" :: strs).map(fromString).reduceLeft(_.concat)
}

case class FlatText(str: String) extends Text {
  def length = str.length
  def apply(n: Int) = str.charAt(n)
  def concat(txt: FlatText): FlatText = FlatText(str ++ txt.str)
  def toStr = str

  common def fromString(str: String) = FlatText(str)
}
```

The `common` method `fromString` is abstract in trait `Text`. It is defined in the implementing companion object of `FlatText`. By contrast, the `fromStrings` method in trait Text is concrete, with an implementation referring to the abstract `fromString`. It is inherited by the companion object `FlatText`. So the following are legal:

```scala
val txt1 = FlatText.fromString("hello")
val txt2 = FlatText.fromStrings("hello", ", world")
```

`common` declarations only define members of the companion objects of classes, not traits. So the following would give a "member not found" error.

```scala
val erroneous = Text.fromStrings("hello", ", world") // error: not found
```

The `common` definition of `fromString` in `FlatText` implicitly defines a member of
`object FlatText`. Alternatively, one could have defined it as a regular method in the
companion object. This is done in the following second implementation of `Text`, which
represents a text as a tree of strings. The two implementations share the definition of the `common` method `fromStrings` in `Text`.

```scala
enum ConcText {

  case Str(s: String)
  case Conc(t1: ConcText, t2: ConcText)

  lazy val length = this match {
    case Str(s) => s.length
    case Conc(t1, t2) => t1.length + t2.length
  }

  def apply(n: Int) = this match {
    case Str(s) => s.charAt(n)
    case Conc(t1, t2) => if (n < t1.length) t1(n) else t2(n - t1.length)
  }

  def concat(txt: Text) = Conc(this, txt)

  def toStr: String = this match {
    case Str(s) => s
    case Conc(t1, t2) => t1.toStr ++ t2.toStr
  }
}
object ConcText {
  def fromString(str: String): ConcText = Str(str)
}
```

## The `common` Reference

Let's add another method to `Text`:
```scala
trait Text {
  ...
  def flatten: Instance = fromString(toStr)
}
```
Why does this work? The `fromString` method is abstract in `Text` so how do we find the correct implementation in `flatten`?
Comparing with the `toStr` reference, that one is an instance method and therefore is expanded to `this.toStr`. But the same does not work for `fromString` because it is a `common` method, not an instance method.
In fact, the application above is syntactic sugar for

```scala
this.common.fromString(this.toStr)
```
The `common` selector is defined in each typeclass trait. It refers at runtime to
the object that implements the `common` declarations.

### Relationship with Parameterization

There are special rules for common declarations in parameterized traits or classes. Parameterized traits and classes can both contain common declarations, but they have different visibility rules. Common declarations in a trait do _not_ see the type parameters of the enclosing trait. So the following is illegal:

```scala
trait T[A] {
  /*!*/ common def f: T[A] // error: not found: A
}
```

On the other hand, common definitions in a class or an extension clause _can_ refer to the type parameters of that class. Consequently, actual type arguments have to be specified when accessing such a common member. Example:

```scala
extension SetMonoid[T] for Set[T] : Monoid {
  def add(that: Set[T]) = this ++ that
  common def unit: Set[T] = Set()
}

Set[Int].unit
Set[Set[String]].unit
```

**Note:** Common definitions in a parameterized class `C[T]` cannot be members of the companion object of `C` because that would lose the visibility of the type parameter `T`. Instead they are members of a separate class that is the result type of an `apply` method
in the companion object. For instance, the `SetMonoid` extension above would be expanded
along the following lines:

```scala
object SetMonoid {
  def apply[T] = new Monoid.Common {
    def unit: Set[T] = Set()
  }
}
```
Then, as always, `Set[Int].unit` expands to `Set.apply[Int].unit`.
