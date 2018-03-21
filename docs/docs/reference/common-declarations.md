---
layout: doc-page
title: "Common Declarations"
---

`common` declarations and definitions are a way to specify members of the companion object of a class. Unlike `static` definitions in Java, `common` declarations can be inherited.

As an example, consider the following trait `Text` with an implementation class `FlatText`.

```scala
trait Text {
  def length: Int
  def apply(idx: Int): Char
  def concat(txt: Text): Text
  def toStr: String

  common def fromString(str: String): Text
  common def fromStrings(strs: String*): Text =
    ("" :: strs).map(fromString).reduceLeft(_.concat)
}

class FlatText(str: String) extends Text {
  def length = str.length
  def apply(n: Int) = str.charAt(n)
  def concat(txt: Text): Text = new FlatText(str ++ txt.toStr)
  def toStr = str

  common def fromString(str: String) = new FlatText(str)
}
```

The `common` method `fromString` is abstract in trait `Text`. It is defined in the implementing companion object of `FlatText`. By contrast, the `fromStrings` method in trait Text is concrete, with an implementation referring to the abstract `fromString`. It is inherited by the companion object `FlatText`. So the following are legal:

```scala
val txt1 = FlatText.fromString("hello")
val txt2 = FlatText.fromStrings("hello", ", world")
```

`common` definitions are only members of the companion objectcs of classes, not traits. So the following would give a "member not found" error.

```scala
val erroneous = Text.fromStrings("hello", ", world") // error: not found
```

## The `Instance` type

In the previous example, the argument and result type of `concat` is just `Text`. So every implementation of `Text` has to be prepared to concatenate all possible implementatons of `Text`. Furthermore, we hide the concrete implementation type in the result type of `concat` and of the construction methods `fromString` and `fromStrings`. Sometimes we want a different design that specifies the actual implementation type instead of the base trait `Text`. We can refer to this type using the predefined type `Instance`:

```scala
trait Text {
  def length: Int
  def apply(idx: Int): Char
  def concat(txt: Instance): Instance
  def toStr: String

  common def fromString(str: String): Instance
  common def fromStrings(strs: String*): Instance =
    ("" :: strs).map(fromString).reduceLeft(_.concat)
}
```

In traits that define or inherit `common` definitions, the `Instance` type refers to the (as yet unknown) instance type whose
companion object implements the trait. To see why `Instance` is useful, consider another possible implementation of `Text`, implemented as a tree of strings. The advantage of the new implementation is that `concat` is constant time. Both old and new implementations share the definition of the `common` method `fromStrings`.

```scala
enum ConcText extends Text {
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

  def concat(txt: ConcText) = Conc(this, txt)

  def toStr: String = this match {
    case Str(s) => s
    case Conc(t1, t2) => t1.toStr ++ t2.toStr
  }
  common def fromString(str: String): ConcText = Str(str)
}
```

The `concat` method of `ConcText` with type `(txt: ConcText): ConcText` is a valid implementation of the
abstract method in `Text` of type `(txt: Instance): Instance` because `ConcText` is a class implementing `Text`
which means that it fixes `Instance` to be `ConcText`.

Note: The `Instance` type is a useful abstraction for traits that are always implemented via `extends`. For type-class like traits that are intended to be implemented after the fact with extension clauses, there is another predefined type `This` that is generally more appropriate (more on `This` in the typeclass section).

## The `common` Reference

Let's add another method to `Text`:

    trait Text {
      ...
      def flatten: Instance = fromString(toStr)
    }

Why does this work? The `fromString` method is abstract in `Text` so how to we find the correct implementation in `flatten`?
Comparing with the `toStr` reference, this one is an instance method and therefore is expanded to `this.toStr`. But the same does not work for `fromString` because it is a `common` method, not an instance method.
In fact, the application above is syntactic sugar for

    this.common.fromString(this.toStr)

The `common` selector is defined in each trait that defines or inherits `common` definitions. It refers at runtime to
the object that implements the `common` definitions.

## Translation

The translation of a trait `T` that defines `common` declarations `common D1, ..., common Dn`
and extends traits with common declarations `P1, ..., Pn` is as follows:
All `common` declarations are put in a trait `T.Common` which is defined in `T`'s companion object:

    object T {
      trait Common extends P1.Common with ... with Pn.Common { self =>
        type Instance <: T { val `common`: self.type }
        D1
        ...
        Dn
      }
    }

The trait inherits all `Common` traits associated with `T`'s parent traits. If no explicit definition of `Instance` is
given, it declares the `Instance` type as shown above. The trait `T` itself is expanded as follows:

    trait T extends ... {
      val `common`: T.Common
      import `common`._

      ...
    }

Any direct reference to `x.common` in the body of `T` is simply translated to

    x.`common`

The translation of a class `C` that defines `common` declarations `common D1, ..., common Dn`
and extends traits with common declarations `P1, ..., Pn` is as follows:
All `common` definitions of the class itself are placed in `C`'s companion object, which also inherits all
`Common` traits of `C`'s parents. If `C` already defines a companion object, the synthesized parents
come after the explicitly declared ones, whereas the common definitions precede all explicitly given statements of the
companion object. The companion object also defines the `Instance` type as

    type Instance = C

unless an explicit definition of `Instance` is given in the same object.

### Example:

As an example, here is the translation of trait `Text` and its two implementations `FlatText` and `ConcText`:

```scala
trait Text {
  val `common`: Text.Common
  import `common`._

  def length: Int
  def apply(idx: Int): Char
  def concat(txt: Instance): Instance
  def toStr: String
  def flatten = `common`.fromString(toStr)
}
object Text {
  trait Common { self =>
    type Instance <: Text { val `common`: self.type }
    def fromString(str: String): Instance
    def fromStrings(strs: String*): Instance =
      ("" :: strs.toList).map(fromString).reduceLeft(_.concat(_))
  }
}

class FlatText(str: String) extends Text {
  val `common`: FlatText.type = FlatText
  import `common`._

  def length = str.length
  def apply(n: Int) = str.charAt(n)
  def concat(txt: FlatText) = new FlatText(str ++ txt.toStr)
  def toStr = str
}
object FlatText extends Text.Common {
  type Instance = FlatText
  def fromString(str: String) = new FlatText(str)
}

enum ConcText extends Text {
  val `common`: ConcText.type = ConcText
  import `common`._

  case Str(s: String)
  case Conc(t1: Text, t2: Text)

  lazy val length = this match {
    case Str(s) => s.length
    case Conc(t1, t2) => t1.length + t2.length
  }

  def apply(n: Int) = this match {
    case Str(s) => s.charAt(n)
    case Conc(t1, t2) => if (n < t1.length) t1(n) else t2(n - t1.length)
  }

  def concat(txt: ConcText): ConcText = Conc(this, txt)

  def toStr: String = this match {
    case Str(s) => s
    case Conc(t1, t2) => t1.toStr ++ t2.toStr
  }
}

object ConcText extends Text.Common {
  type Instance = ConcText
  def fromString(str: String) = Str(str)
}
```

### Relationship with Parameterization

Common definitions do not see the type parameters of their enclosing class or trait. So the following is illegal:

```scala
trait T[A] {
  common def f: T[A]
}
```

The implicit `Instance` declaration of a trait or class follows in its parameters the parameters of the
trait or class. For instance:

```scala
trait Sequence[+T <: AnyRef] {
  def map[U <: AnyRef](f: T => U): Instance[U]

  common def empty[T]: Instance[T]
}
```
The implicitly defined `Instance` declaration would be in this case:

```scala
object Sequence {
  trait Common { self =>
    type Instance[+T <: AnyRef] <: Sequence[T] { val `common`: self.type }
  }
}
```

The rules for mixing `Instance` definitions of different kinds depend on the status of #4150. If #4150 is
accepted, we permit `Instance` definitions to co-exist at different kinds. If #4150 is not accepted, we
have to forbid this case, which means that a class must have the same parameter structure as all the traits
with common members that it extends.
