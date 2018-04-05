

## Specification

>

## Translation

Here is one possible translation of a trait `T` that defines `common` declarations `common D1, ..., common Dn`
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

