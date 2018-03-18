/** A possible type class encoding for

      trait SemiGroup {
        def add(that: This): This
      }

      trait Monoid extends SemiGroup
      common {
        def unit: This
      }

      extension IntOps for Int : Monoid {
        def add(that: Int) = this + that
      }
      common {
        def unit = 0
      }

      extension StringOps for String : Monoid {
        def add(that: Int) = this ++ that
      }
      common {
        def unit = ""
      }

      def sum[T: Monoid](xs: List[T]): T =
        (instance[T, Monoid].unit /: xs)(_ `add` _)

*/
object runtime {

  trait TypeClass {
    val common: TypeClassCommon
    type This = common.This
  }

  trait TypeClassCommon { self =>
    type This
    type Instance <: TypeClass
    def inject(x: This): Instance { val common: self.type }
  }

  trait Extension[From, To <: TypeClass] extends TypeClassCommon {
    type This = From
    type Instance = To
  }

  implicit def inject[From](x: From)
      (implicit ev: Extension[From, _]): ev.Instance { type This = From } =
    ev.inject(x)
}
import runtime._

object semiGroups {

  trait SemiGroup extends TypeClass {
    import common._
    def add(that: This): This
  }
  trait SemiGroupCommon extends TypeClassCommon {
    type Instance <: SemiGroup
  }

  trait Monoid extends SemiGroup {
    val common: MonoidCommon
    import common._
  }
  trait MonoidCommon extends SemiGroupCommon {
    type Instance <: Monoid
    def unit: This
  }

  implicit object IntOps extends Extension[Int, Monoid] with MonoidCommon { self =>
    type This = Int
    type Instance = Monoid
    def unit: Int = 0
    def inject($this: Int) = new Monoid {
      val common: self.type = self
      def add(that: This): This = $this + that
    }
  }

  implicit object StringOps extends Extension[String, Monoid] with MonoidCommon { self =>
    type This = String
    type Instance = Monoid
    def unit = ""
    def inject($this: String) = new Monoid {
      val common: self.type = self
      def add(that: This): This = $this.concat(that)
    }
  }

  def sum[T](xs: List[T])(implicit $ev: Extension[T, Monoid] with MonoidCommon) =
    (implicitly[Extension[T, Monoid] with MonoidCommon].unit /: xs)((x, y) => x `add` y)
}

/** Encoding for

  trait Ord {
    def compareTo(that: This): Int
    def < (that: This) = compareTo(that) < 0
    def > (that: This) = compareTo(that) > 0
  }
  common {
    val minimum: This
  }

  extension IntOrd for Int : Ord {
    def compareTo(that: Int) =
      if (this < that) -1 else if (this > that) +1 else 0
  }
  common {
    val minimum = Int.MinValue
  }

  extension ListOrd[T : Ord] for List[T] : Ord {
    def compareTo(that: List[T]): Int = (this, that) match {
      case (Nil, Nil) => 0
      case (Nil, _) => -1
      case (_, Nil) => +1
      case (x :: xs, y :: ys) =>
        val fst = x.compareTo(y)
        if (fst != 0) fst else xs.compareTo(ys)
    }
  }
  common {
    val minimum = Nil
  }

  def min[T: Ord](x: T, y: T) = if (x < y) x else y

  def inf[T: Ord](xs: List[T]): T = (instance[T, Ord].minimum /: xs)(_ `min` _)
*/

object ord {

  trait Ord extends TypeClass {
    import common._
    def compareTo(that: This): Int
    def < (that: This) = compareTo(that) < 0
    def > (that: This) = compareTo(that) > 0
  }
  trait OrdCommon extends TypeClassCommon {
    type Instance <: Ord
    def minimum: This
  }

  implicit object IntOrd extends Extension[Int, Ord] with OrdCommon { self =>
    type This = Int
    type Instance = Ord
    val minimum: Int = Int.MinValue
    def inject($this: Int) = new Ord {
      val common: self.type = self
      def compareTo(that: This): Int =
        if (this < that) -1 else if (this > that) +1 else 0
    }
  }

  class ListOrd[T](implicit ev: Extension[T, Ord] with OrdCommon)
  extends Extension[List[T], Ord] with OrdCommon { self =>
    type This = List[T]
    type Instance = Ord
    def minimum: List[T] = Nil
    def inject($this: List[T]) = new Ord {
      val common: self.type = self
      def compareTo(that: List[T]): Int = ($this, that) match {
        case (Nil, Nil) => 0
        case (Nil, _) => -1
        case (_, Nil) => +1
        case (x :: xs, y :: ys) =>
          val fst = x.compareTo(y)
          if (fst != 0) fst else xs.compareTo(ys)
      }
    }
  }

  implicit def listOrd[T](implicit ev: Extension[T, Ord] with OrdCommon): ListOrd[T] =
    new ListOrd[T]

  def min[T](x: T, y: T)(implicit ev: Extension[T, Ord] with OrdCommon): T =
    if (x < y) x else y

  def inf[T](xs: List[T])(implicit ev: Extension[T, Ord] with OrdCommon): T = {
    val smallest = implicitly[Extension[T, Ord] with OrdCommon].minimum
    (smallest /: xs)(min(_, _))
  }
}