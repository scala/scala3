/** A possible type class encoding for

      trait SemiGroup {
        def add(that: This): This
      }

      trait Monoid extends SemiGroup {
        static def unit: This
      }

      extend Int : Monoid {
        def add(that: Int) = this + that
        static def unit = 0
      }

      extend String : Monoid {
        def add(that: Int) = this ++ that
        static def unit = ""
      }

      def sum[T: Monoid](xs: List[T]): T =
        xs.foldLeft(inst[T, Monoid].unit)(_ `add` _)

*/
object runtime {

  trait TypeClass {
    type This
    type StaticPart[This]
  }

  trait Implementation[From] {
    type This = From
    type Implemented <: TypeClass
    def inject(x: From): Implemented { type This = From }
  }

  class CompanionOf[T] { type StaticPart[_] }

  def inst[From, To <: TypeClass](
      implicit ev1: Implementation[From] { type Implemented = To },
      ev2: CompanionOf[To]): Implementation[From] { type Implemented = To } & ev2.StaticPart[From] =
    ev1.asInstanceOf  // can we avoid the cast?

  implicit def inject[From](x: From)(
      implicit ev1: Implementation[From]): ev1.Implemented { type This = From } =
    ev1.inject(x)
}

object semiGroups {
  import runtime.*

  trait SemiGroup extends TypeClass {
    def add(that: This): This
  }

  trait Monoid extends SemiGroup {
    type StaticPart[This] <: MonoidStatic[This]
  }
  abstract class MonoidStatic[This] { def unit: This }

  implicit def companionOfMonoid: CompanionOf[Monoid] {
    type StaticPart[X] = MonoidStatic[X]
  } = new CompanionOf[Monoid] {
    type StaticPart[X] = MonoidStatic[X]
  }

  implicit object extend_Int_Monoid extends MonoidStatic[Int], Implementation[Int] {
    type Implemented = Monoid
    def unit: Int = 0
    def inject($this: Int) = new Monoid {
      type This = Int
      def add(that: This): This = $this + that
    }
  }

  implicit object extend_String_Monoid extends MonoidStatic[String], Implementation[String] {
    type Implemented = Monoid
    def unit = ""
    def inject($this: String): Monoid { type This = String } =
      new Monoid {
        type This = String
        def add(that: This): This = $this ++ that
      }
  }

  def sum[T](xs: List[T])(implicit $ev: Implementation[T] { type Implemented = Monoid } ) = {
    xs.foldLeft(inst[T, Monoid].unit)((x, y) => inject(x) `add` y)
    xs.foldLeft(inst[T, Monoid].unit)((x, y) => x `add` y)  // fails in scalac and previous dotc.
  }
}
