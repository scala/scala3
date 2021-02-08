package strawman.typeclasses

trait SemiGroup[T] {
  def append(x: T, y: T): T
}
object SemiGroup {
  class Ops {
    implicit class InfixAppend[T: SemiGroup](self: T) {
      def |+| (other: T): T = implicitly[SemiGroup[T]].append(self, other)
    }
  }
  object ops extends Ops
}

trait Monoid[T] extends SemiGroup[T] {
  val id: T
}
object Monoid {
  object ops extends SemiGroup.Ops
}

trait Ring[T] extends Monoid[T] {
  val zero = id
  val one: T
  def product(x: T, y: T): T
}
object Ring {
  class Ops extends SemiGroup.Ops {
    implicit class InfixProduct[T: Ring](self: T) {
      def |*| (other: T): T = implicitly[Ring[T]].product(self, other)
    }
  }
  object ops extends Ops
}



object Test {
  implicit object StringMonoid extends Monoid[String] {
    def append(x: String, y: String): String = x ++ y
    val id = ""
  }

  implicit object IntRing extends Ring[Int] {
    def append(x: Int, y: Int) = x + y
    val id = 0
    val one = 1
    def product(x: Int, y: Int) = x * y
  }

  import Monoid.ops._ // works in dotty, fails in scalac
  import Ring.ops.*
  "abc" |+| "def"
  "abc" |+| StringMonoid.id
  StringMonoid.id |+| "abc"

  1 |+| 2
  3 |*| 3


}
