
import scala.deriving.Mirror

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
object Semigroup {
  given semigroupInt: Semigroup[Int] = _ + _

  given emptyTuple: Semigroup[EmptyTuple] = (x, _) => x

  given tupleN[H, T <: Tuple](using h: Semigroup[H], t: Semigroup[T]): Semigroup[H *: T] =
    (x, y) => h.combine(x.head, y.head) *: t.combine(x.tail, y.tail)

  def derived[A <: Product](using m: Mirror.ProductOf[A], s: Semigroup[m.MirroredElemTypes]): Semigroup[A] =
    (x, y) => m.fromTuple(s.combine(Tuple.fromProductTyped(x), Tuple.fromProductTyped(y)))
}

case class Foo(i: Int) derives Semigroup
object Foo {
  given overrideSemigroupInt: Semigroup[Int] = _ * _
}

@main def Test =
  assert:
    summon[Semigroup[Foo]].combine(Foo(2), Foo(3)) == Foo(6)

// Scala 3.3 and 3.4
//summon[Semigroup[Foo]].combine(Foo(2), Foo(3)) // => Foo(6)
// Scala 3.5+
//summon[Semigroup[Foo]].combine(Foo(2), Foo(3)) // => Foo(5)
