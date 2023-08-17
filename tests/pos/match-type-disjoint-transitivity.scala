/* Tests that the following property holds for a chosen set of types (S, T, U):
 *
 *   If S <: T and T provably disjoint from U, then S provably disjoint from U.
 */

class Parent[T]
class Child[T] extends Parent[T]
trait ChildTrait[T] extends Parent[T]

class OtherClass

trait Common[A]
trait Left[A] extends Common[A]
trait Right[A] extends Common[A]

// Since Parent[Boolean] disjoint from Parent[Int], we must have Child[Boolean] also disjoint from Parent[Int]
object Test1:
  type MT[X] = X match
    case Parent[Int]     => Int
    case Parent[Boolean] => Boolean

  def test(): Unit =
    summon[MT[Parent[Int]] =:= Int]
    summon[MT[Parent[Boolean]] =:= Boolean]

    summon[MT[Child[Int]] =:= Int]
    summon[MT[Child[Boolean]] =:= Boolean]
  end test
end Test1

// Since Parent[Int] disjoint from OtherClass, we must have Child[Int] and ChildTrait[T] also disjoint from OtherClass
object Test2:
  type MT[X] = X match
    case OtherClass  => Int
    case Parent[Int] => Boolean

  def test(): Unit =
    summon[MT[OtherClass] =:= Int]
    summon[MT[Parent[Int]] =:= Boolean]

    summon[MT[Child[Int]] =:= Boolean]
    summon[MT[ChildTrait[Int]] =:= Boolean]
  end test
end Test2

// Since Common[Int] is disjoint from Right[Boolean], we must have Left[Int] disjoint from Right[Boolean]
object Test3:
  type MT[X] = X match
    case Right[Boolean] => Int
    case Any            => Boolean

  def test(): Unit =
    summon[MT[Common[Int]] =:= Boolean]
    summon[MT[Left[Int]] =:= Boolean]
  end test
end Test3
