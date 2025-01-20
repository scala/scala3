
trait Zippable[-A, -B]:
  type Out
  def zip(left: A, right: B): Out

object Zippable extends ZippableLowPrio:
  given append: [A <: Tuple, B] => (Zippable[A, B] { type Out = Tuple.Append[A, B] }) =
    (left, right) => left :* right

trait ZippableLowPrio:
  given pair: [A, B] => (Zippable[A, B] { type Out = (A, B) }) =
    (left, right) => (left, right)


object Minimization:

  trait Fun1:
    type Out
    def apply(x: Any): Out

  type M[X] = X match
    case String => X

  def test[A] =

    val _: Fun1 { type Out = M[A] } = new Fun1:
      type Out = M[A]
      def apply(x: Any): Out = ???

    val _: Fun1 { type Out = M[A] } = x => ???

    val _: Fun1 { type Out = A match {case String => A} } = x => ???
