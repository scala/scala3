import scala.compiletime._

final case class Coproduct[+Set, +Value, Index <: Int](value: Value & Set, index: Index)

object Coproduct {
  opaque type +:[+A, +B] = A | B

  trait At[+Set, -Value, Index <: Int] {
    def cast: Value <:< Set
  }

  object At {

    given atHead[Head, Tail]: At[Head +: Tail, Head, 0] {
      def cast: Head <:< Head +: Tail = summon[Head <:< Head +: Tail]
    }

    given atTail[Head, Tail, Value, NextIndex <: Int](given atNext: At[Tail, Value, NextIndex]): At[Head +: Tail, Value, S[NextIndex]] {
      val cast: Value <:< Head +: Tail = atNext.cast
    }

    given [A](given A): (() => A)= { () => summon[A]}
  }

  def upCast[A, B](a: A)(given erased evidence: (A <:< B)): B = a.asInstanceOf[B]

  def from[Set, Value, Index <: Int](value: Value)(given erased at: At[Set, Value, Index]): (given ValueOf[Index]) => Coproduct[Set, Value, Index] = {
    Coproduct[Set, Value, Index](upCast(value: Value)(given at.cast.liftCo[[+X] =>> Value & X]), valueOf[Index])
  }

}

object Test extends App {
  import Coproduct._

  // Error: No singleton value available for scala.compiletime.S[scala.compiletime.S[(0 : Int)]].
  val c = from[Set = Int +: String +: Seq[Double] +: Nothing](Nil)
}
