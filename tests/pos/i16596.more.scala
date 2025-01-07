import scala.compiletime.ops.int.*

object NatExample {
  sealed trait Nat
  object Nat {
    case object Zero extends Nat
    case class Succ[N <: Nat](prev: N) extends Nat

    given zero: Zero.type = Zero
    given buildSucc: [N <: Nat] => (n: N) => Succ[N] = Succ(n)

    def value[N <: Nat](using n: N): N = n

    type FromInt[I <: Int] <: Nat = I match
      case 0 => Zero.type
      case _ => Succ[FromInt[I - 1]]

    summon[FromInt[0] =:= Zero.type]
    summon[FromInt[1] =:= Succ[Zero.type]]
    summon[FromInt[2] =:= Succ[Succ[Zero.type]]]
    summon[FromInt[3] =:= Succ[Succ[Succ[Zero.type]]]]
    summon[FromInt[4] =:= Succ[Succ[Succ[Succ[Zero.type]]]]]

    @main def test = {
      require(summon[FromInt[0]] == Zero)
      require(summon[FromInt[1]] == Succ(Zero))
      require(summon[FromInt[2]] == Succ(Succ(Zero)))
      require(summon[FromInt[3]] == Succ(Succ(Succ(Zero))))
      // we can summon 4 if we write it out:
      require(summon[Succ[Succ[Succ[Succ[Zero.type]]]]] == Succ(Succ(Succ(Succ(Zero)))))
      // was: we cannot summon 4 using the match type
      require(summon[FromInt[4]] == Succ(Succ(Succ(Succ(Zero)))))
    }
  }
}
