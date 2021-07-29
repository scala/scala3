opaque type Lie[W <: Int] = Int
object Lie:
  trait TC[-T]:
    type Out
  object TC:
    given [W <: Int]: TC[Lie[W]] with
      type Out = W

val x  = summon[Lie.TC[Lie[7]]]
val works = summon[x.Out =:= 7]
