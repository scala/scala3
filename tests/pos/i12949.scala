object Catch22:
  trait TC[V]
  object TC:
    export Hodor.TC.given

object Hodor:
  object TC:
    import Catch22.TC
    given fromString[V <: String]: TC[V] = ???
    transparent inline given fromDouble[V <: Double]: TC[V] =
      new TC[V]:
        type Out = Double
    given fromInt[V <: Int]: TC[V] with
      type Out = Int

object Test:
  summon[Catch22.TC["hi"]] //works
  summon[Catch22.TC[7.7]] //works
  summon[Catch22.TC[1]] //error
