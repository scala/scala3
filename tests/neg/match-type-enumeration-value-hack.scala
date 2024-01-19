type EnumValueAux[A] = ({ type Value }) { type Value = A }

type EnumValue[E <: Enumeration] = E match
  case EnumValueAux[t] => t

// A class extending Enumeration does not yet define a concrete enumeration
class Suit extends Enumeration:
  val Hearts, Diamonds, Clubs, Spades = Val()

object Test:
  summon[Suit#Value =:= EnumValue[Suit]] // error
end Test
