type EnumValueAux[A] = ({ type Value }) { type Value = A }

type EnumValue[E <: Enumeration] = E match
  case EnumValueAux[t] => t

object Suit extends Enumeration:
  val Hearts, Diamonds, Clubs, Spades = Val()

object Test:
  summon[Suit.Value =:= EnumValue[Suit.type]]
end Test
