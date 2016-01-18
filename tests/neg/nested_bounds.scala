class OnlyInt[T <: Int]

object Test {
  type T = List[OnlyInt[String]] // error: Type argument String does not conform to upper bound Int
}
