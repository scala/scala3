class Parser[T, Elem]

object Test {
  type P[T] = Parser[T, Char]
  class CustomParser extends P[Unit]
}
