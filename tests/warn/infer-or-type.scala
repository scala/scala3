//> using options -Winfer-union

case class Pair[A](x: A, y: A)

def test = {
  val _ = List(1).contains("") // warn
  val _ = Pair(1, "") // warn
}