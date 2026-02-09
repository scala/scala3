//> using options -Werror -Wvalue-discard

case class F(i: Int)

object Main {
  def example() =
    List(1, 2, 3).map(F): @annotation.nowarn
}
