//> using options -Werror -Wunused:all

object test {
  def f(t: Tuple): Nothing = ???
  val _ = (inputTuple: NamedTuple.NamedTuple[Tuple, Tuple]) => f(inputTuple)
}
