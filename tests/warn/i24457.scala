//> using options -Werror -Wunused:all
///> using scala 3.8.0-RC1

object test {
  private case class Impl()
  inline def makeImpl(): Any = Impl() // inline is not germane

  def seth =
    case class Loop()
    println(Loop())
}

@main def main = println:
  test.makeImpl()
