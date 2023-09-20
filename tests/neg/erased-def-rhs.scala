//> using options -experimental -language:experimental.erasedDefinitions

object Test {
  def f(erased i: Int) = {
    def j: Int = i // error
    j
  }
}
