class Test {
  def foo(x: => Int) = bar(x _)
  def bar(x: () => Int) = ???
  def baz = 1
  def bam: () => Int = baz _
  def ban: () => Int = 1 _

  def titi(fun: () => Unit) = ???
  def toto(fun: => Int) = titi(fun _)
  titi(1 _) // rejected by scalac
}
