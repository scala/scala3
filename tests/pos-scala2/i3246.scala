class Test {
  def foo(x: => Int) = bar(x _)
  def bar(x: () => Int) = ???
  def baz = 1
  def bam: () => Int = baz _
  def ban: () => Int = 1 _
}
