object A:
  def foo(fn: => Int) = bar(fn)

  def bar(fn: => Int) = fn
