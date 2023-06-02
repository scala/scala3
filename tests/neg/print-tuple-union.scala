trait Test:
  def foo[A]: Tuple.Union[A]
  def bar[B]: Int = foo[B] // error
