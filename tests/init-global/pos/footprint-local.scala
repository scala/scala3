object A:
  val f = foo()
  val a = f()
  def foo(): () => Int =
    var x = 10
    () => x
