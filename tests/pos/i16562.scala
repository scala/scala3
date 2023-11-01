class Test:
  val a: Array[Any] = Array[Any]()
  val b: Array[Any] = Array[Any]()

  def ko(p: Boolean): Unit = foo((if p then a else b)*)
  def ok(p: Boolean): Unit = foo({ val x = if p then a else b; x }*)

  def foo(in: Any*): Unit = ()
