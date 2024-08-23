class Box(var x: String)

object A:
  val a = new Box("a")
  val c = foo()
  def foo(): Box = C.c

object B:
  val b = new Box("b")
  val a = bar()

  def bar(): Box = A.a

object C:
  val c = new Box("c")
