object A:
  class Pair(val f: Int => Unit, val g: () => Int)
  val p: Pair = foo()

  def foo(): Pair =
    var x = 6
    new Pair(
      (y => x = y), // warn
      () => x
    )

object B:
  A.p.f(10)
