object A:
  class Pair(val f: Int => Unit, val g: () => Int)
  val p: Pair = foo()

  def foo(): Pair =
    var x = 6
    new Pair(
      y => x = y,
      (() => x)   
    )


object B:
  var y = A.p.g()

// nopos-error: No warnings can be incurred under -Werror.