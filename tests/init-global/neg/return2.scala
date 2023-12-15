object A:
  def foo(x: Int): Int => Int =
    val f = (a: Int) => a + B.n      
    var i = 0

    val g = () => return f

    if x <= 0 then g()

    (a: Int) => a * a + x

object B:
  val n = A.foo(-10)(20)

// nopos-error: No warnings can be incurred under -Werror.