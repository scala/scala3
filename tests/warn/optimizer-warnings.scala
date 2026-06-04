//> using options -opt -opt-inline:** -Wopt:all

object Test: // warn
  def foo(n: Int): Int =
    val (a, b) = n match { case 0 => (1, 2); case _ => (3, 4) }; a + b // warn // warn // warn // warn
