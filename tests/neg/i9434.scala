object Foo {
  def bar1: (Int, Int) = {
    val foo = 1.0
    val bar = foo // error
      (1, 1)
  } // error

  def bar2: Vector[Int] = {
    val foo =
      Vector(1) ++ // error
    Vector(2) ++
      Vector(3)
    foo
  }
}
/* Now the diagnostic is multiline, to show more context (the args), and only one caret for the point.
-- [E050] Type Error: i9434.scala:5:14 ---------------------------------------------------------------------------------
5 |    val bar = foo
  |              ^^^
  |              value foo does not take parameters
  |
  | longer explanation available when compiling with `-explain`
*/

object X:
  def f[A](as: List[A]) = as headOption // error
    42

object X2:
  def f[A](as: List[A]) = as headOption // error
  42

object Y:
  def g = 42
  def f = g // error
    (42)

object Z:
  def g = 42
  def f = g
  (42) // warn
