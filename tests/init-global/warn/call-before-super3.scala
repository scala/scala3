class C(i: Int = 42, j: Int = 27) {
  val f = X.foo() // warn
}

object X extends C(j = 5):
  def foo() = 5

@main def test = println:
  X