class annot(x: Any, y: Any = 42) extends annotation.Annotation
def f(x: Int): Int @annot(x) = x
def test =
  val y: Int = ???
  val z = f(y)
