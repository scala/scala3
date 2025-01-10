class annot(x: Any, y: Any = 42) extends annotation.Annotation
class annot2(x: Any = -1, y: Array[Any] = Array("Hello")) extends annotation.Annotation

def f(x: Int): Int @annot(x) = x
def f2(x: Int): Int @annot2(y = Array("Hello", x)) = x

def test =
  val y: Int = ???

  val z = f(y)
  val z2 = f2(y)

  @annot(44) val z3 = 45
  @annot2(y = Array("Hello", y)) val z4 = 45

