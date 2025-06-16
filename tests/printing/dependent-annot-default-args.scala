class annot(x: Any, y: Any = 42) extends annotation.Annotation
class annot2(x: Any = -1, y: Array[Any] = Array("Hello")) extends annotation.Annotation

def f(x: Any): Any @annot(x) = x
def f2(x: Int): Int @annot2(y = Array("Hello", x)) = x
def f3(x: Any, y: Any): Any @annot(y=y, x=x) = x

def test =
  val y: Int = ???

  val z = f(y)
  val z2 = f2(y)

  @annot(44) val z3 = 45
  @annot2(y = Array("Hello", y)) val z4 = 45

  // Arguments are still lifted if the annotation class is instantiated
  // explicitly. See #22526.
  val z5 = new annot(y = Array("World"), x = 1)
  val z6 = new annot2(y = Array("World"), x = 1)
  @annot(y = new annot(y = Array("World"), x = 1), x = 2) val z7 = 45
  @annot(y = 3: Int @annot(y = Array("World"), x = 1), x = 4) val z8 = 45
  val z9: Int @annot(y = new annot(y = Array("World"), x = 1), x = 2)  = 46
  @annot(y = 3: Int @annot(y = Array("World"), x = 1), x = 4) val z10 = 45
  val z11 = f(new annot(y = Array("World"), x = 1))
  val z12 = f3(Array("World"), 1)
  val z13 = f3(y=Array("World"), x=1)
