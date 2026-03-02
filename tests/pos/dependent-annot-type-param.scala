class annot[T] extends annotation.Annotation
class Box[T]()
def f(x: Int): Int @annot[Box[x.type]] = x
def test =
  val foo = f(42)
