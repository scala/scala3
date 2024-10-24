class annot[T](arg: T) extends scala.annotation.Annotation

def main =
  val n: Int = 0
  def f(x: Any): Unit = ()

  val x1: Int @annot(n + 1) = 0 // error
  val x2: Int @annot(f(2)) = 0 // error
  val x3: Int @annot(throw new Error()) = 0 // error
  val x4: Int @annot((x: Int) => x) = 0 // error

  @annot(m1(2)) val y1: Int = 0 // error
  @annot(throw new Error()) val y2: Int = 0 // error
  @annot((x: Int) => x) val y3: Int = 0  // error
  @annot(x + 1) val y4: Int = 0 // error

  ()
