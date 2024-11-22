class annot[T](arg: T) extends scala.annotation.Annotation

def main =
  val x1: Int @annot(new Object {}) = 0 // error
  val x2: Int @annot({class C}) = 0 // error

  @annot(new Object {}) val y1: Int = 0 // error
  @annot({class C}) val y2: Int = 0  // error

  ()
