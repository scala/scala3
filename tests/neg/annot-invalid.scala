class annot[T](arg: T) extends scala.annotation.Annotation

def main =
  val x1: Int @annot(new Object {}) = 0 // error
  val x2: Int @annot({val x = 1}) = 0 // error
  val x3: Int @annot((x: Int) => x) = 0 // error

  @annot(new Object {}) val y1: Int = 0 // error
  @annot({val x = 1}) val y2: Int = 0 // error
  @annot((x: Int) => x) val y3: Int = 0  // error

  ()
