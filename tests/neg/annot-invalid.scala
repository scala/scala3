class annot[T](arg: T) extends scala.annotation.Annotation

def main =
  val x1: Int @annot(new Object {}) = 0 // error
  val x2: Int @annot({class C}) = 0 // error
  val x16: Int @annot({val y: Int = 2}) = 0 // error
  val x17: Int @annot({def f = 2}) = 0 // error
  val x18: Int @annot((x: Int) => x) = 0 // error
  val x19: Int @annot(O.g) = 0 // error

  @annot(new Object {}) val y1: Int = 0 // error
  @annot({class C}) val y2: Int = 0  // error
  @annot({val y: Int = 2}) val y16: Int = 0 // error
  @annot({def f = 2}) val y17: Int = 0 // error
  @annot((x: Int) => x) val y18: Int = 0 // error
  @annot(O.g) val y19: Int = 0 // error

  ()
