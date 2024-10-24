class annot[T](arg: T) extends scala.annotation.Annotation

def main =
  val n: Int = ???
  def f(x: Any): Unit = ()

  val x1: Int @annot(42) = 0
  val x2: Int @annot("hello") = 0
  val x3: Int @annot(classOf[Int]) = 0
  val x4: Int @annot(Array(1,2)) = 0
  val x5: Int @annot(Array(Array(1,2),Array(3,4))) = 0
  val x6: Int @annot((1,2)) = 0
  val x7: Int @annot((1,2,3)) = 0
  val x8: Int @annot(((1,2),3)) = 0
  val x9: Int @annot(((1,2),(3,4))) = 0
  
  @annot(42) val y1: Int = 0
  @annot("hello") val y2: Int = 0
  @annot(classOf[Int]) val y3: Int = 0
  @annot(Array(1,2)) val y4: Int = 0
  @annot(Array(Array(1,2),Array(3,4))) val y5: Int = 0
  @annot((1,2)) val y6: Int = 0
  @annot((1,2,3)) val y7: Int = 0
  @annot(((1,2),3)) val y8: Int = 0
  @annot(((1,2),(3,4))) val y9: Int = 0

  ()
