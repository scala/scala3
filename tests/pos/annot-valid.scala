class annot[T](arg: T) extends scala.annotation.Annotation

def main =
  val n: Int = 0
  def f(x: Any): Unit = ()

  object O:
    def g(x: Any): Unit = ()

  val x1: Int @annot(42) = 0
  val x2: Int @annot("hello") = 0
  val x3: Int @annot(classOf[Int]) = 0
  val x4: Int @annot(Array(1,2)) = 0
  val x5: Int @annot(Array(Array(1,2),Array(3,4))) = 0
  val x6: Int @annot((1,2)) = 0
  val x7: Int @annot((1,2,3)) = 0
  val x8: Int @annot(((1,2),3)) = 0
  val x9: Int @annot(((1,2),(3,4))) = 0
  val x10: Int @annot(Symbol("hello")) = 0
  val x11: Int @annot(n + 1) = 0
  val x12: Int @annot(f(2)) = 0
  val x13: Int @annot(throw new Error()) = 0
  val x14: Int @annot(42: Double) = 0
  val x15: Int @annot(O.g(2)) = 0
  val x16: Int @annot((x: Int) => x) = 0
  val x17: Int @annot([T] => (x: T) => x) = 0
  val x18: Int @annot(O.g) = 0

  @annot(42) val y1: Int = 0
  @annot("hello") val y2: Int = 0
  @annot(classOf[Int]) val y3: Int = 0
  @annot(Array(1,2)) val y4: Int = 0
  @annot(Array(Array(1,2),Array(3,4))) val y5: Int = 0
  @annot((1,2)) val y6: Int = 0
  @annot((1,2,3)) val y7: Int = 0
  @annot(((1,2),3)) val y8: Int = 0
  @annot(((1,2),(3,4))) val y9: Int = 0
  @annot(Symbol("hello")) val y10: Int = 0
  @annot(n + 1) val y11: Int = 0
  @annot(f(2)) val y12: Int = 0
  @annot(throw new Error()) val y13: Int = 0
  @annot(42: Double) val y14: Int = 0
  @annot(O.g(2)) val y15: Int = 0
  @annot((x: Int) => x) val y16: Int = 0
  @annot([T] => (x: T) => x) val y17: Int = 0
  @annot(O.g) val y18: Int = 0

  ()
