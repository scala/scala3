class annot[T] extends scala.annotation.StaticAnnotation
class annot2(x: Any) extends scala.annotation.StaticAnnotation

def f1[T]: Int @annot[T] = 2
def f2(x: Int): Int @annot[x.type] = 2
def f3(x: Int, y: Int @annot[x.type]) = 2
def f4(x: Int): Int @annot2(x) = 2
def f5(x: Int, y: Int @annot2(x)) = 2
def f6(x: Int): Int @annot2(x == 4) = 2
def f7(x: Int, y: Int @annot2(x == 5)): Int @annot2(y) = ???
def f8(x: Int): Int @annot2((z: Int) => x == z) = ???
def f9(x: Int): Int @annot2((z: Int) => (z: Int) => x == z) = ???
def f10(x: String): Int @annot2(throw new Error(x)) = ???

def main =
  val x1 = f1[String]
  val x2 = f2(23)
  val x3: Int @annot[24] = x2
  f3(2, 3)
  val x4 = f4(25)
  val x5: Int @annot2(26) = x4 
  val x6 = f6(27)
  val x7 = f7(29, 30)
  val x8 = f8(31)
  val x9 = f9(32)
  val x10 = f10("test")
