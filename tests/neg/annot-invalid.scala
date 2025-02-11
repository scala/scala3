import scala.quoted.Quotes

class annot[T](arg: T) extends scala.annotation.Annotation

def main =
  object O:
    def g(x: Int): Int = x

  val x1: Int @annot(new Object {}) = 0 // error
  val x2: Int @annot({class C}) = 0 // error
  val x3: Int @annot({val y: Int = 2}) = 0 // error
  val x4: Int @annot({def f = 2}) = 0 // error
  def withQuotes(using Quotes) =
    val x5: Int @annot('{4}) = 0 // error

  @annot(new Object {}) val y1: Int = 0 // error
  @annot({class C}) val y2: Int = 0  // error
  @annot({val y: Int = 2}) val y3: Int = 0 // error
  @annot({def f = 2}) val y4: Int = 0 // error
  def withQuotes2(using Quotes) =
    @annot('{4}) val y5: Int = 0 // error

  ()
