import scala.quoted.*

class annot1[T](x: Expr[T]) extends scala.annotation.Annotation
class annot2[T: Type](x: T) extends scala.annotation.Annotation

class Test()(implicit qtx: Quotes) {
  @annot1('{4}) // error
  def foo(str: String) = ()

  @annot2(4)(using Type.of[Int]) // error
  def foo2(str: String) = ()

}
