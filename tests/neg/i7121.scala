import scala.quoted._

class annot1[T](x: Expr[T]) extends scala.annotation.Annotation
class annot2[T: Type](x: T) extends scala.annotation.Annotation

class Test()(implicit qtx: QuoteContext) {
  @annot1('{4}) // error
  def foo(str: String) = ()

  @annot2(4)(given '[Int]) // error
  def foo2(str: String) = ()

}
