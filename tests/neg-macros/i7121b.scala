import scala.quoted.*

class annot1[T](x: Expr[T]) extends scala.annotation.Annotation

class Test()(implicit qtx: Quotes) {
  @annot1('{4}) // error: expression cannot be used inside an annotation argument
  def foo(str: String) = ()
}
