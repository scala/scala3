import scala.quoted._

class annot1[T](using s: Scope)(x: s.Expr[T]) extends scala.annotation.Annotation
class annot2[T](using s: Scope)(x: s.Type[T]) extends scala.annotation.Annotation

class Test(using s: Scope) {
  @annot1('{4}) // error
  def foo(str: String) = ()

  @annot2(4)(using '[Int]) // error
  def foo2(str: String) = ()

}
