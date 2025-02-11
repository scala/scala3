import scala.quoted.*

class annot2[T: Type](x: T) extends scala.annotation.Annotation

class Test()(implicit qtx: Quotes) {
  @annot2(4)(using Type.of[Int]) // error
  def foo2(str: String) = ()
}
