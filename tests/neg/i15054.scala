import scala.annotation.Annotation

class AnAnnotation(function: Int => String) extends Annotation

@AnAnnotation(_.toString) // error: expression cannot be used inside an annotation argument
val a = 1
@AnAnnotation(_.toString.length.toString) // error: expression cannot be used inside an annotation argument
val b = 2

def test =
  @AnAnnotation(_.toString) // error: expression cannot be used inside an annotation argument
  val a = 1
  @AnAnnotation(_.toString.length.toString) // error: expression cannot be used inside an annotation argument
  val b = 2
  a + b
