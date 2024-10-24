import scala.annotation.Annotation

class AnAnnotation(function: Int => String) extends Annotation

@AnAnnotation(_.toString) // error: not a valid annotation
val a = 1
@AnAnnotation(_.toString.length.toString) // error: not a valid annotation
val b = 2

def test =
  @AnAnnotation(_.toString) // error: not a valid annotation
  val a = 1
  @AnAnnotation(_.toString.length.toString) // error: not a valid annotation
  val b = 2
  a + b
