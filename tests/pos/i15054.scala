import scala.annotation.Annotation

class AnAnnotation(function: Int => String) extends Annotation

@AnAnnotation(_.toString)
val a = 1
@AnAnnotation(_.toString.length.toString)
val b = 2

def test =
  @AnAnnotation(_.toString)
  val a = 1
  @AnAnnotation(_.toString.length.toString)
  val b = 2
  a + b
