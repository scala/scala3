package example

import scala.language.existentials
import scala.language.higherKinds

class ann[T](x: T) extends scala.annotation.StaticAnnotation
class ann1 extends scala.annotation.StaticAnnotation
class ann2 extends scala.annotation.StaticAnnotation

object TypTestAnnots {
  val annType1: T @ann(42) = ???
  val annType2: T @ann1 @ann2 = ???
}
