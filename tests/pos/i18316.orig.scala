import scala.language.implicitConversions
object squerel {
  trait EqualityExpression
  object PrimitiveTypeMode:
    implicit def intToTE(f: Int): TypedExpression[Int] = ???

  trait TypedExpression[A1]:
    def ===[A2](b: TypedExpression[A2]): EqualityExpression = ???
}

object scalactic {
  trait TripleEqualsSupport:
    class Equalizer[L](val leftSide: L):
      def ===(rightSide: Any): Boolean = ???

  trait TripleEquals extends TripleEqualsSupport:
    implicit def convertToEqualizer[T](left: T): Equalizer[T] = ???
}

import squerel.PrimitiveTypeMode._ // remove to make code compile
object Test extends scalactic.TripleEquals {
  import squerel.PrimitiveTypeMode._
  val fails: squerel.EqualityExpression = 1 === 1
}
