
class Equalizer[L](val leftSide: L) {
  def ===(literalNull: Null): Boolean = leftSide == null
}

object Equality {
  implicit def toEqualizer[T](x: T): Equalizer[T] = new Equalizer(x)
}


object Test {
  import scalatest._
  import Equality._

  def main(args: Array[String]): Unit = {
    val x = "String"
    try assert(x === null)
    catch {
      case _: AssertionError => // OK
    }
  }
}
