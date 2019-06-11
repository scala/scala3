package compiletime

class Test {
  import scala.compiletime.{constValue, erasedValue, S}

  trait Nat
  case object Zero extends Nat
  case class Succ[N <: Nat](n: N) extends Nat

  inline def toIntC[N] <: Int =
    inline constValue[N] match {
      case 0 => 0
      case _: S[n1] => 1 + toIntC[n1]
    }

  final val ctwo = toIntC[2]

  inline def defaultValue[T] <: Option[Any] = inline erasedValue[T] match {
    case _: Byte => Some(0: Byte)
    case _: Char => Some(0: Char)
    case _: Short => Some(0: Short)
    case _: Int => Some(0)
    case _: Long => Some(0L)
    case _: Float => Some(0.0f)
    case _: Double => Some(0.0d)
    case _: Boolean => Some(false)
    case _: Unit => Some(())
    case _ => None
  }

  val dInt: Some[Int] = defaultValue[Int]
  val dDouble: Some[Double] = defaultValue[Double]
  val dBoolean: Some[Boolean] = defaultValue[Boolean]
  val dAny: None.type = defaultValue[Any]

  inline def toIntT[N <: Nat] <: Int = inline scala.compiletime.erasedValue[N] match {
    case _: Zero.type => 0
    case _: Succ[n] => toIntT[n] + 1
  }

  final val two = toIntT[Succ[Succ[Zero.type]]]

}