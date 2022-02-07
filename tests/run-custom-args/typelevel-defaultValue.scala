
object compiletime {
  erased def erasedValue[T]: T = compiletime.erasedValue
}

object Test extends App {

  inline def defaultValue[T] <: Option[Any] = inline compiletime.erasedValue[T] match {
    case _: Byte => Some(0: Byte)
    case c: Char => Some(0: Char)
    case d @ (_: Short) => Some(0: Short)
    case _: Int => Some(0)
    case _: Long => Some(0L)
    case _: Float => Some(0.0f)
    case _: Double => Some(0.0d)
    case _: Boolean => Some(false)
    case _: Unit => Some(())
    //case _: t >: Null => Some(null)
    case _ => None
  }

  val dInt = defaultValue[Int]
  val dDouble = defaultValue[Double]
  val dBoolean = defaultValue[Boolean]
  val dChar = defaultValue[Char]
  val dString = defaultValue[String]
  val dAny = defaultValue[Any]
  println(dInt)
  println(dDouble)
  println(dBoolean)
  println(dChar)
  println(dString)
  println(dAny)
  val cInt: Int = dInt.get
  val cDouble: Double = dDouble.get
  val cBoolean: Boolean = dBoolean.get
  val cChar: Char = dChar.get
  assert(dString.isEmpty)
  assert(dAny.isEmpty)
}