object Test extends App {
  import scala.typelevel._

  inline def defaultValue[T]: Option[T] = inline typeOf[T] match {
    case _: Subtype[Singleton] => Some(constValue[T])
    case _: Exactly[Byte] => Some(0: Byte)
    case c: Exactly[Char] => Some(0: Char)
    case d @ (_: Exactly[Short]) => Some(0: Short)
    case _: Exactly[Int] => Some(0)
    case _: Exactly[Long] => Some(0L)
    case _: Exactly[Float] => Some(0.0f)
    case _: Exactly[Double] => Some(0.0d)
    case _: Exactly[Boolean] => Some(false)
    case _: Exactly[Unit] => Some(())
    case _: Supertype[Null] => Some(null)
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
