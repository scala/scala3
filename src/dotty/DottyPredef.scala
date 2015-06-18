package dotty

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.Predef.???  // this is currently ineffective, because of #530

abstract class I1 {
  implicit def classTag[T]: ClassTag[T] = scala.Predef.???
  implicit def typeTag[T]: TypeTag[T] = scala.Predef.???
  implicit val DoubleClassTag: ClassTag[Double]   = ClassTag.Double
}
abstract class I2 extends I1 {
  implicit val FloatClassTag: ClassTag[Double]   = ClassTag.Double
}
abstract class I3 extends I2 {
  implicit val LongClassTag: ClassTag[Long]       = ClassTag.Long
}
abstract class I4 extends I3 {
  implicit val IntClassTag: ClassTag[Int]         = ClassTag.Int
}
abstract class I5 extends I4 {
  implicit val ShortClassTag: ClassTag[Short]     = ClassTag.Short
}
abstract class I6 extends I5 {
  implicit val ByteClassTag: ClassTag[Byte]       = ClassTag.Byte
  implicit val CharClassTag: ClassTag[Char]       = ClassTag.Char
  implicit val BooleanClassTag: ClassTag[Boolean] = ClassTag.Boolean
  implicit val UnitClassTag: ClassTag[Unit]       = ClassTag.Unit
  implicit val NullClassTag: ClassTag[Null]       = ClassTag.Null
}

/** implicits for ClassTag and TypeTag. Should be implemented with macros */
object DottyPredef extends I6 {

  /** ClassTags for final classes */
  implicit val NothingClassTag: ClassTag[Nothing] = ClassTag.Nothing
}
