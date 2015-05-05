package dotty

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.Predef.???  // this is currently ineffective, because of #530

object DottyPredef {
  /** implicits for ClassTag and TypeTag. Should be implemented with macros */
  implicit def classTag[T]: ClassTag[T] = scala.Predef.???
  implicit def typeTag[T]: TypeTag[T] = scala.Predef.???


  /** ClassTags for final classes */
  implicit val IntClassTag: ClassTag[Int]         = ClassTag.Int
  implicit val ByteClassTag: ClassTag[Byte]       = ClassTag.Byte
  implicit val ShortClassTag: ClassTag[Short]     = ClassTag.Short
  implicit val CharClassTag: ClassTag[Char]       = ClassTag.Char
  implicit val LongClassTag: ClassTag[Long]       = ClassTag.Long
  implicit val FloatClassTag: ClassTag[Float]     = ClassTag.Float
  implicit val DoubleClassTag: ClassTag[Double]   = ClassTag.Double
  implicit val BooleanClassTag: ClassTag[Boolean] = ClassTag.Boolean
  implicit val UnitClassTag: ClassTag[Unit]       = ClassTag.Unit
  implicit val NullClassTag: ClassTag[Null]       = ClassTag.Null
  implicit val NothingClassTag: ClassTag[Nothing] = ClassTag.Nothing
}
