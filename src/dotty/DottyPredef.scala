package dotty

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.Predef.???  // this is currently ineffective, because of #530

object DottyPredef {
  /** implicits for ClassTag and TypeTag. Should be implemented with macros */
  implicit def classTag[T]: ClassTag[T] = scala.Predef.???
  implicit def typeTag[T]: TypeTag[T] = scala.Predef.???
  
  implicit val IntClassTag     = ClassTag.Int
  implicit val ByteClassTag    = ClassTag.Byte
  implicit val ShortClassTag   = ClassTag.Short
  implicit val CharClassTag    = ClassTag.Char
  implicit val LongClassTag    = ClassTag.Long
  implicit val FloatClassTag   = ClassTag.Float
  implicit val DoubleClassTag  = ClassTag.Double
  implicit val BooleanClassTag = ClassTag.Boolean
  implicit val UnitClassTag    = ClassTag.Unit
  implicit val AnyClassTag     = ClassTag.Any
  implicit val AnyRefClassTag  = ClassTag.AnyRef
  implicit val AnyValClassTag  = ClassTag.AnyVal
  implicit val ObjectClassTag  = ClassTag.Object
  implicit val NullClassTag    = ClassTag.Null
  implicit val NothingClassTag = ClassTag.Nothing
}
