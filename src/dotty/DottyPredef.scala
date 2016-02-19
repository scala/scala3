package dotty

import scala.reflect.runtime.universe.TypeTag
import scala.reflect.ClassTag
import scala.Predef.{???, implicitly}

/** unimplemented implicit for TypeTag */
object DottyPredef {
  implicit def typeTag[T]: TypeTag[T] = ???

  implicit def arrayTag[T](implicit ctag: ClassTag[T]): ClassTag[Array[T]] = {
    val ctag1 =
      if (ctag == ClassTag.Unit) implicitly[ClassTag[scala.runtime.BoxedUnit]]
      else ctag
    ctag1.wrap.asInstanceOf[ClassTag[Array[T]]]
  }

  def classOf[T](implicit ctag: ClassTag[T]): Class[T] =
    ctag.runtimeClass.asInstanceOf[Class[T]]
}
