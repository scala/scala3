package dotty

import scala.reflect.runtime.universe.TypeTag
import scala.Predef.???

/** unimplemented implicit for TypeTag */
object DottyPredef {
  implicit def typeTag[T]: TypeTag[T] = ???

// not yet:
// def classOf[T](implicit ctag: ClassTag[T]): Class[T] =
//    ctag.runtimeClass
}
