package dotty

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.Predef.???  // this is currently ineffective, because of #530

object DottyPredef {
  /** implicits for ClassTag and TypeTag. Should be implemented with macros */
  implicit def classTag[T]: ClassTag[T] = scala.Predef.???
  implicit def typeTag[T]: TypeTag[T] = scala.Predef.???
}
