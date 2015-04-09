package dotty

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

object DottyPredef {
  /** implicits for ClassTag and TypeTag. Should be implemented with macros */
  implicit def classTag[T]: ClassTag[T] = ???
  implicit def typeTag[T]: TypeTag[T] = ???
}
