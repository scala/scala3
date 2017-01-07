package scala
import scala.reflect.ClassTag

trait Selectable extends Any {
  def selectDynamic(name: String): Any
  def selectDynamicMethod(name: String, paramClasses: ClassTag[_]*): Any =
    new UnsupportedOperationException("selectDynamicMethod")
}
