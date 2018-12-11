package scala
import scala.reflect.ClassTag

trait Selectable extends Any {
  def selectDynamic(name: String): Any
  def applyDynamic(name: String, paramClasses: ClassTag[_]*)(args: Any*): Any =
    new UnsupportedOperationException("applyDynamic")
}
