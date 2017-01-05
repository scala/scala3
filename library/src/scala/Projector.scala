package scala
import scala.reflect.ClassTag
import scala.annotation.implicitNotFound

@implicitNotFound("no projector instance found to implement reflective access to structural type ${T}")
trait Projector[-T] extends Any {
  def get(receiver: T, name: String): Any
  def getMethod(receiver: T, name: String, paramClasses: ClassTag[_]*): Any =
    new UnsupportedOperationException("getMethod")
}
