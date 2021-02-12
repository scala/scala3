import scala.reflect.ClassTag
import scala.util.*

object Main {
  class A

  def constructAs[TTT <: A](implicit ev: ClassTag[TTT]): Try[TTT] = Try {
    new A()
  }.flatMap {
    case ev(inst) =>
      val b: TTT = inst
      Success(inst)
    case inst: TTT =>
      Success(inst)
    case _ =>
      val tag = implicitly[ClassTag[TTT]]
      Failure(new ClassCastException(s"Failed to construct instance of class ${tag.runtimeClass.getName}"))
  }
}
