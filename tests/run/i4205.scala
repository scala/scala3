import scala.reflect.ClassTag
import scala.runtime.BoxedUnit

object Test {
  def main(args: Array[String]): Unit = {
    println(implicitly[ClassTag[Unit]] == ClassTag.Unit)
    println(implicitly[ClassTag[Boolean]] == ClassTag.Boolean)
    println(implicitly[ClassTag[Byte]] == ClassTag.Byte)
    println(implicitly[ClassTag[Char]] == ClassTag.Char)
    println(implicitly[ClassTag[Short]] == ClassTag.Short)
    println(implicitly[ClassTag[Int]] == ClassTag.Int)
    println(implicitly[ClassTag[Long]] == ClassTag.Long)
    println(implicitly[ClassTag[Float]] == ClassTag.Float)
    println(implicitly[ClassTag[Double]] == ClassTag.Double)
    println(implicitly[ClassTag[Object]] == ClassTag.Object)
    println(implicitly[ClassTag[Any]] == ClassTag.Any)
    println(implicitly[ClassTag[AnyRef]] == ClassTag.AnyRef)
    println(implicitly[ClassTag[AnyVal]] == ClassTag.AnyVal)

    println(implicitly[ClassTag[BoxedUnit]] != ClassTag.Unit)
  }
}
