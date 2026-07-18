import scala.reflect.ClassTag

object MyArray:
  def empty[T: ClassTag]: Array[Array[T]] = new Array[Array[T]](0)

@main def Test =
  val arr: Array[Array[String]] = MyArray.empty[String]
  assert(arr.length == 0)
