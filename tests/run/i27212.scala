import scala.reflect.ClassTag

object Test:
  def f[T: ClassTag] = Array.ofDim[Array[T]](1)

  def main(args: Array[String]): Unit =
    assert(f[String].getClass == classOf[Array[Array[String]]])
