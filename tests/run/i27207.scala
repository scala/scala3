import scala.reflect.ClassTag

object Test:

  def f[T: ClassTag](xs: Vector[T]): IArray[IArray[T]] =
    IArray(IArray.from(xs))

  def main(args: Array[String]): Unit =
    val x: IArray[IArray[String]] = f(Vector("s"))
    assert(x.length == 1)
    assert(x(0)(0) == "s")
    assert(x.getClass == classOf[Array[Array[String]]])
