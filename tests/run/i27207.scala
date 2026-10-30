import scala.reflect.ClassTag

object Test:

  def f1[T: ClassTag](xs: Vector[T]): IArray[IArray[T]] =
    IArray(IArray.from(xs))

  def f2[T: ClassTag] = new Array[IArray[T]](1)

  def main(args: Array[String]): Unit =
    val x: IArray[IArray[String]] = f1(Vector("s"))
    assert(x.length == 1)
    assert(x(0)(0) == "s")
    assert(x.getClass == classOf[Array[Array[String]]])

    assert(f2[String].getClass == classOf[Array[Array[String]]])

