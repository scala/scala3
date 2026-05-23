// scalajs: --skip

class Foo:
  @throws[T]
  def sneakyThrow[T <: Throwable, R](t: Throwable): R =
    throw t.asInstanceOf[T]

  @throws[T]("always")
  def typedThrow[T <: RuntimeException](t: T): Unit =
    throw t

object Test:
  def main(args: Array[String]): Unit =
    val m1 = classOf[Foo].getDeclaredMethod("sneakyThrow", classOf[Throwable])
    val ex1 = m1.getExceptionTypes.toList
    assert(ex1 == List(classOf[Throwable]),
      s"sneakyThrow expected List(Throwable), got $ex1")

    val m2 = classOf[Foo].getDeclaredMethod("typedThrow", classOf[RuntimeException])
    val ex2 = m2.getExceptionTypes.toList
    assert(ex2 == List(classOf[RuntimeException]),
      s"typedThrow expected List(RuntimeException), got $ex2")
