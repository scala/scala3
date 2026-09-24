package i10846 {
  final class V(val x: Int) extends AnyVal
  object V { def get: Option[V] = ??? }

  final class U(val y: String) extends AnyVal
  object U { def get: Option[U] = ??? }

  final class W[T](val z: T) extends AnyVal
  object W { def get: Option[W[Int => String]] = ??? }
}


object Test extends scala.App {
  def check[T](implicit tt: reflect.ClassTag[T]): Unit = {
    val companion = tt.runtimeClass.getClassLoader.loadClass(tt.runtimeClass.getName + '$')
    val get = companion.getMethod("get")
    assert(get.getReturnType == classOf[Option[_]])
    println(s"${tt.runtimeClass.getName}: ${get.getGenericReturnType}")
  }

  import i10846._

  check[V]
  check[U]
  check[W[_]]
}
