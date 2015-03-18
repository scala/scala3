object Test {
  val w = new Array[String](10)
  val x = new Array[Int](10)
  def f[T: reflect.ClassTag] = new Array[T](10)
  val y = new Array[Any](10)
  val z = new Array[Unit](10)
}
object Test2 {
  val w: Array[Any] = new Array(10)
  val x: Array[Int] = new Array(10)
  def f[T: reflect.ClassTag]: Array[T] = new Array(10)
  val y: Array[Any] = new Array(10)
  val z: Array[Unit] = new Array(10)
}

