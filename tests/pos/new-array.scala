object Test {
  val w = new Array[String](10)
  val x = new Array[Int](10)
  def f[T: reflect.ClassTag] = new Array[T](10)
  val y = new Array[Any](10)
  val z = new Array[Unit](10)
}
