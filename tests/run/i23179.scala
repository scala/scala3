object Test {
  trait A { def f(a: Array[AnyRef]): Any }
  def g(a: A) = a.f(Array.empty[AnyRef])

  def main(args: Array[String]): Unit = {
    g((x: Array[? >: AnyRef]) => x.headOption)
  }
}
