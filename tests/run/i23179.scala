object Test {
  // Basic case: SAM expects Array[AnyRef], impl has Array[? >: AnyRef]
  trait A { def f(a: Array[AnyRef]): Any }
  def g(a: A) = a.f(Array.empty[AnyRef])

  // Multiple array parameters
  trait MultiParam { def f(a: Array[AnyRef], b: Array[AnyRef]): Any }
  def gMulti(a: MultiParam) = a.f(Array.empty[AnyRef], Array.empty[AnyRef])

  // Mixed parameters: only some need adaptation
  trait MixedParam { def f(a: Array[AnyRef], b: Int): Any }
  def gMixed(a: MixedParam) = a.f(Array.empty[AnyRef], 42)

  // Generic return type
  trait GenericReturn[T] { def f(a: Array[AnyRef]): T }
  def gGeneric[T](a: GenericReturn[T]): T = a.f(Array.empty[AnyRef])

  def main(args: Array[String]): Unit = {
    // Basic case
    g((x: Array[? >: AnyRef]) => x.headOption)

    // Multiple array parameters - both need adaptation
    gMulti((x: Array[? >: AnyRef], y: Array[? >: AnyRef]) => (x.headOption, y.headOption))

    // Mixed - only first param needs adaptation
    gMixed((x: Array[? >: AnyRef], n: Int) => (x.headOption, n))

    // Generic return type
    val result: Option[?] = gGeneric((x: Array[? >: AnyRef]) => x.headOption)
  }
}
