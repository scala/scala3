object Test {

  class C { type I }
  type A = C

  def f1[T] = classOf[T] // error
  def f2[T <: String] = classOf[T] // error
  val x = classOf[Test.type] // error
  val y = classOf[C { type I = String }] // error
  val z = classOf[A] // ok
}
