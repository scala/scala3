class C[T] {
  val a: T = method
  def method = b
  val b: T = a     // warn
}