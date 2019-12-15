class A {
  val a: Int = f()           // error
  def f(): Int = a * a
}