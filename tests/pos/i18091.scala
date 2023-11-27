trait B(val y: Int)

class C extends B(20)  {
  def foo(): Unit = println(y)
}