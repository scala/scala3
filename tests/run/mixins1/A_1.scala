trait A[Self <: A[Self] with B] { self: B =>
  var x = 3
  println("hi")
  val y = x * x

  def f: Int = x + y

  def f(z: Int): Int = f + z
}

trait B extends A[B]
