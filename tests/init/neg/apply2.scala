object O:
  case class A(b: B):
    println(n)

  class B:
    val a = A(this)  // error

  val b = new B
  val n = 10
end O
