class O:
  case class A(b: B):
    println(n)

  class B:
    val a = A(this)  // warn

  val b = new B
  val n = 10
end O
