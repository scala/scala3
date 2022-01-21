object O:
  case class A(b: B):
    println(n)

  class B:
    val a = A(this)

  val b = new B
  val n = 10         // error
end O
