object A:
  val a: Int = 10
  val b: Int = 20

object B:
  var n: Int = A.a * A.b

@main def entry() = println(B.n)