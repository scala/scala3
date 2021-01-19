class A(val x: Int) with
  class B extends A(2) with
    println(x)

@main def Test =
  val a = A(1)
  a.B()
