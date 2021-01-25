class A(val x: Int):
  class B extends A(2):
    println(x)

@main def Test =
  val a = A(1)
  a.B()
