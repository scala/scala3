class A:
  // should report one error here
  val b = new B(this) // error
  val m = 10
  val n = 20

class B(a: A):
  val x = a.m
  val y = a.n
