class A:
  val m: A^ = ???
  var n: A^ = ???

def test1(a: A^) =
  val c1: A^{a.m} = a.m
  val f1: A^{a.n} = a.n // error