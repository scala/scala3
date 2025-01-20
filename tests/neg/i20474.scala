class A
class B extends A

def f(a: A, c: A) =
  val b1: a.type = a
  val b2: a.type & B = a.asInstanceOf[a.type & B]
  val b3: c.type & A = c
  val b4: a.type | c.type = c

  val d1: b1.type = a
  val d2: b2.type = a // ok
  val d3: b3.type = a // error
  val d4: b4.type = a // error