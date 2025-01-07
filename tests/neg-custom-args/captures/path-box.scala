class A:
  val m: A^ = ???
  val self: this.type = this

case class Box[+T](value: T)

def testBox1(a: A^): Box[A^{a}] =
  Box(a.m)

def testBox2(a: A^): Box[A^{a.m}] =
  Box(a.m)

def testBox3(a: A^): Box[A^{a.m}] =
  Box(a) // error

def testBox4(a: A^): Box[A^{a.m}] =
  Box(a.m.m.m)

def testBox5(a: A^): Box[A^{a.m}] =
  Box(a.m.m.self)