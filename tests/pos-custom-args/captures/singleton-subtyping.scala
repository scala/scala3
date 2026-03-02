class Box[+T](x: T)

def Test(c: Object^): Unit =
  val x: Object^{c} = c

  val x2: x.type^{x} = x
  val x3: x.type = x2

  val b: Box[x.type] = Box(x)
  val b1: Box[x.type^{x}] = b
  val b2: Box[x.type] = b1





