import java.sql.Date

class C extends caps.Capability
class D

def Test(c1: C, c2: C) =
  val d: D = ???
  val d1: D^{c1} = ???
  val d2: D^{c2} = ???
  val x1 = if ??? then d else d1
  val _: D^{c1} = x1
  val x2 = if ??? then d1 else d
  val _: D^{c1} = x2
  val x3 = if ??? then d1 else d2
  val _: D^{c1, c2} = x3

  val _: D = x1 // error
  val _: D = x2 // error
  val _: D = x3 // error

