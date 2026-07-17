class Params(val a: Int, val b: Int with b > a)

class Deep(val c: Params)

def wrongDirection(c: Params): Unit =
  val z: Int with z < c.a = c.b // error

def wrongDirectionCompound(d: Deep): Unit =
  val z: Int with z < d.c.a = d.c.b // error

def unrelatedInstances(c1: Params, c2: Params): Unit =
  val z: Int with z < c1.b = c2.a // error

class SelfJustifying:
  def bad: Int with bad == 42 = 0 // error
