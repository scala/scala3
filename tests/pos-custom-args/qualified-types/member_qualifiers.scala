type Pos = {v: Int with v >= 0}

class Params(val a: Int, val b: Int with b > a):
  val lo: Int with lo < b = a

case class ParamsCase(a: Int, b: Int with b > a):
  val lo: Int with lo < b = a

def methodParams(a: Int, b: Int with b > a): Unit =
  val z: Int with z < b = a

def fromOutside(c: Params): Unit =
  val z: Int with z < c.b = c.a

class Deep(val c: Params)

def compoundPrefix(d: Deep): Unit =
  val z: Int with z < d.c.b = d.c.a

trait Base:
  val a: Int
  val b: Int with b > a

def inherited(t: Base): Unit =
  val z: Int with z < t.b = t.a

class Aliased(val p: Pos, val q: Pos with q < p)

def throughAlias(e: Aliased): Unit =
  val z1: Int with z1 >= 0 = e.p
  val z2: Int with 0 <= z2 && z2 < e.p = e.q
