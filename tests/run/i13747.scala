var res = ""
trait Bar:
  def +(that: Bar): Bar = new Plus(this, that)
  transparent inline def -(that: Bar): Bar = new Minus(this, that)

class LHS extends Bar {res += "LHS "}
class RHS extends Bar {res += "RHS "}

class Plus(lhs: Bar, rhs: Bar) extends Bar {res += "op"}
class Minus(lhs: Bar, rhs: Bar) extends Bar {res += "op"}

@main def Test =
  val pls = new LHS + new RHS
  val plsRes = res
  res = ""
  val min = new LHS - new RHS
  assert(plsRes == res)