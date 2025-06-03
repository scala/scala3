class D1
class D2

class Test1:
  type Ds = List[D1] | List[D2]
  def m1(dss: List[Ds]) =
    dss.flatMap:
      case Seq(d)           => Some(1)
      case Seq(head, tail*) => Some(2)
      case Seq()            => None
