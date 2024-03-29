sealed trait Plan[+E <: Err, +F[+e <: E] <: Fal[e]]

case class Err()                                   extends Plan[Err, Nothing]
case class Fal[+E <: Err]()                        extends Plan[E,   Fal]
case class Btw[+E <: Err, +F[+e <: E] <: Fal[e]]() extends Plan[E,   F]

class Tests:
  def test1(plan: Plan[Err, Nothing]): Unit = plan match
    case Err() =>
    case Btw() =>

  def main1 = test1(Btw())

/*

Previously, Plan[Err, Nothing] dropped Btw,
because it didn't instantiate ?F to Nothing

    <== decompose(Plan[Err, Nothing], [Btw[Err, Fal]]) = [Not, Err]
  <== decompose(Btw[Err, Fal] & Plan[Err, Nothing]) = []
<== simplify(Prod(Btw())) = Empty

*/
