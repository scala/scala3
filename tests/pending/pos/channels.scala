// To compile this test, we need some more elaborate GADT capabilities.
// Not sure yet we should invest to get them.
class Channel[a]

import collection.mutable.Set

case class ![a](chan: Channel[a], data: a)

/*
object Bang {
  def unapply[a](x: ![a]): Option[{Channel[a], a}] =
    Some(x.chan, x.data)
}

*/
object Test extends App {
  object IC extends Channel[Int]
  def f[b](x: ![b]): Int = x match {
    case send: ![c] =>
      send.chan match {
        case IC => send.data  // Here, from the fact that `chan` is an IC, we need to conclude that `c` is Int.
      }
  }
}

object Test2 extends App {
  object IC extends Channel[Set[Int]]
  def f[b](s: ![b]): Set[Int] = s match {
    case IC ! x => x
  }
}
