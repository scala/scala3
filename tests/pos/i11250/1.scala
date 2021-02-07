package shapeless3.data

import scala.compiletime._

trait Monoidal {
  type to[_] <: Tuple
  type length[m] = Monoidal.length[to[m]]
}

object Monoidal {
  import Tuple._

  type length[m <: Tuple] = Size[m]
}

trait UnboundedMonoidal[T0[_, _], U0] extends Monoidal {
  type to[t] <: Tuple = t match {
    case T0[hd, tl] => hd *: to[tl]
    case U0 => EmptyTuple
  }
}

object pairs extends UnboundedMonoidal[Tuple2, Unit]

object MonoidalTest { // Compiles fine here
  type p = (Int, (String, (Boolean, Unit)))
  summon[pairs.length[p] =:= 3]
}

