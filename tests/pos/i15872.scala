// From https://github.com/lampepfl/dotty/pull/15872#issuecomment-1218041165
trait Tag[S]
case class TupTag[A, T <: Tuple]() extends Tag[A *: T]

def tupleId[T <: Tuple](x: T): x.type = x

def foo[S](x: S, ev: Tag[S]) = ev.match {
  case _: TupTag[a, t] =>
    val t1: t = tupleId(x.tail)
}
