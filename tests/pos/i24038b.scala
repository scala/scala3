final class MBufferLong

type M[Tup <: Tuple] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t     => MBufferLong *: M[t]

def appendBuffers[T <: Tuple](t: T, buffers: M[T]): Unit = {
  (t, buffers) match
    case abcd: (h *: t, bh *: bt) =>
      val (hh *: tt, bh *: bt) = abcd
      summon[hh.type <:< h]
}
