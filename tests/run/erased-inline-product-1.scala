import scala.compiletime.{erasedValue, summonInline, error}

inline def sizeTuple[T<: Tuple](): Long =
  inline erasedValue[T] match
    case _: EmptyTuple => 0
    case _: (h *: t) => size[h] + sizeTuple[t]()

inline def sizeProduct[T <: Product](m: scala.deriving.Mirror.ProductOf[T]): Long =
  sizeTuple[m.MirroredElemTypes]()

inline def size[T]: Long =
  inline erasedValue[T] match
      case _: Char => 2
      case _: Int => 4
      case _: Long => 8
      case _: Double => 8
      case p: Product => sizeProduct(summonInline[scala.deriving.Mirror.ProductOf[p.type]])
      case _ => error(s"unsupported type")

@main def Test =
  assert(size[(Int, Long)] == 12)
