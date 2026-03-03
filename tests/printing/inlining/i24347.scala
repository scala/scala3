class Box(val value: Int)

inline def unbox(b: Box): Int =
  inline b match
    case b: Box => b.value

@main def Test =
  val b = Box(42)
  unbox(b)
