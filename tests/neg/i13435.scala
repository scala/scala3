type Axis = String&Singleton
type ShapeTuple = Tuple1[(Axis, Int)]|Tuple2[(Axis, Int), (Axis, Int)]
type Shape = (Axis, Int) |ShapeTuple


def mkSchema(s: Shape) =
  s match
    case (dim: Axis, size: Int) => dim // error