import compiletime.*
object Test {
  type T[X] = X match {
    case String => Int
    case Int => String
  }

  type Len[X] <: Int = X match {
    case Unit => 0
    case x *: xs => S[Len[xs]]
  }

  type T2 = Len[(1, 2, 3)]
  erased val x: 3

  type T1 = S[0]

  erased val x2: 1

  erased val y0: S[S[S[0]]]
  erased val z0: T2

  type Head[X <: Tuple] = X match {
    case (x1, _) => x1
  }

  erased val y1: Int
  erased val z1: Head[(Int, String)]

  type Concat[X <: Tuple, Y <: Tuple] <: Tuple = X match {
    case Unit => Y
    case x1 *: xs1 => x1 *: Concat[xs1, Y]
  }

  type Elem[X <: Tuple, N] = X match {
    case x *: xs =>
      N match {
        case 0 => x
        case S[n1] => Elem[xs, n1]
      }
  }

  type Elem1[X <: Tuple, N] = (X, N) match {
    case (x *: xs, 0) => x
    case (x *: xs, S[n1]) => Elem1[xs, n1]
  }

  erased val x3: String
  erased val x4: Int

  erased val y2: Elem[(String, Int, Boolean), 0]
  erased val z2: String
  erased val y3: Elem1[(String, Int, Boolean), 1]
  erased val z3: Int
  erased val y4: Elem[(String, Int, Boolean), 2]
  erased val z4: Boolean

  erased val y5: Concat[Unit, (String, Int)]
  erased val z5: (String, Int)
  erased val y6: Concat[(Boolean, Boolean), (String, Int)]
  erased val z6: Boolean *: Boolean *: (String, Int)
  erased val y7: (Boolean, Boolean, String, Int)
  erased val z7: Concat[(Boolean, Boolean), String *: Int *: EmptyTuple]

  def index[Xs <: NonEmptyTuple](xs: Xs, n: Int): Elem[Xs, n.type] = xs(n).asInstanceOf

  val test = (1, "hi", true, 2.0)
  index(test, 0): Int
  index(test, 1): String
}