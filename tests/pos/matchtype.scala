import typelevel._
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
  erased val x: Exactly[3] = typeOf[T2]

  type T1 = S[0]

  erased val x2: Exactly[1] = typeOf[T1]

  erased val y0: Exactly[S[S[S[0]]]] = typeOf[T2]
  erased val z0: Exactly[T2] = typeOf[S[S[S[0]]]]

  type Head[X <: Tuple] = X match {
    case (x1, _) => x1
  }

  erased val y1: Exactly[Int] = typeOf[Head[(Int, String)]]
  erased val z1: Head[(Int, String)] = 22

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

  erased val x3: Exactly[String] = typeOf[Elem[(String, Int), 0]]
  erased val x4: Exactly[Int] = typeOf[Elem1[(String, Int), 1]]

  erased val y2: Exactly[Elem[(String, Int, Boolean), 0]] = typeOf[String]
  erased val z2: Exactly[String] = typeOf[Elem[(String, Int, Boolean), 0]]
  erased val y3: Exactly[Elem1[(String, Int, Boolean), 1]] = typeOf[Int]
  erased val z3: Exactly[Int] = typeOf[Elem1[(String, Int, Boolean), 1]]
  erased val y4: Exactly[Elem[(String, Int, Boolean), 2]] = typeOf[Boolean]
  erased val z4: Exactly[Boolean] = typeOf[Elem[(String, Int, Boolean), 2]]

  erased val y5: Exactly[Concat[Unit, (String, Int)]] = typeOf[(String, Int)]
  erased val z5: Exactly[(String, Int)] = typeOf[Concat[Unit, (String, Int)]]
  erased val y6: Exactly[Concat[(Boolean, Boolean), (String, Int)]] = typeOf[Boolean *: Boolean *: (String, Int)]
  erased val z6: Exactly[Boolean *: Boolean *: (String, Int)] = typeOf[Concat[(Boolean, Boolean), (String, Int)]]
  erased val y7: Exactly[(Boolean, Boolean, String, Int)] = typeOf[Concat[(Boolean, Boolean), String *: Int *: Unit]]
  erased val z7: Exactly[Concat[(Boolean, Boolean), String *: Int *: Unit]] = typeOf[(Boolean, Boolean, String, Int)]

  def index[Xs <: NonEmptyTuple](xs: Xs, n: Int): Elem[Xs, n.type] = xs(n).asInstanceOf

  val test = (1, "hi", true, 2.0)
  index(test, 0): Int
  index(test, 1): String
}
