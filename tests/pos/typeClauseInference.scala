import language.experimental.typeClauseInference

class Test:
  def test: Unit =
    val it1: [T] => T => T = x => x
    val it2: [T] => (T, Int) => T = (x, y: Int) => x
    val it3: [T, S <: List[T]] => (T, S) => List[T] = (x, y) => x :: y
    val tuple1: (String, String) = (1, 2.0).map[[_] =>> String](_.toString)
    val tuple2: (List[Int], List[Double]) = (1, 2.0).map(List(_))

    // Eta-expansion
    val e1: [T] => T => Option[T] = Option.apply
    val tuple3: (Option[Int], Option[Double]) = (1, 2.0).map(Option.apply)

    // Eta-expansion that wouldn't work with the original SIP-49
    def pair[S, T](x: S, y: T): (S, T) = (x, y)
    val f5: [T] => (Int, T) => (Int, T) = pair
    val f6: [T] => (T, Int) => (T, Int) = pair
    def id[T](x: T): T = x
    val f7: [S] => List[S] => List[S] = id
