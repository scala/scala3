import language.experimental.typeClauseInference

class Test:
  def test: Unit =
    val it1: [T] => T => T = x => x
    val it2: [T] => (T, Int) => T = (x, y: Int) => x
    val it3: [T, S <: List[T]] => (T, S) => List[T] = (x, y) => x :: y
    val tuple1: (String, String) = (1, 2.0).map[[_] =>> String](_.toString)
    val tuple2: (List[Int], List[Double]) = (1, 2.0).map(List(_))
    // Not supported yet, require eta-expansion with a polymorphic expected type
    // val tuple3: (List[Int], List[Double]) = (1, 2.0).map(List.apply)
