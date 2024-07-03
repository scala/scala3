object repro:
  abstract class Mapper[A, B] extends (A => B)

  given Mapper[Int, Double] with
    inline def apply(v: Int): Double = v.toDouble
