import Opaque.*

object Test {
  type FindField[R <: scala.Tuple, K] = R match {
    case FieldType[K, f] *: t => f
    case _ *: t => FindField[t, K]
  }

  val f: FieldType["A", Int] = ???
  val f1: Int = f
  //val f2: Int = f

  type R = FieldType["A", Int] *: FieldType["B", Double] *: FieldType["C", String] *: FieldType["D", Boolean] *: EmptyTuple
  summon[FindField[R, "B"] =:= Double] // error
}
