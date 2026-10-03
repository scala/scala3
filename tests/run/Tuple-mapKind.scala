object Test {

  type Tup = (Int, Double, Boolean, String)

  def main(args: Array[String]): Unit = {

    val headOption: [x] => Seq[x] => Option[x] = [x] => (seq: Seq[x]) => seq.headOption
    val firstTruthy: [T <: Tuple.Union[Tup]] => Seq[T] => Option[T] = [T <: Tuple.Union[Tup]] =>
      (seq: Seq[T]) => seq.find {
        case x: Int => x > 0
        case x: Double => x > 0.0
        case x: Boolean => x
        case x: String => x.nonEmpty
      }

    val seqTuple: Tuple.Map[Tup, Seq] = (Vector(0, 1), Seq.empty, List(false, true), Vector("4", "44"))
    val expectHeadOptionTuple: Tuple.Map[Tup, Option] = (Some(0), None, Some(false), Some("4"))
    val expectFirstTruthyTuple: Tuple.Map[Tup, Option] = (Some(1), None, Some(true), Some("4"))

    assert(
      EmptyTuple.mapKind[Seq, Option](headOption) == EmptyTuple
    )
    assert(
      EmptyTuple.mapKind[Seq, Option](firstTruthy) == EmptyTuple
    )
    assert(
      (List("", "1") *: EmptyTuple).mapKind[Seq, Option](headOption) == (Some("") *: EmptyTuple)
    )
    assert(
      (List("", "1") *: EmptyTuple).mapKind[Seq, Option](firstTruthy) == (Some("1") *: EmptyTuple)
    )
    assert(
      seqTuple.mapKind[Seq, Option](headOption) == expectHeadOptionTuple
    )
    assert(
      seqTuple.mapKind[Seq, Option](firstTruthy) == expectFirstTruthyTuple
    )
  }
}
