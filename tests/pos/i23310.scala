
object Example {
    val pf: PartialFunction[Unit, Unit] = s => (s match {
      case a => a
    }) match {
      case a => ()
    }
}

object ExampleB:
  def test =
    List(42).collect:
      _.match
        case x => x
      .match
        case y => y + 27
