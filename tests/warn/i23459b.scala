case class Test()
object Test:
  def unapply(t: Test): Some[Seq[Int]] = Some(Seq(1, 2))

@main def run(): Unit =
  Test() match
    case Test(_*) => () // warn

