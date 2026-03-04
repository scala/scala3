case class Test()

object Test:
  def unapply(t: Test): Some[Seq[Int]] = Some(Seq(1, 2))

@main def test =
  Test() match
    case Test(x*) => () // error
  Test() match
    case Test(_*) => () // error
