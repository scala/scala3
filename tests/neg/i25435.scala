case class Test()

object Test:
  def unapplySeq(t: Test): Some[Seq[Int]] = Some(Seq(1, 2))

@main def test =
  Test() match
    case Test(x*) => println(x) // error // error
  Test() match
    case Test(_*) => () // error
