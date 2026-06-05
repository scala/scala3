case class Test()

@main def test =
  Test() match
    case Test(x*) => () // error
  Test() match
    case Test(_*) => () // error
