import language.experimental.captureChecking
trait Cap
case class Id[X](x: X)
def mkId[X](x: X): Id[X] = Id(x)
def withCap[X](op: (lcap: Cap^) => () => X): X = ???
def bar() =
  val leak = withCap(lcap => () => mkId(lcap)) // error
