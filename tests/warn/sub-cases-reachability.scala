import scala.language.experimental.subCases

enum Color:
  case Red, Green, Blue

case class Wrapper(c: Color)

enum AB:
  case A, B

def testBoundVarReachability(ab: AB) = ab match
  case x if x match
    case AB.A => "a"
    case AB.B => "b"
  case AB.A => "unreachable" // warn

def testParamIndexReachability(w: Wrapper) = w match
  case Wrapper(c) if c match
    case Color.Red   => "red"
    case Color.Green => "green"
  case Wrapper(Color.Red)  => "unreachable" // warn
  case Wrapper(Color.Blue) => "blue"        // not unreachable

def testCombinedReachability(w: Wrapper) = w match
  case Wrapper(c) if c match
    case Color.Red   => "red"
    case Color.Green => "green"
  case Wrapper(c) if c match
    case Color.Blue  => "blue"
  case Wrapper(_) => "unreachable" // warn

def testBindWrappingUnApply(w: Wrapper) = w match
  case x @ Wrapper(c) if c match
    case Color.Red   => "red"
    case Color.Green => "green"
    case Color.Blue  => "blue"
  case Wrapper(_) => "unreachable" // warn

def testNestedSubcases(c: Color): String = c match
  case c1 if c1 match
    case Color.Red => "red"
    case c2 if c2 match
      case Color.Green => "green"
      case Color.Blue  => "blue"
  case Color.Blue => "unreachable" // warn

def testAlternativeReachability(c: Color): String = c match
  case c1 if c1 match
    case Color.Red | Color.Green | Color.Blue => "all"
  case Color.Red => "unreachable" // warn

enum Version:
  case Legacy
  case Stable(major: Int, minor: Int)

case class Document(title: String, version: Version)

def tupleFirstReachability(pair: (Color, Color)): String = pair match
  case (a, b) if a match
    case Color.Red   => "red first"
    case Color.Green => "green first"
    case Color.Blue  => "blue first"
  case (Color.Red, _) => "unreachable" // warn

def fieldAccessReachability(d: Document): String = d match
  case x if x.version match
    case Version.Stable(m, n) => s"$m.$n"
    case Version.Legacy => "legacy"
  case Document(_, Version.Legacy) => "unreachable" // warn
