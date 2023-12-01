// scalac: -Yno-experimental

import scala.quoted.*

def foo(using Quotes): Unit =
  (??? : Type[?]) match
    case '[ (t, t, t) ] => // error // error
  '{ ??? : Any } match
     case '{ type u; $x: u } => // error
     case '{ type u; ($ls: List[u]).map($f: u => Int) } => // error // error

