import scala.quoted.*

def call() = 0
def foo(using Quotes) =

  val x: Type[Int] = ???
  val hkt: Type[List] = ???

  val '{$t0} = '{0} // ok
  val '{$t1: Int} = '{0} // ok
  val '{$t2: t} = '{0} // ok
  val '{call(); $t3: tpe} = '{0} // warn
  val '[t] = x // ok
  val '[type t <: String; t] = x // warn
  val '[List[t]] = x // warn
  val '[t] = hkt // warn

  // wrapped in other types
  ('{1}, '{0}) match // ok
    case ('{$y}, '{$z}) => ()

  ('{1}, '{0}) match // ok
    case ('{$y: Int}, '{$z}) => ()

  ('{1}, '{""}) match // warn
    case ('{$y}, '{$z: Int}) => ()

  ('{0}, '{1}) match // warn
    case ('{0}, '{1}) => ()

  ('{0}, '{1}) match // warn
    case ('{call(); $y}, '{$z}) => ()

  ('{1}, x) match // ok
    case ('{$y}, '[t]) => ()

  ('{1}, x) match // ok
    case ('{$y: t}, '[q]) => ()

  (x, x) match // ok
    case ('[t], '[q]) => ()

  (x, x) match // warn
    case ('[type t <: Number; t], '[q]) => ()

  (x, x) match // warn
    case ('[List[t]], '[q]) => ()

  (x, x) match // warn
    case ('[List[String]], '[q]) => ()

  // individual
  '{1} match // ok
    case '{$y} => ()

  '{1} match // ok
    case '{$y: Int} => ()

  '{1} match // ok
    case '{$y: t} => ()

  '{""} match // warn
    case '{$z: Int} => ()

  '{0} match // warn
    case '{0} => ()

  '{0} match // warn
    case '{call(); $y} => ()

  x match // ok
    case '[t] => ()

  x match // warn
    case '[type t <: Number; t] => ()

  x match // warn
    case '[List[t]] => ()

  x match // warn
    case '[List[String]] => ()

  x match // warn
    case '[Int] => ()

  hkt match // warn
    case '[t] => ()

