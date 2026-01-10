import scala.quoted.*

def call() = 0
def foo(using Quotes) =

  val x: Type[Int] = ???

  val '{$t0} = '{0} // ok
  val '{$t1: Int} = '{0} // ok
  val '{call(); $t2: tpe} = '{0} // warn
  val '[t] = x // ok
  val '[type t <: String; t] = x // warn
  val '[List[t]] = x // warn

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

  (x, x) match // ok
    case ('[t], '[q]) => ()

  (x, x) match // warn
    case ('[type t <: Number; t], '[q]) => ()

  (x, x) match // warn
    case ('[List[t]], '[q]) => ()

  (x, x) match // warn
    case ('[List[String]], '[q]) => ()
