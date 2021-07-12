enum E:
  case C

trait T

def f(x: E | T): Unit = x match {
  case e: E => ()
  case t: T => ()
}
