package p

trait S:
  sealed trait A
  private class CA() extends A

object O extends S

trait T

class Test:
  def f(e: T) = e match
    case _: O.A =>
    case _      =>
