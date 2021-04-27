sealed trait T

case class C(xs: Int*) extends T

def f(): Unit = (C(42): T) match { case C(xs*) => }
def g(): Unit = (C(42): T) match { case C(_*) => }

def h(): Unit = (C(42): T) match
  case C(5, _*) =>
  case C(x, xs*) =>
