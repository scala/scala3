sealed trait T

case class C(xs: Int*) extends T

def f(): Unit = (C(42): T) match { case C(xs: _*) => }
def g(): Unit = (C(42): T) match { case C(_: _*) => }

def h(): Unit = (C(42): T) match
  case C(5, _: _*) =>
  case C(x, xs: _*) =>
