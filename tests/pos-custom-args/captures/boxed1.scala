class Box[T](val x: T)

class Cap extends caps.Capability

def foo(x: => Int): Unit = ()

def test(c: Cap) =
  val f = () => { c; 1 }
  val _: () ->{c} Int = f
  val g = () => Box(f)
  val _: () ->{g} Box[() ->{f} Int] = g
