class Box[T](val x: T)

@annotation.capability class Cap

def foo(x: => Int): Unit = ()

def test(c: Cap) =
  val f = () => { c; 1 }
  val _: {c} () -> Int = f
  val g = () => Box(f)
  val _: () -> Box[{f} () -> Int] = g
