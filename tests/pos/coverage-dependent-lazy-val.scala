def accept(x: AnyRef)(y: x.type): Unit = ()

def test: Unit =
  lazy val x = new Object
  accept(x)(x)
