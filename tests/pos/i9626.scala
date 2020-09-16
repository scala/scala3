inline def scaffolding(inline op: Unit): Unit =
  val _ = op

def test = scaffolding { println("foo") }
