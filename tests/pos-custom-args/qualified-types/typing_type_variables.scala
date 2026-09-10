class Box[T](val value: T)
def makeBox[T](x: T): Box[T] with true = Box(x)

def test: Unit =
  val box1 = Box(3)
  val box1b: Box[Int] with true = Box(3)

  val box2 = makeBox(3)
  val box2b: Box[Int] with true = makeBox(3)
