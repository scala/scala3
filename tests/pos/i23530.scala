trait TestContainer:
  trait TestPath:
    type AbsMember

  extension (path: TestPath)
    infix def ext(color: path.AbsMember): Unit = ???
    infix def ext(other: Int): Unit = ???

object Repro:
  val dc2: TestContainer = ???
  import dc2.TestPath

  def transition(path: TestPath)(using DummyImplicit): TestPath = ???

  def test: Unit =
    val di: TestPath = ???
    // error
    val z1 = transition(di).ext(1)
