abstract class C {
  def y: Any
}

object test {
  val x = new C{
    def y: String = "abc"
  }
  val z: String = x.y
}

// A tricky case involving inner classes, exercised
// in the large in dotty.tools.dotc.core.NameKinds.scala.
object Test2 {

  class NK {
    class I
    type T
  }

  val x = new NK { type T = I }

  val y = {
    class C extends NK { type T = I }
    new C
  }
}
