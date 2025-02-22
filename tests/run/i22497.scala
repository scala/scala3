// scalajs: --skip

import scala.annotation.publicInBinary
import scala.annotation.experimental

@experimental
class Foo:
  @publicInBinary private def this(i: Int) = this()
  @publicInBinary protected def this(i: String) = this()

@experimental
@main def Test =
  println(classOf[Foo].getConstructors().mkString("\n"))
