import scala.quoted.{_, given}
import Macros._

class Test {
  val a2: Unit = foo2()
}