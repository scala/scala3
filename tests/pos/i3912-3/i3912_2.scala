import scala.quoted.{_, given}
import Macros._

class Test {
  val a3: Unit = foo3()
}