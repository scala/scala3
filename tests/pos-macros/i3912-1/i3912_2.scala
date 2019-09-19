import scala.quoted.{_, given}
import Macros._

class Test {
  val a: Unit = foo()

}