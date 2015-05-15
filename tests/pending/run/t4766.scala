
import scala.language.postfixOps
import scala.language.reflectiveCalls

object Test extends dotty.runtime.LegacyApp {
  val x = new {
    def > = 1
  }

  println(x>)
}
