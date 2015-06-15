import scala.concurrent._
import ExecutionContext.Implicits.global

object Test extends dotty.runtime.LegacyApp {
  Macros.foo
}