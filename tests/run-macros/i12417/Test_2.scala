import scala.deriving.Mirror
import scala.compiletime.{constValue, error}

object Test extends App {
  case class A(x: String, y: Int)
  assert(TestMacro.test1[A] == "Some((x,y))")
}
