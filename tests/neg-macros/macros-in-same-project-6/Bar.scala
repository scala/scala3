import scala.quoted._

object Bar {
  Foo.myMacro() // error
}
