import scala.quoted.*

object Bar {
  Foo.myMacro() // error
}
