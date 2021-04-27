import scala.scalajs.js
import scala.scalajs.js.annotation.*

class StaticContainer extends js.Object

object StaticContainer {
  @JSExportStatic // error
  object A

  @JSExportStatic // error
  trait B

  @JSExportStatic // error
  class C

  class D {
    @JSExportStatic // error
    def this(x: Int) = this()
  }

  @JSExportStatic // error
  lazy val e: Int = 1
}
