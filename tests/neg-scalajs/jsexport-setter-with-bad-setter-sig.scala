import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  @JSExport // error
  def badParamList_=(x: Int, y: Int): Unit = ()

  @JSExport // error
  def badResultType_=(x: Int): String = "string"

  @JSExport // error
  def varArgs_=(x: Int*): Unit = ()

  @JSExport // error
  def defaultParam_=(x: Int = 1): Unit = ()
}

class B extends js.Object
object B {
  @JSExportStatic // error
  def badParamList_=(x: Int, y: Int): Unit = ()

  @JSExportStatic // error
  def badResultType_=(x: Int): String = "string"

  @JSExportStatic // error
  def varArgs_=(x: Int*): Unit = ()

  @JSExportStatic // error
  def defaultParam_=(x: Int = 1): Unit = ()
}
