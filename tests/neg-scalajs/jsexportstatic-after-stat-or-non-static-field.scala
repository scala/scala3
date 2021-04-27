import scala.scalajs.js
import scala.scalajs.js.annotation.*

class AfterVal extends js.Object

object AfterVal {
  val a: Int = 1

  // ---

  @JSExportStatic
  val b: Int = 1 // error

  @JSExportStatic
  var c: Int = 1 // error

  @JSExportStatic
  def d: Int = 1

  @JSExportStatic
  def d_=(v: Int): Unit = ()

  @JSExportStatic
  def e(): Int = 1
}

class AfterVar extends js.Object

object AfterVar {
  var a: Int = 1

  // ---

  @JSExportStatic
  val b: Int = 1 // error

  @JSExportStatic
  var c: Int = 1 // error

  @JSExportStatic
  def d: Int = 1

  @JSExportStatic
  def d_=(v: Int): Unit = ()

  @JSExportStatic
  def e(): Int = 1
}

class AfterStat extends js.Object

object AfterStat {
  val a: Int = 1

  // ---

  @JSExportStatic
  val b: Int = 1 // error

  @JSExportStatic
  var c: Int = 1 // error

  @JSExportStatic
  def d: Int = 1

  @JSExportStatic
  def d_=(v: Int): Unit = ()

  @JSExportStatic
  def e(): Int = 1
}

class OthersValid extends js.Object

object OthersValid {
  @JSExportStatic val a1: Int = 1
  @JSExportStatic var a2: Int = 1
  lazy val a3: Int = 1
  def a4: Int = 1
  def a4_=(v: Int): Unit = ()
  def a5(): Int = 1
  @JSExportStatic def a6: Int = 1
  @JSExportStatic def a6_=(v: Int): Unit = ()
  @JSExportStatic def a7(): Int = 1
  class A8
  object A9
  trait A10
  type A11 = Int

  // ---

  @JSExportStatic
  val b: Int = 1

  @JSExportStatic
  var c: Int = 1

  @JSExportStatic
  def d: Int = 1

  @JSExportStatic
  def d_=(v: Int): Unit = ()

  @JSExportStatic
  def e(): Int = 1
}
