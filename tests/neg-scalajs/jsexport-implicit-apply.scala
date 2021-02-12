import scala.scalajs.js
import scala.scalajs.js.annotation.*

class A {
  @JSExport // error
  def apply(): Int = 1
}

@JSExportAll
class B {
  def apply(): Int = 1 // error
}

@JSExportAll
class C {
  @JSExport("foo")
  def apply(): Int = 1 // error
}

@JSExportAll
class D {
  @JSExport("apply")
  def apply(): Int = 1 // ok
}
