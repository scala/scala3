import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Names {
  val a = "Hello"
}

// @JSGlobal

@JSGlobal(Names.a) // error
@js.native
object A1 extends js.Object

@JSGlobal(Names.a) // error
@js.native
class A2 extends js.Object

@js.native
@JSGlobal
class `not-a-valid-JS-identifier` extends js.Object // error

@js.native
@JSGlobal("not-a-valid-JS-identifier")
object B1 extends js.Object // error

@js.native
@JSGlobal("not-a-valid-JS-identifier.further")
object B2 extends js.Object // error

@js.native
@JSGlobal("TopLevel.not-a-valid-JS-identifier")
object B3 extends js.Object // ok

@js.native
@JSGlobal("")
object B4 extends js.Object // error

@js.native
@JSGlobal(".tricky")
object B5 extends js.Object // error

// @JSImport globalFallback

@js.native
@JSImport("foo.js", "foo", globalFallback = Names.a) // error
object C1 extends js.Object

@js.native
@JSImport("foo.js", "foo", globalFallback = "not-a-valid-JS-identifier")
object C2 extends js.Object // error

@js.native
@JSImport("foo.js", "foo", globalFallback = "not-a-valid-JS-identifier.further")
object C3 extends js.Object // error

@js.native
@JSImport("foo.js", "foo", globalFallback = "TopLevel.not-a-valid-JS-identifier")
object C4 extends js.Object // ok

@js.native
@JSImport("foo.js", "foo", globalFallback = "")
object C5 extends js.Object // error

@js.native
@JSImport("foo.js", "foo", globalFallback = ".tricky")
object C6 extends js.Object // error

// @JSImport first two arguments

@JSImport(Names.a, JSImport.Namespace) // error
@js.native
object D1 extends js.Object

@JSImport(Names.a, "B2") // error
@js.native
object D2 extends js.Object

@JSImport("B3", Names.a) // error
@js.native
object D3 extends js.Object

@JSImport(Names.a, JSImport.Namespace) // error
@js.native
object D4 extends js.Object

@JSImport(Names.a, "C2") // error
@js.native
object D5 extends js.Object

@JSImport("C3", Names.a) // error
@js.native
object D6 extends js.Object

@JSImport(Names.a, Names.a) // error // error
@js.native
object D7 extends js.Object
