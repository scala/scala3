import scala.scalajs.js
import scala.scalajs.js.annotation.*

@JSExport // error
class A1
@JSExport("A2Named") // error
class A2

@JSExport // error
object B1
@JSExport("B2Named") // error
object B2

@JSExport // error
val c1: Int = 5
@JSExport("c2Named") // error
val c2: Int = 5

@JSExport // error
def d1(x: Int): Int = x + 1
@JSExport("d2Named") // error
def d2(x: Int): Int = x + 1

@JSExport // error
def e1: Int = 5
@JSExport("e2Named") // error
def e2: Int = 5

@JSExport // error
def f1_=(x: Int): Unit = ()
@JSExport("f2Named") // error
def f2_=(x: Int): Unit = ()
