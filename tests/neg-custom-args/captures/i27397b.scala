import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class Ctl extends Control
class Cell extends Mutable:
  var x = 0
  update def inc(): Unit = x += 1

class Base extends ExclusiveCapability:
  val c: Ctl^ = Ctl()   // without this field, `Sub` is rejected (illegal inheritance)
  val cell: Cell^ = Cell()
  def bump(): Unit = cell.inc()
  def get: Int = cell.x

class Sub extends Base, Stateful // error

def par(f: () => Unit, g: () => Unit): Unit = ()

def Test =
  val s = Sub()
  par(() => s.bump(), () => s.bump())
