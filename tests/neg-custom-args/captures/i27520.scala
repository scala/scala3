import language.experimental.separationChecking
import caps.*
class Cell extends Mutable:
  var n = 0
  update def bump(): Unit = n += 1
class Box1 extends Mutable:
  var c: Cell^ = new Cell
  def poke(): Unit = c.bump()  // error
class Box2 extends Mutable:
  val c: Cell^ = new Cell
  def poke(): Unit = c.bump()  // error
class Box3 extends Mutable:
  var c: Cell^ = new Cell
  def poke(): Unit =
    val d = c
    d.bump()  // error
  update def poke2(): Unit = c.bump()  // ok
  update def poke3(): Unit =
    val d = c
    d.bump()  // ok
  def read(): Int = c.n  // ok
