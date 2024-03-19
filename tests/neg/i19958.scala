val b = new Object().unit // error

abstract class C:
  def wait: Unit
  def wait(x: Int): Unit
  def blit: Unit
  def blit(x: Int): Unit

val c: C = ???
val d = c.unit // error

