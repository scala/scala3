package example

trait T {
  def x = 2
}

sealed trait U
object U {
  def u: U = new U {}
}

class C
trait V { self: C =>
}
