import dotty.readonly
import dotty.polyread
import dotty.mutable

object reim3 {
  class Foo {}

  def m(x: Foo @polyread): Foo @polyread = x

  val x2: Foo @readonly = new Foo
  var y2 = m(x2)
  var z2: Foo @readonly = new Foo
  y2 = z2
  z2 = y2

  val x: Foo @mutable = new Foo
  var y = m(x)
  var z: Foo @mutable = new Foo
  y = z
  z = y


}
