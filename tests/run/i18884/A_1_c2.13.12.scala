// scalajs: --skip

package lib

case class Foo1(private[lib] var x: Int) {}
case class Foo2(private[lib] var x: Int, private[lib] var y: Int)
case class Foo3(private[lib] var x: Int, var y: Int)
case class Foo4(var x: Int, private[lib] var y: Int) {
  val z: Int = x
}
