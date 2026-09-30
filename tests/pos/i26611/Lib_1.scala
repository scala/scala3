package lib

trait HasOptions { def options: Int }

case class A(x: String)
object A extends HasOptions { val options = 1 }

case class B(x: String)
object B extends HasOptions { val options = 1 }

case class C(x: String)
object C extends HasOptions { val options = 1 }

case class D(x: String)
object D extends HasOptions { val options = 1 }

case class E(x: String)
object E extends HasOptions { val options = 1 }
