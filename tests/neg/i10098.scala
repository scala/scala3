import annotation.implicitNotFound

@implicitNotFound("There's no Foo1[${A}, ${B}]")
trait Foo1[A, B]

@implicitNotFound("There's no Foo2[${A}, ${B}]")
trait Foo2[A, B]

trait Bar12[C, D] extends Foo1[D, C] with Foo2[D, C]

trait Bar21[C, D] extends Foo2[D, C] with Foo1[D, C]

@implicitNotFound("There's no Baz12[${C}, ${D}]")
trait Baz12[C, D] extends Foo1[D, C] with Foo2[D, C]

@implicitNotFound("There's no Baz21[${C}, ${D}]")
trait Baz21[C, D] extends Foo2[D, C] with Foo1[D, C]

object Test {
  implicitly[Bar12[Int, String]] // error
  implicitly[Bar21[Int, String]] // error
  implicitly[Baz12[Int, String]] // error
  implicitly[Baz21[Int, String]] // error
}
