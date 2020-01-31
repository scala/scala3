import scala.language.implicitConversions

class Ops[A](a: A) {
  def bar: Unit = ()
}

implicit def toOps[A](a: A): Ops[A] = new Ops[A](a)

type Id[A] = A

class Foo[A](val value: Id[A]) {
  def same: Foo[A] = new Foo[A](value)
  def map[B](f: A => B): Foo[B] = new Foo[B](f(value))
}

val x: Int = 1
val foo: Foo[Int] = new Foo(1)

val res1 = x.bar
val res2 = foo.value.bar
val res3 = foo.same.value.bar
val res4 = foo.map[Int](_ + 1).value.bar
val res5 = foo.map(_ + 1).value.bar