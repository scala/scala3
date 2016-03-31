// Tests variance checking on default methods
import reflect.ClassTag

class Foo[+A: ClassTag](x: A) {

  private[this] val elems: Array[A] = Array(x)

  def f[B](x: Array[B] = elems): Array[B] = x // error (1) should give a variance error here or ...

}

object Test extends App {

  val foo: Foo[Object] = new Foo[String]("A")

  val arr = foo.f[Object]()

  arr(0) = new Integer(1) // (1) ... will give an ArrayStoreException here

}

class Outer[+A](x: A) {

  private[this] var elem: A = x

  def getElem: A = elem

  class Inner(constrParam: A) {  // error (2) should give a variance error here or ...
    elem = constrParam
  }

}

object Test2 extends App {
  val o1: Outer[String] = new Outer[String]("A")
  val o2: Outer[Object] = o1
  new o2.Inner(new Integer(1))

  val x: String = o1.getElem  // (2) ... will give a classcast exeption here

}


trait HasY { type Y }

// These are neg-tests corresponding to the pos-test Variances.scala
// where all the variance annotations have been inverted.
trait Foo1[+X] { def bar[Y <: X](y: Y) = y } // error
trait Foo2[+X] { def bar(x: HasY { type Y <: X })(y: x.Y) = y } // error
trait Foo3[-X] { def bar[Y >: X](y: Y) = y } // error
trait Foo4[-X] { def bar(x: HasY { type Y >: X })(y: x.Y) = y } // error

// These are neg-tests corresponding to the pos-test Variances.scala
// where all the bounds have been flipped.
trait Foo5[-X] { def bar[Y >: X](y: Y) = y } // error
trait Foo6[-X] { def bar(x: HasY { type Y >: X })(y: x.Y) = y } // error
trait Foo7[+X] { def bar[Y <: X](y: Y) = y } // error
trait Foo8[+X] { def bar(x: HasY { type Y <: X })(y: x.Y) = y } // error

