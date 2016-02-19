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



