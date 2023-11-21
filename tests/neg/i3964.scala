trait Animal
class Dog extends Animal
class Cat extends Animal

object Test1:

  abstract class Bar { val x: Animal }
  val bar: Bar { val x: Cat } = new Bar { val x = new Cat }  // error, but should work

  trait Foo { val x: Animal }
  val foo: Foo { val x: Cat } = new Foo { val x = new Cat }  // error, but should work

object Test3:
  trait Vec(tracked val size: Int)
  class Vec8 extends Vec(8):
    val s: 8 = size    // error, but should work