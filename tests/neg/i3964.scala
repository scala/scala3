trait Animal
class Dog extends Animal
class Cat extends Animal

object Test1:

  abstract class Bar { val x: Animal }
  val bar: Bar { val x: Cat } = new Bar { val x = new Cat }  // error, but should work

  trait Foo { val x: Animal }
  val foo: Foo { val x: Cat } = new Foo { val x = new Cat }  // error, but should work

object Test2:
  abstract class Bar(tracked val x: Animal)
  val b = new Bar(new Cat)
  val bar: Bar { val x: Cat } = new Bar(new Cat)  // ok

  trait Foo(tracked val x: Animal)
  val foo: Foo { val x: Cat } = new Foo(new Cat)  // ok

object Test3:
  trait Vec(tracked val size: Int)
  class Vec8 extends Vec(8)

  abstract class Lst(tracked val size: Int)
  class Lst8 extends Lst(8)

  val v8a: Vec { val size: 8 } = new Vec8        // error, but should work
  val v8b: Vec { val size: 8 } = new Vec(8)      // ok

  val l8a: Lst { val size: 8 } = new Lst8        // error, but should work
  val l8b: Lst { val size: 8 } = new Lst(8)      // ok
