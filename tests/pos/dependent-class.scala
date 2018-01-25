import scala.annotation.dependent

class Animal(name: String)

trait Foo[T]
class Bar(@dependent val a: Animal) extends Foo[a.type]

class Bag(@dependent val a: Animal) {
 def g(n: Int): Foo[a.type] = ???
 def f(n: Int): a.type = ???
}

object Test {
  def foo(a: Animal): Foo[a.type] = ???

  val dog = new Animal("dog")

  // new instance
  new Bar(dog) : Foo[dog.type] // found:    Bar, required: Foo[Animal(Test.dog)]

  // dependent method
  val c: Foo[dog.type] = foo(dog)        // works well

  // dependent function type
  val f : (a: Animal) => Foo[a.type] = ???  // works well
  f(dog) : Foo[dog.type]

  // dependent class method
  new Bag(dog).g(6) : Foo[dog.type]
  new Bag(dog).f(5) : dog.type
}