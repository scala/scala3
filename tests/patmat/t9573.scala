class Foo {

  def foo = {
    abstract sealed class Animal
    case class Goat(age: Int) extends Animal
    case class Horse(age: Int) extends Animal

    val x: Animal = Goat(1)
    x match {
      case Goat(_) => println("a goat")
    }
  }
}