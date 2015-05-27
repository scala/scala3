// Representing the current type
trait Pet {
  type This <: Pet
  def name: String
  def renamed(newName: String): This
}

case class Fish(name: String, age: Int) extends Pet {
  type This = Fish
  def renamed(newName: String): Fish = copy(name = newName)
}

case class Kitty(name: String, age: Int) extends Pet {
  type This = Kitty
  def renamed(newName: String): Kitty = copy(name = newName)
}

object Test {
  def esquire[A <: Pet](a: A): a.This = a.renamed(a.name + ", Esq.")
  val f: Fish = esquire(new Fish("bob", 22))
}
