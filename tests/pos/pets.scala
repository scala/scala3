// Representing the current type
trait Pet {
  type ThisPet <: Pet
  def name: String
  def renamed(newName: String): ThisPet
}

case class Fish(name: String, age: Int) extends Pet {
  type ThisPet = Fish
  def renamed(newName: String): Fish = copy(name = newName)
}

case class Kitty(name: String, age: Int) extends Pet {
  type ThisPet = Kitty
  def renamed(newName: String): Kitty = copy(name = newName)
}

object Test {
  def esquire[A <: Pet](a: A): a.ThisPet = a.renamed(a.name + ", Esq.")
  val f: Fish = esquire(new Fish("bob", 22))
}
