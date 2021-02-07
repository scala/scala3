trait Pet(val name: String)
trait FeatheredPet extends Pet

class Bird(name: String) extends FeatheredPet:
   override def toString = s"bird name: $name"