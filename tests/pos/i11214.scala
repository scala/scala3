trait Pet(val name: String)
trait FeatheredPet extends Pet

class Bird(override val name: String) extends FeatheredPet:
   override def toString = s"bird name: $name"