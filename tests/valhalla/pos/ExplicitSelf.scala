import scala.annotation.valhalla

trait Animal(val cuteness: Int):
  val happiness = 100
  def speak: Unit

@valhalla trait Mammal(val furColour: Tuple) extends Any:
  this: Animal =>
