import scala.annotation.valhalla

@valhalla trait Animal(val cuteness: Int) extends Any:
  def speak: Unit

@valhalla trait Mammal(val furColour: Tuple) extends Any:
  this: Animal =>
