
class phantomClassOf {
  classOf[Boo.Blinky] // error
}

object Boo extends Phantom {
  type Blinky = this.Any
}
