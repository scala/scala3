
class phantomClassOf {
  type Blinky <: Boo.BooAny

  classOf[Blinky] // error
}

object Boo extends Phantom {
  type BooAny = this.Any
}
