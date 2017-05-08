
class phantomClassOf {
  classOf[BooAny] // error
  classOf[BooNothing] // error
}

object Boo extends Phantom {
  type BooAny = this.Any
  type BooNothing = this.Nothing
}
