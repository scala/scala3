object Test {
  sealed trait Base
  class Blub extends Base
  object Blub {
    def unapply(blub: Blub): Some[(Int, blub.type)] =
      Some(1 -> blub)
  }

  (null: Base) match {
    case Blub(i, x) => println(i)
  }
}
