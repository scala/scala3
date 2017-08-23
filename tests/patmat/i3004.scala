object O {
  sealed trait Fruit
  object Apple extends Fruit
  object Banana extends Fruit
  sealed class C(f1: Fruit, f2: Fruit)

  object C {
    def unapply(c: C): Some[Banana.type] = Some(Banana)
  }

  def m(c: C) = c match { case C(b) => b }
}
