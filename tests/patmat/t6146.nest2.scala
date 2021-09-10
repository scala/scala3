// Like nest1, but nested twice.
trait T1[X] {
  trait T2[Y] {
    sealed trait A
    object A {
      case object B extends A
    }

    def provide: Y
    def consume(y: Y): Y
  }

  def give: X
  def take(x: X): X
}

object O1 extends T1[String] {
  object O2 extends T2[Thread] {
    def provide            = new Thread("O2")
    def consume(y: Thread) = new Thread(y, "Love, O2")
  }

  def give            = "O1"
  def take(x: String) = s"$x. Love, O1."
}

case object C extends O1.O2.A
