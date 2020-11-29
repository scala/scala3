import p.A

object B {
  val a = new A // No crash with check-init
  object graph extends A // <<= crach with check-init
}
