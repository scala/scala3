sealed trait Partial
sealed trait Total extends Partial

case object Foo extends Total

trait P[A] {
  def bar(a: A): Partial
}

trait T[A] extends P[A] {
  def bar(a: A): Total
}

object T {
  def make[A](x: Total): T[A] =
    a => x
}

object Test {
  def total[A](a: A)(ev: T[A]): Total = ev.bar(a)
  def partial[A](a: A)(ev: P[A]): Partial = ev.bar(a)

  def go[A](a: A)(ev: T[A]): Unit = {
    println(a)
    println(total(a)(ev))
    println(partial(a)(ev))
  }

  def main(args: Array[String]): Unit =
    go(42)(T.make(Foo))
}
