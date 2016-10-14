sealed trait Trait[T]

final case class Case[T](e: T) extends Trait[T]

object Demo {
  def main(args: Array[String]): Unit = {

    def f[H](t: Trait[H]): Unit =
      t match {
        case Case(e) => println(Some(e))
      }

    f(Case(1))

  }
}
