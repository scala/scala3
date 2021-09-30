trait Foo
trait Bar
object Foo:
  given (using Bar): Foo = ???

object Bug:
  def demonstrate: Unit =
    Option.empty[Unit].flatMap { _ =>
      Option.empty[Unit].map { _ =>
        val foo = summon[Foo] // error: Foo.given_Foo(/* missing */summon[Bar])
        Option.empty[Unit]
      }
    }
