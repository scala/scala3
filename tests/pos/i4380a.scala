import scala.quoted._

object Test {

  trait Producer[A] { self =>
    def step(k: (A => Expr[Unit])): Expr[Unit]
  }

  trait Foo[A]
  case class Bar[A, B](producer: Producer[B], nestedf: B => Expr[Unit]) extends Foo[A]

  def meth[A](stream: Foo[Expr[A]]): Producer[Expr[A]] = {
    stream match {
      case Bar(producer, nestedf) => {
        new Producer[Expr[A]] {
          def step(k: Expr[A] => Expr[Unit]): Expr[Unit] = '{
            val adv: Unit => Unit = { _ => ${producer.step((el) => nestedf(el))} }
          }
        }
      }
    }
  }
}
