import scala.quoted._

trait X[A] { def x(a: A): Boolean }

def hmm[A](using Quotes, Type[A]): Expr[Unit] = {
  def sadFace(f: (Expr[A]) => Expr[Boolean]): Expr[X[A]] = '{
    new X[A] {
      override def x(a: A) = ${f('a)}
    }
  }
  '{()}
}