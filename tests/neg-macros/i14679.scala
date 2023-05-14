import scala.quoted.*

object A {
  inline val a = ${b} // error

  def b(using Quotes): Expr[Unit] = '{ () }
}
