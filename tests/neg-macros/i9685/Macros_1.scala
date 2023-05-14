import scala.language.implicitConversions

class Clue[+T](val value: T)

object Clue {
  import scala.quoted._

  inline implicit def generate[T](value: T): Clue[T] = ${ clueImpl('value) }

  def clueImpl[T:Type](value: Expr[T])(using Quotes): Expr[Clue[T]] = '{ new Clue($value) }
}
