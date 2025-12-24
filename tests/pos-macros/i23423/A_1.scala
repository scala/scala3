package pkg

import scala.quoted.*

trait HasElem {
  type Elem
  type Alias = Elem
}

object Macro:
  inline def foo: Unit = ${fooImpl}
  def fooImpl(using Quotes): Expr[Unit] =
    '{
      val lll: (he: HasElem) => he.Alias =
        (hx: HasElem) => ???
    }
