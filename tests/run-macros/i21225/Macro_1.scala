//> using options -experimental

import scala.quoted.*

trait Codec[-T] { def print(): Unit }
object Codec {
  inline def derivedWithDeps[T](deps: Any): Codec[T] = ${derivedWithDepsImpl[T]('deps)}

  private def derivedWithDepsImpl[T](deps: Expr[Any])(using q: Quotes)(using Type[T]): Expr[Codec[T]] = {
    import q.reflect.*

    val givenSelector: Selector = GivenSelector(None)
    val theImport = Import(deps.asTerm, List(givenSelector))
    Block(List(theImport), '{scala.compiletime.summonInline[Codec[T]]}.asTerm).asExprOf[Codec[T]]
    /* import deps.given
     * summonInline[Codec[T]]
     */
  }

  inline def derivedWithDepsWithNamedOmitted[T](deps: Any): Codec[T] = ${derivedWithDepsWithNamedOmittedImpl[T]('deps)}

  private def derivedWithDepsWithNamedOmittedImpl[T](deps: Expr[Any])(using q: Quotes)(using Type[T]): Expr[Codec[T]] = {
    import q.reflect.*

    val givenSelector: Selector = GivenSelector(None)
    val omitSelector: Selector = OmitSelector("named")
    val theImport = Import(deps.asTerm, List(givenSelector, omitSelector))
    Block(List(theImport), '{scala.compiletime.summonInline[Codec[T]]}.asTerm).asExprOf[Codec[T]]
    /* import deps.{given, named => _}
     * summonInline[Codec[T]]
     */
  }
}
