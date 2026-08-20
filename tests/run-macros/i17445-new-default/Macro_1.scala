import scala.quoted.*

object TestMacro:

  /** Macro that rewrites copy$default$1 (x's default) to copy$default$2 (y's default)
    * in `p.copy(y = 7)`. This exercises Select.copy with a *new* DerivedName string
    * that refers to a different but valid member.
    */
  inline def swapDefault[T](inline body: T): T =
    ${ swapDefaultImpl[T]('body) }

  def swapDefaultImpl[T: Type](body: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.*
    val mapped = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case Select(qual, name) if name == "copy$default$1" =>
            Select.copy(tree)(transformTerm(qual)(owner), "copy$default$2")
          case _ =>
            super.transformTerm(tree)(owner)
    }.transformTerm(body.asTerm)(Symbol.spliceOwner)
    mapped.asExprOf[T]
