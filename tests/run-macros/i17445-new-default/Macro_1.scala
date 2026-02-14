import scala.quoted.*

object TestMacro:

  /** Macro that rewrites `p.copy(y = value)` to `p.copy(x = value)` by
    * changing the Select name for the default getter from copy$default$2 to copy$default$1.
    * This exercises Select.copy with a *new* DerivedName string (not identity).
    */
  inline def swapFirstArg[T](inline body: T): T =
    ${ swapFirstArgImpl[T]('body) }

  def swapFirstArgImpl[T: Type](body: Expr[T])(using Quotes): Expr[T] =
    import quotes.reflect.*
    val mapped = new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case Select(qual, name) if name == "copy$default$2" =>
            Select.copy(tree)(transformTerm(qual)(owner), "copy$default$1")
          case _ =>
            super.transformTerm(tree)(owner)
    }.transformTerm(body.asTerm)(Symbol.spliceOwner)
    mapped.asExprOf[T]
