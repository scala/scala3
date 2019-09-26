package scala.quoted
package matching

/** Sym of an Expr[T] used to know if some Expr[T] is a reference to the symbol
 *
 *  @param name string name of this symbol
 *  @param id unique id used for equality
 */
class Sym[T <: AnyKind] private[scala](val name: String, private[Sym] val id: Object) { self =>

  override def equals(obj: Any): Boolean = obj match {
    case obj: Sym[_] => obj.id == id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()

}

object Sym {

  def unapply[T](expr: Expr[T])(given qctx: QuoteContext): Option[Sym[T]] = {
    import qctx.tasty.{_, given}
    expr.unseal match {
      case IsIdent(ref) =>
        val sym = ref.symbol
        Some(new Sym[T](sym.name, sym))
      case _ => None
    }
  }

}
