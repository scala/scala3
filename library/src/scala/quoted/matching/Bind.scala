package scala.quoted
package matching

/** Bind of an Expr[T] used to know if some Expr[T] is a reference to the binding
 *
 *  @param name string name of this binding
 *  @param id unique id used for equality
 */
class Bind[T <: AnyKind] private[scala](val name: String, private[Bind] val id: Object) { self =>

  override def equals(obj: Any): Boolean = obj match {
    case obj: Bind[_] => obj.id == id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()

}

object Bind {

  def unapply[T](expr: Expr[T]) given (qctx: QuoteContext): Option[Bind[T]] = {
    import qctx.tasty.{Bind => BindPattern, _}
    expr.unseal match {
      case IsIdent(ref) =>
        val sym = ref.symbol
        Some(new Bind[T](sym.name, sym))
      case _ => None
    }
  }

}
