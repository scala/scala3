import scala.annotation.tailrec
import scala.quoted._

object Macros {

  inline def simplified[T <: Tuple]: Seq[String] = ${ impl[T] }

  def impl[T: Type] given (qctx: QuoteContext): Expr[Seq[String]] = {
    import qctx.tasty._

    def unpackTuple(tp: Type): List[Type] = {
      @tailrec
      def loop(tp: Type, acc: List[Type]): List[Type] = tp.dealias.simplified match {
        case Type.AppliedType(_, List(IsType(hd), IsType(tl))) =>
          loop(tl, hd.dealias.simplified :: acc)
        case other => acc
      }
      loop(tp, Nil).reverse
    }

    val tps = unpackTuple(typeOf[T])
    tps.map(_.show.toExpr).toExprOfSeq
  }
}
