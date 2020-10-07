import scala.annotation.tailrec
import scala.quoted._

object Macros {

  inline def simplified[T <: Tuple]: Seq[String] = ${ impl[T] }

  def impl[T: Type](using qctx: QuoteContext) : Expr[Seq[String]] = {
    import qctx.reflect._

    def unpackTuple(tp: Type): List[Type] = {
      @tailrec
      def loop(tp: Type, acc: List[Type]): List[Type] = tp.dealias.simplified match {
        case AppliedType(_, List(hd: Type, tl: Type)) =>
          loop(tl, hd.dealias.simplified :: acc)
        case other => acc
      }
      loop(tp, Nil).reverse
    }

    val tps = unpackTuple(Type.of[T])
    Varargs(tps.map(x => Expr(x.show)))
  }
}
