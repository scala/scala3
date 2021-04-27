import scala.annotation.tailrec
import scala.quoted.*

object Macros {

  inline def simplified[T <: Tuple]: Seq[String] = ${ impl[T] }

  def impl[T: Type](using Quotes) : Expr[Seq[String]] = {
    import quotes.reflect.*

    def unpackTuple(tp: TypeRepr): List[TypeRepr] = {
      @tailrec
      def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = tp.dealias.simplified match {
        case AppliedType(_, List(hd: TypeRepr, tl: TypeRepr)) =>
          loop(tl, hd.dealias.simplified :: acc)
        case other => acc
      }
      loop(tp, Nil).reverse
    }

    val tps = unpackTuple(TypeRepr.of[T])
    Varargs(tps.map(x => Expr(x.show)))
  }
}
