import scala.annotation.tailrec
import scala.quoted._

object Macros {

  inline def simplified[T <: Tuple]: Seq[String] = ${ impl[T] }

  def impl[T](using s: Scope)(using s.Type[T]): s.Expr[Seq[String]] = {
    import s.tasty._

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
