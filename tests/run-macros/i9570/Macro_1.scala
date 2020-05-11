import scala.quoted._

object Macros {

  object HList {
    sealed trait HList
    case class HCons[+HD, TL <: HList](hd: HD, tl: TL) extends HList
    case object HNil extends HList

    private def sizeImpl(using s: Scope)(e: s.Expr[HList], n: Int): s.Expr[Int] = {
      e match {
        case '{HCons($_,$t)} =>
        //case '{HCons($a,$t)} =>
          sizeImpl(t,n+1)
        case '{HNil} => s.Expr(n)
      }
    }

    inline def size(inline expr: HList): Int = {
      ${sizeImpl('expr,0)}
    }

  }
}
