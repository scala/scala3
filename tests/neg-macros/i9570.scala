//> using options -Xfatal-warnings

import scala.quoted.*

object Macros {

  object HList {
    sealed trait HList
    case class HCons[+HD, TL <: HList](hd: HD, tl: TL) extends HList
    case object HNil extends HList

    private def sizeImpl(e: Expr[HList], n:Int)(using qctx:Quotes): Expr[Int] = {
      import quotes.reflect.*
      e match {
        case '{HCons(_,$t)} => // warn (in .check file)
          sizeImpl(t,n+1)
        case '{HNil} => Expr(n)
      }
    }

    inline def size(inline expr: HList ): Int = {
      ${sizeImpl('expr,0)}
    }

  }
}

// nopos-error No warnings can be incurred under -Werror (or -Xfatal-warnings)