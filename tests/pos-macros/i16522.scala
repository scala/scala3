  import scala.quoted.*

  sealed trait HList
  case class HCons[+HD, TL <: HList](hd: HD, tl: TL) extends HList
  case object HNil extends HList

  def showFirstTwoImpl(e: Expr[HList])(using Quotes): Expr[String] = {
    e match {
      case '{HCons($h1, HCons($h2, $_))} => '{$h1.toString ++ $h2.toString}
      case '{type tl <: HList; HCons($h1: hd1, HCons($h2: hd2, $_ : tl))} => '{$h1.toString ++ $h2.toString} // error
      case '{HCons[hd, HCons[sd, tl]]($h1, HCons($h2, $_))} => '{$h1.toString ++ $h2.toString}
      case _ => '{""}
    }
  }

  transparent inline def showFirstTwo(inline xs: HList) = ${ showFirstTwoImpl('xs) }
