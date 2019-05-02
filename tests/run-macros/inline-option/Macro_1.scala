
import scala.quoted._
import scala.quoted.autolift._

object Macros {

  inline def size(inline opt: Option[Int]): Int = ${ Macros.impl(opt) }

  inline def size2(inline i: Int): Int = ${ Macros.impl(None) }

  inline def size3(inline i: Int): Int = ${ Macros.impl(Some(i)) }

  inline def size4(inline i: Int): Int = ${ Macros.impl2(Some(Some(i))) }

  inline def size5(inline opt: Option[Option[Int]]): Int = ${ Macros.impl2(opt) }

  def impl(opt: Option[Int]): Expr[Int] = opt match {
    case Some(i) => i
    case None => '{-1}
  }

  def impl2(opt: Option[Option[Int]]): Expr[Int] = impl(opt.flatten)

}
