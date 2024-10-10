import scala.quoted.*

object Macros{

  inline def A() : String = {
     ${ A_impl }
  }

  def A_impl(using Quotes): Expr[String] = {
    Expr("Whatever")
  }

  inline def B[T]: Int = {
    ${ B_Impl[T] }
  }

  def B_Impl[T](using Quotes): Expr[Int] = {
    Expr(0)
  }
}
