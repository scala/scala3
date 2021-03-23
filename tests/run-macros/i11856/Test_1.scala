import scala.quoted.*

object Str:
    inline def concat(inline a: String, inline b: String): String =
        ${ evalConcat('a, 'b) }

    def evalConcat(expra: Expr[String], exprb: Expr[String])(using Quotes): Expr[String] =
        val a = expra.valueOrError
        val b = exprb.valueOrError
        Expr(a ++ b)

object I:
  inline def sum(inline a: Int, inline b: Int): Int =
      ${ evalConcat('a, 'b) }

  def evalConcat(expra: Expr[Int], exprb: Expr[Int])(using Quotes): Expr[Int] =
      val a = expra.valueOrError
      val b = exprb.valueOrError
      Expr(a + b)