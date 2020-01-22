
import scala.quoted._
import scala.quoted.autolift.{given _}

object Macro {


  object Implementation {

    inline def plus(inline n: Int, m: Int): Int = ${ plus('n, 'm) }

    def plus(n: Expr[Int], m: Expr[Int]) with QuoteContext : Expr[Int] =
      if (n.value == 0) m
      else '{ ${n} + $m }

    object Implementation2 {

      inline def plus(inline n: Int, m: Int): Int = ${ plus('n, 'm) }

      def plus(n: Expr[Int], m: Expr[Int]) with QuoteContext : Expr[Int] =
        if (n.value == 0) m
        else '{ ${n} + $m }
    }
  }

}
