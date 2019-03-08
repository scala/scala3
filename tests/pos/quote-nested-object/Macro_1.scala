
import scala.quoted._

object Macro {


  object Implementation {

    inline def plus(inline n: Int, m: Int): Int = ${ plus(n, 'm) }

    def plus(n: Int, m: Expr[Int]): Expr[Int] =
      if (n == 0) m
      else '{ ${n.toExpr} + $m }

    object Implementation2 {

      inline def plus(inline n: Int, m: Int): Int = ${ plus(n, 'm) }

      def plus(n: Int, m: Expr[Int]): Expr[Int] =
        if (n == 0) m
        else '{ ${n.toExpr} + $m }
    }
  }

}
