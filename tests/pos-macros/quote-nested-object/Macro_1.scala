
import scala.quoted.*

object Macro {


  object Implementation {

    inline def plus(inline n: Int, m: Int): Int = ${ plus('n, 'm) }

    def plus(n: Expr[Int], m: Expr[Int]) (using Quotes): Expr[Int] =
      if (n.valueOrError == 0) m
      else '{ ${n} + $m }

    object Implementation2 {

      inline def plus(inline n: Int, m: Int): Int = ${ plus('n, 'm) }

      def plus(n: Expr[Int], m: Expr[Int]) (using Quotes): Expr[Int] =
        if (n.valueOrError == 0) m
        else '{ ${n} + $m }
    }
  }

}
