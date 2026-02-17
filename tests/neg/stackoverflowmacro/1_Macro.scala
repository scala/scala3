import scala.quoted.*

transparent inline def stackOverflowMacro: Int = ${ StackOverflowMacro.impl }

object StackOverflowMacro:
    def impl(using Quotes): Expr[Int] =  '{ ${impl} + ${impl} }
    

