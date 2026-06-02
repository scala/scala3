//> using options -Xmax-fuel:20
// (because we store the errors caused by a recursive overflow, so if the fuel is too high,
//  the error is too much text for a JVM string constant)

import scala.quoted.*

transparent inline def stackOverflowMacro: Int = ${ StackOverflowMacro.impl }

object StackOverflowMacro:
    def impl(using Quotes): Expr[Int] =  '{ ${impl} + ${impl} }

