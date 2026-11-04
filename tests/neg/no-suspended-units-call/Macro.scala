//> using options -Yno-suspended-units -Xcheck-macros

import scala.quoted.*
inline def three: Int = ${ threeImpl }
def threeImpl(using Quotes): Expr[Int] = '{ 3 }
