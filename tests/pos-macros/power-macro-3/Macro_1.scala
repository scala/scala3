
import scala.quoted.*

inline def power[Num](x: Num, inline n: Int)(using num: Numeric[Num]) = ${powerCode('x, 'n)(using 'num)}

private def powerCode[Num: Type](x: Expr[Num], n: Expr[Int])(using Expr[Numeric[Num]])(using Quotes): Expr[Num] =
  powerCode(x, n.valueOrAbort)

private def powerCode[Num: Type](x: Expr[Num], n: Int)(using num: Expr[Numeric[Num]])(using Quotes): Expr[Num] =
  if (n == 0) '{ $num.one }
  else if (n % 2 == 0) '{
    withGiven($num) {
      val y = $num.times($x, $x)
      ${ powerCode('y, n / 2) }
    }
  }
  else '{
    withGiven($num) {
      $num.times($x, ${powerCode(x, n - 1)})
    }
  }

inline def withGiven[U, T](inline x: T)(inline body: T ?=> U): U = body(using x)
