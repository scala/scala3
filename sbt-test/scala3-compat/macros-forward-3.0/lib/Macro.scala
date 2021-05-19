package lib

import scala.quoted.*

inline def power(x: Double, inline n: Int) = ${ powerCode('x, 'n) }

private def powerCode(x: Expr[Double], n: Expr[Int])(using Quotes): Expr[Double] =
  unrolledPowerCode(x, n.valueOrError)

private def unrolledPowerCode(x: Expr[Double], n: Int)(using Quotes): Expr[Double] =
  if n == 0 then '{ 1.0 } // tests simple quotes without splices
  else if n % 2 == 1 then '{ $x * ${ unrolledPowerCode(x, n - 1) } } // tests simple splices
  else '{ val y = $x * $x; ${ unrolledPowerCode('y, n / 2) } } // tests splice with term capture


inline def let[T, U](x: T)(inline body: T => U): U = ${ letCode('x, 'body) }

private def letCode[T: Type, U: Type](x: Expr[T], body: Expr[T => U])(using Quotes): Expr[U] =
  // tests use of Type
  '{ val y: T = $x; $body(y): U }
