package a

import scala.quoted._

trait CPM[F[_]]

def fun[M[_],T](t:T)(using m:CPM[M]):M[T] = ???

object M {

  inline def transform[F[_],T](t:T): F[T] =
     ${ transformImpl[F,T]('t) }

  def transformImpl[F[_]:Type,T:Type](t:Expr[T])(using Quotes):Expr[F[T]] = {
     import quotes.reflect._
     t match {
       case '{ type mt[_]; a.fun[`mt`, tt]($t)(using $m) } => ???
     }

  }

}
