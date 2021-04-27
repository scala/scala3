package x

import scala.annotation._
import scala.quoted._
import scala.compiletime._


trait CpsMonad[F[_]]:

  def pure[T](x:T):F[T]

  def impure[T](x:F[T]):T

  def map[A,B](x:F[A])(f: A=>B):F[B]


@compileTimeOnly("await should be inside async block")
def await[F[_],T](f:F[T])(using am:CpsMonad[F]):T = ???

inline given conversion[F[_],T](using CpsMonad[F]): Conversion[F[T],T] =
           x => await(x)


object X {

 inline def process[F[_], T](inline t:T)(using m: CpsMonad[F]):F[T] =
    ${ processImpl[F,T]('t, 'm) }


 def processImpl[F[_]:Type, T:Type](t:Expr[T], m:Expr[CpsMonad[F]])(using Quotes):Expr[F[T]] =
   import quotes.reflect._
   val r = processTree[F,T](t.asTerm, m.asTerm)
   r.asExprOf[F[T]]


 def processTree[F[_]:Type, T:Type](using Quotes)(t: quotes.reflect.Term, m: quotes.reflect.Term):quotes.reflect.Term =
   import quotes.reflect._
   val r: Term = t match
     case Inlined(_,List(),body) => processTree[F,T](body, m)
     case Inlined(d,bindings,body) =>
       Inlined(d,bindings,processTree[F,T](body, m))
     case Block(stats,expr) => Block(stats,processTree(expr, m))
     case Apply(Apply(TypeApply(Ident("await"),targs),List(body)),List(m)) => body
     case Apply(f,List(arg)) =>
          val nArg = processTree[F,String](arg, m)
          Apply(Apply(TypeApply(Select.unique(m,"map"),
                               List(Inferred(arg.tpe.widen),Inferred(t.tpe.widen))
                      ),
                      List(nArg)),
                 List(f)
          )
     case Apply(f,List()) =>
          Apply(TypeApply(Select.unique(m,"pure"),List(Inferred(t.tpe.widen))),List(t))
     case Typed(x,tp) => Typed(processTree(x,m), Inferred(TypeRepr.of[F].appliedTo(tp.tpe)) )
     case _ => throw new RuntimeException(s"tree not recoginized: $t")
   r


}