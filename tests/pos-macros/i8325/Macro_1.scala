package a

import scala.quoted._


object A:

  inline def transform[A](inline expr: A): A = ${
    transformImplExpr('expr)
  }

  def pure[A](a:A):A = ???

  def transformImplExpr[A](using s: Scope)(expr: s.Expr[A])(using s.Type[A]): s.Expr[A] = {
    import s.tasty._
     expr match {
         case Inlined(x,y,z) => transformImplExpr(z.seal.asInstanceOf[s.Expr[A]])
         case Apply(fun,args) =>  '{  A.pure(${Apply(fun,args).seal.asInstanceOf[s.Expr[A]]}) }
         case other => expr
     }
  }
