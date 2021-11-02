package x

import scala.annotation._
import scala.quoted._

trait CB[+T]

object CBM:
  def pure[T](t:T):CB[T] = ???
  def map[A,B](fa:CB[A])(f: A=>B):CB[B] = ???
  def flatMap[A,B](fa:CB[A])(f: A=>CB[B]):CB[B] = ???
  def spawn[A](op: =>CB[A]): CB[A] = ???


@compileTimeOnly("await should be inside async block")
def await[T](f: CB[T]): T = ???


trait CpsExpr[T:Type](prev: Seq[Expr[?]]):

   def fLast(using Quotes): Expr[CB[T]]
   def prependExprs(exprs: Seq[Expr[?]]): CpsExpr[T]
   def append[A:Type](chunk: CpsExpr[A])(using Quotes): CpsExpr[A]
   def syncOrigin(using Quotes): Option[Expr[T]]
   def map[A:Type](f: Expr[T => A])(using Quotes): CpsExpr[A] =
           MappedCpsExpr[T,A](Seq(),this,f)
   def flatMap[A:Type](f: Expr[T => CB[A]])(using Quotes): CpsExpr[A] =
           FlatMappedCpsExpr[T,A](Seq(),this,f)

   def transformed(using Quotes): Expr[CB[T]] =
      import quotes.reflect._
      Block(prev.toList.map(_.asTerm), fLast.asTerm).asExprOf[CB[T]]


case class GenericSyncCpsExpr[T:Type](prev: Seq[Expr[?]],last: Expr[T]) extends CpsExpr[T](prev):

       override def fLast(using Quotes): Expr[CB[T]] =
         '{  CBM.pure(${last}:T) }

       override def prependExprs(exprs: Seq[Expr[?]]): CpsExpr[T] =
         copy(prev = exprs ++: prev)

       override def syncOrigin(using Quotes): Option[Expr[T]] =
         import quotes.reflect._
         Some(Block(prev.toList.map(_.asTerm), last.asTerm).asExprOf[T])

       override def append[A:Type](e: CpsExpr[A])(using Quotes) =
         e.prependExprs(Seq(last)).prependExprs(prev)

       override def map[A:Type](f: Expr[T => A])(using Quotes): CpsExpr[A] =
         copy(last = '{ $f($last) })

       override def flatMap[A:Type](f: Expr[T => CB[A]])(using Quotes): CpsExpr[A] =
         GenericAsyncCpsExpr[A](prev, '{ CBM.flatMap(CBM.pure($last))($f) } )


abstract class AsyncCpsExpr[T:Type](
                               prev: Seq[Expr[?]]
                             ) extends CpsExpr[T](prev):

   override def append[A:Type](e: CpsExpr[A])(using Quotes): CpsExpr[A] =
            flatMap( '{ (x:T) => ${e.transformed} })

   override def syncOrigin(using Quotes): Option[Expr[T]] = None



case class GenericAsyncCpsExpr[T:Type](
                             prev: Seq[Expr[?]],
                             fLastExpr: Expr[CB[T]]
                             ) extends AsyncCpsExpr[T](prev):

   override def fLast(using Quotes): Expr[CB[T]] = fLastExpr

   override def prependExprs(exprs: Seq[Expr[?]]): CpsExpr[T] =
         copy(prev = exprs ++: prev)

   override def map[A:Type](f: Expr[T => A])(using Quotes): CpsExpr[A] =
            MappedCpsExpr(Seq(),this,f)

   override def flatMap[A:Type](f: Expr[T => CB[A]])(using Quotes): CpsExpr[A] =
            FlatMappedCpsExpr(Seq(),this,f)



case class MappedCpsExpr[S:Type, T:Type](
                               prev: Seq[Expr[?]],
                               point: CpsExpr[S],
                               mapping: Expr[S=>T]
                               ) extends AsyncCpsExpr[T](prev):

   override def fLast(using Quotes): Expr[CB[T]] =
            '{ CBM.map(${point.transformed})($mapping) }

   override def prependExprs(exprs: Seq[Expr[?]]): CpsExpr[T] =
           copy(prev = exprs ++: prev)



case class FlatMappedCpsExpr[S:Type, T:Type](
                               prev: Seq[Expr[?]],
                               point: CpsExpr[S],
                               mapping: Expr[S => CB[T]]
                              ) extends AsyncCpsExpr[T](prev):

     override def fLast(using Quotes): Expr[CB[T]] =
                   '{ CBM.flatMap(${point.transformed})($mapping) }

     override def prependExprs(exprs: Seq[Expr[?]]): CpsExpr[T] =
           copy(prev = exprs ++: prev)


class ValRhsFlatMappedCpsExpr[T:Type, V:Type](using thisQuotes: Quotes)
                                      (
                                       prev: Seq[Expr[?]],
                                       oldValDef: quotes.reflect.ValDef,
                                       cpsRhs: CpsExpr[V],
                                       next: CpsExpr[T]
                                      )
                                     extends AsyncCpsExpr[T](prev) {

      override def fLast(using Quotes):Expr[CB[T]] =
           import quotes.reflect._
           next.syncOrigin match
             case Some(nextOrigin) =>
               // owner of this block is incorrect
              '{
                 CBM.map(${cpsRhs.transformed})((vx:V) =>
                           ${buildAppendBlockExpr('vx, nextOrigin)})
               }
             case  None =>
              '{
                CBM.flatMap(${cpsRhs.transformed})((v:V)=>
                           ${buildAppendBlockExpr('v, next.transformed)})
              }


      override def prependExprs(exprs: Seq[Expr[?]]): CpsExpr[T] =
           ValRhsFlatMappedCpsExpr(using thisQuotes)(exprs ++: prev,oldValDef,cpsRhs,next)

      override def append[A:quoted.Type](e: CpsExpr[A])(using Quotes) =
           ValRhsFlatMappedCpsExpr(using thisQuotes)(prev,oldValDef,cpsRhs,next.append(e))


      private def buildAppendBlock(using Quotes)(rhs:quotes.reflect.Term,
                                                     exprTerm:quotes.reflect.Term): quotes.reflect.Term =
           import quotes.reflect._
           import scala.quoted.Expr

           val castedOldValDef = oldValDef.asInstanceOf[quotes.reflect.ValDef]
           val valDef = ValDef(castedOldValDef.symbol, Some(rhs.changeOwner(castedOldValDef.symbol)))
           exprTerm match
               case Block(stats,last) =>
                     Block(valDef::stats, last)
               case other =>
                     Block(valDef::Nil,other)

      private def buildAppendBlockExpr[A:Type](using Quotes)(rhs: Expr[V], expr:Expr[A]):Expr[A] =
           import quotes.reflect._
           buildAppendBlock(rhs.asTerm,expr.asTerm).asExprOf[A]

}


object CpsExpr:

    def sync[T:Type](f: Expr[T]): CpsExpr[T] =
      GenericSyncCpsExpr[T](Seq(), f)

    def async[T:Type](f: Expr[CB[T]]): CpsExpr[T] =
      GenericAsyncCpsExpr[T](Seq(), f)


object Async:

   transparent inline def transform[T](inline expr: T) =  ${
      Async.transformImpl[T]('expr)
   }

   def transformImpl[T:Type](f: Expr[T])(using Quotes): Expr[CB[T]] =
      import quotes.reflect._
      // println(s"before transformed: ${f.show}")
      val cpsExpr = rootTransform[T](f)
      val r = '{ CBM.spawn(${cpsExpr.transformed}) }
      // println(s"transformed value: ${r.show}")
      r

   def rootTransform[T:Type](f: Expr[T])(using Quotes): CpsExpr[T] = {
      import quotes.reflect._
      f match
         case '{ while ($cond) { $repeat }  } =>
            val cpsRepeat = rootTransform(repeat.asExprOf[Unit])
            CpsExpr.async('{
                   def _whilefun():CB[Unit] =
                      if ($cond) {
                         ${cpsRepeat.flatMap('{(x:Unit) => _whilefun()}).transformed}
                      } else {
                         CBM.pure(())
                      }
                   _whilefun()
            }.asExprOf[CB[T]])
         case _ =>
            val fTree = f.asTerm
            fTree match {
               case fun@Apply(fun1@TypeApply(obj2,targs2), args1) =>
                      if (obj2.symbol.name == "await") {
                         val awaitArg = args1.head
                         CpsExpr.async(awaitArg.asExprOf[CB[T]])
                      } else {
                         ???
                      }
               case Assign(left,right) =>
                       left match
                         case id@Ident(x) =>
                            right.tpe.widen.asType match
                               case '[r] =>
                                  val cpsRight = rootTransform(right.asExprOf[r])
                                  CpsExpr.async(
                                     cpsRight.map[T](
                                       '{ (x:r) => ${Assign(left,'x.asTerm).asExprOf[T] }
                                        }).transformed )
                         case _ => ???
               case Block(prevs,last) =>
                      val rPrevs = prevs.map{ p =>
                         p match
                            case v@ValDef(vName,vtt,optRhs) =>
                              optRhs.get.tpe.widen.asType match
                                case '[l] =>
                                  val cpsRight = rootTransform(optRhs.get.asExprOf[l])
                                  ValRhsFlatMappedCpsExpr(using quotes)(Seq(), v, cpsRight, CpsExpr.sync('{}))
                            case t: Term =>
                             // TODO: rootTransform
                             t.asExpr match
                                  case '{ $p: tp } =>
                                         rootTransform(p)
                                  case other =>
                                         printf(other.show)
                                         throw RuntimeException(s"can't handle term in block: $other")
                            case other =>
                              printf(other.show)
                              throw RuntimeException(s"unknown tree type in block: $other")
                       }
                       val rLast = rootTransform(last.asExprOf[T])
                       val blockResult = rPrevs.foldRight(rLast)((e,s) => e.append(s))
                       val retval = CpsExpr.async(blockResult.transformed)
                       retval
                       //BlockTransform(cpsCtx).run(prevs,last)
               case id@Ident(name) =>
                       CpsExpr.sync(id.asExprOf[T])
               case tid@Typed(Ident(name), tp) =>
                      CpsExpr.sync(tid.asExprOf[T])
               case matchTerm@Match(scrutinee, caseDefs) =>
                      val nCases = caseDefs.map{ old =>
                         CaseDef.copy(old)(old.pattern, old.guard, rootTransform(old.rhs.asExprOf[T]).transformed.asTerm)
                      }
                      CpsExpr.async(Match(scrutinee, nCases).asExprOf[CB[T]])
               case inlinedTerm@ Inlined(call,List(),body) =>
                         rootTransform(body.asExprOf[T])
               case constTerm@Literal(_)=>
                       CpsExpr.sync(constTerm.asExprOf[T])
               case _ =>
                       throw RuntimeException(s"language construction is not supported: ${fTree}")
            }
   }

