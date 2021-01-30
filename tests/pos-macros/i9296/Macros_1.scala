package a

import scala.quoted.*
import scala.concurrent.*

object M {

  inline def resolveInMacros[F[_],T](f: Future[T]):Conversion[Future[T],F[T]] =
     ${ resolveInMacrosImpl[F,T]('f) }

  def resolveInMacrosImpl[F[_]:Type,T:Type](f:Expr[Future[T]])(using qctx:Quotes):Expr[
                                                      Conversion[Future[T],F[T]]]={
     import quotes.reflect.*
     val conversion = TypeIdent(Symbol.classSymbol("scala.Conversion")).tpe
     val inFuture = f.asTerm.tpe.widen
     val tType = TypeRepr.of[T]
     val fType = TypeRepr.of[F]
     val inCB = fType.appliedTo(tType).simplified
     val taConversion = conversion.appliedTo(List(inFuture, inCB))
     Implicits.search(taConversion) match
       case implSuccess: ImplicitSearchSuccess =>
               implSuccess.tree.asExpr.asInstanceOf[Expr[Conversion[Future[T],F[T]]]]
       case implFailure: ImplicitSearchFailure =>
               println(s"searchFailure: ${implFailure.explanation}")
               throw new RuntimeException("implicit search failed")
  }

}
