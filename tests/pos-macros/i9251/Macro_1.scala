package cps

import scala.quoted._

trait CpsMonad[F[_]]

trait ComputationBound[T]

implicit object ComputationBoundMonad extends CpsMonad[ComputationBound]

inline def async[F[_]](using am:CpsMonad[F]): Async.InferAsyncArg[F] =
   new Async.InferAsyncArg[F]

object Async {

  class InferAsyncArg[F[_]](using am:CpsMonad[F]) {
      inline def apply[T](inline expr: T):Unit = ${ Async.checkPrintTypeImpl[F,T]('expr) }
  }


  def checkPrintTypeImpl[F[_]: Staged,T: Staged](f: Expr[T])(using qctx: QuoteContext): Expr[Unit] =
    import qctx.tasty._

    val fu = f.unseal
    fu match
      case Inlined(_,_,Block(_,Apply(TypeApply(Select(q,n),tparams),List(param)))) =>
        param.tpe match
          case AppliedType(tp,tparams1) =>
            val fType = summon[quoted.Staged[F]]
            val ptp = tparams1.tail.head
            val ptpTree = Inferred(AppliedType(fType.unseal.tpe,List(ptp)))
            '{ println(${Expr(ptpTree.show)}) }

}
