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


  def checkPrintTypeImpl[F[_],T](using s: Scope)(f: s.Expr[T])(using s.Type[F], s.Type[T]): s.Expr[Unit] =
    import s.tasty._

    f match
      case Inlined(_,_,Block(_,Apply(TypeApply(Select(q,n),tparams),List(param)))) =>
        param.tpe match
          case AppliedType(tp,tparams1) =>
            val fType = summon[s.Type[F]]
            val ptp = tparams1.tail.head
            val ptpTree = Inferred(fType.tpe.appliedTo(ptp))
            '{ println(${Expr(ptpTree.show)}) }

}
