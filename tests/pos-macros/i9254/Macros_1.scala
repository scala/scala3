package cps

import scala.quoted.*

trait CpsMonad[F[_]]

trait ComputationBound[T]

implicit object ComputationBoundMonad extends CpsMonad[ComputationBound]

inline def async[F[_]](using am:CpsMonad[F]): Async.InferAsyncArg[F] =
   new Async.InferAsyncArg[F]

object PFHelper {
  def create[X,Y](x:Boolean):PartialFunction[X,Y] = ???
}

object Async {

  class InferAsyncArg[F[_]](using am:CpsMonad[F]) {

       inline def apply[T](inline expr: T):Unit =
       ${
         Async.transformImpl[F,T]('expr)
        }

  }

  def transformImpl[F[_]:Type,T:Type](f: Expr[T])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    def uninline(t:Term):Term =
      t match
        case Inlined(_,_,x) => uninline(x)
        case _ => t

    val fu = uninline(f.asTerm)
    fu match
      case Block(_,Apply(TypeApply(Select(q,n),tparams),List(param))) =>
        param.tpe match
          case AppliedType(tp,tparams1) =>
            val fromTypeOrBounds = tparams1.head
            val fromType = fromTypeOrBounds match
                 case bounds: TypeBounds => bounds.low
                 case tp: TypeRepr => tp
                 case np: NoPrefix => ???
            val toType = tparams1.tail.head
            val fType = TypeRepr.of[F]
            val toWrapped = fType.appliedTo(toType)
            val helper = '{ cps.PFHelper }.asTerm
            val helperSelect = Select.unique(helper,"create")
            val createPF = Apply(
                             TypeApply(helperSelect,List(Inferred(fromType),Inferred(toWrapped))),
                             List(Literal(BooleanConstant(true)))
                           )
            val createPfApply = Apply(Select.unique(createPF,"apply"),List(Literal(IntConstant(1))))
            Block(List(createPfApply),Literal(UnitConstant())).asExpr.asInstanceOf[Expr[Unit]]

}
