package x

import scala.quoted.*

def fun(x:Int): Int = ???

transparent inline def in1[T](inline expr: Int => Int): Int => Int =
     ${
        M.transformLambdaImpl('expr)
     }

object M:

  def transformLambdaImpl(cexpr: Expr[Int => Int])(using Quotes): Expr[Int => Int] =
      import quotes.reflect.*

      def extractLambda(f:Term): (ValDef, Term, Term => Term ) =
         f match
            case Inlined(call, bindings, body) =>
               val inner = extractLambda(body)
               (inner._1, inner._2, t => Inlined(call, bindings, t) )
            case Lambda(params,body) =>
               params match
                  case List(vd) => (vd, body, identity)
                  case _ => report.throwError(s"lambda with one argument expected, we have ${params}",cexpr)
            case Block(Nil,nested@Lambda(params,body)) => extractLambda(nested)
            case _ =>
               report.throwError(s"lambda expected, have: ${f}", cexpr)

      val (oldValDef, body, inlineBack) = extractLambda(cexpr.asTerm)
      val mt = MethodType(List(oldValDef.name))( _ => List(oldValDef.tpt.tpe), _ => TypeRepr.of[Int])
      val nLambda = Lambda(Symbol.spliceOwner, mt, (owner, params) => {
            val argTransformer = new TreeMap() {
                 override def transformTerm(tree: Term)(owner: Symbol): Term =
                    tree match
                       case Ident(name) if (tree.symbol == oldValDef.symbol) =>  Ref(params.head.symbol)
                       case _ => super.transformTerm(tree)(owner)
            }
            argTransformer.transformTerm('{ fun(${body.asExprOf[Int]}) }.asTerm )(owner)
      })
      inlineBack(nLambda).asExprOf[Int => Int]
