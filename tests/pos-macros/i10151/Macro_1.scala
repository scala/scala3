package x

import scala.quoted.*

trait CB[T]:
 def map[S](f: T=>S): CB[S] = ???
 def flatMap[S](f: T=>CB[S]): CB[S] = ???

class MyArr[AK,AV]:
 def map1[BK,BV](f: ((AK,AV)) => (BK, BV)):MyArr[BK,BV] = ???
 def map1Out[BK, BV](f: ((AK,AV)) => CB[(BK,BV)]): CB[MyArr[BK,BV]] = ???

def await[T](x:CB[T]):T = ???

object CBM:
  def pure[T](t:T):CB[T] = ???

object X:

 inline def process[T](inline f:T) = ${
   processImpl[T]('f)
 }

 def processImpl[T:Type](f:Expr[T])(using Quotes):Expr[CB[T]] =
   import quotes.reflect.*

   def transform(term:Term):Term =
     term match
        case Apply(TypeApply(Select(obj,"map1"),targs),args) =>
             val nArgs = args.map(x => shiftLambda(x))
             val nSelect = Select.unique(obj, "map1Out")
             Apply(TypeApply(nSelect,targs),nArgs)
        case Apply(TypeApply(Ident("await"),targs),args) => args.head
        case a@Apply(x,List(y,z)) =>
                 val mty=MethodType(List("y1"))( _ => List(y.tpe.widen), _ => TypeRepr.of[CB].appliedTo(a.tpe.widen))
                 val mtz=MethodType(List("z1"))( _ => List(z.tpe.widen), _ => a.tpe.widen)
                 Apply(
                   TypeApply(Select.unique(transform(y),"flatMap"),
                             List(Inferred(a.tpe.widen))
                            ),
                   List(
                     Lambda(Symbol.spliceOwner, mty, (meth, yArgs) =>
                       Apply(
                          TypeApply(Select.unique(transform(z),"map"),
                             List(Inferred(a.tpe.widen))
                          ),
                          List(
                            Lambda(Symbol.spliceOwner, mtz, (_, zArgs) => {
                              val termYArgs = yArgs.asInstanceOf[List[Term]]
                              val termZArgs = zArgs.asInstanceOf[List[Term]]
                              Apply(x,List(termYArgs.head,termZArgs.head))
                            })
                          )
                       ).changeOwner(meth)
                     )
                   )
                 )
        case Block(stats, last) =>
          val recoverdOwner = stats.headOption.map(_.symbol.owner).getOrElse(Symbol.spliceOwner) // hacky workaround to missing owner tracking in transform
          Block(stats, transform(last).changeOwner(recoverdOwner))
        case Inlined(x,List(),body) => transform(body)
        case l@Literal(x) =>
           l.asExpr match
             case '{ $l: lit } =>
                '{ CBM.pure(${term.asExprOf[lit]}) }.asTerm
        case other =>
             throw RuntimeException(s"Not supported $other")

   def shiftLambda(term:Term): Term =
        term match
          case lt@Lambda(params, body) =>
            val paramTypes = params.map(_.tpt.tpe)
            val paramNames = params.map(_.name)
            val mt = MethodType(paramNames)(_ => paramTypes, _ => TypeRepr.of[CB].appliedTo(body.tpe.widen) )
            Lambda(Symbol.spliceOwner, mt, (meth, args) => changeArgs(params,args,transform(body)).changeOwner(meth) )
          case Block(stats, last) =>
            Block(stats, shiftLambda(last))
          case _ =>
            throw RuntimeException("lambda expected")

   def changeArgs(oldArgs:List[Tree], newArgs:List[Tree], body:Term):Term =
         val association: Map[Symbol, Term] = (oldArgs zip newArgs).foldLeft(Map.empty){
             case (m, (oldParam, newParam: Term)) => m.updated(oldParam.symbol, newParam)
             case (m, (oldParam, newParam: Tree)) => throw RuntimeException("Term expected")
         }
         val changes = new TreeMap() {
             override def transformTerm(tree:Term)(owner: Symbol): Term =
               tree match
                 case ident@Ident(name) => association.getOrElse(ident.symbol, super.transformTerm(tree)(owner))
                 case _ => super.transformTerm(tree)(owner)
         }
         changes.transformTerm(body)(Symbol.spliceOwner)

   val r = transform(f.asTerm).asExprOf[CB[T]]
   r
