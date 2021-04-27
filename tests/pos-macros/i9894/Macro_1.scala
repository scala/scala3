package x

import scala.quoted.*

trait CB[T]:
 def map[S](f: T=>S): CB[S] = ???

class MyArr[A]:
 def map1[B](f: A=>B):MyArr[B] = ???
 def map1Out[B](f: A=> CB[B]): CB[MyArr[B]] = ???

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
        case ap@Apply(TypeApply(Select(obj,"map1"),targs),args) =>
             val nArgs = args.map(x => shiftLambda(x))
             val nSelect = Select.unique(obj, "map1Out")
             Apply(TypeApply(nSelect,targs),nArgs)
             //Apply.copy(ap)(TypeApply(nSelect,targs),nArgs)
        case Apply(TypeApply(Ident("await"),targs),args) => args.head
        case Apply(x,y) =>
             Apply(x, y.map(transform))
        case Block(stats, last) => Block(stats, transform(last))
        case Inlined(x,List(),body) => transform(body)
        case l@Literal(x) =>
          '{ CBM.pure(${term.asExpr}) }.asTerm
        case other =>
             throw RuntimeException(s"Not supported $other")

   def shiftLambda(term:Term): Term =
        term match
          case lt@Lambda(params, body) =>
            val paramTypes = params.map(_.tpt.tpe)
            val paramNames = params.map(_.name)
            val mt = MethodType(paramNames)(_ => paramTypes, _ => TypeRepr.of[CB].appliedTo(body.tpe.widen) )
            val r = Lambda(Symbol.spliceOwner, mt, (newMeth, args) => changeArgs(params,args,transform(body).changeOwner(newMeth)) )
            r
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

   transform(f.asTerm).asExprOf[CB[T]]
