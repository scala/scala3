package x

import scala.quoted.*

trait CB[T]:
 def map[S](f: T=>S): CB[S] = ???

def await[T](x:CB[T]):T = ???

object CBM:
  def pure[T](t:T):CB[T] = ???

object X:

 inline def process[T](inline f:T) = ${
   processImpl[T]('f)
 }

 def processImpl[T:Type](f:Expr[T])(using qctx: Quotes):Expr[CB[T]] =
   import quotes.reflect.*

   def transform(term:Term): Either[Term, Term] =
     term match
        case ap@Apply(Select(obj,"foreach"),args) =>
            //  println("handle-foreach")
             val nArgs = args.map(x => shiftLambda(x).right.get)
             val nSelect = Select.unique(obj, "foreach_async")
             Right(Apply(nSelect,nArgs))
        case ap@Apply(sel@Select(obj,"awrite"),args) =>
            //  println("handle-awrite")
             transform(args.head) match
               case Left(unchanded) => Left(ap)
               case Right(changed) =>
                 val r = '{ ${changed.asExprOf[CB[?]]}.map(x => ${Apply(sel,List('x.asTerm)).asExpr}) }
                 Right(r.asTerm)
        case Apply(tap@TypeApply(Ident("await"),targs),args) =>
            //  println("handle-await")
             transform(args.head) match
                case Left(unchanged) => Right(unchanged)
                case Right(changed) =>
                   val r = Apply(tap,List(changed))
                   Right(r)
        case Lambda(v, body) => ???
        case Block(stats, last) => transform(last) match
                                     case Left(unchanged) => Left(Block(stats, last))
                                     case Right(changed) => Right(Block(stats, changed))
        case Inlined(x,List(),body) => transform(body)
        case Inlined(x,bindings,body) => transform(body) match
                      case Left(unchanged) => Left(term)
                      case Right(changed) => Right(Inlined(x,bindings,changed))
        case Typed(arg,tp) => transform(arg)
        case Ident(x) => Left(term)
        case l@Literal(x) => Left(l)
        case other =>
             throw RuntimeException(s"Not supported $other")

   def shiftLambda(term:Term): Either[Term,Term] =
        term match
          case lt@Lambda(params, body) =>
            transform(body) match
              case Left(unchanged) => Left(term)
              case Right(nBody) =>
                val paramTypes = params.map(_.tpt.tpe)
                val paramNames = params.map(_.name+"Changed")
                val mt = MethodType(paramNames)(_ => paramTypes, _ => TypeRepr.of[CB].appliedTo(body.tpe.widen) )
                val r = Lambda(Symbol.spliceOwner, mt, (owner,args) => changeArgs(params,args,nBody).changeOwner(owner) )
                Right(r)
          case _ =>
            throw RuntimeException("lambda expected")
   def changeArgs(oldArgs:List[Tree], newArgs:List[Tree], body:Term):Term =
        //  println(s"changeArgs: oldArgs=${oldArgs.map(_.symbol.hashCode)}, newArgs=${newArgs.map(_.symbol.hashCode)} ")
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

   transform(f.asTerm) match
      case Left(unchanged) => '{ CBM.pure($f) }
      case Right(changed) => changed.asExprOf[CB[T]]
