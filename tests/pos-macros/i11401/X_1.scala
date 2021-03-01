package i11401

import scala.quoted._

import scala.concurrent.Future

def await[T](x:Future[T]):T = ???

class CIFReader[A](a:A)


class SLSelect[S]:

  def onRead[A](ch: CIFReader[A])(f: A=> S): this.type =
      ???

  def fold[S](s0:S)(step: (S,SLSelect[S])=> S): S = {
     ???
  }  

  def fold_async[S](s0:S)(step: (S,SLSelect[S])=> Future[S]): Future[S] = {
     ???
  }

  inline def apply1[A](inline ch: CIFReader[A], f: A=>S): S =
      val s0 = new SLSelect[S]
      await(s0.onRead(ch)(f).runAsync())

  def runAsync(): Future[S] = ???
      


object X:

 inline def process[T](inline f:T) = ${
   processImpl[T]('f)
 }

 def processImpl[T:Type](t:Expr[T])(using Quotes):Expr[Future[T]] = 
   import quotes.reflect._
   val r = processTree[T](t.asTerm)
   r.asExprOf[Future[T]]

 
 def processTree[T:Type](using Quotes)(t: quotes.reflect.Term):quotes.reflect.Term = 
   import quotes.reflect._
   val r: Term = t match
     case Inlined(_,List(),body) => processTree(body)
     case Inlined(d,bindings,body) => 
       Inlined(d,bindings,processTree[T](body))
     case Block(stats,expr) => Block(stats,processTree(expr))
     case Apply(Apply(TypeApply(Select(x,"fold"),targs),List(state)),List(fun)) =>
       val nFun = processLambda[T](fun) 
       Apply(Apply(TypeApply(Select.unique(x,"fold_async"),targs),List(state)),List(nFun))
     case Apply(TypeApply(Ident("await"),targs),List(body)) => body
     case Typed(x,tp) => Typed(processTree(x), Inferred(TypeRepr.of[Future].appliedTo(tp.tpe)) )
     case _ => throw new RuntimeException(s"tree not recoginized: $t")
   val checker = new TreeMap() {}
   checker.transformTerm(r)(Symbol.spliceOwner)
   r
  
 def processLambda[T:Type](using Quotes)(fun: quotes.reflect.Term):quotes.reflect.Term = 
   import quotes.reflect._

   def changeArgs(oldArgs:List[Tree], newArgs:List[Tree], body:Term, owner: Symbol):Term =
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
         changes.transformTerm(body)(owner)

   val r = fun match
     case Lambda(params, body) =>
            val nBody = processTree[T](body)
            val paramTypes = params.map(_.tpt.tpe)
            val paramNames = params.map(_.name)
            val mt = MethodType(paramNames)(_ => paramTypes, _ => TypeRepr.of[Future].appliedTo(body.tpe.widen) )
            val r = Lambda(Symbol.spliceOwner, mt, (owner,args) => changeArgs(params,args,nBody,owner).changeOwner(owner) )
            r
     case Block(List(),expr) => processLambda(expr)
     case _ =>
          throw new RuntimeException(s"Lambda expected")
   r
