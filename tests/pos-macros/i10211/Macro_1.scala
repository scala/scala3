package x

import scala.quoted.*

trait CB[T]:
 def map[S](f: T=>S): CB[S] = ???


class MyArr[A]:
 def map[B](f: A=>B):MyArr[B] = ???
 def mapOut[B](f: A=> CB[B]): CB[MyArr[B]] = ???
 def flatMap[B](f: A=>MyArr[B]):MyArr[B] = ???
 def flatMapOut[B](f: A=>CB[MyArr[B]]):MyArr[B] = ???
 def withFilter(p: A=>Boolean): MyArr[A] = ???
 def withFilterOut(p: A=>CB[Boolean]): DelayedWithFilter[A] = ???
 def map2[B](f: A=>B):MyArr[B] = ???

class DelayedWithFilter[A]:
 def map[B](f: A=>B):MyArr[B] = ???
 def mapOut[B](f: A=> CB[B]): CB[MyArr[B]] = ???
 def flatMap[B](f: A=>MyArr[B]):MyArr[B] = ???
 def flatMapOut[B](f: A=>CB[MyArr[B]]): CB[MyArr[B]] = ???
 def map2[B](f: A=>B):CB[MyArr[B]] = ???


def await[T](x:CB[T]):T = ???

object CBM:
  def pure[T](t:T):CB[T] = ???
  def map[T,S](a:CB[T])(f:T=>S):CB[S] = ???

object X:

 inline def process[T](inline f:T) = ${
   processImpl[T]('f)
 }

 def processImpl[T:Type](f:Expr[T])(using Quotes):Expr[CB[T]] =
   import quotes.reflect.*

   def transform(term:Term):Term =
     term match
        case ap@Apply(TypeApply(Select(obj,name),targs),args)
               if (name=="map"||name=="flatMap") =>
                  obj match
                    case Apply(Select(obj1,"withFilter"),args1) =>
                      val nObj = transform(obj)
                      transform(Apply(TypeApply(Select.unique(nObj,name),targs),args))
                    case _ =>
                      val nArgs = args.map(x => shiftLambda(x))
                      val nSelect = Select.unique(obj, name+"Out")
                      Apply(TypeApply(nSelect,targs),nArgs)
        case ap@Apply(Select(obj,"withFilter"),args) =>
             val nArgs = args.map(x => shiftLambda(x))
             val nSelect = Select.unique(obj, "withFilterOut")
             Apply(nSelect,nArgs)
        case ap@Apply(TypeApply(Select(obj,"map2"),targs),args) =>
              val nObj = transform(obj)
              Apply(TypeApply(
                     Select.unique(nObj,"map2"),
                     List(TypeTree.of[Int])
                   ),
                   args
              )
        case Apply(TypeApply(Ident("await"),targs),args) => args.head
        case Apply(Select(obj,"=="),List(b)) =>
             val tb = transform(b).asExprOf[CB[Int]]
             val mt = MethodType(List("p"))(_ => List(b.tpe.widen), _ => TypeRepr.of[Boolean])
             val mapLambda = Lambda(Symbol.spliceOwner, mt, (_, x) => Select.overloaded(obj,"==",List(),List(x.head.asInstanceOf[Term]))).asExprOf[Int=>Boolean]
             '{ CBM.map($tb)($mapLambda) }.asTerm
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
            val r = Lambda(Symbol.spliceOwner, mt, (meth, args) => changeArgs(params,args,transform(body)).changeOwner(meth) )
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
