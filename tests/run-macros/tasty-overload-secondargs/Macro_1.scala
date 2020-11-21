import scala.quoted._

object X:

  def andThen[A,B](a:A)(f: A => B): B =
     println("totalFunction")
     f(a)

  def andThen[A,B](a:A)(f: PartialFunction[A,B]): Option[B] =
     println("partialFunction")
     f.lift.apply(a)


object Macro:

    inline def mThen[A,B](inline x:A=>B):B = ${
       mThenImpl[A,B,A=>B,B]('x)
    }

    inline def mThen[A,B](inline x:PartialFunction[A,B]): Option[B] = ${
       mThenImpl[A,B,PartialFunction[A,B],Option[B]]('x)
    }

    def mThenImpl[A:Type, B:Type, S<:(A=>B) :Type, R:Type](x:Expr[S])(using Quotes):Expr[R]=
       import qctx.reflect._
       val fun = Term.of('{X})
       val returnType = TypeRepr.of[(S) => ?]
       val firstPart = Select.overloaded(fun,"andThen",
                                 List(TypeIdent(defn.IntClass).tpe, TypeIdent(defn.IntClass).tpe),
                                 List(Literal(Constant.Int(1))),
                                 TypeRepr.of[(S) => R]
                       )
       val r = Apply(firstPart,List(Term.of(x)))
       r.asExprOf[R]
