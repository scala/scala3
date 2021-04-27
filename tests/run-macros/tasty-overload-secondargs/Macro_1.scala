import scala.quoted.*

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

    def mThenImpl[A:Type, B:Type, S<:(A=>B) :Type, R:Type](x:Expr[S])(using Quotes):Expr[R] =
       import quotes.reflect.*
       val fun = '{X}.asTerm
       val returnType = TypeRepr.of[(S) => ?]
       val firstPart = Select.overloaded(fun,"andThen",
                                 List(TypeIdent(defn.IntClass).tpe, TypeIdent(defn.IntClass).tpe),
                                 List(Literal(IntConstant(1))),
                                 TypeRepr.of[(S) => R]
                       )
       val r = Apply(firstPart,List(x.asTerm))
       r.asExprOf[R]
