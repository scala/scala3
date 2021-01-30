import scala.quoted.*

inline def diveInto[T]: String = ${ diveIntoImpl[T]() }

def diveIntoImpl[T]()(implicit qctx: Quotes, ttype: Type[T]): Expr[String] =
  import quotes.reflect.*
  Expr( unwindType(TypeRepr.of[T]) )

def unwindType(using Quotes)(aType: quotes.reflect.TypeRepr): String =
  import quotes.reflect.*

  aType match {
    case AppliedType(t,tob) =>
      val cs = t.classSymbol.get.primaryConstructor  // this is shared
      val a = cs.paramSymss  // this call succeeds
      // println("a: "+a)
      val b = cs.paramSymss // this call explodes
      // println("b: "+b)

    case _ =>
  }
  "OK!"

