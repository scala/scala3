import scala.quoted._
import scala.tasty._

inline def diveInto[T]: String = ${ diveIntoImpl[T]() }

def diveIntoImpl[T]()(implicit qctx: QuoteContext, ttype: scala.quoted.Type[T]): Expr[String] =
  import qctx.tasty._
  Expr( unwindType(qctx.tasty)(Type.of[T]) )

def unwindType(reflect: Reflection)(aType: reflect.Type): String =
  import reflect._

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

