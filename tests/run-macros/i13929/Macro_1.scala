import scala.quoted.*

inline def testSymLet[T](f: T) = ${ testSymLetImpl[T]('f) }

def testSymLetImpl[T: Type](f: Expr[T])(using Quotes): Expr[Unit] = {
  import quotes.reflect.*

  val tpe = TypeRepr.of[T]

  val valSym = Symbol.newVal(
    Symbol.spliceOwner,
    "myVal",
    tpe,
    Flags.Lazy,
    Symbol.noSymbol
  )

  val earlyRef = Typed(Ref(valSym), Inferred(tpe))

  ValDef.let(Symbol.spliceOwner, valSym, f) { vref =>
    '{
      println("early=" + ${earlyRef.asExpr}.toString())
      println("vref=" + ${vref.asExpr}.toString())
    }.asTerm
  }.asExprOf[Unit]
}
