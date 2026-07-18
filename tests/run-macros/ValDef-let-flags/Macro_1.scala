import scala.quoted._

object Macro:
  inline def makeBlock(): Unit = ${makeBlockImpl}
  def makeBlockImpl(using Quotes): Expr[Unit] = {
    import quotes.reflect._
    ValDef.let(Symbol.spliceOwner, "x", '{0}.asTerm, Flags.Mutable) { x =>
      ValDef.let(Symbol.spliceOwner, "y", '{"string"}.asTerm, Flags.Lazy) { y =>
        '{
          ${Assign(x, '{1}.asTerm).asExpr}
          println("x: " + ${x.asExprOf[Int]} + ", y: " + ${y.asExprOf[String]})
        }.asTerm
      }
    }.asExprOf[Unit]
  }
