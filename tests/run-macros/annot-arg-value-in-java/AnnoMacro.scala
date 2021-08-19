import scala.quoted.*

inline def showAnnots(inline c: String): Unit = ${ showAnnotsImpl('c) }

def showAnnotsImpl(c: Expr[String])(using Quotes): Expr[Unit] =
  import quotes.reflect.*
  val al = Expr(Symbol.requiredClass(c.valueOrError).declaredMethods.flatMap(_.annotations.map(_.show)))
  '{
    println($c + ":")
    $al.foreach(println)
  }