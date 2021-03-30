import scala.quoted._

object X {

  inline def printType[A](inline x: A): String = ${
    printTypeImpl[A]('x)
  }

  def printTypeImpl[A:Type](x:Expr[A])(using Quotes): Expr[String] = {
    import quotes.reflect._
    val value: String = x.asTerm.tpe.show
    println(value)
    Expr( value )
  }

}
