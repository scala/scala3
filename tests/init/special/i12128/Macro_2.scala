import scala.quoted._
import scala.language.experimental.macros

class Location(val line: Int)

object MacroCompat {
  trait LocationMacro {
    inline implicit def generate: Location = ${ locationImpl() }
    implicit def generate: Location = macro MacroCompatScala2.locationImpl
  }

  def locationImpl()(using Quotes): Expr[Location] = '{ new Location(${Expr(0)}) }
}

object MacroCompatScala2 {
  def locationImpl(c: Context): c.Tree = {
    import c.universe._
    val line = Literal(Constant(c.enclosingPosition.line))
    New(c.mirror.staticClass(classOf[Location].getName()), line)
  }
}
