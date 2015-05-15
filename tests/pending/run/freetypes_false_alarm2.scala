import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.tools.reflect.Eval
import internal._

object Test extends dotty.runtime.LegacyApp {
  val tpe = typeOf[ru.Type]
  println(isFreeType(tpe.typeSymbol))
}
