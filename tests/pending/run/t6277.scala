import scala.reflect.runtime.universe._

object Test extends dotty.runtime.LegacyApp {
  locally {
    val sym = typeOf[List[_]].typeSymbol.asClass
    val q = sym.isSealed
    println(q)
  }
}
