import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}

class C

object Test extends App {
  def test(tpe: Type): Unit = {
    val sym = tpe.typeSymbol
    println(s"autoinitialized ${sym.name}: ${sym.pos.source.file.name}")
    internal.initialize(sym)
    println(s"autoinitialized ${sym.name}: ${sym.pos.source.file.name}")
  }

  Macros.foo
  println("runtime")
  test(typeOf[java.io.File])
  test(typeOf[scala.collection.BitSet])
  test(typeOf[C])
}
