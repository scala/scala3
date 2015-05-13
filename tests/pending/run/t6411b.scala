import scala.reflect.runtime.universe._

case class Foo(n: Int) extends AnyVal
case class Bar(foo: Foo)

object Test extends dotty.runtime.LegacyApp {
  val mirror = runtimeMirror(getClass.getClassLoader)
  val cm = mirror.reflectClass(typeOf[Bar].typeSymbol.asClass)
  val ctor = typeOf[Bar].decl(termNames.CONSTRUCTOR).asMethod
  val ctorm = cm.reflectConstructor(ctor)
  println(ctorm(Foo(3)))
}
