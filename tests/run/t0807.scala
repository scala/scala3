trait A
trait B extends A { val x = println("early") }
object Test extends dotty.runtime.LegacyApp {
  new B {}
}
