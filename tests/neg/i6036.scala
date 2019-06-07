class A { class AProxy }

object B {
  inline def head(a: A)(code: => a.AProxy): Unit = code
  head(new A)(new {})  // error: found: Object {...} required: ?1.AProxy

  val a = new A
  head(a)(new a.AProxy{}) // ok
}