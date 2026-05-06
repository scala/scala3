import language.experimental.erasedDefinitions

class E extends compiletime.Erased

object Test {
  erased lazy val i: Int = 1 // error
  lazy val e: E = E() // error
  erased object obj1 // error
  object obj2 extends E // ok, obj2 is not erased
}
