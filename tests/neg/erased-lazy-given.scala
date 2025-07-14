import language.experimental.erasedDefinitions

class E extends compiletime.Erased

object test1:
  given E = E() // OK
object test2:
  lazy given E = E() // error

