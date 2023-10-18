import language.experimental.relaxedExtensionImports

class A
object AA:
  extension (a: A)
    def f = ???
    def f_=(x: String) = ???

object BB:
  extension (b: Long)
    def f = ???
    def f_=(x: String) = ???

def test(a: A) =
  import AA.*
  import BB.*
  a.f
  a.f = "aa"
