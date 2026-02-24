object Foo:
  class Wrap(a: String):
    def wow = s"$a - WOW!"
  extension (s: String)
    //private def wrap: Wrap = Wrap(s) // works
    private def wrap = Wrap(s) // Not found: Wrap
    export wrap.* // comment this line and it works

class Bar:
  class Wrap(a: String):
    def wow = s"$a - WOW!"
  val wrap = Wrap("string") // Not found: Wrap
  export wrap.*

object WorkingAlternative:
  object Foo:
    class Wrap(a: String):
      def wow = s"$a - WOW!"

  object Bar:
    import Foo.Wrap
    extension (s: String)
      private def wrap = Wrap(s) // works when not a member of enclosing element
      export wrap.*

class C:
  class D

class Baz:
  val c = new C
  export c.*

class Bah:
  val c = new C
  type D = c.D

@main def Test =
  val bah = Bah()
  println:
    new bah.D

  val baz = Baz()
  println:
    baz.D()
