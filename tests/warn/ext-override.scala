//> using options -Xfatal-warnings

trait Foo[T]:
  extension (x: T)
    def hi: String

class Bla:
  def hi: String = "hi"
object Bla:
  given Foo[Bla]:
    extension (x: Bla)
      def hi: String = x.hi
