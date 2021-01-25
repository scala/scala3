final case class A()
final case class B(a:A)

object Test:

  extension(a:A)
    def x = 5

  extension(b:B)
    def x = b.a.x  // error
