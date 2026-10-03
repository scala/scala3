// https://github.com/scala/scala3/issues/23137
object Foo23137:
  class Inner

  opaque type Type = Inner

  inline def of[A]: OfOps = new OfOps
  class OfOps:
    inline def apply(): Type = new Inner

@main def run23137: Unit = Foo23137.of()
