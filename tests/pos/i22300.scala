class Foo:

  inline def inlineMatch[T]: String =
    inline compiletime.erasedValue[T] match
      case _: EmptyTuple               => ""
      case _: (h *: _) if checkType[h](0) => ""

  private inline def checkType[T](i: Int): Boolean =
    inline compiletime.erasedValue[T] match
      case _: Int => true
      case _      => false

val foo = Foo()

@main def main () =
  foo.inlineMatch[(Int, Boolean)]
