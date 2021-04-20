import compiletime.erasedValue

object test1:

  inline def length[T]: Int =
      erasedValue[T] match
          case _: (h *: t) => 1 + length[t]
          case _: EmptyTuple => 0

  inline def foo(): Int = 1 + foo()

  val y = length[(1, 2, 3)] // error
  val x = foo() // error


