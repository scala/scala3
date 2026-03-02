transparent inline def bug(using ValueOf[1]): Int = 1
transparent inline def foo(inline arg: Any): Any =
  inline (arg, arg) match
    case (_, _) =>
    case _      =>
  end match
val x = foo(bug)
