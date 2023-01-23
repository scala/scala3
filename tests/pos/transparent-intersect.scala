object Test:

  val x: Any { type T = Int } & Object = ???
  val y = if ??? then x else x
  val _ : Object { type T = Int } = y

