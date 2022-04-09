@main def Test =
  assert(!("a": Any).isInstanceOf[Null])
  assert(null.isInstanceOf[Null])
  assert((null: Any).isInstanceOf[Null])

  ("a": Any) match
    case _: Null => assert(false)
    case _ =>

  null match
    case _: Null => ()

  (null: Any) match
    case _: Null => ()

