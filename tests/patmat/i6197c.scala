def foo(x: Option[Any]) = x match {
  case _: Some[Some[?]] =>
  case _: Some[?] =>         // unreachable
  case None    =>
}