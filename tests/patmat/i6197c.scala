def foo(x: Option[Any]) = x match {
  case _: Some[Some[_]] =>
  case _: Some[_] =>         // unreachable
  case None    =>
}