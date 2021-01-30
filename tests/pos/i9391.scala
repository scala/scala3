def g(x: => Any): Any = x

val a: PartialFunction[Any => Any, Any] = (f => g(f(0)) match { case v => v })    // was an error, now OK
