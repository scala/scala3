def foo(x: => String) = 1 match {
    case _ => x
}