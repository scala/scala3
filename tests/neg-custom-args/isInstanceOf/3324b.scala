class C[T] {
  val x: Any = ???
  if (x.isInstanceOf[List[String]]) // error: unchecked
    if (x.isInstanceOf[T])          // error: unchecked
      x match {
        case x: List[String] =>     // error: unchecked
        case x: T =>                // error: unchecked
      }
}
