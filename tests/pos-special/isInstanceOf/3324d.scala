class Test {
  val x: Any = ???

  x match {
    case _: List[Int @unchecked] => ()
    case _: Option[Int] @unchecked => ()
  }
}