object Test {
  (1, 2) match {
    case x *: xs =>  // error: call to inline unapply
  }
}
