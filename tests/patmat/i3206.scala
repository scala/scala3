object Test {
  val foo: PartialFunction[Option[Int], Int] = {
    case Some(x) => x
  }
}
