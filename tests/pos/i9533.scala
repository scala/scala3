object Test {
  sealed trait Base[-X]
  case class Child[X]() extends Base[X]

  def apply[A, B](r: Base[A & B]): Unit = {
    r match {
      case Child() => ()
    }
  }
}