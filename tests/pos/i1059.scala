object Repeated {
  val list = List(1, 2, 3)

  list match {
    case List(_, _, _, _ @ _*)     =>   0  // error: only allowed in Scala2 mode
    case List(_, _, _*)            =>   1  // error: only allowed in Scala2 mode
    case List(_, _: _*)            =>   2
    case Nil                       =>   3
  }
}
