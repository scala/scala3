object Repeated {
  val list = List(1, 2, 3)

  list match {
    case List(_, _, _, _ @ _*)     =>   0
    case List(_, _, _*)            =>   1
    case List(_, _: _*)            =>   2
    case Nil                       =>   3
  }
}
