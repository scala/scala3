object `inline-match-specialize` {
  case class Box[+T](value: T)
  transparent inline def specialize[T](box: Box[T]): Box[T] = inline box match {
    case box: Box[t] => box
  }

  val ibox: Box[Int] = specialize[Any](Box(0))
}
