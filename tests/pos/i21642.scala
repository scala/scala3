import annotation.tailrec

final class C {
  @tailrec def minimal9_2[A](value: Option[Int]): A =
    value match {
      case Some(n) => minimal9_2(None)
      case _ => ???
      //case _ => (??? : A) // workaround eliminates or relocates the cast by patmat
    }
}
