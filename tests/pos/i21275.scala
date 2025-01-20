class Box[+O]:
  def ++[O2 >: O](other: Box[O2]): Box[O2] = ???
object Box:
  val empty: Box[Nothing] = ???

def test[T]: Box[T] =
  List(Box.empty, Box.empty)
    // .reduceOption[Box[T]](_ ++ _) // works
    .reduceOption(_ ++ _) // fails
    .getOrElse(Box.empty)