case class C(someName: Int)

object NameBaseExtractor3 {
  def unapply(x: Int): Some[C] = Some(C(someName = x + 3))
}

@main
def Test = {
  val C(someName = xx) = C(3)
  println(xx)
  val NameBaseExtractor3(C(someName = x)) = 3
  println(x)
  C(3) match
    case C(someName = xx) => println(xx)
}