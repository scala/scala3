object NameBaseExtractor {
  def unapply(x: Int): Some[(someName: Int)] = Some((someName = x + 3))
}
object NameBaseExtractor2 {
  def unapply(x: Int): Some[(someName: Int, age: Int)] = Some((someName = x + 3, age = x + 4))
}
@main
def Test =
  val x1 = 3 match
    case NameBaseExtractor(someName = x) => x
  println(x1)
  val NameBaseExtractor(someName = x2) = 3
  println(x2)
  val NameBaseExtractor((someName = x3)) = 3
  println(x3)

  val NameBaseExtractor2(someName = x4, age = x5) = 3
  println(x4)
  println(x5)

  val NameBaseExtractor2((someName = x6, age = x7)) = 3
  println(x6)
  println(x7)

  val NameBaseExtractor(y1) = 3
  println(y1)
