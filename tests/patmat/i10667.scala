sealed trait A

enum Nums {
  case One
  case Two extends Nums with A
  case Three
}

object Test {
  val list = List[Nums & A](Nums.Two)

  list.map {
    case Nums.Two => ()
  }
}
