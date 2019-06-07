object Test {

  type LeafElem[X] = X match {
    case String => Char
    case Array[t] => LeafElem[t]
    case Iterable[t] => LeafElem[t]
    case AnyVal => X
  }

  the[LeafElem[String] =:= Char]
  the[LeafElem[Array[Int]] =:= Int]
  the[LeafElem[Iterable[Int]] =:= Int]
  the[LeafElem[Int] =:= Int]
}