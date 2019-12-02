object Test {

  type LeafElem[X] = X match {
    case String => Char
    case Array[t] => LeafElem[t]
    case Iterable[t] => LeafElem[t]
    case AnyVal => X
  }

  summon[LeafElem[String] =:= Char]
  summon[LeafElem[Array[Int]] =:= Int]
  summon[LeafElem[Iterable[Int]] =:= Int]
  summon[LeafElem[Int] =:= Int]
}