object Test {
  
  type AV[t <: AnyVal] = t

  type LeafElem[X] = X match {
    case String => Char
    case Array[t] => LeafElem[t]
    case Iterable[t] => LeafElem[t]
    case AV[t] => t
  }

  the[LeafElem[String] =:= Char]
  the[LeafElem[Array[Int]] =:= Int]
  the[LeafElem[Iterable[Int]] =:= Int]
  the[LeafElem[Int] =:= Int]
}