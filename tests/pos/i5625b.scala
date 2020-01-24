object Test {

  type AV[t <: AnyVal] = t

  type LeafElem[X] <: Any = X match {
    case String => Char
    case Array[t] => LeafElem[t]
    case Iterable[t] => LeafElem[t]
    case AV[t] => t
  }

  summon[LeafElem[String] =:= Char]
  summon[LeafElem[Array[Int]] =:= Int]
  summon[LeafElem[Iterable[Int]] =:= Int]
  summon[LeafElem[Int] =:= Int]
}
