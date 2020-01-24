object Test {
  type LeafElem[X] <: Any = X match {
    case String => Char
    case Array[t] => LeafElem[t]
    case Iterable[t] => LeafElem[t]
    case AnyVal => X
  }
}
