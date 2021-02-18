type LeafElem[X] = X match
   case String => Char
   case Array[t] => LeafElem[t]
   case Iterable[t] => LeafElem[t]
   case AnyVal => X

def leafElem[X](x: X): LeafElem[X] = x match
   case x: String      => x.charAt(0)
   case x: Array[t]    => leafElem(x(9))
   case x: Iterable[t] => leafElem(x.head)
   case x: AnyVal      => x
