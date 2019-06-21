object ElemExample {

  type Elem[X] = X match {
    case String => Char
    case Array[t] => t
    case Iterable[t] => t
  }


  the[Elem[String]       =:=  Char]
  the[Elem[Array[Int]]   =:=  Int]
  the[Elem[List[Float]]  =:=  Float]
  the[Elem[Nil.type]     =:=  Nothing]
}
