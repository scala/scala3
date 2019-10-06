object ElemExample {

  type Elem[X] = X match {
    case String => Char
    case Array[t] => t
    case Iterable[t] => t
  }


  summon[Elem[String]       =:=  Char]
  summon[Elem[Array[Int]]   =:=  Int]
  summon[Elem[List[Float]]  =:=  Float]
  summon[Elem[Nil.type]     =:=  Nothing]
}
