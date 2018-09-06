object Test extends App {
  (1, 2) match {
    case (1, x) => println(x)
  }
  val x: Any = (1, 2)
  x match {
    case (1, x) => println(x)
  }

  //final class tuple2[+A, +B](_1: A, _2: B)

  final class TupleXXL1 private (es: Array[Object]) {
    override def toString = elems.mkString("(", ",", ")")
    def elems: Array[Object] = es
  }
  object TupleXXL1 {
    def apply(elems: Array[Object]) = new TupleXXL1(elems.clone)
    def apply(elems: Any*) = new TupleXXL1(elems.asInstanceOf[Seq[Object]].toArray)
    def unapplySeq(x: TupleXXL1): Option[Seq[Any]] = Some(x.elems.toSeq)
  }

  val x3 = TupleXXL1(1, 2, 3)
  x3 match {
    case TupleXXL1(x1, x2, x3) => println(x3)
  }

  val x23 = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
  x23 match {
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23) =>
      println(x1)
      println(x10)
      println(x23)
  }
  (x23: Any) match {
    case (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23) =>
      println(x1)
      println(x10)
      println(x23)
  }
}
