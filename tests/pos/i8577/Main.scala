package i8577

def main: Unit = {
  {
    1 match
      case mac"$x" => x
  }
}

























//  {
//    // B
//    object F2
//    extension (o: F2.type) inline def unapplySeq[T](inline x: T) = Some(Seq(x))
//
//    val F2(x) = 16
//    println(s"F2: $x")
//  }
//
//  {
//    // C
//    object F1
//    extension [T] (o: F1.type) inline def unapplySeq(inline x: T) = Some(Seq(x))
//
//    val F1(x) = 15
//    println(s"F1: $x")
//  }
//
//  {
//    // D
//    object F4b
//    extension [T] (o: F4b.type) inline def unapplySeq[U](inline x: T) = Some(Seq(x))
//
//    val F4b(x) = 18.2
//    println(s"F4b: $x")
//  }
//
//  {
//    // E
//    object F4b
//    extension [T] (o: F4b.type) inline def unapplySeq[U](inline x: U) = Some(Seq(x))
//
//    val F4b(x) = 18.2
//    println(s"F4b: $x")
//  }
//
//  {
//    // F
//    object F4d
//    extension [T] (o: F4d.type) inline def unapplySeq[U](inline x: (T, U)) = Some(Seq(x))
//
//    val F4d(x) = (18.4, 18.5)
//    println(s"F4d: $x")
//  }
//
//  {
//    // G
//    object H1
//    extension (inline o: H1.type) inline def unapplySeq(inline x: Int) = Some(Seq(x))
//
//    val H1(x) = 23
//    println(s"H1: $x")
//  }
//
//  {
//    // H
//    object H2
//    extension (inline o: H2.type) inline def unapplySeq[T](inline x: T) = Some(Seq(x))
//
//    val H2(x) = 24
//    println(s"H2: $x")
//  }
//
//  {
//    // I
//    object H2
//    extension [T] (inline o: H2.type) inline def unapplySeq(inline x: T) = Some(Seq(x))
//
//    val H2(x) = 24
//    println(s"H2: $x")
//  }
//
//  {
//    // J
//    object H2
//    extension [T] (inline o: H2.type) inline def unapplySeq[U](inline x: T) = Some(Seq(x))
//
//    val H2(x) = 24
//    println(s"H2: $x")
//  }
//
//  {
//    // K
//    object H2
//    extension [T] (inline o: H2.type) inline def unapplySeq[U](inline x: U) = Some(Seq(x))
//
//    val H2(x) = 24
//    println(s"H2: $x")
//  }
//
//  {
//    // L
//    object H2
//    extension [T] (inline o: H2.type) inline def unapplySeq[U](inline x: (T, U)) = Some(Seq(x))
//
//    val H2(x) = (24, "a")
//    println(s"H2: $x")
//  }
//}
