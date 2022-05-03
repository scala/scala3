def main: Unit = {
  {
    object A1:
      def unapplySeq(x: Int) = Some(Seq(x))

    val A1(x) = 1
    println(s"A1: $x")
  }

  {
    object A2:
      def unapplySeq[T](x: T) = Some(Seq(x))

    val A2(x) = 2
    println(s"A2: $x")
  }

  {
    object B1:
      inline def unapplySeq(x: Int) = Some(Seq(x))

    val B1(x) = 3
    println(s"B1: $x")
  }

  {
    object B2:
      inline def unapplySeq[T](x: T) = Some(Seq(x))

    val B2(x) = 4
    println(s"B2: $x")
  }

  {
    object C1:
      inline def unapplySeq(inline x: Int) = Some(Seq(x))

    val C1(x) = 5
    println(s"C1: $x")
  }

  {
    object C2:
      inline def unapplySeq[T](inline x: T) = Some(Seq(x))

    val C2(x) = 6
    println(s"C2: $x")
  }

  {
    object D1
    extension (o: D1.type) def unapplySeq(x: Int) = Some(Seq(x))

    val D1(x) = 7
    println(s"D1: $x")
  }

  {
    object D2
    extension (o: D2.type) def unapplySeq[T](x: T) = Some(Seq(x))

    val D2(x) = 8
    println(s"D2: $x")
  }

  {
    object D3
    extension [T] (o: D3.type) def unapplySeq(x: T) = Some(Seq(x))

    val D3(x) = 9
    println(s"D3: $x")
  }

  {
    object D4
    extension [T] (o: D4.type) def unapplySeq[U](x: T | U) = Some(Seq(x))

    val D4(x) = 10
    println(s"D4: $x")
  }

  {
    object E1
    extension (o: E1.type) inline def unapplySeq(x: Int) = Some(Seq(x))

    val E1(x) = 11
    println(s"E1: $x")
  }

  {
    object E2
    extension (o: E2.type) inline def unapplySeq[T](x: T) = Some(Seq(x))

    val E2(x) = 12
    println(s"E2: $x")
  }

  {
    object E4b
    extension [T] (o: E4b.type) inline def unapplySeq[U](x: U) = Some(Seq(x))

    val E4b(x) = 14.2
    println(s"E4b: $x")
  }

  {
    object F1
    extension (o: F1.type) inline def unapplySeq(inline x: Int) = Some(Seq(x))

    val F1(x) = 15
    println(s"F1: $x")
  }

  {
    object F2
    extension (o: F2.type) inline def unapplySeq[T](inline x: T) = Some(Seq(x))

    val F2(x) = 16
    println(s"F2: $x")
  }

  {
    object F4b
    extension [T] (o: F4b.type) inline def unapplySeq[U](inline x: U) = Some(Seq(x))

    val F4b(x) = 18.2
    println(s"F4b: $x")
  }

  {
    object F4d
    extension [T] (o: F4d.type) inline def unapplySeq[U](inline x: (T, U)) = Some(Seq(x))

    val F4d(x) = (18.4, 18.5)
    println(s"F4d: $x")
  }

  {
    object G1
    extension (inline o: G1.type) inline def unapplySeq(x: Int) = Some(Seq(x))

    val G1(x) = 19
    println(s"G1: $x")
  }

  {
    object G2
    extension (inline o: G2.type) inline def unapplySeq[T](x: T) = Some(Seq(x))

    val G2(x) = 20
    println(s"G2: $x")
  }

  {
    object H1
    extension (inline o: H1.type) inline def unapplySeq(inline x: Int) = Some(Seq(x))

    val H1(x) = 23
    println(s"H1: $x")
  }

  {
    object H2
    extension (inline o: H2.type) inline def unapplySeq[T](inline x: T) = Some(Seq(x))

    val H2(x) = 24
    println(s"H2: $x")
  }
}
