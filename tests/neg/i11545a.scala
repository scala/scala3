@main def test: Unit = {
  trait Inv[A]

  trait S[+A]
  final class P[+X] extends S[Inv[String] & X]


  def patmat[A](s: S[Inv[A]]): A = s match {
    // When inferring the GADT cstr here, we need to infer cstr following from:
    //   Inv[String] & a <: Inv[A]
    // We end up invoking nonExprBaseType(`Inv[String] & a`, `Inv`),
    // which returns just `Inv[String]`. After that we approximate with:
    //   Inv[String] <: Inv[A]
    // Which is simply wrong.
    case p: P[a] => "Hello" // error
  }

  val i: Int = patmat[Int](P[Inv[Int]]())
  i
}
