abstract class Source[A] { self =>
  def consume(a: A): Int
  def contramap[B](f: B => A): Source[B] = {
    new Source[B] { // OfClass($anon).outerValue = {OfClass(Source), OfClass($anon)} ???
      override def consume(b: B) = self.consume(f(b))
    }
  }
}

object O {
  val identity: Source[Int] = new Source[Int] {
    override def consume(a: Int): Int = a
  } // OfClass(Source[A])
  val longToInt: Source[Long] = identity.contramap((l: Long) => l.toInt) // longToInt.outer == identity
  val doubleToLongToInt: Source[Double] = longToInt.contramap((d: Double) => (d + 2.4).toLong) // doubleToLongToInt == longToInt
  // OfClass(Source[Double]).outer = {LocalEnv(contramap)};
  // LocalEnv(contramap).outer = {OfClass(Source[Long]), OfClass(Source[Double])}
  println(doubleToLongToInt.consume(3.5))
}

