object Test {
  import language.higherKinds

  class NotUsed

  trait FO[+Out, +Mat] { self =>
    type Repr[+O] <: FO[O, Mat] {
      type Repr[+OO] = self.Repr[OO]
    }
    def map[T](f: Out => T): Repr[T] = ???
  }

  class Source[+O, +M] extends FO[O, M] {
    type Repr[+OO] <: Source[OO, M]
  }

  class Flow[-I, +O, +M] extends FO[O, M] {
    type Repr[+OO] <: Flow[I, OO, M]
  }

  implicit class x[O, M, F[o, m] <: FO[o, m]](val f: F[O, M]) extends AnyVal {
    def xx(i: Int): f.Repr[O] = f.map(identity)
  }

  type IntFlow[O, M] = Flow[Int, O, M]

  val s1 = new Source[Int, NotUsed].xx(12)
  val s2: Source[Int, NotUsed] = s1
  val f1 = x[Int, NotUsed, IntFlow](new Flow[Int, Int, NotUsed]).xx(12)
  val f2: Flow[Int, Int, NotUsed] = f1
}
