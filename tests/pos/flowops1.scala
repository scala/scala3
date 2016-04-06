object Test {
  class NotUsed

  trait FO[type +Out, type +Mat] { self =>
    type Repr <: FO[Mat = self.Mat] {
      type Repr = self.Repr
    }
    def map[T](f: Out => T): Repr[Out = T] = ???
  }

  class Source[type +Out, type +Mat] extends FO[Out, Mat] { self =>
    type Repr <: Source[Mat = self.Mat]
  }

  class Flow[type -In, type +Out, type +Mat] extends FO[Out, Mat] { self =>
    type Repr <: Flow[In = self.In, Mat = self.Mat]
  }

  implicit class x[O, M, F <: FO](val f: F[Out = O, Mat = M]) extends AnyVal {
    def xx(i: Int): f.Repr[Out = O] = f.map(identity)
  }

  class xalt[O, M, F <: FO](val f: F[Out = O, Mat = M]) extends AnyVal {
    def xx(i: Int): FO[Out = O, Mat = M] = ???
  }

  val s1 = new Source[Int, NotUsed].xx(12)
  val s2: Source[Int, NotUsed] = s1
  val f1 = x[Int, NotUsed, Flow[In = Int]](new Flow[Int, Int, NotUsed]).xx(12)
  val f2: Flow[Int, Int, NotUsed] = f1


  val f3 = x(new Flow[Int, Int, NotUsed]).xx(12)
  val f4: Flow[Int, Int, NotUsed] = f3
  val f5 = new Flow[Int, Int, NotUsed].xx(12)
  val f6: Flow[Int, Int, NotUsed] = f5
  val f7 = new xalt(new Flow[Int, Int, NotUsed]).xx(12)
  val f8: FO[Int, NotUsed] = f7
}
