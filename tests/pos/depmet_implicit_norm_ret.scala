object Test{
  def ?[S <: AnyRef](implicit w : S) : w.type = w

  // fallback, lower priority (overloading rules apply: pick alternative in subclass lowest in subtyping lattice)
  class ZipWithDefault {
    implicit def ZeroZipWith[S]: Test.ZipWith[S]{type T = Stream[S]} = new ZipWith[S] {
      type T = Stream[S]
    }
  }

  object ZipWith extends ZipWithDefault {
    // def apply[S: ZipWith](s : S) = ?[ZipWith[S]].zipWith(s) // TODO: bug return type should be inferred
    def apply[S](s : S)(implicit zw: ZipWith[S]): zw.T = zw.zipWith(s)

    implicit def SuccZipWith[S,R](implicit zWith : ZipWith[R]): Test.ZipWith[S => R]{type T = Stream[S] => zWith.T} = new ZipWith[S => R] {
      type T = Stream[S] => zWith.T // dependent types replace the associated types functionality
    }
  }

  import ZipWith.*

  trait ZipWith[S] {
    type T
    def zipWith : S => T = sys.error("")
  }

  // bug: inferred return type = (Stream[A]) => java.lang.Object with Test.ZipWith[B]{type T = Stream[B]}#T
  // this seems incompatible with vvvvvvvvvvvvvvvvvvvvvv   -- #3731
  def map1[A,B](f : A => B) = ZipWith(f)(SuccZipWith) // this typechecks but fails in -Ycheck:first
  val tst1: Stream[Int] = map1[String, Int]{(x: String) => x.length}.apply(Stream("a"))

  def map2[A,B](f : A => B) = ZipWith(f) // this finds ZeroZipWith where scalac finds SuccZipWith and fails typechecking in the next line.
  val tst2: Stream[Int] = map2{(x: String) => x.length}.apply(Stream("a"))
}
