object test {

  trait A { type T; }

  trait B { type T; }

  /** def functor(x: A): B { type T = x.T } */
  abstract class functor() {
    val arg: A;
    val res: B { type T = arg.T } =
      new B { type T = arg.T; };
  }

  val a = new  A { type T = String };
  /** val b: B { type T = String } = functor(a) */
  val b: B { type T = String } = {
    val tmp = new functor() { val arg: A { type T = String } = a };
      // Dotty deviaton: arg needs an explicit type here, or else the inherited type `A` would be assumed.
    tmp.res
  }

}
