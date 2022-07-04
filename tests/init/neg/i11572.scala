class A {
  trait Cov[+X] {
    def get: X
  }
  trait Bounded {
    type T >: Cov[Int] <: Cov[String] // error
  }
  val t: Bounded = new Bounded {  // error
    // Note: using this instead of t produces an error (as expected)
    override type T >: t.T <: t.T
  }

  val covInt = new Cov[Int] {
    override def get: Int = 3
  }
  val str: String = ((covInt: t.T): Cov[String]).get // ClassCastException: class Integer cannot be cast to class String
}
