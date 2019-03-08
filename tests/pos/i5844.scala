package p1 {
  trait A
  trait B[X, Y]{
    def m(): X
  }
  trait C[X] extends B[X, X & A]

  object O{
    def m(c: C[_]) = {
      val x = c.m()
    }
  }
}
package p2 {
  trait A
  trait B[X]{
    def m(): X
  }
  trait C[X] extends B[X with A]

  object O{
    def m(c: C[_]) = {
      val x: A = c.m()
    }
  }
}