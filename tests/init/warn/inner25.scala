class A[K, V] { self =>
  def foreach[U](f: (K, V) => U): Unit = println(a)
  def withFilter(p: (K, V) => Boolean): A[K, V] = ???

  class O {
    def foreach[U](f: V => U): Unit = self.foreach {
      case (k, v) => f(v)         // `k` has type `K` instead of a TermRef
    }
  }

  println(new O)   // warn

  val a = 10
}