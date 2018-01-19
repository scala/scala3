object Test {

  class C

  def outer(implicit c: C) = {

    def f(c: C) = implicitly[C]   // error: shadowing
    def g(c: Int) = implicitly[C] // ok since type is different

    f(new C)
  }
}
