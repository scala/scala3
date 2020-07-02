import annotation.varargs

// Failing varargs annotation
object Test {

  trait A {
    def v1(a: Int, b: Array[String]) = a
  }

  trait B extends A { // error (could we get rid of that one?)
    @varargs def v1(a: Int, b: String*) = a + b.length // error
  }

  @varargs def nov(a: Int) = 0 // error: A method without repeated parameters cannot be annotated with @varargs
  @varargs def v(a: Int, b: String*) = a + b.length // ok

  @varargs def v2(a: Int, b: String*) = 0 // error
  def v2(a: Int, b: Array[String]) = 0

  @varargs def v3(a: String*)(b: Int) = b + a.length // error
  @varargs def v4(a: String)(b: Int) = b + a.length // error
  @varargs def v5(a: String)(b: Int*) = a + b.sum // ok

  @varargs def v6: Int = 1 // error
  @varargs def v7(i: Int*)() = i.sum // error

}
