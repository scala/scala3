trait M {
  @scala.annotation.init
  def f: Int

  def g: Int

  class B {
    def m: Int = f
  }

  class C {
    def m: Int = g
  }

  println(new B)
  println(new C)  // error
}

trait N {
  @scala.annotation.init
  def f: Int

  def g: Int

  class C {
    class B {
      def m: Int = f
    }
    println(new B)
  }

  new C

  class X {
    class Y {
      def m: Int = g
    }
    println(new Y)    // error
  }

  new X               // error
}