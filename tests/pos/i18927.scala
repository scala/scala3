class A

class B {
  val a = new A

  class C(i: Int) {
    def this() = {
      this(1)
      class Inner() {
        println(a)
      }
    }
  }
}