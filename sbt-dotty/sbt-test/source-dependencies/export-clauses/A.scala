class A {

  private val b: B = new B
  export b._

  class Inner {
    private val c: C = new C
    export c._
  }

  def local = {
    class Local {
      private val d: D = new D
      export d._
    }
  }

}
