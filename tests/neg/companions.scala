object companionsNeg {

  def foo() = {

    class C {
      private val q = 2
    }

    { object C {
      private val p = 1
      println(new C().q)   // error
    }}
  }

}

