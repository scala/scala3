object companions {

  def foo() = {

    class C {
      println(C.p)
    }

    object C {
      private val p = 1
    }
  }

}
