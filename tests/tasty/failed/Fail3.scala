//compilation fails after unpickling
  class Test() {
    import Test._

    val myStatus = Unknown
  }
  object Test {
    private val Unknown: Int = 0 // not yet computed
  }