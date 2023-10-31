object Test {
  def test: Unit = {
    val e: Enum[?] = java.util.concurrent.TimeUnit.SECONDS
    e match { case x => println(x) }


    trait TA[X <: CharSequence]
    val ta: TA[?] = new TA[String] {}

    ta match {
      case _ => println("hi")
    }

    def f(ta: TA[?]) = ta match { case _ => "hi" }
  }
}
