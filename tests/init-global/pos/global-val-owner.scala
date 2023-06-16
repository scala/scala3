object Test:
  val n = {
    def fact(x: Int): Int = if x == 0 then 1 else x * fact(x - 1)
    fact(5)
  }

  def foo() =
    val n = {
      def fact(x: Int): Int = if x == 0 then 1 else x * fact(x - 1)
      fact(5)
    }
    n

  val x = foo()

