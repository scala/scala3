object test {

  def foo(x: Int) = {

    def bar(y: Int) = x + y
    def baz(z: Int) = bar(z)

    baz(1)

  }

  def foo2(x: Int) = {

    class C {
      def bam(y: Int): String => Int = {
        def baz = x + y
        z => baz * z.length
      }
    }

    val fun = new C().bam(1)
    fun("abc")

  }
}
