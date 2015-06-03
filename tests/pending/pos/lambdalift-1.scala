class Super(x: Int)

class Sub extends Super({
  def foo3(x: Int) = {

    class C {
      def this(name: String) = this()

      def bam(y: Int): String => Int = {
        def baz = x + y
        z => baz * z.length
      }
    }

    val fun = new C("dummy").bam(1)
    fun("abc")

  }
  foo3(22)
})
