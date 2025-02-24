object Test:
  def main(args: Array[String]): Unit =
    val a = new A
    println(a.m)

class A:
  def m: String =
    val x1 = "x1"
    class B:
      def m: String =
        val x2 = "x2"
        def m: String =
          val x3 = "x3"
          class C:
            def m: String =
              val x4 = "x4"
              def m: String =
                x1 + x2 + x3 + x4
              m
          val c = new C
          c.m
        m
      end m
    end B
    val b = new B
    b.m
