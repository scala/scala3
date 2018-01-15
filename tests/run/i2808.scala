class C {
  def m1_:(f: Int) = ()
  def m2_:(f: => Int) = ()
  def m3_:(f: Int)(implicit c: C) = ()
  def m4_:(f: => Int)(implicit c: C) = ()

}

object Test {
  def foo1() = { println("foo1") ; 5 }
  def foo2() = { println("foo2") ; 5 }
  def foo3() = { println("foo3") ; 5 }
  def foo4() = { println("foo4") ; 5 }

  def main(args: Array[String]): Unit = {
    implicit val c = new C
    foo1() m1_: c // foo1
    foo2() m2_: c
    foo3() m3_: c // foo3
    foo4() m4_: c

    def bar() = { println("bar"); c }
    foo1() m1_: bar() // foo1
                      // bar
    foo2() m2_: bar() // bar
    foo3() m3_: bar() // foo3
                      // bar
    foo4() m4_: bar() // bar
  }
}
