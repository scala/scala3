class Varargs {
  val xs1: Seq[String] = ???
  val xs2: Seq[String | Null] = ???
  val xs3: Seq[String | Null] | Null = ???
  val xs4: Seq[String] | Null = ???

  val ys1: Array[String] = ???
  val ys2: Array[String | Null] = ???
  val ys3: Array[String | Null] | Null = ???
  val ys4: Array[String] | Null = ???

  def f1(xs: String*): Unit = ???
  def f2(xs: (String | Null)*): Unit = ???

  def test = {
    f1()
    f1(null) // error
    f1("")
    f1("", null) // error
    f1(null*) // error

    f1(xs1*)
    f1(xs2*) // error
    f1(xs3*) // error
    f1(xs4*) // error

    f1(ys1*)
    f1(ys2*) // error
    f1(ys3*) // error
    f1(ys4*) // error
  }

  def test2 = {
    f2()
    f2(null)
    f2("")
    f2("", null)
    f2(null*) // error

    f2(xs1*)
    f2(xs2*)
    f2(xs3*) // error
    f2(xs4*) // error

    f2(ys1*)
    f2(ys2*)
    f2(ys3*) // error
    f2(ys4*) // error
  }
}