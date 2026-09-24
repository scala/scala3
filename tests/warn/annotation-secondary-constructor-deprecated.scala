//> using options -deprecation

class A1(x: String) extends scala.annotation.StaticAnnotation {
  @deprecated("use String param") def this(x: Int) = this(x.toString)
}

@A1("string") def f1 = "x"

@A1(12345) def f2 = "y" // warn
