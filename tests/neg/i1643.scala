trait T extends Array { // error // error
  def t1(as: String*): Array[String] = { varargs1(as*) } // error
  def t2(as: String*): Array[String] = { super.varargs1(as*) } // error
}
class C extends Base_1 { // error
  def c1(as: String*): Array[String] = { varargs1(as*) } // error
  def c2(as: String*): Array[String] = { super.varargs1(as*) } // error
}
object Test extends App {
  val t = new T {}  // error
  println(t.t1("a", "b").mkString(","))
  println(t.t2("a", "b").mkString(","))
  val c = new C {}
  println(c.c1("a", "b").mkString(","))
  println(c.c2("a", "b").mkString(","))

  class CC[T]
  val x = new CC[_]  // error
}
