object Foo {
  def foo[U](i: Int, s: String, x: Long)(c: Boolean, a: U, d: String)(e: Object): Float = 0.0f
}

object Test {
  def main(args: Array[String]): Unit = {
    val f1 = Foo.getClass.getMethods.find(_.getName.endsWith("foo")).get
    println(f1.toGenericString)
  }
}
