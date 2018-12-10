object Foo {
  def foo[U](u: Array[Int] & Array[U]): Unit = ()
}

object Test {
  def main(args: Array[String]): Unit = {
    val f1 = Foo.getClass.getMethods.find(_.nn.getName.endsWith("foo")).get.nn
    println(f1.toGenericString)
  }
}
