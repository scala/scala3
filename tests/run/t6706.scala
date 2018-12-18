object Test {
  var name: String|Null = "foo" + 1
  var s1: Symbol|Null = Symbol(name.nn)
  s1 = null
  System.gc
  val s2 = Symbol("foo1")
  name = null
  System.gc
  val s3 = Symbol("foo1")

  def main(args: Array[String]): Unit = {
    assert(s2 eq s3, ((s2, System.identityHashCode(s2), s3, System.identityHashCode(s3))))
  }
}
