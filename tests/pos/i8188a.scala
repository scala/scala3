object Test {
  extension (tree: String) {
    def show(using DummyImplicit): String = ???
    def show(color: Boolean)(using DummyImplicit): String = ???
  }

  val a: String = "foo".show
  val b: String = "foo".show(true)
  val c: Any = "foo".show
  val d: Any = "foo".show(true)
}
