object Test {
  def test0: Unit = {
    trait I {
      def foo: Any = null
    }
    val s: I = null
    s.foo
  }

  def test1: Unit = {
    // `s1` is used once as a value, several times as a type. This tests shows
    // that Ycheck is happy despite devalify inlining (and removing) `s1`.
    val s1: String = "singleton"
    val s2: s1.type = s1

    val t: Option[s1.type] = None

    println(t)
    println((s2: s1.type))
  }

  def test2: Unit = {
    class Foo {
      class Bar
    }

    val foo = new Foo
    val subFoo = foo
    // Inlining `subFoo` changes the type of `subFooBar` from `subFoo.Bar` to `foo.Bar`
    val subFooBar = new subFoo.Bar

    println(subFooBar)
  }
}
