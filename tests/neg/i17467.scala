object Test:
  def test(): Unit =
    val a1: String = "foo"
    val a2: a1.type = null // OK

    val b1: "foo" = null // error

    val c1: "foo" = "foo"
    val c2: c1.type = null // error

    type MyNullable = String
    val d1: MyNullable = "foo"
    val d2: d1.type = null // OK

    type MyNonNullable = Int
    val e1: MyNonNullable = 5
    val e2: e1.type = null // error

    summon[Null <:< "foo"] // error

    val f1: Mod.type = null // error

    var g1: AnyRef = "foo"
    val g2: g1.type = null // error // error

    val h1: Null = null
    val h2: h1.type = null
  end test

  object Mod

  class Bar:
    def me: this.type = null // error
end Test
