object Test:
  def test(): Unit =
    val a1: String = "foo"
    val a2: a1.type = null // error

    val b1: String | Null = "foo"
    val b2: b1.type = null // error
    summon[Null <:< b1.type] // error

    /* The following would be sound, but it would require a specific subtyping
     * rule (and implementation code) for debatable value. So it is an error.
     */
    val c1: Null = null
    val c2: c1.type = null // error
  end test
end Test
