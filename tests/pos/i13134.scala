def test =
  {
    val end = 0
    assert(~end == -1) //Not found: ~
  }

  {
    val end = false
    assert(!end) // postfix operator `end` needs to be enabled
  }              // by making the implicit value scala.language.postfixOps visible.