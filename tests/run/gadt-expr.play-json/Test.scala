final case class Foo(bar: Int, baz: Int)

// A minimisation of a play-json test failure
// that came while implementing GadtExpr
// which was causing an inline match to fall through to the default case
// because a bug in TreeTypeMap wasn't substituting symbols correctly
object Test:
  def main(args: Array[String]): Unit =
    val json  = JsObject(Map("bar" -> JsNumber(1), "baz" -> JsNumber(2)))
    val reads = Macro.reads[Foo]
    val baz   = reads.reads(json) match
      case JsSuccess(foo) => foo.baz
      case _              => ???
    assert(baz == 2, s"expected 2 but was $baz")
