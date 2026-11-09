object Test:
  def main(args: Array[String]): Unit =
    val s: String | Null = "abc"
    val n = Opt.map(s)(_.length) // used to crash: MatchError on AndType during macro expansion
    assert(n == 3, n)
