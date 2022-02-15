class Bar:
  @foo def bar(x: String): String = x // error
  bar("a")
