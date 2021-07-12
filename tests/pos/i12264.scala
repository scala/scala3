object test1:

  object Html {
    final opaque type Tag[+N] = String
    def apply[N](name: String): Tag[N] = ???
  }

  object HtmlTags {
    final def br: Html.Tag[Int] = Html("br")
    final def p = Html[Long]("p")
  }

  object Test {
    type Expect = Html.Tag[Any]

    val x = List[Expect](HtmlTags.br, HtmlTags.p) // ok

    val y = List(HtmlTags.br, HtmlTags.p)
    y: List[Expect] // was error
  }

class test2:
  type Tag[+N]
  object Html:
    def apply[N](name: String): Tag[N] = ???

  object HtmlTags {
    final def br: Tag[Int] = Html("br")
    final def p = Html[Long]("p")
  }

  object Test {
    type Expect = Tag[Any]

    val x = List[Expect](HtmlTags.br, HtmlTags.p) // ok

    val y = List(HtmlTags.br, HtmlTags.p)
    y: List[Expect] // was error
  }

