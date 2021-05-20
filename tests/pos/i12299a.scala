object Outer {

  object Wrap {
    export Outer.Bar
  }

  class Bar

  val wrapBar = Wrap.Bar()
}

