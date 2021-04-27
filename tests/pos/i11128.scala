package foo

object Wrap {
  export foo.Bar
}

class Bar

val wrapBar = new Wrap.Bar()
