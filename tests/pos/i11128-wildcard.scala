package foo

object Outer {

  object Wrap {
    export Outer._
  }

  class Bar

}

import Outer.*

val wrapBar = new Wrap.Bar()
