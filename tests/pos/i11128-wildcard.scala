package foo

object Outer {

  object Wrap {
    export Outer._
  }

  class Bar

}

import Outer._

val wrapBar = new Wrap.Bar()
