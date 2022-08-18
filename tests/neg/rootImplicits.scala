package test

import Predef.{any2stringadd as _, *}

object rootImplicits {

  println((new Object()) + "abc")    // error: `+` is not  member of Object
  println(any2stringadd(new Object()) + "abc")  // error: not found: any2stringadd

}
