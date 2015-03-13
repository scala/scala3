package test

import dotty._
import Predef.{any2stringadd => _, StringAdd => _, _}

object rootImplicits {

  println((new Object()) + "abc")    // error: `+` is not  member of Object
  println(any2stringadd(new Object()) + "abc")  // error: not found: any2stringadd

}
