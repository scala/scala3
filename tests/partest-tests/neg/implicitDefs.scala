package test

import dotty._
import Predef.{any2stringadd => _, StringAdd => _, _}

object implicitDefs {

  implicit val x = 2
  implicit def y(x: Int) = 3
  implicit def z(a: x.type): String = ""
}
