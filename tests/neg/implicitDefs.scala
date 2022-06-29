package test

import Predef.{any2stringadd as _, *}

object implicitDefs {

  implicit val x = 2 // error: type of implicit definition needs to be given explicitly
  implicit def y(x: Int) = 3 // error: result type of implicit definition needs to be given explicitly
  implicit def z(a: x.type): String = "" // ok, used to be: implicit conversion may not have a parameter of singleton type

  def foo(implicit x: String) = 1

  def bar() = {
    implicit val x = foo // error: cyclic reference
    x
  }
}
