// Keep synchronized with dottyApp/Api.scala
package scala2Lib

import scala.scalajs.js
import js.|

class A {
  def foo(x: Int | String): String = "1"
  def foo(x: Array[Int]): String = "2"
}

class B extends js.Object {
  def foo(x: Int | String): String = "1"
  def foo(x: Array[Int]): String = "2"
}
