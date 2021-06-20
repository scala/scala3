package example

import scala.math.Ordering
import scala.language.existentials

class Methods[T] {
  class List[T]
  type AList[T] = List[T]
  def m1 = ???
  def m2() = ???
  def m3(x: Int) = ???
  def m4(x: Int)(y: Int) = ???
  def m5(x: String) = ???
  def m5(x: Int) = ???
  def m6(x: Int) = ???
  def m6(x: List[T]) = ???
  def m6(x: scala.List[T]) = ???
  def m7[U: Ordering](c: Methods[T], l: List[U]) = ???
  def `m8().`() = ???
  class `m9().`
  def m9(x: `m9().`) = ???
  def m10(x: AList[T]) = ???
  def m11(x: Predef.type) = ???
  def m11(x: Example.type) = ???
  def m12a(x: {}) = ???
  def m12b(x: { val x: Int }) = ???
  def m12c(x: { val x: Int; def y: Int }) = ???
  def m13(x: Int @unchecked) = ???
  def m15(x: => Int) = ???
  def m16(x: Int*) = ???
  object m17 { def m() = ??? }
  def m17(a: Int) = ???
  def m17(b: String) = ???
  val m18 = m17
  def m18(a: Int) = ???
  def m18(b: String) = ???
  def m19(x: Int, y: Int = 2)(z: Int = 3) = ???
  def m20(a: Int) = ???
  def m20(b: String) = ???
  var m20 = m17
}
