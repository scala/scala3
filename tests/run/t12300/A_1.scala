trait One
trait Two

class X
class Y extends X

trait YSub extends Y

class A {
  def foo1[T <: Object with One](x: T): Unit = {}
  def foo2[T <: One with Object](x: T): Unit = {}
  def foo3[T <: One with Two](x: T): Unit = {}
  def foo4[T <: Two with One](x: T): Unit = {}
  def foo5[T <: X with Y](x: T): Unit = {}
  def foo6[T <: Y with X](x: T): Unit = {}
  def foo7[T <: X with YSub](x: T): Unit = {}
  def foo8[T <: YSub with X](x: T): Unit = {}
}
