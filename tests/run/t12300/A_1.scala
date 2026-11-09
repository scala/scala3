trait One
trait Two

class X
class Y extends X

trait YSub extends Y

class A {
  def foo1[T <: Object & One](x: T): Unit = {}
  def foo2[T <: One & Object](x: T): Unit = {}
  def foo3[T <: One & Two](x: T): Unit = {}
  def foo4[T <: Two & One](x: T): Unit = {}
  def foo5[T <: X & Y](x: T): Unit = {}
  def foo6[T <: Y & X](x: T): Unit = {}
  def foo7[T <: X & YSub](x: T): Unit = {}
  def foo8[T <: YSub & X](x: T): Unit = {}
}
