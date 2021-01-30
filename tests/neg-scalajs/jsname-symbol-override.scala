import scala.scalajs.js
import scala.scalajs.js.annotation.*

object Syms {
  val sym1 = js.Symbol()
  val sym2 = js.Symbol()
}

abstract class A1 extends js.Object {
  @JSName(Syms.sym1)
  def bar(): Int
}
class A2 extends A1 {
  @JSName(Syms.sym2)
  override def bar() = 1 // error
}

abstract class B1 extends js.Object {
  @JSName(Syms.sym1)
  def bar(): Int
}
class B2 extends B1 {
  @JSName("baz")
  override def bar() = 1 // error
}

abstract class C1 extends js.Object {
  @JSName("foo")
  def bar(): Int
}
class C2 extends C1 {
  @JSName(Syms.sym1)
  override def bar() = 1 // error
}

abstract class D1 extends js.Object {
  @JSName(Syms.sym1)
  def bar(): Int
}
class D2 extends D1 {
  override def bar() = 1 // error
}

abstract class E1 extends js.Object {
  @JSName(Syms.sym1)
  def bar(): Object
}
abstract class E2 extends E1 {
  override def bar(): String // error
}
class E3 extends E2 {
  override def bar() = "1" // error
}

abstract class F1 extends js.Object {
  def bar(): Object
}
abstract class F2 extends F1 {
  @JSName(Syms.sym1)
  override def bar(): String // error
}
class F3 extends F2 {
  override def bar() = "1" // error
}

class G1 extends js.Object {
  def foo: Int = 5
}
trait G2 extends G1 {
  @JSName(Syms.sym1)
  def foo: Int // error
}
class G3 extends G2

class H1 extends js.Object {
  @JSName(Syms.sym1)
  def foo: Int = 5
}
trait H2 extends H1 {
  def foo: Int // error
}
class H3 extends H2

class I1[T] extends js.Object {
  @JSName(Syms.sym1)
  def foo(x: T): T = x
}
class I2 extends I1[Int] {
  override def foo(x: Int): Int = x // error
}

trait J1[T] extends js.Object {
  @JSName(Syms.sym1)
  def foo(x: T): T
}
class J2 extends J1[Int] {
  override def foo(x: Int): Int = x // error
}

class K1[T] extends js.Object {
  @JSName(Syms.sym1)
  def foo(x: T): T = x
}
trait K2 extends K1[Int] {
  def foo(x: Int): Int // error
}
class K3 extends K2 {
  override def foo(x: Int): Int = x // error
}

class L1[T] extends js.Object {
  def foo(x: T): T = x
}
trait L2 extends L1[Int] {
  @JSName(Syms.sym1)
  def foo(x: Int): Int // error
}
class L3 extends L2 {
  override def foo(x: Int): Int = x // error
}

trait M1 extends js.Object {
  def foo: Int
}
trait M2 extends js.Object {
  @JSName(Syms.sym1)
  def foo: Int
}
trait M3 extends M1 with M2 // error

trait N1 extends js.Object {
  def foo: Int
}
trait N2 extends js.Object {
  @JSName(Syms.sym1)
  def foo: Int
}
abstract class N3 extends N1 with N2 // error
