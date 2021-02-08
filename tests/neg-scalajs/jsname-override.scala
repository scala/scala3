import scala.scalajs.js
import scala.scalajs.js.annotation.*

abstract class A1 extends js.Object {
  @JSName("foo")
  def bar(): Int
}
class A2 extends A1 {
  @JSName("baz")
  override def bar() = 1 // error
}

abstract class B1 extends js.Object {
  @JSName("foo")
  def bar(): Int
}
class B2 extends B1 {
  override def bar() = 1 // error
}

abstract class C1 extends js.Object {
  @JSName("foo")
  def bar(): Object
}
abstract class C2 extends C1 {
  override def bar(): String // error
}
class C3 extends C2 {
  override def bar() = "1" // error
}

abstract class D1 extends js.Object {
  def bar(): Object
}
abstract class D2 extends D1 {
  @JSName("foo")
  override def bar(): String // error
}
class D3 extends D2 {
  override def bar() = "1" // error
}

class E1 extends js.Object {
  def foo: Int = 5
}
trait E2 extends E1 {
  @JSName("bar")
  def foo: Int // error
}
class E3 extends E2

class F1 extends js.Object {
  @JSName("bar")
  def foo: Int = 5
}
trait F2 extends F1 {
  def foo: Int // error
}
class F3 extends F2

class G1[T] extends js.Object {
  @JSName("bar")
  def foo(x: T): T = x
}
class G2 extends G1[Int] {
  override def foo(x: Int): Int = x // error
}

trait H1[T] extends js.Object {
  @JSName("bar")
  def foo(x: T): T
}
class H2 extends H1[Int] {
  override def foo(x: Int): Int = x // error
}

class I1[T] extends js.Object {
  @JSName("bar")
  def foo(x: T): T = x
}
trait I2 extends I1[Int] {
  def foo(x: Int): Int // error
}
class I3 extends I2 {
  override def foo(x: Int): Int = x // error
}

class J1[T] extends js.Object {
  def foo(x: T): T = x
}
trait J2 extends J1[Int] {
  @JSName("bar")
  def foo(x: Int): Int // error
}
class J3 extends J2 {
  override def foo(x: Int): Int = x // error
}

trait K1 extends js.Object {
  def foo: Int
}
trait K2 extends js.Object {
  @JSName("bar")
  def foo: Int
}
trait K3 extends K1 with K2 // error

trait L1 extends js.Object {
  def foo: Int
}
trait L2 extends js.Object {
  @JSName("bar")
  def foo: Int
}
abstract class L3 extends L1 with L2 // error
