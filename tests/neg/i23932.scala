//> using options -language:experimental.erasedDefinitions

// Test for issue #23932: Missing abstract method validation with erased types
// https://github.com/scala/scala3/issues/23932

import scala.compiletime.Erased
import scala.annotation.experimental
import scala.annotation.targetName

@experimental
class In[T <: AnyKind] extends Erased

// Case 1: Original issue - extension method with erased param should not count as implementation
trait Semigroup:
  type Self
  def add(x: Self, y: Self): Self

@experimental
trait Monoid extends Semigroup:
  def empty: Self
  extension (in: In[Self])
    @targetName("empty_in")
    def empty: Self = this.empty  // extension method with same name, different target name

object Semigroup:
  @experimental
  given badMonoid: Monoid with  // error: missing implementation of empty
    type Self = String
    def add(x: Self, y: Self): Self = x
    // Missing: empty method implementation - should be detected at compile time

// Case 2: Extension method should not implement abstract method even with matching signature
trait HasMethod:
  def foo: Int

trait HasExtension extends HasMethod:
  extension (x: String)
    def foo: Int = 42  // extension method, should not implement abstract foo

class BadImpl extends HasExtension  // error: missing implementation of foo

// Case 3: Normal (non-extension) method should still work
trait GoodBase:
  def bar: String

class GoodImpl extends GoodBase:
  def bar: String = "ok"  // OK - proper implementation

// Case 4: Extension method with params should not implement abstract method with same name
trait HasMethodWithParam:
  def compute(x: Int): Int

trait HasExtensionWithParam extends HasMethodWithParam:
  extension (s: String)
    def compute(x: Int): Int = x + s.length

class BadImplWithParam extends HasExtensionWithParam  // error: missing implementation of compute

// Case 5: Multiple abstract methods, one provided, one with only extension
trait TwoMethods:
  def first: Int
  def second: String

trait MixedImpl extends TwoMethods:
  def first: Int = 1  // OK - proper implementation
  extension (x: Double)
    def second: String = x.toString  // extension, should not implement abstract second

class BadMixedImpl extends MixedImpl  // error: missing implementation of second

// Case 6: Extension method with type parameter should not implement abstract method
trait GenericTrait[T]:
  def process(x: T): T

trait ExtensionGeneric[T] extends GenericTrait[T]:
  extension (s: String)
    def process(x: T): T = x  // extension with type param

class BadGenericImpl extends ExtensionGeneric[Int]  // error: missing implementation of process

// ============================================================================
// POSITIVE CASES: Extension methods CAN implement abstract extension methods
// ============================================================================

// Case 7: Extension method properly implementing abstract extension method
trait AbstractExtension[T]:
  extension (x: T) def show: String

class ConcreteExtension extends AbstractExtension[Int]:
  extension (x: Int) def show: String = x.toString  // OK - extension implementing abstract extension

// Case 8: Type class pattern with extension methods
trait Showable[T]:
  extension (x: T) def display: String

object IntShowable extends Showable[Int]:
  extension (x: Int) def display: String = s"Int($x)"  // OK - proper extension implementation

// Case 9: Multiple extension methods in type class
trait Numeric[T]:
  extension (x: T)
    def plus(y: T): T
    def times(y: T): T

object IntNumeric extends Numeric[Int]:
  extension (x: Int)
    def plus(y: Int): Int = x + y  // OK
    def times(y: Int): Int = x * y  // OK
