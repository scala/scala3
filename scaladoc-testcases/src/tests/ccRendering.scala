package tests
package ccRendering

import language.experimental.captureChecking
import caps.*
import tests.ccRenderingLegacy.LegacyBase

// Members inherited from a parent compiled without capture checking keep their non-cc rendering
abstract class CCChild extends LegacyBase

class FS extends SharedCapability

// A capability-class self type does not make a class pure
trait Logger:
  self: FS =>

class Outer:
  // A pure class can take arguments that capture the impure `Outer`
  trait Helper extends Pure:
    def describe(x: AnyRef^{Outer.this}): String

trait Viewable:
  def view: AnyRef^{this}

// `this` of a pure class captures nothing, so `^{this}` is elided in inherited members
abstract class PureViewable extends Viewable, Pure
//expected: def view: AnyRef

trait Signatures:
  // The capture set of a by-name result type is not the capture set of the by-name arrow
  def byNameResultCap(x: -> AnyRef^): Int
  def byNameResultCapSet(c: AnyRef^)(x: -> AnyRef^{c}): Int
  def byNameArrowCapSet(c: AnyRef^)(x: ->{c} AnyRef): Int
  def byNameBoth(c: AnyRef^, d: AnyRef^)(x: ->{c} AnyRef^{d}): Int

  // Expanding an alias of an impure function type keeps it impure
  type ImpureCallback[A] = A => Unit
  type PureCallback[A] = A -> Unit
  def registerImpure(cb: ImpureCallback[Int]): Unit //expected: def registerImpure(cb: Int => Unit): Unit
  def registerPure(cb: PureCallback[Int]): Unit //expected: def registerPure(cb: Int -> Unit): Unit

  // An explicit empty capture set of a capability class is kept, since `FS` alone means `FS^`
  def pureFS(fs: FS^{}): Unit
  def impureFS(fs: FS): Unit
  def loggerCapturing(fs: FS)(l: Logger^{fs}): Unit

  // A capture set on a polymorphic function type is shown on the whole function, not its result type
  val polyCapturing: ([A] -> A -> Int)^
