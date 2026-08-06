package ccdefs

import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class MyIO extends SharedCapability
trait Ctrl extends SharedCapability, Classifier

trait Defs:
  val a: AnyRef^
  val b: AnyRef^
  val multiCap: Int ->{a, b} Int
  val mkFresh: () -> AnyRef^{fresh}
  val impure: Int => Int
  val pure: Int -> Int
  val ctxImpure: Int ?=> Int
  val ctxPure: Int ?-> Int
  val capped: Int ->{a} Int
  val emptySet: Int ->{} Int
  val universal: AnyRef^
  val rdCap: AnyRef^{a.rd}
  val onlyCap: AnyRef^{any.only[Ctrl]}
  val exceptCap: AnyRef^{any.except[Ctrl]}
  def byname(x: ->{a} Int): Int

trait PureBase:
  this: PureBase =>
  def id: Int

class PureSub extends PureBase:
  def id = 1

trait Impure:
  val z: AnyRef^

object ImpureModule extends ExclusiveCapability:
  val io: MyIO^ = MyIO()

object UsesA extends ExclusiveCapability

class HasUsesClause uses UsesA:
  def go: Unit = ()
