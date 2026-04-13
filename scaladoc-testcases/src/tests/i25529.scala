package tests.i25529

import language.experimental.captureChecking
import caps.*

class A extends SharedCapability:
  private var state = 0
  def f(): Unit = state += 1

object ObjA extends ExclusiveCapability:
  val a: A^ = A()

object ObjA2 extends ExclusiveCapability:
  val a: A^ = A()

object ObjB uses ObjA initially: //expected: object ObjB uses ObjA initially
  val x = ObjA.a.f()

object ObjC uses ObjA: //expected: object ObjC uses ObjA
  def x = ObjA.a.f()

object ObjD uses ObjA initially, ObjA2: //expected: object ObjD uses ObjA initially, ObjA2
  val x = ObjA.a.f()
  def y = ObjA2.a.f()

object ObjE uses ObjA initially, ObjA2 initially: //expected: object ObjE uses ObjA initially, ObjA2 initially
  val x = ObjA.a.f()
  val y = ObjA2.a.f()

object ObjF uses ObjA, ObjA2: //expected: object ObjF uses ObjA, ObjA2
  def x = ObjA.a.f()
  def y = ObjA2.a.f()

class Outer extends ExclusiveCapability:
  val o: A^ = A()

  class Inner uses Outer.this: //expected: class Inner uses Outer.this
    def x = o.f()

object Nest:
  object Sub extends ExclusiveCapability:
    val a: A^ = A()

// Module/object prefixes are elided by scaladoc's general signature-rendering
// convention (see skipPrefix in TypesSupport), so `Nest.Sub` shows as `Sub`.
class UsesPath uses Nest.Sub: //expected: class UsesPath uses Sub
  def x = Nest.Sub.a.f()
