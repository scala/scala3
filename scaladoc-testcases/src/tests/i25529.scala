package tests.i25529

import language.experimental.captureChecking
import caps.*

class A extends SharedCapability:
  private var state = 0
  def f(): Unit = state += 1

object ObjA:
  val a: A^ = A()

object ObjB uses_init ObjA: //expected: object ObjB uses_init ObjA
  val x = ObjA.a.f()

object ObjC uses ObjA: //expected: object ObjC uses ObjA
  def x = ObjA.a.f()
