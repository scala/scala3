//> using options -Yexplicit-nulls

import caps.*
import java.util.concurrent.atomic.AtomicReference

class A:
  def hi() = "hi"

class Concrete(x: A^):
  val ref: AtomicReference[A^{x}] = AtomicReference[A^{x}](x)
  ref.set(x)
  def hi =
    val v = ref.get()  // boxed result of Java method, wrapped in a flexible null type
    v.hi()
