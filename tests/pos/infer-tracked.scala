import scala.language.experimental.modularity
import scala.language.future

abstract class C:
  type T
  def foo: T

class F(val x: C):
  val result: x.T = x.foo

class G(override val x: C) extends F(x)

class H(val x: C):
  type T1 = x.T
  val result: T1 = x.foo

class I(val c: C, val t: c.T)

case class J(c: C):
  val result: c.T = c.foo

case class K(c: C):
  def result[B >: c.T]: B = c.foo

case class L(c: C):
  type T = c.T

class M

given mInst: (c: C) => M:
  def foo: c.T = c.foo

def Test =
  val c = new C:
    type T = Int
    def foo = 42

  val f = new F(c)
  val _: Int = f.result

  // val g = new G(c) // current limitation of infering in Namer, should emit a lint
  // val _: Int = g.result

  val h = new H(c)
  val _: Int = h.result

  val i = new I(c, c.foo)
  val _: Int = i.t

  val j = J(c)
  val _: Int = j.result

  val k = K(c)
  val _: Int = k.result

  val l = L(c)
  summon[l.T =:= Int]

  // val m = mInst(using c) // current limitation, we infer tracked after this desugaring
  // val _: Int = m.foo
