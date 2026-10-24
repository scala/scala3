import caps.{any, Classifier, SharedCapability, Control}

// Capabilities that are classified accordingly still flow into projections of
// inferred capture set arguments.

trait C extends SharedCapability, Classifier
trait C1 extends C, Classifier
trait C2 extends C, Classifier
trait K1 extends SharedCapability, Classifier
trait K2 extends SharedCapability, Classifier
trait IOCap extends SharedCapability, Classifier
class Console extends IOCap:
  def print(): Unit = ()
class Label extends Control:
  def break(): Unit = ()
class A

def onlyC[X^](g: () ->{X.only[C]} Unit): Unit = ()
def onlyControl[X^](g: () ->{X.only[Control]} Unit): Unit = ()
def noControl[X^](g: () ->{X.except[Control]} Unit): Unit = ()
def onlyCNotC1[X^](g: () ->{X.only[C].except[C1]} Unit): Unit = ()
def onlyK2[X^](g: () ->{X.only[K2]} Unit): Unit = ()
def keep[X^](x: A^{X}, g: () ->{X.only[C]} Unit): Unit = ()
def keepC[X^](g: () ->{X.only[C]} Unit): A^{X} = ???
def mk[X^](g: () ->{X} Unit): () ->{X.only[C]} Unit = ???
def arg[X^](a: A^{X.only[C]}): A^{X} = ???
def inner[X^](g: () ->{X.only[C]} Unit): Unit = onlyC(g)
def h[X^](x: C^{X}, g: C^{X} => () ->{X.only[C]} Unit): Unit = ()
def noC[X^](g: () ->{X.except[C]} Unit): Unit = ()

def t1(c: C^, c1: C1^) =
  onlyC(() => println(c))
  onlyC(() => println(c1))
  onlyC(() => { println(c); println(c1) })
def t2(l: Label^, io: Console^) =
  onlyControl(() => l.break())
  noControl(() => io.print())
def t3(c: C^) =
  val g = () => println(c)
  onlyC(() => g())
  onlyC(g)
def t4[Y^ <: {any.only[C]}](k: () ->{Y} Unit) = onlyC(k)
def t5(c: C^) =
  val r: A^{c} = keepC(() => println(c))
  r
def t6(y: A^{any.only[C]}) = onlyC(() => println(y))
def t7(c: C^) = onlyC(mk(() => println(c)))
def t8(c2: C2^) = onlyCNotC1(() => println(c2))
def t9(c: C^, d: A^{c}) = onlyC(() => println(d))
def t10(c: C^) = h(c, a => () => println(a))
def t11(c1: C1^, c2: C2^) =
  val a: A^{c1, c2} = ???
  val r: A^{c1, c2} = arg(a)
def t12(c: C^) =
  val a: A^{c} = ???
  keep(a, () => println(a))
def t13(c: C^) = inner(() => println(c))

// The kind of z follows from an exclusion: y : K1 | K2, so y.except[K1] : K2.
def t14(k1: K1^, k2: K2^) =
  val y: A^{k1, k2} = ???
  val z: A^{y.except[K1]} = ???
  onlyK2(() => println(z))

// A projection of a classified capability, and a known-empty projection.
def t15(c: C^, l: Label^) =
  val a: A^{c.only[C1]} = ???
  val b: A^{l.except[Control]} = ???
  onlyC(() => { println(a); println(b) })

// z has no C parts, since y.except[C1] removes the C1 part of y : C1 | K1.
def t16(c1: C1^, k1: K1^) =
  val y: A^{c1, k1} = ???
  val z: A^{y.except[C1]} = ???
  noC(() => println(z))

// Explicit instantiations are unaffected.
class Svc(val c: C^):
  def foo(): Unit = println(c)
  def run() = onlyC[{this}](() => this.foo())

