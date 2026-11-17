import caps.{Classifier, SharedCapability, Control}

// No instance of an inferred capture set argument X makes X.only[C] account for a
// capability that is not classified as C, or X.except[C] account for a capability
// that might have C-classified parts.

trait C extends SharedCapability, Classifier
trait C1 extends C, Classifier
class A
class Holder(val f: A^)
class Label extends Control:
  def break(): Unit = ()

def onlyC[X^](g: () ->{X.only[C]} Unit): Unit = ()
def noControl[X^](g: () ->{X.except[Control]} Unit): Unit = ()
def onlyCNotC1[X^](g: () ->{X.only[C].except[C1]} Unit): Unit = ()
def keep[X^](x: A^{X}, g: () ->{X.only[C]} Unit): Unit = ()
def keepField[X^](x: Holder^{X}, g: () ->{X.only[C]} Unit): Unit = ()
def lam[X^](x: A^{X}, g: A^{X} => () ->{X.only[C]} Unit): Unit = ()
def arg[X^](a: A^{X.only[C]}): A^{X} = a
def inner[X^](g: () ->{X.only[C]} Unit): Unit = onlyC(g)

def test1(y: A^) = onlyC(() => println(y))                     // error
def test2(l: Label^) = noControl(() => l.break())               // error
def test3(l: Label^) = noControl[{l}](() => l.break())          // error
def test4(c: C^) = onlyCNotC1(() => println(c))                 // error: c might be a C1
def test5(y: A^) = keep(y, () => println(y))                    // error, although X accounts for y
def test6(z: Holder^) = keepField(z, () => println(z.f))        // error, although X accounts for z.f through z
def test7(y: A^) = lam(y, a => () => println(a))                // error
def test8(c: Object^, y: A^{c}): A^{c} = arg(y)                 // error
def test9(y: A^) = inner(() => println(y))                      // error
def test10(c: C^, d: Object^) = onlyC(() => { println(c); println(d) }) // error: d is not a C
def test11(c1: C1^) =
  val a: A^{c1} = ???
  def chain[X^](a: A^{X.only[C].except[C1]}): A^{X} = ???
  chain(a)                                                      // error
def test12(z: A^) =
  val y: A^{z} = ???
  keep(z, () => println(y))                                     // error, although X accounts for y by widening to z

class Svc(val c: C^, val d: Object^):
  def foo(): Unit = { println(c); println(d) }
  def run() = onlyC(() => this.foo())                           // error

