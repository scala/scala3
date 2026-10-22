import language.experimental.captureChecking
import caps.{Classifier, SharedCapability}

trait C  extends SharedCapability, Classifier
trait C1 extends C, Classifier

class A

// The projection of `d` covers the C part, and the remainder `d.except[C]`
// widens to `{c.except[C]}`.
def splitWithWidening(c: Object^) =
  val d: Object^{c} = c
  val e: Object^{d.only[C], c.except[C]} = d

def splitWithWideningMirror(c: Object^) =
  val d: Object^{c} = c
  val e: Object^{c.only[C], d.except[C]} = d

def splitWithWideningNested(c: Object^) =
  val d: Object^{c} = c
  val e: Object^{d.only[C].except[C1], c.only[C1], d.except[C]} = d

def splitWithWidening2(c: Object^) =
  val d: Object^{c} = c
  val f: Object^{d} = d
  val e: Object^{f.only[C], c.except[C]} = f

def splitWithWideningMany(c: Object^, b: Object^) =
  val d: Object^{c, b} = c
  val e: Object^{d.only[C], c.except[C], b.except[C]} = d

def splitWithWideningParam(c: Object^, d: Object^{c}) =
  val e: Object^{d.only[C], c.except[C]} = d

class SelfCycle(c: Object^):
  val d: A^{this} = ???
  def m: A^{d.only[C], this.except[C]} = d

class Outer(c: Object^):
  class Inner:
    val i: A^{Outer.this} = ???
    def m: A^{i.only[C], Outer.this.except[C]} = i

class Fwd(c: Object^):
  def a: A^{b.only[C], c.except[C]} = b
  lazy val b: A^{c} = ???

class SelfType(c: Object^):
  self: SelfType^{c.only[C], c.except[C]} =>
  val d: A^{self} = ???
  def m: A^{d.only[C], self.except[C]} = d

def rec(c: Object^)(n: Int): A^{c} =
  val d: A^{c} = rec(c)(n - 1)
  val e: A^{d.only[C], c.except[C]} = d
  e
