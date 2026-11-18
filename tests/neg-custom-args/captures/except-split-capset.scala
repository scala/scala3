import language.experimental.captureChecking
import caps.{any, Classifier, SharedCapability}

trait C   extends SharedCapability, Classifier
trait C1  extends C, Classifier
trait C11 extends C1, Classifier
trait C2  extends C, Classifier
trait D   extends SharedCapability, Classifier

class A[+T]

def twoElementBound(c: Object^, d: Object^) =
  def f1[X^ <: {c, d}](s: A[Unit]^{X}): A[Unit]^{X.only[C], c.except[C]} = s // error: d.except[C] is missing
  def f2[X^ <: {c, d}](s: A[Unit]^{X}): A[Unit]^{X.only[C], c.except[D], d.except[D]} = s // error: wrong classifier
  def f3[X^ <: {c, d}](s: A[Unit]^{X}): A[Unit]^{X.only[C], c.only[C], d.only[C]} = s // error: X.except[C] is not covered
  def f4[X^ <: {c, d}](s: A[Unit]^{X.except[C]}): A[Unit]^{c.except[C]} = s // error
  def f5[X^ <: {c, d}](s: A[Unit]^{X.except[C1]}): A[Unit]^{X.only[C1], c.except[C], d.except[C]} = s // error: X.only[C].except[C1] is not covered
  ()

def exclusionInBound(c1: Object^, c2: Object^) =
  def g[X^ <: {c1.only[C1], c2.only[C2]}](x: A[Unit]^{X.only[C]}): A[Unit]^{c1.only[C1]} = x // error: X.only[C2] is not covered
  ()

// X.except[C1] keeps the part outside C, which is not in C11
def kindBound[X^ <: {any.except[C], any.only[C11]}](s: A[Unit]^{X.except[C1]}): A[Unit]^{X.only[C11]} = s // error

// An unbounded X widens only to the root
def unbounded(a: Object^) =
  def g[X^](x: A[Unit]^{X}): A[Unit]^{X.only[C], a.except[C]} = x // error
  def k[X^](x: A[Unit]^{X.except[C]}): A[Unit]^{a} = x // error
  ()

def chain(a: Object^, b: Object^) =
  def g[X^ <: {a, b}, Y^ <: {X}](y: A[Unit]^{Y}): A[Unit]^{Y.only[C], a.except[C]} = y // error
  ()

def readOnly(c: Object^, d: Object^) =
  def h[X^ <: {c, d}](s: A[Unit]^{X}): A[Unit]^{X.only[C].rd, c.except[C], d.except[C]} = s // error
  def k[X^ <: {c, d}](s: A[Unit]^{X}): A[Unit]^{X.only[C], c.except[C].rd, d.except[C]} = s // error
  ()

trait Members(c: Object^, d: Object^):
  type X^ <: {c, d}
  def m(s: A[Unit]^{X}): A[Unit]^{X.only[C], c.except[C]} = s // error

def paths(c: Object^, d: Object^) =
  trait T:
    type X^ <: {c, d}
  def h(t: T)(s: A[Unit]^{t.X}): A[Unit]^{t.X.only[C], c.except[C]} = s // error
  ()

// X's kind is (Any \ C) | C11, so its SharedCapability parts are not all C11.
def holedUnionBound[X^ <: {any.except[C], any.only[C11]}](s: A[Unit]^{X.only[SharedCapability]}) =
  val r: A[Unit]^{X.only[C11]} = s // error

// A whole capture set parameter is not split, so its bound is not projected.
def boundNotProjected(c: Object^) =
  def f[X^ <: {c}](s: A[Unit]^{X}): A[Unit]^{c.only[C]} = s // error
  ()
