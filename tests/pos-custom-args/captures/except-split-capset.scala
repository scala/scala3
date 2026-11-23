import language.experimental.captureChecking
import caps.{any, Classifier, SharedCapability}

trait C   extends SharedCapability, Classifier
trait C1  extends C, Classifier
trait C11 extends C1, Classifier
trait C2  extends C, Classifier

class A[+T]

// The remainder X.except[C] of a split is widened through the bound of X,
// keeping the projection: X <: {c, d} gives X.except[C] <: {c.except[C], d.except[C]}.
def twoElementBound(c: Object^, d: Object^) =
  def f[X^ <: {c, d}](s: A[Unit]^{X}): A[Unit]^{X.only[C], c.except[C], d.except[C]} = s
  def g[X^ <: {c, d}](s: A[Unit]^{X.except[C1]}): A[Unit]^{X.only[C], c.except[C], d.except[C]} = s
  ()

// Exclusions are applied to the bound elements: c1.only[C1].except[C1] is empty.
def exclusionInBound(c1: Object^, c2: Object^) =
  def g[X^ <: {c1.only[C1], c2.only[C2]}](x: A[Unit]^{X}): A[Unit]^{X.only[C1], c2.only[C2]} = x
  ()

// Kind bounds encoded with projected roots: the widened remainder is empty.
def kindBound1[X^ <: {any.except[C], any.only[C11]}](s: A[Unit]^{X.only[C1]}): A[Unit]^{X.only[C11]} = s
def kindBound2[X^ <: {any.except[C], any.only[C11]}](s: A[Unit]^{X}): A[Unit]^{X.except[C1], X.only[C11]} = s

// Chains of capture set parameters: Y.except[C] widens to X.except[C], which widens again.
def chain(a: Object^, b: Object^) =
  def g[X^ <: {a, b}, Y^ <: {X}](y: A[Unit]^{Y}): A[Unit]^{Y.only[C], a.except[C], b.except[C]} = y
  ()

// Read-only remainders widen to read-only projections of the bound,
// and read-only bound elements stay read-only.
def readOnly(c: Object^, d: Object^) =
  def f[X^ <: {c, d}](s: A[Unit]^{X.rd}): A[Unit]^{X.only[C].rd, c.except[C].rd, d.except[C].rd} = s
  def g[X^ <: {c.rd, d}](s: A[Unit]^{X}): A[Unit]^{X.only[C], c.except[C].rd, d.except[C]} = s
  ()

// Capture set members are widened through their bounds as well.
trait Members(c: Object^, d: Object^):
  type X^ <: {c, d}
  def m(s: A[Unit]^{X}): A[Unit]^{X.only[C], c.except[C], d.except[C]} = s
