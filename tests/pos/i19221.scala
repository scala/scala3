// Product component types, and the sequence element type
final class A; final class B; final class C
final class E

// Conforms to both sequence matches and product sequence matches
class Both extends Product1[A]:
  def length: Int          = toSeq.length
  def apply(i: Int): E     = toSeq.apply(i)
  def drop(n: Int): Seq[E] = toSeq.drop(n)
  def toSeq: Seq[E]        = Seq(new E, new E)

  def canEqual(that: Any) = that.isInstanceOf[Both @unchecked]

  val _1: A      = new A
  val _2: B      = new B
  val _3: Seq[C] = Seq(new C)

// Like Both, but with a missing _2
class AlmostBoth extends Product1[A]:
  def length: Int          = toSeq.length
  def apply(i: Int): E     = toSeq.apply(i)
  def drop(n: Int): Seq[E] = toSeq.drop(n)
  def toSeq: Seq[E]        = Seq(new E, new E)

  def canEqual(that: Any) = that.isInstanceOf[AlmostBoth @unchecked]

  val _1: A      = new A
  val _3: Seq[C] = Seq(new C)

// An extractor result holder, to return Both or BothAlmost
class GetBoth       { def isEmpty: Boolean = false; def get = new Both       }
class GetAlmostBoth { def isEmpty: Boolean = false; def get = new AlmostBoth }

// The extractors
object Both          { def unapplySeq(x: Any): Both          = new Both          }
object AlmostBoth    { def unapplySeq(x: Any): AlmostBoth    = new AlmostBoth    }
object GetBoth       { def unapplySeq(x: Any): GetBoth       = new GetBoth       }
object GetAlmostBoth { def unapplySeq(x: Any): GetAlmostBoth = new GetAlmostBoth }

class Test:
  def t1a(x: Any): Seq[E] = x match { case Both(es*)          => es }
  def t1b(x: Any): Seq[E] = x match { case AlmostBoth(es*)    => es }
  def t1c(x: Any): Seq[E] = x match { case GetBoth(es*)       => es }
  def t1d(x: Any): Seq[E] = x match { case GetAlmostBoth(es*) => es }

  def t2a(x: Any): (E, Seq[E]) = x match { case Both(e, es*)          => (e, es) }
  def t2b(x: Any): (E, Seq[E]) = x match { case AlmostBoth(e, es*)    => (e, es) }
  def t2c(x: Any): (E, Seq[E]) = x match { case GetBoth(e, es*)       => (e, es) }
  def t2d(x: Any): (E, Seq[E]) = x match { case GetAlmostBoth(e, es*) => (e, es) }
