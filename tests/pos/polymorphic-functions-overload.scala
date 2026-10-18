// Overloading resolution between a method and a polymorphic function value
// of the same name (reduced from scodec's DiscriminatorCodec).
trait Codec[A]
class Prism[R] { def repCodec: Codec[R] = ??? }
class Case[R] { def prism: Prism[R] = ??? }

class DC(cases: List[Case[Any]], framing: [x] => Codec[x] => Codec[x]):
  def framing(framing: [x] => Codec[x] => Codec[x]): DC = new DC(cases, framing)
  def test = cases.map(c => framing(c.prism.repCodec))
  def test2(c: Case[Any]): Codec[Any] = framing(c.prism.repCodec)
  def test3(c: Case[Int]): Codec[Int] = framing(c.prism.repCodec)
  def test4: DC = framing([x] => (c: Codec[x]) => c)

object Test:
  val f: [T] => T => List[T] = [T] => (x: T) => List(x)
  def f(x: String, y: String): Int = x.length + y.length
  val a: List[Int] = f(1)
  val b: Int = f("a", "b")
  val c: List[String] = f[String]("a")

  // polymorphic method returning a function, applied directly, with an overloaded sibling
  def g[T]: T => Option[T] = Some(_)
  def g(x: Int, y: Int): Int = x + y
  val d: Option[String] = g("x")
  val e: Int = g(1, 2)
