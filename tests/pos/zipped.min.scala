// Justifies the need for TypeApply in tryInsertImplicitOnQualifier
// after failing ys.map[?B, C] using Zipped2's map
// we want to try ys.map[?B] using Coll's map, after toColl
final class Coll[+A]:
  def map[B](f: A => B): Coll[B] = new Coll[B]
  def lazyZip[B](that: Coll[B]): Zipped2[A, B] = new Zipped2[A, B](this, that)
final class Zipped2[+X, +Y](xs: Coll[X], ys: Coll[Y]):
  def map[B, C](f: (X, Y) => B): Coll[C] = new Coll[C]
object Zipped2:
  import scala.language.implicitConversions
  implicit def toColl[X, Y](zipped2: Zipped2[X, Y]): Coll[(X, Y)] = new Coll[(X, Y)]
class Test:
  def test(xs: Coll[Int]): Unit =
    val ys = xs.lazyZip(xs)
    ys.map((x: (Int, Int)) => x._1 + x._2)
