import scala.quoted.*
import scala.quoted.staging.*

trait Ring[T]:
  val zero: T
  val one: T
  val add: (x: T, y: T) => T
  val sub: (x: T, y: T) => T
  val mul: (x: T, y: T) => T
end Ring

class RingInt extends Ring[Int]:
  val zero = 0
  val one  = 1
  val add  = (x, y) => x + y
  val sub  = (x, y) => x - y
  val mul  = (x, y) => x * y


class RingIntExpr(using Quotes) extends Ring[Expr[Int]]:
  val zero = '{0}
  val one  = '{1}
  val add  = (x, y) => '{$x + $y}
  val sub  = (x, y) => '{$x - $y}
  val mul  = (x, y) => '{$x * $y}

class RingComplex[U](u: Ring[U]) extends Ring[Complex[U]]:
  val zero = Complex(u.zero, u.zero)
  val one  = Complex(u.one, u.zero)
  val add  = (x, y) => Complex(u.add(x.re, y.re), u.add(x.im, y.im))
  val sub  = (x, y) => Complex(u.sub(x.re, y.re), u.sub(x.im, y.im))
  val mul  = (x, y) => Complex(u.sub(u.mul(x.re, y.re), u.mul(x.im, y.im)), u.add(u.mul(x.re, y.im), u.mul(x.im, y.re)))

sealed trait PV[T]:
  def expr(using ToExpr[T], Quotes): Expr[T]

case class Sta[T](x: T) extends PV[T]:
  def expr(using ToExpr[T], Quotes): Expr[T] = Expr(x)

case class Dyn[T](x: Expr[T]) extends PV[T]:
  def expr(using ToExpr[T], Quotes): Expr[T] = x

class RingPV[U: ToExpr](u: Ring[U], eu: Ring[Expr[U]])(using Quotes) extends Ring[PV[U]]:
  val zero: PV[U] = Sta(u.zero)
  val one: PV[U] = Sta(u.one)
  val add = (x: PV[U], y: PV[U]) => (x, y) match
    case (Sta(u.zero), x) => x
    case (x, Sta(u.zero)) => x
    case (Sta(x), Sta(y)) => Sta(u.add(x, y))
    case (x, y) => Dyn(eu.add(x.expr, y.expr))
  val sub = (x: PV[U], y: PV[U]) => (x, y) match
    case (x, Sta(u.zero)) => x
    case (Sta(x), Sta(y)) => Sta(u.sub(x, y))
    case (x, y) => Dyn(eu.sub(x.expr, y.expr))
  val mul = (x: PV[U], y: PV[U]) => (x, y) match
    case (Sta(u.zero), _) => Sta(u.zero)
    case (_, Sta(u.zero)) => Sta(u.zero)
    case (Sta(u.one), x) => x
    case (x, Sta(u.one)) => x
    case (Sta(x), Sta(y)) => Sta(u.mul(x, y))
    case (x, y) => Dyn(eu.mul(x.expr, y.expr))

case class Complex[T](re: T, im: T)

object Complex:
  implicit def isToExpr[T: Type: ToExpr]: ToExpr[Complex[T]] = new ToExpr[Complex[T]]:
    def apply(comp: Complex[T])(using Quotes) = '{Complex(${Expr(comp.re)}, ${Expr(comp.im)})}

case class Vec[Idx, T](size: Idx, get: Idx => T):
  def map[U](f: T => U): Vec[Idx, U] = Vec(size, i => f(get(i)))
  def zipWith[U, V](other: Vec[Idx, U], f: (T, U) => V): Vec[Idx, V] = Vec(size, i => f(get(i), other.get(i)))

object Vec:
  def from[T](elems: T*): Vec[Int, T] = new Vec(elems.size, i => elems(i))

trait VecOps[Idx, T]:
  val reduce: ((T, T) => T, T, Vec[Idx, T]) => T

class StaticVecOps[T] extends VecOps[Int, T]:
  val reduce: ((T, T) => T, T, Vec[Int, T]) => T = (plus, zero, vec) =>
    var sum = zero
    for (i <- 0 until vec.size)
      sum = plus(sum, vec.get(i))
    sum

class ExprVecOps[T: Type](using Quotes) extends VecOps[Expr[Int], Expr[T]]:
  val reduce: ((Expr[T], Expr[T]) => Expr[T], Expr[T], Vec[Expr[Int], Expr[T]]) => Expr[T] = (plus, zero, vec) => '{
    var sum = $zero
    var i = 0
    while i < ${vec.size} do
      sum = ${ plus('sum, vec.get('i)) }
      i += 1
    sum
  }

class Blas1[Idx, T](r: Ring[T], ops: VecOps[Idx, T]):
  def dot(v1: Vec[Idx, T], v2: Vec[Idx, T]): T = ops.reduce(r.add, r.zero, v1.zipWith(v2, r.mul))

object Test:

  given Compiler = Compiler.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit =
    val arr1 = Array(0, 1, 2, 4, 8)
    val arr2 = Array(1, 0, 1, 0, 1)
    val cmpxArr1 = Array(Complex(1, 0), Complex(2, 3), Complex(0, 2), Complex(3, 1))
    val cmpxArr2 = Array(Complex(0, 1), Complex(0, 0), Complex(0, 1), Complex(2, 0))

    val vec1 = new Vec(arr1.size, i => arr1(i))
    val vec2 = new Vec(arr2.size, i => arr2(i))
    val cmpxVec1 = new Vec(cmpxArr1.size, i => cmpxArr1(i))
    val cmpxVec2 = new Vec(cmpxArr2.size, i => cmpxArr2(i))

    val blasInt = new Blas1(new RingInt, new StaticVecOps)
    val res1 = blasInt.dot(vec1, vec2)
    println(res1)
    println()

    val blasComplexInt = new Blas1(new RingComplex(new RingInt), new StaticVecOps)
    val res2 = blasComplexInt.dot(
      cmpxVec1,
      cmpxVec2
    )
    println(res2)
    println()

    def blasStaticIntExpr(using Quotes) = new Blas1(new RingIntExpr, new StaticVecOps)
    def resCode1(using Quotes) = blasStaticIntExpr.dot(
      vec1.map(Expr(_)),
      vec2.map(Expr(_))
    )
    println(withQuotes(resCode1.show))
    println(run(resCode1))
    println()

    def blasExprIntExpr(using Quotes) = new Blas1(new RingIntExpr, new ExprVecOps)
    def resCode2(using Quotes): Expr[(Array[Int], Array[Int]) => Int] = '{
      (arr1, arr2) =>
        if (arr1.length != arr2.length) throw new Exception("...")
        ${
          blasExprIntExpr.dot(
            new Vec('{arr1.size}, i => '{arr1($i)}),
            new Vec('{arr2.size}, i => '{arr2($i)})
          )
        }
    }
    println(withQuotes(resCode2.show))
    println(run(resCode2).apply(arr1, arr2))
    println()

    def blasStaticIntPVExpr(using Quotes) = new Blas1(new RingPV[Int](new RingInt, new RingIntExpr), new StaticVecOps)
    def resCode3(using Quotes) = blasStaticIntPVExpr.dot(
      vec1.map(i => Dyn(Expr(i))),
      vec2.map(i => Sta(i))
    ).expr
    println(withQuotes(resCode3.show))
    println(run(resCode3))
    println()

    def blasExprIntPVExpr(using Quotes) = new Blas1(new RingPV[Int](new RingInt, new RingIntExpr), new StaticVecOps)
    def resCode4(using Quotes): Expr[Array[Int] => Int] = '{
      arr =>
        if (arr.length != ${Expr(vec2.size)}) throw new Exception("...")
        ${
          blasExprIntPVExpr.dot(
            new Vec(vec2.size, i => Dyn('{arr(${Expr(i)})})),
            vec2.map(i => Sta(i))
          ).expr
        }

    }
    println(withQuotes(resCode4.show))
    println(run(resCode4).apply(arr1))
    println()

    import Complex.isToExpr
    def blasExprComplexPVInt(using Quotes) = new Blas1[Int, Complex[PV[Int]]](new RingComplex(new RingPV[Int](new RingInt, new RingIntExpr)), new StaticVecOps)
    def resCode5(using Quotes): Expr[Array[Complex[Int]] => Complex[Int]] = '{
      arr =>
        if (arr.length != ${Expr(cmpxVec2.size)}) throw new Exception("...")
        ${
          val cpx = blasExprComplexPVInt.dot(
            new Vec(cmpxVec2.size, i => Complex(Dyn('{arr(${Expr(i)}).re}), Dyn('{arr(${Expr(i)}).im}))),
            new Vec(cmpxVec2.size, i => Complex(Sta(cmpxVec2.get(i).re), Sta(cmpxVec2.get(i).im)))
          )
          '{Complex(${cpx.re.expr}, ${cpx.im.expr})}
        }
    }
    println(withQuotes(resCode5.show))
    println(run(resCode5).apply(cmpxArr1))
    println()

    def RingPVInt(using Quotes) = new RingPV[Int](new RingInt, new RingIntExpr)
    // Staged loop of dot product on vectors of Int or Expr[Int]
    def dotIntOptExpr(using Quotes) = new Blas1(RingPVInt, new StaticVecOps).dot
    // will generate the code '{ ((arr: scala.Array[scala.Int]) => arr.apply(1).+(arr.apply(3))) }
    def staticVec(using Quotes) = Vec[Int, PV[Int]](5, i => Sta((i % 2)))
    def code(using Quotes) = '{(arr: Array[Int]) => ${dotIntOptExpr(Vec(5, i => Dyn('{arr(${Expr(i)})})), staticVec).expr} }
    println(withQuotes(code.show))
    println()

