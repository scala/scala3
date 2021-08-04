/* Boehm-Berarducci encoding of lists in polymorphic typed lambda calculus */
type Op[T, C] = T => C => C
type List[T] = [C] => Op[T, C] => C => C

def nil[T]: List[T] =
  [C] => (op: Op[T, C]) => (s: C) => s

def cons[T](hd: T, tl: List[T]): List[T] =
  [C] => (op: Op[T, C]) => (s: C) => op(hd)(tl(op)(s))

/** A trait that can be instantiated with a list decomposition `ListView` */
trait ListOps:
  type ListView[T]
  def decompose[T](xs: List[T]): ListView[T]
  def fst[T](v: ListView[T]): T
  def snd[T](v: ListView[T]): List[T]
  def isPair[T](v: ListView[T]): Boolean

  // Some operations and tests that operate with the decomposition
  def head[T](xs: List[T]): T = fst(decompose[T](xs))
  def tail[T](xs: List[T]): List[T] = snd(decompose[T](xs))
  def isEmpty[T](xs: List[T]): Boolean = !isPair(decompose[T](xs))

  def toScalaList[T](xs: List[T]): scala.List[T] =
    xs[scala.List[T]](h => t => h :: t)(Nil)

  def print[T](xs: List[T]): Unit =
    println(toScalaList[T](xs))

  def test() =
    val xs: List[Int] = cons(1, cons(2, nil))
    print[Int](xs)
    print[Int](tail(xs))
    println(head[Int](xs))
    println(isEmpty[Int](xs))
end ListOps

// A ListView based on regular Scala classes - options of pairs
object ListOps1 extends ListOps:
  type ListView[T] = Option[(T, List[T])]

  def push[T](h: T, v: ListView[T]): ListView[T] = v match
    case Some((h2, xs2)) => Some(h, cons[T](h2, xs2))
    case None => Some(h, nil[T])

  def decompose[T](xs: List[T]): ListView[T] =
    xs[Option[(T, List[T])]](h => c => push(h, c))(None)

  def fst[T](v: ListView[T]): T = v.get._1
  def snd[T](v: ListView[T]): List[T] = v.get._2
  def isPair[T](v: ListView[T]): Boolean = v.isDefined

// A ListView based on (non-recursive) Church encodings in polymorphic lambda calculus
object ListOps2 extends ListOps:
  type ListView[T] = [K] => (T => List[T] => K) => (() => K) => K

  def consView[T](x: T, xs: List[T]): ListView[T] =
    [K] => (caseCons: T => List[T] => K) => (caseNil: () => K) => caseCons(x)(xs)

  def nilView[T]: ListView[T] =
    [K] => (caseCons: T => List[T] => K) => (caseNil: () => K) => caseNil()

  def push[T](h: T)(c: ListView[T]): ListView[T] =
    c[ListView[T]](h2 => xs2 => consView(h, cons[T](h2, xs2)))(() => consView(h, nil[T]))

  def decompose[T](xs: List[T]): ListView[T] =
    xs[ListView[T]](push)(nilView)

  def fst[T](v: ListView[T]): T = v(hd => tl => hd)(() => ???)
  def snd[T](v: ListView[T]): List[T] = v(hd => tl => tl)(() => ???)
  def isPair[T](v: ListView[T]): Boolean = v(hd => tl => true)(() => false)

@main def Test() =
  ListOps1.test()
  ListOps2.test()
