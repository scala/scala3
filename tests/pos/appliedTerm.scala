object SimpleEqs {
  val x = 1
  val y: {x} = x

  type YPlusOne = {y + 1}

  implicitly[{x + 1} =:= {y + 1}]
  implicitly[{x + 1} =:= YPlusOne]
}


object AvoidLocalRefs {
  type Id[T] = T

  val x = 1
  def y = { val a: {x} = x; val t: Id[{a + 1}] = a + 1; t }
  def z: {x + 1} = { val a: {x} = x; val t: Id[{a + 1}] = a + 1; t }

  val _ = { val a = 0; a + 1 }
  val _ = { val a = 0; 1 + a }
}


object Bounds {
  @annotation.implicitNotFound(msg = "Cannot prove that ${B} holds.")
  sealed abstract class P[B <: Boolean](val b: B)
  private[this] val prop_singleton = new P[true](true) {}
  object P {
    def assume(b: Boolean): P[b.type] = prop_singleton.asInstanceOf[P[b.type]]
  }

  def if_(cond: Boolean): (P[cond.type] ?=> Unit) => Unit =
    thn => if (cond) thn(using P.assume(cond))


  // Bounds-checked

  def index(k: Int)(implicit ev: P[{k >= 0}]): Int = k

  def run(i: Int) =
    if_(i >= 0) {
      index(i)
    }


  // Boxed value with a predicate

  class PredBox[T, B <: Boolean](val v: T)(val p: P[B])
  object PredBox {
    def apply[T, B <: Boolean](v: T)(implicit ev: P[B]) = new PredBox[T, B](v)(ev)
  }

  def run2(i: Int) =
    if_(i != 0) {
      PredBox[Int, {i != 0}](i)
    }
}


object ArithmeticIdentities {
  type SInt = Int & Singleton

  class DecomposeHelper[V <: SInt](val v: V) {
    import DecomposeHelper._
    def asSumOf[X <: SInt, Y <: SInt](x: X, y: Y)(implicit ev: {v} =:= {x + y}): SumOf[{x}, {y}] = SumOf(x, y)(ev(v))
  }

  object DecomposeHelper {
    /* Axioms */
    sealed trait Decomposition[V <: SInt]
    case class SumOf[X <: SInt, Y <: SInt](x: X, y: Y)(val v: {x + y}) extends Decomposition[{v}] {
      def commuted: SumOf[Y, X] = SumOf(y, x)(v.asInstanceOf[{y + x}])
    }
  }

  implicit def toDecomposeHelper[V <: Int](v: V): DecomposeHelper[v.type] = new DecomposeHelper(v)


  // Let's "show" that x + 1 == 1 + x

  val x = 123
  (x + 1).asSumOf(x, 1).v: {x + 1}
  (x + 1).asSumOf(x, 1).commuted.v: {1 + x}
}


object Matrices {
  type SInt = Int & Singleton

  case class Matrix[M <: SInt, N <: SInt](m: M, n: N) {
    val size: {m * n} = m * n
  }

  val a: 123 = 123
  val b: Int = ???
  val mat = Matrix(a, b)
  val _ = mat.m: 123
  val _ = mat.n: b.type
  val _ = mat.size: {123 * b}
}
