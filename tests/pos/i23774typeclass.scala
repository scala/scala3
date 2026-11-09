import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

enum Expr[+T]:
  case UpcastToIterable[T, C <: Iterable[T]](v: Expr[C]) extends Expr[Iterable[T]]
  case Seq[T](elements: Expr[T]*) extends Expr[scala.Seq[T]]
  case Const(value: T)

trait Fold[E]:
  def apply[Acc](acc: Acc, expr: E, f: [t] => (Acc, Expr[t]) => Acc): Acc

object Fold:
  private inline def summonAll[Elems <: Tuple]: List[Fold[?]] =
    inline erasedValue[Elems] match
      case _: (h *: tail) => summonInline[Fold[h]] :: summonAll[tail]
      case _: EmptyTuple => Nil

  final class Leaf[E] extends Fold[E]:
    def apply[Acc](acc: Acc, expr: E, f: [t] => (Acc, Expr[t]) => Acc): Acc = acc

  given [T: Fold as fold] => Fold[Seq[T]] = new Fold[Seq[T]] {
    def apply[Acc](acc: Acc, expr: Seq[T], f: [t] => (Acc, Expr[t]) => Acc): Acc =
      expr.foldLeft(acc)((a, e) => fold(a, e, f))
  }

  given Fold[EmptyTuple] = new Fold[EmptyTuple]:
    def apply[Acc](acc: Acc, expr: EmptyTuple, f: [t] => (Acc, Expr[t]) => Acc): Acc = acc

  given [H: Fold as h, T <: Tuple: Fold as t] => Fold[H *: T] =
    new Fold[H *: T]:
      def apply[Acc](acc: Acc, expr: H *: T, f: [t] => (Acc, Expr[t]) => Acc): Acc =
        val acc1 = h(acc, expr.head, f)
        t(acc1, expr.tail, f)

  private def product[E](m: Mirror.ProductOf[E], tupleFold: Fold[m.MirroredElemTypes]): Fold[E] =
    new Fold[E]:
      def apply[Acc](acc: Acc, expr: E, f: [t] => (Acc, Expr[t]) => Acc): Acc =
        // The mirror makes this safe according to https://github.com/scala/scala3/issues/22382#issuecomment-2613187822
        tupleFold(acc, Tuple.fromProduct(expr.asInstanceOf[Product]).asInstanceOf[m.MirroredElemTypes], f)

  private def sum[E](m: Mirror.SumOf[E], cases0: () => List[Fold[?]]): Fold[E] =
    new Fold[E]:
      lazy val cases = cases0()
      def apply[Acc](acc: Acc, expr: E, f: [t] => (Acc, Expr[t]) => Acc): Acc =
        val ord = m.ordinal(expr)
        val caseFold = cases.apply(ord)
        caseFold.apply(acc, expr.asInstanceOf, f)

  inline given derived[E](using m: Mirror.Of[E]): Fold[E] =
    inline m match
      case m: Mirror.SumOf[E] => sum(m, () => summonAll[m.MirroredElemTypes])
      case m: Mirror.ProductOf[E] => product[E](m, summonInline[Fold[m.MirroredElemTypes]])

  given [T] => Fold[Expr.Const[T]] = Leaf()
  given Fold[Expr.UpcastToIterable[Any, Iterable[Any]]] = derived
  given [T] => Fold[Expr[T]] = new Fold[Expr[T]]:
    val default = derived[Expr[T]]
    def apply[Acc](acc: Acc, expr: Expr[T], f: [t] => (Acc, Expr[t]) => Acc): Acc =
      default(f(acc, expr), expr, f)

@main def test(): Unit =
  def count[T](expr: Expr[T], f: [t] => Expr[t] => Boolean)(using fold: Fold[Expr[T]]): Int =
    fold(0, expr, [t] => (acc, e) => if f(e) then acc + 1 else acc)

  val ast: Expr[Iterable[Int]] = Expr.UpcastToIterable(Expr.Seq(Expr.Const(1), Expr.Const(2), Expr.Const(3)))
  val constCount = count(
    ast,
    [t] =>
      _ match {
        case Expr.Const(_) => true
        case _ => false
      }
  )
  println(s"Number of Const nodes: $constCount")
