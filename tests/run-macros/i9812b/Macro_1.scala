import quoted._
import SomeEnum._

trait Liftable[T] {
  /** Lift a value into an expression containing the construction of that value */
  def toExpr(x: T): Quotes ?=> Expr[T]
}

object Lift:
  def apply[T: Liftable](t: T)(using q: Quotes, ev: Liftable[T]): Expr[T] = ev.toExpr(t)

sealed abstract class SomeEnum
object SomeEnum:
  final val Foo = new SomeEnum {}
  final case class Bar[S <: SomeEnum](s: S) extends SomeEnum
  object Bar:
    def apply[S <: SomeEnum](s: S): SomeEnum = new Bar(s)

given LiftFoo as Liftable[Foo.type]:
  def toExpr(x: Foo.type): Quotes ?=> Expr[Foo.type] = '{Foo}

given LiftBar[S <: SomeEnum: Type: Liftable] as Liftable[Bar[S]]:
  def toExpr(x: Bar[S]): Quotes ?=> Expr[Bar[S]] = '{new Bar(${Lift(x.s)})}

sealed abstract class Lst[+T]
final case class CONS[+T](head: T, tail: Lst[T]) extends Lst[T]
case object NIL extends Lst[Nothing]

given IntLiftable[T <: Int] as Liftable[T]:
  def toExpr(x: T): Quotes ?=> Expr[T] = (using qctx) => {
    import qctx.reflect._
    Literal(Constant.Int(x)).asExpr.asInstanceOf[Expr[T]]
  }

given LiftLst[T: Type: Liftable](using ev1: => Liftable[CONS[T]], ev2: => Liftable[NIL.type]) as Liftable[Lst[T]]:
  def toExpr(xs: Lst[T]): Quotes ?=> Expr[Lst[T]] = xs match
    case NIL               => ev2.toExpr(NIL)
    case cons @ CONS(_, _) => ev1.toExpr(cons)

given LiftCONS[T: Type: Liftable](using Liftable[Lst[T]]) as Liftable[CONS[T]]:
  def toExpr(x: CONS[T]): Quotes ?=> Expr[CONS[T]] = '{CONS(${Lift(x.head)}, ${Lift(x.tail)})}

given LiftNIL as Liftable[NIL.type]:
  def toExpr(x: NIL.type): Quotes ?=> Expr[NIL.type] = '{NIL}

def mkLst[T](ts: T*) = ts.foldRight(NIL: Lst[T])(CONS(_,_))

def quote123: Quotes ?=> Expr[Lst[Int]] = Lift(mkLst(1,2,3))

inline def get123: Lst[Int] = ${ quote123 }

