import scala.quoted._
import scala.quoted.matching._

object Macros {


  inline def lift[R[_]](sym: Symantics { type Repr = R })(a: => Int): R[Int] = ${impl('sym, 'a)}


  private def impl[R[_]: Type](sym: Expr[Symantics { type Repr[X] = R[X] }], expr: Expr[Int]) given QuoteContext: Expr[R[Int]] = {

    type Env = Map[Any, Any]

    delegate ev0 for Env = Map.empty

    def envWith[T](id: Bind[T], ref: Expr[R[T]]) given (env: Env): Env =
      env.updated(id, ref)

    object FromEnv {
      def unapply[T](id: Bind[T]) given Env: Option[Expr[R[T]]] =
        the[Env].get(id).asInstanceOf[Option[Expr[R[T]]]] // We can only add binds that have the same type as the refs
    }

    def lift[T: Type](e: Expr[T]) given (env: Env): Expr[R[T]] = ((e: Expr[Any]) match {
      case Const(e: Int) => '{ $sym.int(${e.toExpr}).asInstanceOf[R[T]] }
      case Const(e: Boolean) => '{ $sym.bool(${e.toExpr}).asInstanceOf[R[T]] }

      case '{ ($x: Int) + ($y: Int) } =>
        '{ $sym.add(${lift(x)}, ${lift(y)}).asInstanceOf[R[T]] }

      case '{ ($x: Int) * ($y: Int) } =>
        '{ $sym.mult(${lift(x)}, ${lift(y)}).asInstanceOf[R[T]] }

      case '{ ($x: Int) <= ($y: Int) } =>
        '{ $sym.leq(${lift(x)}, ${lift(y)}).asInstanceOf[R[T]] }

      case '{ ($f: $t => $u)($arg) } =>
        '{ $sym.app[$t, $u](${lift(f)}, ${lift(arg)}).asInstanceOf[R[T]] }

      case '{ (if ($cond) $thenp else $elsep): $t } =>
        '{ $sym.ifThenElse[$t](${lift(cond)}, ${lift(thenp)}, ${lift(elsep)}) }.asInstanceOf[Expr[R[T]]]

      case '{ ($x0: Int) => $body: Any } =>
        '{ $sym.lam((x: R[Int]) => ${delegate for Env = envWith(x0, 'x) given env; lift(body)}).asInstanceOf[R[T]] }
      case '{ ($x0: Boolean) => $body: Any } =>
        '{ $sym.lam((x: R[Boolean]) => ${delegate for Env = envWith(x0, 'x) given env; lift(body)}).asInstanceOf[R[T]] }
      case '{ ($x0: Int => Int) => $body: Any } =>
        '{ $sym.lam((x: R[Int => Int]) => ${delegate for Env = envWith(x0, 'x) given env; lift(body)}).asInstanceOf[R[T]] }

      case '{ Symantics.fix[$t, $u]($f) } =>
        '{ $sym.fix[$t, $u]((x: R[$t => $u]) => $sym.app(${lift(f)}, x)).asInstanceOf[R[T]] }

      case Bind(FromEnv(expr)) => expr.asInstanceOf[Expr[R[T]]]

      case _ =>
        the[QuoteContext].error("Expected explicit value but got: " + e.show, e)
        '{ ??? }

    })

    lift(expr)
  }

}

trait Symantics {
  type Repr[X]
  def int(x: Int): Repr[Int]
  def bool(x: Boolean): Repr[Boolean]
  def lam[A, B](f: Repr[A] => Repr[B]): Repr[A => B]
  def app[A, B](f: Repr[A => B], arg: Repr[A]): Repr[B]
  def fix[A, B]: (Repr[A => B] => Repr[A => B]) => Repr[A => B]
  def add(x: Repr[Int], y: Repr[Int]): Repr[Int]
  def mult(x: Repr[Int], y: Repr[Int]): Repr[Int]
  def leq(x: Repr[Int], y: Repr[Int]): Repr[Boolean]
  def ifThenElse[A](cond: Repr[Boolean], thenp: => Repr[A], elsep: => Repr[A]): Repr[A]
}

object Symantics {
  def fix[A, B](f: (A => B) => (A => B)): A => B = throw new Exception("Must be used inside of `lift`")
}
