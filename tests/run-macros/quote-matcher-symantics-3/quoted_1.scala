import scala.quoted._

object Macros {


  inline def lift[R[_]](sym: Symantics { type Repr[X] = R[X] })(inline a: Int): R[Int] = ${impl('sym, 'a)}


  private def impl[R[_]: Type](sym: Expr[Symantics { type Repr[X] = R[X] }], expr: Expr[Int])(using QuoteContext): Expr[R[Int]] = {

    type Env = Map[Int, Any]

    given ev0 as Env = Map.empty

    def envWith[T](id: Int, ref: Expr[R[T]])(using env: Env): Env =
      env.updated(id, ref)

    object FromEnv {
      def unapply[T](e: Expr[Any])(using env: Env): Option[Expr[R[T]]] =
        e match
          case '{envVar[t](${Const(id)})} =>
            env.get(id).asInstanceOf[Option[Expr[R[T]]]] // We can only add binds that have the same type as the refs
          case _ =>
            None
    }

    def lift[T: Type](e: Expr[T])(using env: Env): Expr[R[T]] = ((e: Expr[Any]) match {
      case Const(e: Int) => '{ $sym.int(${Expr(e)}).asInstanceOf[R[T]] }
      case Const(e: Boolean) => '{ $sym.bool(${Expr(e)}).asInstanceOf[R[T]] }

      case '{ ($x: Int) + ($y: Int) } =>
        '{ $sym.add(${lift(x)}, ${lift(y)}).asInstanceOf[R[T]] }

      case '{ ($x: Int) * ($y: Int) } =>
        '{ $sym.mult(${lift(x)}, ${lift(y)}).asInstanceOf[R[T]] }

      case '{ ($x: Int) <= ($y: Int) } =>
        '{ $sym.leq(${lift(x)}, ${lift(y)}).asInstanceOf[R[T]] }

      case '{ ${f}($arg: a): b } =>
        '{ $sym.app[a, b](${lift(f)}, ${lift(arg)}).asInstanceOf[R[T]] }

      case '{ (if ($cond) $thenp else $elsep): a } =>
        '{ $sym.ifThenElse[a](${lift(cond)}, ${lift(thenp)}, ${lift(elsep)}) }.asInstanceOf[Expr[R[T]]]

      case '{ (x0: Int) => $bodyFn(x0): Any } =>
        val (i, nEnvVar) = freshEnvVar[Int]()
        val body2 = UnsafeExpr.open(bodyFn) { (body1, close) => close(body1)(nEnvVar) }
        '{ $sym.lam((x: R[Int]) => ${given Env = envWith(i, 'x)(using env); lift(body2)}).asInstanceOf[R[T]] }

      case '{ (x0: Boolean) => $bodyFn(x0): Any } =>
        val (i, nEnvVar) = freshEnvVar[Boolean]()
        val body2 = UnsafeExpr.open(bodyFn) { (body1, close) => close(body1)(nEnvVar) }
        '{ $sym.lam((x: R[Boolean]) => ${given Env = envWith(i, 'x)(using env); lift(body2)}).asInstanceOf[R[T]] }

      case '{ (x0: Int => Int) => $bodyFn(x0): Any } =>
        val (i, nEnvVar) = freshEnvVar[Int => Int]()
        val body2 = UnsafeExpr.open(bodyFn) { (body1, close) => close(body1)(nEnvVar) }
        '{ $sym.lam((x: R[Int => Int]) => ${given Env = envWith(i, 'x)(using env); lift(body2)}).asInstanceOf[R[T]] }

      case '{ Symantics.fix[a, b]($f) } =>
        '{ $sym.fix[a, b]((x: R[a => b]) => $sym.app(${lift(f)}, x)).asInstanceOf[R[T]] }

      case FromEnv(expr) => expr.asInstanceOf[Expr[R[T]]]

      case _ =>
        report.error("Expected explicit value but got: " + e.show, e)
        '{ ??? }

    })

    lift(expr)
  }

}

object UnsafeExpr {
  def open[T1, R, X](f: Expr[T1 => R])(content: (Expr[R], [t] => Expr[t] => Expr[T1] => Expr[t]) => X)(using qctx: QuoteContext): X = {
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v: Expr[T1]) => bodyFn[t](e.unseal, params, List(v.unseal)).seal.asInstanceOf[Expr[t]])
  }
  private def paramsAndBody[R](using qctx: QuoteContext)(f: Expr[Any]): (List[qctx.reflect.ValDef], Expr[R]) = {
    import qctx.reflect._
    val Block(List(DefDef("$anonfun", Nil, List(params), _, Some(body))), Closure(Ident("$anonfun"), None)) = f.unseal.etaExpand
    (params, body.seal.asInstanceOf[Expr[R]])
  }

  private def bodyFn[t](using qctx: QuoteContext)(e: qctx.reflect.Term, params: List[qctx.reflect.ValDef], args: List[qctx.reflect.Term]): qctx.reflect.Term = {
    import qctx.reflect._
    val map = params.map(_.symbol).zip(args).toMap
    new TreeMap {
      override def transformTerm(tree: Term)(using ctx: Context): Term =
        super.transformTerm(tree) match
          case tree: Ident => map.getOrElse(tree.symbol, tree)
          case tree => tree
    }.transformTerm(e)
  }
}

def freshEnvVar[T: Type]()(using QuoteContext): (Int, Expr[T]) = {
  v += 1
  (v, '{envVar[T](${Expr(v)})})
}
var v = 0
def envVar[T](i: Int): T = ???

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
