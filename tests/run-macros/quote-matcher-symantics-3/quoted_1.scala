import scala.quoted.*

object Macros {


  inline def lift[R[_]](sym: Symantics { type Repr[X] = R[X] })(inline a: Int): R[Int] = ${impl('sym, 'a)}


  private def impl[R[_]: Type](sym: Expr[Symantics { type Repr[X] = R[X] }], expr: Expr[Int])(using Quotes): Expr[R[Int]] = {

    type Env = Map[Int, Any]

    given ev0: Env = Map.empty

    def envWith[T](id: Int, ref: Expr[R[T]])(using env: Env): Env =
      env.updated(id, ref)

    object FromEnv {
      def unapply[T](e: Expr[Any])(using env: Env): Option[Expr[R[T]]] =
        e match
          case '{envVar[t](${Expr(id)})} =>
            env.get(id).asInstanceOf[Option[Expr[R[T]]]] // We can only add binds that have the same type as the refs
          case _ =>
            None
    }

    def lift[T: Type](e: Expr[T])(using env: Env)(using Quotes): Expr[R[T]] = ((e: Expr[Any]) match {
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
        import quotes.reflect.report
        report.error("Expected explicit value but got: " + e.show, e)
        '{ ??? }

    })

    lift(expr)
  }

}

object UnsafeExpr {
  def open[T1, R, X](f: Expr[T1 => R])(content: (Expr[R], [t] => Expr[t] => Expr[T1] => Expr[t]) => X)(using Quotes): X = {
    import quotes.reflect.*
    val (params, bodyExpr) = paramsAndBody[R](f)
    content(bodyExpr, [t] => (e: Expr[t]) => (v: Expr[T1]) => bodyFn[t](e.asTerm, params, List(v.asTerm)).asExpr.asInstanceOf[Expr[t]])
  }
  private def paramsAndBody[R](using Quotes)(f: Expr[Any]): (List[quotes.reflect.ValDef], Expr[R]) = {
    import quotes.reflect.*
    val Block(List(DefDef("$anonfun", List(TermParamClause(params)), _, Some(body))), Closure(Ident("$anonfun"), None)) = f.asTerm.etaExpand(Symbol.spliceOwner)
    (params, body.asExpr.asInstanceOf[Expr[R]])
  }

  private def bodyFn[t](using Quotes)(e: quotes.reflect.Term, params: List[quotes.reflect.ValDef], args: List[quotes.reflect.Term]): quotes.reflect.Term = {
    import quotes.reflect.*
    val map = params.map(_.symbol).zip(args).toMap
    new TreeMap {
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        super.transformTerm(tree)(owner) match
          case tree: Ident => map.getOrElse(tree.symbol, tree)
          case tree => tree
    }.transformTerm(e)(Symbol.spliceOwner)
  }
}

def freshEnvVar[T: Type]()(using Quotes): (Int, Expr[T]) = {
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

object Const {
  def unapply[T](expr: Expr[T])(using Quotes): Option[T] = {
    import quotes.reflect.*
    def rec(tree: Term): Option[T] = tree match {
      case Literal(c) =>
        c match
          case NullConstant() | UnitConstant() | ClassOfConstant(_) => None
          case _ => Some(c.value.asInstanceOf[T])
      case Block(Nil, e) => rec(e)
      case Typed(e, _) => rec(e)
      case Inlined(_, Nil, e) => rec(e)
      case _  => None
    }
    rec(expr.asTerm)
  }
}