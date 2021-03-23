import language.experimental.namedTypeArguments
object TypeSafeLambda {

  trait Category[Arr[_, _]] {
    def id[A]: Arr[A, A]
    def comp[A, B, C](ab: Arr[A, B], bc: Arr[B, C]): Arr[A, C]
  }

  trait Terminal[Term, Arr[_, _]] extends Category[Arr] {
    def terminal[A]: Arr[A, Term]
  }

  trait ProductCategory[Prod[_, _], Arr[_, _]] extends Category[Arr] {
    def first[A, B]: Arr[Prod[A, B], A]
    def second[A, B]: Arr[Prod[A, B], B]
    def pair[A, B, C](ab: Arr[A, B], ac: Arr[A, C]): Arr[A, Prod[B, C]]
  }

  trait Exponential[Exp[_, _], Prod[_, _], Arr[_, _]]
    extends ProductCategory[Prod, Arr] {
    def eval[A, B]: Arr[Prod[Exp[A, B], A], B]
    def curry[A, B, C](a: Arr[Prod[C, A], B]): Arr[C, Exp[A, B]]
  }

  trait CartesianClosed[Term, Exp[_, _], Prod[_, _], Arr[_, _]]
    extends Exponential[Exp, Prod, Arr] with Terminal[Term, Arr]

  sealed trait V[Prod[_, _], Env, R]
  case class Zero[Prod[_, _], Env, R]() extends V[Prod, Prod[Env, R], R]
  case class Succ[Prod[_, _], Env, R, X](
    v: V[Prod, Env, R]
  ) extends V[Prod, Prod[Env, X], R]

  sealed trait Lambda[Terminal, Exp[_, _], Prod[_, _], Env, R]

  case class LUnit[Terminal, Exp[_, _], Prod[_, _], Env]()
    extends Lambda[Terminal, Exp, Prod, Env, Terminal]

  case class Var[Terminal, Exp[_, _], Prod[_, _], Env, R](
    v: V[Prod, Env, R]
  ) extends Lambda[Terminal, Exp, Prod, Env, R]

  case class Lam[Terminal, Exp[_, _], Prod[_, _], Env, R, A](
    lam: Lambda[Terminal, Exp, Prod, Prod[Env, A], R]
  ) extends Lambda[Terminal, Exp, Prod, Env, Exp[A, R]]

  case class App[Terminal, Exp[_, _], Prod[_, _], Env, R, R_](
    f: Lambda[Terminal, Exp, Prod, Env, Exp[R, R_]],
    a: Lambda[Terminal, Exp, Prod, Env, R]
  ) extends Lambda[Terminal, Exp, Prod, Env, R_]

  def interp[Term, Exp[_, _], Prod[_, _], Arr[_, _], S, T](
    c: CartesianClosed[Term, Exp, Prod, Arr],
    exp: Lambda[Term, Exp, Prod, S, T]
  ): Arr[S, T] = exp match {
    case LUnit() => c.terminal
    case v: Var[t, e, var_p, env, r] => v.v match {
      case _: Zero[z_p, z_env, z_r] => c.second[z_env, z_r]
      case s: Succ[s_prod, s_env, s_r, x] =>
        c.comp[A = s_prod[s_env, x], C = s_r](
          c.first,
          interp(c, Var[Term, Exp, Prod, s_env, s_r](s.v))
        )
    }
    case Lam(lam) => c.curry(interp(c, lam))
    case app: App[t, e, p, env, r, r_] =>
      c.comp(
        c.pair(
          interp(c, app.f),
          interp(c, app.a)),
        c.eval[r, r_]
      )
  }

  object example {
    type Term = Unit
    type Prod[A, B] = (A, B)
    type Exp[A, B] = A => B
    type Arr[A, B] = A => B

    val c = new CartesianClosed[Term, Exp, Prod, Arr] {
      def id[A]: A => A = a => a
      def comp[A, B, C](f: A => B, g: B => C): A => C = f andThen g

      def terminal[A]: A => Unit = a => ()

      def first[A, B]: ((A, B)) => A = { case (a, _) => a }
      def second[A, B]: ((A, B)) => B = { case (_, b) => b }
      def pair[A, B, C](f: A => B, g: A => C): A => (B, C) =
        a => (f(a), g(a))

      def eval[A, B]: ((A => B, A)) => B = { case (f, a) => f(a) }
      def curry[A, B, C](f: ((C, A)) => B): C => A => B =
        c => a => f((c, a))
    }

    type Env = Unit Prod Int Prod (Int => String)
    val exp = App[Term, Exp, Prod, Env, Int, String](
      // args to Var are RHS "indices" into Env
      Var(Zero()),
      Var(Succ(Zero()))
    )

    val interped: (Env) => String =
      interp[Term, Exp, Prod, Arr, Env, String] (c, exp)

    interped((((), 1), { (i: Int) => i.toString })) : String // "1"
  }

}
