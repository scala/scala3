import language.experimental.namedTypeArguments
object i3666 {
  sealed trait Exp[T]
  case class Num(n: Int) extends Exp[Int]
  case class Plus(e1: Exp[Int], e2: Exp[Int]) extends Exp[Int]
  case class Var[T](name: String) extends Exp[T]
  case class Lambda[T, U](x: Var[T], e: Exp[U]) extends Exp[T => U]
  case class App[T, U](f: Exp[T => U], e: Exp[T]) extends Exp[U]

  abstract class Env { outer =>
    def apply[T](x: Var[T]): T

    def + [T](xe: (Var[T], T)) = new Env {
      def apply[T](x: Var[T]): T =
        if (x == xe._1) xe._2.asInstanceOf[T]
        else outer(x)
    }
  }

  object Env {
    val empty = new Env {
      def apply[T](x: Var[T]): T = ???
    }
  }

  object Test {

    val exp = App(Lambda(Var[Int]("x"), Plus(Var[Int]("x"), Num(1))), Var[Int]("2"))

    def eval[T](e: Exp[T])(env: Env): T = e match {
      case Num(n) => n
      case Plus(e1, e2) => eval(e1)(env) + eval(e2)(env)
      case v: Var[_] => env(v)
      case Lambda(x: Var[s], e) => ((y: s) => eval(e)(env + (x -> y)))
      case App(f, e) => eval(f)(env)(eval(e)(env))
    }

    eval(exp)(Env.empty)
  }
}
// A HOAS well-typed interpreter
object i3666Hoas {
  sealed trait Exp[T]
  case class IntLit(n: Int) extends Exp[Int]
  case class BooleanLit(b: Boolean) extends Exp[Boolean]

  case class GenLit[T](t: T) extends Exp[T]
  case class Plus(e1: Exp[Int], e2: Exp[Int]) extends Exp[Int]
  case class Fun[S, T](f: Exp[S] => Exp[T]) extends Exp[S => T]
  case class App[T, U](f: Exp[T => U], e: Exp[T]) extends Exp[U]


  def eval[T](e: Exp[T]): T = e match {
    case IntLit(n) => n
    case BooleanLit(b) => b
    case GenLit(t) => t
    case Plus(e1, e2) => eval(e1) + eval(e2)
    case f: Fun[s, t]  =>
      (v: s) => eval(f.f(GenLit(v)))
    case App(f, e) => eval(f)(eval(e))
  }

  val exp = App(Fun[S = Int](x => Plus(x, IntLit(1))), IntLit(2))

  eval(exp)
}
