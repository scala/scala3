// scalajs: --skip

import Term.*
import Pattern.*
import Type.*

type Gamma = Set[EnvVar]
type Delta = Set[EnvVar]

case class EnvVar(name: Name, level: 0 | 1, tpe: Type)

class Name private (base: String, i: Int) {
  assert(base.matches("[a-zA-Z]+"))
  override def toString(): String = s"`$base$i`"
  def fresh(): Name = Name.fresh(base)
}


object Name {
  private val map: collection.mutable.Map[String, Int] = collection.mutable.Map.empty
  def fresh(base: String): Name = {
    val i = map.getOrElse(base, 0)
    map(base) = i + 1
    new Name(base, i)
  }
}


enum Term:
  case Nat(n: Int)
  case Ref(name: Name)
  case Lambda(name: Name, tpe: Type, body: Term)
  case App(fun: Term, arg: Term)
  case Box(body: Term)
  case Splice(body: Term)
  case Lift(body: Term)
  case Match(scrutinee: Term, pat: Pattern, thenp: Term, elsep: Term)
  case Fix(term: Term)


enum Pattern:
  case PNat(n: Int)
  case PRef(name: Name)
  case PApp(fun: Pattern, arg: Pattern)
  case PLit(name: Name)
  case PBind(name: Name)
  case PFun(name: Name)


enum Type:
  case NatType
  case LambdaType(arg: Type, res: Type)
  case BoxType(inner: Type)
  case TypeVar(name: Name)


def isPlainTerm(term: Term): Boolean =
  term match
    case Nat(_) => true
    case Ref(_) => true
    case Lambda(_, _, body) => isPlainTerm(body)
    case App(fun, arg) => isPlainTerm(fun) && isPlainTerm(arg)
    case _ => false


def isValue(term: Term): Boolean =
  term match
    case Nat(_) => true
    case Lambda(_, _, body) => true
    case Box(body) => isPlainTerm(body)
    case _ => false


def typeChecks(g: Gamma)(level: 0 | 1)(term: Term): Option[Type] =
  val res: Option[Type] = term match
    case Nat(n) if n >= 0 => // T-NAt
      Some(NatType)
    case Ref(name) => // T-Var
      g.collectFirst { case EnvVar(`name`, level, tpe) => tpe }
    case Lambda(name, t, body) if !g.exists(_.name == name) => // T-Abs
      for res <- typeChecks(g + EnvVar(name, level, t))(level)(body)
      yield LambdaType(t, res)
    case App(fun, arg) => // T-App
      for
        LambdaType(t1, t2) <- typeChecks(g)(level)(fun)
        `t1` <- typeChecks(g)(level)(arg)
      yield t2
    case Box(body) if level == 0 => // T-Box
      for t <- typeChecks(g)(1)(body) yield BoxType(t)
    case Lift(body) if level == 0 => // T-Lift
      for NatType <- typeChecks(g)(0)(body) yield BoxType(NatType)
    case Splice(body) if level == 1 => // T-Unbox
      for BoxType(t) <- typeChecks(g)(0)(body) yield t
    case Match(scrutinee, pat, thenp, elsep) => // T-Pat
      for
        BoxType(t1) <- typeChecks(g)(0)(scrutinee)
        delta <- typePatChecks(g, t1)(pat)
        t <- typeChecks(g ++ delta)(0)(thenp)
        `t` <- typeChecks(g)(0)(elsep)
      yield t
    case Fix(t) if level == 0 =>
      for LambdaType(t1, t2) <- typeChecks(g)(0)(t) yield t2 // T-Fix
    case _ => None
  if res.isEmpty then
    println(s"Failed to type $term at level $level with environment $g")
  res


def typePatChecks(g: Gamma, tpe: Type)(pat: Pattern): Option[Delta] =
  (pat, tpe) match
    case (PNat(n), NatType) => Some(Set.empty) // T-Pat-Nat
    case (PLit(name), NatType) => Some(Set(EnvVar(name, 0, NatType))) // T-Pat-Lit
    case (PRef(name), _) if g.contains(EnvVar(name, 0, tpe)) => Some(Set.empty) // T-Pat-Ref
    case (PBind(name), _) if !g.exists(_.name == name) => Some(Set(EnvVar(name, 0, BoxType(tpe)))) // T-Pat-Var
    case (PApp(fun, arg), _) => // T-Pat-App
      val t1 = Name.fresh("T")
      for
        d1 <- typePatChecks(g, LambdaType(TypeVar(t1), tpe))(fun)
        d2 <- typePatChecks(g, TypeVar(t1))(arg)
      yield d1 ++ d2
    case (PFun(name), LambdaType(t1, t2)) if !g.exists(_._1 == name) =>
      Some(Set(EnvVar(name, 0, LambdaType(BoxType(t1), BoxType(t2))))) // T-Pat-Abs
    case _ => None


def step(level: 0 | 1)(term: Term): Term =
  term match
    case Lambda(name, tpe, body) if level == 1 =>
      Lambda(name, tpe, step(1)(body)) // E-Abs
    case App(fun, arg) =>
      if !isValue(fun) then App(step(level)(fun), arg) // E-App-1
      else if !isValue(arg) then App(fun, step(level)(arg)) // E-App-2
      else // E-Beta
        assert(level == 0)
        val Lambda(name, tpe, body) = fun
        subst(body, name, arg)
    case Box(body) if level == 0 =>
      Box(step(1)(body)) // E-Box
    case Splice(body) if level == 1 =>
      body match
        case Box(inner) if isPlainTerm(inner) => inner // E-Splice
        case _ => Splice(step(0)(body)) // E-Unbox
    case Lift(body) if level == 0 =>
      body match
        case Nat(n) => Box(Nat(n)) // E-Lift-Red
        case _ => Lift(step(0)(body)) // E-Lift
    case Match(scrutinee, pat, thenp, elsep) if level == 0=>
      if !isValue(scrutinee) then Match(step(0)(scrutinee), pat, thenp, elsep) // E-Pat
      else
        val Box(s) = scrutinee
        matchPat(s, pat) match
          case Some(subs) => subs(thenp) // E-Pat-Succ
          case None => elsep // E-Pat-Fail
    case Fix(t) =>
      t match
        case Lambda(n, _, body) => subst(body, n, Fix(t)) // E-Fix-Red
        case _ => Fix(step(0)(t)) // E-Fix
    case _ => term


def matchPat(scrutinee: Term, pat: Pattern): Option[Term => Term] =
  assert(isPlainTerm(scrutinee))
  (scrutinee, pat) match
    case (Nat(n), PNat(n2)) if n == n2 => Some(identity)
    case (Nat(n), PLit(x)) => Some(t => subst(t, x, Nat(n)))
    case (Ref(name1), PRef(name2)) if name1 == name2 => Some(identity)
    case (App(t1, t2), PApp(p1, p2)) =>
      for
        s1 <- matchPat(t1, p1)
        s2 <- matchPat(t2, p2)
      yield s1.compose(s2)
    case (scrutinee, PBind(x)) =>
      Some(t => subst(t, x, Box(scrutinee)))
    case (Lambda(x1, t1, t2), PFun(x)) =>
      val x3 = x.fresh()
      Some(t => subst(t, x, Lambda(x3, BoxType(t1), Box(subst(t2, x1, Splice(Ref(x3)))))))
    case _ => None


def subst(body: Term, name: Name, term: Term): Term =
  val substitutedTerm = refresh(term) // Could also be done within the substitution to ensure globally unique names
  // def substitutedTerm = refresh(term) // Alternative with globally unique names
  def subst(body: Term): Term =
    body match
      case Nat(n) => Nat(n)
      case Ref(n) => if n == name then substitutedTerm else Ref(n)
      case Lambda(n, tpe, b) => Lambda(n, tpe, subst(b))
      case App(fun, arg) => App(subst(fun), subst(arg))
      case Box(b) => Box(subst(b))
      case Splice(b) => Splice(subst(b))
      case Lift(b) => Lift(subst(b))
      case Match(scrutinee, pat, thenp, elsep) => Match(subst(scrutinee), pat, subst(thenp), subst(elsep))
      case Fix(body) => Fix(subst(body))
  subst(body)


def refresh(term: Term): Term =
  term match
    case Lambda(n, tpe, b) =>
      val n2 = n.fresh()
      Lambda(n2, tpe, refresh(substName(b, n, n2)))
    case App(fun, arg) => App(refresh(fun), refresh(arg))
    case Box(b) => Box(refresh(b))
    case Splice(b) => Splice(refresh(b))
    case Lift(b) => Lift(refresh(b))
    case Match(scrutinee, pat, thenp, elsep) =>
      def f(p: Pattern): (Pattern, Term => Term) =
        p match
          case PNat(_) => (p, identity)
          case PLit(name) =>
            val n2 = name.fresh()
            (PLit(n2), t => substName(t, name, n2))
          case PRef(_) => (p, identity)
          case PApp(p1, p2) =>
            val (np1, s1) = f(p1)
            val (np2, s2) = f(p2)
            (PApp(np1, np2), s1.compose(s2))
          case PBind(x) =>
            val n2 = x.fresh()
            (PBind(n2), t => substName(t, x, n2))
          case PFun(x) =>
            val n2 = x.fresh()
            (PFun(n2), t => substName(t, x, n2))
      val (pat2, s) = f(pat)
      Match(refresh(scrutinee), pat2, refresh(s(thenp)), refresh(elsep))
    case Fix(body) => Fix(refresh(body))
    case _ => term


def substName(body: Term, name: Name, newName: Name): Term =
  def substPat(p: Pattern): Pattern =
    p match
      case PRef(`name`) => PRef(newName)
      case PApp(p1, p2) => PApp(substPat(p1), substPat(p2))
      case _ => p
  def subst(body: Term): Term =
    body match
      case Nat(n) => Nat(n)
      case Ref(n) => if n == name then Ref(newName) else Ref(n)
      case Lambda(n, tpe, b) => Lambda(n, tpe, subst(b))
      case App(fun, arg) => App(subst(fun), subst(arg))
      case Box(b) => Box(subst(b))
      case Splice(b) => Splice(subst(b))
      case Lift(b) => Lift(subst(b))
      case Match(scrutinee, pat, thenp, elsep) => Match(subst(scrutinee), substPat(pat), subst(thenp), subst(elsep))
      case Fix(body) => Fix(subst(body))
  subst(body)


def eval(term: Term, tpe: Type): Term =
  if isValue(term) then
    term
  else
    val newTerm = step(0)(term)
    println(s"Step: $newTerm")
    assert(isValue(newTerm) || term != newTerm, "Term is stuck")
    assert(typeChecks(Set.empty)(0)(newTerm) == Some(tpe), "Failed to typecheck")
    eval(newTerm, tpe)


@main def Test = {
  def run(term: Term): Unit = {
    typeChecks(Set.empty)(0)(term) match
      case Some(tpe) =>
        println(s"Input: $term\n Type: $tpe")
        println("Result: " + eval(term, tpe))
      case _ =>
        println(s"Failed to typechecks: $term")
    println()
  }
  val x = Name.fresh("x")
  val y = Name.fresh("y")
  val z = Name.fresh("z")
  val f = Name.fresh("f")
  val n = Name.fresh("n")
  val a = Name.fresh("a")
  val b = Name.fresh("b")

  run(Nat(1))
  run(Nat(-1))
  run(Box(Nat(2)))
  run(Lift(Nat(3)))
  run(Lambda(x, NatType, Ref(x)))
  run(App(Lambda(x, NatType, Ref(x)), Nat(4)))
  run(Box(Splice(Box(Nat(1)))))
  run(Box(Splice(Lift(Nat(1)))))
  run(Box(Splice(Box(App(Lambda(x, NatType, Ref(x)), Splice(Lift(Nat(4))))))))
  run(Match(Box(Nat(2)), PNat(2), Nat(0), Nat(1)))
  run(Match(Box(Nat(2)), PNat(3), Nat(1), Nat(0)))
  run(Match(Box(Nat(2)), PLit(x), Ref(x), Nat(0)))
  run(Match(Box(Nat(2)), PBind(x), Ref(x), Box(Nat(0))))
  run(Match(Box(Lambda(y, NatType, Ref(y))), PBind(x), Ref(x), Box(Lambda(z, NatType, Nat(2)))))
  run(Match(Box(App(Lambda(y, NatType, Ref(y)), Nat(4))), PApp(PFun(f), PBind(x)), App(Ref(f), Ref(x)), Box(Nat(3))))
  run(Box(Lambda(x, NatType, Lambda(x, NatType, Ref(x)))))
  run(Fix(Lambda(x, NatType, Nat(1))))
  run(App(
        Fix(
          Lambda(f, LambdaType(NatType, NatType),
          Lambda(n, NatType,
          Match(
            Lift(Ref(n)),
            PNat(0),
            Nat(1),
            App(Lambda(a, NatType, App(Ref(f), Ref(a))), Nat(0)))))),
        Nat(1)))
  run(
    App(
      Lambda(x, BoxType(NatType),
          Match(
            Ref(x),
            PApp(PFun(f), PBind(z)),
            App(Ref(f), Ref(x)),
            Box(Nat(4))
          )
      ),
      Box(
        App(Lambda(a, NatType, App(Lambda(b, NatType, Ref(b)), Ref(a))), Nat(2))
      )
    )
  )

}
