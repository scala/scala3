package scala.tasty.interpreter

import scala.tasty.interpreter.abstr.{Ref, Eager, Lazy, Var}
import scala.tasty.interpreter.jvm.JVMReflection
import scala.tasty.Reflection

abstract class TreeInterpreter[R <: Reflection & Singleton](val reflect: R) {
  import reflect._

  type Env = Map[Symbol, Ref]

  def interpretCall(fn: Tree, argss: List[List[Term]])(implicit env: Env): Any = {
    fn.symbol match {
      case IsDefSymbol(sym) =>
        val evaluatedArgs = argss.flatten.map(arg => new Eager(eval(arg)))
        val env1 = env ++ sym.tree.paramss.headOption.getOrElse(Nil).map(_.symbol).zip(evaluatedArgs)
        eval(sym.tree.rhs.get)(env1)
      case _ =>
        env(fn.symbol).get
    }
  }

  def interpretNew(fn: Tree, argss: List[List[Term]])(implicit env: Env): Any

  def interpretIf(cond: Term, thenp: Term, elsep: Term)(implicit env: Env): Any =
    if (eval(cond).asInstanceOf[Boolean]) eval(thenp)
    else eval(elsep)

  def interpretWhile(cond: Term, body: Term)(implicit env: Env): Any =
    while (eval(cond).asInstanceOf[Boolean]) eval(body)

  def interpretBlock(stats: List[Statement], expr: Term)(implicit env: Env): Any = {
    val newEnv = stats.foldLeft(env)((accEnv, stat) => stat match {
      case ValDef(name, tpt, Some(rhs)) =>
        val evalRef: Ref =
          if (stat.symbol.flags.isLazy)
          // do not factor out rhs from here (laziness)
            new Lazy(eval(rhs)(accEnv))
          else if (stat.symbol.flags.isMutable)
            new Var(eval(rhs)(accEnv))
          else
            new Eager(eval(rhs)(accEnv))

        accEnv.updated(stat.symbol, evalRef)
      case DefDef(_, _, _, _, _) =>
        // TODO: record the environment for closure purposes
        accEnv
      case stat =>
        eval(stat)(accEnv)
        accEnv
    })
    eval(expr)(newEnv)
  }

  def interpretLiteral(const: Constant)(implicit env: Env): Any = const.value

  def interpretIsInstanceOf(prefix: Term, tpt: TypeTree)(implicit env: Env): Any
  def interpretAsInstanceOf(prefix: Term, tpt: TypeTree)(implicit env: Env): Any

  def eval(tree: Statement)(implicit env: Env): Any = {
    tree match {
      case Call(fn, targs, argss) =>
        fn match {
          case Term.Select(_, "<init>") => interpretNew(fn, argss)
          case Term.Select(prefix, "isInstanceOf") => interpretIsInstanceOf(prefix, targs.head)
          case Term.Select(prefix, "asInstanceOf") => interpretAsInstanceOf(prefix, targs.head)
          case _ => interpretCall(fn, argss)
        }

      case Term.Assign(lhs, rhs) =>
        env(lhs.symbol) match {
          case varf: Var => varf.update(eval(rhs))
        }

      case Term.If(cond, thenp, elsep) => interpretIf(cond, thenp, elsep)
      case Term.While(cond, body)      => interpretWhile(cond, body)
      case Term.Block(stats, expr)     => interpretBlock(stats, expr)
      case Term.Literal(const)         => interpretLiteral(const)
      case Term.Typed(expr, _)         => eval(expr)

      case _ => throw new MatchError(tree.show)
    }
  }

  private object Call {
    def unapply(arg: Tree): Option[(Tree, List[TypeTree], List[List[Term]])] = arg match {
      case fn@ Term.Select(_, _) => Some((fn, Nil, Nil))
      case fn@ Term.Ident(_) => Some((fn, Nil, Nil))
      case Term.Apply(Call(fn, targs, args1), args2) => Some((fn, targs, args1 :+ args2))
      case Term.TypeApply(Call(fn, _, _), targs) => Some((fn, targs, Nil))
      case _ => None
    }
  }
}