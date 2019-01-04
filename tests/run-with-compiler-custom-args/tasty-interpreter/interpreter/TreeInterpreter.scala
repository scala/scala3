package scala.tasty.interpreter

import scala.tasty.interpreter.jvm.JVMReflection
import scala.tasty.Reflection

abstract class TreeInterpreter[R <: Reflection & Singleton](val reflect: R) {
  import reflect._

  type Env = Map[Symbol, LocalValue]

  /** Representation of objects and values in the interpreter */
  type AbstractAny

  def interpretCall(fn: Tree, argss: List[List[Term]])(implicit env: Env): AbstractAny = {
    fn.symbol match {
      case IsDefSymbol(sym) =>
        val evaluatedArgs = argss.flatten.map(arg => LocalValue.valFrom(eval(arg)))
        val env1 = env ++ sym.tree.paramss.headOption.getOrElse(Nil).map(_.symbol).zip(evaluatedArgs)
        eval(sym.tree.rhs.get)(env1)
      case _ =>
        env(fn.symbol).get
    }
  }

  def interpretNew(fn: Tree, argss: List[List[Term]])(implicit env: Env): AbstractAny

  def interpretIf(cond: Term, thenp: Term, elsep: Term)(implicit env: Env): AbstractAny =
    if (eval(cond).asInstanceOf[Boolean]) eval(thenp)
    else eval(elsep)

  def interpretWhile(cond: Term, body: Term)(implicit env: Env): AbstractAny = {
    while (eval(cond).asInstanceOf[Boolean]) eval(body)
    interpretUnit()
  }

  def interpretBlock(stats: List[Statement], expr: Term)(implicit env: Env): AbstractAny = {
    val newEnv = stats.foldLeft(env)((accEnv, stat) => stat match {
      case ValDef(name, tpt, Some(rhs)) =>
        def evalRhs = eval(rhs)(accEnv)
        val evalRef: LocalValue =
          if (stat.symbol.flags.isLazy) LocalValue.lazyValFrom(evalRhs)
          else if (stat.symbol.flags.isMutable) LocalValue.varFrom(evalRhs)
          else LocalValue.valFrom(evalRhs)

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

  def interpretUnit(): AbstractAny
  def interpretLiteral(const: Constant)(implicit env: Env): AbstractAny

  def interpretIsInstanceOf(o: AbstractAny, tpt: TypeTree)(implicit env: Env): AbstractAny
  def interpretAsInstanceOf(o: AbstractAny, tpt: TypeTree)(implicit env: Env): AbstractAny

  def interpretEqEq(x: AbstractAny, y: AbstractAny): AbstractAny

  def interpretPrivitiveLt(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitiveGt(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitivePlus(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitiveMinus(x: AbstractAny, y: AbstractAny): AbstractAny

  def eval(tree: Statement)(implicit env: Env): AbstractAny = {

    tree match {
      case Call(fn, targs, argss) =>
        fn match {
          case Term.Select(_, "<init>") => interpretNew(fn, argss)
          case Term.Select(prefix, "isInstanceOf") => interpretIsInstanceOf(eval(prefix), targs.head)
          case Term.Select(prefix, "asInstanceOf") => interpretAsInstanceOf(eval(prefix), targs.head)
          case Term.Select(prefix, "==") => interpretEqEq(eval(prefix), eval(argss.head.head))
          case Term.Select(prefix, name) if isNumericPrimitive(prefix.tpe) =>
            val lhs = eval(prefix)
            val rhs = eval(argss.head.head)
            name match {
              case "+" => interpretPrivitivePlus(lhs, rhs)
              case "-" => interpretPrivitiveMinus(lhs, rhs)
              case "<" => interpretPrivitiveLt(lhs, rhs)
              case ">" => interpretPrivitiveGt(lhs, rhs)
            }
          case _ => interpretCall(fn, argss)
        }

      case Term.Assign(lhs, rhs) =>
        env(lhs.symbol).update(eval(rhs))

      case Term.If(cond, thenp, elsep) => interpretIf(cond, thenp, elsep)
      case Term.While(cond, body)      => interpretWhile(cond, body)
      case Term.Block(stats, expr)     => interpretBlock(stats, expr)
      case Term.Literal(const)         => interpretLiteral(const)
      case Term.Typed(expr, _)         => eval(expr)

      case _ => throw new MatchError(tree.show)
    }
  }

  trait LocalValue {
    def get: AbstractAny
    def update(rhs: AbstractAny): AbstractAny = throw new UnsupportedOperationException
  }

  object LocalValue {
    def valFrom(rhs: AbstractAny): LocalValue = new LocalValue {
      def get: AbstractAny = rhs
    }
    def lazyValFrom(rhs: => AbstractAny): LocalValue = new LocalValue {
      lazy val get: AbstractAny = rhs
    }
    def varFrom(rhs: AbstractAny): LocalValue = new LocalValue {
      private[this] var value = rhs
      def get = value
      override def update(rhs: AbstractAny): AbstractAny = {
        value = rhs
        interpretUnit()
      }
    }
  }

  private def isNumericPrimitive(tpe: Type): Boolean = {
    // TODO: improve
    tpe <:< definitions.ByteType ||
    tpe <:< definitions.CharType ||
    tpe <:< definitions.ShortType ||
    tpe <:< definitions.IntType ||
    tpe <:< definitions.LongType ||
    tpe <:< definitions.FloatType ||
    tpe <:< definitions.DoubleType
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