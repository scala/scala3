package scala.tasty.interpreter

import scala.tasty.interpreter.jvm.JVMReflection
import scala.tasty.Reflection

abstract class TreeInterpreter[R <: Reflection & Singleton](val reflect: R) {
  import reflect._

  final val LOG = false

  type Env = Map[Symbol, LocalValue]

  /** Representation of objects and values in the interpreter */
  type AbstractAny

  def interpretCall(fn: Term, argss: List[List[Term]])(implicit env: Env): AbstractAny = {
    val env0 = fn match {
      case Term.Select(prefix, _) =>
        val pre = eval(prefix)
        env // FIXME add pre to the env as `this`
      case _ => env
    }
    fn.symbol match {
      case IsDefSymbol(sym) =>
        val evaluatedArgs = argss.flatten.map(arg => LocalValue.valFrom(eval(arg)))
        val env1 = env0 ++ sym.tree.paramss.headOption.getOrElse(Nil).map(_.symbol).zip(evaluatedArgs)
        eval(sym.tree.rhs.get)(env1)
      case _ =>
        env0(fn.symbol).get
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
  def interpretPrivitiveLtEq(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitiveGtEq(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitivePlus(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitiveMinus(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitiveTimes(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitiveDiv(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitiveQuot(x: AbstractAny, y: AbstractAny): AbstractAny
  def interpretPrivitiveRem(x: AbstractAny, y: AbstractAny): AbstractAny

  def eval(tree: Statement)(implicit env: Env): AbstractAny = {
    tree match {
      case Call(fn, targs, argss) =>
        fn match {
          case Term.Select(_, "<init>") =>  log("interpretNew", tree)(interpretNew(fn, argss))
          case Term.Select(prefix, "isInstanceOf") => log("interpretIsInstanceOf", tree)(interpretIsInstanceOf(eval(prefix), targs.head))
          case Term.Select(prefix, "asInstanceOf") => log("interpretAsInstanceOf", tree)(interpretAsInstanceOf(eval(prefix), targs.head))
          case Term.Select(prefix, "==") => log("interpretEqEq", tree)(interpretEqEq(eval(prefix), eval(argss.head.head)))
          case Term.Select(prefix, name @ ("+" | "-" | "*" | "<" | ">" | "<=" | "=>")) if isNumericPrimitive(prefix.tpe) =>
            val lhs = eval(prefix)
            val rhs = eval(argss.head.head)
            name match {
              case "+" => log("interpretPrivitivePlus", tree)(interpretPrivitivePlus(lhs, rhs))
              case "-" => log("interpretPrivitiveMinus", tree)(interpretPrivitiveMinus(lhs, rhs))
              case "*" => log("interpretPrivitiveTimes", tree)(interpretPrivitiveTimes(lhs, rhs))
              case "<" => log("interpretPrivitiveLt", tree)(interpretPrivitiveLt(lhs, rhs))
              case ">" => log("interpretPrivitiveGt", tree)(interpretPrivitiveGt(lhs, rhs))
              case "<=" => log("interpretPrivitiveLtEq", tree)(interpretPrivitiveLtEq(lhs, rhs))
              case ">=" => log("interpretPrivitiveGtEq", tree)(interpretPrivitiveGtEq(lhs, rhs))
            }
          case Term.Select(prefix, name @ ("/" | "%")) if isIntegralPrimitive(prefix.tpe) =>
            def lhs = eval(prefix)
            def rhs = eval(argss.head.head)
            if (name == "/") log("interpretPrivitiveQuot", tree)(interpretPrivitiveQuot(lhs, rhs))
            else log("interpretPrivitiveRem", tree)(interpretPrivitiveRem(lhs, rhs))
          case Term.Select(prefix, name @ "/") if isFractionalPrimitive(prefix.tpe) =>
            def lhs = eval(prefix)
            def rhs = eval(argss.head.head)
            log("interpretPrivitiveDiv", tree)(interpretPrivitiveDiv(lhs, rhs))
          case _ => log("interpretCall", tree)(interpretCall(fn, argss))
        }

      case Term.Assign(lhs, rhs) =>
        log("<interpretAssing>", tree)(env(lhs.symbol).update(eval(rhs)))

      case Term.If(cond, thenp, elsep) => log("interpretIf", tree)(interpretIf(cond, thenp, elsep))
      case Term.While(cond, body)      => log("interpretWhile", tree)(interpretWhile(cond, body))
      case Term.Block(stats, expr)     => log("interpretBlock", tree)(interpretBlock(stats, expr))
      case Term.Literal(const)         => log("interpretLiteral", tree)(interpretLiteral(const))
      case Term.Typed(expr, _)         => log("<interpretTyped>", tree)(eval(expr))

      case _ => throw new MatchError(tree.show)
    }
  }

  inline def log[T](tag: => String, tree: => Statement)(thunk: => T): T = {
    if (LOG)
      println(s"#> $tag: ${tree.show}")
    thunk
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

  private def isNumericPrimitive(tpe: Type): Boolean =
    isIntegralPrimitive(tpe) || isFractionalPrimitive(tpe)

  private def isIntegralPrimitive(tpe: Type): Boolean = {
    tpe <:< definitions.ByteType ||
    tpe <:< definitions.CharType ||
    tpe <:< definitions.ShortType ||
    tpe <:< definitions.IntType ||
    tpe <:< definitions.LongType
  }

  private def isFractionalPrimitive(tpe: Type): Boolean =
    tpe <:< definitions.FloatType || tpe <:< definitions.DoubleType


  private object Call {
    def unapply(arg: Tree): Option[(Term, List[TypeTree], List[List[Term]])] = arg match {
      case Term.IsSelect(fn) => Some((fn, Nil, Nil))
      case Term.IsIdent(fn) => Some((fn, Nil, Nil))
      case Term.Apply(Call(fn, targs, args1), args2) => Some((fn, targs, args1 :+ args2))
      case Term.TypeApply(Call(fn, _, _), targs) => Some((fn, targs, Nil))
      case _ => None
    }
  }
}