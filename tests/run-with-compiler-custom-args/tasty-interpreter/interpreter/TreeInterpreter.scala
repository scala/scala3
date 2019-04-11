package scala.tasty.interpreter

import scala.tasty.interpreter.jvm.JVMReflection
import scala.tasty.Reflection

abstract class TreeInterpreter[R <: Reflection & Singleton](val reflect: R) {
  import reflect._

  final val LOG = false

  type Env = Map[Symbol, LocalValue]

  /** Representation of objects and values in the interpreter */
  type AbstractAny

  type Result = given Env => AbstractAny

  def localValue(sym: Symbol)(implicit env: Env): LocalValue = env(sym)

  def withLocalValue[T](sym: Symbol, value: LocalValue)(in: given Env => T)(implicit env: Env): T =
    in given env.updated(sym, value)

  def withLocalValues[T](syms: List[Symbol], values: List[LocalValue])(in: given Env => T)(implicit env: Env): T =
    in given (env ++ syms.zip(values))

  def interpretCall(inst: AbstractAny, sym: DefDefSymbol, args: List[AbstractAny]): Result = {
    // TODO
    // withLocalValue(`this`, inst) {
      val syms = sym.tree.paramss.headOption.getOrElse(Nil).map(_.symbol)
      withLocalValues(syms, args.map(LocalValue.valFrom(_))) {
        eval(sym.tree.rhs.get)
      }
    // }
  }

  def interpretCall(fn: Term, argss: List[List[Term]]): Result = {
    fn match {
      case Select (prefix, _) =>
        val pre = eval (prefix)
        // TODO use
      case _ =>
    }
    val evaluatedArgs = argss.flatten.map(arg => LocalValue.valFrom(eval(arg)))
    val IsDefDefSymbol(sym) = fn.symbol
    val syms = sym.tree.paramss.headOption.getOrElse(Nil).map(_.symbol)
    withLocalValues(syms, evaluatedArgs) {
      eval(sym.tree.rhs.get)
    }
  }

  def interpretValGet(fn: Term): Result =
    localValue(fn.symbol).get

  def interpretNew(fn: Tree, argss: List[List[Term]]): Result

  def interpretIf(cond: Term, thenp: Term, elsep: Term): Result =
    if (eval(cond).asInstanceOf[Boolean]) eval(thenp)
    else eval(elsep)

  def interpretWhile(cond: Term, body: Term): Result = {
    while (eval(cond).asInstanceOf[Boolean]) eval(body)
    interpretUnit()
  }

  def interpretBlock(stats: List[Statement], expr: Term): Result = {
    val newEnv = stats.foldLeft(implicitly[Env])((accEnv, stat) => stat match {
      case ValDef(name, tpt, Some(rhs)) =>
        def evalRhs = eval(rhs) given accEnv
        val evalRef: LocalValue =
          if (stat.symbol.flags.is(Flags.Lazy)) LocalValue.lazyValFrom(evalRhs)
          else if (stat.symbol.flags.is(Flags.Mutable)) LocalValue.varFrom(evalRhs)
          else LocalValue.valFrom(evalRhs)

        accEnv.updated(stat.symbol, evalRef)
      case DefDef(_, _, _, _, _) =>
        // TODO: record the environment for closure purposes
        accEnv
      case stat =>
        eval(stat) given accEnv
        accEnv
    })
    eval(expr) given newEnv
  }

  def interpretUnit(): AbstractAny
  def interpretLiteral(const: Constant): Result

  def interpretIsInstanceOf(o: AbstractAny, tpt: TypeTree): Result
  def interpretAsInstanceOf(o: AbstractAny, tpt: TypeTree): Result

  def interpretRepeated(elems: List[AbstractAny]): AbstractAny

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

  def eval(tree: Statement): Result = {
    tree match {
      case Call(fn, targs, argss) =>
        fn match {
          case Select(_, "<init>") =>  log("interpretNew", tree)(interpretNew(fn, argss))
          case Select(prefix, "isInstanceOf") => log("interpretIsInstanceOf", tree)(interpretIsInstanceOf(eval(prefix), targs.head))
          case Select(prefix, "asInstanceOf") => log("interpretAsInstanceOf", tree)(interpretAsInstanceOf(eval(prefix), targs.head))
          case Select(prefix, "==") => log("interpretEqEq", tree)(interpretEqEq(eval(prefix), eval(argss.head.head)))
          case Select(prefix, name @ ("+" | "-" | "*" | "<" | ">" | "<=" | "=>")) if isNumericPrimitive(prefix.tpe) =>
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
          case Select(prefix, name @ ("/" | "%")) if isIntegralPrimitive(prefix.tpe) =>
            def lhs = eval(prefix)
            def rhs = eval(argss.head.head)
            if (name == "/") log("interpretPrivitiveQuot", tree)(interpretPrivitiveQuot(lhs, rhs))
            else log("interpretPrivitiveRem", tree)(interpretPrivitiveRem(lhs, rhs))
          case Select(prefix, name @ "/") if isFractionalPrimitive(prefix.tpe) =>
            def lhs = eval(prefix)
            def rhs = eval(argss.head.head)
            log("interpretPrivitiveDiv", tree)(interpretPrivitiveDiv(lhs, rhs))
          case _ =>
            fn.symbol match {
              case IsDefDefSymbol(sym) => log("interpretCall", tree)(interpretCall(fn, argss))
              case _ =>
                assert(argss.isEmpty)
                log("interpretValGet", tree)(interpretValGet(fn))

            }
        }

      case Assign(lhs, rhs) =>
        log("<interpretAssing>", tree)(localValue(lhs.symbol).update(eval(rhs)))

      case If(cond, thenp, elsep) => log("interpretIf", tree)(interpretIf(cond, thenp, elsep))
      case While(cond, body)      => log("interpretWhile", tree)(interpretWhile(cond, body))
      case Block(stats, expr)     => log("interpretBlock", tree)(interpretBlock(stats, expr))
      case Literal(const)         => log("interpretLiteral", tree)(interpretLiteral(const))
      case Typed(expr, _)         => log("<interpretTyped>", tree)(eval(expr))
      case Repeated(elems, _)     => log("<interpretRepeated>", tree)(interpretRepeated(elems.map(elem => eval(elem))))

      case _ => throw new MatchError(tree.showExtractors)
    }
  }

  inline def log[T](tag: => String, tree: => Statement)(thunk: => T): T = {
    if (LOG)
      println(
        s"""#> $tag:
           |${tree.show}
           |${tree.showExtractors}
           |
           |""".stripMargin)
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
      case IsSelect(fn) => Some((fn, Nil, Nil))
      case IsIdent(fn) => Some((fn, Nil, Nil))
      case Apply(Call(fn, targs, args1), args2) => Some((fn, targs, args1 :+ args2))
      case TypeApply(Call(fn, _, _), targs) => Some((fn, targs, Nil))
      case _ => None
    }
  }
}
