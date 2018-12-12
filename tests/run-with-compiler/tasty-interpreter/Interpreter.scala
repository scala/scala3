import scala.tasty.Reflection

trait Ref {
  def get: Any
}

class Eager(val get: Any) extends Ref

class Lazy(thunk: => Any) extends Ref {
  lazy val get: Any = thunk
}

class Var(private var value: Any) extends Ref {
  def get = value

  def update(rhs: Any): Unit = {
    value = rhs
  }
}

class Interpreter[R <: Reflection & Singleton](val reflect: R)(implicit ctx: reflect.Context) {
  import reflect._

  val jvmReflection = new JVMReflection(reflect)

  type Env = Map[Symbol, Ref]

  enum CallMode {
    case Interpret, EvalOp, ReflectPrecompiled, Predef, Default
  }

  object Call {
    /*
      fn
      args
      mode
    */
    def unapply(arg: Tree): Option[(AnyRef, Tree, List[List[Term]], CallMode)] = arg match {
      case fn@ Term.Apply(Term.Ident("println"), arg) => Some((AnyRef, fn, List(arg), CallMode.Predef))
      case Term.Apply(fn@ Term.Select(a, op), args) =>
        op match {
          case "+" | "-" | ">" | "==" => Some((a, fn, List(args), CallMode.EvalOp))
        }
      case fn@ Term.Select(_, _) => Some((AnyRef, fn, Nil, CallMode.Default))
      case fn@ Term.Ident(_) => Some((AnyRef, fn, Nil, CallMode.Default))
      case Term.Apply(Call(_, fn, args1, _), args2) => Some((AnyRef, fn, args1 :+ args2, CallMode.Default))
      case Term.TypeApply(Call(_, fn, args, _), _) => Some((AnyRef, fn, args, CallMode.Default))

      case _ => None
    }
  }

  def eval(tree: Statement)(implicit env: Env): Any = {
    tree match {
      case Call(prefix, fn, argss, mode) =>
        mode match {
          case CallMode.Predef => fn match {
            case Term.Apply(Term.Ident("println"), List(arg)) => println(eval(arg))
            case Term.Apply(Term.Ident("println"), List()) => println()
          }
          case CallMode.EvalOp => prefix match {
            case Term.Apply(Term.Select(a, op), List(b)) =>
              val b = argss.head.head
              op match {
                case "+" => eval(a).asInstanceOf[Int] + eval(b).asInstanceOf[Int]
                case "-" => eval(a).asInstanceOf[Int] - eval(b).asInstanceOf[Int]
                case ">" => eval(a).asInstanceOf[Int] > eval(b).asInstanceOf[Int]
                case "==" => eval(a).asInstanceOf[Int] == eval(b).asInstanceOf[Int]
              }
            case _ => println("Blah: " + prefix)
          }
          case CallMode.Default => argss match {
            case Nil | List(List()) =>
              fn.symbol match {
                case IsDefSymbol(sym) =>
                  if (fn.symbol.isDefinedInCurrentRun) {
                    eval(sym.tree.rhs.get)
                  }
                  // call to a static def, no parameters
                  else {
                    jvmReflection.interpretStaticMethodCall(fn.symbol.owner, fn.symbol, Nil)
                  }
                case _ =>
                  if (fn.symbol.isDefinedInCurrentRun) {
                    env(tree.symbol).get
                  }
                  // call to a module
                  else if (fn.symbol.flags.isObject) {
                    jvmReflection.loadModule(fn.symbol.asVal.moduleClass.get)
                  }
                  // call to a static val
                  else {
                    jvmReflection.interpretStaticVal(fn.symbol.owner, fn.symbol)
                  }
              }
            case x :: Nil =>
              fn.symbol match {
                case IsDefSymbol(sym) =>
                  val evaluatedArgs = x.map(arg => new Eager(eval(arg)))
                  val env1 = env ++ sym.tree.paramss.head.map(_.symbol).zip(evaluatedArgs)
                  eval(sym.tree.rhs.get)(env1)
              }
          }
        }

      case Term.Assign(lhs, rhs) =>
        env(lhs.symbol) match {
          case varf: Var => varf.update(eval(rhs))
        }

      case Term.If(cond, thenp, elsep) =>
        if(eval(cond).asInstanceOf[Boolean])
          eval(thenp)
        else
          eval(elsep)

      case Term.While(cond, expr) =>
        while(eval(cond).asInstanceOf[Boolean]){
          eval(expr)
        }

      case Term.Block(stats, expr) =>
        val newEnv = stats.foldLeft(env)((accEnv, stat) => stat match {
          case ValDef(name, tpt, Some(rhs)) =>
            val evalRef: Ref =
              if (stat.symbol.flags.isLazy)
                new Lazy(eval(rhs)(accEnv)) // do not factor out rhs from here (laziness)
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


      case Term.Literal(const) =>
        const.value

      case Term.Typed(expr, _) => eval(expr)

      case _ => throw new MatchError(tree.show)
    }
  }
}