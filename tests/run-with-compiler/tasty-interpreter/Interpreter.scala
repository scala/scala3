import scala.tasty.Reflection

class JVMReflection[R <: Reflection & Singleton](val reflect: R) {
  import reflect._

  private val classLoader: ClassLoader = getClass.getClassLoader

  // taken from StdNames
  final val MODULE_INSTANCE_FIELD      = "MODULE$"

  def loadModule(sym: Symbol): Object = {
      if (sym.owner.flags.isPackage) {
        // is top level object
        val moduleClass = loadClass(sym.fullName)
        moduleClass.getField(MODULE_INSTANCE_FIELD).get(null)
      }
      else {
        // nested object in an object
        // val clazz = loadClass(sym.fullNameSeparated(FlatName))
        // clazz.getConstructor().newInstance().asInstanceOf[Object]
        ???
      }
    }

 def loadClass(name: String): Class[_] = {
    try classLoader.loadClass(name)
    catch {
      case _: ClassNotFoundException =>
        val msg = s"Could not find class $name in classpath"
        throw new Exception(msg)
    }
  }
}

class Interpreter[R <: Reflection & Singleton](val reflect: R)(implicit ctx: reflect.Context) {
  import reflect._

  val jvmReflection = new JVMReflection(reflect)

  type Env = Map[Symbol, Ref]

  object Call {
    def unapply(arg: Tree): Option[(Tree, List[List[Term]])] = arg match {
      case fn@ Term.Ident(_) => Some((fn, Nil))
      case fn@ Term.Select(_, _) => Some((fn, Nil))
      case Term.Apply(Call(fn, args1), args2) => Some((fn, args1 :+ args2))
      case Term.TypeApply(Call(fn, args), _) => Some((fn, args))
      case _ => None
    }
  }

  def eval(tree: Statement)(implicit env: Env): Any = {
    // Our debug println
    // println(tree.show)

    tree match {
      case Term.Apply(Term.Ident("println"), List(arg)) =>
        println(eval(arg))

      case Term.Apply(Term.Ident("println"), List()) =>
        println()

      case Term.Apply(Term.Select(a, op), List(b)) =>
        op match {
          case "+" => eval(a).asInstanceOf[Int] + eval(b).asInstanceOf[Int]
          case "-" => eval(a).asInstanceOf[Int] - eval(b).asInstanceOf[Int]
          case ">" => eval(a).asInstanceOf[Int] > eval(b).asInstanceOf[Int]
          case "==" => eval(a).asInstanceOf[Int] == eval(b).asInstanceOf[Int]
        }

      case Call(fn, argss) =>
        argss match {
          case Nil =>
            fn.symbol match {
              case IsDefSymbol(sym) =>
                eval(sym.tree.rhs.get)
              case _ =>
                if (fn.symbol.isDefinedInCurrentRun) {
                  env(tree.symbol).get
                }
                else {
                  jvmReflection.loadModule(fn.symbol.asVal.moduleClass.get)
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