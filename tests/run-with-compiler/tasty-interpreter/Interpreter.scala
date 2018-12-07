import scala.tasty.Reflection

class JVMReflection[R <: Reflection & Singleton](val reflect: R) {
  import reflect._
  import java.lang.reflect.{InvocationTargetException, Method}
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
        val msg = s"Could not find class $name in classpath$extraMsg"
        throw new Exception(msg)
    }
  }

  def interpretStaticMethodCall(moduleClass: Symbol, fn: Symbol, args: => List[Object]): Object = {
      val instance = loadModule(moduleClass)
      val name = fn.name

      val method = getMethod(instance.getClass, name, paramsSig(fn))
      method.invoke(instance, args: _*)
    }

  def getMethod(clazz: Class[_], name: String, paramClasses: List[Class[_]]): Method = {
    try clazz.getMethod(name, paramClasses: _*)
    catch {
      case _: NoSuchMethodException =>
        val msg = s"Could not find method ${clazz.getCanonicalName}.$name with parameters ($paramClasses%, %)$extraMsg"
        throw new Exception(msg)
    }
  }

  /** List of classes of the parameters of the signature of `sym` */
  private def paramsSig(sym: Symbol): List[Class[_]] = {
    def paramClass(param: Type): Class[_] = {
      // def arrayDepth(tpe: Type, depth: Int): (Type, Int) = tpe match {
      //   case JavaArrayType(elemType) => arrayDepth(elemType, depth + 1)
      //   case _ => (tpe, depth)
      // }
      // def javaArraySig(tpe: Type): String = {
      //   val (elemType, depth) = arrayDepth(tpe, 0)
      //   val sym = elemType.classSymbol
      //   val suffix =
      //     if (sym == defn.BooleanClass) "Z"
      //     else if (sym == defn.ByteClass) "B"
      //     else if (sym == defn.ShortClass) "S"
      //     else if (sym == defn.IntClass) "I"
      //     else if (sym == defn.LongClass) "J"
      //     else if (sym == defn.FloatClass) "F"
      //     else if (sym == defn.DoubleClass) "D"
      //     else if (sym == defn.CharClass) "C"
      //     else "L" + javaSig(elemType) + ";"
      //   ("[" * depth) + suffix
      // }
      def javaSig(tpe: Type): String = tpe match {
        // case tpe: JavaArrayType => javaArraySig(tpe)
        case _ =>
          // Take the flatten name of the class and the full package name
          // val pack = tpe.classSymbol.get.topLevelClass.owner
          val packageName = "" //if (pack == defn.EmptyPackageClass) "" else pack.fullName + "."
          packageName + tpe.classSymbol.get.fullName //.fullNameSeparated(FlatName).toString
      }

      val sym = param.classSymbol
      if (sym == definitions.BooleanClass) classOf[Boolean]
      else if (sym == definitions.ByteClass) classOf[Byte]
      else if (sym == definitions.CharClass) classOf[Char]
      else if (sym == definitions.ShortClass) classOf[Short]
      else if (sym == definitions.IntClass) classOf[Int]
      else if (sym == definitions.LongClass) classOf[Long]
      else if (sym == definitions.FloatClass) classOf[Float]
      else if (sym == definitions.DoubleClass) classOf[Double]
      else java.lang.Class.forName(javaSig(param), false, classLoader)
    }
    // def getExtraParams(tp: Type): List[Type] = tp.widenDealias match {
    //   case tp: AppliedType if defn.isImplicitFunctionType(tp) =>
    //     // Call implicit function type direct method
    //     tp.args.init.map(arg => TypeErasure.erasure(arg)) ::: getExtraParams(tp.args.last)
    //   case _ => Nil
    // }
    // val extraParams = getExtraParams(sym.info.finalResultType)
    // val allParams = TypeErasure.erasure(sym.info) match {
    //   case meth: MethodType => meth.paramInfos ::: extraParams
    //   case _ => extraParams
    // }
    // allParams.map(paramClass)

    Nil
  }

  private def extraMsg = ". The most common reason for that is that you apply macros in the compilation run that defines them"
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
        // https://github.com/lampepfl/dotty/blob/4ff8dacd9ca9a1ade6ddf3d6b7a0c78d0c204ec4/compiler/src/dotty/tools/dotc/transform/Splicer.scala#L314
        // todo: add dispatcher
        argss match {
          case Nil =>
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
                  jvmReflection.interpretStaticMethodCall(fn.symbol.owner, fn.symbol, Nil)
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