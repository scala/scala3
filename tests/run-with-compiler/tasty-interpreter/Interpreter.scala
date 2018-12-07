import scala.tasty.Reflection

class Interpreter[R <: Reflection & Singleton](val reflect: R)(implicit ctx: reflect.Context) {
  import reflect._

  type Env = Map[Symbol, Ref]

  // object Call {
  //   def unapply(arg: Tree): Option[(Tree, List[List[Tree]])] = arg match {
  //     case fn@ Term.Select(_, _, _) => Some((fn, Nil))
  //     case Term.Apply(Call(fn, args1), args2) => Some((fn, args1 :+ args2))
  //     case Term.TypeApply(Call(fn, args), _) => Some((fn, args))
  //     case _ => None
  //   }
  // }

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

      // TODO: handle multiple parameters
      case Term.Apply(meth@Term.Ident(_), args) =>
        meth.symbol match {
          case IsDefSymbol(sym) =>
            val evaluatedArgs = args.map(arg => new Eager(eval(arg)))
            val env1 = env ++ sym.tree.paramss.head.map(_.symbol).zip(evaluatedArgs)

            eval(sym.tree.rhs.get)(env1)
        }

      case Term.Ident(_) =>
        tree.symbol match {
          case IsDefSymbol(sym) =>
            eval(sym.tree.rhs.get)
          case _ =>
            env(tree.symbol).get
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