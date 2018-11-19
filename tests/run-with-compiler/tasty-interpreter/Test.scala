import scala.tasty.Reflection
import scala.tasty.file._

object Test {
  def main(args: Array[String]): Unit = {
    ConsumeTasty("", List("Foo"), new TastyInterpreter)
  }
}

trait Ref {
  def value: Any
}
class Eager(val value: Any) extends Ref
class Lazy(thunk: => Any) extends Ref {
  lazy val value: Any = thunk
}

class Interpreter[R <: Reflection & Singleton](val reflect: R)(implicit ctx: reflect.Context) {
  import reflect._

  type Env = Map[Symbol, Ref]

  def eval(tree: Statement)(implicit env: Env): Any = {
    // Our debug println
    // println(tree.show)

    tree match {
      case Term.Apply(Term.Ident("println"), List(arg)) =>
        println(eval(arg))

      case Term.Apply(Term.Select(a, "+", Some(sig)), List(b)) =>
        eval(a).asInstanceOf[Int] + eval(b).asInstanceOf[Int]

      case Term.Ident(_) =>

        tree.symbol match {
          case IsDefSymbol(sym) =>
            eval(sym.tree.rhs.get)
          case _ =>
            env(tree.symbol).value
        }

      case Term.Block(stats, expr) =>

        val newEnv = stats.foldLeft(env)((accEnv, stat) => stat match {
          case ValDef(name, tpt, Some(rhs)) =>

            val evalRef: Ref =
              if (stat.symbol.flags.isLazy)
                new Lazy(eval(rhs)(accEnv))
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

class TastyInterpreter extends TastyConsumer {

  final def apply(reflect: Reflection)(root: reflect.Tree): Unit = {
    import reflect._
    object Traverser extends TreeTraverser {

      override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
        // TODO: check the correct sig and object enclosement for main
        case DefDef("main", _, _, _, Some(rhs)) =>
          val interpreter = new Interpreter(reflect)

          println("Found main")

          interpreter.eval(rhs)(Map.empty)
        // TODO: recurse only for PackageDef, ClassDef
        case tree =>
          super.traverseTree(tree)
      }
    }
    Traverser.traverseTree(root)(reflect.rootContext)
  }
}
