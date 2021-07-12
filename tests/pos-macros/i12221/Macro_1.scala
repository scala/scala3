import scala.quoted.*
import scala.reflect.*

object Macros {

    inline def show[A,B](inline expr: A => B): Unit =
        ${printExpr('expr)}

    def printExpr[A,B](expr: Expr[A=>B])(using Quotes) = '{
        println(${showExpr3(expr)})
    }

    def showExpr3[A,B](expr: Expr[A=>B])(using Quotes): Expr[String] =
        import quotes.reflect.*

        val sb = new StringBuilder

        // Makes us only print the body of thr function
        def printDefFun(tree: Tree): Unit ={
            val acc = new TreeAccumulator[Unit]{
                def foldTree(s: Unit, tree: Tree)(owner: Symbol): Unit =
                    tree match
                        case deff : DefDef =>
                            treePrint(deff.rhs.get, 0)
                            sb.append("++++++++++++++++\n")
                            sb.append(deff.rhs.get.show(using Printer.TreeStructure)).append('\n')
                        case _ =>
                            foldOverTree(s, tree)(owner)
                }
            acc.foldTree(List(), tree)(tree.symbol)
        }

        def treePrint(tree: Tree, level: Int): Unit = {
            val pre = "   " * level
            tree match {
                case body : Term => {
                    body match {
                        // Normal typed
                        case typed: Typed =>
                            sb.append(pre + typed.getClass()).append('\n')
                            sb.append(pre + s"Typed with ${typed.tpt}:\n")
                            treePrint(typed.expr , level + 1)
                        case Block(statements, expr) =>
                            sb.append(pre + "Block:{").append('\n')
                            statements.map(stat => stat match{
                                case term: Term => treePrint(term, level + 1)
                                case deff: Definition =>
                                    sb.append(pre + "Definition statement\n")
                                    treePrint(deff, level + 1)
                                case _ =>
                                    sb.append(pre + "Non-term statement\n")
                                    sb.append(stat.show(using Printer.TreeStructure)).append('\n')
                                })
                            treePrint(expr, level + 1)
                            sb.append(pre + "}\n")

                        case Match(scrutinee, cases) =>
                            sb.append(pre + "Match:\n")
                            treePrint(scrutinee, level + 1)
                            sb.append(pre + "with\n")
                            cases.map(treePrint(_, level +1))

                        case Ident(name) =>
                            sb.append(pre + s"Identifier(${name})\n")

                        case Apply(fun, args) =>
                            sb.append(pre + "Apply\n")
                            treePrint(fun, level + 1)
                            if !args.isEmpty then
                                sb.append(pre + "with arguments\n")
                                args.zipWithIndex.map(
                                    (arg, index) =>
                                        treePrint(arg, level +1)
                                        if args.size > 1 && index < args.size -1  then
                                            // Used to seperate list of parameters
                                            sb.append(pre + ",\n")
                                    )
                        case _ =>
                            sb.append("Term\n")
                            sb.append(tree.getClass()).append('\n')
                            sb.append(tree.show(using Printer.TreeStructure)).append('\n')
                    }
                }

                case CaseDef(pattern, guard, rhs) =>
                    sb.append(pre + "caseDef:\n" )
                    treePrint(pattern, level + 1)
                    treePrint(rhs, level + 1)

                //Adding this unappy makes the typed get swallowed
                /*
                case Unapply(fun, implicits, pattern) =>
                    sb.append(pre + "Unapply with function").append('\n')
                    treePrint(fun , level + 1)
                    sb.append(pre + "with patterns").append('\n')
                    pattern.map(treePrint(_ , level + 1))
                */
                case b: Bind => sb.append(pre + "Bind with stuff").append('\n')

                case typed : Typed =>
                        //sb.append(pre + typed.getClass()).append('\n')
                        sb.append(pre + tree.getClass()).append('\n')
                        sb.append(pre + s"Typed2 with ${typed.tpt}:").append('\n')
                        treePrint(typed.expr , level + 1)

                case Unapply(_,_,_) => sb.append(pre + "Unapply with stuff").append('\n')
                case _ =>
                    tree match
                        case t: Term => sb.append("Term").append('\n')
                        case _ => ()
                    sb.append(tree.getClass()).append('\n')
                    sb.append(tree.show(using Printer.TreeStructure)).append('\n')
            }
        }

        val tree: Term = expr.asTerm
        printDefFun(tree)
        sb.append("Finished").append('\n')
        Expr(sb.result())
}
