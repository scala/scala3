package dotty.tools.dotc
package ast

import core._
import Symbols._, Types._, Contexts._, Decorators._, util.Spans._, Flags._, Constants._
import StdNames.nme
import ast.Trees._
import Names.TermName

/** Generate proxy classes for @main functions.
 *  A function like
 *
 *     @main def f(x: S, ys: T*) = ...
 *
 *  would be translated to something like
 *
 *     object f extends main {
 *       @static def main(args: Array[String]): Unit =
 *         val cmd = command(args, "f", "")
 *         val arg1 = cmd.argGetter[S]("x")
 *         val arg2 = cmd.argsGetter[T]("ys")
 *         cmd.run(f(arg1(), arg2(): _*))
 *     }
 */
object MainProxies {

  def mainProxies(stats: List[tpd.Tree])(using Context): List[untpd.Tree] = {
    import tpd._
    def mainMethods(stats: List[Tree]): List[Symbol] = stats.flatMap {
      case stat: DefDef if stat.symbol.hasAnnotation(defn.MainAnnot) =>
        stat.symbol :: Nil
      case stat @ TypeDef(name, impl: Template) if stat.symbol.is(Module) =>
        mainMethods(impl.body)
      case _ =>
        Nil
    }
    mainMethods(stats).flatMap(mainProxy)
  }

  import untpd._
  def mainProxy(mainFun: Symbol)(using Context): List[ModuleDef] = {
    val mainAnnotSpan = mainFun.getAnnotation(defn.MainAnnot).get.tree.span
    def pos = mainFun.sourcePos
    val mainArgsName: TermName = nme.args
    val cmdName: TermName = Names.termName("cmd")

    def createValArgs(mt: MethodType, cmdName: TermName, idx: Int): List[ValDef] =
      if (mt.isImplicitMethod) {
        report.error(s"@main method cannot have implicit parameters", pos)
        Nil
      }
      else {
        var valArgs: List[ValDef] = Nil
        // TODO check & handle default value
        val ValDefargs = mt.paramInfos.zip(mt.paramNames).zipWithIndex map {
          case ((formal, paramName), n) =>
            val (getterSym, formalElem) =
              if (formal.isRepeatedParam) (defn.MainAnnotCommand_argsGetter, formal.argTypes.head)
              else (defn.MainAnnotCommand_argGetter, formal)
            val valArg = ValDef(
              mainArgsName ++ (idx + n).toString, // FIXME
              TypeTree(defn.FunctionOf(Nil, formalElem)),
              Apply(
                TypeApply(Select(Ident(cmdName), getterSym.name), TypeTree(formalElem) :: Nil),
                Literal(Constant(paramName.toString)) :: Nil
              ),
            )
            valArgs = valArgs :+ valArg
        }
        mt.resType match {
          case restpe: MethodType =>
            if (mt.paramInfos.lastOption.getOrElse(NoType).isRepeatedParam)
              report.error(s"varargs parameter of @main method must come last", pos)
            valArgs ::: createValArgs(restpe, cmdName, idx + valArgs.length)
          case _ =>
            valArgs
        }
      }

    var result: List[ModuleDef] = Nil
    if (!mainFun.owner.isStaticOwner)
      report.error(s"@main method is not statically accessible", pos)
    else {
      val cmd = ValDef(
        cmdName,
        TypeTree(), // TODO check if good practice
        Apply(
          Ident(defn.MainAnnot_command.name),
          Ident(mainArgsName) :: Literal(Constant(mainFun.showName)) :: /* TODO Docstring */ Literal(Constant("")) :: Nil
        )
      )
      var args: List[ValDef] = Nil
      var mainCall: Tree = ref(mainFun.termRef)

      mainFun.info match {
        case _: ExprType =>
        case mt: MethodType =>
          args = createValArgs(mt, cmdName, 0)
          mainCall = Apply(mainCall, args map (arg => Apply(Ident(arg.name), Nil)))
        case _: PolyType =>
          report.error(s"@main method cannot have type parameters", pos)
        case _ =>
          report.error(s"@main can only annotate a method", pos)
      }

      val run = Apply(Select(Ident(cmdName), defn.MainAnnotCommand_run.name), mainCall)
      val body = Block(cmd :: args, run)
      val mainArg = ValDef(mainArgsName, TypeTree(defn.ArrayType.appliedTo(defn.StringType)), EmptyTree)
        .withFlags(Param)
      /** Replace typed `Ident`s that have been typed with a TypeSplice with the reference to the symbol.
       *  The annotations will be retype-checked in another scope that may not have the same imports.
       */
      def insertTypeSplices = new TreeMap {
          override def transform(tree: Tree)(using Context): Tree = tree match
            case tree: tpd.Ident @unchecked => TypedSplice(tree)
            case tree => super.transform(tree)
      }
      val annots = mainFun.annotations
        .filterNot(_.matches(defn.MainAnnot))
        .map(annot => insertTypeSplices.transform(annot.tree))
      val mainMeth = DefDef(nme.main, (mainArg :: Nil) :: Nil, TypeTree(defn.UnitType), body)
        //.withFlags(JavaStatic) // TODO check if necessary
        .withAnnotations(annots)
      val mainTempl = Template(emptyConstructor, TypeTree(defn.MainAnnot.typeRef) :: Nil, Nil, EmptyValDef, mainMeth :: Nil)
      val mainObj = ModuleDef(mainFun.name.toTermName, mainTempl)
      if (!ctx.reporter.hasErrors) result = mainObj.withSpan(mainAnnotSpan.toSynthetic) :: Nil
    }
    result
  }
}
