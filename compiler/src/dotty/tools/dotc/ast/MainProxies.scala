package dotty.tools.dotc
package ast

import core._
import Symbols._, Types._, Contexts._, Decorators._, util.Spans._, Flags._, Constants._
import StdNames.{nme, tpnme}
import ast.Trees._
import Names.TermName
import Comments.Comment

/** Generate proxy classes for @main functions.
 *  A function like
 *
 *     /**
 *       * Lorem ipsum dolor sit amet
 *       * consectetur adipiscing elit.
 *       *
 *       * @param x my param x
 *       * @param ys all my params y
 *       */
 *     @main def f(x: S, ys: T*) = ...
 *
 *  would be translated to something like
 *
 *     object f extends main {
 *       @static def main(args: Array[String]): Unit =
 *         val cmd = command(args, "f", "Lorem ipsum dolor sit amet consectetur adipiscing elit.")
 *         val arg1: () => S = cmd.argGetter[S]("x", "S", "my param x")
 *         val arg2: () => Seq[T] = cmd.argsGetter[T]("ys", "T", "all my params y")
 *         cmd.run(f(arg1(), arg2()*))
 *     }
 */
object MainProxies {

  def mainProxies(stats: List[tpd.Tree])(using Context): List[untpd.Tree] = {
    import tpd._
    def mainMethods(stats: List[Tree]): List[(Symbol, Option[Comment])] = stats.flatMap {
      case stat: DefDef if stat.symbol.hasAnnotation(defn.MainAnnot) =>
        (stat.symbol, stat.rawComment) :: Nil
      case stat @ TypeDef(name, impl: Template) if stat.symbol.is(Module) =>
        mainMethods(impl.body)
      case _ =>
        Nil
    }
    mainMethods(stats).flatMap(mainProxy)
  }

  import untpd._
  def mainProxy(mainFun: Symbol, docComment: Option[Comment])(using Context): List[ModuleDef] = {
    val mainAnnotSpan = mainFun.getAnnotation(defn.MainAnnot).get.tree.span
    def pos = mainFun.sourcePos
    val mainArgsName: TermName = nme.args
    val cmdName: TermName = Names.termName("cmd")

    val documentation = new Documentation(docComment)

    def createValArgs(mt: MethodType, cmdName: TermName, idx: Int): List[(ValDef, Boolean)] =
      if (mt.isImplicitMethod) {
        report.error(s"@main method cannot have implicit parameters", pos)
        Nil
      }
      else {
        // TODO check & handle default value
        var valArgs: List[(ValDef, Boolean)] = mt.paramInfos.zip(mt.paramNames).zipWithIndex map {
          case ((formal, paramName), n) =>
            // TODO make me cleaner
            val (getterSym, formalType, isVararg) =
              if (formal.isRepeatedParam) (defn.MainAnnotCommand_argsGetter, formal.argTypes.head, true)
              else (defn.MainAnnotCommand_argGetter, formal, false)
            val returnType = if isVararg then defn.SeqType.appliedTo(formalType) else formalType
            val valDef = ValDef(
              mainArgsName ++ (idx + n).toString, // FIXME
              TypeTree(defn.FunctionOf(Nil, returnType)),
              Apply(
                TypeApply(Select(Ident(cmdName), getterSym.name), TypeTree(formalType) :: Nil),
                Literal(Constant(paramName.toString))
                :: Literal(Constant(formalType.show))
                :: Literal(Constant(documentation.argDocs(paramName.toString)))
                :: Nil  // TODO check if better way to print name of formalType
              ),
            )
            (valDef, isVararg)
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
          Ident(mainArgsName) :: Literal(Constant(mainFun.showName)) :: Literal(Constant(documentation.mainDoc)) :: Nil
        )
      )
      var args: List[ValDef] = Nil
      var mainCall: Tree = ref(mainFun.termRef)

      mainFun.info match {
        case _: ExprType =>
        case mt: MethodType =>
          val valArgs = createValArgs(mt, cmdName, 0)
          args = valArgs.unzip._1
          mainCall = Apply(mainCall, valArgs map {
            case (arg, isVararg) =>
              var argCall: Tree = Apply(Ident(arg.name), Nil)
              if isVararg then argCall = repeated(argCall)
              argCall
          })
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

  private class Documentation(docComment: Option[Comment], val maxLineLength: Int = 120):
    import util.CommentParsing._

    /** The main part of the documentation. */
    lazy val mainDoc: String = _mainDoc
    /** The parameters identified by @param. Maps from param name to documentation. */
    lazy val argDocs: Map[String, String] = _argDocs

    private var _mainDoc: String = ""
    private var _argDocs: Map[String, String] = Map().withDefaultValue("")

    docComment match {
      case Some(comment) => if comment.isDocComment then parseDocComment(comment.raw) else _mainDoc = comment.raw
      case None =>
    }

    private def breakLongLines(s: String): String =
      // TODO
      s

    private def cleanComment(raw: String): String =
      var lines: Seq[String] = raw.trim.split('\n').toSeq
      lines = lines.map(l => l.substring(skipLineLead(l, -1), l.length).trim)
      var s = lines.foldLeft("") {
        case ("", s2) => s2
        case (s1, "") if s1.last == '\n' => s1 // Multiple newlines are kept as single newlines
        case (s1, "") => s1 + '\n'
        case (s1, s2) => s1 + ' ' + s2
      }
      s.trim

    private def parseDocComment(raw: String): Unit =
      // Positions of the sections (@) in the docstring
      val tidx: List[(Int, Int)] = tagIndex(raw)

      // Parse main comment
      var mainComment: String = raw.substring(skipLineLead(raw, 0), startTag(raw, tidx))
      mainComment = cleanComment(mainComment)
      _mainDoc = breakLongLines(mainComment)

      // Parse arguments comments
      val argsCommentsSpans: Map[String, (Int, Int)] = paramDocs(raw, "@param", tidx)
      val argsCommentsTextSpans = argsCommentsSpans.view.mapValues(extractSectionText(raw, _))
      val argsCommentsTexts = argsCommentsTextSpans.mapValues({ case (beg, end) => raw.substring(beg, end) })
      _argDocs = argsCommentsTexts.mapValues(cleanComment(_)).mapValues(breakLongLines(_)).toMap.withDefaultValue("")
  end Documentation
}
