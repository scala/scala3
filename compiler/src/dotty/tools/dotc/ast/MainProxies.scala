package dotty.tools.dotc
package ast

import core._
import Symbols._, Types._, Contexts._, Decorators._, util.Spans._, Flags._, Constants._
import StdNames.{nme, tpnme}
import ast.Trees._
import Names.{Name, TermName}
import Comments.Comment
import NameKinds.DefaultGetterName

/** Generate proxy classes for main functions.
 *  A function like
 *
 *     /**
 *       * Lorem ipsum dolor sit amet
 *       * consectetur adipiscing elit.
 *       *
 *       * @param x my param x
 *       * @param ys all my params y
 *       */
 *     @main(80) def f(x: S, ys: T*) = ...
 *
 *  would be translated to something like
 *
 *     final class f {
 *       @static def main(args: Array[String]): Unit =
 *         val cmd: MainAnnotation#Command[..., ...] =
 *           (new scala.main(80)).command(args, "f", "Lorem ipsum dolor sit amet consectetur adipiscing elit.")
 *         val arg1: () => S = cmd.argGetter[S]("x", "S", "my param x")
 *         val arg2: () => Seq[T] = cmd.argsGetter[T]("ys", "T", "all my params y")
 *         cmd.run(f(arg1(), arg2()*))
 *     }
 */
object MainProxies {

  def mainProxies(stats: List[tpd.Tree])(using Context): List[untpd.Tree] = {
    import tpd._

    def defaultValues(scope: Tree, funSymbol: Symbol): Map[Int, Tree] =
      scope match {
        case TypeDef(_, template: Template) =>
          template.body.flatMap((_: Tree) match {
            case dd @ DefDef(name, _, _, _) if name.is(DefaultGetterName) && name.firstPart == funSymbol.name =>
              val index: Int = name.toString.split("\\$").last.toInt - 1 // FIXME please!!
              val valueTree = dd.rhs
              List(index -> valueTree)
            case _ => List()
          }).toMap
        case _ => Map[Int, Tree]()
      }

    def mainMethods(scope: Tree, stats: List[Tree]): List[(Symbol, Map[Int, Tree], Option[Comment])] = stats.flatMap {
      case stat: DefDef =>
        val sym = stat.symbol
        sym.annotations.filter(_.matches(defn.MainAnnot)) match {
          case Nil => Nil
          case _ :: Nil => (sym, defaultValues(scope, sym), stat.rawComment) :: Nil
          case mainAnnot :: others =>
            report.error(s"method cannot have multiple main annotations", mainAnnot.tree)
            Nil
        }
      case stat @ TypeDef(name, impl: Template) if stat.symbol.is(Module) =>
        mainMethods(stat, impl.body)
      case _ =>
        Nil
    }

    // Assuming that the top-level object was already generated, all main methods will have a scope
    mainMethods(EmptyTree, stats).flatMap(mainProxy)
  }

  import untpd._
  def mainProxy(mainFun: Symbol, defaultValues: Map[Int, Tree], docComment: Option[Comment])(using Context): List[TypeDef] = {
    val mainAnnot = mainFun.getAnnotation(defn.MainAnnot).get
    def pos = mainFun.sourcePos
    val mainArgsName: TermName = nme.args
    val cmdName: TermName = Names.termName("cmd")

    val documentation = new Documentation(docComment)

    inline def lit(any: Any): Literal = Literal(Constant(any))

    def createArgs(mt: MethodType, cmdName: TermName): List[(Tree, ValDef)] =
      if (mt.isImplicitMethod) {
        report.error(s"main method cannot have implicit parameters", pos)
        Nil
      }
      else {
        var valArgs: List[(Tree, ValDef)] = mt.paramInfos.zip(mt.paramNames).zipWithIndex.map {
          case ((formal, paramName), n) =>
            val argName = mainArgsName ++ n.toString

            val isRepeated = formal.isRepeatedParam
            val hasDefaultValue = defaultValues.contains(n)

            var argRef: Tree = Apply(Ident(argName), Nil)
            var formalType = formal
            if isRepeated then
              argRef = repeated(argRef)
              formalType = formalType.argTypes.head

            val getterSym =
              if isRepeated then
                defn.MainAnnotCommand_argsGetter
              else if hasDefaultValue then
                defn.MainAnnotCommand_argGetterDefault
              else
                defn.MainAnnotCommand_argGetter

            val getterArgs = {
              val param = paramName.toString
              val optionDefaultValueTree = defaultValues.get(n)

              lit(param)
              :: lit(formalType.show)
              :: lit(documentation.argDocs(param))
              :: optionDefaultValueTree.map(List(_)).getOrElse(Nil)
            }

            val argDef = ValDef(
              argName,
              TypeTree(),
              Apply(TypeApply(Select(Ident(cmdName), getterSym.name), TypeTree(formalType) :: Nil), getterArgs),
            )

            (argRef, argDef)
        }
        mt.resType match {
          case restpe: MethodType =>
            report.error(s"main method cannot be curried", pos)
            Nil
          case _ =>
            valArgs
        }
      }

    var result: List[TypeDef] = Nil
    if (!mainFun.owner.isStaticOwner)
      report.error(s"main method is not statically accessible", pos)
    else {
      val mainAnnotArgs = mainAnnot.tree match {
        case Apply(_, namedArgs) => namedArgs.filter {
          case Select(_, name) => !name.is(DefaultGetterName)  // Remove default arguments of main
          case _ => true
        }
      }
      val cmd = ValDef(
        cmdName,
        TypeTree(),
        Apply(
          Select(New(TypeTree(mainAnnot.symbol.typeRef), List(mainAnnotArgs)), defn.MainAnnot_command.name),
          Ident(mainArgsName) :: lit(mainFun.showName) :: lit(documentation.mainDoc) :: Nil
        )
      )
      var args: List[ValDef] = Nil
      var mainCall: Tree = ref(mainFun.termRef)

      mainFun.info match {
        case _: ExprType =>
        case mt: MethodType =>
          val (argRefs, argVals) = createArgs(mt, cmdName).unzip
          args = argVals
          mainCall = Apply(mainCall, argRefs)
        case _: PolyType =>
          report.error(s"main method cannot have type parameters", pos)
        case _ =>
          report.error(s"main can only annotate a method", pos)
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
        .withFlags(JavaStatic)
        .withAnnotations(annots)
      val mainTempl = Template(emptyConstructor, Nil, Nil, EmptyValDef, mainMeth :: Nil)
      val mainCls = TypeDef(mainFun.name.toTypeName, mainTempl)
        .withFlags(Final | Invisible)
      if (!ctx.reporter.hasErrors) result = mainCls.withSpan(mainAnnot.tree.span.toSynthetic) :: Nil
    }
    result
  }

  private class Documentation(docComment: Option[Comment]):
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

    private def cleanComment(raw: String): String =
      var lines: Seq[String] = raw.trim.split('\n').toSeq
      lines = lines.map(l => l.substring(skipLineLead(l, -1), l.length).trim)
      var s = lines.foldLeft("") {
        case ("", s2) => s2
        case (s1, "") if s1.last == '\n' => s1 // Multiple newlines are kept as single newlines
        case (s1, "") => s1 + '\n'
        case (s1, s2) if s1.last == '\n' => s1 + s2
        case (s1, s2) => s1 + ' ' + s2
      }
      s.replaceAll(raw"\{\{", "").replaceAll(raw"\}\}", "").trim

    private def parseDocComment(raw: String): Unit =
      // Positions of the sections (@) in the docstring
      val tidx: List[(Int, Int)] = tagIndex(raw)

      // Parse main comment
      var mainComment: String = raw.substring(skipLineLead(raw, 0), startTag(raw, tidx))
      _mainDoc = cleanComment(mainComment)

      // Parse arguments comments
      val argsCommentsSpans: Map[String, (Int, Int)] = paramDocs(raw, "@param", tidx)
      val argsCommentsTextSpans = argsCommentsSpans.view.mapValues(extractSectionText(raw, _))
      val argsCommentsTexts = argsCommentsTextSpans.mapValues({ case (beg, end) => raw.substring(beg, end) })
      _argDocs = argsCommentsTexts.mapValues(cleanComment(_)).toMap.withDefaultValue("")
  end Documentation
}
