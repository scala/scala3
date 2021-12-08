package dotty.tools.dotc
package ast

import core._
import Symbols._, Types._, Contexts._, Decorators._, util.Spans._, Flags._, Constants._
import StdNames.{nme, tpnme}
import ast.Trees._
import Names.{Name, TermName}
import Comments.Comment
import NameKinds.DefaultGetterName
import Annotations.Annotation

/** Generate proxy classes for main functions.
  * A function like
  *
  *     /**
  *       * Lorem ipsum dolor sit amet
  *       * consectetur adipiscing elit.
  *       *
  *       * @param x my param x
  *       * @param ys all my params y
  *       */
  *     @main(80) def f(@main.arg(shortName = 'x', name = "myX") x: S, ys: T*) = ...
  *
  *  would be translated to something like
  *
  *     final class f {
  *       static def main(args: Array[String]): Unit = {
  *         val cmd = new main(80).command(args, "f", "Lorem ipsum dolor sit amet consectetur adipiscing elit.")
  *
  *         val args0: () => S = cmd.argGetter[S]({
  *           val args0paramInfos = new scala.annotation.MainAnnotation.ParameterInfos[S]("x", "S")
  *           args0paramInfos.documentation = Some("my param x")
  *           args0paramInfos.annotation = Some(new scala.main.arg(name = "myX", shortName = 'x'))
  *           args0paramInfos
  *         })(util.CommandLineParser.FromString.given_FromString_Int)
  *
  *         val args1: () => Seq[T] = cmd.varargGetter[T]({
  *           val args1paramInfos = new scala.annotation.MainAnnotation.ParameterInfos[T]("ys", "T")
  *           args1paramInfos.documentation = Some("all my params y")
  *           args1paramInfos
  *         })(util.CommandLineParser.FromString.given_FromString_String)
  *
  *         cmd.run(f(args0.apply(), args1.apply()*))
  *       }
  *     }
  */
object MainProxies {
  private type DefaultValueSymbols = Map[Int, Symbol]
  private type ParameterAnnotationss = Seq[Seq[Annotation]]

  def mainProxies(stats: List[tpd.Tree])(using Context): List[untpd.Tree] = {
    import tpd._

    /**
      * Computes the symbols of the default values of the function. Since they cannot be infered anymore at this
      * point of the compilation, they must be explicitely passed by [[mainProxy]].
      */
    def defaultValueSymbols(scope: Tree, funSymbol: Symbol): DefaultValueSymbols =
      scope match {
        case TypeDef(_, template: Template) =>
          template.body.flatMap((_: Tree) match {
            case dd @ DefDef(name, _, _, _) if name.is(DefaultGetterName) && name.firstPart == funSymbol.name =>
              val index: Int = name.toString.split("\\$").last.toInt - 1 // FIXME please!!
              List(index -> dd.symbol)
            case _ => List()
          }).toMap
        case _ => Map.empty
      }

    /** Computes the list of main methods present in the code. */
    def mainMethods(scope: Tree, stats: List[Tree]): List[(Symbol, ParameterAnnotationss, DefaultValueSymbols, Option[Comment])] = stats.flatMap {
      case stat: DefDef =>
        val sym = stat.symbol
        sym.annotations.filter(_.matches(defn.MainAnnot)) match {
          case Nil =>
            Nil
          case _ :: Nil =>
            val paramAnnotations = stat.paramss.flatMap(_.map(
              valdef => valdef.symbol.annotations.filter(_.matches(defn.MainAnnotParameterAnnotation))
            ))
            (sym, paramAnnotations.toVector, defaultValueSymbols(scope, sym), stat.rawComment) :: Nil
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
  def mainProxy(mainFun: Symbol, paramAnnotations: ParameterAnnotationss, defaultValueSymbols: DefaultValueSymbols, docComment: Option[Comment])(using Context): List[TypeDef] = {
    val mainAnnot = mainFun.getAnnotation(defn.MainAnnot).get
    def pos = mainFun.sourcePos
    val mainArgsName: TermName = nme.args
    val cmdName: TermName = Names.termName("cmd")

    val documentation = new Documentation(docComment)

    inline def lit(any: Any): Literal = Literal(Constant(any))

    inline def some(value: Tree): Tree = Apply(ref(defn.SomeClass.companionModule.termRef), value)

    def unitToValue(value: Tree): Tree =
      val anonName = nme.ANON_FUN
      val defdef = DefDef(anonName, List(Nil), TypeTree(), value)
      Block(defdef, Closure(Nil, Ident(anonName), EmptyTree))

    /**
      * Creates a list of references and definitions of arguments, the first referencing the second.
      * The goal is to create the
      *   `val arg0: () => S = ...`
      * part of the code. The first element of the tuple is a ref to `arg0`, the second is the whole definition.
      */
    def createArgs(mt: MethodType, cmdName: TermName): List[(Tree, ValDef)] =
      mt.paramInfos.zip(mt.paramNames).zipWithIndex.map {
        case ((formal, paramName), n) =>
          val argName = mainArgsName ++ n.toString

          val isRepeated = formal.isRepeatedParam

          var argRef: Tree = Apply(Ident(argName), Nil)
          var formalType = formal
          if isRepeated then
            argRef = repeated(argRef)
            formalType = formalType.argTypes.head

          val getterSym =
            if isRepeated then
              defn.MainAnnotCommand_varargGetter
            else
              defn.MainAnnotCommand_argGetter

          // The ParameterInfos to be passed to the arg getter
          val parameterInfos = {
            val param = paramName.toString
            val paramInfosName = argName ++ "paramInfos"
            val paramInfosIdent = Ident(paramInfosName)
            val paramInfosTree = New(
              AppliedTypeTree(TypeTree(defn.MainAnnotParameterInfos.typeRef), List(TypeTree(formalType))),
              // Arguments to be passed to ParameterInfos' constructor
              List(List(lit(param), lit(formalType.show)))
            )

            /*
             * Assignations to be made after the creation of the ParameterInfos.
             * For example:
             *   args0paramInfos.withDocumentation = Some("my param x")
             * is represented by the pair
             *   (defn.MainAnnotationParameterInfos_withDocumentation, some(lit("my param x")))
             */
            var assignations: List[(Symbol, List[Tree])] = Nil
            for (dvSym <- defaultValueSymbols.get(n))
              assignations = (defn.MainAnnotationParameterInfos_withDefaultValue -> List(unitToValue(ref(dvSym.termRef)))) :: assignations
            for (doc <- documentation.argDocs.get(param))
              assignations = (defn.MainAnnotationParameterInfos_withDocumentation -> List(lit(doc))) :: assignations

            val instanciatedAnnots = paramAnnotations(n).map(instanciateAnnotation).toList
            if instanciatedAnnots.nonEmpty then
              assignations = (defn.MainAnnotationParameterInfos_withAnnotations -> instanciatedAnnots) :: assignations

            if assignations.isEmpty then
              paramInfosTree
            else
              assignations.foldLeft[Tree](paramInfosTree){ case (tree, (setterSym, values)) => Apply(Select(tree, setterSym.name), values) }
          }

          val argDef = ValDef(
            argName,
            TypeTree(),
            Apply(TypeApply(Select(Ident(cmdName), getterSym.name), TypeTree(formalType) :: Nil), parameterInfos),
          )

          (argRef, argDef)
      }
    end createArgs

    /** Turns an annotation (e.g. `@main(40)`) into an instance of the class (e.g. `new scala.main(40)`). */
    def instanciateAnnotation(annot: Annotation): Tree =
      val argss = {
        def recurse(t: Tree, acc: List[List[Tree]]): List[List[Tree]] = t match {
          case Apply(t, args: List[Tree]) => recurse(t, extractArgs(args) :: acc)
          case _ => acc
        }

        def extractArgs(args: List[Tree]): List[Tree] =
          args.flatMap {
            case Typed(SeqLiteral(varargs, _), _) => varargs
            case arg @ Select(_, name) => if name.is(DefaultGetterName) then List() else List(arg)
            case arg => List(arg)
          }

        recurse(annot.tree, Nil)
      }

      New(TypeTree(annot.symbol.typeRef), argss)
    end instanciateAnnotation

    var result: List[TypeDef] = Nil
    if (!mainFun.owner.isStaticOwner)
      report.error(s"main method is not statically accessible", pos)
    else {
      val cmd = ValDef(
        cmdName,
        TypeTree(),
        Apply(
          Select(instanciateAnnotation(mainAnnot), defn.MainAnnot_command.name),
          Ident(mainArgsName) :: lit(mainFun.showName) :: lit(documentation.mainDoc) :: Nil
        )
      )
      var args: List[ValDef] = Nil
      var mainCall: Tree = ref(mainFun.termRef)

      mainFun.info match {
        case _: ExprType =>
        case mt: MethodType =>
          if (mt.isImplicitMethod) {
            report.error(s"main method cannot have implicit parameters", pos)
          }
          else mt.resType match {
            case restpe: MethodType =>
              report.error(s"main method cannot be curried", pos)
              Nil
            case _ =>
              val (argRefs, argVals) = createArgs(mt, cmdName).unzip
              args = argVals
              mainCall = Apply(mainCall, argRefs)
          }
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

  /** A class responsible for extracting the docstrings of a method. */
  private class Documentation(docComment: Option[Comment]):
    import util.CommentParsing._

    /** The main part of the documentation. */
    lazy val mainDoc: String = _mainDoc
    /** The parameters identified by @param. Maps from parameter name to its documentation. */
    lazy val argDocs: Map[String, String] = _argDocs

    private var _mainDoc: String = ""
    private var _argDocs: Map[String, String] = Map()

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
      s.replaceAll(raw"\[\[", "").replaceAll(raw"\]\]", "").trim

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
      _argDocs = argsCommentsTexts.mapValues(cleanComment(_)).toMap
  end Documentation
}
