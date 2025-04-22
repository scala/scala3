package dotty.tools.dotc
package ast

import core.*
import Symbols.*, Types.*, Contexts.*, Decorators.*, util.Spans.*, Flags.*, Constants.*
import StdNames.{nme, tpnme}
import ast.Trees.*
import Names.Name
import Comments.Comment
import NameKinds.DefaultGetterName
import Annotations.Annotation

object MainProxies {

  /** Generate proxy classes for @main functions and @myMain functions where myMain <:< MainAnnotation */
  def proxies(stats: List[tpd.Tree])(using Context): List[untpd.Tree] = {
    mainAnnotationProxies(stats) ++ mainProxies(stats)
  }

  /** Generate proxy classes for @main functions.
   *  A function like
   *
   *     @main def f(x: S, ys: T*) = ...
   *
   *  would be translated to something like
   *
   *     import CommandLineParser.*
   *     class f {
   *       @static def main(args: Array[String]): Unit =
   *         try
   *           f(
   *             parseArgument[S](args, 0),
   *             parseRemainingArguments[T](args, 1): _*
   *           )
   *         catch case err: ParseError => showError(err)
   *       }
   */
  private def mainProxies(stats: List[tpd.Tree])(using Context): List[untpd.Tree] = {
    import tpd.*
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

  import untpd.*
  private def mainProxy(mainFun: Symbol)(using Context): List[TypeDef] = {
    val mainAnnotSpan = mainFun.getAnnotation(defn.MainAnnot).get.tree.span
    def pos = mainFun.sourcePos
    val argsRef = Ident(nme.args)

    def addArgs(call: untpd.Tree, mt: MethodType, idx: Int): untpd.Tree =
      if (mt.isImplicitMethod) {
        report.error(em"@main method cannot have implicit parameters", pos)
        call
      }
      else {
        val args = mt.paramInfos.zipWithIndex map {
          (formal, n) =>
            val (parserSym, formalElem) =
              if (formal.isRepeatedParam) (defn.CLP_parseRemainingArguments, formal.argTypes.head)
              else (defn.CLP_parseArgument, formal)
            val arg = Apply(
              TypeApply(ref(parserSym.termRef), TypeTree(formalElem) :: Nil),
              argsRef :: Literal(Constant(idx + n)) :: Nil)
            if (formal.isRepeatedParam) repeated(arg) else arg
        }
        val call1 = Apply(call, args)
        mt.resType match {
          case restpe: MethodType =>
            if (mt.paramInfos.lastOption.getOrElse(NoType).isRepeatedParam)
              report.error(em"varargs parameter of @main method must come last", pos)
            addArgs(call1, restpe, idx + args.length)
          case _ =>
            call1
        }
      }

    var result: List[TypeDef] = Nil
    if (!mainFun.owner.isStaticOwner)
      report.error(em"@main method is not statically accessible", pos)
    else {
      var call = ref(mainFun.termRef)
      mainFun.info match {
        case _: ExprType =>
        case mt: MethodType =>
          call = addArgs(call, mt, 0)
        case _: PolyType =>
          report.error(em"@main method cannot have type parameters", pos)
        case _ =>
          report.error(em"@main can only annotate a method", pos)
      }
      val errVar = Ident(nme.error)
      val handler = CaseDef(
        Typed(errVar, TypeTree(defn.CLP_ParseError.typeRef)),
        EmptyTree,
        Apply(ref(defn.CLP_showError.termRef), errVar :: Nil))
      val body = Try(call, handler :: Nil, EmptyTree)
      val mainArg = ValDef(nme.args, TypeTree(defn.ArrayType.appliedTo(defn.StringType)), EmptyTree)
        .withFlags(Param)

      /** This context is used to create the `TypeSplices` wrapping annotations
       *  below. These should have `mainFun` as their owner (and not the
       *  enclosing package class that we would get otherwise) so that
       *  subsequent owner changes (for example in `Typer.typedTypedSplice`) are
       *  correct. See #22364 and associated tests.
       */
      val annotsCtx = ctx.fresh.setOwner(mainFun)
      val annots = mainFun.annotations
        .filterNot(_.matches(defn.MainAnnot))
        .map(annot => TypedSplice(annot.tree)(using annotsCtx))
      val mainMeth = DefDef(nme.main, (mainArg :: Nil) :: Nil, TypeTree(defn.UnitType), body)
        .withFlags(JavaStatic | Synthetic)
        .withAnnotations(annots)
      val mainTempl = Template(emptyConstructor, Nil, Nil, EmptyValDef, mainMeth :: Nil)
      val mainCls = TypeDef(mainFun.name.toTypeName, mainTempl)
        .withFlags(Final | Invisible)

      if (!ctx.reporter.hasErrors)
        result = mainCls.withSpan(mainAnnotSpan.toSynthetic) :: Nil
    }
    result
  }

  private type DefaultValueSymbols = Map[Int, Symbol]
  private type ParameterAnnotationss = Seq[Seq[Annotation]]

  /**
   * Generate proxy classes for main functions.
   * A function like
   *
   *     /**
   *       * Lorem ipsum dolor sit amet
   *       * consectetur adipiscing elit.
   *       *
   *       * @param x my param x
   *       * @param ys all my params y
   *       */
   *     @myMain(80) def f(
   *       @myMain.Alias("myX") x: S,
   *       y: S,
   *       ys: T*
   *     ) = ...
   *
   *  would be translated to something like
   *
   *     final class f {
   *       static def main(args: Array[String]): Unit = {
   *         val annotation = new myMain(80)
   *         val info = new Info(
   *           name = "f",
   *           documentation = "Lorem ipsum dolor sit amet consectetur adipiscing elit.",
   *           parameters = Seq(
   *             new scala.annotation.MainAnnotation.Parameter("x", "S", false, false, "my param x", Seq(new scala.main.Alias("myX"))),
   *             new scala.annotation.MainAnnotation.Parameter("y", "S", true, false, "", Seq()),
   *             new scala.annotation.MainAnnotation.Parameter("ys", "T", false, true, "all my params y", Seq())
   *           )
   *         ),
   *         val command = annotation.command(info, args)
   *         if command.isDefined then
   *           val cmd = command.get
   *           val args0: () => S = annotation.argGetter[S](info.parameters(0), cmd(0), None)
   *           val args1: () => S = annotation.argGetter[S](info.parameters(1), mainArgs(1), Some(() => sum$default$1()))
   *           val args2: () => Seq[T] = annotation.varargGetter[T](info.parameters(2), cmd.drop(2))
   *           annotation.run(() => f(args0(), args1(), args2()*))
   *       }
   *     }
   */
  private def mainAnnotationProxies(stats: List[tpd.Tree])(using Context): List[untpd.Tree] = {
    import tpd.*

    /**
      * Computes the symbols of the default values of the function. Since they cannot be inferred anymore at this
      * point of the compilation, they must be explicitly passed by [[mainProxy]].
      */
    def defaultValueSymbols(scope: Tree, funSymbol: Symbol): DefaultValueSymbols =
      scope match {
        case TypeDef(_, template: Template) =>
          template.body.flatMap((_: Tree) match {
            case dd: DefDef if dd.name.is(DefaultGetterName) && dd.name.firstPart == funSymbol.name =>
              val DefaultGetterName.NumberedInfo(index) = dd.name.info: @unchecked
              List(index -> dd.symbol)
            case _ => Nil
          }).toMap
        case _ => Map.empty
      }

    /** Computes the list of main methods present in the code. */
    def mainMethods(scope: Tree, stats: List[Tree]): List[(Symbol, ParameterAnnotationss, DefaultValueSymbols, Option[Comment])] = stats.flatMap {
      case stat: DefDef =>
        val sym = stat.symbol
        sym.annotations.filter(_.matches(defn.MainAnnotationClass)) match {
          case Nil =>
            Nil
          case _ :: Nil =>
            val paramAnnotations = stat.paramss.flatMap(_.map(
              valdef => valdef.symbol.annotations.filter(_.matches(defn.MainAnnotationParameterAnnotation))
            ))
            (sym, paramAnnotations.toVector, defaultValueSymbols(scope, sym), stat.rawComment) :: Nil
          case mainAnnot :: others =>
            report.error(em"method cannot have multiple main annotations", mainAnnot.tree)
            Nil
        }
      case stat @ TypeDef(_, impl: Template) if stat.symbol.is(Module) =>
        mainMethods(stat, impl.body)
      case _ =>
        Nil
    }

    // Assuming that the top-level object was already generated, all main methods will have a scope
    mainMethods(EmptyTree, stats).flatMap(mainAnnotationProxy)
  }

  private def mainAnnotationProxy(mainFun: Symbol, paramAnnotations: ParameterAnnotationss, defaultValueSymbols: DefaultValueSymbols, docComment: Option[Comment])(using Context): Option[TypeDef] = {
    val mainAnnot = mainFun.getAnnotation(defn.MainAnnotationClass).get
    def pos = mainFun.sourcePos

    val documentation = new Documentation(docComment)

    /** () => value */
    def unitToValue(value: Tree): Tree =
      val defDef = DefDef(nme.ANON_FUN, List(Nil), TypeTree(), value)
      Block(defDef, Closure(Nil, Ident(nme.ANON_FUN), EmptyTree))

    /** Generate a list of trees containing the ParamInfo instantiations.
     *
     *  A ParamInfo has the following shape
     *  ```
     *  new scala.annotation.MainAnnotation.Parameter("x", "S", false, false, "my param x", Seq(new scala.main.Alias("myX")))
     *  ```
     */
    def parameterInfos(mt: MethodType): List[Tree] =
      extension (tree: Tree) def withProperty(sym: Symbol, args: List[Tree]) =
        Apply(Select(tree, sym.name), args)

      for ((formal, paramName), idx) <- mt.paramInfos.zip(mt.paramNames).zipWithIndex yield
        val param = paramName.toString
        val paramType0 = if formal.isRepeatedParam then formal.argTypes.head.dealias else formal.dealias
        val paramType = paramType0.dealias
        val paramTypeOwner = paramType.typeSymbol.owner
        val paramTypeStr =
          if paramTypeOwner == defn.EmptyPackageClass then paramType.show
          else paramTypeOwner.showFullName + "." + paramType.show
        val hasDefault = defaultValueSymbols.contains(idx)
        val isRepeated = formal.isRepeatedParam
        val paramDoc = documentation.argDocs.getOrElse(param, "")
        val paramAnnots =
          val annotationTrees = paramAnnotations(idx).map(instantiateAnnotation).toList
          Apply(ref(defn.SeqModule.termRef), annotationTrees)

        val constructorArgs = List(param, paramTypeStr, hasDefault, isRepeated, paramDoc)
          .map(value => Literal(Constant(value)))

        New(TypeTree(defn.MainAnnotationParameter.typeRef), List(constructorArgs :+ paramAnnots))

    end parameterInfos

    /**
      * Creates a list of references and definitions of arguments.
      * The goal is to create the
      *   `val args0: () => S = annotation.argGetter[S](0, cmd(0), None)`
      * part of the code.
      */
    def argValDefs(mt: MethodType): List[ValDef] =
      for ((formal, paramName), idx) <- mt.paramInfos.zip(mt.paramNames).zipWithIndex yield
        val argName = nme.args ++ idx.toString
        val isRepeated = formal.isRepeatedParam
        val formalType = if isRepeated then formal.argTypes.head else formal
        val getterName = if isRepeated then nme.varargGetter else nme.argGetter
        val defaultValueGetterOpt = defaultValueSymbols.get(idx) match
          case None => ref(defn.NoneModule.termRef)
          case Some(dvSym) =>
              val value = unitToValue(ref(dvSym.termRef))
              Apply(ref(defn.SomeClass.companionModule.termRef), value)
        val argGetter0 = TypeApply(Select(Ident(nme.annotation), getterName), TypeTree(formalType) :: Nil)
        val index = Literal(Constant(idx))
        val paramInfo = Apply(Select(Ident(nme.info), nme.parameters), index)
        val argGetter =
          if isRepeated then Apply(argGetter0, List(paramInfo, Apply(Select(Ident(nme.cmd), nme.drop), List(index))))
          else Apply(argGetter0, List(paramInfo, Apply(Ident(nme.cmd), List(index)), defaultValueGetterOpt))
        ValDef(argName, TypeTree(), argGetter)
    end argValDefs


    /** Create a list of argument references that will be passed as argument to the main method.
     *  `args0`, ...`argn*`
     */
    def argRefs(mt: MethodType): List[Tree] =
      for ((formal, paramName), idx) <- mt.paramInfos.zip(mt.paramNames).zipWithIndex yield
        val argRef = Apply(Ident(nme.args ++ idx.toString), Nil)
        if formal.isRepeatedParam then repeated(argRef) else argRef
    end argRefs


    /** Turns an annotation (e.g. `@main(40)`) into an instance of the class (e.g. `new scala.main(40)`). */
    def instantiateAnnotation(annot: Annotation): Tree =
      val argss = {
        def recurse(t: tpd.Tree, acc: List[List[Tree]]): List[List[Tree]] = t match {
          case Apply(t, args: List[tpd.Tree]) => recurse(t, extractArgs(args) :: acc)
          case _ => acc
        }

        def extractArgs(args: List[tpd.Tree]): List[Tree] =
          args.flatMap {
            case Typed(SeqLiteral(varargs, _), _) => varargs.map(arg => TypedSplice(arg))
            case arg: Select if arg.name.is(DefaultGetterName) => Nil  // Ignore default values, they will be added later by the compiler
            case arg => List(TypedSplice(arg))
          }

        recurse(annot.tree, Nil)
      }

      New(TypeTree(annot.symbol.typeRef), argss)
    end instantiateAnnotation

    def generateMainClass(mainCall: Tree, args: List[Tree], parameterInfos: List[Tree]): TypeDef =
      val cmdInfo =
        val nameTree = Literal(Constant(mainFun.showName))
        val docTree = Literal(Constant(documentation.mainDoc))
        val paramInfos = Apply(ref(defn.SeqModule.termRef), parameterInfos)
        New(TypeTree(defn.MainAnnotationInfo.typeRef), List(List(nameTree, docTree, paramInfos)))

      val annotVal = ValDef(
        nme.annotation,
        TypeTree(),
        instantiateAnnotation(mainAnnot)
      )
      val infoVal = ValDef(
        nme.info,
        TypeTree(),
        cmdInfo
      )
      val command = ValDef(
        nme.command,
        TypeTree(),
        Apply(
          Select(Ident(nme.annotation), nme.command),
          List(Ident(nme.info), Ident(nme.args))
        )
      )
      val argsVal = ValDef(
        nme.cmd,
        TypeTree(),
        Select(Ident(nme.command), nme.get)
      )
      val run = Apply(Select(Ident(nme.annotation), nme.run), mainCall)
      val body0 = If(
        Select(Ident(nme.command), nme.isDefined),
        Block(argsVal :: args, run),
        EmptyTree
      )
      val body = Block(List(annotVal, infoVal, command), body0) // TODO add `if (cmd.nonEmpty)`

      val mainArg = ValDef(nme.args, TypeTree(defn.ArrayType.appliedTo(defn.StringType)), EmptyTree)
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
        .filterNot(_.matches(defn.MainAnnotationClass))
        .map(annot => insertTypeSplices.transform(annot.tree))
      val mainMeth = DefDef(nme.main, (mainArg :: Nil) :: Nil, TypeTree(defn.UnitType), body)
        .withFlags(JavaStatic)
        .withAnnotations(annots)
      val mainTempl = Template(emptyConstructor, Nil, Nil, EmptyValDef, mainMeth :: Nil)
      val mainCls = TypeDef(mainFun.name.toTypeName, mainTempl)
        .withFlags(Final | Invisible)
      mainCls.withSpan(mainAnnot.tree.span.toSynthetic)
    end generateMainClass

    if (!mainFun.owner.isStaticOwner)
      report.error(em"main method is not statically accessible", pos)
      None
    else mainFun.info match {
      case _: ExprType =>
        Some(generateMainClass(unitToValue(ref(mainFun.termRef)), Nil, Nil))
      case mt: MethodType =>
        if (mt.isImplicitMethod)
          report.error(em"main method cannot have implicit parameters", pos)
          None
        else mt.resType match
          case restpe: MethodType =>
            report.error(em"main method cannot be curried", pos)
            None
          case _ =>
            Some(generateMainClass(unitToValue(Apply(ref(mainFun.termRef), argRefs(mt))), argValDefs(mt), parameterInfos(mt)))
      case _: PolyType =>
        report.error(em"main method cannot have type parameters", pos)
        None
      case _ =>
        report.error(em"main can only annotate a method", pos)
        None
    }
  }

  /** A class responsible for extracting the docstrings of a method. */
  private class Documentation(docComment: Option[Comment]):
    import util.CommentParsing.*

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
      var lines: Seq[String] = raw.trim.nn.split('\n').nn.toSeq
      lines = lines.map(l => l.substring(skipLineLead(l, -1), l.length).nn.trim.nn)
      var s = lines.foldLeft("") {
        case ("", s2) => s2
        case (s1, "") if s1.last == '\n' => s1 // Multiple newlines are kept as single newlines
        case (s1, "") => s1 + '\n'
        case (s1, s2) if s1.last == '\n' => s1 + s2
        case (s1, s2) => s1 + ' ' + s2
      }
      s.replaceAll(raw"\[\[", "").nn.replaceAll(raw"\]\]", "").nn.trim.nn

    private def parseDocComment(raw: String): Unit =
      // Positions of the sections (@) in the docstring
      val tidx: List[(Int, Int)] = tagIndex(raw)

      // Parse main comment
      var mainComment: String = raw.substring(skipLineLead(raw, 0), startTag(raw, tidx)).nn
      _mainDoc = cleanComment(mainComment)

      // Parse arguments comments
      val argsCommentsSpans: Map[String, (Int, Int)] = paramDocs(raw, "@param", tidx)
      val argsCommentsTextSpans = argsCommentsSpans.view.mapValues(extractSectionText(raw, _))
      val argsCommentsTexts = argsCommentsTextSpans.mapValues({ case (beg, end) => raw.substring(beg, end).nn })
      _argDocs = argsCommentsTexts.mapValues(cleanComment(_)).toMap
  end Documentation
}
