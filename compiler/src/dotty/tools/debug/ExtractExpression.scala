package dotty.tools.debug

import dotty.tools.dotc.ast.tpd.*
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.transform.MacroTransform
import dotty.tools.dotc.core.Phases.*
import dotty.tools.dotc.report
/**
  * This phase extracts the typed expression from the source tree, transforms it and places it
  * in the evaluate method of the Expression class.
  * 
  * Before:
  *   package example:
  *     class A:
  *       def m: T =
  *         val expression =
  *           println("")
  *           typed_expr
  *         body
  *     
  *     class Expression(thisObject: Any, names: Array[String], values: Array[Any]):
  *       def evaluate(): Any = ()
  * 
  * After:
  *   package example:
  *     class A:
  *       def m: T = body
  *     
  *     class Expression(thisObject: Any, names: Array[String], values: Array[Any]):
  *       def evaluate(): Any = 
            {
  *           transformed_expr
  *         }
  * 
  * Every access to a local variable, or an inaccessible member is transformed into a temporary reflectEval call.
  * A ReflectEvalStrategy is attached to each reflectEval call to describe what should be evaluated and how.
  * When printing trees for debugging, the ReflectEvalStrategy appears as a String literal argument.
  * 
  * Examples:
  *
  * 1. Get local variable `a`:
  *      reflectEval(null, "ReflectEvalStrategy.LocalValue(a)", [])
  * 
  * 2. Call private method `a.m(x1, x2)`:
  *      reflectEval(a, "ReflectEvalStrategy.MethodCall(m)", [x1, x2])
  *
  * 3. Set private field `a.b = c`:
  *      reflectEval(a, "ReflectEvalStrategy.FieldAssign(b)", [c])
  * 
  * etc
  *
  */
private class ExtractExpression(
    config: ExpressionCompilerConfig,
    expressionStore: ExpressionStore
) extends MacroTransform with DenotTransformer:
  override def phaseName: String = ExtractExpression.name

  /** Update the owner of the symbols inserted into `evaluate`. */
  override def transform(ref: SingleDenotation)(using Context): SingleDenotation =
    ref match
      case ref: SymDenotation if isExpressionVal(ref.symbol.maybeOwner) =>
        // update owner of the symDenotation, e.g. local vals
        // that are extracted out of the expression val to the evaluate method
        ref.copySymDenotation(owner = config.evaluateMethod)
      case _ =>
        ref

  override def transformPhase(using Context): Phase = this.next

  override protected def newTransformer(using Context): Transformer = new ExtractExpressionTransformer

  private class ExtractExpressionTransformer extends Transformer:
    private var expressionTree: Tree | Null = null
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case PackageDef(pid, stats) =>
          val (evaluationClassDef, others) = stats.partition(_.symbol == config.expressionClass)
          val transformedStats = (others ++ evaluationClassDef).map(transform)
          cpy.PackageDef(tree)(pid, transformedStats)
        case tree: ValDef if isExpressionVal(tree.symbol) =>
          expressionTree = tree.rhs
          expressionStore.store(tree.symbol)
          unitLiteral
        case tree: DefDef if tree.symbol == config.evaluateMethod =>
          val transformedExpr = ExpressionTransformer.transform(expressionTree.nn)
          cpy.DefDef(tree)(rhs = transformedExpr)
        case tree =>
          super.transform(tree)

  private object ExpressionTransformer extends TreeMap:
    override def transform(tree: Tree)(using Context): Tree =
      tree match
        case tree: ImportOrExport => tree
        case tree if isLocalToExpression(tree.symbol) => super.transform(tree)

        // static object
        case tree: (Ident | Select | This) if isStaticObject(tree.symbol) =>
          getStaticObject(tree)(tree.symbol.moduleClass)

        // non-static this or outer this
        case tree: This if !tree.symbol.is(Package) =>
          thisOrOuterValue(tree)(tree.symbol.enclosingClass.asClass)

        // non-static object
        case tree: (Ident | Select) if isNonStaticObject(tree.symbol) =>
          callMethod(tree)(getTransformedQualifier(tree), tree.symbol.asTerm, Nil)

        // local variable
        case tree: Ident if isLocalVariable(tree.symbol) =>
          if tree.symbol.is(Lazy) then
            report.error(s"Evaluation of local lazy val not supported", tree.srcPos)
            tree
          else
            getCapturer(tree.symbol.asTerm) match
              case Some(capturer) =>
                if capturer.isClass then getClassCapture(tree)(tree.symbol, capturer.asClass)
                else getMethodCapture(tree)(tree.symbol, capturer.asTerm)
              case None => getLocalValue(tree)(tree.symbol)

        // assignment to local variable
        case tree @ Assign(lhs, _) if isLocalVariable(lhs.symbol) =>
          val variable = lhs.symbol.asTerm
          val rhs = transform(tree.rhs)
          getCapturer(variable) match
            case Some(capturer) =>
              if capturer.isClass then setClassCapture(tree)(variable, capturer.asClass, rhs)
              else setMethodCapture(tree)(variable, capturer.asTerm, rhs)
            case None => setLocalValue(tree)(variable, rhs)

        // inaccessible fields
        case tree: (Ident | Select) if tree.symbol.isField && !isAccessibleMember(tree) =>
          if tree.symbol.is(JavaStatic) then getField(tree)(nullLiteral, tree.symbol.asTerm)
          else getField(tree)(getTransformedQualifier(tree), tree.symbol.asTerm)

        // assignment to inaccessible fields
        case tree @ Assign(lhs, rhs) if lhs.symbol.isField && !isAccessibleMember(lhs) =>
          if lhs.symbol.is(JavaStatic) then setField(tree)(nullLiteral, lhs.symbol.asTerm, transform(rhs))
          else setField(tree)(getTransformedQualifier(lhs), lhs.symbol.asTerm, transform(rhs))

        // inaccessible constructors
        case tree: (Select | Apply | TypeApply)
            if tree.symbol.isConstructor && (!tree.symbol.owner.isStatic || !isAccessibleMember(tree)) =>
          callConstructor(tree)(getTransformedQualifierOfNew(tree), tree.symbol.asTerm, getTransformedArgs(tree))

        // inaccessible methods
        case tree: (Ident | Select | Apply | TypeApply) if tree.symbol.isRealMethod && !isAccessibleMember(tree) =>
          val args = getTransformedArgs(tree)
          if tree.symbol.is(JavaStatic) then callMethod(tree)(nullLiteral, tree.symbol.asTerm, args)
          else callMethod(tree)(getTransformedQualifier(tree), tree.symbol.asTerm, args)

        // accessible members
        case tree: (Ident | Select) if !tree.symbol.isStatic =>
          val qualifier = getTransformedQualifier(tree)
          val qualifierType = widenDealiasQualifierType(tree)
          val castQualifier = if qualifier.tpe <:< qualifierType then qualifier else
            qualifier.select(defn.Any_asInstanceOf).appliedToType(qualifierType)
          cpy.Select(tree)(castQualifier, tree.name)

        case Typed(tree, tpt) if tpt.symbol.isType && !isTypeAccessible(tpt.tpe) => transform(tree)
        case tree => super.transform(tree)

    private def getCapturer(variable: TermSymbol)(using Context): Option[Symbol] =
      // a local variable can be captured by a class or method
      val candidates = expressionStore.symbol.nn.ownersIterator
        .takeWhile(_ != variable.owner)
        .filter(s => s.isClass || s.is(Method))
        .toSeq
      candidates
        .findLast(_.isClass)
        .orElse(candidates.find(_.is(Method)))

    private def getTransformedArgs(tree: Tree)(using Context): List[Tree] =
      tree match
        case _: (Ident | Select) => Nil
        case Apply(fun, args) => getTransformedArgs(fun) ++ args.map(transform)
        case TypeApply(fun, _) => getTransformedArgs(fun)

    private def getTransformedQualifier(tree: Tree)(using Context): Tree =
      tree match
        case Ident(_) =>
          tree.tpe match
            case TermRef(NoPrefix, _) =>
              // it's a local method, it can capture its outer value
              thisOrOuterValue(tree)(tree.symbol.enclosingClass.asClass)
            case TermRef(prefix: NamedType, _) => transform(ref(prefix))
            case TermRef(prefix: ThisType, _) => transform(This(prefix.cls))
        case Select(qualifier, _) => transform(qualifier)
        case Apply(fun, _) => getTransformedQualifier(fun)
        case TypeApply(fun, _) => getTransformedQualifier(fun)

    private def getTransformedQualifierOfNew(tree: Tree)(using Context): Tree =
      tree match
        case Select(New(tpt), _) => getTransformedPrefix(tpt)
        case Apply(fun, _) => getTransformedQualifierOfNew(fun)
        case TypeApply(fun, _) => getTransformedQualifierOfNew(fun)

    private def getTransformedPrefix(typeTree: Tree)(using Context): Tree =
      def transformPrefix(prefix: Type): Tree =
        prefix match
          case NoPrefix =>
            // it's a local class, it can capture its outer value
            thisOrOuterValue(typeTree)(typeTree.symbol.owner.enclosingClass.asClass)
          case prefix: ThisType => thisOrOuterValue(typeTree)(prefix.cls)
          case ref: TermRef => transform(Ident(ref).withSpan(typeTree.span))
      def rec(tpe: Type): Tree =
        tpe match
          case TypeRef(prefix, _) => transformPrefix(prefix)
          case AppliedType(tycon, _) => rec(tycon)
      rec(typeTree.tpe)
  end ExpressionTransformer

  private def isExpressionVal(sym: Symbol)(using Context): Boolean =
    sym.name == config.expressionTermName

  // symbol can be a class or a method
  private def thisOrOuterValue(tree: Tree)(cls: ClassSymbol)(using Context): Tree =
    val ths = getThis(tree)(expressionStore.classOwners.head)
    val target = expressionStore.classOwners.indexOf(cls)
    if target >= 0 then
      expressionStore.classOwners
        .drop(1)
        .take(target)
        .foldLeft(ths) { (innerObj, outerSym) =>
          if innerObj == ths && config.localVariables.contains("$outer") then getLocalOuter(tree)(outerSym)
          else getOuter(tree)(innerObj, outerSym)
        }
    else nullLiteral

  private def getThis(tree: Tree)(cls: ClassSymbol)(using Context): Tree =
    reflectEval(tree)(nullLiteral, ReflectEvalStrategy.This(cls), Nil)

  private def getLocalOuter(tree: Tree)(outerCls: ClassSymbol)(using Context): Tree =
    val strategy = ReflectEvalStrategy.LocalOuter(outerCls)
    reflectEval(tree)(nullLiteral, strategy, Nil)

  private def getOuter(tree: Tree)(qualifier: Tree, outerCls: ClassSymbol)(using Context): Tree =
    val strategy = ReflectEvalStrategy.Outer(outerCls)
    reflectEval(tree)(qualifier, strategy, Nil)

  private def getLocalValue(tree: Tree)(variable: Symbol)(using Context): Tree =
    val isByName = isByNameParam(variable.info)
    val strategy = ReflectEvalStrategy.LocalValue(variable.asTerm, isByName)
    reflectEval(tree)(nullLiteral, strategy, Nil)

  private def isByNameParam(tpe: Type)(using Context): Boolean =
    tpe match
      case _: ExprType => true
      case ref: TermRef => isByNameParam(ref.symbol.info)
      case _ => false

  private def setLocalValue(tree: Tree)(variable: Symbol, rhs: Tree)(using Context): Tree =
    val strategy = ReflectEvalStrategy.LocalValueAssign(variable.asTerm)
    reflectEval(tree)(nullLiteral, strategy, List(rhs))

  private def getClassCapture(tree: Tree)(variable: Symbol, cls: ClassSymbol)(using Context): Tree =
    val byName = isByNameParam(variable.info)
    val strategy = ReflectEvalStrategy.ClassCapture(variable.asTerm, cls, byName)
    val qualifier = thisOrOuterValue(tree)(cls)
    reflectEval(tree)(qualifier, strategy, Nil)

  private def setClassCapture(tree: Tree)(variable: Symbol, cls: ClassSymbol, value: Tree)(using Context) =
    val strategy = ReflectEvalStrategy.ClassCaptureAssign(variable.asTerm, cls)
    val qualifier = thisOrOuterValue(tree)(cls)
    reflectEval(tree)(qualifier, strategy, List(value))

  private def getMethodCapture(tree: Tree)(variable: Symbol, method: TermSymbol)(using Context): Tree =
    val isByName = isByNameParam(variable.info)
    val strategy =
      ReflectEvalStrategy.MethodCapture(variable.asTerm, method.asTerm, isByName)
    reflectEval(tree)(nullLiteral, strategy, Nil)

  private def setMethodCapture(tree: Tree)(variable: Symbol, method: Symbol, value: Tree)(using Context) =
    val strategy =
      ReflectEvalStrategy.MethodCaptureAssign(variable.asTerm, method.asTerm)
    reflectEval(tree)(nullLiteral, strategy, List(value))

  private def getStaticObject(tree: Tree)(obj: Symbol)(using ctx: Context): Tree =
    val strategy = ReflectEvalStrategy.StaticObject(obj.asClass)
    reflectEval(tree)(nullLiteral, strategy, Nil)

  private def getField(tree: Tree)(qualifier: Tree, field: TermSymbol)(using Context): Tree =
    val byName = isByNameParam(field.info)
    val strategy = ReflectEvalStrategy.Field(field, byName)
    reflectEval(tree)(qualifier, strategy, Nil)

  private def setField(tree: Tree)(qualifier: Tree, field: TermSymbol, rhs: Tree)(using Context): Tree =
    val strategy = ReflectEvalStrategy.FieldAssign(field)
    reflectEval(tree)(qualifier, strategy, List(rhs))

  private def callMethod(tree: Tree)(qualifier: Tree, method: TermSymbol, args: List[Tree])(using Context): Tree =
    val strategy = ReflectEvalStrategy.MethodCall(method)
    reflectEval(tree)(qualifier, strategy, args)

  private def callConstructor(tree: Tree)(qualifier: Tree, ctr: TermSymbol, args: List[Tree])(using Context): Tree =
    val strategy = ReflectEvalStrategy.ConstructorCall(ctr, ctr.owner.asClass)
    reflectEval(tree)(qualifier, strategy, args)

  private def reflectEval(tree: Tree)(
      qualifier: Tree,
      strategy: ReflectEvalStrategy,
      args: List[Tree]
  )(using Context): Tree =
    val evalArgs = List(
      qualifier,
      Literal(Constant(strategy.toString)), // only useful for debugging, when printing trees
      JavaSeqLiteral(args, TypeTree(ctx.definitions.ObjectType))
    )
    cpy
      .Apply(tree)(Select(This(config.expressionClass), termName("reflectEval")), evalArgs)
      .withAttachment(ReflectEvalStrategy, strategy)

  private def isStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) && symbol.isStatic && !symbol.is(JavaDefined) && !symbol.isRoot

  private def isNonStaticObject(symbol: Symbol)(using Context): Boolean =
    symbol.is(Module) && !symbol.isStatic && !symbol.isRoot

  private def isLocalVariable(symbol: Symbol)(using Context): Boolean =
    !symbol.is(Method) && symbol.isLocalToBlock

  /** Check if a term is accessible from the expression class.
   *  At this phase, there is no need test privateWithin, as it will no longer be checked.
   *  This eliminates the need to use reflection to evaluate privateWithin members,
   *  which would otherwise degrade performances.
   */
  private def isAccessibleMember(tree: Tree)(using Context): Boolean =
    val symbol = tree.symbol
    symbol.owner.isType &&
    !symbol.isPrivate &&
    !symbol.is(Protected) &&
    isTypeAccessible(widenDealiasQualifierType(tree))

  private def widenDealiasQualifierType(tree: Tree)(using Context): Type =
    tree match
      case Ident(_) => tree.symbol.enclosingClass.thisType.widenDealias
      case Select(qualifier, _) => qualifier.tpe.widenDealias
      case Apply(fun, _) => widenDealiasQualifierType(fun)
      case TypeApply(fun, _) => widenDealiasQualifierType(fun)

  // Check if a type is accessible from the expression class
  private def isTypeAccessible(tpe: Type)(using Context): Boolean =
    def isPublic(sym: Symbol): Boolean =
      !sym.isLocal && (sym.isPublic || sym.privateWithin.is(PackageClass))
    tpe.forallParts {
      case tpe: NamedType if tpe.symbol != NoSymbol =>
        isLocalToExpression(tpe.symbol) || isPublic(tpe.symbol)
      case _ => true
    }

  private def isLocalToExpression(symbol: Symbol)(using Context): Boolean =
    val evaluateMethod = config.evaluateMethod
    symbol.ownersIterator.exists(_ == evaluateMethod)

private object ExtractExpression:
  val name: String = "extractExpression"
