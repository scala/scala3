package dotty.tools.dotc
package transform

import util.SrcPos
import ast.tpd
import core.Contexts.{ctx, Context}
import core.Decorators.*
import core.Flags.*
import core.Names.*
import core.StdNames.nme
import core.Symbols.*
import core.Types.*
import MegaPhase.MiniPhase

/** A Termination Checker for methods annotated with `@terminates`.
 *
 *  What it does:
 *
 *  Traverses every method annotated with `@terminates` and verifies that the method will always terminate.
 *
 *  Termination is defined as "the method either returns a value or throws an exception".
 *
 *  The checker assigns every value a `Size` relative to the enclosing method's
 *  parameters:
 *    - `Smaller`: the value is a strict structural sub-value of a parameter.
 *    - `Same`:    the value is the same size as a parameter.
 *    - `Unknown`: the size relationship cannot be determined syntactically.
 *
 *  At each call site the argument tuple is compared to the method's parameter tuple using `areSmaller`,
 *  which checks lexicographically if arguments are smaller than the parameters.
 *  The order of comparison can be overridden with `@decreases`.
 */
class CheckTermination extends MiniPhase {
  import tpd.*

  override def phaseName: String = CheckTermination.name

  override def description: String = CheckTermination.description

  override def isEnabled(using Context): Boolean = ctx.settings.YcheckTermination.value

  override def transformDefDef(tree: DefDef)(using Context): Tree = {
    val method = tree.symbol
    val mandatory = method.hasAnnotation(defn.TerminationAnnot)

    if mandatory && !method.is(Deferred) then
      val checker = new TerminationChecker(method)
      checker.traverse(tree.rhs)
    // if the method overrides a method that has `@terminates`,
    // the overriding method is required to have `@terminates` as well.
    else if method.allOverriddenSymbols.exists(_.hasAnnotation(defn.TerminationAnnot)) then
      report.error(
        s"${method.name} must be annotated with @terminates.",
        tree.srcPos
      )

    tree
  }

  private class TerminationChecker(startMethod: Symbol)(using Context)
      extends TreeTraverser {

    enum Size:
      case Smaller, Same, Unknown

    var shouldCheckCalls = true

    // Maps `val` symbol to a (source, size) pair.
    // The source may be a tree as it is also used to keep track of closures.
    private var valMap = Map.empty[Symbol, (Symbol | Tree, Size)]

    // Analogous map for mutable `var` symbols.
    private var varMap = Map.empty[Symbol, Size]

    // The chain of methods currently being inlined. Used to detect (mutually) recursive calls.
    private var callStack = List[Symbol](startMethod)

    override def traverse(tree: Tree)(using Context): Unit = {
      tree match
        case tree: DefDef => () // Don't traverse inner function definitions. They handled when called.

        case tree: WhileDo =>
          val savedMap = varMap
          varMap = Map.empty
          val savedshouldCheckCalls = shouldCheckCalls
          shouldCheckCalls = false
          // First traversal to update `varMap` and identify a decreasing var.
          traverseChildren(tree)
          shouldCheckCalls = savedshouldCheckCalls
          if !varMap.exists((_, size) => size == Size.Smaller) then
            report.error(
              s"${startMethod.name} may not terminate due to infinite while loop.",
              tree.srcPos
            )
          else
            varMap = savedMap
            // Second traversal to check calls inside loop.
            traverseChildren(tree)
            varMap = mergeVarSize(savedMap, varMap)

        case tree @ Apply(fn, _) if fn.symbol == defn.uncheckedTerminationMethod =>
          () // Don't traverse, as we assume they terminate.

        case tree: Apply if shouldCheckCalls =>
          val (fn, args) = peelApplies(tree)
          traverse(fn)
          args.foreach {
            case tree: Tree => traverse(tree)
            case _ => ()
          }
          // Check all possible overrides if any or the specific method.
          val methodsSymbol = methodOverrides(fn)
          if methodsSymbol.isEmpty then
            checkMethodCall(fn.symbol, args, tree.srcPos, Some(getMethodSymbol(fn)))
          else
            methodsSymbol.foreach(checkMethodCall(_, args, tree.srcPos, None))

        case tree @ Select(qualifier, _) if shouldCheckCalls =>
          val pm = methodOverrides(tree)
          val methodsSymbol = if pm.isEmpty then List(tree.symbol) else pm
          methodsSymbol.foreach(methodSymbol =>
            // Only check methods with no parameters (getMethodParams also returns `this` as a param).
            if getMethodParams(methodSymbol).length <= 1 then
              checkMethodCall(methodSymbol, Nil, tree.srcPos, None)
          )
          traverseChildren(tree)

        case tree @ If(cond, thenp, elsep) =>
          traverse(cond)
          val savedVarMap = varMap
          traverse(thenp)
          val thenMap = varMap
          varMap = savedVarMap
          traverse(elsep)
          varMap = updateVarSize(savedVarMap, mergeVarSize(thenMap, varMap))

        case tree @ Match(selector, cases) =>
          val syntheticUnapply = isUnapplySynthetic(selector)
          val savedMap = valMap
          val savedVarMap = varMap
          var foldedMap = Map.empty[Symbol, Size]
          cases.foreach(cse =>
            if syntheticUnapply then
              mapMatchedSymbols(selector.symbol, cse.pat)
            traverse(cse.guard)
            traverse(cse.body)
            valMap = savedMap
            foldedMap = mergeVarSize(foldedMap, varMap)
            varMap = savedVarMap
          )
          varMap = updateVarSize(savedVarMap, foldedMap)

        case tree @ Assign(lhs, rhs) =>
          varMap += lhs.symbol -> (
            if areSmaller(List(rhs), List(lhs.symbol), NoSymbol) then Size.Smaller
            else Size.Unknown
          )
          traverseChildren(tree)

        case tree: ValDef =>
          tree.rhs match
            case Select(qualifier, _) =>
              peelSelects(tree.rhs) match
                case Some((symbol, isSmaller)) =>
                  valMap += tree.symbol -> (symbol -> (if isSmaller then Size.Smaller else Size.Same))
                case _ => valMap += tree.symbol -> (tree.rhs -> Size.Same)
            case _ => valMap += tree.symbol -> (tree.rhs -> Size.Same)
          traverseChildren(tree)

        case _ => traverseChildren(tree)
    }

    /** Verify that the call to `methodSymbol` with `args` is terminating.
     *
     *  If `methodSymbol` is already present in `callStack`, the call is (mutually) recursive and `areSmaller` is called
     *  to assert that `args` is lexicographically smaller than `methodSymbol`'s parameters.
     *
     *  If `methodSymbol` is not yet on the stack, `traverseCalled` inlines its body.
     *
     *  @param methodSymbol   the method being called.
     *  @param args           the arguments (including `this` as the first element).
     *  @param pos            source position for error/warning reporting.
     *  @param fallBackSymbol when non-empty, the body of this symbol is inlined instead of `methodSymbol`'s body.
     *                        Used when `methodOverrides` resolves a polymorphic call but the concrete implementation
     *                        is a closure.
     */
    private def checkMethodCall(
        methodSymbol: Symbol,
        args: List[Symbol | Tree],
        pos: SrcPos,
        fallBackSymbol: Option[Symbol]
    )(using Context) = {
      def traverseCalled(methodSymbol: Symbol) = {
        methodSymbol.defTree match
          case defTree: DefDef if !defTree.rhs.isEmpty || methodSymbol.isConstructor =>
            // Push callee to the stack
            callStack = methodSymbol :: callStack
            val savedMap = valMap

            // Map arguments to parameters of the method.
            args.zip(getMethodParams(methodSymbol))
              .foreach((arg, param) => valMap += param -> (arg, Size.Same))
            traverse(defTree.rhs)

            valMap = savedMap
            // Pop callee from the stack
            callStack = callStack.tail
          case _ =>
            if methodSymbol.isRealMethod &&
              !methodSymbol.owner.is(JavaDefined) &&
              methodSymbol != defn.throwMethod
            then
              report.warning(s"Method ${methodSymbol.name} has an empty tree.", pos)
      }

      if !methodSymbol.hasAnnotation(defn.AssumeTerminatesAnnot) then
        callStack.find(_ == methodSymbol) match
          case Some(_) => // Recursive call.
            val params = getMethodParams(methodSymbol)
            if !areSmaller(args, params, methodSymbol) then
              report.error(s"${startMethod.name} may not terminate due to (mutually) recursive call.", pos)
          case None => traverseCalled(fallBackSymbol.getOrElse(methodSymbol))
    }

    /** Enumerate all concrete overrides of the method referred to by `tree`.
     *
     *  If the type is `sealed`, collects every strict sealed descendant that declares an override of the method.
     *
     *  If the static type is effectively open (and not a `FunctionN` trait), an error is reported because the checker
     *  cannot guarantee termination of any override.
     *
     *  Returns an empty list when the type has no sealed descendants or
     *  when the call is on a `FunctionN` type (handled separately via `getMethodSymbol`).
     */
    private def methodOverrides(tree: Tree)(using Context): List[Symbol] = {
      val tpeSym = getTypeSymbol(tree)
      if tpeSym.isEffectivelySealed then
        tpeSym.sealedStrictDescendants.map(_.info.typeSymbol.info.member(tree.symbol.name).symbol)
      else
        // Throw an error if the hierarchy is not sealed exept for trait Function<N> which is handled separately.
        if tpeSym.isOneOf(EffectivelyOpenFlags) && !defn.isFunctionSymbol(tpeSym) then
          report.error(
            s"${tpeSym.name} is not sealed, termination of any overrides of ${tree.symbol} is not guaranteed.",
            tpeSym.srcPos
          )
        Nil
    }

    // Merge var size info: worst case is kept.
    private def mergeVarSize(left: Map[Symbol, Size], right: Map[Symbol, Size]) = {
      (left.keySet | right.keySet).map(symbol =>
        val size = (left.get(symbol), right.get(symbol)) match
          case (Some(Size.Unknown), _) | (_, Some(Size.Unknown)) => Size.Unknown
          case (None, _) | (_, None) | (Some(Size.Same), _) | (_, Some(Size.Same)) => Size.Same
          case _ => Size.Smaller
        symbol -> size
      ).toMap
    }

    private def updateVarSize(before: Map[Symbol, Size], after: Map[Symbol, Size]) = {
      after.map((symbol, afterSize) =>
        val size = before.get(symbol) match
          case Some(beforeSize) =>
            (beforeSize, afterSize) match
              case (Size.Unknown, _) | (_, Size.Unknown) => Size.Unknown
              case (Size.Smaller, _) | (_, Size.Smaller) => Size.Smaller
              case _ => Size.Same
          case None => afterSize
        symbol -> size
      )
    }

    private def getTypeSymbol(tree: Tree)(using Context): Symbol = {
      tree match
        case Select(qualifier @ Select(_, name), _) =>
          getTypeSymbol(qualifier).typeRef.select(name).typeSymbol
        case Select(qualifier, _) => getTypeSymbol(qualifier)
        case Apply(tree: Select, _) => getTypeSymbol(tree)
        case _ => tree.symbol.info.typeSymbol
    }

    private def getMethodParams(methodSymbol: Symbol)(using Context): List[Symbol] = {
      // Include `this` as a parameter.
      val thisSymbol = methodSymbol.enclosingClass.thisType.typeSymbol
      thisSymbol :: methodSymbol.paramSymss.filter(!_.exists(_.isTypeParam)).flatten
    }

    private def getMethodSymbol(fn: Tree)(using Context): Symbol = {
      val symbol = fn match
        case Select(qualifier, _) => qualifier.symbol
        case _ => fn.symbol
      // The valMap may contain a closure of a lambda function.
      valMap.get(symbol) match
        case Some((op, _)) =>
          op match
            case Block(tree :: _, Closure(_, _, _)) => tree.symbol
            case op: Tree => getMethodSymbol(op)
            case _ => fn.symbol
        case None => fn.symbol
    }

    /** Decompose a curried `Apply` node into the function tree and the flat argument list.
     *  For example, `f(a)(b)` is decomposed into `(f, [this, a, b])`.
     */
    private def peelApplies(tree: Apply)(using Context): (Tree, List[Symbol | Tree]) = {
      def loop(tree: Tree, acc: List[Tree] = Nil): (Tree, List[Tree]) = {
        tree match
          case Apply(fn, args) => loop(fn, args ++ acc)
          case TypeApply(fn, _) => loop(fn, acc)
          case _ => (tree, acc)
      }

      val (fn, args) = loop(tree)
      (
        fn,
        (fn match
          case Select(qualifier, _) => qualifier.symbol
          case TypeApply(Select(qualifier, _), _) => qualifier.symbol
          case _ => fn.symbol.enclosingClass.thisType.typeSymbol
        ) :: args
      )
    }

    /** Resolve a chain of field-selection back to an original symbol,
     *  and determine whether a structural decrease occurred along the way.
     *
     *  Returns `Some((originalSymbol, isSmaller))` when the tree can be traced back to a symbol.
     *
     *  Used in `ValDef` handling and in `areSmaller` to derive size information.
     */
    private def peelSelects(tree: Tree)(using Context): Option[(Symbol, Boolean)] = {
      def isSelectedMethodAnnotated(tpeSym: Symbol, name: Name, cls: Symbol) = {
        tpeSym.info.member(name).symbol.hasAnnotation(cls)
      }

      def isQualifierSmaller(qualifier: Tree): Option[(Symbol, Boolean)] = {
        peelSelects(qualifier) match
          case Some(symbol, _) =>
            val tpeSym = symbol.info.typeSymbol
            val res = isSelectedMethodAnnotated(tpeSym, tree.symbol.name, defn.AssumeDecreasesAnnot) ||
              (!tpeSym.children.isEmpty &&
                tpeSym.children.forall(isSelectedMethodAnnotated(_, tree.symbol.name, defn.AssumeDecreasesAnnot))) || {
                tpeSym.is(Case) && tpeSym.isClass && {
                  val constructorParams = tpeSym.asClass.primaryConstructor
                    .paramSymss.filter(!_.exists(_.isTypeParam)).head
                  constructorParams.exists(_.name == tree.symbol.name)
                }
              }
            if res then Some((symbol, res)) else None
          case None => None
      }
      tree match
        case Ident(_) => Some((tree.symbol, false))
        case Select(qualifier, _) => isQualifierSmaller(qualifier)
        case Apply(Select(qualifier, _), _) => isQualifierSmaller(qualifier)
        case _ => None
    }

    private def areSmaller(args: List[Symbol | Tree], params: List[Symbol], methodSymbol: Symbol)(using Context) = {
      // Get parameter symbols specified in @Decreases annotation.
      def getMeasure(tree: DefDef)(using Context) = {
        val candidates = tree.tpt.tpe.getAnnotation(defn.DecreasesAnnot) match
          case Some(annot) =>
            annot.argument(0) match
              case Some(Apply(_, args)) => args.map(_.symbol)
              case Some(arg @ Ident(_)) => List(arg.symbol)
              case _ => Nil
          case _ => Nil
        candidates.filter(getMethodParams(tree.symbol).contains)
      }

      def compareSize(arg: Symbol, param: Symbol, decreased: Boolean = false): Size = {
        if arg == param then
          if decreased then Size.Smaller else Size.Same
        else
          valMap.get(arg) match
            case None => Size.Unknown
            case Some((result, size)) =>
              val (symbol, isSmaller) = result match
                case symbol: Symbol => (symbol, false)
                case tree: Tree =>
                  peelSelects(tree) match
                    case Some(res) => res
                    case None => (tree.symbol, false)
              size match
                case Size.Smaller => compareSize(symbol, param, true)
                case Size.Same => compareSize(symbol, param, isSmaller || decreased)
                case Size.Unknown => Size.Unknown
      }

      def isLexicoDecreasing(argsParams: List[(Symbol | Tree, Symbol)]): Boolean = {
        argsParams match
          case (arg, param) :: tail =>
            val (argSymbol, decreased) = arg match
              case sym: Symbol => (sym, false)
              case tree: Tree =>
                peelSelects(tree) match
                  case Some(res) => res
                  case None => (tree.symbol, false)
            if varMap.get(argSymbol) == Some(Size.Unknown) then false
            else
              compareSize(argSymbol, param, decreased || varMap.get(argSymbol).exists(_ == Size.Smaller)) match
                case Size.Smaller => typeWellFounded(param.info.typeSymbol, argSymbol.srcPos)
                case Size.Same => isLexicoDecreasing(tail)
                case Size.Unknown => false
          case Nil => false
      }

      val measure = methodSymbol.defTree match
        case tree: DefDef => getMeasure(tree)
        case _ => Nil
      val measureMap = measure.zipWithIndex.toMap
      val orderedArgsParams = args.zip(params).sortBy((_, param) =>
        measureMap.getOrElse(param, Int.MaxValue)
      )
      isLexicoDecreasing(orderedArgsParams)
    }

    private def isUnapplySynthetic(selector: Tree)(using Context): Boolean = {
      def check(tpeSym: Symbol): Boolean = {
        val res = tpeSym.is(Case) && tpeSym.isClass && {
          val unapplyMethod = tpeSym.asClass.companionClass.info.member(nme.unapply).symbol
          unapplyMethod.exists && unapplyMethod.is(Synthetic)
        }
        if !res then
          report.warning(
            s"${selector.symbol.name} may not structurally decrease because ${tpeSym.name} unapply method is overridden or undefined.",
            selector.srcPos
          )
        res
      }

      val tpeSym = selector.symbol.info.typeSymbol
      if tpeSym.is(Sealed) then tpeSym.children.forall(c => c.is(CaseVal) || check(c))
      else check(tpeSym)
    }

    /** Verify that `tpeSym` cannot have an infinite chain of smaller values.
     *
     *  - A type annotated with `@assumeWellFounded` is accepted unconditionally.
     *  - A `sealed` type is well-founded if every child is well-founded.
     *  - A `case object` is trivially well-founded.
     *  - A `case class` is well-founded if all of its field symbols are stable
     *    members and whose names match constructor parameters.
     *
     *  A warning is emitted when a type fails the check, and `false` is returned
     *  so that `areSmaller` treats the argument as `Unknown`.
     */
    private def typeWellFounded(tpeSym: Symbol, pos: SrcPos)(using Context): Boolean = {
      def caseClassCheck(classSym: ClassSymbol): Boolean = {
        val fields = classSym.paramAccessors
        val constructorParams = classSym.primaryConstructor.paramSymss.filter(!_.exists(_.isTypeParam)).head
        fields.forall(symbol => symbol.isStableMember && constructorParams.exists(_.name == symbol.name))
      }

      val res = tpeSym.hasAnnotation(defn.AssumeWellFoundedAnnot) || {
        if tpeSym.is(Sealed) then tpeSym.children.forall(typeWellFounded(_, pos))
        else tpeSym.is(CaseVal) || (tpeSym.is(Case) && tpeSym.isClass && caseClassCheck(tpeSym.asClass))
      }

      if !res then
        report.warning(
          s"Argument of type ${tpeSym.name} decreases but the type is not well-founded.",
          pos
        )

      res
    }

    // Traverses pattern to update valMap bound variables with a Smaller size than selector.
    private def mapMatchedSymbols(selector: Symbol, pat: Tree)(using Context): Unit = {
      val traverser = new TreeTraverser:
        override def traverse(tree: Tree)(using Context): Unit = {
          tree match
            case bind @ Bind(_, _) =>
              valMap += bind.symbol -> (selector, Size.Smaller)
            case _ => ()
          traverseChildren(tree)
        }

      pat match
        case bind @ Bind(_, tree) =>
          valMap += bind.symbol -> (selector, Size.Same)
          traverser.traverse(tree)
        case _ => traverser.traverse(pat)
    }

  }
}

object CheckTermination:
  val name = "check-termination"
  val description = "check if annotated functions terminate"
