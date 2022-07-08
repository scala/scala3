package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import Trees._
import core._
import Flags._
import Symbols._
import Flags._
import Types._
import Decorators._
import StdNames.nme
import Contexts._
import Names.{Name, TermName}
import NameKinds.{InlineAccessorName, UniqueInlineName}
import NameOps._
import Annotations._
import transform.{AccessProxies, PCPCheckAndHeal, Splicer}
import transform.SymUtils.*
import config.Printers.inlining
import util.Property
import dotty.tools.dotc.transform.TreeMapWithStages._

object PrepareInlineable {
  import tpd._

  private val InlineAccessorsKey = new Property.Key[InlineAccessors]

  def initContext(ctx: Context): Context =
    ctx.fresh.setProperty(InlineAccessorsKey, new InlineAccessors)

  def makeInlineable(tree: Tree)(using Context): Tree =
    ctx.property(InlineAccessorsKey).get.makeInlineable(tree)

  def addAccessorDefs(cls: Symbol, body: List[Tree])(using Context): List[Tree] =
    ctx.property(InlineAccessorsKey) match
      case Some(inlineAccessors) => inlineAccessors.addAccessorDefs(cls, body)
      case _ => body

  class InlineAccessors extends AccessProxies {

    /** If an inline accessor name wraps a unique inline name, this is taken as indication
     *  that the inline accessor takes its receiver as first parameter. Such accessors
     *  are created by MakeInlineablePassing.
     */
    override def passReceiverAsArg(name: Name)(using Context): Boolean = name match {
      case InlineAccessorName(UniqueInlineName(_, _)) => true
      case _ => false
    }

    /** A tree map which inserts accessors for non-public term members accessed from inlined code.
     */
    abstract class MakeInlineableMap(val inlineSym: Symbol) extends TreeMap with Insert {
      def accessorNameOf(name: TermName, site: Symbol)(using Context): TermName =
        InlineAccessorName(name).expandedName(site)

      /** A definition needs an accessor if it is private, protected, or qualified private
       *  and it is not part of the tree that gets inlined. The latter test is implemented
       *  by excluding all symbols properly contained in the inline method.
       *
       *  Constant vals don't need accessors since they are inlined in FirstTransform.
       *  Inline methods don't need accessors since they are inlined in Typer.
       *
       *  When creating accessors for staged/quoted code we only need to create accessors
       *  for the code that is staged. This excludes code at level 0 (except if it is inlined).
       */
      def needsAccessor(sym: Symbol)(using Context): Boolean =
        sym.isTerm &&
        (sym.isOneOf(AccessFlags) || sym.privateWithin.exists) &&
        !sym.isContainedIn(inlineSym) &&
        !(sym.isStableMember && sym.info.widenTermRefExpr.isInstanceOf[ConstantType]) &&
        !sym.isInlineMethod &&
        (Inliner.inInlineMethod || StagingContext.level > 0)

      def preTransform(tree: Tree)(using Context): Tree

      def postTransform(tree: Tree)(using Context): Tree = tree match {
        case Assign(lhs, rhs) if lhs.symbol.name.is(InlineAccessorName) =>
          cpy.Apply(tree)(useSetter(lhs), rhs :: Nil)
        case _ =>
          tree
      }

      override def transform(tree: Tree)(using Context): Tree =
        inContext(stagingContext(tree)) {
          postTransform(super.transform(preTransform(tree)))
        }

      private def stagingContext(tree: Tree)(using Context): Context = tree match
        case tree: Apply if tree.symbol.isQuote => StagingContext.quoteContext
        case tree: Apply if tree.symbol.isExprSplice => StagingContext.spliceContext
        case _ => ctx
    }

    /** Direct approach: place the accessor with the accessed symbol. This has the
     *  advantage that we can re-use the receiver as is. But it is only
     *  possible if the receiver is essentially this or an outer this, which is indicated
     *  by the test that we can find a host for the accessor.
     */
    class MakeInlineableDirect(inlineSym: Symbol) extends MakeInlineableMap(inlineSym) {
      def preTransform(tree: Tree)(using Context): Tree = tree match {
        case tree: RefTree if needsAccessor(tree.symbol) =>
          if (tree.symbol.isConstructor) {
            report.error("Implementation restriction: cannot use private constructors in inline methods", tree.srcPos)
            tree // TODO: create a proper accessor for the private constructor
          }
          else useAccessor(tree)
        case _ =>
          tree
      }
      override def ifNoHost(reference: RefTree)(using Context): Tree = reference
    }

    /** Fallback approach if the direct approach does not work: Place the accessor method
     *  in the same class as the inline method, and let it take the receiver as parameter.
     *  This is tricky, since we have to find a suitable type for the parameter, which might
     *  require additional type parameters for the inline accessor. An example is in the
     *  `TestPassing` class in test `run/inline/inlines_1`:
     *
     *    class C[T](x: T) {
     *      private[inlines] def next[U](y: U): (T, U) = (x, y)
     *    }
     *    class TestPassing {
     *      inline def foo[A](x: A): (A, Int) = {
     *      val c = new C[A](x)
     *      c.next(1)
     *    }
     *    inline def bar[A](x: A): (A, String) = {
     *      val c = new C[A](x)
     *      c.next("")
     *    }
     *
     *  `C` could be compiled separately, so we cannot place the inline accessor in it.
     *  Instead, the inline accessor goes into `TestPassing` and takes the actual receiver
     *  type as argument:
     *
     *    def inline$next$i1[A, U](x$0: C[A])(y: U): (A, U) =
     *      x$0.next[U](y)
     *
     *  Since different calls might have different receiver types, we need to generate one
     *  such accessor per call, so they need to have unique names.
     */
    class MakeInlineablePassing(inlineSym: Symbol) extends MakeInlineableMap(inlineSym) {

      def preTransform(tree: Tree)(using Context): Tree = tree match {
        case _: Apply | _: TypeApply | _: RefTree
        if needsAccessor(tree.symbol) && tree.isTerm && !tree.symbol.isConstructor =>
          val refPart = funPart(tree)
          val argss = allArgss(tree)
          val qual = qualifier(refPart)
          inlining.println(i"adding receiver passing inline accessor for $tree/$refPart -> (${qual.tpe}, $refPart: ${refPart.getClass}, $argss%, %")

          // Need to dealias in order to cagtch all possible references to abstracted over types in
          // substitutions
          val dealiasMap = new TypeMap {
            def apply(t: Type) = mapOver(t.dealias)
          }
          val qualType = dealiasMap(qual.tpe.widen)

          // The types that are local to the inline method, and that therefore have
          // to be abstracted out in the accessor, which is external to the inline method
          val localRefs = qualType.namedPartsWith(ref =>
            ref.isType && ref.symbol.isContainedIn(inlineSym)).toList

          // Add qualifier type as leading method argument to argument `tp`
          def addQualType(tp: Type): Type = tp match {
            case tp: PolyType => tp.derivedLambdaType(tp.paramNames, tp.paramInfos, addQualType(tp.resultType))
            case tp: ExprType => addQualType(tp.resultType)
            case tp => MethodType(qualType.simplified :: Nil, tp)
          }

          // Abstract accessed type over local refs
          def abstractQualType(mtpe: Type): Type =
            if (localRefs.isEmpty) mtpe
            else PolyType.fromParams(localRefs.map(_.symbol.asType), mtpe)
              .asInstanceOf[PolyType].flatten

          val accessed = refPart.symbol.asTerm
          val accessedType = refPart.tpe.widen
          val accessor = accessorSymbol(
            owner = inlineSym.owner,
            accessorName = InlineAccessorName(UniqueInlineName.fresh(accessed.name)),
            accessorInfo = abstractQualType(addQualType(dealiasMap(accessedType))),
            accessed = accessed)

          val (leadingTypeArgs, otherArgss) = splitArgs(argss)
          val argss1 = joinArgs(
            localRefs.map(TypeTree(_)) ++ leadingTypeArgs, // TODO: pass type parameters in two sections?
            (qual :: Nil) :: otherArgss
          )
          ref(accessor).appliedToArgss(argss1).withSpan(tree.span)

            // TODO: Handle references to non-public types.
            // This is quite tricky, as such types can appear anywhere, including as parts
            // of types of other things. For the moment we do nothing and complain
            // at the implicit expansion site if there's a reference to an inaccessible type.
            // Draft code (incomplete):
            //
            //  val accessor = accessorSymbol(tree, TypeAlias(tree.tpe)).asType
            //  myAccessors += TypeDef(accessor).withPos(tree.pos.focus)
            //  ref(accessor).withSpan(tree.span)
            //
        case _: TypeDef if tree.symbol.is(Case) =>
          report.error(reporting.CaseClassInInlinedCode(tree), tree)
          tree
        case _ =>
          tree
      }
    }

    /** Adds accessors for all non-public term members accessed
     *  from `tree`. Non-public type members are currently left as they are.
     *  This means that references to a private type will lead to typing failures
     *  on the code when it is inlined. Less than ideal, but hard to do better (see below).
     *
     *  @return If there are accessors generated, a thicket consisting of the rewritten `tree`
     *          and all accessors, otherwise the original tree.
     */
    def makeInlineable(tree: Tree)(using Context): Tree = {
      val inlineSym = ctx.owner
      if (inlineSym.owner.isTerm)
        // Inlineable methods in local scopes can only be called in the scope they are defined,
        // so no accessors are needed for them.
        tree
      else
        new MakeInlineablePassing(inlineSym).transform(
          new MakeInlineableDirect(inlineSym).transform(tree))
    }
  }

  def isLocalOrParam(sym: Symbol, inlineMethod: Symbol)(using Context): Boolean =
    sym.isContainedIn(inlineMethod) && sym != inlineMethod

  def isLocal(sym: Symbol, inlineMethod: Symbol)(using Context): Boolean =
    isLocalOrParam(sym, inlineMethod) && !(sym.is(Param) && sym.owner == inlineMethod)

  /** The type ascription `rhs: tpt`, unless `original` is `transparent`. */
  def wrapRHS(original: untpd.DefDef, tpt: Tree, rhs: Tree)(using Context): Tree =
    if original.mods.is(Transparent) then rhs else Typed(rhs, tpt)

  /** Return result of evaluating `op`, but drop `Inline` flag and `Body` annotation
   *  of `sym` in case that leads to errors.
   */
  def dropInlineIfError(sym: Symbol, op: => Tree)(using Context): Tree =
    val initialErrorCount = ctx.reporter.errorCount
    try op
    finally
      if ctx.reporter.errorCount != initialErrorCount then
        sym.resetFlag(Inline)
        sym.resetFlag(Transparent)
        sym.removeAnnotation(defn.BodyAnnot)

  /** Register inline info for given inlineable method `sym`.
   *
   *  @param sym         The symbol denotation of the inlineable method for which info is registered
   *  @param treeExpr    A function that computes the tree to be inlined, given a context
   *                     This tree may still refer to non-public members.
   *  @param ctx         The context to use for evaluating `treeExpr`. It needs
   *                     to have the inline method as owner.
   */
  def registerInlineInfo(
      inlined: Symbol, treeExpr: Context ?=> Tree)(using Context): Unit =
    inlined.unforcedAnnotation(defn.BodyAnnot) match {
      case Some(ann: ConcreteBodyAnnotation) =>
      case Some(ann: LazyBodyAnnotation) if ann.isEvaluated || ann.isEvaluating =>
      case _ =>
        if (!ctx.isAfterTyper) {
          val inlineCtx = ctx
          inlined.updateAnnotation(LazyBodyAnnotation {
            given ctx: Context = inlineCtx
            var inlinedBody = dropInlineIfError(inlined, treeExpr)
            if inlined.isInlineMethod then
              inlinedBody = dropInlineIfError(inlined,
                checkInlineMethod(inlined,
                  PrepareInlineable.makeInlineable(inlinedBody)))
            inlining.println(i"Body to inline for $inlined: $inlinedBody")
            inlinedBody
          })
        }
    }

  private def checkInlineMethod(inlined: Symbol, body: Tree)(using Context): body.type = {
    if Inliner.inInlineMethod(using ctx.outer) then
      report.error(ex"Implementation restriction: nested inline methods are not supported", inlined.srcPos)

    if (inlined.is(Macro) && !ctx.isAfterTyper) {

      def checkMacro(tree: Tree): Unit = tree match {
        case Spliced(code) =>
          if (code.symbol.flags.is(Inline))
            report.error("Macro cannot be implemented with an `inline` method", code.srcPos)
          Splicer.checkValidMacroBody(code)
          new PCPCheckAndHeal(freshStagingContext).transform(body) // Ignore output, only check PCP
        case Block(List(stat), Literal(Constants.Constant(()))) => checkMacro(stat)
        case Block(Nil, expr) => checkMacro(expr)
        case Typed(expr, _) => checkMacro(expr)
        case Block(DefDef(nme.ANON_FUN, _, _, _) :: Nil, Closure(_, fn, _)) if fn.symbol.info.isImplicitMethod =>
          // TODO Support this pattern
          report.error(
            """Macros using a return type of the form `foo(): X ?=> Y` are not yet supported.
              |
              |Place the implicit as an argument (`foo()(using X): Y`) to overcome this limitation.
              |""".stripMargin, tree.srcPos)
        case _ =>
          report.error(
            """Malformed macro.
              |
              |Expected the splice ${...} to be at the top of the RHS:
              |  inline def foo(inline x: X, ..., y: Y): Int = ${ impl('x, ... 'y) }
              |
              | * The contents of the splice must call a static method
              | * All arguments must be quoted
            """.stripMargin, inlined.srcPos)
      }
      checkMacro(body)
    }
    body
  }
}
