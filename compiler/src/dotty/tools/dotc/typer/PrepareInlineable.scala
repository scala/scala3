package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import Trees._
import core._
import Flags._
import Symbols._
import Types._
import Decorators._
import NameKinds._
import StdNames.nme
import Contexts.Context
import Names.Name
import NameKinds.{InlineAccessorName, UniqueInlineName}
import NameOps._
import Annotations._
import transform.{AccessProxies, PCPCheckAndHeal, Splicer, TreeMapWithStages}
import config.Printers.inlining
import util.{Property, SourcePosition}
import dotty.tools.dotc.core.StagingContext._
import dotty.tools.dotc.transform.TreeMapWithStages._

object PrepareInlineable {
  import tpd._

  class InlineAccessors extends AccessProxies {

    /** If an inline accessor name wraps a unique inline name, this is taken as indication
     *  that the inline accessor takes its receiver as first parameter. Such accessors
     *  are created by MakeInlineablePassing.
     */
    override def passReceiverAsArg(name: Name)(implicit ctx: Context): Boolean = name match {
      case InlineAccessorName(UniqueInlineName(_, _)) => true
      case _ => false
    }

    /** A tree map which inserts accessors for non-public term members accessed from inlined code.
     */
    abstract class MakeInlineableMap(val inlineSym: Symbol) extends TreeMap with Insert {
      def accessorNameKind: PrefixNameKind = InlineAccessorName

      /** A definition needs an accessor if it is private, protected, or qualified private
       *  and it is not part of the tree that gets inlined. The latter test is implemented
       *  by excluding all symbols properly contained in the inline method.
       *
       *  Constant vals don't need accessors since they are inlined in FirstTransform.
       *  Inline methods don't need accessors since they are inlined in Typer.
       */
      def needsAccessor(sym: Symbol)(implicit ctx: Context): Boolean =
        sym.isTerm &&
        (sym.is(AccessFlags) || sym.privateWithin.exists) &&
        !sym.isContainedIn(inlineSym) &&
        !(sym.isStableMember && sym.info.widenTermRefExpr.isInstanceOf[ConstantType]) &&
        !sym.isInlineMethod

      def preTransform(tree: Tree)(implicit ctx: Context): Tree

      def postTransform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case Assign(lhs, rhs) if lhs.symbol.name.is(InlineAccessorName) =>
          cpy.Apply(tree)(useSetter(lhs), rhs :: Nil)
        case _ =>
          tree
      }

      override def transform(tree: Tree)(implicit ctx: Context): Tree =
        postTransform(super.transform(preTransform(tree)))
    }

    /** Direct approach: place the accessor with the accessed symbol. This has the
     *  advantage that we can re-use the receiver as is. But it is only
     *  possible if the receiver is essentially this or an outer this, which is indicated
     *  by the test that we can find a host for the accessor.
     */
    class MakeInlineableDirect(inlineSym: Symbol) extends MakeInlineableMap(inlineSym) {
      def preTransform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case tree: RefTree if needsAccessor(tree.symbol) =>
          if (tree.symbol.isConstructor) {
            ctx.error("Implementation restriction: cannot use private constructors in inlineinline methods", tree.sourcePos)
            tree // TODO: create a proper accessor for the private constructor
          }
          else useAccessor(tree)
        case _ =>
          tree
      }
      override def ifNoHost(reference: RefTree)(implicit ctx: Context): Tree = reference
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

      def preTransform(tree: Tree)(implicit ctx: Context): Tree = tree match {
        case _: Apply | _: TypeApply | _: RefTree
        if needsAccessor(tree.symbol) && tree.isTerm && !tree.symbol.isConstructor =>
          val (refPart, targs, argss) = decomposeCall(tree)
          val qual = qualifier(refPart)
          inlining.println(i"adding receiver passing inline accessor for $tree/$refPart -> (${qual.tpe}, $refPart: ${refPart.getClass}, [$targs%, %], ($argss%, %))")

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

          ref(accessor)
            .appliedToTypeTrees(localRefs.map(TypeTree(_)) ++ targs)
            .appliedToArgss((qual :: Nil) :: argss)
            .withSpan(tree.span)

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
        case _ => tree
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
    def makeInlineable(tree: Tree)(implicit ctx: Context): Tree = {
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

  def isLocalOrParam(sym: Symbol, inlineMethod: Symbol)(implicit ctx: Context): Boolean =
    sym.isContainedIn(inlineMethod) && sym != inlineMethod

  def isLocal(sym: Symbol, inlineMethod: Symbol)(implicit ctx: Context): Boolean =
    isLocalOrParam(sym, inlineMethod) && !(sym.is(Param) && sym.owner == inlineMethod)

  /** Register inline info for given inlineable method `sym`.
   *
   *  @param sym         The symbol denotation of the inlineable method for which info is registered
   *  @param treeExpr    A function that computes the tree to be inlined, given a context
   *                     This tree may still refer to non-public members.
   *  @param ctx         The context to use for evaluating `treeExpr`. It needs
   *                     to have the inline method as owner.
   */
  def registerInlineInfo(
      inlined: Symbol, treeExpr: Context => Tree)(implicit ctx: Context): Unit = {
    inlined.unforcedAnnotation(defn.BodyAnnot) match {
      case Some(ann: ConcreteBodyAnnotation) =>
      case Some(ann: LazyBodyAnnotation) if ann.isEvaluated =>
      case _ =>
        if (!ctx.isAfterTyper) {
          val inlineCtx = ctx
          inlined.updateAnnotation(LazyBodyAnnotation { _ =>
            implicit val ctx = inlineCtx
            val rawBody = treeExpr(ctx)
            val typedBody =
              if (ctx.reporter.hasErrors) rawBody
              else ctx.compilationUnit.inlineAccessors.makeInlineable(rawBody)
            checkInlineMethod(inlined, typedBody)
            val inlineableBody = typedBody
            inlining.println(i"Body to inline for $inlined: $inlineableBody")
            inlineableBody
          })
        }
    }
  }

  def checkInlineMethod(inlined: Symbol, body: Tree)(implicit ctx: Context): Unit = {
    if (ctx.outer.inInlineMethod)
      ctx.error(ex"implementation restriction: nested inline methods are not supported", inlined.sourcePos)
    if (inlined.name.isUnapplyName && tupleArgs(body).isEmpty)
      ctx.warning(
        em"inline unapply method can be rewritten only if its right hand side is a tuple (e1, ..., eN)",
        body.sourcePos)
  }

  def checkInlineMacro(sym: Symbol, rhs: Tree, pos: SourcePosition)(implicit ctx: Context) = {
    if (!ctx.isAfterTyper) {

      /** InlineSplice is used to detect cases where the expansion
       *  consists of a (possibly multiple & nested) block or a sole expression.
       */
      object InlineSplice {
        def unapply(tree: Tree)(implicit ctx: Context): Option[Tree] = tree match {
          case Spliced(code) if Splicer.canBeSpliced(code) => Some(code)
          case Block(List(stat), Literal(Constants.Constant(()))) => unapply(stat)
          case Block(Nil, expr) => unapply(expr)
          case Typed(expr, _) => unapply(expr)
          case _ => None
        }
      }

      var isMacro = false
      new TreeMapWithStages(freshStagingContext) {
        override protected def transformSplice(body: tpd.Tree, splice: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
          isMacro = true
          splice
        }
        override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree =
          if (isMacro) tree else super.transform(tree)
      }.transform(rhs)

      if (isMacro) {
        sym.setFlag(Macro)
        if (level == 0)
          rhs match {
            case InlineSplice(_) =>
              new PCPCheckAndHeal(freshStagingContext).transform(rhs) // Ignore output, only check PCP
            case _ =>
              ctx.error(
                """Malformed macro.
                  |
                  |Expected the splice ${...} to be at the top of the RHS:
                  |  inline def foo(inline x: X, ..., y: Y): Int = ${impl(x, ... '{y}})
                  |
                  | * The contents of the splice must call a static method
                  | * All arguments must be quoted or inline
                """.stripMargin, pos)
          }
      }
    }
  }

}
