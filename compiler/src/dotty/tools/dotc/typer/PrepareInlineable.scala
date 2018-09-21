package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees.NamedArg
import dotty.tools.dotc.ast.{Trees, untpd, tpd, TreeTypeMap}
import Trees._
import core._
import Flags._
import Symbols._
import Types._
import Decorators._
import Constants._
import StdNames.nme
import Contexts.Context
import Names.{Name, TermName, EmptyTermName}
import NameOps._
import NameKinds.{ClassifiedNameKind, InlineAccessorName, UniqueInlineName}
import ProtoTypes.selectionProto
import SymDenotations.SymDenotation
import Annotations._
import transform.{ExplicitOuter, AccessProxies}
import Inferencing.fullyDefinedType
import config.Printers.inlining
import ErrorReporting.errorTree
import collection.mutable
import transform.TypeUtils._
import reporting.trace
import util.Positions.Position
import util.Property
import ast.TreeInfo

object PrepareInlineable {
  import tpd._

  /** Marks an implicit reference found in the context (as opposed to the implicit scope)
   *  from an inlineable body. Such references will be carried along with the body to
   *  the expansion site.
   */
  private val ContextualImplicit = new Property.StickyKey[Unit]

  def markContextualImplicit(tree: Tree)(implicit ctx: Context): Unit =
    if (!defn.ScalaPredefModule.moduleClass.derivesFrom(tree.symbol.maybeOwner))
      methPart(tree).putAttachment(ContextualImplicit, ())

  class InlineAccessors extends AccessProxies {

    /** If an inline accessor name wraps a unique inline name, this is taken as indication
     *  that the inline accessor takes its receiver as first parameter. Such accessors
     *  are created by MakeInlineablePassing.
     */
    override def passReceiverAsArg(name: Name)(implicit ctx: Context) = name match {
      case InlineAccessorName(UniqueInlineName(_, _)) => true
      case _ => false
    }

    /** A tree map which inserts accessors for non-public term members accessed from inlined code.
     */
    abstract class MakeInlineableMap(val inlineSym: Symbol) extends TreeMap with Insert {
      def accessorNameKind = InlineAccessorName

      /** A definition needs an accessor if it is private, protected, or qualified private
       *  and it is not part of the tree that gets inlined. The latter test is implemented
       *  by excluding all symbols properly contained in the inline method.
       *
       *  Constant vals don't need accessors since they are inlined in FirstTransform.
       *  Inline methods don't need accessors since they are inlined in Typer.
       */
      def needsAccessor(sym: Symbol)(implicit ctx: Context) =
        sym.isTerm &&
        (sym.is(AccessFlags) || sym.privateWithin.exists) &&
        !sym.isContainedIn(inlineSym) &&
        !(sym.isStable && sym.info.widenTermRefExpr.isInstanceOf[ConstantType]) &&
        !sym.isInlineMethod

      def preTransform(tree: Tree)(implicit ctx: Context): Tree

      def postTransform(tree: Tree)(implicit ctx: Context) = tree match {
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
            ctx.error("Implementation restriction: cannot use private constructors in inlineinline methods", tree.pos)
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
            .withPos(tree.pos)

            // TODO: Handle references to non-public types.
            // This is quite tricky, as such types can appear anywhere, including as parts
            // of types of other things. For the moment we do nothing and complain
            // at the implicit expansion site if there's a reference to an inaccessible type.
            // Draft code (incomplete):
            //
            //  val accessor = accessorSymbol(tree, TypeAlias(tree.tpe)).asType
            //  myAccessors += TypeDef(accessor).withPos(tree.pos.focus)
            //  ref(accessor).withPos(tree.pos)
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
    def makeInlineable(tree: Tree)(implicit ctx: Context) = {
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

  def isLocalOrParam(sym: Symbol, inlineMethod: Symbol)(implicit ctx: Context) =
    sym.isContainedIn(inlineMethod) && sym != inlineMethod

  def isLocal(sym: Symbol, inlineMethod: Symbol)(implicit ctx: Context) =
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
      inlined: Symbol, originalBody: untpd.Tree, treeExpr: Context => Tree)(implicit ctx: Context): Unit = {
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
            val inlineableBody =
              if (Inliner.typedInline) typedBody
              else addReferences(inlined, originalBody, typedBody)
            inlining.println(i"Body to inline for $inlined: $inlineableBody")
            inlineableBody
          })
        }
    }
  }

  def checkInlineMethod(inlined: Symbol, body: Tree)(implicit ctx: Context) = {
    if (ctx.outer.inInlineMethod)
      ctx.error(ex"implementation restriction: nested inline methods are not supported", inlined.pos)
    if (inlined.name == nme.unapply && tupleArgs(body).isEmpty)
      ctx.warning(
        em"inline unapply method can be rewritten only if its right hand side is a tuple (e1, ..., eN)",
        body.pos)
  }

  /** Tweak untyped tree `original` so that all external references are typed
   *  and it reflects the changes in the corresponding typed tree `typed` that
   *  make `typed` inlineable. Concretely:
   *
   *   - all external references via identifiers or this-references are converted
   *     to typed splices,
   *   - if X gets an inline accessor in `typed`, references to X in `original`
   *     are converted to the inline accessor name.
   */
  private def addReferences(inlineMethod: Symbol,
      original: untpd.Tree, typed: tpd.Tree)(implicit ctx: Context): tpd.Tree = {

    // Maps from positions to external reference types and inline selector names.
    object referenced extends TreeTraverser {
      val typeAtPos = mutable.Map[Position, Type]()
      val accessorAtPos = mutable.Map[Position, Symbol]()
      val implicitRefTypes = mutable.Set[Type]()
      val implicitRefs = new mutable.ListBuffer[Tree]

      def registerIfContextualImplicit(tree: Tree) = tree match {
        case tree: RefTree
        if tree.removeAttachment(ContextualImplicit).isDefined &&
           tree.symbol.exists &&
           !isLocalOrParam(tree.symbol, inlineMethod) &&
           !implicitRefTypes.contains(tree.tpe) =>
          if (tree.existsSubTree(t => isLocal(tree.symbol, inlineMethod)))
            ctx.warning(i"implicit reference $tree is dropped at inline site because it refers to local symbol(s)", tree.pos)
          else {
            implicitRefTypes += tree.tpe
            implicitRefs += tree
          }
        case _ =>
      }

      def registerAccessor(tree: Tree) = {
        inlining.println(i"accessor: $tree at ${tree.pos}")
        accessorAtPos(tree.pos.toSynthetic) = tree.symbol
          // Note: It's possible that during traversals several accessors are stored under the same
          // position. This could happen for instance for implicit conersions added around a tree.
          // or for a setter containing a getter in an op-assignment node.
          // In general, it's always the innermost tree that holds the relevant symbol. The traversal
          // order guarantees that the innermost tree's symbol is stored last, and thereby replaces all previously
          // stored symbols.
      }

      def traverse(tree: Tree)(implicit ctx: Context): Unit = {
        val sym = tree.symbol
        tree match {
          case Ident(nme.WILDCARD) =>
          case _: Ident | _: This =>
            //println(i"leaf: $tree at ${tree.pos}")
            if (sym.exists && !isLocal(sym, inlineMethod)) {
              if (ctx.debug) inlining.println(i"type at $tree @ ${tree.pos.toSynthetic} = ${tree.tpe}")
              tree.tpe match {
                case tp: NamedType if tp.prefix.member(sym.name).isOverloaded =>
                  // refer to prefix instead of to ident directly, so that overloading can be resolved
                  // again at expansion site
                  typeAtPos(tree.pos.startPos) = tp.prefix
                case _ =>
                  typeAtPos(tree.pos.toSynthetic) = tree.tpe
              }
                // Note: It's possible that during traversals several types are stored under the same
                // position. This could happen for instance for implicit conersions added around a tree.
                // In general, it's always the innermost tree that holds the relevant type. The traversal
                // order guarantees that the innermost tree's type is stored last, and thereby replaces all previously
                // stored types.
            }
          case _: Select =>
            sym.name match {
              case InlineAccessorName(UniqueInlineName(_, _)) => return // was already recorded in Apply
              case InlineAccessorName(_) => registerAccessor(tree)
              case _ =>
            }
          case Apply(_: RefTree | _: TypeApply, receiver :: Nil) =>
            sym.name match {
              case InlineAccessorName(UniqueInlineName(_, _)) => registerAccessor(tree)
              case _ =>
            }
          case _ =>
        }
        registerIfContextualImplicit(tree)
        traverseChildren(tree)
      }
    }
    referenced.traverse(typed)

    // The untyped tree transform that applies the tweaks
    object addRefs extends untpd.UntypedTreeMap {
      override def transform(tree: untpd.Tree)(implicit ctx: Context): untpd.Tree = {

        def adjustLeaf(tree: untpd.Tree): untpd.Tree = referenced.typeAtPos.get(tree.pos.toSynthetic) match {
          case Some(tpe) => untpd.TypedSplice(tree.withType(tpe))
          case none => tree
        }

        def adjustForAccessor(ref: untpd.RefTree) =
          referenced.accessorAtPos.get(ref.pos.toSynthetic) match {
            case Some(acc) =>
              def accessorRef = untpd.TypedSplice(tpd.ref(acc))
              acc.name match {
                case InlineAccessorName(UniqueInlineName(_, _)) =>
                  // In this case we are seeing a pair like this:
                  //   untyped                typed
                  //   t.x                    inline$x(t)
                  // Drop the selection, since it is part of the accessor
                  val Select(qual, _) = ref
                  untpd.Apply(accessorRef, qual :: Nil)
                case _ =>
                  accessorRef
              }
            case none => ref
          }

        def adjustQualifier(tree: untpd.Tree): untpd.Tree = tree match {
          case tree @ Ident(name1) =>
            referenced.typeAtPos.get(tree.pos.startPos) match {
              case Some(tp: ThisType) =>
                val qual = untpd.TypedSplice(This(tp.cls).withPos(tree.pos.startPos))
                cpy.Select(tree)(qual, name1)
              case none =>
                tree
            }
          case tree => tree
        }

        def isAccessorLHS(lhs: untpd.Tree): Boolean = lhs match {
          case lhs: untpd.Apply => isAccessorLHS(lhs.fun)
          case lhs: untpd.TypeApply => isAccessorLHS(lhs.fun)
          case lhs: untpd.RefTree => lhs.name.is(InlineAccessorName)
          case untpd.TypedSplice(lhs1) => lhs1.symbol.name.is(InlineAccessorName)
          case _ => false
        }

        val tree1 = super.transform(tree)
        tree1 match {
          case This(_) =>
            adjustLeaf(tree1)
          case tree1: untpd.Ident =>
            adjustQualifier(adjustLeaf(adjustForAccessor(tree1)))
          case tree1: untpd.Select =>
            adjustForAccessor(tree1)
          case Assign(lhs, rhs) if isAccessorLHS(lhs) =>
            cpy.Apply(tree1)(lhs, rhs :: Nil)
          case tree: untpd.DerivedTypeTree =>
            inlining.println(i"inlining derived $tree --> ${ctx.typer.typed(tree)}")
            untpd.TypedSplice(ctx.typer.typed(tree))
          case _ =>
            tree1
        }
      }
    }
    val implicitBindings =
      for (iref <- referenced.implicitRefs.toList) yield {
        val localImplicit = iref.symbol.asTerm.copy(
          owner = inlineMethod,
          name = UniqueInlineName.fresh(iref.symbol.name.asTermName),
          flags = Implicit | Method | Stable | iref.symbol.flags & (Inline | Erased),
          info = iref.tpe.widen.ensureMethodic,
          coord = inlineMethod.pos).asTerm
        val idef = polyDefDef(localImplicit, tps => vrefss =>
            iref.appliedToTypes(tps).appliedToArgss(vrefss))
        if (localImplicit.is(Inline)) {
          // produce a Body annotation for inlining
          def untype(tree: Tree): untpd.Tree = tree match {
            case Apply(fn, args) => untpd.cpy.Apply(tree)(untype(fn), args)
            case TypeApply(fn, args) => untpd.cpy.TypeApply(tree)(untype(fn), args)
            case _ => untpd.TypedSplice(tree)
          }
          val inlineBody = tpd.UntypedSplice(untype(idef.rhs)).withType(idef.rhs.tpe)
          inlining.println(i"body annot for $idef: $inlineBody")
          localImplicit.addAnnotation(ConcreteBodyAnnotation(inlineBody))
        }
        idef
      }
    val untpdSplice = tpd.UntypedSplice(addRefs.transform(original)).withType(typed.tpe)
    seq(implicitBindings, untpdSplice)
  }
}