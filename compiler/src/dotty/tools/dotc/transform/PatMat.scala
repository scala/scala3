package dotty.tools.dotc
package transform

import core._
import TreeTransforms._
import collection.mutable
import SymDenotations._, Symbols._, Contexts._, Types._, Names._, StdNames._, NameOps._
import ast.Trees._
import util.Positions._
import typer.Applications.{isProductMatch, isGetMatch, productSelectors}
import SymUtils._
import Flags._, Constants._
import Decorators._
import patmat.Space
import NameKinds.{UniqueNameKind, PatMatStdBinderName, PatMatCaseName}
import config.Printers.patmatch

object PatMat {
  import ast.tpd._

  final val selfCheck = true

  abstract class Node

  class Translator(resultType: Type, trans: TreeTransform)(implicit ctx: Context, info: TransformerInfo) {

    def patmatGenerated(sym: Symbol) =
      sym.is(Synthetic) &&
      (sym.is(Label) || sym.name.is(PatMatStdBinderName))

    val sanitize = new TypeMap {
      def apply(t: Type): Type = t.widenExpr match {
        case t: TermRef
        if patmatGenerated(t.symbol) || t.info.isInstanceOf[ExprType] => apply(t.info)
        case t => mapOver(t)
      }
    }

    case class BinderInfo(rhs: Tree)

    val binding = mutable.Map[Symbol, AnyRef/*Tree | Node*/]()

    def rhs(sym: Symbol) = {
      assert(!sym.is(Label))
      binding(sym).asInstanceOf[Tree]
    }

    def labelled(sym: Symbol) = {
      assert(sym.is(Label))
      binding(sym).asInstanceOf[Node]
    }

      // assert(owner ne null); assert(owner ne NoSymbol)
    def freshSym(info: Type, pos: Position, owner: Symbol = ctx.owner): TermSymbol =
      ctx.newSymbol(owner, PatMatStdBinderName.fresh(), Synthetic | Case, info, coord = pos)

    def freshLabel(info: Type, owner: Symbol = ctx.owner): TermSymbol =
      ctx.newSymbol(owner, PatMatCaseName.fresh(), Synthetic | Label | Method, info)

    def newBinder(rhs: Tree) = {
      val sym = freshSym(sanitize(rhs.tpe), rhs.pos)
      binding(sym) = rhs
      sym
    }

    def newLabel(body: Node) = {
      val label = freshLabel(MethodType(Nil, sanitize(body.tpe)))
      binding(label) = body
      label
    }

    private var nxId = 0

    sealed abstract class Node {
      def tpe: Type
      val id = nxId
      nxId += 1
    }

    abstract case class Test(var scrutinee: Tree, var onSuccess: Node, var onFailure: Node) extends Node {
      def this(scrut: Symbol, ons: Node, onf: Node) = this(ref(scrut), ons, onf)
      def condition: Tree
      def pos: Position
      val tpe =
        try sanitize(onSuccess.tpe | onFailure.tpe)
        catch {
          case ex: AssertionError =>
            println(i"cannot | $onSuccess: ${onSuccess.tpe} with $onFailure ${onFailure.tpe}")
            throw ex
        }
    }

    class UnApplyTest(scrut: Symbol, ons: Node, onf: Node) extends Test(scrut, ons, onf) {
      def pos = scrut.pos
      def condition = scrutinee
        .select(nme.isEmpty, _.info.isParameterless)
        .select(nme.UNARY_!, _.info.isParameterless)
      override def toString = i"UnApplyTest($scrutinee)"
    }

    class TypeTest(scrut: Symbol, tpt: Tree, ons: Node, onf: Node) extends Test(scrut, ons, onf) {
      def pos = tpt.pos
      private val expectedTp = tpt.tpe

      private def outerTestNeeded(implicit ctx: Context): Boolean = {
        // See the test for SI-7214 for motivation for dealias. Later `treeCondStrategy#outerTest`
        // generates an outer test based on `patType.prefix` with automatically dealises.
        expectedTp.dealias match {
          case tref @ TypeRef(pre: SingletonType, name) =>
            tref.symbol.isClass &&
            ExplicitOuter.needsOuterIfReferenced(tref.symbol.asClass)
          case _ =>
            false
        }
      }

      private def outerTest: Tree = trans.transformFollowingDeep {
        val expectedOuter = singleton(expectedTp.normalizedPrefix)
        val expectedClass = expectedTp.dealias.classSymbol.asClass
        ExplicitOuter.ensureOuterAccessors(expectedClass)(ctx.withPhase(ctx.explicitOuterPhase.next))
        scrutinee.ensureConforms(expectedTp)
          .outerSelect(1, expectedOuter.tpe.widen)
          .select(defn.Object_eq)
          .appliedTo(expectedOuter)
      }

      def condition = expectedTp.dealias match {
        case expectedTp: SingletonType =>
          val test =
            if (expectedTp.widen.derivesFrom(defn.ObjectClass))
              scrutinee.ensureConforms(defn.ObjectType).select(defn.Object_eq)
            else
              scrutinee.select(defn.Any_==)
          test.appliedTo(singleton(expectedTp))
        case _ =>
          val typeTest = scrutinee.select(defn.Any_typeTest).appliedToType(tpt.tpe)
          if (outerTestNeeded) typeTest.and(outerTest) else typeTest
      }
      override def toString = i"TypeTest($scrutinee: $tpt)"
    }

    class EqualTest(scrut: Symbol, tree: Tree, ons: Node, onf: Node) extends Test(scrut, ons, onf) {
      def pos = tree.pos
      def condition = applyOverloaded(tree, nme.EQ, scrutinee :: Nil, Nil, defn.BooleanType)
      override def toString = i"EqualTest($tree == $scrutinee)"
    }

    class LengthTest(scrut: Symbol, len: Int, exact: Boolean, ons: Node, onf: Node) extends Test(scrut, ons, onf) {
      def pos = scrut.pos
      def condition = scrutinee
        .select(defn.Any_!=)
        .appliedTo(Literal(Constant(null)))
        .select(defn.Boolean_&&)
        .appliedTo(
          scrutinee
            .select(defn.Seq_lengthCompare.matchingMember(scrutinee.tpe))
            .appliedTo(Literal(Constant(len)))
            .select(if (exact) defn.Int_== else defn.Int_>=)
            .appliedTo(Literal(Constant(0))))
      override def toString =
        i"Lengthtest($scrutinee.length ${if (exact) "==" else ">="} $len)"
    }

    class GuardTest(val cond: Tree, ons: Node, onf: Node) extends Test(cond, ons, onf) {
      def pos = condition.pos
      def condition = scrutinee
      override def toString = i"GuardTest($scrutinee)"
    }

    case class LetNode(sym: TermSymbol, var body: Node) extends Node {
      val tpe = sanitize(body.tpe)
    }

    case class BodyNode(var tree: Tree) extends Node {
      val tpe = tree.tpe
    }

    case class CallNode(label: TermSymbol) extends Node {
      val tpe = label.info.finalResultType
    }

    /** A conservative approximation of which patterns do not discern anything.
      * They are discarded during the translation.
      */
    object WildcardPattern {
      def unapply(pat: Tree): Boolean = pat match {
        case Typed(_, tpt) if tpt.tpe.isRepeatedParam => true
        case Bind(nme.WILDCARD, WildcardPattern()) => true // don't skip when binding an interesting symbol!
        case t if isWildcardArg(t)                 => true
        case x: BackquotedIdent                    => false
        case x: Ident                              => x.name.isVariableName
        case Alternative(ps)                       => ps.forall(unapply)
        case EmptyTree                             => true
        case _                                     => false
      }
    }

    object VarArgPattern {
      def unapply(pat: Tree): Option[Tree] = swapBind(pat) match {
        case Typed(pat1, tpt) if tpt.tpe.isRepeatedParam => Some(pat1)
        case _ => None
      }
    }

    def isSyntheticScala2Unapply(sym: Symbol) =
      sym.is(SyntheticCase) && sym.owner.is(Scala2x)

    def swapBind(tree: Tree) = tree match {
      case Bind(name, Typed(pat, tpt)) => Typed(cpy.Bind(tree)(name, pat), tpt)
      case _ => tree
    }

    def translatePattern(scrutinee: Symbol, tree: Tree, onSuccess: Node, onFailure: Node): Node = {

      def translateArgs(selectors: List[Tree], args: List[Tree], onSuccess: Node): Node =
        args match {
          case arg :: args1 =>
            val selector :: selectors1 = selectors
            letAbstract(selector)(
              translatePattern(_, arg, translateArgs(selectors1, args1, onSuccess), onFailure))
          case Nil => onSuccess
        }

      def translateElems(seqSym: Symbol, args: List[Tree], exact: Boolean, onSuccess: Node) = {
        val selectors = args.indices.toList.map(idx =>
          ref(seqSym).select(nme.apply).appliedTo(Literal(Constant(idx))))
        new LengthTest(seqSym, args.length, exact,
          translateArgs(selectors, args, onSuccess), onFailure)
      }

      def translateUnApplySeq(getResult: Symbol, args: List[Tree]): Node = args.lastOption match {
        case Some(VarArgPattern(arg)) =>
          val matchRemaining =
            if (args.length == 1)
              translatePattern(getResult, arg, onSuccess, onFailure)
            else {
              val dropped = ref(getResult)
                .select(defn.Seq_drop.matchingMember(getResult.info))
                .appliedTo(Literal(Constant(args.length - 1)))
              letAbstract(dropped) { droppedResult =>
                translatePattern(droppedResult, arg, onSuccess, onFailure)
              }
            }
          translateElems(getResult, args.init, exact = false, matchRemaining)
        case _ =>
          translateElems(getResult, args, exact = true, onSuccess)
      }

      def translateUnApply(unapp: Tree, args: List[Tree]): Node = {
        def caseClass = unapp.symbol.owner.linkedClass
        lazy val caseAccessors = caseClass.caseAccessors.filter(_.is(Method))
        if (isSyntheticScala2Unapply(unapp.symbol) && caseAccessors.length == args.length)
          translateArgs(caseAccessors.map(ref(scrutinee).select(_)), args, onSuccess)
        else if (unapp.tpe.isRef(defn.BooleanClass))
          new GuardTest(unapp, onSuccess, onFailure)
        else {
          letAbstract(unapp) { unappResult =>
            val isUnapplySeq = unapp.symbol.name == nme.unapplySeq
            if (isProductMatch(unapp.tpe.widen, args.length) && !isUnapplySeq) {
              val selectors = productSelectors(unapp.tpe).take(args.length)
                .map(ref(unappResult).select(_))
              translateArgs(selectors, args, onSuccess)
            }
            else {
              assert(isGetMatch(unapp.tpe))
              val get = ref(unappResult).select(nme.get, _.info.isParameterless)
              letAbstract(get) { getResult =>
                if (isUnapplySeq)
                  translateUnApplySeq(getResult, args)
                else {
                  val selectors =
                    if (args.tail.isEmpty) ref(getResult) :: Nil
                    else productSelectors(get.tpe).map(ref(getResult).select(_))
                  new UnApplyTest(unappResult, translateArgs(selectors, args, onSuccess), onFailure)
                }
              }
            }
          }
        }
      }

      swapBind(tree) match {
        case Typed(pat, tpt) =>
          new TypeTest(scrutinee, tpt,
            letAbstract(ref(scrutinee).asInstance(tpt.tpe))(casted =>
              translatePattern(casted, pat, onSuccess, onFailure)),
            onFailure)
        case UnApply(extractor, implicits, args) =>
          val mt @ MethodType(_) = extractor.tpe.widen
          var unapp = extractor.appliedTo(ref(scrutinee).ensureConforms(mt.paramInfos.head))
          if (implicits.nonEmpty) unapp = unapp.appliedToArgs(implicits)
          translateUnApply(unapp, args)
        case Bind(name, body) =>
          val body1 = translatePattern(scrutinee, body, onSuccess, onFailure)
          if (name == nme.WILDCARD) body1
          else {
            val bound = tree.symbol.asTerm
            binding(bound) = ref(scrutinee)
            LetNode(bound, body1)
          }
        case Alternative(alts) =>
          labelAbstract(onSuccess) { ons =>
            (alts :\ onFailure) { (alt, onf) =>
              labelAbstract(onf) { onf1 =>
                translatePattern(scrutinee, alt, ons, onf1)
              }
            }
          }
        case WildcardPattern() =>
          onSuccess
        case _ =>
          new EqualTest(scrutinee, tree, onSuccess, onFailure)
      }
    }

    def translateCaseDef(scrutinee: Symbol, cdef: CaseDef, onFailure: Node): Node =
      labelAbstract(onFailure) { onf =>
        var onSuccess: Node = BodyNode(cdef.body)
        if (!cdef.guard.isEmpty) onSuccess = new GuardTest(cdef.guard, onSuccess, onf)
        translatePattern(scrutinee, cdef.pat, onSuccess, onf)
      }

    def labelAbstract(node: Node)(in: Node => Node): Node = {
      val label = newLabel(node)
      LetNode(label, in(CallNode(label)))
    }

    def letAbstract(rhs: Tree)(in: Symbol => Node): Node = {
      val sym = newBinder(rhs)
      LetNode(sym, in(sym))
    }

    def referenceCount(node: Node): collection.Map[Symbol, Int] = {
      val count = new mutable.HashMap[Symbol, Int] {
        override def default(key: Symbol) = 0
      }
      val refCounter = new TreeTraverser {
        def traverse(tree: Tree)(implicit ctx: Context) = tree match {
          case tree: Ident =>
            if (binding contains tree.symbol) count(tree.symbol) += 1
          case _ =>
            traverseChildren(tree)
        }
      }
      def traverse(node: Node): Unit = node match {
        case node: Test =>
          refCounter.traverse(node.scrutinee)
          traverse(node.onSuccess)
          traverse(node.onFailure)
        case LetNode(sym, body) =>
          traverse(body)
          if (count(sym) != 0 || !patmatGenerated(sym)) {
            binding(sym) match {
              case tree: Tree => refCounter.traverse(tree)
              case node: Node => traverse(node)
            }
          }
        case BodyNode(tree) =>
          ;
        case CallNode(label) =>
          count(label) += 1
      }
      traverse(node)
      count
    }

    def specialize(node: Node): Node = {
      val refCount = referenceCount(node)
      val LetNode(topSym, _) = node
      def toDrop(sym: Symbol) =
        binding.contains(sym) && patmatGenerated(sym) && refCount(sym) <= 1 && sym != topSym
      val treeMap = new TreeMap {
        override def transform(tree: Tree)(implicit ctx: Context) = tree match {
          case tree: Ident =>
            val sym = tree.symbol
            if (toDrop(sym)) transform(rhs(sym)) else tree
          case _ =>
            super.transform(tree)
        }
      }
      def transform(node: Node): Node = node match {
        case node: Test =>
          node.scrutinee = treeMap.transform(node.scrutinee)
          node.onSuccess = transform(node.onSuccess)
          node.onFailure = transform(node.onFailure)
          node
        case node @ LetNode(sym, body) =>
          val body1 = transform(body)
          if (toDrop(sym)) body1
          else {
            binding(sym) = binding(sym) match {
              case tree: Tree => treeMap.transform(tree)
              case node: Node => transform(node)
            }
            node.body = body1
            node
          }
        case node @ BodyNode(tree) =>
          node.tree = treeMap.transform(tree)
          node
        case CallNode(label) =>
          if (refCount(label) == 1) transform(labelled(label))
          else node
      }
      val LetNode(sym, body) = node
      transform(node)
    }

    val emitted = mutable.Set[Int]()

    def emit(node: Node): Tree = {
      if (selfCheck) {
        assert(node.isInstanceOf[CallNode] || !emitted.contains(node.id), node.id)
        emitted += node.id
      }
      node match {
        case node: Test =>
          If(node.condition, emit(node.onSuccess), emit(node.onFailure)).withPos(node.pos)
        case node @ LetNode(sym, body) =>
          val symDef =
            if (sym.is(Label)) DefDef(sym, emit(labelled(sym)))
            else ValDef(sym, rhs(sym).ensureConforms(sym.info))
          seq(symDef :: Nil, emit(body))
        case BodyNode(tree) =>
          tree
        case CallNode(label) =>
          ref(label).ensureApplied
      }
    }

    def show(node: Node): String = {
      val refCount = referenceCount(node)
      val sb = new StringBuilder
      val seen = mutable.Set[Int]()
      def showNode(node: Node): Unit =
        if (!seen.contains(node.id)) {
          seen += node.id
          sb append s"\n${node.id}: "
          node match {
            case node: Test =>
              sb.append(i"$node(${node.onSuccess.id}, ${node.onFailure.id})")
              showNode(node.onSuccess)
              showNode(node.onFailure)
            case LetNode(sym, body) =>
              val rhsStr = binding(sym) match {
                case tree: Tree => tree.show
                case node: Node => node.id.toString
              }
              sb.append(s"Let($sym = $rhsStr}, ${body.id})")
              sb.append(s", refcount = ${refCount(sym)}")
              showNode(body)
              binding(sym) match {
                case tree: Tree =>
                case node: Node => showNode(node)
              }
            case BodyNode(tree) =>
              sb.append(tree.show)
            case CallNode(label) =>
              sb.append(s"Call($label)")
          }
        }
      showNode(node)
      sb.toString
    }

    def translateMatch(tree: Match): Tree = {
      val raw = letAbstract(tree.selector) { scrutinee =>
        val matchError: Node = BodyNode(Throw(New(defn.MatchErrorType, ref(scrutinee) :: Nil)))
        (tree.cases :\ matchError)(translateCaseDef(scrutinee, _, _))
      }
      patmatch.println(i"Nodes for $tree:${show(raw)}")
      val specialized = specialize(raw)
      patmatch.println(s"Specialized: ${show(specialized)}")
      emit(specialized)
    }
  }
}

class PatMat extends MiniPhaseTransform {
  import ast.tpd._
  import PatMat._

  override def phaseName = "patternMatcher"
  override def runsAfter = Set(classOf[ElimRepeated])
  override def runsAfterGroupsOf = Set(classOf[TailRec]) // tailrec is not capable of reversing the patmat tranformation made for tree

  override def transformMatch(tree: Match)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val translated = new Translator(tree.tpe, this).translateMatch(tree)

    // check exhaustivity and unreachability
    val engine = new patmat.SpaceEngine

    if (engine.checkable(tree)) {
      engine.checkExhaustivity(tree)
      engine.checkRedundancy(tree)
    }

    translated.ensureConforms(tree.tpe)
  }
}
