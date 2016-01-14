package dotty.tools.dotc
package transform

import TreeTransforms._
import core.Names.Name
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Contexts._
import core.Symbols._
import core.Types._
import core.Flags._
import core.Constants._
import core.StdNames._
import core.Decorators._
import core.TypeErasure.isErasedType
import core.Phases.Phase
import typer._
import typer.ErrorReporting._
import reporting.ThrowingReporter
import ast.Trees._
import ast.{tpd, untpd}
import util.SourcePosition
import collection.mutable
import ProtoTypes._
import config.Printers
import java.lang.AssertionError
import scala.util.control.NonFatal

/** Run by -Ycheck option after a given phase, this class retypes all syntax trees
 *  and verifies that the type of each tree node so obtained conforms to the type found in the tree node.
 *  It also performs the following checks:
 *
 *   - The owner of each definition is the same as the owner of the current typing context.
 *   - Ident nodes do not refer to a denotation that would need a select to be accessible
 *     (see tpd.needsSelect).
 *   - After typer, identifiers and select nodes refer to terms only (all types should be
 *     represented as TypeTrees then).
 */
class TreeChecker extends Phase with SymTransformer {
  import ast.tpd._


  private val seenClasses = collection.mutable.HashMap[String, Symbol]()
  private val seenModuleVals = collection.mutable.HashMap[String, Symbol]()

  def isValidJVMName(name: Name) =
      !name.exists(c => c == '.' || c == ';' || c =='[' || c == '/')

  def isValidJVMMethodName(name: Name) =
      !name.exists(c => c == '.' || c == ';' || c =='[' || c == '/' || c == '<' || c == '>')

  def printError(str: String)(implicit ctx: Context) = {
    ctx.println(Console.RED + "[error] " + Console.WHITE  + str)
  }

  val NoSuperClass = Trait | Package

  def testDuplicate(sym: Symbol, registry: mutable.Map[String, Symbol], typ: String)(implicit ctx: Context) = {
    val name = sym.fullName.toString
    if (this.flatClasses && registry.contains(name))
        printError(s"$typ defined twice $sym ${sym.id} ${registry(name).id}")
    registry(name) = sym
  }

  def checkCompanion(symd: SymDenotation)(implicit ctx: Context): Unit = {
    val cur = symd.linkedClass
    val prev = ctx.atPhase(ctx.phase.prev) {
      ct => {
        implicit val ctx: Context = ct.addMode(Mode.FutureDefsOK)
        symd.symbol.linkedClass
      }
    }

    if (prev.exists)
      assert(cur.exists, i"companion disappeared from $symd")
  }

  def transformSym(symd: SymDenotation)(implicit ctx: Context): SymDenotation = {
    val sym = symd.symbol

    if (sym.isClass && !sym.isAbsent) {
      val validSuperclass = sym.isPrimitiveValueClass ||  defn.syntheticCoreClasses.contains(sym) ||
        (sym eq defn.ObjectClass) || (sym is NoSuperClass) || (sym.asClass.superClass.exists)
      if (!validSuperclass)
        printError(s"$sym has no superclass set")

      testDuplicate(sym, seenClasses, "class")
    }

    if (sym.is(Method) && sym.is(Deferred) && sym.is(Private))
      assert(false, s"$sym is both Deferred and Private")

    checkCompanion(symd)

    symd
  }

  def phaseName: String = "Ycheck"

  def run(implicit ctx: Context): Unit = {
    check(ctx.allPhases, ctx)
  }

  private def previousPhases(phases: List[Phase])(implicit ctx: Context): List[Phase] = phases match {
    case (phase: TreeTransformer) :: phases1 =>
      val subPhases = phase.miniPhases
      val previousSubPhases = previousPhases(subPhases.toList)
      if (previousSubPhases.length == subPhases.length) previousSubPhases ::: previousPhases(phases1)
      else previousSubPhases
    case phase :: phases1 if phase ne ctx.phase =>
      phase :: previousPhases(phases1)
    case _ =>
      Nil
  }

  def check(phasesToRun: Seq[Phase], ctx: Context) = {
    val prevPhase = ctx.phase.prev // can be a mini-phase
    val squahsedPhase = ctx.squashed(prevPhase)
    ctx.println(s"checking ${ctx.compilationUnit} after phase ${squahsedPhase}")
    val checkingCtx = ctx.fresh
      .setTyperState(ctx.typerState.withReporter(new ThrowingReporter(ctx.reporter)))
    val checker = new Checker(previousPhases(phasesToRun.toList)(ctx))
    try checker.typedExpr(ctx.compilationUnit.tpdTree)(checkingCtx)
    catch {
      case NonFatal(ex) =>     //TODO CHECK. Check that we are bootstrapped
        implicit val ctx: Context = checkingCtx
        ctx.println(i"*** error while checking ${ctx.compilationUnit} after phase ${checkingCtx.phase.prev} ***")
        ctx.println(ex.toString)
        ctx.println(ex.getStackTrace.take(30).deep.mkString("\n"))
        ctx.println("<<<")
        throw ex
    }
  }

  class Checker(phasesToCheck: Seq[Phase]) extends ReTyper {

    val nowDefinedSyms = new mutable.HashSet[Symbol]
    val everDefinedSyms = new mutable.HashMap[Symbol, Tree]

    def withDefinedSym[T](tree: untpd.Tree)(op: => T)(implicit ctx: Context): T = tree match {
      case tree: DefTree =>
        val sym = tree.symbol
        assert(isValidJVMName(sym.name), s"${sym.fullName} name is invalid on jvm")
        everDefinedSyms.get(sym) match {
          case Some(t)  =>
            if (t ne tree)
              ctx.warning(i"symbol ${sym.fullName} is defined at least twice in different parts of AST")
            // should become an error
          case None =>
            everDefinedSyms(sym) = tree
        }
        assert(!nowDefinedSyms.contains(sym), i"doubly defined symbol: ${sym.fullName} in $tree")

        if (ctx.settings.YcheckMods.value) {
          tree match {
            case t: MemberDef =>
              if (t.name ne sym.name) ctx.warning(s"symbol ${sym.fullName} name doesn't correspond to AST: ${t}")
              if (sym.flags != t.mods.flags) ctx.warning(s"symbol ${sym.fullName} flags ${sym.flags} doesn't match AST definition flags ${t.mods.flags}")
            // todo: compare trees inside annotations
            case _ =>
          }
        }

        nowDefinedSyms += tree.symbol
        //ctx.println(i"defined: ${tree.symbol}")
        val res = op
        nowDefinedSyms -= tree.symbol
        //ctx.println(i"undefined: ${tree.symbol}")
        res
      case _ => op
    }

    def withDefinedSyms[T](trees: List[untpd.Tree])(op: => T)(implicit ctx: Context) =
      trees.foldRightBN(op)(withDefinedSym(_)(_))

    def withDefinedSymss[T](vparamss: List[List[untpd.ValDef]])(op: => T)(implicit ctx: Context): T =
      vparamss.foldRightBN(op)(withDefinedSyms(_)(_))

    def assertDefined(tree: untpd.Tree)(implicit ctx: Context) =
      if (tree.symbol.maybeOwner.isTerm)
        assert(nowDefinedSyms contains tree.symbol, i"undefined symbol ${tree.symbol}")

    override def typedUnadapted(tree: untpd.Tree, pt: Type)(implicit ctx: Context): tpd.Tree = {
      val res = tree match {
        case _: untpd.UnApply =>
          // can't recheck patterns
          tree.asInstanceOf[tpd.Tree]
        case _: untpd.TypedSplice | _: untpd.Thicket | _: EmptyValDef[_] =>
          super.typedUnadapted(tree)
        case _ if tree.isType =>
          promote(tree)
        case _ =>
          val tree1 = super.typedUnadapted(tree, pt)
          def isSubType(tp1: Type, tp2: Type) =
            (tp1 eq tp2) || // accept NoType / NoType
            (tp1 <:< tp2)
          def divergenceMsg(tp1: Type, tp2: Type) =
            s"""Types differ
               |Original type : ${tree.typeOpt.show}
               |After checking: ${tree1.tpe.show}
               |Original tree : ${tree.show}
               |After checking: ${tree1.show}
               |Why different :
             """.stripMargin + core.TypeComparer.explained((tp1 <:< tp2)(_))
          if (tree.hasType) // it might not be typed because Typer sometimes constructs new untyped trees and resubmits them to typedUnadapted
            assert(isSubType(tree1.tpe, tree.typeOpt), divergenceMsg(tree1.tpe, tree.typeOpt))
          tree1
      }
      checkNoOrphans(res.tpe)
      phasesToCheck.foreach(_.checkPostCondition(res))
      res
    }

    /** Check that PolyParams and MethodParams refer to an enclosing type */
    def checkNoOrphans(tp: Type)(implicit ctx: Context) = new TypeMap() {
      val definedBinders = mutable.Set[Type]()
      def apply(tp: Type): Type = {
        tp match {
          case tp: BindingType =>
            definedBinders += tp
            mapOver(tp)
            definedBinders -= tp
          case tp: ParamType =>
            assert(definedBinders.contains(tp.binder), s"orphan param: $tp")
          case tp: TypeVar =>
            apply(tp.underlying)
          case _ =>
            mapOver(tp)
        }
        tp
      }
    }.apply(tp)

    def checkNotRepeated(tree: Tree)(implicit ctx: Context): tree.type = {
      assert(!tree.tpe.widen.isRepeatedParam, i"repeated parameter type not allowed here: $tree")
      tree
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.isTerm || !ctx.isAfterTyper, tree.show + " at " + ctx.phase)
      assert(tree.isType || !needsSelect(tree.tpe), i"bad type ${tree.tpe} for $tree # ${tree.uniqueId}")
      assertDefined(tree)
      checkNotRepeated(super.typedIdent(tree, pt))
    }

    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      assert(tree.isTerm || !ctx.isAfterTyper, tree.show + " at " + ctx.phase)
      checkNotRepeated(super.typedSelect(tree, pt))
    }

    override def typedThis(tree: untpd.This)(implicit ctx: Context) = {
      val res = super.typedThis(tree)
      val cls = res.symbol
      assert(cls.isStaticOwner || ctx.owner.isContainedIn(cls), i"error while typing $tree, ${ctx.owner} is not contained in $cls")
      res
    }

    private def checkOwner(tree: untpd.Tree)(implicit ctx: Context): Unit = {
      def ownerMatches(symOwner: Symbol, ctxOwner: Symbol): Boolean =
        symOwner == ctxOwner ||
        ctxOwner.isWeakOwner && ownerMatches(symOwner, ctxOwner.owner) ||
        ctx.phase.labelsReordered && symOwner.isWeakOwner && ownerMatches(symOwner.owner, ctxOwner)
      assert(ownerMatches(tree.symbol.owner, ctx.owner),
        i"bad owner; ${tree.symbol} has owner ${tree.symbol.owner}, expected was ${ctx.owner}\n" +
        i"owner chain = ${tree.symbol.ownersIterator.toList}%, %, ctxOwners = ${ctx.outersIterator.map(_.owner).toList}%, %")
    }

    override def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(implicit ctx: Context) = {
      val TypeDef(_, impl @ Template(constr, _, _, _)) = cdef
      assert(cdef.symbol == cls)
      assert(impl.symbol.owner == cls)
      assert(constr.symbol.owner == cls)
      assert(cls.primaryConstructor == constr.symbol, i"mismatch, primary constructor ${cls.primaryConstructor}, in tree = ${constr.symbol}")
      checkOwner(impl)
      checkOwner(impl.constr)

      def isNonMagicalMethod(x: Symbol) =
        x.is(Method) &&
          !x.isCompanionMethod &&
          !x.isValueClassConvertMethod &&
          x != defn.newRefArrayMethod

      val symbolsNotDefined = cls.classInfo.decls.toSet.filter(isNonMagicalMethod) -- impl.body.map(_.symbol) - constr.symbol

      assert(symbolsNotDefined.isEmpty, i" $cls tree does not define methods: $symbolsNotDefined")

      super.typedClassDef(cdef, cls)
    }

    override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) =
      withDefinedSyms(ddef.tparams) {
        withDefinedSymss(ddef.vparamss) {
          if (!sym.isClassConstructor) assert(isValidJVMMethodName(sym.name), s"${sym.fullName} name is invalid on jvm")
          super.typedDefDef(ddef, sym)
        }
      }

    override def typedCase(tree: untpd.CaseDef, pt: Type, selType: Type, gadtSyms: Set[Symbol])(implicit ctx: Context): CaseDef = {
      withDefinedSyms(tree.pat.asInstanceOf[tpd.Tree].filterSubTrees(_.isInstanceOf[ast.Trees.Bind[_]])) {
        super.typedCase(tree, pt, selType, gadtSyms)
      }
    }

    override def typedBlock(tree: untpd.Block, pt: Type)(implicit ctx: Context) =
      withDefinedSyms(tree.stats) { super.typedBlock(tree, pt) }

    /** Check that all defined symbols have legal owners.
     *  An owner is legal if it is either the same as the context's owner
     *  or there's an owner chain of valdefs starting at the context's owner and
     *  reaching up to the symbol's owner. The reason for this relaxed matching
     *  is that we should be able to pull out an expression as an initializer
     *  of a helper value without having to do a change owner traversal of the expression.
     */
    override def typedStats(trees: List[untpd.Tree], exprOwner: Symbol)(implicit ctx: Context): List[Tree] = {
      for (tree <- trees) tree match {
        case tree: untpd.DefTree => checkOwner(tree)
        case _: untpd.Thicket => assert(false, i"unexpanded thicket $tree in statement sequence $trees%\n%")
        case _ =>
      }
      super.typedStats(trees, exprOwner)
    }

    override def ensureNoLocalRefs(tree: Tree, pt: Type, localSyms: => List[Symbol], forcedDefined: Boolean = false)(implicit ctx: Context): Tree =
      tree

    override def adapt(tree: Tree, pt: Type, original: untpd.Tree = untpd.EmptyTree)(implicit ctx: Context) = {
      def isPrimaryConstructorReturn =
        ctx.owner.isPrimaryConstructor && pt.isRef(ctx.owner.owner) && tree.tpe.isRef(defn.UnitClass)
      if (ctx.mode.isExpr &&
          !tree.isEmpty &&
          !isPrimaryConstructorReturn &&
          !pt.isInstanceOf[FunProto])
        assert(tree.tpe <:< pt,
            s"error at ${sourcePos(tree.pos)}\n" +
            err.typeMismatchStr(tree.tpe, pt) + "\ntree = " + tree)
      tree
    }
  }
}
