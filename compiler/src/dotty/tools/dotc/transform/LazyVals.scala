package dotty.tools.dotc
package transform

import java.util.IdentityHashMap

import ast.tpd
import core.Annotations.Annotation
import core.Constants.Constant
import core.Contexts._
import core.Decorators._
import core.DenotTransformers.IdentityDenotTransformer
import core.Flags._
import core.NameKinds.{LazyBitMapName, LazyLocalInitName, LazyLocalName, ExpandedName}
import core.StdNames.nme
import core.Symbols._
import core.Types._
import core.{Names, StdNames}
import transform.MegaPhase.MiniPhase
import transform.SymUtils._
import scala.collection.mutable

class LazyVals extends MiniPhase with IdentityDenotTransformer {
  import LazyVals._
  import tpd._

  /** this map contains mutable state of transformation: OffsetDefs to be appended to companion object definitions,
    * and number of bits currently used */
  class OffsetInfo(var defs: List[Tree], var ord:Int)
  private val appendOffsetDefs = mutable.Map.empty[Symbol, OffsetInfo]

  override def phaseName: String = LazyVals.name

  override def description: String = LazyVals.description

  /** List of names of phases that should have finished processing of tree
    * before this phase starts processing same tree */
  override def runsAfter: Set[String] = Set(Mixin.name, CollectNullableFields.name)

  override def changesMembers: Boolean = true  // the phase adds lazy val accessors

  val containerFlags: FlagSet = Synthetic | Mutable | Lazy
  val initFlags: FlagSet      = Synthetic | Method

  val containerFlagsMask: FlagSet = Method | Lazy | Accessor | Module

  /** A map of lazy values to the fields they should null after initialization. */
  private var lazyValNullables: IdentityHashMap[Symbol, mutable.ListBuffer[Symbol]] | Uninitialized = _
  private def nullableFor(sym: Symbol)(using Context) = {
    // optimisation: value only used once, we can remove the value from the map
    val nullables = lazyValNullables.nn.remove(sym)
    if (nullables == null) Nil
    else nullables.toList
  }


  override def prepareForUnit(tree: Tree)(using Context): Context = {
    if (lazyValNullables == null)
      lazyValNullables = ctx.base.collectNullableFieldsPhase.asInstanceOf[CollectNullableFields].lazyValNullables
    ctx
  }

  override def transformDefDef(tree: DefDef)(using Context): Tree =
   transformLazyVal(tree)


  override def transformValDef(tree: ValDef)(using Context): Tree =
    transformLazyVal(tree)

  def transformLazyVal(tree: ValOrDefDef)(using Context): Tree = {
    val sym = tree.symbol
    if (!sym.is(Lazy) ||
        sym.owner.is(Trait) || // val is accessor, lazy field will be implemented in subclass
        (sym.isStatic && sym.is(Module, butNot = Method))) // static module vals are implemented in the JVM by lazy loading
      tree
    else {
      val isField = sym.owner.isClass
      if (isField)
        if sym.isAllOf(SyntheticModule)
           && sym.allOverriddenSymbols.isEmpty
           && !sym.name.is(ExpandedName) then
          // I am not sure what the conditions for this optimization should be.
          // It was applied for all synthetic objects, but this is clearly false, as t704 demonstrates.
          // It seems we have to at least exclude synthetic objects that derive from mixins.
          // This is done by demanding that the object does not override anything.
          // Figuring out whether a symbol implements a trait module is not so simple.
          // For non-private trait members we can check whether there is an overridden symbol.
          // For private trait members this does not work, since `ensureNotPrivate` in phase Mixins
          // does change the name but does not update the owner's scope, so `allOverriddenSymbols` does
          // not work in that case. However, we can check whether the name is an ExpandedName instead.
          transformSyntheticModule(tree)
        else if (sym.isThreadUnsafe || ctx.settings.scalajs.value)
          if (sym.is(Module) && !ctx.settings.scalajs.value) {
            report.error(em"@threadUnsafe is only supported on lazy vals", sym.srcPos)
            transformMemberDefThreadSafe(tree)
          }
          else
            transformMemberDefThreadUnsafe(tree)
        else
          transformMemberDefThreadSafe(tree)
      else transformLocalDef(tree)
    }
  }


  /** Append offset fields to companion objects
    */
  override def transformTemplate(template: Template)(using Context): Tree = {
    val cls = ctx.owner.asClass

    appendOffsetDefs.get(cls) match {
      case None => template
      case Some(data) =>
        data.defs.foreach(_.symbol.addAnnotation(Annotation(defn.ScalaStaticAnnot)))
        cpy.Template(template)(body = addInFront(data.defs, template.body))
    }
  }


  private def addInFront(prefix: List[Tree], stats: List[Tree]) = stats match {
    case first :: rest if isSuperConstrCall(first) => first :: prefix ::: rest
    case _ => prefix ::: stats
  }

  /** Make an eager val that would implement synthetic module.
    * Eager val ensures thread safety and has less code generated.
    *
    */
  def transformSyntheticModule(tree: ValOrDefDef)(using Context): Thicket = {
    val sym = tree.symbol
    val holderSymbol = newSymbol(sym.owner, LazyLocalName.fresh(sym.asTerm.name),
      Synthetic, sym.info.widen.resultType).enteredAfter(this)
    val field = ValDef(holderSymbol, tree.rhs.changeOwnerAfter(sym, holderSymbol, this))
    val getter = DefDef(sym.asTerm, ref(holderSymbol))
    Thicket(field, getter)
  }

  /** Desugar a local `lazy val x: Int = <RHS>` into:
   *
   *  ```
   *  val x$lzy = new scala.runtime.LazyInt()
   *
   *  def x$lzycompute(): Int = x$lzy.synchronized {
   *    if (x$lzy.initialized()) x$lzy.value()
   *    else x$lzy.initialize(<RHS>)
   *      // TODO: Implement Unit-typed lazy val optimization described below
   *      // for a Unit-typed lazy val, this becomes `{ rhs ; x$lzy.initialize() }`
   *      // to avoid passing around BoxedUnit
   *  }
   *
   *  def x(): Int = if (x$lzy.initialized()) x$lzy.value() else x$lzycompute()
   *  ```
   */
  def transformLocalDef(x: ValOrDefDef)(using Context): Thicket = {
    val xname = x.name.asTermName
    val tpe = x.tpe.widen.resultType.widen

    // val x$lzy = new scala.runtime.LazyInt()
    val holderName = LazyLocalName.fresh(xname)
    val holderImpl = defn.LazyHolder()(tpe.typeSymbol)
    val holderSymbol = newSymbol(x.symbol.owner, holderName, containerFlags, holderImpl.typeRef, coord = x.span)
    val holderTree = ValDef(holderSymbol, New(holderImpl.typeRef, Nil))

    val holderRef = ref(holderSymbol)
    val getValue = holderRef.select(lazyNme.value).ensureApplied.withSpan(x.span)
    val initialized = holderRef.select(lazyNme.initialized).ensureApplied

    // def x$lzycompute(): Int = x$lzy.synchronized {
    //   if (x$lzy.initialized()) x$lzy.value()
    //   else x$lzy.initialize(<RHS>)
    // }
    val initName = LazyLocalInitName.fresh(xname)
    val initSymbol = newSymbol(x.symbol.owner, initName, initFlags, MethodType(Nil, tpe), coord = x.span)
    val rhs = x.rhs.changeOwnerAfter(x.symbol, initSymbol, this)
    val initialize = holderRef.select(lazyNme.initialize).appliedTo(rhs)
    val initBody = holderRef
      .select(defn.Object_synchronized)
      .appliedToType(tpe)
      .appliedTo(If(initialized, getValue, initialize).ensureConforms(tpe))
    val initTree = DefDef(initSymbol, initBody)

    // def x(): Int = if (x$lzy.initialized()) x$lzy.value() else x$lzycompute()
    val accessorBody = If(initialized, getValue, ref(initSymbol).ensureApplied).ensureConforms(tpe)
    val accessor = DefDef(x.symbol.asTerm, accessorBody)

    report.debuglog(s"found a lazy val ${x.show},\nrewrote with ${holderTree.show}")
    Thicket(holderTree, initTree, accessor)
  }


  override def transformStats(trees: List[tpd.Tree])(using Context): List[Tree] = {
    // backend requires field usage to be after field definition
    // need to bring containers to start of method
    val (holders, stats) =
      trees.partition {
        _.symbol.flags.&~(Touched) == containerFlags
        // Filtering out Touched is not required currently, as there are no LazyTypes involved here
        // but just to be more safe
      }
    holders:::stats
  }

  private def nullOut(nullables: List[Symbol])(using Context): List[Tree] =
    nullables.map { field =>
      assert(field.isField)
      field.setFlag(Mutable)
      ref(field).becomes(nullLiteral)
    }

  /** Create thread-unsafe lazy accessor equivalent to such code
    * ```
    * def methodSymbol() = {
    *   if (!flag) {
    *     target = rhs
    *     flag = true
    *     nullable = null
    *   }
    *   target
    * }
    * ```
    */
  def mkThreadUnsafeDef(sym: Symbol, flag: Symbol, target: Symbol, rhs: Tree)(using Context): DefDef = {
    val targetRef = ref(target)
    val flagRef = ref(flag)
    val stats = targetRef.becomes(rhs) :: flagRef.becomes(Literal(Constant(true))) :: nullOut(nullableFor(sym))
    val init = If(
      flagRef.ensureApplied.select(nme.UNARY_!).ensureApplied,
      Block(stats.init, stats.last),
      unitLiteral
    )
    DefDef(sym.asTerm, Block(List(init), targetRef.ensureApplied))
  }

  /** Create thread-unsafe lazy accessor for not-nullable types  equivalent to such code
    * ```
    * def methodSymbol() = {
    *   if (target eq null) {
    *     target = rhs
    *     nullable = null
    *   }
    *   target
    * }
    * ```
    */
  def mkDefThreadUnsafeNonNullable(sym: Symbol, target: Symbol, rhs: Tree)(using Context): DefDef = {
    val targetRef = ref(target)
    val stats = targetRef.becomes(rhs) :: nullOut(nullableFor(sym))
    val init = If(
      targetRef.select(nme.eq).appliedTo(nullLiteral),
      Block(stats.init, stats.last),
      unitLiteral
    )
    DefDef(sym.asTerm, Block(List(init), targetRef.ensureApplied))
  }

  def transformMemberDefThreadUnsafe(x: ValOrDefDef)(using Context): Thicket = {
    val claz = x.symbol.owner.asClass
    val tpe = x.tpe.widen.resultType.widen
    assert(!(x.symbol is Mutable))
    val containerName = LazyLocalName.fresh(x.name.asTermName)
    val containerSymbol = newSymbol(claz, containerName,
      x.symbol.flags &~ containerFlagsMask | containerFlags | Private,
      tpe, coord = x.symbol.coord
    ).enteredAfter(this)

    val containerTree = ValDef(containerSymbol, defaultValue(tpe))
    if (x.tpe.isNotNull && tpe <:< defn.ObjectType)
      // can use 'null' value instead of flag
      Thicket(containerTree, mkDefThreadUnsafeNonNullable(x.symbol, containerSymbol, x.rhs))
    else {
      val flagName = LazyBitMapName.fresh(x.name.asTermName)
      val flagSymbol = newSymbol(x.symbol.owner, flagName,  containerFlags | Private, defn.BooleanType).enteredAfter(this)
      val flag = ValDef(flagSymbol, Literal(Constant(false)))
      Thicket(containerTree, flag, mkThreadUnsafeDef(x.symbol, flagSymbol, containerSymbol, x.rhs))
    }
  }

  /** Create a threadsafe lazy accessor equivalent to such code
    * ```
    * def methodSymbol(): Int = {
    *   while (true) {
    *     val flag = LazyVals.get(this, bitmap_offset)
    *     val state = LazyVals.STATE(flag, <field-id>)
    *
    *     if (state == <state-3>) {
    *       return value_0
    *     } else if (state == <state-0>) {
    *       if (LazyVals.CAS(this, bitmap_offset, flag, <state-1>, <field-id>)) {
    *         try {
    *           val result = <RHS>
    *           value_0 = result
    *           nullable = null
    *           LazyVals.setFlag(this, bitmap_offset, <state-3>, <field-id>)
    *           return result
    *         }
    *         catch {
    *           case ex =>
    *             LazyVals.setFlag(this, bitmap_offset, <state-0>, <field-id>)
    *             throw ex
    *         }
    *       }
    *     } else /* if (state == <state-1> || state == <state-2>) */ {
    *       LazyVals.wait4Notification(this, bitmap_offset, flag, <field-id>)
    *     }
    *   }
    * }
    * ```
    */
  def mkThreadSafeDef(methodSymbol: TermSymbol,
                      claz: ClassSymbol,
                      ord: Int,
                      target: Symbol,
                      rhs: Tree,
                      tp: Type,
                      offset: Tree,
                      getFlag: Tree,
                      stateMask: Tree,
                      casFlag: Tree,
                      setFlagState: Tree,
                      waitOnLock: Tree)(using Context): DefDef = {
    val initState = Literal(Constant(0))
    val computeState = Literal(Constant(1))
    val computedState = Literal(Constant(3))

    val thiz = This(claz)
    val fieldId = Literal(Constant(ord))

    val flagSymbol = newSymbol(methodSymbol, lazyNme.flag, Synthetic, defn.LongType)
    val flagDef = ValDef(flagSymbol, getFlag.appliedTo(thiz, offset))
    val flagRef = ref(flagSymbol)

    val stateSymbol = newSymbol(methodSymbol, lazyNme.state, Synthetic, defn.LongType)
    val stateDef = ValDef(stateSymbol, stateMask.appliedTo(ref(flagSymbol), Literal(Constant(ord))))
    val stateRef = ref(stateSymbol)

    val compute = {
      val resultSymbol = newSymbol(methodSymbol, lazyNme.result, Synthetic, tp)
      val resultRef = ref(resultSymbol)
      val stats = (
        ValDef(resultSymbol, rhs) ::
        ref(target).becomes(resultRef) ::
        (nullOut(nullableFor(methodSymbol)) :+
        setFlagState.appliedTo(thiz, offset, computedState, fieldId))
      )
      Block(stats, Return(resultRef, methodSymbol))
    }

    val retryCase = {
      val caseSymbol = newSymbol(methodSymbol, nme.DEFAULT_EXCEPTION_NAME, Synthetic | Case, defn.ThrowableType)
      val triggerRetry = setFlagState.appliedTo(thiz, offset, initState, fieldId)
      CaseDef(
        Bind(caseSymbol, ref(caseSymbol)),
        EmptyTree,
        Block(List(triggerRetry), Throw(ref(caseSymbol)))
      )
    }

    val initialize = If(
      casFlag.appliedTo(thiz, offset, flagRef, computeState, fieldId),
      Try(compute, List(retryCase), EmptyTree),
      unitLiteral
    )

    val condition = If(
      stateRef.equal(computedState),
      Return(ref(target), methodSymbol),
      If(
        stateRef.equal(initState),
        initialize,
        waitOnLock.appliedTo(thiz, offset, flagRef, fieldId)
      )
    )

    val loop = WhileDo(EmptyTree, Block(List(flagDef, stateDef), condition))
    DefDef(methodSymbol, loop)
  }

  def transformMemberDefThreadSafe(x: ValOrDefDef)(using Context): Thicket = {
    assert(!(x.symbol is Mutable))

    val tpe = x.tpe.widen.resultType.widen
    val claz = x.symbol.owner.asClass
    val thizClass = Literal(Constant(claz.info))
    val helperModule = requiredModule("scala.runtime.LazyVals")
    val getOffset = Select(ref(helperModule), lazyNme.RLazyVals.getOffset)
    var offsetSymbol: TermSymbol | Uninitialized = initiallyNull
    var flag: Tree = EmptyTree
    var ord = 0

    def offsetName(id: Int) = s"${StdNames.nme.LAZY_FIELD_OFFSET}${if (x.symbol.owner.is(Module)) "_m_" else ""}$id".toTermName

    // compute or create appropriate offsetSymbol, bitmap and bits used by current ValDef
    appendOffsetDefs.get(claz) match {
      case Some(info) =>
        val flagsPerLong = (64 / scala.runtime.LazyVals.BITS_PER_LAZY_VAL).toInt
        info.ord += 1
        ord = info.ord % flagsPerLong
        val id = info.ord / flagsPerLong
        val offsetById = offsetName(id)
        if (ord != 0) // there are unused bits in already existing flag
          offsetSymbol = claz.info.decl(offsetById)
            .suchThat(sym => sym.is(Synthetic) && sym.isTerm)
             .symbol.asTerm
        else { // need to create a new flag
          offsetSymbol = newSymbol(claz, offsetById, Synthetic, defn.LongType).enteredAfter(this)
          offsetSymbol.nn.addAnnotation(Annotation(defn.ScalaStaticAnnot))
          val flagName = LazyBitMapName.fresh(id.toString.toTermName)
          val flagSymbol = newSymbol(claz, flagName, containerFlags, defn.LongType).enteredAfter(this)
          flag = ValDef(flagSymbol, Literal(Constant(0L)))
          val offsetTree = ValDef(offsetSymbol.nn, getOffset.appliedTo(thizClass, Literal(Constant(flagName.toString))))
          info.defs = offsetTree :: info.defs
        }

      case None =>
        offsetSymbol = newSymbol(claz, offsetName(0), Synthetic, defn.LongType).enteredAfter(this)
        offsetSymbol.nn.addAnnotation(Annotation(defn.ScalaStaticAnnot))
        val flagName = LazyBitMapName.fresh("0".toTermName)
        val flagSymbol = newSymbol(claz, flagName, containerFlags, defn.LongType).enteredAfter(this)
        flag = ValDef(flagSymbol, Literal(Constant(0L)))
        val offsetTree = ValDef(offsetSymbol.nn, getOffset.appliedTo(thizClass, Literal(Constant(flagName.toString))))
        appendOffsetDefs += (claz -> new OffsetInfo(List(offsetTree), ord))
    }

    val containerName = LazyLocalName.fresh(x.name.asTermName)
    val containerSymbol = newSymbol(claz, containerName, x.symbol.flags &~ containerFlagsMask | containerFlags, tpe, coord = x.symbol.coord).enteredAfter(this)

    val containerTree = ValDef(containerSymbol, defaultValue(tpe))

    val offset =  ref(offsetSymbol.nn)
    val getFlag = Select(ref(helperModule), lazyNme.RLazyVals.get)
    val setFlag = Select(ref(helperModule), lazyNme.RLazyVals.setFlag)
    val wait =    Select(ref(helperModule), lazyNme.RLazyVals.wait4Notification)
    val state =   Select(ref(helperModule), lazyNme.RLazyVals.state)
    val cas =     Select(ref(helperModule), lazyNme.RLazyVals.cas)

    val accessor = mkThreadSafeDef(x.symbol.asTerm, claz, ord, containerSymbol, x.rhs, tpe, offset, getFlag, state, cas, setFlag, wait)
    if (flag eq EmptyTree)
      Thicket(containerTree, accessor)
    else Thicket(containerTree, flag, accessor)
  }
}

object LazyVals {
  val name: String = "lazyVals"
  val description: String = "expand lazy vals"

  object lazyNme {
    import Names.TermName
    object RLazyVals {
      import scala.runtime.LazyVals.{Names => N}
      val get: TermName               = N.get.toTermName
      val setFlag: TermName           = N.setFlag.toTermName
      val wait4Notification: TermName = N.wait4Notification.toTermName
      val state: TermName             = N.state.toTermName
      val cas: TermName               = N.cas.toTermName
      val getOffset: TermName         = N.getOffset.toTermName
    }
    val flag: TermName        = "flag".toTermName
    val state: TermName       = "state".toTermName
    val result: TermName      = "result".toTermName
    val value: TermName       = "value".toTermName
    val initialized: TermName = "initialized".toTermName
    val initialize: TermName  = "initialize".toTermName
    val retry: TermName       = "retry".toTermName
  }
}
