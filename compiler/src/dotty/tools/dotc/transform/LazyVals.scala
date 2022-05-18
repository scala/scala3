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

  /**
   * The map contains the list of the offset trees.
   */
  class OffsetInfo(var defs: List[Tree])
  /** 
   * This map contains mutable state of transformation: OffsetDefs to be appended 
   * to companion object definitions, and number of bits currently used. 
   */
  class LegacyOffsetInfo(defs: List[Tree], var ord: Int) extends OffsetInfo(defs)
  private val appendOffsetDefs = mutable.Map.empty[Symbol, OffsetInfo]
  private val oldAppendOffsetDefs = mutable.Map.empty[Symbol, LegacyOffsetInfo]

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
  private var lazyValNullables: IdentityHashMap[Symbol, mutable.ListBuffer[Symbol]] | Null = _
  private def nullableFor(sym: Symbol)(using Context) = {
    // optimisation: value only used once, we can remove the value from the map
    val nullables = lazyValNullables.nn.remove(sym)
    if (nullables == null) Nil
    else nullables.toList
  }

  private inline def isLegacyLazyVals(using ctx: Context): Boolean =
    ctx.settings.XlegacyLazyVals.value

  private def needsBoxing(tp: Type)(using Context): Boolean = tp != NoType && tp != defn.UnitType && tp.classSymbol.isPrimitiveValueClass
    
  private def boxIfCan(tp: Type)(using Context): Type =
    assert(needsBoxing(tp))
    defn.boxedType(tp)

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

  /** 
   * Append offset fields to companion objects.
   */
  override def transformTemplate(template: Template)(using Context): Tree = {
    val cls = ctx.owner.asClass
    (if isLegacyLazyVals then oldAppendOffsetDefs else appendOffsetDefs).get(cls) match {
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

  /** 
   * Make an eager val that would implement synthetic module.
   * Eager val ensures thread safety and has less code generated.
   */
  def transformSyntheticModule(tree: ValOrDefDef)(using Context): Thicket = {
    val sym = tree.symbol
    val holderSymbol = newSymbol(sym.owner, LazyLocalName.fresh(sym.asTerm.name),
      Synthetic, sym.info.widen.resultType).enteredAfter(this)
    val field = ValDef(holderSymbol, tree.rhs.changeOwnerAfter(sym, holderSymbol, this))
    val getter = DefDef(sym.asTerm, ref(holderSymbol))
    Thicket(field, getter)
  }

  /** 
   * Desugar a local `lazy val x: Int = <RHS>` into:
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

  /**
   * Create a threadsafe lazy accessor equivalent to the following code:
   * ```
   * private @volatile var _x: AnyRef = null
   * @tailrec def x: A =
   *   _x match
   *   case current: A => 
   *     current
   *   case NullValue => null
   *   case null =>
   *     if CAS(_x, null, Evaluating) then
   *       var result: AnyRef = null // here, we need `AnyRef` to possibly assign `NullValue`
   *       try
   *         result = rhs
   *         nullable = null // if the field is nullable; see `CollectNullableFields`
   *       finally
   *         if result == null then result = NullValue // drop if A is non-nullable
   *         if !CAS(_x, Evaluating, result) then
   *           val lock = _x.asInstanceOf[Waiting]
   *           CAS(_x, lock, result)
   *           lock.release()
   *     x
   *   case Evaluating =>
   *     CAS(_x, Evaluating, new Waiting)
   *     x
   *   case current: Waiting =>
   *     current.awaitRelease()
   *     x
   * ```
   * Where `Evaluating` and `NullValue` are represented by `object`s and `Waiting` by a class that
   * allows awaiting the completion of the evaluation. Note that since tail-recursive
   * functions are transformed *before* lazy-vals, this implementation directly implements
   * the resulting loop. `PatternMatcher` coming before `LazyVals`, the pattern matching block
   * is implemented using if-s. Additionally, the code can be optimized to be better inlined by 
   * introducing separate function for computing the lazy value and awaiting it. That is:
   * 
   * ```
   * private var _x: AnyRef = null
   * 
   * def x: A =
   *   if !(_x == null || _x.isInstanceOf[LazyValControlState]) then
   *     return _x.asInstanceOf[A]
   *   else if _x == NullValue then
   *     return null
   *   else 
   *     return x_compute()
   * 
   * private def x_compute() = 
   *   while <EmptyTree> do
   *     val current: AnyRef = _x
   *     if current == null then
   *       if CAS(_x, null, Evaluating) then
   *         var result: AnyRef = null
   *         try
   *           nullable = null
   *           if result == null then result = NullValue
   *           return result
   *         finally
   *            if !CAS(_x, Evaluating, result) then
   *              val lock = _x.asInstanceOf[Waiting]
   *              CAS(_x, lock, result)
   *              lock.release()
   *     else
   *       if current.isInstanceOf[LazyValControlState] then
   *         if current.isInstanceOf[Evaluating] then // To avoid creating Waiting instance
   *           CAS(current, Evaluating, new Waiting)
   *         else if current.isInstanceOf[NullValue] then
   *           return null
   *         else if current.isInstanceOf[Waiting] then
   *           current.asInstanceOf[Waiting].await()
   *       else
   *         return current.asInstanceOf[A]
   *   end while
   *  * ```
   * 
   * @param memberDef     the transformed lazy field member definition
   * @param claz          the class containing this lazy val field
   * @param target        the target synthetic field
   * @param offset        the offset of the field in the storage allocation of the class
   * @param thiz          a reference to the transformed class
   * @param helperModule  the symbol of the runtime LazyVals helper module
   * @param runtimeModule the runtime LazyVals module path

   */
  def mkThreadSafeDef(memberDef: ValOrDefDef,
                      claz: ClassSymbol,
                      target: Symbol,
                      offset: Tree,
                      thiz: Tree,
                      helperModule: TermSymbol,
                      runtimeModule: String)(using Context): (DefDef, DefDef) = {
    val tp = memberDef.tpe.widen.resultType.widen
    val waiting = ref(requiredClass(s"$runtimeModule.${lazyNme.RLazyVals.waiting}"))
    val controlState = ref(requiredClass(s"$runtimeModule.${lazyNme.RLazyVals.controlState}"))
    val evaluating = Select(ref(helperModule), lazyNme.RLazyVals.evaluating)
    val nullValue = Select(ref(helperModule), lazyNme.RLazyVals.nullValue)
    val objCasFlag = Select(ref(helperModule), lazyNme.RLazyVals.objCas)
    val accessorMethodSymbol = memberDef.symbol.asTerm
    val lazyInitMethodName = LazyLocalInitName.fresh(memberDef.name.asTermName)
    val lazyInitMethodSymbol = newSymbol(claz, lazyInitMethodName, Synthetic | Method | Private, MethodType(Nil)(_ => Nil, _ => defn.ObjectType))

    val rhs = memberDef.rhs
    val rhsMappedOwner = rhs.changeOwnerAfter(memberDef.symbol, lazyInitMethodSymbol, this)
    val valueSymbol = newSymbol(accessorMethodSymbol, lazyNme.result, Synthetic | Mutable, defn.ObjectType)
    val accessorBody =
      Block(
        ValDef(valueSymbol, ref(target)) :: Nil,
        If( // if _x != null && !_x.isInstanceOf[LazyValControlState] then
          ref(valueSymbol).select(defn.Any_!=).appliedTo(nullLiteral).select(nme.And).appliedTo(ref(valueSymbol).select(defn.Any_isInstanceOf).appliedToTypeTree(controlState)
              .select(nme.UNARY_!).appliedToNone),
            Return(ref(valueSymbol).ensureConforms(tp), accessorMethodSymbol), // then return _x.asInstanceOf[A]
            If(
              ref(valueSymbol).select(defn.Any_==).appliedTo(nullValue),
              Return(nullLiteral.ensureConforms(tp), accessorMethodSymbol),
              Return(ref(lazyInitMethodSymbol).ensureApplied.ensureConforms(tp), accessorMethodSymbol) // else return x_compute()
            )
        )
      )

    val accessorDef = DefDef(accessorMethodSymbol, accessorBody)

    // if observed a null (uninitialized) value
    val initialize = {
      // var result: AnyRef
      val resSymb = newSymbol(lazyInitMethodSymbol, lazyNme.result, Synthetic | Mutable, defn.ObjectType)
      // releasing block in finally
      val lockRel = {
        val lockSymb = newSymbol(lazyInitMethodSymbol, lazyNme.lock, Synthetic, waiting.typeOpt)
        Block(ValDef(lockSymb, ref(target).cast(waiting.typeOpt))
          :: objCasFlag.appliedTo(thiz, offset, ref(lockSymb), ref(resSymb)) :: Nil,
          ref(lockSymb).select(lazyNme.RLazyVals.waitingRelease).ensureApplied)
      }
      // finally block
      val fin = If(
          objCasFlag.appliedTo(thiz, offset, evaluating, ref(resSymb)).select(nme.UNARY_!).appliedToNone,
          lockRel,
          unitLiteral
        ).withType(defn.UnitType)
      // entire try block
      val evaluate = Try(

        Block(
          (Assign(ref(resSymb), if needsBoxing(tp) && rhsMappedOwner != EmptyTree then rhsMappedOwner.ensureConforms(boxIfCan(tp)) else rhsMappedOwner) // try result = rhs
          :: If(
              ref(resSymb).select(defn.Any_==).appliedTo(nullLiteral),
              Assign(ref(resSymb), nullValue),
              unitLiteral
          ) :: Nil)
          ::: nullOut(nullableFor(accessorMethodSymbol)),
          unitLiteral),
        Nil,
        fin
      )
      // if CAS(_, null, Evaluating)
      If(
        objCasFlag.appliedTo(thiz, offset, nullLiteral, evaluating),
        Block(ValDef(resSymb, nullLiteral) :: Nil, // var result: AnyRef = null
          evaluate), // try ... finally ...
        unitLiteral
      ).withType(defn.UnitType)
    }

    val current = newSymbol(lazyInitMethodSymbol, lazyNme.current, Synthetic, defn.ObjectType)
    val ifNotUninitialized =
      If(
        ref(current).select(defn.Any_isInstanceOf).appliedToTypeTree(controlState),
          // if a control state
          If(
            ref(current).select(defn.Any_==).appliedTo(evaluating),
            // if is Evaluating then CAS(_, Evaluating, new Waiting)
            Block(
              objCasFlag.appliedTo(thiz, offset, evaluating, Select(New(waiting), StdNames.nme.CONSTRUCTOR).ensureApplied) :: Nil,
              unitLiteral
            ),
            // if not Evaluating
            If(
              ref(current).select(defn.Any_isInstanceOf).appliedToTypeTree(waiting),
              // if is waiting
              ref(current).select(defn.Any_asInstanceOf).appliedToTypeTree(waiting).select(lazyNme.RLazyVals.waitingAwaitRelease, _.info.paramInfoss.exists(_.size == 0)).ensureApplied,
              If(ref(current).select(defn.Any_==).appliedTo(nullValue),
                Return(nullLiteral, lazyInitMethodSymbol),
                unitLiteral
              )
            )
          ),
        // if not a control state
        Return(ref(current), lazyInitMethodSymbol)
      )

    val initBody = Block(ValDef(current, ref(target)) :: Nil, If(ref(current).equal(nullLiteral), initialize, ifNotUninitialized).withType(defn.UnitType))
    val initMainLoop = WhileDo(EmptyTree, initBody) // becomes: while (true) do { body }
    val initMethodDef = DefDef(lazyInitMethodSymbol, initMainLoop)
    (accessorDef, initMethodDef)
  }

  def transformMemberDefThreadSafe(x: ValOrDefDef)(using Context): Thicket = {
    assert(!(x.symbol is Mutable))
    if isLegacyLazyVals then
      transformMemberDefThreadSafeLegacy(x)
    else
      transformMemberDefThreadSafeNew(x)
  }

  def transformMemberDefThreadSafeNew(x: ValOrDefDef)(using Context): Thicket = {
    import dotty.tools.dotc.core.Types._
    import dotty.tools.dotc.core.Flags._

    val runtimeModule = "scala.runtime.LazyVals"
    val claz = x.symbol.owner.asClass
    val thizClass = Literal(Constant(claz.info))
    val helperModule = requiredModule(runtimeModule)
    var offsetSymbol: TermSymbol | Null = null

    def offsetName(id: Int) = s"${StdNames.nme.LAZY_FIELD_OFFSET}${if (x.symbol.owner.is(Module)) "_m_" else ""}$id".toTermName
    val containerName = LazyLocalName.fresh(x.name.asTermName)
    val containerSymbol = newSymbol(claz, containerName, containerFlags, defn.ObjectType).enteredAfter(this)
    containerSymbol.addAnnotation(Annotation(defn.VolatileAnnot)) // private @volatile var _x: AnyRef
    containerSymbol.addAnnotations(x.symbol.annotations) // pass annotations from original definition
    val stat = x.symbol.isStatic
    if stat then
      containerSymbol.setFlag(JavaStatic)
    val getOffset =
      if stat then
        Select(ref(helperModule), lazyNme.RLazyVals.getStaticFieldOffset)
      else
        Select(ref(helperModule), lazyNme.RLazyVals.getOffsetStatic)
    val containerTree = ValDef(containerSymbol, nullLiteral)

    // create an offset for this lazy val
    appendOffsetDefs.get(claz) match
      case Some(info) =>
        offsetSymbol = newSymbol(claz, offsetName(info.defs.size), Synthetic, defn.LongType).enteredAfter(this)
        offsetSymbol.nn.addAnnotation(Annotation(defn.ScalaStaticAnnot))
        val fieldTree = thizClass.select(lazyNme.RLazyVals.getDeclaredField).appliedTo(Literal(Constant(containerName.mangledString)))
        val offsetTree = ValDef(offsetSymbol.nn, getOffset.appliedTo(fieldTree))
        info.defs = offsetTree :: info.defs
      case None =>
        offsetSymbol = newSymbol(claz, offsetName(0), Synthetic, defn.LongType).enteredAfter(this)
        offsetSymbol.nn.addAnnotation(Annotation(defn.ScalaStaticAnnot))
        val fieldTree = thizClass.select(lazyNme.RLazyVals.getDeclaredField).appliedTo(Literal(Constant(containerName.mangledString)))
        val offsetTree = ValDef(offsetSymbol.nn, getOffset.appliedTo(fieldTree))
        appendOffsetDefs += (claz -> new OffsetInfo(List(offsetTree)))
    
    val offset = ref(offsetSymbol.nn)

    val swapOver =
      if stat then
        tpd.clsOf(x.symbol.owner.typeRef)
      else
        This(claz)

    val (accessorDef, initMethodDef) = mkThreadSafeDef(x, claz, containerSymbol, offset, swapOver, helperModule, runtimeModule)
    Thicket(containerTree, accessorDef, initMethodDef)
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
  def mkThreadSafeDefLegacy(methodSymbol: TermSymbol,
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
      val caseSymbol = newSymbol(methodSymbol, nme.DEFAULT_EXCEPTION_NAME, SyntheticCase, defn.ThrowableType)
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

  def transformMemberDefThreadSafeLegacy(x: ValOrDefDef)(using Context): Thicket = {
    val tpe = x.tpe.widen.resultType.widen
    val claz = x.symbol.owner.asClass
    val thizClass = Literal(Constant(claz.info))
    val helperModule = requiredModule("scala.runtime.LazyVals")
    val getOffset = Select(ref(helperModule), lazyNme.RLazyVals.getOffset)
    val getOffsetStatic = Select(ref(helperModule), lazyNme.RLazyVals.getOffsetStatic)
    var offsetSymbol: TermSymbol | Null = null
    var flag: Tree = EmptyTree
    var ord = 0

    def offsetName(id: Int) = s"${StdNames.nme.LAZY_FIELD_OFFSET}${if (x.symbol.owner.is(Module)) "_m_" else ""}$id".toTermName

    // compute or create appropriate offsetSymbol, bitmap and bits used by current ValDef
    oldAppendOffsetDefs.get(claz) match {
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
          val fieldTree = thizClass.select(lazyNme.RLazyVals.getDeclaredField).appliedTo(Literal(Constant(flagName.toString)))
          val offsetTree = ValDef(offsetSymbol.nn, getOffsetStatic.appliedTo(fieldTree))
          info.defs = offsetTree :: info.defs
        }

      case None =>
        offsetSymbol = newSymbol(claz, offsetName(0), Synthetic, defn.LongType).enteredAfter(this)
        offsetSymbol.nn.addAnnotation(Annotation(defn.ScalaStaticAnnot))
        val flagName = LazyBitMapName.fresh("0".toTermName)
        val flagSymbol = newSymbol(claz, flagName, containerFlags, defn.LongType).enteredAfter(this)
        flag = ValDef(flagSymbol, Literal(Constant(0L)))
        val fieldTree = thizClass.select(lazyNme.RLazyVals.getDeclaredField).appliedTo(Literal(Constant(flagName.toString)))
        val offsetTree = ValDef(offsetSymbol.nn, getOffsetStatic.appliedTo(fieldTree))
        oldAppendOffsetDefs += (claz -> new LegacyOffsetInfo(List(offsetTree), ord))
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

    val accessor = mkThreadSafeDefLegacy(x.symbol.asTerm, claz, ord, containerSymbol, x.rhs, tpe, offset, getFlag, state, cas, setFlag, wait)
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
      val waiting: TermName                = N.waiting.toTermName
      val waitingAwaitRelease: TermName    = N.waitingAwaitRelease.toTermName
      val waitingRelease: TermName         = N.waitingRelease.toTermName
      val evaluating: TermName             = N.evaluating.toTermName
      val controlState: TermName           = N.controlState.toTermName
      val nullValue: TermName             = N.nullValue.toTermName
      val objCas: TermName                 = N.objCas.toTermName
      val getStaticFieldOffset: TermName   = N.getStaticFieldOffset.toTermName
      val get: TermName               = N.get.toTermName
      val setFlag: TermName           = N.setFlag.toTermName
      val wait4Notification: TermName = N.wait4Notification.toTermName
      val state: TermName             = N.state.toTermName
      val cas: TermName               = N.cas.toTermName
      val getOffset: TermName         = N.getOffset.toTermName
      val getOffsetStatic: TermName   = N.getOffsetStatic.toTermName
      val getDeclaredField: TermName  = "getDeclaredField".toTermName
    }
    val flag: TermName        = "flag".toTermName
    val state: TermName       = "state".toTermName
    val result: TermName      = "result".toTermName
    val value: TermName       = "value".toTermName
    val initialized: TermName = "initialized".toTermName
    val initialize: TermName  = "initialize".toTermName
    val retry: TermName       = "retry".toTermName
    val current: TermName     = "current".toTermName
    val lock: TermName        = "lock".toTermName
    val discard: TermName     = "discard".toTermName
  }
}
