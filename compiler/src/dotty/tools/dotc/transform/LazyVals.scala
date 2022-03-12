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
  private var lazyValNullables: IdentityHashMap[Symbol, mutable.ListBuffer[Symbol]] = _
  private def nullableFor(sym: Symbol)(using Context) = {
    // optimisation: value only used once, we can remove the value from the map
    val nullables = lazyValNullables.remove(sym)
    if (nullables == null) Nil
    else nullables.toList
  }

  private def initBlock(stats: List[Tree])(using Context): Block = stats match
    case Nil => throw new IllegalArgumentException("trying to create an empty Block")
    case x :: Nil => Block(List(x), EmptyTree)
    case x :: xs => Block(stats.init, stats.last)

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
   *    _x match
   *    case current: A =>
   *        current
   *    case null =>
   *        if CAS(_x, null, Evaluating) then
   *            var result: AnyRef = null // here, we need `AnyRef` to possibly assign `NULL`
   *            try
   *                result = rhs
   *                nullable = null // if the field is nullable; see `CollectNullableFields`
   *                if result == null then result = NULL // drop if A is non-nullable
   *            finally
   *                if !CAS(_x, Evaluating, result) then
   *                    val lock = _x.asInstanceOf[Waiting]
   *                    CAS(_x, lock, result)
   *                    lock.release()
   *        x
   *    case Evaluating =>
   *        CAS(_x, Evaluating, new Waiting)
   *        x
   *    case current: Waiting =>
   *        current.awaitRelease()
   *        x
   *    case NULL => null
   * ```
   * Where `Evaluating` and `NULL` are represented by `object`s and `Waiting` by a class that
   * allows awaiting the completion of the evaluation. Note that since tail-recursive
   * functions are transformed *before* lazy-vals, this implementation directly implements
   * the resulting loop. `PatternMatcher` coming before `LazyVals`, the pattern matching block
   * is implemented using if-s. That is:
   * 
   * ```
   * private @volatile var _x: AnyRef = null
   * def x: A =
   *   while true do
   *     val current: AnyRef = _x
   *     if current == null then
   *       if CAS(_x, null, Evaluating) then
   *         var result: AnyRef = null
   *         try
   *           result = rhs
   *           nullable = null
   *           if result == null then result = NULL
   *         finally
   *           if !CAS(_x, Evaluating, result) then
   *             val lock = _x.asInstanceOf[Waiting]
   *             CAS(_x, lock, result)
   *             lock.release()
   *     else
   *       if current.isInstanceOf[Evaluating] then
   *         CAS(current, Evaluating, new Waiting)
   *       else if current.isInstanceOf[NULL] then
   *         null
   *       else if current.isInstanceOf[Waiting] then
   *         current.asInstanceOf[Waiting].awaitRelease()
   *       else
   *         current.asInstanceOf[A]
   *   end while
   * ```
   * 
   * @param methodSymbol  the symbol of the new method
   * @param claz          the class containing this lazy val field
   * @param target        the target synthetic field
   * @param rhs           the right-hand side expression of the lazy val
   * @param tp            the type of the lazy val
   * @param offset        the offset of the field in the bitmap
   * @param getFlag       a flag for the volatile get function
   * @param objCasFlag    a flag for the CAS function operating on objects
   * @param waiting       a reference to the `Waiting` runtime class
   * @param evaluating    a reference to the `Evaluating` runtime object
   * @param nullValued    a reference to the `NULL` runtime object
   */
  def mkThreadSafeDef(methodSymbol: TermSymbol,
                      claz: ClassSymbol,
                      target: Symbol,
                      rhs: Tree,
                      tp: Type,
                      offset: Tree,
                      objCasFlag: Tree,
                      waiting: Tree,
                      evaluating: Tree,
                      nullValued: Tree,
                      thiz: Tree)(using Context): DefDef = {
    val discardSymb = newSymbol(methodSymbol, lazyNme.discard, Method | Synthetic, MethodType(Nil)(_ => Nil, _ => defn.UnitType))
    val discardDef = DefDef(discardSymb, initBlock(
      objCasFlag.appliedTo(thiz, offset, evaluating, Select(New(waiting), StdNames.nme.CONSTRUCTOR).ensureApplied)
        :: Return(unitLiteral, discardSymb) :: Nil))
    // if observed a null value
    val unevaluated = {
      // var res: AnyRef
      val resSymb = newSymbol(methodSymbol, lazyNme.result, Synthetic | Mutable, defn.ObjectType)
      // releasing block in finally
      val lockRel = {
        val lockSymb = newSymbol(methodSymbol, lazyNme.lock, Synthetic, waiting.typeOpt)
        initBlock(ValDef(lockSymb, ref(target).cast(waiting.typeOpt))
          :: objCasFlag.appliedTo(thiz, offset, ref(lockSymb), ref(resSymb))
          :: ref(lockSymb).select(lazyNme.RLazyVals.waitingRelease).ensureApplied :: Nil)
      }
      // finally block
      val fin = If(
        objCasFlag.appliedTo(thiz, offset, evaluating, ref(resSymb)).equal(Literal(Constant(false))), 
        lockRel,
        EmptyTree
      ).withType(defn.UnitType)
      // entire try block
      val evaluate = Try(
        initBlock(
          Assign(ref(resSymb), if needsBoxing(tp) && rhs != EmptyTree then rhs.ensureConforms(boxIfCan(tp)) else rhs) // try result = rhs
          :: nullOut(nullableFor(methodSymbol))
          ::: If(ref(resSymb).equal(nullLiteral), Assign(ref(resSymb), nullValued), EmptyTree).withType(defn.UnitType) // if result == null then result = NULL
          :: Nil
        ),
        Nil,
        fin
      )
      // if CAS(...)
      If(
        objCasFlag.appliedTo(thiz, offset, nullLiteral, evaluating),
        initBlock(ValDef(resSymb, nullLiteral) // var result: AnyRef = null
          :: evaluate // try ... finally ...
          :: Nil),
        EmptyTree
      ).withType(defn.UnitType)
    }
    val current = newSymbol(methodSymbol, lazyNme.current, Synthetic, defn.ObjectType)
    val ifNotNull =
      If(
        ref(current).select(defn.Any_isInstanceOf).appliedToTypeTree(evaluating),
        ref(discardSymb).ensureApplied,
        // not an Evaluating
        If(
          ref(current).select(defn.Any_isInstanceOf).appliedToTypeTree(nullValued),
          Return(defaultValue(tp), methodSymbol),
          // not a NULL
          If(
            ref(current).select(defn.Any_isInstanceOf).appliedToTypeTree(waiting),
            ref(current).select(defn.Any_asInstanceOf).appliedToTypeTree(waiting).select(lazyNme.RLazyVals.waitingAwaitRelease).ensureApplied,
            // not a Waiting, then is an A
            Return(ref(current).ensureConforms(tp), methodSymbol)
          )
        )
      )
    val body = initBlock(ValDef(current, ref(target)) :: If(ref(current).equal(nullLiteral), unevaluated, ifNotNull) :: Nil)
    val mainLoop = WhileDo(EmptyTree, body) // becomes: while (true) do { body }
    val ret = DefDef(methodSymbol, initBlock(discardDef :: mainLoop :: Nil))
    ret
  }

  def transformMemberDefThreadSafe(x: ValOrDefDef)(using Context): Thicket = {
    assert(!(x.symbol is Mutable))
    import dotty.tools.dotc.core.Types._
    import dotty.tools.dotc.core.Flags._
    
    val runtimeModule = "scala.runtime.LazyVals"
    val tpe = x.tpe.widen.resultType.widen
    val claz = x.symbol.owner.asClass
    val thizClass = Literal(Constant(claz.info))
    val helperModule = requiredModule(runtimeModule)
    val getOffset = Select(ref(helperModule), lazyNme.RLazyVals.getOffset)
    var offsetSymbol: TermSymbol = null

    def offsetName(id: Int) = s"${StdNames.nme.LAZY_FIELD_OFFSET}${if (x.symbol.owner.is(Module)) "_m_" else ""}$id".toTermName

    val containerName = LazyLocalName.fresh(x.name.asTermName)
    val containerSymbol = newSymbol(claz, containerName, containerFlags, defn.ObjectType).enteredAfter(this)
    containerSymbol.addAnnotation(Annotation(defn.VolatileAnnot)) // private @volatile var _x: AnyRef
    val stat = x.symbol.isStatic
    if stat then
      containerSymbol.setFlag(JavaStatic)
    val containerTree = ValDef(containerSymbol, nullLiteral)
    def staticOrFieldOff: Tree = getOffset.appliedTo(thizClass, Literal(Constant(containerName.toString)))

    // create an offset for this lazy val
    appendOffsetDefs.get(claz) match
      case Some(info) =>
        offsetSymbol = newSymbol(claz, offsetName(info.defs.size), Synthetic, defn.LongType).enteredAfter(this)
        offsetSymbol.addAnnotation(Annotation(defn.ScalaStaticAnnot))
        val offsetTree = ValDef(offsetSymbol, staticOrFieldOff)
        info.defs = offsetTree :: info.defs
      case None =>
        offsetSymbol = newSymbol(claz, offsetName(0), Synthetic, defn.LongType).enteredAfter(this)
        offsetSymbol.addAnnotation(Annotation(defn.ScalaStaticAnnot))
        val offsetTree = ValDef(offsetSymbol, staticOrFieldOff)
        appendOffsetDefs += (claz -> new OffsetInfo(List(offsetTree)))

    val waiting = requiredClass(s"$runtimeModule.${lazyNme.RLazyVals.waiting}")
    val evaluating = Select(ref(helperModule), lazyNme.RLazyVals.evaluating)
    val nullValued = Select(ref(helperModule), lazyNme.RLazyVals.nullValued)
    val objCas = Select(ref(helperModule), lazyNme.RLazyVals.objCas)
    
    val offset = ref(offsetSymbol)

    val swapOver =
      if stat then
        tpd.clsOf(x.symbol.owner.typeRef)
      else
        This(claz)

    val methodSymbol = x.symbol.asTerm
    val accessor = mkThreadSafeDef(methodSymbol, claz, containerSymbol, x.rhs, tpe, offset, objCas, 
      ref(waiting), evaluating, nullValued, swapOver)
    Thicket(containerTree, accessor)
  }
}

object LazyVals {
  val name: String = "lazyVals"
  val description: String = "expand lazy vals"

  object lazyNme {
    import Names.TermName
    object RLazyVals {
      import scala.runtime.LazyVals.{Names => N}
      val waiting: TermName             = N.waiting.toTermName
      val waitingAwaitRelease: TermName = N.waitingAwaitRelease.toTermName
      val waitingRelease: TermName      = N.waitingRelease.toTermName
      val evaluating: TermName        = N.evaluating.toTermName
      val nullValued: TermName        = N.nullValued.toTermName
      val objCas: TermName            = N.objCas.toTermName
      val getOffset: TermName         = N.getOffset.toTermName
      val getStaticOffset: TermName   = N.getStaticOffset.toTermName
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
