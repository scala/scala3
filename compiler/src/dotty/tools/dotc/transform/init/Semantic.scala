package dotty.tools.dotc
package transform
package init

import core.*
import Contexts.*
import Symbols.*
import Types.*
import StdNames.*
import NameKinds.OuterSelectName

import ast.tpd.*
import config.Printers.init as printer
import reporting.trace as log

import Errors.*

import scala.collection.mutable
import scala.annotation.tailrec

object Semantic:

// ----- Domain definitions --------------------------------

  /** Abstract values
   *
   *  Value = Hot | Cold | Warm | ThisRef | Fun | RefSet
   *
   *                 Cold
   *        ┌──────►  ▲  ◄────┐  ◄────┐
   *        │         │       │       │
   *        │         │       │       │
   *        |         │       │       │
   *        |         │       │       │
   *     ThisRef     Warm   Fun    RefSet
   *        │         ▲       ▲       ▲
   *        │         │       │       │
   *        |         │       │       │
   *        ▲         │       │       │
   *        │         │       │       │
   *        └─────────┴───────┴───────┘
   *                  Hot
   *
   *   The diagram above does not reflect relationship between `RefSet`
   *   and other values. `RefSet` represents a set of values which could
   *   be `ThisRef`, `Warm` or `Fun`. The following ordering applies for
   *   RefSet:
   *
   *         R_a ⊑ R_b if R_a ⊆ R_b
   *
   *         V ⊑ R if V ∈ R
   *
   */
  sealed abstract class Value:
    def show: String = this.toString()

    def isHot = this == Hot
    def isCold = this == Cold
    def isWarm = this.isInstanceOf[Warm]
    def isThisRef = this.isInstanceOf[ThisRef]

  /** A transitively initialized object */
  case object Hot extends Value

  /** An object with unknown initialization status */
  case object Cold extends Value

  sealed abstract class Ref extends Value:
    def klass: ClassSymbol
    def outer: Value

  /** A reference to the object under initialization pointed by `this` */
  case class ThisRef(klass: ClassSymbol) extends Ref:
    val outer = Hot

  /** An object with all fields initialized but reaches objects under initialization
   *
   *  We need to restrict nesting levels of `outer` to finitize the domain.
   */
  case class Warm(klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value]) extends Ref:

    /** If a warm value is in the process of populating parameters, class bodies are not executed. */
    private var populatingParams: Boolean = false

    def isPopulatingParams = populatingParams

    /** Ensure that outers and class parameters are initialized.
     *
     *  Fields in class body are not initialized.
     *
     *  We need to populate class parameters and outers for warm values for the
     *  following cases:
     *
     *  - Widen an already checked warm value to another warm value without
     *    corresponding object
     *
     *  - Using a warm value from the cache, whose corresponding object from
     *    the last iteration have been remove due to heap reversion
     *    {@see Cache.prepareForNextIteration}
     *
     *  After populating class parameters and outers, it is possible to lazily
     *  compute the field values in class bodies when they are accessed.
     */
    private def populateParams(): Contextual[this.type] = log("populating parameters", printer, (_: Warm).objekt.toString) {
      assert(!populatingParams, "the object is already populating parameters")
      populatingParams = true
      val tpl = klass.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
      extendTrace(klass.defTree) { this.callConstructor(ctor, args.map(arg => ArgInfo(arg, trace))) }
      populatingParams = false
      this
    }

    def ensureObjectExistsAndPopulated(): Contextual[this.type] =
      if cache.containsObject(this) then this
      else this.ensureFresh().populateParams()

  end Warm

  /** A function value */
  case class Fun(expr: Tree, thisV: Ref, klass: ClassSymbol) extends Value

  /** A value which represents a set of addresses
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: List[Fun | Ref]) extends Value

  // end of value definition

  /** The abstract object which stores value about its fields and immediate outers.
   *
   *  Semantically it suffices to store the outer for `klass`. We cache other outers
   *  for performance reasons.
   *
   *  Note: Object is NOT a value.
   */
  case class Objekt(val klass: ClassSymbol, val fields: Map[Symbol, Value], val outers: Map[ClassSymbol, Value]):
    def field(f: Symbol): Value = fields(f)

    def outer(klass: ClassSymbol) = outers(klass)

    def hasOuter(klass: ClassSymbol) = outers.contains(klass)

    def hasField(f: Symbol) = fields.contains(f)

  object Promoted:
    class PromotionInfo:
      var isCurrentObjectPromoted: Boolean = false
      val values = mutable.Set.empty[Value]
      override def toString(): String = values.toString()

    /** Values that have been safely promoted */
    opaque type Promoted = PromotionInfo

    /** Note: don't use `val` to avoid incorrect sharing */
    def empty: Promoted = new PromotionInfo

    extension (promoted: Promoted)
      def isCurrentObjectPromoted: Boolean = promoted.isCurrentObjectPromoted
      def promoteCurrent(thisRef: ThisRef): Unit = promoted.isCurrentObjectPromoted = true
      def contains(value: Value): Boolean = promoted.values.contains(value)
      def add(value: Value): Unit = promoted.values += value
      def remove(value: Value): Unit = promoted.values -= value
    end extension
  end Promoted
  type Promoted = Promoted.Promoted

  import Promoted.*
  inline def promoted(using p: Promoted): Promoted = p

  /** Cache used in fixed point computation
   *
   *  The analysis computes the least fixed point for the cache (see doc for
   *   `ExprValueCache`).
   *
   *  For the fixed point computation to terminate, we need to make sure that
   *  the domain of the cache, i.e. the key pair (Ref, Tree) is finite. As the
   *  code is finite, we only need to carefully design the abstract domain to
   *  be finitary.
   *
   *  We also need to make sure that the computing function (i.e. the abstract
   *  interpreter) is monotone. Error handling breaks monotonicity of the
   *  abstract interpreter, because when an error happens, we always return
   *  the bottom value `Hot` for an expression. It is not a threat for
   *  termination because when an error happens, we stop the fixed point
   *  computation at the end of the iteration where the error happens.
   *
   *  Note: It's tempting to use location of trees as key. That should
   *  be avoided as a template may have the same location as its single
   *  statement body. Macros may also create incorrect locations.
   */
  object Cache:
    /** Cache for expressions
     *
     *  Ref -> Tree -> Value
     *
     *  The first key is the value of `this` for the expression.
     *
     *  We do not need the heap in the key, because the value of an expression
     *  is only determined by the value of `this`. The heap is immutable: the
     *  abstract values for object fields never change within one iteration.
     *  The initial abstraction of a field is always a safe over-approximation
     *  thanks to monotonicity of initialization states.
     *
     *  If the heap is unstable in an iteration, the cache should also be
     *  unstable. This is because all values stored in the heap are also present
     *  in the cache. Therefore, we only need to track whether the cache is
     *  stable between two iterations.
     *
     *  The heap is not part of the fixed point computation -- we throw the
     *  unstable heap from last iteration away. In contrast, we use the unstable
     *  output cache from the last iteration as input for the next iteration.
     *  This is safe because the heap is determined by the cache -- it is a
     *  "local" data to the computing function, conceptually. Local data is
     *  always safe be discarded.
     *
     *  Now, if a fixed point is reached, the local data contains stable data
     *  that could be reused to check other classes. We employ this trick to
     *  improve performance of the analysis.
     */
    private type ExprValueCache = Map[Value, Map[TreeWrapper, Value]]

    /** The heap for abstract objects
     *
     *  The heap objects are immutable.
     */
    private type Heap = Map[Ref, Objekt]

    /** A wrapper for trees for storage in maps based on referential equality of trees. */
    private abstract class TreeWrapper:
      def tree: Tree

      override final def equals(other: Any): Boolean =
        other match
        case that: TreeWrapper => this.tree eq that.tree
        case _ => false

      override final def hashCode = tree.hashCode

    /** The immutable wrapper is intended to be stored as key in the heap. */
    private class ImmutableTreeWrapper(val tree: Tree) extends TreeWrapper

    /** For queries on the heap, reuse the same wrapper to avoid unnecessary allocation. */
    private class MutableTreeWrapper extends TreeWrapper:
      var queryTree: Tree | Null = null
      def tree: Tree = queryTree match
        case tree: Tree => tree
        case null => ???

    class Cache:
      /** The cache for expression values from last iteration */
      private var last: ExprValueCache =  Map.empty

      /** The updated cache for expression values based on the cache values from the last iteration
       *
       *  Both `last` and `current` are required to make sure an expression is evaluated once in each iteration.
       */
      private var current: ExprValueCache = Map.empty

      /** Global cached values for expressions
       *
       *  The values are only added when a fixed point is reached.
       *
       *  It is intended to improve performance for computation related to warm values.
       */
      private var stable: ExprValueCache = Map.empty

      /** Whether the current heap is different from the last heap?
       *
       *  `changed == false` implies that the fixed point has been reached.
       */
      private var changed: Boolean = false

      /** Abstract heap stores abstract objects
       *
       *  The heap serves as cache of summaries for warm objects and is shared for checking all classes.
       *
       *  The fact that objects of `ThisRef` are stored in heap is just an engineering convenience.
       *  Technically, we can also store the object directly in `ThisRef`.
       *
       *  The heap contains objects of two conceptually distinct kinds.
       *
       *  - Objects that are also in `heapStable` are flow-insensitive views of already initialized objects that are
       *    cached for reuse in analysis of later classes. These objects and their fields should never change; this is
       *    enforced using assertions.
       *
       *  - Objects that are not (yet) in `heapStable` are the flow-sensitive abstract state of objects being analyzed
       *    in the current iteration of the analysis of the current class. Their fields do change flow-sensitively: more
       *    fields are added as fields become initialized. These objects are valid only within the current iteration and
       *    are removed when moving to a new iteration of analyzing the current class. When the analysis of a class
       *    reaches a fixed point, these now stable flow-sensitive views of the object at the end of the constructor
       *    of the analyzed class now become the flow-insensitive views of already initialized objects and can therefore
       *    be added to `heapStable`.
       */
      private var heap: Heap = Map.empty

      /** Used to revert heap to last stable heap. */
      private var heapStable: Heap = Map.empty

      /** Used to avoid allocation, its state does not matter */
      private given MutableTreeWrapper = new MutableTreeWrapper

      def hasChanged = changed

      def get(value: Value, expr: Tree): Option[Value] =
        current.get(value, expr) match
        case None => stable.get(value, expr)
        case res => res

      /** Backup the state of the cache
       *
       *  All the shared data structures must be immutable.
       */
      def backup(): Cache =
        val cache = new Cache
        cache.last = this.last
        cache.current = this.current
        cache.stable = this.stable
        cache.heap = this.heap
        cache.heapStable = this.heapStable
        cache.changed = this.changed
        cache

      /** Restore state from a backup */
      def restore(cache: Cache) =
        this.last = cache.last
        this.current = cache.current
        this.stable = cache.stable
        this.heap = cache.heap
        this.heapStable = cache.heapStable
        this.changed = cache.changed

      /** Copy the value of `(value, expr)` from the last cache to the current cache
       *
       *  It assumes the value is `Hot` if it doesn't exist in the last cache.
       *
       *  It updates the current caches if the values change.
       *
       *  The two caches are required because we want to make sure in a new iteration, an expression is evaluated once.
       */
      def assume(value: Value, expr: Tree, cacheResult: Boolean)(fun: => Value): Contextual[Value] =
        val assumeValue: Value =
          last.get(value, expr) match
          case Some(value) => value
          case None =>
            this.last = last.updatedNested(value, expr, Hot)
            Hot

        this.current = current.updatedNested(value, expr, assumeValue)

        val actual = fun
        if actual != assumeValue then
          this.changed = true
          this.current = this.current.updatedNested(value, expr, actual)
        else
          // It's tempting to cache the value in stable, but it's unsound.
          // The reason is that the current value may depend on other values
          // which might change.
          //
          // stable.put(value, expr, actual)
          ()
        end if

        actual
      end assume

      /** Commit current cache to stable cache. */
      private def commitToStableCache() =
        for
          (v, m) <- current
          if v.isWarm          // It's useless to cache value for ThisRef.
          (wrapper, res) <- m
        do
          this.stable = stable.updatedNestedWrapper(v, wrapper.asInstanceOf[ImmutableTreeWrapper], res)

      /** Prepare cache for the next iteration
       *
       *  1. Reset changed flag.
       *
       *  2. Use current cache as last cache and set current cache to be empty.
       *
       *  3. Revert heap to stable.
       */
      def prepareForNextIteration()(using Context) =
        this.changed = false
        this.last = this.current
        this.current = Map.empty
        this.heap = this.heapStable

      /** Prepare for checking next class
       *
       *  1. Reset changed flag.
       *
       *  2. Commit current cache to stable cache if not changed.
       *
       *  3. Update stable heap if not changed.
       *
       *  4. Reset last cache.
       */
      def prepareForNextClass()(using Context) =
        if this.changed then
          this.changed = false
          this.heap = this.heapStable
        else
          this.commitToStableCache()
          this.heapStable = this.heap

        this.last = Map.empty
        this.current = Map.empty

      def updateObject(ref: Ref, obj: Objekt) =
        assert(!this.heapStable.contains(ref))
        this.heap = this.heap.updated(ref, obj)

      def containsObject(ref: Ref) = heap.contains(ref)

      def getObject(ref: Ref) = heap(ref)
    end Cache

    extension (cache: ExprValueCache)
      private def get(value: Value, expr: Tree)(using queryWrapper: MutableTreeWrapper): Option[Value] =
        queryWrapper.queryTree = expr
        cache.get(value).flatMap(_.get(queryWrapper))

      private def removed(value: Value, expr: Tree)(using queryWrapper: MutableTreeWrapper) =
        queryWrapper.queryTree = expr
        val innerMap2 = cache(value).removed(queryWrapper)
        cache.updated(value, innerMap2)

      private def updatedNested(value: Value, expr: Tree, result: Value): ExprValueCache =
        val wrapper = new ImmutableTreeWrapper(expr)
        updatedNestedWrapper(value, wrapper, result)

      private def updatedNestedWrapper(value: Value, wrapper: ImmutableTreeWrapper, result: Value): ExprValueCache =
        val innerMap = cache.getOrElse(value, Map.empty[TreeWrapper, Value])
        val innerMap2 = innerMap.updated(wrapper, result)
        cache.updated(value, innerMap2)
    end extension
  end Cache

  import Cache.*

  inline def cache(using c: Cache): Cache = c

// ----- Checker State -----------------------------------

  /** The state that threads through the interpreter */
  type Contextual[T] = (Context, Trace, Promoted, Cache, Reporter) ?=> T

// ----- Error Handling -----------------------------------

  object Trace:
    opaque type Trace = Vector[Tree]

    val empty: Trace = Vector.empty

    extension (trace: Trace)
      def add(node: Tree): Trace = trace :+ node
      def toVector: Vector[Tree] = trace

  type Trace = Trace.Trace

  import Trace.*
  def trace(using t: Trace): Trace = t
  inline def withTrace[T](t: Trace)(op: Trace ?=> T): T = op(using t)
  inline def extendTrace[T](node: Tree)(using t: Trace)(op: Trace ?=> T): T = op(using t.add(node))

  /** Error reporting */
  trait Reporter:
    def report(err: Error): Unit
    def reportAll(errs: Seq[Error]): Unit = for err <- errs do report(err)

  /** A TryReporter cannot be simply thrown away
   *
   *  Either `abort` should be called or the errors be reported.
   */
  trait TryReporter extends Reporter:
    def abort()(using Cache): Unit
    def errors: List[Error]

  object Reporter:
    class BufferedReporter extends Reporter:
      private val buf = new mutable.ArrayBuffer[Error]
      def errors = buf.toList
      def report(err: Error) = buf += err

    class TryBufferedReporter(backup: Cache) extends BufferedReporter with TryReporter:
      def abort()(using Cache): Unit = cache.restore(backup)

    class ErrorFound(val error: Error) extends Exception
    class StopEarlyReporter extends Reporter:
      def report(err: Error) = throw new ErrorFound(err)

    /** Capture all errors with a TryReporter
     *
     *  The TryReporter cannot be thrown away: either `abort` must be called or
     *  the errors must be reported.
     */
    def errorsIn(fn: Reporter ?=> Unit)(using Cache): TryReporter =
      val reporter = new TryBufferedReporter(cache.backup())
      fn(using reporter)
      reporter

    /** Stop on first error */
    def stopEarly(fn: Reporter ?=> Unit): List[Error] =
      val reporter: Reporter = new StopEarlyReporter

      try
        fn(using reporter)
        Nil
      catch case ex: ErrorFound =>
        ex.error :: Nil

    def hasErrors(fn: Reporter ?=> Unit)(using Cache): Boolean =
      val backup = cache.backup()
      val errors = stopEarly(fn)
      cache.restore(backup)
      errors.nonEmpty

  inline def reporter(using r: Reporter): Reporter = r

// ----- Operations on domains -----------------------------
  extension (a: Value)
    def join(b: Value): Value =
      (a, b) match
      case (Hot, _)  => b
      case (_, Hot)  => a

      case (Cold, _) => Cold
      case (_, Cold) => Cold

      case (a: (Fun | Warm | ThisRef), b: (Fun | Warm | ThisRef)) =>
        if a == b then a else RefSet(a :: b :: Nil)

      case (a: (Fun | Warm | ThisRef), RefSet(refs)) =>
        if refs.exists(_ == a) then b: Value // fix pickling test
        else RefSet(a :: refs)

      case (RefSet(refs), b: (Fun | Warm | ThisRef)) =>
        if refs.exists(_ == b) then a: Value // fix pickling test
        else RefSet(b :: refs)

      case (RefSet(refs1), RefSet(refs2)) =>
        val diff = refs2.filter(ref => refs1.forall(_ != ref))
        RefSet(refs1 ++ diff)

    /** Conservatively approximate the value with `Cold` or `Hot` */
    def widenArg: Contextual[Value] =
      a match
      case _: Ref | _: Fun =>
        val hasError = Reporter.hasErrors { a.promote("Argument cannot be promoted to hot") }
        if hasError then Cold else Hot

      case RefSet(refs) =>
        refs.map(_.widenArg).join

      case _ => a


  extension (values: Seq[Value])
    def join: Value =
      if values.isEmpty then Hot
      else values.reduce { (v1, v2) => v1.join(v2) }

    def widenArgs: Contextual[List[Value]] = values.map(_.widenArg).toList


  extension (ref: Ref)
    def objekt: Contextual[Objekt] =
      // TODO: improve performance
      ref match
        case warm: Warm => warm.ensureObjectExistsAndPopulated()
        case _ =>
      cache.getObject(ref)

    def ensureObjectExists()(using Cache): ref.type =
      if cache.containsObject(ref) then
        printer.println("object " + ref + " already exists")
        ref
      else
        ensureFresh()

    def ensureFresh()(using Cache): ref.type =
      val obj = Objekt(ref.klass, fields = Map.empty, outers = Map(ref.klass -> ref.outer))
      printer.println("reset object " + ref)
      cache.updateObject(ref, obj)
      ref

    /** Update field value of the abstract object
     *
     *  Invariant: fields are immutable and only set once
     */
    def updateField(field: Symbol, value: Value): Contextual[Unit] = log("set field " + field + " of " + ref + " to " + value) {
      val obj = objekt
      // We may reset the outers or params of a populated warm object.
      // This is the case if we need access the field of a warm object, which
      // requires population of parameters and outers; and later create an
      // instance of the exact warm object, whose initialization will reset
      // the outer and constructor parameters.
      //
      // See tests/init/neg/unsound1.scala
      val changed = !obj.hasField(field) || obj.field(field) != value
      def isParamUpdate = field.isOneOf(Flags.ParamAccessor | Flags.Param) && obj.field(field) == value
      assert(!obj.hasField(field) || isParamUpdate, field.show + " already init, new = " + value + ", old = " + obj.field(field) + ", ref = " + ref)
      val obj2 = obj.copy(fields = obj.fields.updated(field, value))
      if changed then cache.updateObject(ref, obj2)
    }

    /** Update the immediate outer of the given `klass` of the abstract object
     *
     *  Invariant: outers are immutable and only set once
     */
    def updateOuter(klass: ClassSymbol, value: Value): Contextual[Unit] = log("set outer " + klass + " of " + ref + " to " + value) {
      val obj = objekt
      // See the comment in `updateField` for setting the value twice.
      assert(!obj.hasOuter(klass) || obj.outer(klass) == value, klass.show + " already has outer, new = " + value + ", old = " + obj.outer(klass) + ", ref = " + ref)
      val obj2 = obj.copy(outers = obj.outers.updated(klass, value))
      cache.updateObject(ref, obj2)
    }
  end extension

  extension (value: Value)
    def ensureHot(msg: String): Contextual[Value] =
      value.promote(msg)
      value

    def select(field: Symbol, needResolve: Boolean = true): Contextual[Value] = log("select " + field.show + ", this = " + value, printer, (_: Value).show) {
      if promoted.isCurrentObjectPromoted then Hot
      else value match {
        case Hot  =>
          Hot

        case Cold =>
          val error = AccessCold(field, trace.toVector)
          reporter.report(error)
          Hot

        case ref: Ref =>
          val target = if needResolve then resolve(ref.klass, field) else field
          if target.is(Flags.Lazy) then
            val rhs = target.defTree.asInstanceOf[ValDef].rhs
            eval(rhs, ref, target.owner.asClass, cacheResult = true)
          else
            val obj = ref.objekt
            if obj.hasField(target) then
              obj.field(target)
            else if ref.isInstanceOf[Warm] then
              assert(obj.klass.isSubClass(target.owner))
              if target.is(Flags.ParamAccessor) then
                // possible for trait parameters
                // see tests/init/neg/trait2.scala
                //
                // return `Hot` here, errors are reported in checking `ThisRef`
                Hot
              else if target.hasSource then
                val rhs = target.defTree.asInstanceOf[ValOrDefDef].rhs
                eval(rhs, ref, target.owner.asClass, cacheResult = true)
              else
                val error = CallUnknown(field, trace.toVector)
                reporter.report(error)
                Hot
            else
              val error = AccessNonInit(target, trace.toVector)
              reporter.report(error)
              Hot

        case fun: Fun =>
          report.error("[Internal error] unexpected tree in selecting a function, fun = " + fun.expr.show, fun.expr)
          Hot

        case RefSet(refs) =>
          refs.map(_.select(field)).join
      }
    }

    def call(meth: Symbol, args: List[ArgInfo], receiver: Type, superType: Type, needResolve: Boolean = true): Contextual[Value] = log("call " + meth.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
      def promoteArgs(): Contextual[Unit] = args.foreach(_.promote)

      def isSyntheticApply(meth: Symbol) =
        meth.is(Flags.Synthetic)
        && meth.name == nme.apply
        && meth.owner.is(Flags.Module)
        && meth.owner.companionClass.is(Flags.Case)

      def isAlwaysSafe(meth: Symbol) =
        (meth eq defn.Object_eq)
        || (meth eq defn.Object_ne)
        || (meth eq defn.Any_isInstanceOf)

      def checkArgsWithParametricity() =
        val methodType = atPhaseBeforeTransforms { meth.info.stripPoly }
        var allArgsHot = true
        val allParamTypes = methodType.paramInfoss.flatten.map(_.repeatedToSingle)
        val errors = allParamTypes.zip(args).flatMap { (info, arg) =>
          val tryReporter = Reporter.errorsIn { arg.promote }
          allArgsHot = allArgsHot && tryReporter.errors.isEmpty
          if tryReporter.errors.isEmpty then tryReporter.errors
          else
            info match
            case typeParamRef: TypeParamRef =>
              val bounds = typeParamRef.underlying.bounds
              val isWithinBounds = bounds.lo <:< defn.NothingType && defn.AnyType <:< bounds.hi
              def otherParamContains = allParamTypes.exists { param => param != typeParamRef && param.typeSymbol != defn.ClassTagClass && typeParamRef.occursIn(param) }
              // A non-hot method argument is allowed if the corresponding parameter type is a
              // type parameter T with Any as its upper bound and Nothing as its lower bound.
              // the other arguments should either correspond to a parameter type that is T
              // or that does not contain T as a component.
              if isWithinBounds && !otherParamContains then
                tryReporter.abort()
                Nil
              else
                tryReporter.errors
            case _ => tryReporter.errors
        }
        (errors, allArgsHot)

      // fast track if the current object is already initialized
      if promoted.isCurrentObjectPromoted then Hot
      else if isAlwaysSafe(meth) then Hot
      else if meth eq defn.Any_asInstanceOf then value
      else value match {
        case Hot  =>
          if isSyntheticApply(meth) && meth.hasSource then
            val klass = meth.owner.companionClass.asClass
            instantiate(klass, klass.primaryConstructor, args)
          else
            if receiver.typeSymbol.isStaticOwner then
              val (errors, allArgsHot) = checkArgsWithParametricity()
              if allArgsHot then
                Hot: Value
              else if errors.nonEmpty then
                reporter.reportAll(errors)
                Hot: Value
              else
                Cold: Value
            else
              promoteArgs()
              Hot

        case Cold =>
          promoteArgs()
          val error = CallCold(meth, trace.toVector)
          reporter.report(error)
          Hot

        case ref: Ref =>
          val isLocal = !meth.owner.isClass
          val target =
            if !needResolve then
              meth
            else if superType.exists then
              resolveSuper(ref.klass, superType, meth)
            else
              resolve(ref.klass, meth)

          if target.isOneOf(Flags.Method) then
            if target.hasSource then
              val cls = target.owner.enclosingClass.asClass
              val ddef = target.defTree.asInstanceOf[DefDef]
              val tryReporter = Reporter.errorsIn { promoteArgs() }
              // normal method call
              if tryReporter.errors.nonEmpty && isSyntheticApply(meth) then
                tryReporter.abort()
                val klass = meth.owner.companionClass.asClass
                val outerCls = klass.owner.lexicallyEnclosingClass.asClass
                val outer = resolveOuterSelect(outerCls, ref, 1)
                outer.instantiate(klass, klass.primaryConstructor, args)
              else
                reporter.reportAll(tryReporter.errors)
                extendTrace(ddef) {
                  eval(ddef.rhs, ref, cls, cacheResult = true)
                }
            else if ref.canIgnoreMethodCall(target) then
              Hot
            else
              // no source code available
              promoteArgs()
              val error = CallUnknown(target, trace.toVector)
              reporter.report(error)
              Hot
          else
            // method call resolves to a field
            val obj = ref.objekt
            if obj.hasField(target) then
              obj.field(target)
            else
              value.select(target, needResolve = false)

        case Fun(body, thisV, klass) =>
          // meth == NoSymbol for poly functions
          if meth.name.toString == "tupled" then value // a call like `fun.tupled`
          else
            promoteArgs()
            eval(body, thisV, klass, cacheResult = true)

        case RefSet(refs) =>
          refs.map(_.call(meth, args, receiver, superType)).join
      }
    }

    def callConstructor(ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("call " + ctor.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
      // init "fake" param fields for parameters of primary and secondary constructors
      def addParamsAsFields(args: List[Value], ref: Ref, ctorDef: DefDef) =
        val params = ctorDef.termParamss.flatten.map(_.symbol)
        assert(args.size == params.size, "arguments = " + args.size + ", params = " + params.size)
        for (param, value) <- params.zip(args) do
          ref.updateField(param, value)
          printer.println(param.show + " initialized with " + value)

      value match {
        case Hot | Cold | _: RefSet | _: Fun =>
          report.error("unexpected constructor call, meth = " + ctor + ", value = " + value, trace.toVector.last)
          Hot

        case ref: Warm if ref.isPopulatingParams =>
          if ctor.hasSource then
            val cls = ctor.owner.enclosingClass.asClass
            val ddef = ctor.defTree.asInstanceOf[DefDef]
            val args2 = args.map(_.value).widenArgs
            addParamsAsFields(args2, ref, ddef)
            if ctor.isPrimaryConstructor then
              val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
              extendTrace(cls.defTree) { init(tpl, ref, cls) }
            else
              val initCall = ddef.rhs match
                case Block(call :: _, _) => call
                case call => call
              extendTrace(ddef) { eval(initCall, ref, cls) }
            end if
          else
            Hot

        case ref: Ref =>
          if ctor.hasSource then
            val cls = ctor.owner.enclosingClass.asClass
            val ddef = ctor.defTree.asInstanceOf[DefDef]
            val args2 = args.map(_.value).widenArgs
            addParamsAsFields(args2, ref, ddef)
            if ctor.isPrimaryConstructor then
              val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
              extendTrace(cls.defTree) { eval(tpl, ref, cls, cacheResult = true) }
              ref
            else
              extendTrace(ddef) { eval(ddef.rhs, ref, cls, cacheResult = true) }
          else if ref.canIgnoreMethodCall(ctor) then
            Hot
          else
            // no source code available
            val error = CallUnknown(ctor, trace.toVector)
            reporter.report(error)
            Hot
      }

    }

    /** Handle a new expression `new p.C` where `p` is abstracted by `value` */
    def instantiate(klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("instantiating " + klass.show + ", value = " + value + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
      def tryLeak(warm: Warm, nonHotOuterClass: Symbol, argValues: List[Value]): Contextual[Value] =
        val argInfos2 = args.zip(argValues).map { (argInfo, v) => argInfo.copy(value = v) }
        val errors = Reporter.stopEarly {
          given Trace = Trace.empty
          warm.callConstructor(ctor, argInfos2)
        }
        if errors.nonEmpty then
          val indices =
            for
              (arg, i) <- argValues.zipWithIndex
              if arg.isCold
            yield
              i + 1

          val error = UnsafeLeaking(trace.toVector, errors.head, nonHotOuterClass, indices)
          reporter.report(error)
          Hot
        else
          warm

      if promoted.isCurrentObjectPromoted then Hot
      else value match {
        case Hot  =>
          var allHot = true
          val args2 = args.map { arg =>
            val hasErrors = Reporter.hasErrors { arg.promote }
            allHot = allHot && !hasErrors
            if hasErrors then arg.value.widenArg
            else Hot
          }

          if allHot then
            Hot
          else
            val outer = Hot
            val warm = Warm(klass, outer, ctor, args2).ensureObjectExists()
            tryLeak(warm, NoSymbol, args2)

        case Cold =>
          val error = CallCold(ctor, trace.toVector)
          reporter.report(error)
          Hot

        case ref: Ref =>
          // widen the outer to finitize the domain
          val outer = ref match
            case warm @ Warm(_, _: Warm, _, _) =>
              // the widened warm object might not exist in the heap
              warm.copy(outer = Cold).ensureObjectExistsAndPopulated()
            case _ => ref

          val argsWidened = args.map(_.value).widenArgs
          val warm = Warm(klass, outer, ctor, argsWidened).ensureObjectExists()
          if argsWidened.exists(_.isCold) then
            tryLeak(warm, klass.owner.lexicallyEnclosingClass, argsWidened)
          else
            val argInfos2 = args.zip(argsWidened).map { (argInfo, v) => argInfo.copy(value = v) }
            warm.callConstructor(ctor, argInfos2)
            warm

        case Fun(body, thisV, klass) =>
          report.error("[Internal error] unexpected tree in instantiating a function, fun = " + body.show, trace.toVector.last)
          Hot

        case RefSet(refs) =>
          refs.map(_.instantiate(klass, ctor, args)).join
      }
    }
  end extension

  extension (ref: Ref)
    def accessLocal(tmref: TermRef, klass: ClassSymbol): Contextual[Value] =
      val sym = tmref.symbol

      if sym.is(Flags.Param) && sym.owner.isConstructor then
        val enclosingClass = sym.owner.enclosingClass.asClass
        val thisValue2 = resolveThis(enclosingClass, ref, klass)
        thisValue2 match
        case Hot => Hot
        case ref: Ref => ref.objekt.field(sym)
        case _ =>
            report.error("[Internal error] unexpected this value accessing local variable, sym = " + sym.show + ", thisValue = " + thisValue2.show, trace.toVector.last)
            Hot
      else if sym.is(Flags.Param) then
        Hot
      else
        sym.defTree match {
          case vdef: ValDef =>
            // resolve this for local variable
            val enclosingClass = sym.owner.enclosingClass.asClass
            val thisValue2 = resolveThis(enclosingClass, ref, klass)
            thisValue2 match
              case Hot => Hot

              case Cold => Cold

              case ref: Ref => eval(vdef.rhs, ref, enclosingClass)

              case _ =>
                 report.error("[Internal error] unexpected this value when accessing local variable, sym = " + sym.show + ", thisValue = " + thisValue2.show, trace.toVector.last)
                 Hot
            end match

          case _ => Hot
        }
  end extension

// ----- Promotion ----------------------------------------------------
  extension (ref: Ref)
    /** Whether the object is fully assigned
     *
     *  It means all fields and outers are set. For performance, we don't check
     *  outers here, because Scala semantics ensure that they are always set
     *  before any user code in the constructor.
     *
     *  Note that `isFullyFilled = true` does not mean we can use the
     *  object freely, as its fields or outers may still reach uninitialized
     *  objects.
     */
    def isFullyFilled: Contextual[Boolean] = log("isFullyFilled " + ref, printer) {
      val obj = ref.objekt
      ref.klass.baseClasses.forall { klass =>
        !klass.hasSource || {
          val nonInits = klass.info.decls.filter { member =>
            !member.isOneOf(Flags.Method | Flags.Lazy | Flags.Deferred)
            && !member.isType
            && !obj.hasField(member)
          }
          printer.println("nonInits = " + nonInits)
          nonInits.isEmpty
        }
      }
    }

  end extension

  extension (thisRef: ThisRef)
    def tryPromoteCurrentObject(): Contextual[Boolean] = log("tryPromoteCurrentObject ", printer) {
      if promoted.isCurrentObjectPromoted then
        true
      else if thisRef.isFullyFilled then
        // If we have all fields initialized, then we can promote This to hot.
        promoted.promoteCurrent(thisRef)
        true
      else
        false
    }

  extension (value: Value)
    /** Promotion of values to hot */
    def promote(msg: String): Contextual[Unit] = log("promoting " + value + ", promoted = " + promoted, printer) {
      if promoted.isCurrentObjectPromoted then Nil else

        value.match
        case Hot   =>

        case Cold  =>
          reporter.report(PromoteError(msg, trace.toVector))

        case thisRef: ThisRef =>
          if !thisRef.tryPromoteCurrentObject() then
            reporter.report(PromoteError(msg, trace.toVector))

        case warm: Warm =>
          if !promoted.contains(warm) then
            promoted.add(warm)
            val errors = warm.tryPromote(msg)
            if errors.nonEmpty then promoted.remove(warm)
            reporter.reportAll(errors)

        case fun @ Fun(body, thisV, klass) =>
          if !promoted.contains(fun) then
            val errors = Reporter.stopEarly {
              val res = {
                given Trace = Trace.empty
                eval(body, thisV, klass)
              }
              given Trace = Trace.empty.add(body)
              res.promote("The function return value is not fully initialized.")
            }
            if errors.nonEmpty then
              reporter.report(UnsafePromotion(msg, trace.toVector, errors.head))
            else
              promoted.add(fun)

        case RefSet(refs) =>
          refs.foreach(_.promote(msg))
    }
  end extension

  extension (warm: Warm)
    /** Try early promotion of warm objects
     *
     *  Promotion is expensive and should only be performed for small classes.
     *
     *  1. for each concrete method `m` of the warm object:
     *     call the method and promote the result
     *
     *  2. for each concrete field `f` of the warm object:
     *     promote the field value
     *
     *  If the object contains nested classes as members, the checker simply
     *  reports a warning to avoid expensive checks.
     *
     *  TODO: we need to revisit whether this is needed once we make the
     *  system more flexible in other dimentions: e.g. leak to
     *  methods or constructors, or use ownership for creating cold data structures.
     */
    def tryPromote(msg: String): Contextual[List[Error]] = log("promote " + warm.show + ", promoted = " + promoted, printer) {
      val classRef = warm.klass.appliedRef
      if classRef.memberClasses.nonEmpty || !warm.isFullyFilled then
        return PromoteError(msg, trace.toVector) :: Nil

      val errors = Reporter.stopEarly {
        for klass <- warm.klass.baseClasses if klass.hasSource do
          for member <- klass.info.decls do
            if !member.isType && !member.isConstructor && member.hasSource  && !member.is(Flags.Deferred) then
              if member.is(Flags.Method, butNot = Flags.Accessor) then
                withTrace(Trace.empty) {
                  val args = member.info.paramInfoss.flatten.map(_ => ArgInfo(Hot, Trace.empty))
                  val res = warm.call(member, args, receiver = NoType, superType = NoType)
                  res.promote("Cannot prove that the return value of " + member + " is fully initialized.")
                }
              else
                withTrace(Trace.empty) {
                  val res = warm.select(member)
                  res.promote("Cannot prove that the field " + member + " is fully initialized.")
                }
          end for
        end for
      }

      if errors.isEmpty then Nil
      else UnsafePromotion(msg, trace.toVector, errors.head) :: Nil
    }

  end extension

// ----- Policies ------------------------------------------------------
  extension (value: Ref)
    /** Can the method call on `value` be ignored?
     *
     *  Note: assume overriding resolution has been performed.
     */
    def canIgnoreMethodCall(meth: Symbol)(using Context): Boolean =
      val cls = meth.owner
      cls == defn.AnyClass ||
      cls == defn.AnyValClass ||
      cls == defn.ObjectClass

// ----- Work list ---------------------------------------------------
  case class Task(value: ThisRef)

  class WorkList private[Semantic]():
    private var pendingTasks: List[Task] = Nil

    def addTask(task: Task): Unit =
      if !pendingTasks.contains(task) then pendingTasks = task :: pendingTasks

    /** Process the worklist until done */
    final def work()(using Cache, Context): Unit =
      for task <- pendingTasks
      do doTask(task)

    /** Check an individual class
     *
     *  This method should only be called from the work list scheduler.
     */
    private def doTask(task: Task)(using Cache, Context): Unit =
      val thisRef = task.value
      val tpl = thisRef.klass.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]

      @tailrec
      def iterate(): Unit = {
        given Promoted = Promoted.empty
        given Trace = Trace.empty.add(thisRef.klass.defTree)
        given reporter: Reporter.BufferedReporter = new Reporter.BufferedReporter

        thisRef.ensureFresh()

        // set up constructor parameters
        for param <- tpl.constr.termParamss.flatten do
          thisRef.updateField(param.symbol, Hot)

        log("checking " + task) { eval(tpl, thisRef, thisRef.klass) }
        reporter.errors.foreach(_.issue)

        if cache.hasChanged && reporter.errors.isEmpty then
          // code to prepare cache and heap for next iteration
          cache.prepareForNextIteration()
          iterate()
        else
          cache.prepareForNextClass()
      }

      iterate()
    end doTask
  end WorkList
  inline def workList(using wl: WorkList): WorkList = wl

// ----- API --------------------------------

  /** Add a checking task to the work list */
  def addTask(thisRef: ThisRef)(using WorkList) = workList.addTask(Task(thisRef))

  /** Perform check on the work list until it becomes empty
   *
   *  Should only be called once from the checker.
   */
  def check()(using Cache, WorkList, Context) = workList.work()

  /** Perform actions with initial checking state.
   *
   *      Semantic.withInitialState {
   *         Semantic.addTask(...)
   *         ...
   *         Semantic.check()
   *      }
   */
  def withInitialState[T](work: (Cache, WorkList) ?=> T): T =
    work(using new Cache, new WorkList)

// ----- Semantic definition --------------------------------

  /** Utility definition used for better error-reporting of argument errors */
  case class ArgInfo(value: Value, trace: Trace):
    def promote: Contextual[Unit] = withTrace(trace) {
      value.promote("Cannot prove the argument is fully initialized. Only fully initialized values are safe to leak.")
    }

  /** Evaluate an expression with the given value for `this` in a given class `klass`
   *
   *  Note that `klass` might be a super class of the object referred by `thisV`.
   *  The parameter `klass` is needed for `this` resolution. Consider the following code:
   *
   *  class A {
   *    A.this
   *    class B extends A { A.this }
   *  }
   *
   *  As can be seen above, the meaning of the expression `A.this` depends on where
   *  it is located.
   *
   *  This method only handles cache logic and delegates the work to `cases`.
   *
   *  The parameter `cacheResult` is used to reduce the size of the cache.
   */
  def eval(expr: Tree, thisV: Ref, klass: ClassSymbol, cacheResult: Boolean = false): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + " in " + klass.show, printer, (_: Value).show) {
    cache.get(thisV, expr) match
    case Some(value) => value
    case None =>
      cache.assume(thisV, expr, cacheResult) { cases(expr, thisV, klass) }
  }

  /** Evaluate a list of expressions */
  def eval(exprs: List[Tree], thisV: Ref, klass: ClassSymbol): Contextual[List[Value]] =
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Evaluate arguments of methods */
  def evalArgs(args: List[Arg], thisV: Ref, klass: ClassSymbol): Contextual[List[ArgInfo]] =
    val argInfos = new mutable.ArrayBuffer[ArgInfo]
    args.foreach { arg =>
      val res =
        if arg.isByName then
          Fun(arg.tree, thisV, klass)
        else
          eval(arg.tree, thisV, klass)

      argInfos += ArgInfo(res, trace.add(arg.tree))
    }
    argInfos.toList

  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   */
  def cases(expr: Tree, thisV: Ref, klass: ClassSymbol): Contextual[Value] =
    val trace2 = trace.add(expr)
    expr match
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Hot

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        withTrace(trace2) { cases(expr.tpe, thisV, klass) }

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        withTrace(trace2) {
          val outer = outerValue(tref, thisV, klass)
          outer.instantiate(cls, ctor, args)
        }

      case Call(ref, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe: @unchecked
          val thisValue2 = extendTrace(ref) { resolveThis(thisTp.classSymbol.asClass, thisV, klass) }
          withTrace(trace2) { thisValue2.call(ref.symbol, args, thisTp, superTp) }

        case Select(qual, _) =>
          val receiver = eval(qual, thisV, klass)
          if ref.symbol.isConstructor then
            withTrace(trace2) { receiver.callConstructor(ref.symbol, args) }
          else
            withTrace(trace2) { receiver.call(ref.symbol, args, receiver = qual.tpe, superType = NoType) }

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = extendTrace(ref) { resolveThis(enclosingClass, thisV, klass) }
            // local methods are not a member, but we can reuse the method `call`
            withTrace(trace2) { thisValue2.call(id.symbol, args, receiver = NoType, superType = NoType, needResolve = false) }
          case TermRef(prefix, _) =>
            val receiver = cases(prefix, thisV, klass)
            if id.symbol.isConstructor then
              withTrace(trace2) { receiver.callConstructor(id.symbol, args) }
            else
              withTrace(trace2) { receiver.call(id.symbol, args, receiver = prefix, superType = NoType) }

      case Select(qualifier, name) =>
        val qual = eval(qualifier, thisV, klass)

        name match
          case OuterSelectName(_, hops) =>
            val SkolemType(tp) = expr.tpe: @unchecked
            withTrace(trace2) { resolveOuterSelect(tp.classSymbol.asClass, qual, hops) }
          case _ =>
            withTrace(trace2) { qual.select(expr.symbol) }

      case _: This =>
        cases(expr.tpe, thisV, klass)

      case Literal(_) =>
        Hot

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot))
          Hot
        else
          checkTermUsage(tpt, thisV, klass)
          eval(expr, thisV, klass)

      case NamedArg(name, arg) =>
        eval(arg, thisV, klass)

      case Assign(lhs, rhs) =>
        lhs match
        case Select(qual, _) =>
          eval(qual, thisV, klass)
          val res = eval(rhs, thisV, klass)
          extendTrace(rhs) {
            res.ensureHot("The RHS of reassignment must be fully initialized.")
          }
        case id: Ident =>
          val res = eval(rhs, thisV, klass)
          extendTrace(rhs) {
            res.ensureHot("The RHS of reassignment must be fully initialized.")
          }

      case closureDef(ddef) =>
        Fun(ddef.rhs, thisV, klass)

      case PolyFun(body) =>
        Fun(body, thisV, klass)

      case Block(stats, expr) =>
        eval(stats, thisV, klass)
        eval(expr, thisV, klass)

      case If(cond, thenp, elsep) =>
        eval(cond :: thenp :: elsep :: Nil, thisV, klass).join

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Hot
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        val res = eval(selector, thisV, klass)
        extendTrace(selector) {
          res.ensureHot("The value to be matched needs to be fully initialized.")
        }
        eval(cases.map(_.body), thisV, klass).join

      case Return(expr, from) =>
        val res = eval(expr, thisV, klass)
        extendTrace(expr) {
          res.ensureHot("return expression must be fully initialized.")
        }

      case WhileDo(cond, body) =>
        eval(cond :: body :: Nil, thisV, klass)
        Hot

      case Labeled(_, expr) =>
        eval(expr, thisV, klass)

      case Try(block, cases, finalizer) =>
        eval(block, thisV, klass)
        if !finalizer.isEmpty then
          eval(finalizer, thisV, klass)
        eval(cases.map(_.body), thisV, klass).join

      case SeqLiteral(elems, elemtpt) =>
        elems.map { elem => eval(elem, thisV, klass) }.join

      case Inlined(call, bindings, expansion) =>
        eval(bindings, thisV, klass)
        withTrace(trace2) { eval(expansion, thisV, klass) }

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Hot

      case vdef : ValDef =>
        // local val definition
        eval(vdef.rhs, thisV, klass)

      case ddef : DefDef =>
        // local method
        Hot

      case tdef: TypeDef =>
        // local type definition
        if tdef.isClassDef then
          Hot
        else
          checkTermUsage(tdef.rhs, thisV, klass)
          Hot

      case tpl: Template =>
        init(tpl, thisV, klass)

      case _: Import | _: Export =>
        Hot

      case _ =>
        report.error("[Internal error] unexpected tree", expr)
        Hot

  /** Handle semantics of leaf nodes */
  def cases(tp: Type, thisV: Ref, klass: ClassSymbol): Contextual[Value] = log("evaluating " + tp.show, printer, (_: Value).show) {
    tp match
      case _: ConstantType =>
        Hot

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        thisV.accessLocal(tmref, klass)

      case tmref: TermRef =>
        cases(tmref.prefix, thisV, klass).select(tmref.symbol)

      case tp @ ThisType(tref) =>
        val cls = tref.classSymbol.asClass
        if cls.isStaticOwner && !klass.isContainedIn(cls) then
          // O.this outside the body of the object O
          Hot
        else
          val value = resolveThis(cls, thisV, klass)
          value

      case _: TermParamRef | _: RecThis  =>
        // possible from checking effects of types
        Hot

      case _ =>
        report.error("[Internal error] unexpected type " + tp, trace.toVector.last)
        Hot
  }

  /** Resolve C.this that appear in `klass` */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol): Contextual[Value] = log("resolving " + target.show + ", this = " + thisV.show + " in " + klass.show, printer, (_: Value).show) {
    if target == klass then thisV
    else if target.is(Flags.Package) then Hot
    else
      thisV match
        case Hot => Hot
        case ref: Ref =>
          val obj = ref.objekt
          val outerCls = klass.owner.lexicallyEnclosingClass.asClass
          if !obj.hasOuter(klass) then
            val error = PromoteError("[Internal error] outer not yet initialized, target = " + target + ", klass = " + klass + ", object = " + obj, trace.toVector)
            report.error(error.show, trace.toVector.last)
            Hot
          else
            resolveThis(target, obj.outer(klass), outerCls)
        case RefSet(refs) =>
          refs.map(ref => resolveThis(target, ref, klass)).join
        case fun: Fun =>
          report.error("[Internal error] unexpected thisV = " + thisV + ", target = " + target.show + ", klass = " + klass.show, trace.toVector.last)
          Cold
        case Cold => Cold

  }

  /** Resolve outer select introduced during inlining.
   *
   *  See `tpd.outerSelect` and `ElimOuterSelect`.
   */
  def resolveOuterSelect(target: ClassSymbol, thisV: Value, hops: Int): Contextual[Value] = log("resolving outer " + target.show + ", this = " + thisV.show + ", hops = " + hops, printer, (_: Value).show) {
    // Is `target` reachable from `cls` with the given `hops`?
    def reachable(cls: ClassSymbol, hops: Int): Boolean = log("reachable from " + cls + " -> " + target + " in " + hops, printer) {
      if hops == 0 then cls == target
      else reachable(cls.owner.lexicallyEnclosingClass.asClass, hops - 1)
    }

    thisV match
      case Hot => Hot

      case ref: Ref =>
        val obj = ref.objekt
        val curOpt = obj.klass.baseClasses.find(cls => reachable(cls, hops))
        curOpt match
          case Some(cur) =>
            resolveThis(target, thisV, cur)

          case None =>
            // TODO: use error once we fix https://github.com/lampepfl/dotty/issues/15465
            report.warning("[Internal error] unexpected outerSelect, thisV = " + thisV + ", target = " + target.show + ", hops = " + hops, trace.toVector.last.srcPos)
            Cold

      case RefSet(refs) =>
        refs.map(ref => resolveOuterSelect(target, ref, hops)).join

      case fun: Fun =>
        report.error("[Internal error] unexpected thisV = " + thisV + ", target = " + target.show + ", hops = " + hops, trace.toVector.last.srcPos)
        Cold

      case Cold => Cold
  }

  /** Compute the outer value that correspond to `tref.prefix` */
  def outerValue(tref: TypeRef, thisV: Ref, klass: ClassSymbol): Contextual[Value] =
    val cls = tref.classSymbol.asClass
    if tref.prefix == NoPrefix then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      val outerV = resolveThis(enclosing, thisV, klass)
      outerV
    else
      if cls.isAllOf(Flags.JavaInterface) then Hot
      else cases(tref.prefix, thisV, klass)

  /** Initialize part of an abstract object in `klass` of the inheritance chain */
  def init(tpl: Template, thisV: Ref, klass: ClassSymbol): Contextual[Value] = log("init " + klass.show, printer, (_: Value).show) {
    val paramsMap = tpl.constr.termParamss.flatten.map { vdef =>
      vdef.name -> thisV.objekt.field(vdef.symbol)
    }.toMap

    // init param fields
    klass.paramGetters.foreach { acc =>
      val value = paramsMap(acc.name.toTermName)
      thisV.updateField(acc, value)
      printer.println(acc.show + " initialized with " + value)
    }

    // Tasks is used to schedule super constructor calls.
    // Super constructor calls are delayed until all outers are set.
    type Tasks = mutable.ArrayBuffer[() => Unit]
    def superCall(tref: TypeRef, ctor: Symbol, args: List[ArgInfo], tasks: Tasks): Unit =
      val cls = tref.classSymbol.asClass
      // update outer for super class
      val res = outerValue(tref, thisV, klass)
      thisV.updateOuter(cls, res)

      // follow constructor
      if cls.hasSource then
        tasks.append { () =>
          printer.println("init super class " + cls.show)
          thisV.callConstructor(ctor, args)
          ()
        }

    // parents
    def initParent(parent: Tree, tasks: Tasks) =
      parent match
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        eval(stats, thisV, klass)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args, tasks)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>       // extends A(args)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args, tasks)

      case _ =>   // extends A or extends A[T]
        val tref = typeRefOf(parent.tpe)
        superCall(tref, tref.classSymbol.primaryConstructor, Nil, tasks)

    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html
    if !klass.is(Flags.Trait) then
      // outers are set first
      val tasks = new mutable.ArrayBuffer[() => Unit]

      // 1. first init parent class recursively
      // 2. initialize traits according to linearization order
      val superParent = tpl.parents.head
      val superCls = superParent.tpe.classSymbol.asClass
      extendTrace(superParent) { initParent(superParent, tasks) }

      val parents = tpl.parents.tail
      val mixins = klass.baseClasses.tail.takeWhile(_ != superCls)

      // The interesting case is the outers for traits.  The compiler
      // synthesizes proxy accessors for the outers in the class that extends
      // the trait. As those outers must be stable values, they are initialized
      // immediately following class parameters and before super constructor
      // calls and user code in the class body.
      mixins.reverse.foreach { mixin =>
        parents.find(_.tpe.classSymbol == mixin) match
        case Some(parent) =>
          extendTrace(parent) { initParent(parent, tasks) }
        case None =>
          // According to the language spec, if the mixin trait requires
          // arguments, then the class must provide arguments to it explicitly
          // in the parent list. That means we will encounter it in the Some
          // branch.
          //
          // When a trait A extends a parameterized trait B, it cannot provide
          // term arguments to B. That can only be done in a concrete class.
          val tref = typeRefOf(klass.typeRef.baseType(mixin).typeConstructor)
          val ctor = tref.classSymbol.primaryConstructor
          if ctor.exists then extendTrace(superParent) {
            superCall(tref, ctor, Nil, tasks)
          }
      }

      // initialize super classes after outers are set
      tasks.foreach(task => task())
    end if

    var fieldsChanged = true

    // class body
    if thisV.isThisRef || !thisV.asInstanceOf[Warm].isPopulatingParams then tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) && !vdef.rhs.isEmpty =>
        val res = eval(vdef.rhs, thisV, klass)
        thisV.updateField(vdef.symbol, res)
        fieldsChanged = true

      case _: MemberDef =>

      case tree =>
        if fieldsChanged && thisV.isThisRef then
          thisV.asInstanceOf[ThisRef].tryPromoteCurrentObject()
        fieldsChanged = false
        eval(tree, thisV, klass)
    }

    // ensure we try promotion once even if class body is empty
    if fieldsChanged && thisV.isThisRef then
      thisV.asInstanceOf[ThisRef].tryPromoteCurrentObject()

    // The result value is ignored, use Hot to avoid futile fixed point computation
    Hot
  }

  /** Check that path in path-dependent types are initialized
   *
   *  This is intended to avoid type soundness issues in Dotty.
   */
  def checkTermUsage(tpt: Tree, thisV: Ref, klass: ClassSymbol): Contextual[Unit] =
    val traverser = new TypeTraverser:
      def traverse(tp: Type): Unit =
        tp match
        case TermRef(_: SingletonType, _) =>
          extendTrace(tpt) { cases(tp, thisV, klass) }
        case _ =>
          traverseChildren(tp)

    traverser.traverse(tpt.tpe)

// ----- Utility methods and extractors --------------------------------

  def typeRefOf(tp: Type)(using Context): TypeRef = tp.dealias.typeConstructor match
    case tref: TypeRef => tref
    case hklambda: HKTypeLambda => typeRefOf(hklambda.resType)


  opaque type Arg  = Tree | ByNameArg
  case class ByNameArg(tree: Tree)

  extension (arg: Arg)
    def isByName = arg.isInstanceOf[ByNameArg]
    def tree: Tree = arg match
      case t: Tree      => t
      case ByNameArg(t) => t

  object Call:

    def unapply(tree: Tree)(using Context): Option[(Tree, List[List[Arg]])] =
      tree match
      case Apply(fn, args) =>
        val argTps = fn.tpe.widen match
          case mt: MethodType => mt.paramInfos
        val normArgs: List[Arg] = args.zip(argTps).map {
          case (arg, _: ExprType) => ByNameArg(arg)
          case (arg, _)           => arg
        }
        unapply(fn) match
        case Some((ref, args0)) => Some((ref, args0 :+ normArgs))
        case None => None

      case TypeApply(fn, targs) =>
        unapply(fn)

      case ref: RefTree if ref.tpe.widenSingleton.isInstanceOf[MethodicType] =>
        Some((ref, Nil))

      case _ => None

  object NewExpr:
    def unapply(tree: Tree)(using Context): Option[(TypeRef, New, Symbol, List[List[Arg]])] =
      tree match
      case Call(fn @ Select(newTree: New, init), argss) if init == nme.CONSTRUCTOR =>
        val tref = typeRefOf(newTree.tpe)
        Some((tref, newTree, fn.symbol, argss))
      case _ => None

  object PolyFun:
    def unapply(tree: Tree)(using Context): Option[Tree] =
      tree match
      case Block((cdef: TypeDef) :: Nil, Typed(NewExpr(tref, _, _, _), _))
      if tref.symbol.isAnonymousClass && tref <:< defn.PolyFunctionType
      =>
        val body = cdef.rhs.asInstanceOf[Template].body
        val apply = body.head.asInstanceOf[DefDef]
        Some(apply.rhs)
      case _ =>
        None

  extension (symbol: Symbol) def hasSource(using Context): Boolean =
    !symbol.defTree.isEmpty

  def resolve(cls: ClassSymbol, sym: Symbol)(using Context): Symbol = log("resove " + cls + ", " + sym, printer, (_: Symbol).show) {
    if (sym.isEffectivelyFinal || sym.isConstructor) sym
    else sym.matchingMember(cls.appliedRef)
  }

  def resolveSuper(cls: ClassSymbol, superType: Type, sym: Symbol)(using Context): Symbol =
    import annotation.tailrec
    @tailrec def loop(bcs: List[ClassSymbol]): Symbol = bcs match {
      case bc :: bcs1 =>
        val cand = sym.matchingDecl(bcs.head, cls.thisType)
          .suchThat(alt => !alt.is(Flags.Deferred)).symbol
        if (cand.exists) cand else loop(bcs.tail)
      case _ =>
        NoSymbol
    }
    loop(cls.info.baseClasses.dropWhile(sym.owner != _))

