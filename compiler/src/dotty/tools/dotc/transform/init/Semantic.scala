package dotty.tools.dotc
package transform
package init

import core._
import Contexts._
import Symbols._
import Types._
import StdNames._
import NameKinds.OuterSelectName

import ast.tpd._
import util.EqHashMap
import config.Printers.init as printer
import reporting.trace as log

import Errors._

import scala.collection.mutable
import scala.annotation.tailrec

object Semantic {

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
  sealed abstract class Value {
    def show: String = this.toString()

    def isHot = this == Hot
    def isCold = this == Cold
    def isWarm = this.isInstanceOf[Warm]
    def isThisRef = this.isInstanceOf[ThisRef]
  }

  /** A transitively initialized object */
  case object Hot extends Value

  /** An object with unknown initialization status */
  case object Cold extends Value

  sealed abstract class Ref extends Value {
    def klass: ClassSymbol
    def outer: Value
  }

  /** A reference to the object under initialization pointed by `this` */
  case class ThisRef(klass: ClassSymbol) extends Ref {
    val outer = Hot
  }

  /** An object with all fields initialized but reaches objects under initialization
   *
   *  We need to restrict nesting levels of `outer` to finitize the domain.
   */
  case class Warm(klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value]) extends Ref {

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
      this.callConstructor(ctor, args.map(arg => ArgInfo(arg, EmptyTree)), tpl)
      populatingParams = false
      this
    }

    def ensureObjectExistsAndPopulated(): Contextual[this.type] =
      if cache.containsObject(this) then this
      else this.ensureFresh().populateParams()
  }

  /** A function value */
  case class Fun(expr: Tree, thisV: Ref, klass: ClassSymbol, env: Env) extends Value

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
  case class Objekt(val klass: ClassSymbol, val fields: Map[Symbol, Value], val outers: Map[ClassSymbol, Value]) {
    def field(f: Symbol): Value = fields(f)

    def outer(klass: ClassSymbol) = outers(klass)

    def hasOuter(klass: ClassSymbol) = outers.contains(klass)

    def hasField(f: Symbol) = fields.contains(f)
  }

  /** The environment for method parameters
   *
   *  For performance and usability, we restrict parameters to be either `Cold`
   *  or `Hot`.
   *
   *  Despite that we have environment for evaluating expressions in secondary
   *  constructors, we don't need to put environment as the cache key. The
   *  reason is that constructor parameters are determined by the value of
   *  `this` --- it suffices to make the value of `this` as part of the cache
   *  key.
   *
   *  This crucially depends on the fact that in the initialization process
   *  there can be exactly one call to a specific constructor for a given
   *  receiver. However, once we relax the design to allow non-hot values to
   *  methods and functions, we have to put the environment as part of the cache
   *  key. The reason is that given the same receiver, a method or function may
   *  be called with different arguments -- they are not decided by the receiver
   *  anymore.
   */
  object Env {
    opaque type Env = Map[Symbol, Value]

    val empty: Env = Map.empty

    def apply(bindings: Map[Symbol, Value]): Env = bindings

    def apply(ddef: DefDef, args: List[Value])(using Context): Env =
      val params = ddef.termParamss.flatten.map(_.symbol)
      assert(args.size == params.size, "arguments = " + args.size + ", params = " + params.size)
      params.zip(args).toMap

    extension (env: Env)
      def lookup(sym: Symbol)(using Context): Value = env(sym)

      def getOrElse(sym: Symbol, default: Value)(using Context): Value = env.getOrElse(sym, default)

      def union(other: Env): Env = env ++ other

      def isHot: Boolean = env.values.forall(_ == Hot)
  }

  type Env = Env.Env
  inline def env(using env: Env) = env
  inline def withEnv[T](env: Env)(op: Env ?=> T): T = op(using env)

  import Env._

  object Promoted {
    class PromotionInfo {
      var isCurrentObjectPromoted: Boolean = false
      val values = mutable.Set.empty[Value]
      override def toString(): String = values.toString()
    }
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
  }
  type Promoted = Promoted.Promoted

  import Promoted._
  inline def promoted(using p: Promoted): Promoted = p

  /** Interpreter configuration
   *
   * The (abstract) interpreter can be seen as a push-down automaton
   * that transits between the configurations where the stack is the
   * implicit call stack of the meta-language.
   *
   * It's important that the configuration is finite for the analysis
   * to terminate.
   *
   * For soundness, we need to compute fixed point of the cache, which
   * maps configuration to evaluation result.
   *
   * Thanks to heap monotonicity, heap is not part of the configuration.
   *
   * This class is only used for the purpose of documentation.
   */
  case class Config(thisV: Value, expr: Tree)

  /** Cache used to terminate the analysis
   *
   * A finitary configuration is not enough for the analysis to
   * terminate.  We need to use cache to let the interpreter "know"
   * that it can terminate.
   *
   * For performance reasons we use curried key.
   *
   * Note: It's tempting to use location of trees as key. That should
   * be avoided as a template may have the same location as its single
   * statement body. Macros may also create incorrect locations.
   *
   */

  object Cache {
    opaque type CacheStore = mutable.Map[Value, EqHashMap[Tree, Value]]
    private type Heap = Map[Ref, Objekt]

    class Cache {
      private var last: CacheStore =  mutable.Map.empty
      private var current: CacheStore = mutable.Map.empty
      private val stable: CacheStore = mutable.Map.empty
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

      def hasChanged = changed

      def contains(value: Value, expr: Tree) =
        current.contains(value, expr) || stable.contains(value, expr)

      def apply(value: Value, expr: Tree) =
        if current.contains(value, expr) then current(value)(expr)
        else stable(value)(expr)

      /** Copy the value of `(value, expr)` from the last cache to the current cache
       * (assuming it's `Hot` if it doesn't exist in the cache).
       *
       * Then, runs `fun` and update the caches if the values change.
       */
      def assume(value: Value, expr: Tree, cacheResult: Boolean)(fun: => Value): Contextual[Value] =
        val assumeValue: Value =
          if last.contains(value, expr) then
            last.get(value, expr)
          else
            last.put(value, expr, Hot)
            Hot
          end if
        current.put(value, expr, assumeValue)

        val actual = fun
        if actual != assumeValue then
          this.changed = true
          last.put(value, expr, actual)
          current.put(value, expr, actual)
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
        current.foreach { (v, m) =>
          // It's useless to cache value for ThisRef.
          if v.isWarm then m.iterator.foreach { (e, res) =>
            stable.put(v, e, res)
          }
        }

      /** Prepare cache for the next iteration
       *
       *  1. Reset changed flag.
       *
       *  2. Reset current cache (last cache already synced in `assume`).
       *
       *  3. Revert heap if instable.
       *
       */
      def prepareForNextIteration()(using Context) =
        this.changed = false
        this.current = mutable.Map.empty
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

        this.last = mutable.Map.empty
        this.current = mutable.Map.empty

      def updateObject(ref: Ref, obj: Objekt) =
        assert(!this.heapStable.contains(ref))
        this.heap = this.heap.updated(ref, obj)

      def containsObject(ref: Ref) = heap.contains(ref)

      def getObject(ref: Ref) = heap(ref)
    }

    extension (cache: CacheStore)
      def contains(value: Value, expr: Tree) = cache.contains(value) && cache(value).contains(expr)
      def get(value: Value, expr: Tree): Value = cache(value)(expr)
      def remove(value: Value, expr: Tree) = cache(value).remove(expr)
      def put(value: Value, expr: Tree, result: Value): Unit = {
        val innerMap = cache.getOrElseUpdate(value, new EqHashMap[Tree, Value])
        innerMap(expr) = result
      }
    end extension
  }

  import Cache._

  inline def cache(using c: Cache): Cache = c

  /** Error reporting */
  trait Reporter:
    def errors: List[Error]
    def report(err: Error): Unit

  object Reporter:
    class BufferReporter extends Reporter:
      val buf = new mutable.ArrayBuffer[Error]
      def errors = buf.toList
      def report(err: Error) = buf += err

    class ErrorFound(val error: Error) extends Exception
    class StopEarlyReporter extends Reporter:
      def report(err: Error) = throw new ErrorFound(err)
      def errors = ???

    def fresh(): Reporter = new BufferReporter

    /** Capture all errors and return as a list */
    def errorsIn(fn: Reporter ?=> Unit): List[Error] =
      val reporter = Reporter.fresh()
      fn(using reporter)
      reporter.errors.toList

    /** Stop on first found error */
    def stopEarly(fn: Reporter ?=> Unit): List[Error] =
      // use promotion reporter to stop the analysis on the first error
      val promotionReporter: Reporter = new StopEarlyReporter

      try
        fn(using promotionReporter)
        Nil
      catch case ex: ErrorFound =>
        ex.error :: Nil


  inline def reporter(using r: Reporter): Reporter = r

// ----- Checker State -----------------------------------

  /** The state that threads through the interpreter */
  type Contextual[T] = (Env, Context, Trace, Promoted, Cache, Reporter) ?=> T

// ----- Error Handling -----------------------------------

  object Trace {
    opaque type Trace = Vector[Tree]

    val empty: Trace = Vector.empty

    extension (trace: Trace)
      def add(node: Tree): Trace = trace :+ node
      def toVector: Vector[Tree] = trace
  }

  type Trace = Trace.Trace

  import Trace._
  def trace(using t: Trace): Trace = t
  inline def withTrace[T](t: Trace)(op: Trace ?=> T): T = op(using t)

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
    def widenArg: Value =
      a match
      case _: Ref | _: Fun => Cold
      case RefSet(refs) => refs.map(_.widenArg).join
      case _ => a


  extension (values: Seq[Value])
    def join: Value =
      if values.isEmpty then Hot
      else values.reduce { (v1, v2) => v1.join(v2) }

    def widenArgs: List[Value] = values.map(_.widenArg).toList


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
      // instance of the exact warm object, which requires initialization check.
      //
      // See tests/init/neg/unsound1.scala
      assert(!obj.hasField(field) || field.is(Flags.ParamAccessor) && obj.field(field) == value, field.show + " already init, new = " + value + ", old = " + obj.field(field) + ", ref = " + ref)
      val obj2 = obj.copy(fields = obj.fields.updated(field, value))
      cache.updateObject(ref, obj2)
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
    def ensureHot(msg: String, source: Tree): Contextual[Value] =
      value.promote(msg, source)
      value

    def select(field: Symbol, source: Tree, needResolve: Boolean = true): Contextual[Value] = log("select " + field.show + ", this = " + value, printer, (_: Value).show) {
      if promoted.isCurrentObjectPromoted then Hot
      else value match {
        case Hot  =>
          Hot

        case Cold =>
          val error = AccessCold(field, source, trace.toVector)
          reporter.report(error)
          Hot

        case ref: Ref =>
          val target = if needResolve then resolve(ref.klass, field) else field
          val trace1 = trace.add(source)
          if target.is(Flags.Lazy) then
            given Trace = trace1
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
                val error = CallUnknown(field, source, trace.toVector)
                reporter.report(error)
                Hot
            else
              val error = AccessNonInit(target, trace.add(source).toVector)
              reporter.report(error)
              Hot

        case fun: Fun =>
          report.error("unexpected tree in selecting a function, fun = " + fun.expr.show, source)
          Hot

        case RefSet(refs) =>
          refs.map(_.select(field, source)).join
      }
    }

    def call(meth: Symbol, args: List[ArgInfo], receiver: Type, superType: Type, source: Tree, needResolve: Boolean = true): Contextual[Value] = log("call " + meth.show + ", args = " + args, printer, (_: Value).show) {
      def checkArgs = args.foreach(_.promote)

      def isSyntheticApply(meth: Symbol) =
        meth.is(Flags.Synthetic)
        && meth.owner.is(Flags.Module)
        && meth.owner.companionClass.is(Flags.Case)

      def isAlwaysSafe(meth: Symbol) =
        (meth eq defn.Object_eq)
        || (meth eq defn.Object_ne)
        || (meth eq defn.Any_isInstanceOf)

      def checkArgsWithParametricity() =
        val methodType = atPhaseBeforeTransforms { meth.info.stripPoly }
        var allArgsPromote = true
        val allParamTypes = methodType.paramInfoss.flatten.map(_.repeatedToSingle)
        val errors = allParamTypes.zip(args).flatMap { (info, arg) =>
          val errors = Reporter.errorsIn { arg.promote }
          allArgsPromote = allArgsPromote && errors.isEmpty
          info match
            case typeParamRef: TypeParamRef =>
              val bounds = typeParamRef.underlying.bounds
              val isWithinBounds = bounds.lo <:< defn.NothingType && defn.AnyType <:< bounds.hi
              def otherParamContains = allParamTypes.exists { param => param != info && param.typeSymbol != defn.ClassTagClass && info.occursIn(param) }
              // A non-hot method argument is allowed if the corresponding parameter type is a
              // type parameter T with Any as its upper bound and Nothing as its lower bound.
              // the other arguments should either correspond to a parameter type that is T
              // or that does not contain T as a component.
              if isWithinBounds && !otherParamContains then Nil else errors
            case _ => errors
        }
        (errors, allArgsPromote)

      // fast track if the current object is already initialized
      if promoted.isCurrentObjectPromoted then Hot
      else if isAlwaysSafe(meth) then Hot
      else if meth eq defn.Any_asInstanceOf then value
      else value match {
        case Hot  =>
          if isSyntheticApply(meth) then
            val klass = meth.owner.companionClass.asClass
            instantiate(klass, klass.primaryConstructor, args, source)
          else
            if receiver.typeSymbol.isStaticOwner then
              val (errors, allArgsPromote) = checkArgsWithParametricity()
              if allArgsPromote then
                Hot
              else if errors.nonEmpty then
                for error <- errors do reporter.report(error)
                Hot
              else
                Cold
            else
              checkArgs
              Hot

        case Cold =>
          checkArgs
          val error = CallCold(meth, source, trace.toVector)
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
            val trace1 = trace.add(source)
            if target.hasSource then
              given Trace = trace1
              val cls = target.owner.enclosingClass.asClass
              val ddef = target.defTree.asInstanceOf[DefDef]
              val argErrors = Reporter.errorsIn { args.foreach(_.promote) }
              // normal method call
              if argErrors.nonEmpty && isSyntheticApply(meth) then
                val klass = meth.owner.companionClass.asClass
                val outerCls = klass.owner.lexicallyEnclosingClass.asClass
                val outer = resolveOuterSelect(outerCls, ref, 1, source)
                outer.instantiate(klass, klass.primaryConstructor, args, source)
              else
                for error <- argErrors do reporter.report(error)
                withEnv(if isLocal then env else Env.empty) {
                  eval(ddef.rhs, ref, cls, cacheResult = true)
                }
            else if ref.canIgnoreMethodCall(target) then
              Hot
            else
              // no source code available
              checkArgs
              val error = CallUnknown(target, source, trace.toVector)
              reporter.report(error)
              Hot
          else
            // method call resolves to a field
            val obj = ref.objekt
            if obj.hasField(target) then
              obj.field(target)
            else
              value.select(target, source, needResolve = false)

        case Fun(body, thisV, klass, env) =>
          // meth == NoSymbol for poly functions
          if meth.name.toString == "tupled" then value // a call like `fun.tupled`
          else
            checkArgs
            withEnv(env) {
              eval(body, thisV, klass, cacheResult = true)
            }

        case RefSet(refs) =>
          refs.map(_.call(meth, args, receiver, superType, source)).join
      }
    }

    def callConstructor(ctor: Symbol, args: List[ArgInfo], source: Tree): Contextual[Value] = log("call " + ctor.show + ", args = " + args, printer, (_: Value).show) {
      // init "fake" param fields for the secondary constructor
      def addParamsAsFields(env: Env, ref: Ref, ctorDef: DefDef) = {
        val paramSyms = ctorDef.termParamss.flatten.map(_.symbol)
        paramSyms.map { acc =>
          val value = env.lookup(acc)
          ref.updateField(acc, value)
          printer.println(acc.show + " initialized with " + value)
        }
      }
      value match {
        case Hot | Cold | _: RefSet | _: Fun =>
          report.error("unexpected constructor call, meth = " + ctor + ", value = " + value, source)
          Hot

        case ref: Warm if ref.isPopulatingParams =>
          val trace1 = trace.add(source)
          if ctor.hasSource then
            given Trace = trace1
            val cls = ctor.owner.enclosingClass.asClass
            val ddef = ctor.defTree.asInstanceOf[DefDef]
            given Env = Env(ddef, args.map(_.value).widenArgs)
            if ctor.isPrimaryConstructor then
              val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
              init(tpl, ref, cls)
            else
              addParamsAsFields(env, ref, ddef)
              val initCall = ddef.rhs match
                case Block(call :: _, _) => call
                case call => call
              eval(initCall, ref, cls)
            end if
          else
            Hot

        case ref: Ref =>
          val trace1 = trace.add(source)
          if ctor.hasSource then
            given Trace = trace1
            val cls = ctor.owner.enclosingClass.asClass
            val ddef = ctor.defTree.asInstanceOf[DefDef]
            given Env = Env(ddef, args.map(_.value).widenArgs)
            if ctor.isPrimaryConstructor then
              val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
              withTrace(trace.add(cls.defTree)) { eval(tpl, ref, cls, cacheResult = true) }
              ref
            else
              addParamsAsFields(env, ref, ddef)
              eval(ddef.rhs, ref, cls, cacheResult = true)
          else if ref.canIgnoreMethodCall(ctor) then
            Hot
          else
            // no source code available
            val error = CallUnknown(ctor, source, trace.toVector)
            reporter.report(error)
            Hot
      }

    }

    /** Handle a new expression `new p.C` where `p` is abstracted by `value` */
    def instantiate(klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo], source: Tree): Contextual[Value] = log("instantiating " + klass.show + ", value = " + value + ", args = " + args, printer, (_: Value).show) {
      val trace1 = trace.add(source)
      if promoted.isCurrentObjectPromoted then Hot
      else value match {
        case Hot  =>
          val buffer = new mutable.ArrayBuffer[Error]
          val args2 = args.map { arg =>
            given reporter: Reporter = Reporter.fresh()
            arg.promote
            buffer ++= reporter.errors
            if reporter.errors.isEmpty then Hot
            else arg.value.widenArg
          }

          if buffer.isEmpty then
            Hot
          else
            val outer = Hot
            val warm = Warm(klass, outer, ctor, args2).ensureObjectExists()
            val argInfos2 = args.zip(args2).map { (argInfo, v) => argInfo.copy(value = v) }
            warm.callConstructor(ctor, argInfos2, source)
            warm

        case Cold =>
          val error = CallCold(ctor, source, trace1.toVector)
          reporter.report(error)
          Hot

        case ref: Ref =>
          given Trace = trace1
          // widen the outer to finitize the domain
          val outer = ref match
            case warm @ Warm(_, _: Warm, _, _) =>
              // the widened warm object might not exist in the heap
              warm.copy(outer = Cold).ensureObjectExistsAndPopulated()
            case _ => ref

          val argsWidened = args.map(_.value).widenArgs
          val argInfos2 = args.zip(argsWidened).map { (argInfo, v) => argInfo.copy(value = v) }
          val warm = Warm(klass, outer, ctor, argsWidened).ensureObjectExists()
          warm.callConstructor(ctor, argInfos2, source)
          warm

        case Fun(body, thisV, klass, env) =>
          report.error("unexpected tree in instantiating a function, fun = " + body.show, source)
          Hot

        case RefSet(refs) =>
          refs.map(_.instantiate(klass, ctor, args, source)).join
      }
    }
  end extension

  extension (ref: Ref)
    def accessLocal(tmref: TermRef, klass: ClassSymbol, source: Tree): Contextual[Value] =
      val sym = tmref.symbol

      if sym.is(Flags.Param) && sym.owner.isConstructor then
        // if we can get the field from the Ref (which can only possibly be
        // a secondary constructor parameter), then use it.
        if (ref.objekt.hasField(sym))
          ref.objekt.field(sym)
        // instances of local classes inside secondary constructors cannot
        // reach here, as those values are abstracted by Cold instead of Warm.
        // This enables us to simplify the domain without sacrificing
        // expressiveness nor soundess, as local classes inside secondary
        // constructors are uncommon.
        else if sym.isContainedIn(klass) then
          env.lookup(sym)
        else
          // We don't know much about secondary constructor parameters in outer scope.
          // It's always safe to approximate them with `Cold`.
          Cold
      else if sym.is(Flags.Param) then
        Hot
      else
        sym.defTree match {
          case vdef: ValDef =>
            // resolve this for local variable
            val enclosingClass = sym.owner.enclosingClass.asClass
            val thisValue2 = resolveThis(enclosingClass, ref, klass, source)
            thisValue2 match
              case Hot => Hot

              case Cold => Cold

              case ref: Ref => eval(vdef.rhs, ref, enclosingClass)

              case _ =>
                 report.error("unexpected defTree when accessing local variable, sym = " + sym.show + ", defTree = " + sym.defTree.show, source)
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
    def promote(msg: String, source: Tree): Contextual[Unit] = log("promoting " + value + ", promoted = " + promoted, printer) {
      val trace2 = trace.add(source)
      if promoted.isCurrentObjectPromoted then Nil else
        given Trace = trace2

        value.match
        case Hot   =>

        case Cold  =>
          reporter.report(PromoteError(msg, source, trace.toVector))

        case thisRef: ThisRef =>
          if !thisRef.tryPromoteCurrentObject() then
            reporter.report(PromoteError(msg, source, trace.toVector))

        case warm: Warm =>
          if !promoted.contains(warm) then
            promoted.add(warm)
            val errors = warm.tryPromote(msg, source)
            if errors.nonEmpty then promoted.remove(warm)
            for error <- errors do reporter.report(error)

        case fun @ Fun(body, thisV, klass, env) =>
          if !promoted.contains(fun) then
            val errors = Reporter.stopEarly {
              given Trace = Trace.empty.add(body)
              val res = withEnv(env) { eval(body, thisV, klass) }
              res.promote("The function return value is not fully initialized.", body)
            }
            if (errors.nonEmpty)
              reporter.report(UnsafePromotion(msg, source, trace.toVector, errors.head))
            else
              promoted.add(fun)

        case RefSet(refs) =>
          refs.foreach(_.promote(msg, source))
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
    def tryPromote(msg: String, source: Tree): Contextual[List[Error]] = log("promote " + warm.show + ", promoted = " + promoted, printer) {
      val classRef = warm.klass.appliedRef
      if classRef.memberClasses.nonEmpty || !warm.isFullyFilled then
        return PromoteError(msg, source, trace.toVector) :: Nil

      val errors = Reporter.stopEarly {
        for klass <- warm.klass.baseClasses if klass.hasSource do
          for member <- klass.info.decls do
            if !member.isType && !member.isConstructor && member.hasSource  && !member.is(Flags.Deferred) then
              given Trace = Trace.empty.add(source)
              if member.is(Flags.Method, butNot = Flags.Accessor) then
                locally {
                  val args = member.info.paramInfoss.flatten.map(_ => ArgInfo(Hot, EmptyTree))
                  val res = warm.call(member, args, receiver = NoType, superType = NoType, source = member.defTree)
                  res.promote("Cannot prove that the return value of " + member + " is fully initialized.", source)
                }
              else
                val res = warm.select(member, source)
                res.promote("Cannot prove that the field " + member + " is fully initialized.", source)
          end for
        end for
      }

      if errors.isEmpty then Nil
      else UnsafePromotion(msg, source, trace.toVector, errors.head) :: Nil
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

  class WorkList private[Semantic]() {
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
    private def doTask(task: Task)(using Cache, Context): Unit = {
      val thisRef = task.value
      val tpl = thisRef.klass.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]

      val paramValues = tpl.constr.termParamss.flatten.map(param => param.symbol -> Hot).toMap

      @tailrec
      def iterate(): Unit = {
        given Promoted = Promoted.empty
        given Trace = Trace.empty
        given Env = Env(paramValues)
        given Reporter = Reporter.fresh()

        thisRef.ensureFresh()
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
    }
  }
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
  def withInitialState[T](work: (Cache, WorkList) ?=> T): T = {
    work(using new Cache, new WorkList)
  }

// ----- Semantic definition --------------------------------

  /** Utility definition used for better error-reporting of argument errors */
  case class ArgInfo(value: Value, source: Tree) {
    def promote: Contextual[Unit] = value.promote("Cannot prove the argument is fully initialized.", source)
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
   */
  def eval(expr: Tree, thisV: Ref, klass: ClassSymbol, cacheResult: Boolean = false): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + " in " + klass.show, printer, (_: Value).show) {
    if (cache.contains(thisV, expr)) cache(thisV, expr)
    else cache.assume(thisV, expr, cacheResult) { cases(expr, thisV, klass) }
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
          Fun(arg.tree, thisV, klass, env)
        else
          eval(arg.tree, thisV, klass)

      argInfos += ArgInfo(res, arg.tree)
    }
    argInfos.toList

  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   */
  def cases(expr: Tree, thisV: Ref, klass: ClassSymbol): Contextual[Value] =
    expr match {
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Hot

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        cases(expr.tpe, thisV, klass, expr)

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        val outer = outerValue(tref, thisV, klass, tpt)
        val trace2 = trace.add(expr)
        locally {
          given Trace = trace2
          outer.instantiate(cls, ctor, args, source = expr)
        }

      case Call(ref, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe: @unchecked
          val thisValue2 = resolveThis(thisTp.classSymbol.asClass, thisV, klass, ref)
          thisValue2.call(ref.symbol, args, thisTp, superTp, expr)

        case Select(qual, _) =>
          val receiver = eval(qual, thisV, klass)
          if ref.symbol.isConstructor then
            receiver.callConstructor(ref.symbol, args, source = expr)
          else
            receiver.call(ref.symbol, args, receiver = qual.tpe, superType = NoType, source = expr)

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = resolveThis(enclosingClass, thisV, klass, id)
            // local methods are not a member, but we can reuse the method `call`
            thisValue2.call(id.symbol, args, receiver = NoType, superType = NoType, expr, needResolve = false)
          case TermRef(prefix, _) =>
            val receiver = cases(prefix, thisV, klass, id)
            if id.symbol.isConstructor then
              receiver.callConstructor(id.symbol, args, source = expr)
            else
              receiver.call(id.symbol, args, receiver = prefix, superType = NoType, source = expr)

      case Select(qualifier, name) =>
        val qual = eval(qualifier, thisV, klass)

        name match
          case OuterSelectName(_, hops) =>
            val SkolemType(tp) = expr.tpe: @unchecked
            resolveOuterSelect(tp.classSymbol.asClass, qual, hops, source = expr)
          case _ =>
            qual.select(expr.symbol, expr)

      case _: This =>
        cases(expr.tpe, thisV, klass, expr)

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
          eval(rhs, thisV, klass).ensureHot("May only assign fully initialized value.", rhs)
        case id: Ident =>
          eval(rhs, thisV, klass).ensureHot("May only assign fully initialized value.", rhs)

      case closureDef(ddef) =>
        Fun(ddef.rhs, thisV, klass, env)

      case PolyFun(body) =>
        Fun(body, thisV, klass, env)

      case Block(stats, expr) =>
        eval(stats, thisV, klass)
        eval(expr, thisV, klass)

      case If(cond, thenp, elsep) =>
        eval(cond :: thenp :: elsep :: Nil, thisV, klass).join

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Hot
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        eval(selector, thisV, klass).ensureHot("The value to be matched needs to be fully initialized.", selector)
        eval(cases.map(_.body), thisV, klass).join

      case Return(expr, from) =>
        eval(expr, thisV, klass).ensureHot("return expression may only be initialized value.", expr)

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
        val trace1 = trace.add(expr)
        eval(bindings, thisV, klass)
        withTrace(trace1)(eval(expansion, thisV, klass))

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
        throw new Exception("unexpected tree: " + expr.show)
    }

  /** Handle semantics of leaf nodes */
  def cases(tp: Type, thisV: Ref, klass: ClassSymbol, source: Tree): Contextual[Value] = log("evaluating " + tp.show, printer, (_: Value).show) {
    tp match {
      case _: ConstantType =>
        Hot

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        thisV.accessLocal(tmref, klass, source)

      case tmref: TermRef =>
        cases(tmref.prefix, thisV, klass, source).select(tmref.symbol, source)

      case tp @ ThisType(tref) =>
        val cls = tref.classSymbol.asClass
        if cls.isStaticOwner && !klass.isContainedIn(cls) then
          // O.this outside the body of the object O
          Hot
        else
          val value = resolveThis(cls, thisV, klass, source)
          value

      case _: TermParamRef | _: RecThis  =>
        // possible from checking effects of types
        Hot

      case _ =>
        throw new Exception("unexpected type: " + tp)
    }
  }

  /** Resolve C.this that appear in `klass` */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol, source: Tree): Contextual[Value] = log("resolving " + target.show + ", this = " + thisV.show + " in " + klass.show, printer, (_: Value).show) {
    if target == klass then thisV
    else if target.is(Flags.Package) then Hot
    else
      thisV match
        case Hot => Hot
        case ref: Ref =>
          val obj = ref.objekt
          val outerCls = klass.owner.lexicallyEnclosingClass.asClass
          if !obj.hasOuter(klass) then
            val error = PromoteError("outer not yet initialized, target = " + target + ", klass = " + klass + ", object = " + obj, source, trace.toVector)
            report.error(error.show + error.stacktrace, source)
            Hot
          else
            resolveThis(target, obj.outer(klass), outerCls, source)
        case RefSet(refs) =>
          refs.map(ref => resolveThis(target, ref, klass, source)).join
        case fun: Fun =>
          report.warning("unexpected thisV = " + thisV + ", target = " + target.show + ", klass = " + klass.show, source.srcPos)
          Cold
        case Cold => Cold

  }

  /** Resolve outer select introduced during inlining.
   *
   *  See `tpd.outerSelect` and `ElimOuterSelect`.
   */
  def resolveOuterSelect(target: ClassSymbol, thisV: Value, hops: Int, source: Tree): Contextual[Value] = log("resolving outer " + target.show + ", this = " + thisV.show + ", hops = " + hops, printer, (_: Value).show) {
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
            resolveThis(target, thisV, cur, source)

          case None =>
            report.warning("unexpected outerSelect, thisV = " + thisV + ", target = " + target.show + ", hops = " + hops, source.srcPos)
            Cold

      case RefSet(refs) =>
        refs.map(ref => resolveOuterSelect(target, ref, hops, source)).join

      case fun: Fun =>
        report.warning("unexpected thisV = " + thisV + ", target = " + target.show + ", hops = " + hops, source.srcPos)
        Cold

      case Cold => Cold
  }

  /** Compute the outer value that correspond to `tref.prefix` */
  def outerValue(tref: TypeRef, thisV: Ref, klass: ClassSymbol, source: Tree): Contextual[Value] =
    val cls = tref.classSymbol.asClass
    if tref.prefix == NoPrefix then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      val outerV = resolveThis(enclosing, thisV, klass, source)
      outerV
    else
      if cls.isAllOf(Flags.JavaInterface) then Hot
      else cases(tref.prefix, thisV, klass, source)

  /** Initialize part of an abstract object in `klass` of the inheritance chain */
  def init(tpl: Template, thisV: Ref, klass: ClassSymbol): Contextual[Value] = log("init " + klass.show, printer, (_: Value).show) {
    val paramsMap = tpl.constr.termParamss.flatten.map { vdef =>
      vdef.name -> env.lookup(vdef.symbol)
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
    def superCall(tref: TypeRef, ctor: Symbol, args: List[ArgInfo], source: Tree, tasks: Tasks)(using Env): Unit =
      val cls = tref.classSymbol.asClass
      // update outer for super class
      val res = outerValue(tref, thisV, klass, source)
      thisV.updateOuter(cls, res)

      // follow constructor
      if cls.hasSource then
        tasks.append { () =>
          printer.println("init super class " + cls.show)
          thisV.callConstructor(ctor, args, source)
          ()
        }

    // parents
    def initParent(parent: Tree, tasks: Tasks)(using Env) = parent match {
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        eval(stats, thisV, klass)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args, tree, tasks)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>       // extends A(args)
      val args = evalArgs(argss.flatten, thisV, klass)
      superCall(tref, ctor, args, tree, tasks)

      case _ =>   // extends A or extends A[T]
        val tref = typeRefOf(parent.tpe)
        superCall(tref, tref.classSymbol.primaryConstructor, Nil, parent, tasks)
    }

    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html
    if !klass.is(Flags.Trait) then
      given Env = Env.empty

      // outers are set first
      val tasks = new mutable.ArrayBuffer[() => Unit]

      // 1. first init parent class recursively
      // 2. initialize traits according to linearization order
      val superParent = tpl.parents.head
      val superCls = superParent.tpe.classSymbol.asClass
      initParent(superParent, tasks)

      val parents = tpl.parents.tail
      val mixins = klass.baseClasses.tail.takeWhile(_ != superCls)

      // The interesting case is the outers for traits.  The compiler
      // synthesizes proxy accessors for the outers in the class that extends
      // the trait. As those outers must be stable values, they are initialized
      // immediately following class parameters and before super constructor
      // calls and user code in the class body.
      mixins.reverse.foreach { mixin =>
        parents.find(_.tpe.classSymbol == mixin) match
        case Some(parent) => initParent(parent, tasks)
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
          if ctor.exists then superCall(tref, ctor, Nil, superParent, tasks)
      }

      // initialize super classes after outers are set
      tasks.foreach(task => task())
    end if

    var fieldsChanged = true

    // class body
    if thisV.isThisRef || !thisV.asInstanceOf[Warm].isPopulatingParams then tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) && !vdef.rhs.isEmpty =>
        given Env = Env.empty
        val res = eval(vdef.rhs, thisV, klass)
        thisV.updateField(vdef.symbol, res)
        fieldsChanged = true

      case _: MemberDef =>

      case tree =>
        if fieldsChanged && thisV.isThisRef then thisV.asInstanceOf[ThisRef].tryPromoteCurrentObject()
        fieldsChanged = false

        given Env = Env.empty
        eval(tree, thisV, klass)
    }

    // The result value is ignored, use Hot to avoid futile fixed point computation
    Hot
  }

  /** Check that path in path-dependent types are initialized
   *
   *  This is intended to avoid type soundness issues in Dotty.
   */
  def checkTermUsage(tpt: Tree, thisV: Ref, klass: ClassSymbol): Contextual[Unit] =
    val traverser = new TypeTraverser {
      def traverse(tp: Type): Unit = tp match {
        case TermRef(_: SingletonType, _) =>
          cases(tp, thisV, klass, tpt)
        case _ =>
          traverseChildren(tp)
      }
    }
    traverser.traverse(tpt.tpe)

// ----- Utility methods and extractors --------------------------------

  def typeRefOf(tp: Type)(using Context): TypeRef = tp.dealias.typeConstructor match {
    case tref: TypeRef => tref
    case hklambda: HKTypeLambda => typeRefOf(hklambda.resType)
  }

  opaque type Arg  = Tree | ByNameArg
  case class ByNameArg(tree: Tree)

  extension (arg: Arg)
    def isByName = arg.isInstanceOf[ByNameArg]
    def tree: Tree = arg match
      case t: Tree      => t
      case ByNameArg(t) => t

  object Call {

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
  }

  object NewExpr {
    def unapply(tree: Tree)(using Context): Option[(TypeRef, New, Symbol, List[List[Arg]])] =
      tree match
      case Call(fn @ Select(newTree: New, init), argss) if init == nme.CONSTRUCTOR =>
        val tref = typeRefOf(newTree.tpe)
        Some((tref, newTree, fn.symbol, argss))
      case _ => None
  }

  object PolyFun {
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
  }

  extension (symbol: Symbol) def hasSource(using Context): Boolean =
    !symbol.defTree.isEmpty

  def resolve(cls: ClassSymbol, sym: Symbol)(using Context): Symbol = log("resove " + cls + ", " + sym, printer, (_: Symbol).show) {
    if (sym.isEffectivelyFinal || sym.isConstructor) sym
    else sym.matchingMember(cls.appliedRef)
  }

  def resolveSuper(cls: ClassSymbol, superType: Type, sym: Symbol)(using Context): Symbol = {
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
  }

}
