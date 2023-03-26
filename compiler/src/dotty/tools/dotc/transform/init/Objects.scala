package dotty.tools.dotc
package transform
package init

import core.*
import Contexts.*
import Symbols.*
import Types.*
import StdNames.*
import NameKinds.OuterSelectName
import NameKinds.SuperAccessorName

import ast.tpd.*
import util.SourcePosition
import config.Printers.init as printer
import reporting.StoreReporter
import reporting.trace as log

import Errors.*
import Trace.*
import Util.*

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.annotation.tailrec
import scala.annotation.constructorOnly

/** Check initialization safety of static objects
 *
 *  The problem is illustrated by the example below:
 *
 *      class Foo(val opposite: Foo)
 *      case object A extends Foo(B)     // A -> B
 *      case object B extends Foo(A)     // B -> A
 *
 *  In the code above, the initialization of object `A` depends on `B` and vice versa. There is no
 *  correct way to initialize the code above. The current checker issues a warning for the code
 *  above.
 *
 *  At the high-level, the analysis has the following characteristics:
 *
 *  1. The check enforces the principle of "initialization-time irrelevance", which means that the
 *     time when an object is initialized should not change program semantics. For that purpose, it
 *     enforces the following rule:
 *
 *         The initialization of a static object should not directly or indirectly read or write
 *         mutable state of another static object.
 *
 *     This principle not only put initialization of static objects on a solid foundation, but also
 *     avoids whole-program analysis.
 *
 *  2. The design is based on the concept of "cold aliasing" --- a cold alias may not be actively
 *     used during initialization, i.e., it's forbidden to call methods or access fields of a cold
 *     alias. Method arguments are cold aliases by default unless specified to be sensitive. Method
 *     parameters captured in lambdas or inner classes are always cold aliases.
 *
 *  3. It is inter-procedural and flow-sensitive.
 *
 *  4. It is object-sensitive by default and parameter-sensitive on-demand.
 *
 *  5. The check is modular in the sense that each object is checked separately and there is no
 *     whole-program analysis. However, the check is not modular in terms of project boundaries.
 *
 */
object Objects:

  // ----------------------------- abstract domain -----------------------------

  sealed abstract class Value:
    def show(using Context): String


  /**
   * A reference caches the values for outers and immutable fields.
   */
  sealed abstract class Ref(
    valsMap: mutable.Map[Symbol, Value],
    varsMap: mutable.Map[Symbol, Heap.Addr],
    outersMap: mutable.Map[ClassSymbol, Value])
  extends Value:
    protected val vals: mutable.Map[Symbol, Value] = valsMap
    protected val vars: mutable.Map[Symbol, Heap.Addr] = varsMap
    protected val outers: mutable.Map[ClassSymbol, Value] = outersMap

    def isObjectRef: Boolean = this.isInstanceOf[ObjectRef]

    def klass: ClassSymbol

    def valValue(sym: Symbol): Value = vals(sym)

    def varAddr(sym: Symbol): Heap.Addr = vars(sym)

    def outerValue(cls: ClassSymbol): Value = outers(cls)

    def hasVal(sym: Symbol): Boolean = vals.contains(sym)

    def hasVar(sym: Symbol): Boolean = vars.contains(sym)

    def hasOuter(cls: ClassSymbol): Boolean = outers.contains(cls)

    def initVal(field: Symbol, value: Value)(using Context) = log("Initialize " + field.show + " = " + value + " for " + this, printer) {
      assert(!field.is(Flags.Mutable), "Field is mutable: " + field.show)
      assert(!vals.contains(field), "Field already set " + field.show)
      vals(field) = value
    }

    def initVar(field: Symbol, addr: Heap.Addr)(using Context) = log("Initialize " + field.show + " = " + addr + " for " + this, printer) {
      assert(field.is(Flags.Mutable), "Field is not mutable: " + field.show)
      assert(!vars.contains(field), "Field already set: " + field.show)
      vars(field) = addr
    }

    def initOuter(cls: ClassSymbol, value: Value)(using Context) = log("Initialize outer " + cls.show + " = " + value + " for " + this, printer) {
      assert(!outers.contains(cls), "Outer already set " + cls)
      outers(cls) = value
    }

  /** A reference to a static object */
  case class ObjectRef(klass: ClassSymbol)
  extends Ref(valsMap = mutable.Map.empty, varsMap = mutable.Map.empty, outersMap = mutable.Map.empty):
    val owner = klass

    def show(using Context) = "ObjectRef(" + klass.show + ")"

  /**
   * Rerepsents values that are instances of the specified class.
   *
   * Note that the 2nd parameter block does not take part in the definition of equality.
   */
  case class OfClass private (
    klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value], env: Env.Data)(
    valsMap: mutable.Map[Symbol, Value], varsMap: mutable.Map[Symbol, Heap.Addr], outersMap: mutable.Map[ClassSymbol, Value])
  extends Ref(valsMap, varsMap, outersMap):
    def widenedCopy(outer: Value, args: List[Value], env: Env.Data): OfClass =
      new OfClass(klass, outer, ctor, args, env)(this.valsMap, this.varsMap, outersMap)

    def show(using Context) =
      val valFields = vals.map(_.show +  " -> " +  _.show)
      "OfClass(" + klass.show + ", outer = " + outer + ", args = " + args.map(_.show) + ", vals = " + valFields + ")"

  object OfClass:
    def apply(
      klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value], env: Env.Data)(
      using Context
    ): OfClass =
      val instance = new OfClass(klass, outer, ctor, args, env)(
        valsMap = mutable.Map.empty, varsMap = mutable.Map.empty, outersMap = mutable.Map.empty
      )
      instance.initOuter(klass, outer)
      instance

  /**
   * Rerepsents arrays.
   *
   * Note that the 2nd parameter block does not take part in the definition of equality.
   *
   * Different arrays are distinguished by the context. Currently the default context is the static
   * object whose initialization triggers the creation of the array.
   *
   * In the future, it is possible that we introduce a mechanism for end-users to mark the context.
   *
   * @param owner The static object whose initialization creates the array.
   */
  case class OfArray(owner: ClassSymbol, regions: Regions.Data)(using @constructorOnly ctx: Context)
  extends Ref(valsMap = mutable.Map.empty, varsMap = mutable.Map.empty, outersMap = mutable.Map.empty):
    val klass: ClassSymbol = defn.ArrayClass
    val addr: Heap.Addr = Heap.arrayAddr(regions, owner)
    def show(using Context) = "OfArray(owner = " + owner.show + ")"

  /**
   * Represents a lambda expression
   */
  case class Fun(code: Tree, thisV: Value, klass: ClassSymbol, env: Env.Data) extends Value:
    def show(using Context) = "Fun(" + code.show + ", " + thisV.show + ", " + klass.show + ")"

  /**
   * Represents a set of values
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: ListSet[Value]) extends Value:
    assert(refs.forall(!_.isInstanceOf[RefSet]))
    def show(using Context) = refs.map(_.show).mkString("[", ",", "]")

  /** A cold alias which should not be used during initialization. */
  case object Cold extends Value:
    def show(using Context) = "Cold"

  val Bottom = RefSet(ListSet.empty)

  /** Checking state  */
  object State:
    class Data:
      // objects under check
      private[State] val checkingObjects = new mutable.ArrayBuffer[ObjectRef]
      private[State] val checkedObjects = new mutable.ArrayBuffer[ObjectRef]
      private[State] val pendingTraces = new mutable.ArrayBuffer[Trace]
    end Data

    def currentObject(using data: Data): ClassSymbol = data.checkingObjects.last.klass

    private def doCheckObject(classSym: ClassSymbol)(using ctx: Context, data: Data) =
      val tpl = classSym.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]

      var count = 0
      given Cache.Data = new Cache.Data

      @tailrec
      def iterate()(using Context): ObjectRef =
        count += 1

        given Trace = Trace.empty.add(classSym.defTree)
        given Env.Data = Env.emptyEnv(tpl.constr.symbol)
        given Heap.MutableData = Heap.empty()
        given regions: Regions.Data = Regions.empty // explicit name to avoid naming conflict

        val obj = ObjectRef(classSym)
        log("Iteration " + count) {
          data.checkingObjects += obj
          init(tpl, obj, classSym)
          assert(data.checkingObjects.last.klass == classSym, "Expect = " + classSym.show + ", found = " + data.checkingObjects.last.klass)
          data.checkingObjects.remove(data.checkingObjects.size - 1)
        }

        val hasError = ctx.reporter.pendingMessages.nonEmpty
        if cache.hasChanged && !hasError then
          cache.prepareForNextIteration()
          iterate()
        else
          data.checkedObjects += obj
          obj
      end iterate

      val reporter = new StoreReporter(ctx.reporter)
      val obj = iterate()(using ctx.fresh.setReporter(reporter))
      for warning <- reporter.pendingMessages do
        ctx.reporter.report(warning)

      obj
    end doCheckObject

    def checkObjectAccess(clazz: ClassSymbol)(using data: Data, ctx: Context, pendingTrace: Trace): Value =
      val index = data.checkingObjects.indexOf(ObjectRef(clazz))

      if index != -1 then
        if data.checkingObjects.size - 1 > index then
          // Only report errors for non-trivial cycles, ignore self cycles.
          val joinedTrace = data.pendingTraces.slice(index + 1, data.checkingObjects.size).foldLeft(pendingTrace) { (a, acc) => acc ++ a }
          val callTrace = Trace.buildStacktrace(joinedTrace, "Calling trace:\n")
          val cycle = data.checkingObjects.slice(index, data.checkingObjects.size)
          val pos = clazz.defTree
          report.warning("Cyclic initialization: " + cycle.map(_.klass.show).mkString(" -> ") + " -> " + clazz.show + ". " + callTrace, pos)
        end if
        data.checkingObjects(index)
      else
        val objOpt = data.checkedObjects.find(_.klass == clazz)
        objOpt match
        case Some(obj) => obj

        case None =>
          data.pendingTraces += pendingTrace
          val obj = doCheckObject(clazz)
          data.pendingTraces.remove(data.pendingTraces.size - 1)
          obj
    end checkObjectAccess
  end State

  /** Environment for parameters */
  object Env:
    abstract class Data:
      private[Env] def getVal(x: Symbol)(using Context): Option[Value]
      private[Env] def getVar(x: Symbol)(using Context): Option[Heap.Addr]

      def widen(height: Int)(using Context): Data

      def level: Int

      def show(using Context): String

    /** Local environments can be deeply nested, therefore we need `outer`.
     *
     *  For local variables in rhs of class field definitions, the `meth` is the primary constructor.
     */
    private case class LocalEnv
      (private[Env] val params: Map[Symbol, Value], meth: Symbol, outer: Data)
      (valsMap: mutable.Map[Symbol, Value], varsMap: mutable.Map[Symbol, Heap.Addr])
      (using Context)
    extends Data:
      val level = outer.level + 1

      if (level > 3)
        report.warning("[Internal error] Deeply nested environemnt, level =  " + level + ", " + meth.show + " in " + meth.enclosingClass.show, meth.defTree)

      private[Env] val vals: mutable.Map[Symbol, Value] = valsMap
      private[Env] val vars: mutable.Map[Symbol, Heap.Addr] = varsMap

      private[Env] def getVal(x: Symbol)(using Context): Option[Value] =
        if x.is(Flags.Param) then params.get(x)
        else vals.get(x)

      private[Env] def getVar(x: Symbol)(using Context): Option[Heap.Addr] =
        vars.get(x)

      def widen(height: Int)(using Context): Data =
        new LocalEnv(params.map(_ -> _.widen(height)), meth, outer.widen(height))(this.vals, this.vars)

      def show(using Context) =
        "owner: " + meth.show + "\n" +
        "params: " + params.map(_.show + " ->" + _.show).mkString("{", ", ", "}") + "\n" +
        "vals: " + vals.map(_.show + " ->" + _.show).mkString("{", ", ", "}") + "\n" +
        "vars: " + vars.map(_.show + " ->" + _).mkString("{", ", ", "}") + "\n" +
        "outer = {\n" + outer.show + "\n}"

    end LocalEnv

    object NoEnv extends Data:
      val level = 0

      private[Env] def getVal(x: Symbol)(using Context): Option[Value] =
        throw new RuntimeException("Invalid usage of non-existent env")

      private[Env] def getVar(x: Symbol)(using Context): Option[Heap.Addr] =
        throw new RuntimeException("Invalid usage of non-existent env")

      def widen(height: Int)(using Context): Data = this

      def show(using Context): String = "NoEnv"
    end NoEnv

    /** An empty environment can be used for non-method environments, e.g., field initializers.
     *
     *  The owner for the local environment for field initializers is the primary constructor of the
     *  enclosing class.
     */
    def emptyEnv(meth: Symbol)(using Context): Data =
      new LocalEnv(Map.empty, meth, NoEnv)(valsMap = mutable.Map.empty, varsMap = mutable.Map.empty)

    def valValue(x: Symbol)(using data: Data, ctx: Context): Value = data.getVal(x).get

    def varAddr(x: Symbol)(using data: Data, ctx: Context): Heap.Addr = data.getVar(x).get

    def getVal(x: Symbol)(using data: Data, ctx: Context): Option[Value] = data.getVal(x)

    def getVar(x: Symbol)(using data: Data, ctx: Context): Option[Heap.Addr] = data.getVar(x)

    def of(ddef: DefDef, args: List[Value], outer: Data)(using Context): Data =
      val params = ddef.termParamss.flatten.map(_.symbol)
      assert(args.size == params.size, "arguments = " + args.size + ", params = " + params.size)
      assert(ddef.symbol.owner.isClass ^ (outer != NoEnv), "ddef.owner = " + ddef.symbol.owner.show + ", outer = " + outer + ", " + ddef.source)
      new LocalEnv(params.zip(args).toMap, ddef.symbol, outer)(valsMap = mutable.Map.empty, varsMap = mutable.Map.empty)

    def setLocalVal(x: Symbol, value: Value)(using data: Data, ctx: Context): Unit =
      assert(!x.isOneOf(Flags.Param | Flags.Mutable), "Only local immutable variable allowed")
      data match
      case localEnv: LocalEnv =>
        assert(!localEnv.vals.contains(x), "Already initialized local " + x.show)
        localEnv.vals(x) = value
      case _ =>
        throw new RuntimeException("Incorrect local environment for initializing " + x.show)

    def setLocalVar(x: Symbol, addr: Heap.Addr)(using data: Data, ctx: Context): Unit =
      assert(x.is(Flags.Mutable, butNot = Flags.Param), "Only local mutable variable allowed")
      data match
      case localEnv: LocalEnv =>
        assert(!localEnv.vars.contains(x), "Already initialized local " + x.show)
        localEnv.vars(x) = addr
      case _ =>
        throw new RuntimeException("Incorrect local environment for initializing " + x.show)

    /**
     * Resolve the environment owned by the given method.
     *
     * The method could be located in outer scope with intermixed classes between its definition
     * site and usage site.
     *
     * Due to widening, the corresponding environment might not exist. As a result reading the local
     * variable will return `Cold` and it's forbidden to write to the local variable.
     *
     * @param meth  The method which owns the environment
     * @param thisV The value for `this` of the enclosing class where the local variable is referenced.
     * @param env   The local environment where the local variable is referenced.
     */
    def resolveEnv(meth: Symbol, thisV: Value, env: Data)(using Context): Option[(Value, Data)] = log("Resolving env for " + meth.show + ", this = " + thisV.show + ", env = " + env.show, printer) {
      env match
      case localEnv: LocalEnv =>
        if localEnv.meth == meth then Some(thisV -> env)
        else resolveEnv(meth, thisV, localEnv.outer)
      case NoEnv =>
        // TODO: handle RefSet
        thisV match
        case ref: OfClass =>
          resolveEnv(meth, ref.outer, ref.env)
        case _ =>
          None
    }

    def withEnv[T](env: Data)(fn: Data ?=> T): T = fn(using env)
  end Env

  /** Abstract heap for mutable fields
   */
  object Heap:
    abstract class Addr:
      /** The static object which owns the mutable slot */
      def owner: ClassSymbol

    /** The address for mutable fields of objects. */
    private case class FieldAddr(regions: Regions.Data, field: Symbol, owner: ClassSymbol) extends Addr

    /** The address for mutable local variables . */
    private case class LocalVarAddr(regions: Regions.Data, sym: Symbol, owner: ClassSymbol) extends Addr

    /** Immutable heap data used in the cache.
     *
     *  We need to use structural equivalence so that in different iterations the cache can be effective.
     *
     *  TODO: speed up equality check for heap.
     */
    opaque type Data = Map[Addr, Value]

    /** Store the heap as a mutable field to avoid thread through it in the program. */
    class MutableData(private[Heap] var heap: Data):
      private[Heap] def update(addr: Addr, value: Value): Unit =
        heap.get(addr) match
        case None =>
          heap = heap.updated(addr, value)

        case Some(current) =>
          val value2 = value.join(current)
          if value2 != current then
            heap = heap.updated(addr, value2)


    def empty(): MutableData = new MutableData(Map.empty)

    def contains(addr: Addr)(using mutable: MutableData): Boolean =
      mutable.heap.contains(addr)

    def read(addr: Addr)(using mutable: MutableData): Value =
      mutable.heap(addr)

    def write(addr: Addr, value: Value)(using mutable: MutableData): Unit =
      mutable.update(addr, value)

    def localVarAddr(regions: Regions.Data, sym: Symbol, owner: ClassSymbol): Addr =
      LocalVarAddr(regions, sym, owner)

    def fieldVarAddr(regions: Regions.Data, sym: Symbol, owner: ClassSymbol): Addr =
      FieldAddr(regions, sym, owner)

    def arrayAddr(regions: Regions.Data, owner: ClassSymbol)(using Context): Addr =
      FieldAddr(regions, defn.ArrayClass, owner)

    def getHeapData()(using mutable: MutableData): Data = mutable.heap

  /** Cache used to terminate the check  */
  object Cache:
    case class Config(thisV: Value, env: Env.Data, heap: Heap.Data)
    case class Res(value: Value, heap: Heap.Data)

    class Data extends Cache[Config, Res]:
      def get(thisV: Value, expr: Tree)(using Heap.MutableData, Env.Data): Option[Value] =
        val config = Config(thisV, summon[Env.Data], Heap.getHeapData())
        super.get(config, expr).map(_.value)

      def cachedEval(thisV: Value, expr: Tree, cacheResult: Boolean)(fun: Tree => Value)(using Heap.MutableData, Env.Data): Value =
        val config = Config(thisV, summon[Env.Data], Heap.getHeapData())
        val result = super.cachedEval(config, expr, cacheResult, default = Res(Bottom, Heap.getHeapData())) { expr =>
          Res(fun(expr), Heap.getHeapData())
        }
        result.value
  end Cache

  /**
   * Region context for mutable states
   *
   * By default, the region context is empty.
   */
  object Regions:
    opaque type Data = List[SourcePosition]
    val empty: Data = Nil
    def extend(pos: SourcePosition)(using data: Data): Data = pos :: data
    def exists(pos: SourcePosition)(using data: Data): Boolean = data.indexOf(pos) >= 0
    def show(using data: Data, ctx: Context): String = data.map(_.show).mkString("[", ", ", "]")

  inline def cache(using c: Cache.Data): Cache.Data = c

  type Contextual[T] = (Context, State.Data, Env.Data, Cache.Data, Heap.MutableData, Regions.Data, Trace) ?=> T

  // --------------------------- domain operations -----------------------------

  type ArgInfo = TraceValue[Value]

  extension (a: Value)
    def join(b: Value): Value =
      (a, b) match
      case (Cold, b)                          => Cold
      case (a, Cold)                          => Cold
      case (Bottom, b)                        => b
      case (a, Bottom)                        => a
      case (RefSet(refs1), RefSet(refs2))     => RefSet(refs1 ++ refs2)
      case (a, RefSet(refs))                  => RefSet(refs + a)
      case (RefSet(refs), b)                  => RefSet(refs + b)
      case (a, b)                             => RefSet(ListSet(a, b))

    def widen(height: Int)(using Context): Value =
      a match
      case Bottom => Bottom

      case RefSet(refs) =>
        refs.map(ref => ref.widen(height)).join

      case Fun(code, thisV, klass, env) =>
        if height == 0 then Cold
        else Fun(code, thisV.widen(height), klass, env.widen(height))

      case ref @ OfClass(klass, outer, _, args, env) =>
        if height == 0 then
          Cold
        else
          val outer2 = outer.widen(height - 1)
          val args2 = args.map(_.widen(height - 1))
          val env2 = env.widen(height - 1)
          ref.widenedCopy(outer2, args2, env2)
      case _ => a


  extension (values: Iterable[Value])
    def join: Value = if values.isEmpty then Bottom else values.reduce { (v1, v2) => v1.join(v2) }

    def widen(height: Int): Contextual[List[Value]] = values.map(_.widen(height)).toList

  def call(value: Value, meth: Symbol, args: List[ArgInfo], receiver: Type, superType: Type, needResolve: Boolean = true): Contextual[Value] = log("call " + meth.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    value match
    case Cold =>
      report.warning("Using cold alias", Trace.position)
      Bottom

    case Bottom =>
      Bottom

    case arr: OfArray =>
      val target = resolve(defn.ArrayClass, meth)

      if target == defn.Array_apply || target == defn.Array_clone then
        if arr.addr.owner == State.currentObject then
          Heap.read(arr.addr)
        else
          errorReadOtherStaticObject(State.currentObject, arr.addr.owner)
          Bottom
      else if target == defn.Array_update then
        assert(args.size == 2, "Incorrect number of arguments for Array update, found = " + args.size)
        if arr.addr.owner != State.currentObject then
          errorMutateOtherStaticObject(State.currentObject, arr.addr.owner)
        else
          Heap.write(arr.addr, args.tail.head.value)
        Bottom
      else
        // Array.length is OK
        Bottom

    case ref: Ref =>
      val isLocal = !meth.owner.isClass
      val target =
        if !needResolve then
          meth
        else if superType.exists then
          meth
        else if meth.name.is(SuperAccessorName) then
          ResolveSuper.rebindSuper(ref.klass, meth)
        else
          resolve(ref.klass, meth)

      if target.isOneOf(Flags.Method) then
        if target.hasSource then
          val cls = target.owner.enclosingClass.asClass
          val ddef = target.defTree.asInstanceOf[DefDef]
          val meth = ddef.symbol

          val (thisV, outerEnv) =
            if meth.owner.isClass then
              (ref, Env.NoEnv)
            else
              Env.resolveEnv(meth.owner.enclosingMethod, ref, summon[Env.Data]).getOrElse(Cold -> Env.NoEnv)

          val env2 = Env.of(ddef, args.map(_.value), outerEnv)
          extendTrace(ddef) {
            given Env.Data = env2
            eval(ddef.rhs, ref, cls, cacheResult = true)
          }
        else
          Bottom
      else if target.exists then
        select(ref, target, receiver, needResolve = false)
      else
        if ref.klass.isSubClass(receiver.widenSingleton.classSymbol) then
          report.error("[Internal error] Unexpected resolution failure: ref.klass = " + ref.klass.show + ", meth = " + meth.show + Trace.show, Trace.position)
          Bottom
        else
          // This is possible due to incorrect type cast.
          // See tests/init/pos/Type.scala
          Bottom

    case Fun(code, thisV, klass, env) =>
      // meth == NoSymbol for poly functions
      if meth.name.toString == "tupled" then
        value // a call like `fun.tupled`
      else
        code match
        case ddef: DefDef =>
          given Env.Data = Env.of(ddef, args.map(_.value), env)
          extendTrace(code) { eval(ddef.rhs, thisV, klass, cacheResult = true) }

        case _ =>
          // by-name closure
          given Env.Data = env
          extendTrace(code) { eval(code, thisV, klass, cacheResult = true) }

    case RefSet(vs) =>
      vs.map(v => call(v, meth, args, receiver, superType)).join
  }

  def callConstructor(thisV: Value, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("call " + ctor.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {

    thisV match
    case ref: Ref =>
      if ctor.hasSource then
        val cls = ctor.owner.enclosingClass.asClass
        val ddef = ctor.defTree.asInstanceOf[DefDef]
        val argValues = args.map(_.value)

        given Env.Data = Env.of(ddef, argValues, Env.NoEnv)
        if ctor.isPrimaryConstructor then
          val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
          extendTrace(cls.defTree) { eval(tpl, ref, cls, cacheResult = true) }
          ref
        else
          extendTrace(ddef) { eval(ddef.rhs, ref, cls, cacheResult = true) }
      else
        // no source code available
        Bottom

    case _ =>
      report.error("[Internal error] unexpected constructor call, meth = " + ctor + ", this = " + thisV + Trace.show, Trace.position)
      Bottom
  }

  def select(thisV: Value, field: Symbol, receiver: Type, needResolve: Boolean = true): Contextual[Value] = log("select " + field.show + ", this = " + thisV.show, printer, (_: Value).show) {
    thisV match
    case Cold =>
      report.warning("Using cold alias", Trace.position)
      Bottom

    case ref: Ref =>
      val target = if needResolve then resolve(ref.klass, field) else field
      if target.is(Flags.Lazy) then
        given Env.Data = Env.emptyEnv(target.owner.asInstanceOf[ClassSymbol].primaryConstructor)
        val rhs = target.defTree.asInstanceOf[ValDef].rhs
        eval(rhs, ref, target.owner.asClass, cacheResult = true)
      else if target.exists then
        if target.isOneOf(Flags.Mutable) then
          if ref.hasVar(target) then
            val addr = ref.varAddr(target)
            if addr.owner == State.currentObject then
              Heap.read(addr)
            else
              errorReadOtherStaticObject(State.currentObject, addr.owner)
              Bottom
          else if ref.isObjectRef then
            report.warning("Access uninitialized field " + field.show + ". Call trace: " + Trace.show, Trace.position)
            Bottom
          else
            // initialization error, reported by the initialization checker
            Bottom
        else if ref.hasVal(target) then
          ref.valValue(target)
        else if ref.isObjectRef then
          report.warning("Access uninitialized field " + field.show + ". Call trace: " + Trace.show, Trace.position)
          Bottom
        else
          // initialization error, reported by the initialization checker
          Bottom

      else
        if ref.klass.isSubClass(receiver.widenSingleton.classSymbol) then
          report.error("[Internal error] Unexpected resolution failure: ref.klass = " + ref.klass.show + ", field = " + field.show + Trace.show, Trace.position)
          Bottom
        else
          // This is possible due to incorrect type cast.
          // See tests/init/pos/Type.scala
          Bottom

    case fun: Fun =>
      report.error("[Internal error] unexpected tree in selecting a function, fun = " + fun.code.show + Trace.show, fun.code)
      Bottom

    case Bottom =>
      if field.isStaticObject then ObjectRef(field.moduleClass.asClass)
      else Bottom

    case RefSet(refs) =>
      refs.map(ref => select(ref, field, receiver)).join
  }

  def assign(receiver: Value, field: Symbol, rhs: Value, rhsTyp: Type): Contextual[Value] = log("Assign" + field.show + " of " + receiver.show + ", rhs = " + rhs.show, printer, (_: Value).show) {
    receiver match
    case fun: Fun =>
      report.error("[Internal error] unexpected tree in assignment, fun = " + fun.code.show + Trace.show, Trace.position)

    case Cold =>
      report.warning("Assigning to cold aliases is forbidden", Trace.position)

    case Bottom =>

    case RefSet(refs) =>
      refs.foreach(ref => assign(ref, field, rhs, rhsTyp))

    case ref: Ref =>
      if ref.hasVar(field) then
        val addr = ref.varAddr(field)
        if addr.owner != State.currentObject then
          errorMutateOtherStaticObject(State.currentObject, addr.owner)
        else
          Heap.write(addr, rhs)
      else
        report.warning("Mutating a field before its initialization: " + field.show, Trace.position)
    end match

    Bottom
  }

  def instantiate(outer: Value, klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("instantiating " + klass.show + ", outer = " + outer + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    outer match

    case _ : Fun | _: OfArray  =>
      report.error("[Internal error] unexpected outer in instantiating a class, outer = " + outer.show + ", class = " + klass.show + ", " + Trace.show, Trace.position)
      Bottom

    case value: (Bottom.type | ObjectRef | OfClass | Cold.type) =>
      // The outer can be a bottom value for top-level classes.

      if klass == defn.ArrayClass then
        val arr = OfArray(State.currentObject, summon[Regions.Data])
        Heap.write(arr.addr, Bottom)
        arr
      else
        // Widen the outer to finitize the domain. Arguments already widened in `evalArgs`.
        val (outerWidened, envWidened) =
          if klass.owner.isClass then
            (outer.widen(1), Env.NoEnv)
          else
            // klass.enclosingMethod returns its primary constructor
            Env.resolveEnv(klass.owner.enclosingMethod, outer, summon[Env.Data]).getOrElse(Cold -> Env.NoEnv)

        val instance = OfClass(klass, outerWidened, ctor, args.map(_.value), envWidened)
        callConstructor(instance, ctor, args)
        instance

    case RefSet(refs) =>
      refs.map(ref => instantiate(ref, klass, ctor, args)).join
  }

  def initLocal(ref: Ref, sym: Symbol, value: Value): Contextual[Unit] = log("initialize local " + sym.show + " with " + value.show, printer) {
    if sym.is(Flags.Mutable) then
      val addr = Heap.localVarAddr(summon[Regions.Data], sym, State.currentObject)
      Env.setLocalVar(sym, addr)
      Heap.write(addr, value)
    else
      Env.setLocalVal(sym, value)
  }

  def readLocal(thisV: Value, sym: Symbol): Contextual[Value] = log("reading local " + sym.show, printer, (_: Value).show) {
    Env.resolveEnv(sym.enclosingMethod, thisV, summon[Env.Data]) match
    case Some(thisV -> env) =>
      if sym.is(Flags.Mutable) then
        // Assume forward reference check is doing a good job
        given Env.Data = env
        val addr = Env.varAddr(sym)
        if addr.owner == State.currentObject then
          Heap.read(addr)
        else
          errorReadOtherStaticObject(State.currentObject, addr.owner)
          Bottom
        end if
      else if sym.isPatternBound then
        // TODO: handle patterns
        Cold
      else
        given Env.Data = env
        try
          // Assume forward reference check is doing a good job
          val value = Env.valValue(sym)
          if sym.is(Flags.Param) && sym.info.isInstanceOf[ExprType] then
            value match
            case fun: Fun =>
              given Env.Data = fun.env
              eval(fun.code, fun.thisV, fun.klass)
            case Cold =>
              report.warning("Calling cold by-name alias. Call trace: \n" + Trace.show, Trace.position)
              Bottom
            case _: RefSet | _: Ref =>
              report.warning("[Internal error] Unexpected by-name value " + value.show  + ". Calling trace:\n" + Trace.show, Trace.position)
              Bottom
          else
            value

        catch ex =>
          report.warning("[Internal error] Not found " + sym.show + "\nenv = " + env.show + ". Calling trace:\n" + Trace.show, Trace.position)
          Bottom

    case _ =>
      if sym.is(Flags.Param) && sym.info.isInstanceOf[ExprType] then
        report.warning("Calling cold by-name alias. Call trace: \n" + Trace.show, Trace.position)
        Bottom
      else
        Cold
  }

  def writeLocal(thisV: Value, sym: Symbol, value: Value): Contextual[Value] = log("write local " + sym.show + " with " + value.show, printer, (_: Value).show) {

    assert(sym.is(Flags.Mutable), "Writing to immutable variable " + sym.show)
    Env.resolveEnv(sym.enclosingMethod, thisV, summon[Env.Data]) match
    case Some(thisV -> env) =>
      given Env.Data = env
      val addr = Env.varAddr(sym)
      if addr.owner != State.currentObject then
        errorMutateOtherStaticObject(State.currentObject, addr.owner)
      else
        Heap.write(addr, value)

    case _ =>
      report.warning("Assigning to variables in outer scope", Trace.position)

    Bottom
  }

  // -------------------------------- algorithm --------------------------------

  /** Check an individual object */
  private def accessObject(classSym: ClassSymbol)(using Context, State.Data, Trace): Value = log("accessing " + classSym.show, printer, (_: Value).show) {
    if classSym.hasSource then
      State.checkObjectAccess(classSym)
    else
      ObjectRef(classSym)
  }


  def checkClasses(classes: List[ClassSymbol])(using Context): Unit =
    given State.Data = new State.Data
    given Trace = Trace.empty

    for
      classSym <- classes  if classSym.isStaticObject
    do
      accessObject(classSym)

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
   * @param expr        The expression to be evaluated.
   * @param thisV       The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass       The enclosing class where the expression is located.
   * @param cacheResult It is used to reduce the size of the cache.
   */
  def eval(expr: Tree, thisV: Value, klass: ClassSymbol, cacheResult: Boolean = false): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + ", regions = " + Regions.show + " in " + klass.show, printer, (_: Value).show) {
    cache.cachedEval(thisV, expr, cacheResult) { expr => cases(expr, thisV, klass) }
  }


  /** Evaluate a list of expressions */
  def evalExprs(exprs: List[Tree], thisV: Value, klass: ClassSymbol): Contextual[List[Value]] =
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   *
   * @param expr   The expression to be evaluated.
   * @param thisV  The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass  The enclosing class where the expression `expr` is located.
   */
  def cases(expr: Tree, thisV: Value, klass: ClassSymbol): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + " in " + klass.show, printer, (_: Value).show) {
    val trace2 = trace.add(expr)

    expr match
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Bottom

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        withTrace(trace2) { evalType(expr.tpe, thisV, klass) }

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        withTrace(trace2) {
          val outer = outerValue(tref, thisV, klass)
          instantiate(outer, cls, ctor, args)
        }

      case Call(ref, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe: @unchecked
          val thisValue2 = extendTrace(ref) { resolveThis(thisTp.classSymbol.asClass, thisV, klass) }
          withTrace(trace2) { call(thisValue2, ref.symbol, args, thisTp, superTp) }

        case Select(qual, _) =>
          val receiver = eval(qual, thisV, klass)
          if ref.symbol.isConstructor then
            withTrace(trace2) { callConstructor(receiver, ref.symbol, args) }
          else
            withTrace(trace2) { call(receiver, ref.symbol, args, receiver = qual.tpe, superType = NoType) }

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = extendTrace(ref) { resolveThis(enclosingClass, thisV, klass) }
            // local methods are not a member, but we can reuse the method `call`
            withTrace(trace2) { call(thisValue2, id.symbol, args, receiver = NoType, superType = NoType, needResolve = false) }
          case TermRef(prefix, _) =>
            val receiver = withTrace(trace2) { evalType(prefix, thisV, klass) }
            if id.symbol.isConstructor then
              withTrace(trace2) { callConstructor(receiver, id.symbol, args) }
            else
              withTrace(trace2) { call(receiver, id.symbol, args, receiver = prefix, superType = NoType) }

      case Select(qualifier, name) =>
        val qual = eval(qualifier, thisV, klass)

        name match
          case OuterSelectName(_, _) =>
            val current = qualifier.tpe.classSymbol
            val target = expr.tpe.widenSingleton.classSymbol.asClass
            withTrace(trace2) { resolveThis(target, qual, current.asClass) }
          case _ =>
            withTrace(trace2) { select(qual, expr.symbol, receiver = qualifier.tpe) }

      case _: This =>
        evalType(expr.tpe, thisV, klass)

      case Literal(_) =>
        Bottom

      case Typed(expr, tpt) =>
        if tpt.tpe.hasAnnotation(defn.UncheckedAnnot) then
          Bottom
        else if tpt.tpe.hasAnnotation(defn.InitRegionAnnot) then
          val regions2 = Regions.extend(tpt.sourcePos)
          if Regions.exists(tpt.sourcePos) then
            report.warning("Cyclic region detected. Trace: " + Trace.show, tpt.sourcePos)
            Bottom
          else
            given Regions.Data = regions2
            eval(expr, thisV, klass)
        else
          eval(expr, thisV, klass)

      case NamedArg(name, arg) =>
        eval(arg, thisV, klass)

      case Assign(lhs, rhs) =>
        var isLocal = false
        val receiver =
          lhs match
          case Select(qual, _) =>
            eval(qual, thisV, klass)
          case id: Ident =>
            id.tpe match
            case TermRef(NoPrefix, _) =>
              isLocal = true
              thisV
            case TermRef(prefix, _) =>
              extendTrace(id) { evalType(prefix, thisV, klass) }

        val value = eval(rhs, thisV, klass)

        if isLocal then
          writeLocal(thisV, lhs.symbol, value)
        else
          withTrace(trace2) { assign(receiver, lhs.symbol, value, rhs.tpe) }

      case closureDef(ddef) =>
        Fun(ddef, thisV, klass, summon[Env.Data])

      case PolyFun(ddef) =>
        Fun(ddef, thisV, klass, summon[Env.Data])

      case Block(stats, expr) =>
        evalExprs(stats, thisV, klass)
        eval(expr, thisV, klass)

      case If(cond, thenp, elsep) =>
        evalExprs(cond :: thenp :: elsep :: Nil, thisV, klass).join

      case Annotated(arg, annot) =>
        if expr.tpe.hasAnnotation(defn.UncheckedAnnot) then
          Bottom
        else
          eval(arg, thisV, klass)

      case Match(selector, cases) =>
        eval(selector, thisV, klass)
        evalExprs(cases.map(_.body), thisV, klass).join

      case Return(expr, from) =>
        eval(expr, thisV, klass)

      case WhileDo(cond, body) =>
        evalExprs(cond :: body :: Nil, thisV, klass)
        Bottom

      case Labeled(_, expr) =>
        eval(expr, thisV, klass)

      case Try(block, cases, finalizer) =>
        eval(block, thisV, klass)
        if !finalizer.isEmpty then
          eval(finalizer, thisV, klass)
        evalExprs(cases.map(_.body), thisV, klass).join

      case SeqLiteral(elems, elemtpt) =>
        evalExprs(elems, thisV, klass).join

      case Inlined(call, bindings, expansion) =>
        evalExprs(bindings, thisV, klass)
        eval(expansion, thisV, klass)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Bottom

      case vdef : ValDef =>
        // local val definition
        val rhs = eval(vdef.rhs, thisV, klass)
        val sym = vdef.symbol
        initLocal(thisV.asInstanceOf[Ref], vdef.symbol, rhs)
        Bottom

      case ddef : DefDef =>
        // local method
        Bottom

      case tdef: TypeDef =>
        // local type definition
        Bottom

      case _: Import | _: Export =>
        Bottom

      case tpl: Template =>
        init(tpl, thisV.asInstanceOf[Ref], klass)

      case _ =>
        report.error("[Internal error] unexpected tree: " + expr + "\n" + Trace.show, expr)
        Bottom
  }

  /** Handle semantics of leaf nodes
   *
   * For leaf nodes, their semantics is determined by their types.
   *
   * @param tp      The type to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by `klass`.
   * @param klass   The enclosing class where the type `tp` is located.
   * @param elideObjectAccess Whether object access should be omitted.
   *
   * Object access elission happens when the object access is used as a prefix
   * in `new o.C` and `C` does not need an outer.
   */
  def evalType(tp: Type, thisV: Value, klass: ClassSymbol, elideObjectAccess: Boolean = false): Contextual[Value] = log("evaluating " + tp.show, printer, (_: Value).show) {
    tp match
      case _: ConstantType =>
        Bottom

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        val sym = tmref.symbol
        if sym.is(Flags.Package) then
          Bottom
        else if sym.owner.isClass then
          // The typer incorrectly assigns a TermRef with NoPrefix for `config`,
          // while the actual denotation points to the symbol of the class member
          // instead of the parameter symbol for the primary constructor.
          //
          // abstract class Base(implicit config: Int)
          // case class A(x: Int)(implicit config: Int) extends Base
          evalType(sym.termRef, thisV, klass, elideObjectAccess)
        else
          readLocal(thisV, sym)

      case tmref: TermRef =>
        val sym = tmref.symbol
        if sym.isStaticObject then
          if elideObjectAccess then
            ObjectRef(sym.moduleClass.asClass)
          else
            accessObject(sym.moduleClass.asClass)
        else
          val value = evalType(tmref.prefix, thisV, klass)
          select(value, tmref.symbol, tmref.prefix)

      case tp @ ThisType(tref) =>
        val sym = tref.symbol
        if sym.is(Flags.Package) then
          Bottom
        else if sym.isStaticObject && sym != klass then
          if elideObjectAccess then
            ObjectRef(sym.moduleClass.asClass)
          else
            accessObject(sym.moduleClass.asClass)

        else
          resolveThis(tref.classSymbol.asClass, thisV, klass)

      case _ =>
        throw new Exception("unexpected type: " + tp)
  }

  /** Evaluate arguments of methods and constructors */
  def evalArgs(args: List[Arg], thisV: Value, klass: ClassSymbol): Contextual[List[ArgInfo]] =
    val argInfos = new mutable.ArrayBuffer[ArgInfo]
    args.foreach { arg =>
      val res =
        if arg.isByName then
          Fun(arg.tree, thisV, klass, summon[Env.Data])
        else
          eval(arg.tree, thisV, klass)

      // TODO: handle @widen(n)
      val widened =
        if arg.tree.tpe.hasAnnotation(defn.InitExposeAnnot) then
          res.widen(1)
        else
          Cold

      argInfos += TraceValue(widened, trace.add(arg.tree))
    }
    argInfos.toList

  /** Initialize part of an abstract object in `klass` of the inheritance chain
   *
   * @param tpl       The class body to be evaluated.
   * @param thisV     The value of the current object to be initialized.
   * @param klass     The class to which the template belongs.
   */
  def init(tpl: Template, thisV: Ref, klass: ClassSymbol): Contextual[Value] = log("init " + klass.show, printer, (_: Value).show) {
    val paramsMap = tpl.constr.termParamss.flatten.map { vdef =>
      vdef.name -> Env.valValue(vdef.symbol)
    }.toMap

    // init param fields
    klass.paramGetters.foreach { acc =>
      val value = paramsMap(acc.name.toTermName)
      if acc.is(Flags.Mutable) then
        val addr = Heap.fieldVarAddr(summon[Regions.Data], acc, State.currentObject)
        thisV.initVar(acc, addr)
        Heap.write(addr, value)
      else
        thisV.initVal(acc, value)
      printer.println(acc.show + " initialized with " + value)
    }

    // Tasks is used to schedule super constructor calls.
    // Super constructor calls are delayed until all outers are set.
    type Tasks = mutable.ArrayBuffer[() => Unit]
    def superCall(tref: TypeRef, ctor: Symbol, args: List[ArgInfo], tasks: Tasks): Unit =
      val cls = tref.classSymbol.asClass
      // update outer for super class
      val res = outerValue(tref, thisV, klass)
      thisV.initOuter(cls, res)

      // follow constructor
      if cls.hasSource then
        tasks.append { () =>
          printer.println("init super class " + cls.show)
          callConstructor(thisV, ctor, args)
          ()
        }

    // parents
    def initParent(parent: Tree, tasks: Tasks) =
      parent match
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        evalExprs(stats, thisV, klass)
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
          if ctor.exists then
            // The parameter check of traits comes late in the mixin phase.
            // To avoid crash we supply hot values for erroneous parent calls.
            // See tests/neg/i16438.scala.
            val args: List[ArgInfo] = ctor.info.paramInfoss.flatten.map(_ => new ArgInfo(Bottom, Trace.empty))
            extendTrace(superParent) {
              superCall(tref, ctor, args, tasks)
            }
      }

      // initialize super classes after outers are set
      tasks.foreach(task => task())
    end if

    // class body
    tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) && !vdef.rhs.isEmpty =>
        val res = eval(vdef.rhs, thisV, klass)
        val sym = vdef.symbol
        if sym.is(Flags.Mutable) then
          val addr = Heap.fieldVarAddr(summon[Regions.Data], sym, State.currentObject)
          thisV.initVar(sym, addr)
          Heap.write(addr, res)
        else
          thisV.initVal(sym, res)

      case _: MemberDef =>

      case tree =>
        eval(tree, thisV, klass)
    }

    thisV
  }


  /** Resolve C.this that appear in `klass`
   *
   * @param target  The class symbol for `C` for which `C.this` is to be resolved.
   * @param thisV   The value for `D.this` where `D` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `C.this` is located.
   * @param elideObjectAccess Whether object access should be omitted.
   *
   * Object access elission happens when the object access is used as a prefix
   * in `new o.C` and `C` does not need an outer.
   */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol, elideObjectAccess: Boolean = false): Contextual[Value] = log("resolveThis target = " + target.show + ", this = " + thisV.show, printer, (_: Value).show) {
    if target == klass then
      thisV
    else if target.is(Flags.Package) then
      Bottom
    else if target.isStaticObject then
      val res = ObjectRef(target.moduleClass.asClass)
      if target == klass || elideObjectAccess then res
      else accessObject(target)
    else
      thisV match
        case Bottom => Bottom
        case Cold => Cold
        case ref: Ref =>
          val outerCls = klass.owner.lexicallyEnclosingClass.asClass
          if !ref.hasOuter(klass) then
            val error = "[Internal error] outer not yet initialized, target = " + target + ", klass = " + klass + Trace.show
            report.error(error, Trace.position)
            Bottom
          else
            resolveThis(target, ref.outerValue(klass), outerCls)
        case RefSet(refs) =>
          refs.map(ref => resolveThis(target, ref, klass)).join
        case fun: Fun =>
          report.error("[Internal error] unexpected thisV = " + thisV + ", target = " + target.show + ", klass = " + klass.show + Trace.show, Trace.position)
          Bottom
  }

  /** Compute the outer value that correspond to `tref.prefix`
   *
   * @param tref    The type whose prefix is to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `tref` is located.
   */
  def outerValue(tref: TypeRef, thisV: Value, klass: ClassSymbol): Contextual[Value] =
    val cls = tref.classSymbol.asClass
    if tref.prefix == NoPrefix then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      resolveThis(enclosing, thisV, klass, elideObjectAccess = cls.isStatic)
    else
      if cls.isAllOf(Flags.JavaInterface) then Bottom
      else evalType(tref.prefix, thisV, klass, elideObjectAccess = cls.isStatic)

  def errorMutateOtherStaticObject(currentObj: ClassSymbol, otherObj: ClassSymbol)(using Trace, Context) =
    val msg =
      s"Mutating ${otherObj.show} during initialization of ${currentObj.show}.\n" +
      "Mutating other static objects during the initialization of one static object is discouraged. " +
      "Calling trace:\n" + Trace.show

    report.warning(msg, Trace.position)

  def errorReadOtherStaticObject(currentObj: ClassSymbol, otherObj: ClassSymbol)(using Trace, Context) =
    val msg =
      "Reading mutable state of " + otherObj.show + " during initialization of " + currentObj.show + ".\n" +
      "Reading mutable state of other static objects is discouraged as it breaks initialization-time irrelevance. " +
      "Calling trace: " + Trace.show

    report.warning(msg, Trace.position)
