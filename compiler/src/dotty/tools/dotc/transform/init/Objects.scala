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
import config.Printers.init as printer
import reporting.StoreReporter
import reporting.trace as log

import Errors.*
import Trace.*
import Util.*

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.annotation.tailrec

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
   * A reference caches the current value.
   */
  sealed abstract class Ref extends Value:
    private val fields: mutable.Map[Symbol, Value] = mutable.Map.empty
    private val outers: mutable.Map[ClassSymbol, Value] = mutable.Map.empty

    def owner: ClassSymbol

    def klass: ClassSymbol

    def fieldValue(sym: Symbol): Value = fields(sym)

    def outerValue(cls: ClassSymbol): Value = outers(cls)

    def hasField(sym: Symbol): Boolean = fields.contains(sym)

    def hasOuter(cls: ClassSymbol): Boolean = outers.contains(cls)

    def updateField(field: Symbol, value: Value)(using Context) = log("Update field " + field + " = " + value + " for " + this, printer) {
      assert(!fields.contains(field), "Field already set " + field)
      fields(field) = value
    }

    def updateOuter(cls: ClassSymbol, value: Value)(using Context) = log("Update outer " + cls + " = " + value + " for " + this, printer) {
      assert(!outers.contains(cls), "Outer already set " + cls)
      outers(cls) = value
    }

  /** A reference to a static object */
  case class ObjectRef(klass: ClassSymbol) extends Ref:
    val owner = klass

    def show(using Context) = "ObjectRef(" + klass.show + ")"

  /**
   * Rerepsents values that are instances of the specified class
   *
   * `tp.classSymbol` should be the concrete class of the value at runtime.
   */
  case class OfClass private(klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value], owner: ClassSymbol) extends Ref:
    def show(using Context) = "OfClass(" + klass.show + ", outer = " + outer + ", args = " + args.map(_.show) + ")"

  object OfClass:
    def apply(klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value], owner: ClassSymbol)(using Context): OfClass =
      val instance = new OfClass(klass, outer, ctor, args, owner)
      instance.updateOuter(klass, outer)
      instance

  /**
   * Represents a lambda expression
   */
  case class Fun(expr: Tree, thisV: Value, klass: ClassSymbol) extends Value:
    def show(using Context) = "Fun(" + expr.show + ", " + thisV.show + ", " + klass.show + ")"

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
      private[State] val checkingObjects = new mutable.ListBuffer[ClassSymbol]
      private[State] val checkedObjects = new mutable.ArrayBuffer[ClassSymbol]
      private[State] val pendingTraces = new mutable.ListBuffer[Trace]

      // Use a mutable field to avoid thread through it in the program.
      private[State] var heap = Heap.empty
    end Data

    def getHeap()(using data: Data): Heap.Data = data.heap
    def setHeap(heap: Heap.Data)(using data: Data) =
      data.heap = heap

    def currentObject(using data: Data): ClassSymbol = data.checkingObjects.last

    def checkCycle(clazz: ClassSymbol)(work: => Unit)(using data: Data, ctx: Context, pendingTrace: Trace) =
      val index = data.checkingObjects.indexOf(clazz)

      if index != -1 && data.checkingObjects.size > 1 then
        val joinedTrace = data.pendingTraces.slice(index + 1, data.checkingObjects.size).foldLeft(pendingTrace) { (a, acc) => acc ++ a }
        val callTrace = Trace.buildStacktrace(joinedTrace, "Calling trace:\n")
        val cycle = data.checkingObjects.slice(index, data.checkingObjects.size)
        val pos = clazz.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template].constr
        report.warning("Cyclic initialization: " + cycle.map(_.show).mkString(" -> ") + " -> " + clazz.show + ". " + callTrace, pos)
      else if index == -1 && data.checkedObjects.indexOf(clazz) == -1 then
        data.pendingTraces += pendingTrace
        data.checkingObjects += clazz
        work
        assert(data.checkingObjects.last == clazz, "Expect = " + clazz.show + ", found = " + data.checkingObjects.last)
        data.pendingTraces.remove(data.pendingTraces.size - 1)
        data.checkedObjects += data.checkingObjects.remove(data.checkingObjects.size - 1)
    end checkCycle
  end State

  /** Environment for parameters */
  object Env:
    case class Data(private[Env] val params: Map[Symbol, Value]):
      private[Env] val locals: mutable.Map[Symbol, Value] = mutable.Map.empty

      private[Env] def get(x: Symbol)(using Context): Option[Value] =
        if x.is(Flags.Param) then params.get(x)
        else locals.get(x)

      private[Env] def contains(x: Symbol): Boolean = params.contains(x) || locals.contains(x)


    def empty: Data = new Data(Map.empty)

    def apply(x: Symbol)(using data: Data, ctx: Context): Value = data.get(x).get

    def get(x: Symbol)(using data: Data, ctx: Context): Option[Value] = data.get(x)

    def setLocalVal(x: Symbol, value: Value)(using data: Data, ctx: Context): Unit =
      assert(!x.isOneOf(Flags.Param | Flags.Mutable), "Only local immutable variable allowed")
      data.locals(x) = value

    def contains(x: Symbol)(using data: Data): Boolean = data.contains(x)

    def of(ddef: DefDef, args: List[Value])(using Context): Data =
      val params = ddef.termParamss.flatten.map(_.symbol)
      assert(args.size == params.size, "arguments = " + args.size + ", params = " + params.size)
      new Data(params.zip(args).toMap)

  /** Abstract heap for mutable fields
   *
   *  To avoid threading through it in the code, we use a mutable field in `State.Data` to hold the
   *  information.
   */
  object Heap:
    private abstract class Addr

    /** The address for mutable fields of objects. */
    private case class FieldAddr(ref: Ref, field: Symbol) extends Addr

    /** The address for mutable local variables . */
    private case class LocalVarAddr(ref: Ref, env: Env.Data, sym: Symbol) extends Addr

    opaque type Data = ImmutableMapWithRefEquality

    /** Wrap the map in a class such that equality of two maps is defined as referential equality.
     *
     *  This is a performance optimization.
     */
    private class ImmutableMapWithRefEquality(val map: Map[Addr, Value]):
      private[Heap] def updated(addr: Addr, value: Value): Data =
        map.get(addr) match
        case None =>
          new Data(map.updated(addr, value))

        case Some(current) =>
          val value2 = value.join(current)
          if value2 != current then
            new Data(map.updated(addr, value2))
          else
            this

    val empty: Data = new Data(Map.empty)

    def contains(ref: Ref, field: Symbol)(using state: State.Data): Boolean =
      val data: Data = State.getHeap()
      data.map.contains(FieldAddr(ref, field))

    def read(ref: Ref, field: Symbol)(using state: State.Data): Value =
      val data: Data = State.getHeap()
      data.map(FieldAddr(ref, field))

    def write(ref: Ref, field: Symbol, value: Value)(using state: State.Data): Unit =
      val addr = FieldAddr(ref, field)
      val data: Data = State.getHeap()
      val data2 = data.updated(addr, value)
      State.setHeap(data2)

    def containsLocalVar(ref: Ref, env: Env.Data, sym: Symbol)(using state: State.Data): Boolean =
      val data: Data = State.getHeap()
      data.map.contains(LocalVarAddr(ref, env, sym))

    def readLocalVar(ref: Ref, env: Env.Data, sym: Symbol)(using state: State.Data): Value =
      val data: Data = State.getHeap()
      data.map(LocalVarAddr(ref, env, sym))

    def writeLocalVar(ref: Ref, env: Env.Data, sym: Symbol, value: Value)(using state: State.Data): Unit =
      val addr = LocalVarAddr(ref, env, sym)
      val data: Data = State.getHeap()
      val data2 = data.updated(addr, value)
      State.setHeap(data2)

  /** Cache used to terminate the check  */
  object Cache:
    case class Config(thisV: Value, env: Env.Data, heap: Heap.Data)
    case class Res(value: Value, heap: Heap.Data)

    class Data extends Cache[Config, Res]:
      def get(thisV: Value, expr: Tree)(using State.Data, Env.Data): Option[Value] =
        val config = Config(thisV, summon[Env.Data], State.getHeap())
        super.get(config, expr).map(_.value)

      def cachedEval(thisV: Value, expr: Tree, cacheResult: Boolean)(fun: Tree => Value)(using State.Data, Env.Data): Value =
        val config = Config(thisV, summon[Env.Data], State.getHeap())
        val result = super.cachedEval(config, expr, cacheResult, default = Res(Bottom, State.getHeap())) { expr =>
          Res(fun(expr), State.getHeap())
        }
        result.value
  end Cache

  inline def cache(using c: Cache.Data): Cache.Data = c

  type Contextual[T] = (Context, State.Data, Env.Data, Cache.Data, Trace) ?=> T

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

      case Fun(expr, thisV, klass) =>
        if height == 0 then Cold
        else Fun(expr, thisV.widen(height), klass)

      case ref @ OfClass(klass, outer, init, args, owner) =>
        if height == 0 then
          Cold
        else
          val outer2 = outer.widen(height - 1)
          val args2 = args.map(arg => arg.widen(height - 1))
          OfClass(klass, outer2, init, args2, owner)
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
          ref match
          case obj: ObjectRef if obj.klass != State.currentObject && target.isOneOf(Flags.Mutable) =>
            errorMutateOtherStaticObject(State.currentObject, obj.klass)
            Bottom

          case _ =>
            val cls = target.owner.enclosingClass.asClass
            val ddef = target.defTree.asInstanceOf[DefDef]
            given Env.Data = Env.of(ddef, args.map(_.value))
            extendTrace(ddef) {
              eval(ddef.rhs, ref, cls, cacheResult = true)
            }
        else
          Bottom
      else if target.exists then
        if ref.hasField(target) then
          ref.fieldValue(target)
        else
          select(ref, target, receiver, needResolve = false)
      else
        if ref.klass.isSubClass(receiver.widenSingleton.classSymbol) then
          report.error("[Internal error] Unexpected resolution failure: ref.klass = " + ref.klass.show + ", meth = " + meth.show + Trace.show, Trace.position)
          Bottom
        else
          // This is possible due to incorrect type cast.
          // See tests/init/pos/Type.scala
          Bottom

    case Fun(expr, thisV, klass) =>
      // meth == NoSymbol for poly functions
      if meth.name.toString == "tupled" then
        value // a call like `fun.tupled`
      else
        eval(expr, thisV, klass, cacheResult = true)

    case RefSet(vs) =>
      vs.map(v => call(v, meth, args, receiver, superType)).join
  }

  def callConstructor(thisV: Value, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("call " + ctor.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    // init "fake" param fields for parameters of primary and secondary constructors
    def addParamsAsFields(args: List[Value], ref: Ref, ctorDef: DefDef) =
      val params = ctorDef.termParamss.flatten.map(_.symbol)
      assert(args.size == params.size, "arguments = " + args.size + ", params = " + params.size + ", ctor = " + ctor.show)
      for (param, value) <- params.zip(args) do
        ref.updateField(param, value)
        printer.println(param.show + " initialized with " + value)

    thisV match
    case ref: Ref =>
      if ctor.hasSource then
        val cls = ctor.owner.enclosingClass.asClass
        val ddef = ctor.defTree.asInstanceOf[DefDef]
        val argValues = args.map(_.value)
        addParamsAsFields(argValues, ref, ddef)
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

  def select(thisV: Value, field: Symbol, receiver: Type, needResolve: Boolean = true): Contextual[Value] = log("select " + field.show + ", this = " + thisV, printer, (_: Value).show) {
    thisV match
    case Cold =>
      report.warning("Using cold alias", Trace.position)
      Bottom

    case ref: Ref =>
      val target = if needResolve then resolve(ref.klass, field) else field
      if target.is(Flags.Lazy) then
        val rhs = target.defTree.asInstanceOf[ValDef].rhs
        eval(rhs, ref, target.owner.asClass, cacheResult = true)
      else if target.exists then
        if target.isOneOf(Flags.Mutable) then
          if ref.owner == State.currentObject then
            Heap.read(ref, field)
          else
            report.warning("Reading mutable state of " + ref.owner.show + " during initialization of " + State.currentObject + " is discouraged as it breaks initialization-time irrelevance. Calling trace: " + Trace.show, Trace.position)
            Bottom
          end if
        else if ref.hasField(target) then
          ref.fieldValue(target)
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
      report.error("[Internal error] unexpected tree in selecting a function, fun = " + fun.expr.show + Trace.show, fun.expr)
      Bottom

    case Bottom =>
      if field.isStaticObject then ObjectRef(field.moduleClass.asClass)
      else Bottom

    case RefSet(refs) =>
      refs.map(ref => select(ref, field, receiver)).join
  }

  def assign(receiver: Value, field: Symbol, rhs: Value, rhsTyp: Type): Contextual[Value] = log("Assign" + field.show + " of " + receiver.show + ", rhs = " + rhs.show, printer, (_: Value).show) {
    receiver match
    case Fun(body, thisV, klass) =>
      report.error("[Internal error] unexpected tree in assignment, fun = " + body.show + Trace.show, Trace.position)

    case Cold =>
      report.warning("Assigning to cold aliases is forbidden", Trace.position)

    case Bottom =>

    case RefSet(refs) =>
      refs.foreach(ref => assign(ref, field, rhs, rhsTyp))

    case ref: Ref =>
      if ref.owner != State.currentObject then
        errorMutateOtherStaticObject(State.currentObject, ref.owner)
      else
        Heap.write(ref, field, rhs)
    end match

    Bottom
  }

  def instantiate(outer: Value, klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("instantiating " + klass.show + ", outer = " + outer + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    outer match

    case Fun(body, thisV, klass) =>
      report.error("[Internal error] unexpected tree in instantiating a function, fun = " + body.show + Trace.show, Trace.position)
      Bottom

    case value: (Bottom.type | ObjectRef | OfClass | Cold.type) =>
      // The outer can be a bottom value for top-level classes.

      // Widen the outer to finitize the domain. Arguments already widened in `evalArgs`.
      val outerWidened = outer.widen(1)

      val instance = OfClass(klass, outerWidened, ctor, args.map(_.value), State.currentObject)
      callConstructor(instance, ctor, args)
      instance

    case RefSet(refs) =>
      refs.map(ref => instantiate(ref, klass, ctor, args)).join
  }

  // -------------------------------- algorithm --------------------------------

  /** Check an individual object */
  private def accessObject(classSym: ClassSymbol)(using Context, State.Data, Trace): Value = log("accessing " + classSym.show, printer, (_: Value).show) {
    if classSym.hasSource then
      val tpl = classSym.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]

      var count = 0
      given Cache.Data = new Cache.Data

      @tailrec
      def iterate()(using Context): Unit =
        count += 1

        given Trace = Trace.empty.add(tpl.constr)
        given env: Env.Data = Env.empty

        log("Iteration " + count) {
          init(tpl, ObjectRef(classSym), classSym)
        }

        val hasError = ctx.reporter.pendingMessages.nonEmpty
        if cache.hasChanged && !hasError then
          cache.prepareForNextIteration()
          iterate()
      end iterate

      State.checkCycle(classSym) {
        val reporter = new StoreReporter(ctx.reporter)
        iterate()(using ctx.fresh.setReporter(reporter))
        for warning <- reporter.pendingMessages do
          ctx.reporter.report(warning)
      }
    end if

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
  def eval(expr: Tree, thisV: Value, klass: ClassSymbol, cacheResult: Boolean = false): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + " in " + klass.show, printer, (_: Value).show) {
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
          // TODO: the local var might be from outer environment.
          Heap.writeLocalVar(receiver.asInstanceOf[Ref], summon[Env.Data], lhs.symbol, value)
          Bottom
        else
          withTrace(trace2) { assign(receiver, lhs.symbol, value, rhs.tpe) }

      case closureDef(ddef) =>
        Fun(ddef.rhs, thisV, klass)

      case PolyFun(body) =>
        Fun(body, thisV, klass)

      case Block(stats, expr) =>
        evalExprs(stats, thisV, klass)
        eval(expr, thisV, klass)

      case If(cond, thenp, elsep) =>
        evalExprs(cond :: thenp :: elsep :: Nil, thisV, klass).join

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Bottom
        else eval(arg, thisV, klass)

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
        if vdef.symbol.is(Flags.Mutable) then
          val ref = thisV.asInstanceOf[Ref]
          val env = summon[Env.Data]
          Heap.writeLocalVar(ref, env, sym, rhs)
        else
          Env.setLocalVal(vdef.symbol, rhs)

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
        val valueOpt = Env.get(sym)
        if valueOpt.nonEmpty then
          valueOpt.get

        else if sym.is(Flags.Mutable) then
          val ref = thisV.asInstanceOf[Ref]
          val env = summon[Env.Data]
          if Heap.containsLocalVar(ref, env, sym) then
            Heap.readLocalVar(ref, env, sym)
          else
            Cold

        else if sym.is(Flags.Package) then
          Bottom

        else
          Cold

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
          Fun(arg.tree, thisV, klass)
        else
          eval(arg.tree, thisV, klass)

      // TODO: handle @widen(n)
      val widened =
        if arg.tree.tpe.hasAnnotation(defn.InitExposeAnnot) then
          res.widen(1)
        else
          Cold

      argInfos += TraceValue(res, trace.add(arg.tree))
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
      vdef.name -> thisV.fieldValue(vdef.symbol)
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
        if vdef.symbol.is(Flags.Mutable) then
          Heap.write(thisV, vdef.symbol, res)
        else
          thisV.updateField(vdef.symbol, res)

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
