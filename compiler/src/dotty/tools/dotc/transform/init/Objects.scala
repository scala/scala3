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
import reporting.trace.force as log

import Errors.*
import Trace.*
import Util.*

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
 *  In the code above, the initialization of object `A` depends on `B` and vice
 *  versa. There is no correct way to initialize the code above. The current
 *  checker issues a warning for the code above.
 *
 *  At the high-level, the analysis has the following characteristics:
 *
 *  1. It is inter-procedural and flow-insensitive.
 *
 *  2. It is receiver-sensitive but not heap-sensitive nor parameter-sensitive.
 *
 *     Fields and parameters are always abstracted by their types.
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
    def show(using Context) = "ObjectRef(" + klass.show + ")"

  /**
   * Rerepsents values that are instances of the specified class
   *
   * `tp.classSymbol` should be the concrete class of the value at runtime.
   */
  case class OfClass private(tp: Type, klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value]) extends Ref:
    def show(using Context) = "OfClass(" + klass.show + ", outer = " + outer + ", args = " + args.map(_.show) + ")"

  object OfClass:
    def apply(tp: Type, klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value])(using Context): OfClass =
      val instance = new OfClass(tp, klass, outer, ctor, args)
      instance.updateOuter(klass, outer)
      instance

  /**
   * Rerepsents values of a specific type
   *
   * `OfType` is just a short-cut referring to currently instantiated sub-types.
   *
   * Note: this value should never be an index in the cache.
   */
  case class OfType(tp: Type) extends Value:
    def show(using Context) = "OfType(" + tp.show + ")"

  /**
   * Represents a lambda expression
   *
   * TODO: add concrete type of lambdas
   */
  case class Fun(expr: Tree, thisV: Value, klass: ClassSymbol) extends Value:
    def show(using Context) = "Fun(" + expr.show + ", " + thisV.show + ", " + klass.show + ")"

  /**
   * Represents a set of values
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: List[Value]) extends Value:
    assert(refs.forall(!_.isInstanceOf[RefSet]))
    def show(using Context) = refs.map(_.show).mkString("[", ",", "]")

  val Bottom = RefSet(Nil)

  object State:
    /** Record leaked instantiated types and lambdas.
     *
     *  For more fine-grained approximation, the values are distinguished by their types.
     */
    class LeakedInstances(classes: Map[ClassSymbol, List[Type]], funs: Map[Type, List[Fun]])

    object LeakedInstances:
      val empty = LeakedInstances(Map.empty, Map.empty)

    /**
     * Remembers the instantiated types during instantiation of a static object.
     */
    class Data:
      // objects under check
      private[State] val checkingObjects = new mutable.ListBuffer[ClassSymbol]
      private[State] val checkedObjects = new mutable.ArrayBuffer[ClassSymbol]
      private[State] val pendingTraces = new mutable.ListBuffer[Trace]

      private[State] val leakedInstancesByObject = mutable.Map.empty[ClassSymbol, LeakedInstances]
    end Data

    def currentObject(using data: Data): ClassSymbol = data.checkingObjects.last

    def leakedInstances(using data: Data): LeakedInstances =
      data.leakedInstancesByObject.getOrElseUpdate(currentObject, LeakedInstances.empty)

    def checkCycle(clazz: ClassSymbol)(work: => Unit)(using data: Data, ctx: Context, pendingTrace: Trace) =
      val index = data.checkingObjects.indexOf(clazz)

      if index != -1 then
        val joinedTrace = data.pendingTraces.slice(index + 1, data.checkingObjects.size).foldLeft(pendingTrace) { (a, acc) => acc ++ a }
        val callTrace = Trace.buildStacktrace(joinedTrace, "Calling trace:\n")
        val cycle = data.checkingObjects.slice(index, data.checkingObjects.size)
        report.warning("Cyclic initialization: " + cycle.map(_.show).mkString(" -> ") + " -> " + clazz.show + ". " + callTrace, clazz.defTree)
      else if data.checkedObjects.indexOf(clazz) == -1 then
        data.pendingTraces += pendingTrace
        data.checkingObjects += clazz
        work
        assert(data.checkingObjects.last == clazz, "Expect = " + clazz.show + ", found = " + data.checkingObjects.last)
        data.pendingTraces.remove(data.pendingTraces.size - 1)
        data.checkedObjects += data.checkingObjects.remove(data.checkingObjects.size - 1)
    end checkCycle
  end State

  object Cache:
    case class Config(thisV: Value, leakedInstances: State.LeakedInstances)
    case class Res(value: Value, leakedInstances: State.LeakedInstances)

    class Data extends Cache[Config, Res]:
      def get(thisV: Value, expr: Tree)(using State.Data): Option[Value] =
        val config = Config(thisV, State.leakedInstances)
        super.get(config, expr).map(_.value)

      def assume(thisV: Value, expr: Tree, cacheResult: Boolean)(fun: => Value)(using State.Data): Value =
        val config = Config(thisV, State.leakedInstances)
        val result = super.assume(config, expr, cacheResult, default = Res(Bottom, State.leakedInstances)) {
          Res(fun, State.leakedInstances)
        }
        result.value
  end Cache

  inline def cache(using c: Cache.Data): Cache.Data = c

  type Contextual[T] = (Context, State.Data, Cache.Data, Trace) ?=> T

  // --------------------------- domain operations -----------------------------

  type ArgInfo = TraceValue[Value]

  extension (a: Value)
    def join(b: Value): Value =
      (a, b) match
      case (Bottom, b)                        => b
      case (a, Bottom)                        => a
      case (RefSet(refs1), RefSet(refs2))     => RefSet(refs1 ++ refs2)
      case (a, RefSet(refs))                  => RefSet(a :: refs)
      case (RefSet(refs), b)                  => RefSet(b :: refs)
      case (a, b)                             => RefSet(a :: b :: Nil)

    def widenArg(using Context): Value =
      a match
      case Bottom => Bottom
      case RefSet(refs) => refs.map(_.widenArg).join
      case OfClass(tp, _, _: OfClass, _, _) => OfType(tp)
      case _ => a

  extension (values: Seq[Value])
    def join: Value = values.reduce { (v1, v2) => v1.join(v2) }

    def widenArgs: Contextual[List[Value]] = values.map(_.widenArg).toList

  def call(value: Value, meth: Symbol, args: List[ArgInfo], receiver: Type, superType: Type, needResolve: Boolean = true): Contextual[Value] = log("call " + meth.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    def checkArgs() =
      // TODO: check aliasing of static objects
      for
        arg <- args
      do
        arg.value match
        case ObjectRef(obj) =>
          report.warning("Aliasing object " + obj.show, Trace.position)

        case _ =>

    value match
    case Bottom =>
      Bottom

    case ref: Ref =>
      checkArgs()

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

    case OfType(tp) =>
      checkArgs()

      if meth.exists && meth.isEffectivelyFinal then
        if meth.hasSource then
          val isLocal = meth.owner.isClass
          val ddef = meth.defTree.asInstanceOf[DefDef]
          extendTrace(ddef) {
            eval(ddef.rhs, value, meth.owner.enclosingClass.asClass, cacheResult = true)
          }
        else
          Bottom
      else
        // TODO: approximate call with instantiated types
        report.warning("Virtual method call ", Trace.position)
        Bottom

    case Fun(expr, thisV, klass) =>
      checkArgs()

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
        val args2 = args.map(_.value).widenArgs
        addParamsAsFields(args2, ref, ddef)
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
    case ref: Ref =>
      val target = if needResolve then resolve(ref.klass, field) else field
      if target.is(Flags.Lazy) then
        val rhs = target.defTree.asInstanceOf[ValDef].rhs
        eval(rhs, ref, target.owner.asClass, cacheResult = true)
      else if target.exists then
        ref match
        case obj: ObjectRef if State.currentObject != obj.klass =>
          if target.isOneOf(Flags.Mutable) then
            report.warning("Reading mutable state of " + obj.klass + " during initialization of " + State.currentObject + " is discouraged as it breaks initialization-time irrelevance", Trace.position)
            Bottom
          else
            if ref.hasField(target) then
              // TODO:  check immutability
              ref.fieldValue(target)
            else
              // initialization error, reported by the initialization checker
              Bottom

        case _ =>
          if ref.hasField(target) then
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

    case OfType(tp) =>
      if field.isEffectivelyFinal then
        if field.hasSource then
          val vdef = field.defTree.asInstanceOf[ValDef]
          eval(vdef.rhs, thisV, field.owner.enclosingClass.asClass, cacheResult = true)
        else
          Bottom
      else
        val fieldType = tp.memberInfo(field)
        OfType(fieldType)

    case fun: Fun =>
      report.error("[Internal error] unexpected tree in selecting a function, fun = " + fun.expr.show + Trace.show, fun.expr)
      Bottom

    case Bottom =>
      if field.isStaticObject then ObjectRef(field.moduleClass.asClass)
      else Bottom

    case RefSet(refs) =>
      refs.map(ref => select(ref, field, receiver)).join
  }

  def instantiate(outer: Value, klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("instantiating " + klass.show + ", outer = " + outer + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    outer match

    case Fun(body, thisV, klass) =>
      report.error("[Internal error] unexpected tree in instantiating a function, fun = " + body.show + Trace.show, Trace.position)
      Bottom

    case value: (Bottom.type | ObjectRef | OfClass | OfType) =>
      // widen the outer to finitize the domain
      val outerWidened = outer.widenArg
      val argsWidened = args.map(_.value).widenArgs

      // TODO: type arguments
      val tp = value match
        case v: ObjectRef => v.klass.typeRef.memberInfo(klass)
        case v: OfClass   => v.tp.memberInfo(klass)
        case v: OfType    => v.tp.memberInfo(klass)
        case Bottom       => klass.typeRef

      val instance = OfClass(tp, klass, outerWidened, ctor, argsWidened)
      val argInfos2 = args.zip(argsWidened).map { (argInfo, v) => argInfo.copy(value = v) }
      callConstructor(instance, ctor, argInfos2)
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

        given Trace = Trace.empty.add(classSym.defTree)

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
    cache.get(thisV, expr) match
    case Some(value) => value
    case None =>
      cache.assume(thisV, expr, cacheResult) { cases(expr, thisV, klass) }
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
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot))
          Bottom
        else
          eval(expr, thisV, klass)

      case NamedArg(name, arg) =>
        eval(arg, thisV, klass)

      case Assign(lhs, rhs) =>
        lhs match
        case Select(qual, _) =>
          eval(qual, thisV, klass)
          eval(rhs, thisV, klass)
        case id: Ident =>
          eval(rhs, thisV, klass)

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
        eval(vdef.rhs, thisV, klass)

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
        // - params and var definitions are abstract by its type
        // - evaluate the rhs of the local definition for val definitions
        val sym = tmref.symbol
        if sym.isOneOf(Flags.Param | Flags.Mutable) then
          OfType(sym.info)
        else if sym.is(Flags.Package) then
          Bottom
        else if sym.hasSource then
          val rhs = sym.defTree.asInstanceOf[ValDef].rhs
          eval(rhs, thisV, klass)
        else
          // pattern-bound variables
          OfType(sym.info)

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

  /** Evaluate arguments of methods */
  def evalArgs(args: List[Arg], thisV: Value, klass: ClassSymbol): Contextual[List[ArgInfo]] =
    val argInfos = new mutable.ArrayBuffer[ArgInfo]
    args.foreach { arg =>
      val res =
        if arg.isByName then
          Fun(arg.tree, thisV, klass)
        else
          eval(arg.tree, thisV, klass)

      argInfos += TraceValue(res, trace.add(arg.tree))
    }
    argInfos.toList

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
        case OfType(tp) =>
          OfType(target.appliedRef)
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
