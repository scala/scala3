package dotty.tools.dotc
package transform
package init

import core.*
import Contexts.*
import Symbols.*
import Types.*
import Denotations.Denotation
import StdNames.*
import Names.TermName
import NameKinds.OuterSelectName
import NameKinds.SuperAccessorName
import Decorators.*

import ast.tpd.*
import util.{ SourcePosition, NoSourcePosition }
import config.Printers.init as printer
import reporting.StoreReporter
import reporting.trace as log
import reporting.trace.force as forcelog
import typer.Applications.*

import Errors.*
import Trace.*
import Util.*

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.annotation.tailrec
import scala.annotation.constructorOnly
import dotty.tools.dotc.core.Flags.AbstractOrTrait
import dotty.tools.dotc.util.SrcPos

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
 *  2. It is inter-procedural and flow-sensitive.
 *
 *  3. The check is modular in the sense that each object is checked separately and there is no
 *     whole-program analysis. However, the check is not modular in terms of project boundaries.
 *
 */
class Objects(using Context @constructorOnly):
  val immutableHashSetNode: Symbol = requiredClass("scala.collection.immutable.SetNode")
  // TODO: this should really be an annotation on the rhs of the field initializer rather than the field itself.
  val SetNode_EmptySetNode: Symbol = Denotations.staticRef("scala.collection.immutable.SetNode.EmptySetNode".toTermName).symbol
  val immutableHashSet: Symbol = requiredModule("scala.collection.immutable.HashSet")
  val HashSet_EmptySet: Symbol = Denotations.staticRef("scala.collection.immutable.HashSet.EmptySet".toTermName).symbol
  val immutableVector: Symbol = requiredModule("scala.collection.immutable.Vector")
  val Vector_EmptyIterator: Symbol = immutableVector.requiredValue("emptyIterator")
  val immutableMapNode: Symbol = requiredModule("scala.collection.immutable.MapNode")
  val MapNode_EmptyMapNode: Symbol = immutableMapNode.requiredValue("EmptyMapNode")
  val immutableHashMap: Symbol = requiredModule("scala.collection.immutable.HashMap")
  val HashMap_EmptyMap: Symbol = immutableHashMap.requiredValue("EmptyMap")
  val ManifestFactory_ObjectTYPE = defn.ManifestFactoryModule.requiredValue("ObjectTYPE")
  val ManifestFactory_NothingTYPE = defn.ManifestFactoryModule.requiredValue("NothingTYPE")
  val ManifestFactory_NullTYPE = defn.ManifestFactoryModule.requiredValue("NullTYPE")

  val allowList: Set[Symbol] = Set(SetNode_EmptySetNode, HashSet_EmptySet, Vector_EmptyIterator, MapNode_EmptyMapNode, HashMap_EmptyMap,
    ManifestFactory_ObjectTYPE, ManifestFactory_NothingTYPE, ManifestFactory_NullTYPE)

  /**
   * Whether the analysis work in best-effort mode in contrast to aggressive mode.
   *
   * - In best-effort mode, the analysis tries to be fast, useful and unobtrusive.
   * - In the aggressive mode, the analysis tries to be sound and verbose by spending more check time.
   *
   * In both mode, there is a worst-case guarantee based on a quota on the
   * number of method calls in initializing a global object.
   *
   * We use a patch to set `BestEffort` to `false` in testing community projects.
   */
  val BestEffort: Boolean = true

  /** The analysis has run out of quota */
  class OutOfQuotaException(count: Int) extends Exception

  def checkCost(count: Int): Unit =
    if count > 500 && BestEffort || count > 2000 && !BestEffort then
      throw new OutOfQuotaException(count)

  // ----------------------------- abstract domain -----------------------------

  /** Syntax for the data structure abstraction used in abstract domain:
   *
   * ve ::= ObjectRef(class)                                             // global object
   *      | InstanceRef(class, ownerObject, ctor, regions)               // instance of a class
   *      | ArrayRef(ownerObject, regions)                               // represents values of native array class in Array.scala
   *      | Fun(code, thisV, scope)                                      // value elements that can be contained in ValueSet
   *      | SafeValue                                                    // values on which method calls and field accesses won't cause warnings. Int, String, etc.
   *      | UnknownValue                                                 // values whose source are unknown at compile time
   * vs ::= ValueSet(Set(ve))                                            // set of abstract values
   * Value ::= ve | vs | Package
   * Ref ::= ObjectRef | InstanceRef | ArrayRef                          // values that represent a reference to some (global or instance) object
   * RefSet ::= Set(ref)                                                 // set of refs
   * Bottom ::= RefSet(Empty)                                            // unreachable code
   * ThisValue ::= Ref | RefSet                                          // possible values for 'this'
   * EnvRef(tree, ownerObject)                                           // represents environments for evaluating methods, functions, or lazy/by-name values
   * EnvSet ::= Set(EnvRef)
   * InstanceBody ::= (valsMap: Map[Symbol, Value],
   *                   outersMap: Map[ClassSymbol, Value],
   *                   outerEnv: EnvSet)                                 // represents combined information of all instances represented by a ref
   * Heap ::= Ref -> InstanceBody                                        // heap is mutable
   * EnvBody ::= (valsMap: Map[Symbol, Value],
   *              thisV: Value,
   *              outerEnv: EnvSet)                                      // represents combined information of all instances represented by an env
   * EnvMap ::= EnvRef -> EnvBody
   * Scope ::= Ref | EnvRef
   * Config ::= (thisV: Value, scope: Scope, Heap, EnvMap)
   * Cache ::= Config -> (Heap, EnvMap)
   *
   * regions ::= List(sourcePosition)
   */

  sealed trait Value:
    def show(using Context): String

  /** ValueElement are elements that can be contained in a ValueSet */
  sealed trait ValueElement extends Value

  /**
   * Represents the possible values of the current enclosing scope when evaluating an expression
   */
  sealed abstract class Scope(using trace: Trace): // TODO: rename it to reflect that it is key to the heap
    def getTrace: Trace = trace

    def isRef = this.isInstanceOf[Ref]

    def isEnv = this.isInstanceOf[Env.EnvRef]

    def asRef: Ref = this.asInstanceOf[Ref]

    def asEnv: Env.EnvRef = this.asInstanceOf[Env.EnvRef]

    def owner: ClassSymbol

    def show(using Context): String
  end Scope

  sealed abstract class Ref(using Trace) extends Scope with ValueElement:
    def klass: ClassSymbol

    def isObjectRef: Boolean = this.isInstanceOf[ObjectRef]

    def asObjectRef: ObjectRef = this.asInstanceOf[ObjectRef]

    def valValue(sym: Symbol)(using Heap.MutableData): Value = Heap.readVal(this, sym)

    def varValue(sym: Symbol)(using Heap.MutableData): Value = Heap.readVal(this, sym)

    def hasVal(sym: Symbol)(using Heap.MutableData): Boolean = Heap.containsVal(this, sym)

    def hasVar(sym: Symbol)(using Heap.MutableData): Boolean = Heap.containsVal(this, sym)

    def initVal(field: Symbol, value: Value)(using Context, Heap.MutableData) = log("Initialize " + field.show + " = " + value + " for " + this, printer) {
      assert(!field.is(Flags.Mutable), "Field is mutable: " + field.show)
      Heap.writeJoinVal(this, field, value)
    }

    def initVar(field: Symbol, value: Value)(using Context, Heap.MutableData) = log("Initialize " + field.show + " = " + value + " for " + this, printer) {
      assert(field.is(Flags.Mutable), "Field is not mutable: " + field.show)
      Heap.writeJoinVal(this, field, value)
    }

    def initOuter(sym: Symbol, outers: Value)(using Context, Heap.MutableData) = log("Initialize outer " + sym.show + " = " + outers + " for " + this, printer) {
      Heap.writeJoinOuter(this, sym, outers)
    }

    def initOuterEnv(outerEnvs: Env.EnvSet)(using Context, Heap.MutableData) =
      Heap.writeJoinOuterEnv(this, outerEnvs)

    def outerValue(sym: Symbol)(using Heap.MutableData): Value = Heap.readOuter(this, sym)

    def hasOuter(classSymbol: Symbol)(using Heap.MutableData): Boolean = Heap.hasOuter(this, classSymbol)

    def outer(using Heap.MutableData): Value = this.outerValue(klass)

    def outerEnv(using Heap.MutableData): Env.EnvSet = Heap.readOuterEnv(this)
  end Ref

  /** A reference to a static object
   *
   *  Invariant: The reference itself should not contain any state
   *
   *  Rationale: There can be multiple references to the same object. They must
   *  share the same state.
   */
  case class ObjectRef private (klass: ClassSymbol)(using Trace) extends Ref:
    /** Use the special outer to denote whether the super constructor of the
     *  object has been called or not.
     */
    def isAfterSuperCall(using Heap.MutableData) =
      this.hasOuter(klass.sourceModule)

    def setAfterSuperCall()(using Heap.MutableData): Unit =
      this.initOuter(klass.sourceModule, Bottom)

    def owner = klass

    def show(using Context) = "ObjectRef(" + klass.show + ")"

  object ObjectRef:
    def apply(klass: ClassSymbol)(using Context, Heap.MutableData, Trace): ObjectRef =
      val obj = new ObjectRef(klass)
      obj.initOuter(klass, Bottom)
      obj.initOuterEnv(Env.NoEnv)
      obj

  /**
   * Represents values that are instances of the specified class.
   *
   * Note that the 2nd parameter block does not take part in the definition of equality.
   */
  case class InstanceRef private (
    klass: ClassSymbol, owner: ClassSymbol, ctor: Symbol, regions: Regions.Data)(using Trace)
  extends Ref:
    def show(using Context) =
      "InstanceRef(" + klass.show + ", ctor = " + ctor.show + ", owner = " + owner + ")"

  object InstanceRef:
    def apply(
      klass: ClassSymbol, outer: Value, outerEnv: Env.EnvSet, ctor: Symbol)(
      using Context, Heap.MutableData, State.Data, Regions.Data, Trace
    ): InstanceRef =
      val owner = State.currentObject
      val instance = new InstanceRef(klass, owner, ctor, summon[Regions.Data])
      instance.initOuter(klass, outer)
      instance.initOuterEnv(outerEnv)
      instance

  /**
   * Represents arrays.
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
  case class ArrayRef private (owner: ClassSymbol, regions: Regions.Data)(using Trace) extends Ref:
    val elementSymbol = defn.ArrayConstructor

    def klass: ClassSymbol = defn.ArrayClass

    def show(using Context) = "ArrayRef(owner = " + owner.show + ")"

    def readElement(using Heap.MutableData) = valValue(elementSymbol)

    def writeElement(value: Value)(using Heap.MutableData) = Heap.writeJoinVal(this, elementSymbol, value)

  object ArrayRef:
    def apply(owner: ClassSymbol, regions: Regions.Data)(using Context, Trace, Heap.MutableData): ArrayRef =
      val arr = new ArrayRef(owner, regions)
      arr.initVal(arr.elementSymbol, Bottom)
      arr.initOuter(arr.klass, Bottom)
      arr.initOuterEnv(Env.NoEnv)
      arr

  /**
   * Represents a lambda expression
   * @param klass The enclosing class of the anonymous function's creation site
   */
  case class Fun(code: Tree, thisV: ThisValue, klass: ClassSymbol, scope: Scope) extends ValueElement:
    def show(using Context) = "Fun(" + code.show + ", " + scope.show + ", " + klass.show + ")"

  /**
   * Represents common base values like Int, String, etc.
   * Assumption: all field initializers and methods calls on such values should not trigger initialization of global objects
   * or read/write mutable fields
   */
  case class SafeValue(typeSymbol: Symbol) extends ValueElement:
    assert(SafeValue.safeTypeSymbols.contains(typeSymbol), "Invalid creation of SafeValue! Type = " + typeSymbol)
    def show(using Context): String = "SafeValue of " + typeSymbol.show

  object SafeValue:
    val safeTypeSymbols =
      defn.StringClass ::
      (defn.ScalaNumericValueTypeList ++
       List(defn.UnitType, defn.BooleanType, defn.NullType, defn.ClassClass.typeRef))
      .map(_.symbol)

    def getSafeTypeSymbol(tpe: Type): Option[Symbol] =
      val baseType = if tpe.isInstanceOf[AppliedType] then tpe.asInstanceOf[AppliedType].underlying else tpe
      if baseType.isInstanceOf[TypeRef] then
        val typeRef = baseType.asInstanceOf[TypeRef]
        val typeSymbol = typeRef.symbol
        val typeAlias = typeRef.translucentSuperType
        if safeTypeSymbols.contains(typeSymbol) then
          Some(typeSymbol)
        else if typeAlias.isInstanceOf[TypeRef] && typeAlias.asInstanceOf[TypeRef].symbol == defn.StringClass then
          // Special case, type scala.Predef.String = java.lang.String
          Some(defn.StringClass)
        else None
      else
        None

    def apply(tpe: Type): SafeValue =
      // tpe could be a AppliedType(java.lang.Class, T)
      val typeSymbol = getSafeTypeSymbol(tpe)
      assert(typeSymbol.isDefined, "Invalid creation of SafeValue with type " + tpe)
      new SafeValue(typeSymbol.get)

  /** Represents values unknown to the checker, such as values loaded without source
   */
  case object UnknownValue extends ValueElement:
    def show(using Context): String = "UnknownValue"

  /**
   * Represents a set of values
   *
   * It comes from `if` expressions.
   */
  case class ValueSet(values: Set[ValueElement]) extends Value:
    def show(using Context) = values.map(_.show).mkString("[", ",", "]")

    def isRefSet = this.isInstanceOf[RefSet]

  /**
   * Represents a set of Refs. The `equals` method inherits from `ValueSet`,
   * so no more fields should be added to `RefSet`
   */
  class RefSet(val refs: Set[Ref]) extends ValueSet(refs.asInstanceOf[Set[ValueElement]]):
    def joinOuters(sym: ClassSymbol)(using Heap.MutableData): ThisValue =
      refs.map(_.outerValue(sym)).join.asInstanceOf[ThisValue]

    def joinOuterEnvs(using Heap.MutableData): Env.EnvSet =
      refs.map(_.outerEnv).join

  case class Package(packageModuleClass: ClassSymbol) extends Value: // TODO: try to remove packages
    def show(using Context): String = "Package(" + packageModuleClass.show + ")"

  object Package:
    def apply(packageSym: Symbol): Package =
      assert(packageSym.is(Flags.Package), "Invalid symbol to create Package!")
      Package(packageSym.moduleClass.asClass)

  val Bottom = new RefSet(Set.empty)

  /** Possible types for 'this' */
  type ThisValue = Ref | RefSet

  /** Checking state  */
  object State:
    class Data:
      // objects under check
      private[State] val checkingObjects = new mutable.ArrayBuffer[ObjectRef]
      private[State] val checkedObjects = new mutable.ArrayBuffer[ObjectRef]
      private[State] val pendingTraces = new mutable.ArrayBuffer[Trace]

      /** It records how many calls have being analyzed for the current object under check */
      private[State] val checkingCosts = new mutable.ArrayBuffer[Int]

      private[State] val quotaExhaustedObjects = new mutable.ArrayBuffer[ObjectRef]

      def addChecking(obj: ObjectRef): Unit =
        this.checkingObjects += obj
        this.checkingCosts += 0

      def popChecking(): Unit =
        val index = this.checkingObjects.size - 1
        checkingObjects.remove(index)
        checkingCosts.remove(index)

      def addChecked(obj: ObjectRef): Unit =
        this.checkedObjects += obj

      def addQuotaExhausted(obj: ObjectRef): Unit =
        this.quotaExhaustedObjects += obj
    end Data

    def currentObject(using data: Data): ClassSymbol = data.checkingObjects.last.klass

    def recordCall()(using data: Data): Unit =
      val lastIndex = data.checkingCosts.size - 1
      val callCount = data.checkingCosts(lastIndex) + 1
      data.checkingCosts(lastIndex) = callCount
      checkCost(callCount)

    def isQuotaExhausted(obj: ObjectRef)(using data: Data): Boolean =
      data.quotaExhaustedObjects.contains(obj)

    private def doCheckObject(classSym: ClassSymbol)(using ctx: Context, data: Data, heap: Heap.MutableData, envMap: EnvMap.EnvMapMutableData) =
      val tpl = classSym.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]

      var count = 0
      given Cache.Data = new Cache.Data

      @tailrec
      def iterate()(using Context): ObjectRef =
        count += 1

        given Trace = Trace.empty.add(classSym.defTree)
        // given Heap.MutableData = Heap.empty
        given returns: Returns.Data = Returns.empty()
        given regions: Regions.Data = Regions.empty // explicit name to avoid naming conflict

        val obj = ObjectRef(classSym)
        given Scope = obj
        log("Iteration " + count) {
          data.addChecking(obj)

          try
            init(tpl, obj, classSym)
          catch case _: OutOfQuotaException =>
            report.warning("Giving up checking initialization of " + classSym + " due to exhausted budget", classSym.sourcePos)
            data.addQuotaExhausted(obj)
            data.addChecked(obj)
            data.popChecking()
            return obj

          assert(data.checkingObjects.last.klass == classSym, "Expect = " + classSym.show + ", found = " + data.checkingObjects.last.klass)

          data.popChecking()
        }

        val hasError = ctx.reporter.pendingMessages.nonEmpty
        if cache.hasChanged && !hasError then
          if count <= 3 then
            cache.prepareForNextIteration()
            iterate()
          else
            if !BestEffort then
              report.warning("Giving up checking initialization of " + classSym + " due to iteration > = " + count, classSym.sourcePos)
            data.addChecked(obj)
            obj
        else
          data.addChecked(obj)
          obj
      end iterate

      val reporter = new StoreReporter(ctx.reporter)
      val obj = iterate()(using ctx.fresh.setReporter(reporter))
      for warning <- reporter.pendingMessages do
        ctx.reporter.report(warning)

      obj
    end doCheckObject

    def checkObjectAccess(clazz: ClassSymbol)(using data: Data, ctx: Context, pendingTrace: Trace, heap: Heap.MutableData, envMap: EnvMap.EnvMapMutableData): ObjectRef =
      val index = data.checkingObjects.indexWhere(_.klass == clazz)

      if index != -1 then
        if data.checkingObjects.size - 1 > index then
          // Only report errors for non-trivial cycles, ignore self cycles.
          val joinedTrace = data.pendingTraces.slice(index + 1, data.checkingObjects.size).foldLeft(pendingTrace) { (a, acc) => acc ++ a }
          val callTrace = Trace.buildStacktrace(joinedTrace, "Calling trace:\n")
          val cycle = data.checkingObjects.slice(index, data.checkingObjects.size)
          val pos = clazz.defTree.sourcePos.focus
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
    /** Represents environments for evaluating methods, functions, or lazy/by-name values
     *  For methods or closures, `tree` is the DefDef of the method.
     *  For lazy/by-name values, `tree` is the rhs of the definition or the argument passed to by-name param
     */
    case class EnvRef(tree: Tree, owner: ClassSymbol)(using Trace) extends Scope:
      override def equals(that: Any): Boolean =
        that.isInstanceOf[EnvRef] &&
        (that.asInstanceOf[EnvRef].tree eq tree) &&
        (that.asInstanceOf[EnvRef].owner == owner)

      def show(using Context) =
        "tree: " + tree.show + "\n" +
        "owner: " + owner.show

      def valValue(sym: Symbol)(using EnvMap.EnvMapMutableData): Value = EnvMap.readVal(this, sym)

      def varValue(sym: Symbol)(using EnvMap.EnvMapMutableData): Value = EnvMap.readVal(this, sym)

      def hasVal(sym: Symbol)(using EnvMap.EnvMapMutableData): Boolean = EnvMap.containsVal(this, sym)

      def hasVar(sym: Symbol)(using EnvMap.EnvMapMutableData): Boolean = EnvMap.containsVal(this, sym)

      def initVal(field: Symbol, value: Value)(using Context, EnvMap.EnvMapMutableData) = log("Initialize " + field.show + " = " + value + " for " + this, printer) {
        assert(!field.is(Flags.Mutable), "Field is mutable: " + field.show)
        EnvMap.writeJoinVal(this, field, value)
      }

      def initVar(field: Symbol, value: Value)(using Context, EnvMap.EnvMapMutableData) = log("Initialize " + field.show + " = " + value + " for " + this, printer) {
        assert(field.is(Flags.Mutable), "Field is not mutable: " + field.show)
        EnvMap.writeJoinVal(this, field, value)
      }

      def initThisV(thisV: ThisValue)(using EnvMap.EnvMapMutableData) =
        EnvMap.writeJoinThisV(this, thisV)

      def initOuterEnvs(outerEnvs: EnvSet)(using EnvMap.EnvMapMutableData) =
        EnvMap.writeJoinOuterEnv(this, outerEnvs)

      def thisV(using EnvMap.EnvMapMutableData): ThisValue = EnvMap.getThisV(this)

      def outerEnvs(using EnvMap.EnvMapMutableData): EnvSet = EnvMap.getOuterEnvs(this)
    end EnvRef

    case class EnvSet(envs: Set[EnvRef]):
      def show(using Context) = envs.map(_.show).mkString("[", ",", "]")

      def lookupSymbol(sym: Symbol)(using EnvMap.EnvMapMutableData): Value = envs.map(_.valValue(sym)).join

      def joinThisV(using EnvMap.EnvMapMutableData): ThisValue = envs.map(_.thisV).join.asInstanceOf[ThisValue]

      def joinOuterEnvs(using EnvMap.EnvMapMutableData): EnvSet = envs.map(_.outerEnvs).join

    val NoEnv = EnvSet(Set.empty)

    def valValue(x: Symbol)(using env: EnvRef, ctx: Context, trace: Trace, envMap: EnvMap.EnvMapMutableData): Value =
      if env.hasVal(x) then
        env.valValue(x)
      else
        report.warning("[Internal error] Value not found " + x.show + "\nenv = " + env.show + ". " + Trace.show, Trace.position)
        Bottom

    /** The method of creating an Env that evaluates `tree` */
    private[Env] def _of(argMap: Map[Symbol, Value], tree: Tree, thisV: ThisValue, outerEnv: EnvSet)
                        (using State.Data, EnvMap.EnvMapMutableData, Trace): EnvRef =
      val env = EnvRef(tree, State.currentObject)
      argMap.foreach(env.initVal(_, _))
      env.initThisV(thisV)
      env.initOuterEnvs(outerEnv)
      env

    /**
     * Creates an environment that evaluates the body of a method or the body of a closure
     */
    def ofDefDef(ddef: DefDef, args: List[Value], thisV: ThisValue, outerEnv: EnvSet)
                (using State.Data, EnvMap.EnvMapMutableData, Trace): EnvRef =
      val params = ddef.termParamss.flatten.map(_.symbol)
      assert(args.size == params.size, "arguments = " + args.size + ", params = " + params.size)
      // assert(ddef.symbol.owner.is(Method) ^ (outerEnv == NoEnv), "ddef.owner = " + ddef.symbol.owner.show + ", outerEnv = " + outerEnv + ", " + ddef.source)
      _of(params.zip(args).toMap, ddef, thisV, outerEnv)


    /**
     * Creates an environment that evaluates a lazy val with `tree` as rhs
     * or evaluates a by-name parameter where `tree` is the argument tree
     */
    def ofByName(sym: Symbol, tree: Tree, thisV: ThisValue, outerEnv: EnvSet)
                (using State.Data, EnvMap.EnvMapMutableData, Trace): EnvRef =
      assert((sym.is(Flags.Param) && sym.info.isInstanceOf[ExprType]) || sym.is(Flags.Lazy));
      _of(Map.empty, tree, thisV, outerEnv)

    /**
     * The main procedure for searching through the outer chain
     * @param target The symbol to search for if `bySymbol = true`; otherwise the method symbol of the target environment
     * @param envSet The set of scopes as starting point
     * @return The scopes that contains symbol `target` or whose method is `target`,
     *         and the value for `C.this` where C is the enclosing class of the result scopes
     */
    private[Env] def resolveEnvRecur(
        target: Symbol, envSet: EnvSet, bySymbol: Boolean = true)
        : Contextual[Option[EnvSet]] = log("Resolving environment, target = " + target + ", envSet = " + envSet, printer) {
      if envSet == Env.NoEnv then None
      else
        val filter =
          if bySymbol then
            envSet.envs.filter(_.hasVal(target))
          else
            envSet.envs.filter(env => env.tree.isInstanceOf[DefDef] && env.tree.asInstanceOf[DefDef].symbol == target)

        assert(filter.isEmpty || filter.size == envSet.envs.size, "Either all scopes or no scopes contain " + target)
        if (!filter.isEmpty) then
          val resultSet = EnvSet(filter)
          Some(resultSet)
        else
          val outerEnvs = envSet.joinOuterEnvs
          if outerEnvs != NoEnv then // Search for the outerEnvs of the current envSet
            resolveEnvRecur(target, outerEnvs, bySymbol)
          else
            // Search through the outerEnvs of the instances represented by `this`
            // in case that `target` is in outer methods separated by local class definitions
            // See `tests/init-global/warn/local-class.scala`
            val thisV = envSet.joinThisV
            val outerEnvsOfThis = thisV match {
              case ref: Ref => ref.outerEnv
              case refSet: RefSet => refSet.joinOuterEnvs
            }
            resolveEnvRecur(target, outerEnvsOfThis, bySymbol)
    }

    def setLocalVal(x: Symbol, value: Value)(using scope: Scope, ctx: Context, heap: Heap.MutableData, envMap: EnvMap.EnvMapMutableData): Unit =
      assert(!x.isOneOf(Flags.Param | Flags.Mutable), "Only local immutable variable allowed")
      scope match
      case env: EnvRef =>
        env.initVal(x, value)
      case ref: Ref =>
        ref.initVal(x, value) // This is possible for match statement in class body.

    def setLocalVar(x: Symbol, value: Value)(using scope: Scope, ctx: Context, heap: Heap.MutableData, envMap: EnvMap.EnvMapMutableData): Unit =
      assert(x.is(Flags.Mutable, butNot = Flags.Param), "Only local mutable variable allowed")
      scope match
      case env: EnvRef =>
        env.initVar(x, value)
      case ref: Ref =>
        ref.initVar(x, value) // This is possible for match statement in class body.

    /**
     * Resolve the environment by searching for a given symbol.
     *
     * Searches for the environment that defines `target`, starting from `env` as the innermost.
     *
     * @param target The symbol to search for.
     * @param thisV  The value for `this` of the enclosing class where the local variable is referenced.
     * @param scope  The scope where the local variable is referenced.
     *
     * @return the environment that owns the `target`.
     */
    def resolveEnvByValue(target: Symbol, thisV: ThisValue, scope: Scope): Contextual[Option[EnvSet]] = log("Resolving env by value for " + target.show + ", this = " + thisV.show + ", scope = " + scope.show, printer) {
      val currentEnv = scope match {
        case ref: Ref => ref.outerEnv
        case env: Env.EnvRef => EnvSet(Set(env))
      }
      resolveEnvRecur(target, currentEnv)
    }

    /**
     * Resolve the environment associated by the given method `enclosing`, starting from `env` as the innermost.
     *
     * The method could be located in outer scope with intermixed classes between its definition
     * site and usage site.
     *
     * @param enclosing The method which owns the environment. This method is called to look up the environment
     *                  owned by the enclosing method of some symbol.
     * @param thisV     The value for `this` of the enclosing class where the local variable is referenced.
     * @param scope     The scope where the local variable is referenced.
     *
     * @return the environment whose symbol == `enclosing`.
     */
    def resolveEnvByMethod(enclosing: Symbol, thisV: ThisValue, scope: Scope): Contextual[EnvSet] = log("Resolving env which corresponds to method " + enclosing.show + ", this = " + thisV.show + ", scope = " + scope.show, printer) {
      assert(enclosing.is(Flags.Method), "Only method symbols allows, got " + enclosing.show)
      val currentEnv = scope match {
        case ref: Ref => ref.outerEnv
        case env: Env.EnvRef => EnvSet(Set(env))
      }
      val result = resolveEnvRecur(enclosing, currentEnv, bySymbol = false)
      assert(!result.isEmpty, "Failed to find environment for " + enclosing + "!")
      result.get
    }

    def withEnv[T](env: EnvRef)(fn: EnvRef ?=> T): T = fn(using env)
  end Env

  /** Abstract heap for mutable fields
   */
  object Heap:
    private case class InstanceBody(
      valsMap: Map[Symbol, Value],
      outersMap: Map[Symbol, Value],
      outerEnvs: Env.EnvSet
    )

    private def emptyInstanceBody(): InstanceBody = InstanceBody(
      valsMap = Map.empty,
      outersMap = Map.empty,
      outerEnvs = Env.NoEnv
    )

    /** Immutable heap data used in the cache.
     *
     *  We need to use structural equivalence so that in different iterations the cache can be effective.
     *
     *  TODO: speed up equality check for heap.
     */
    opaque type Data = Map[Ref, InstanceBody]

    /** Store the heap as a mutable field to avoid threading it through the program. */
    class MutableData(private[Heap] var heap: Data):
      private[Heap] def writeJoinVal(ref: Ref, valSymbol: Symbol, value: Value): Unit =
        heap.get(ref) match
        case None =>
          heap = heap.updated(ref, Heap.emptyInstanceBody())
          writeJoinVal(ref, valSymbol, value)

        case Some(current) =>
          val newValsMap = current.valsMap.join(valSymbol, value)
          heap = heap.updated(ref, new InstanceBody(
            valsMap = newValsMap,
            outersMap = current.outersMap,
            outerEnvs = current.outerEnvs
          ))

      private[Heap] def writeJoinOuter(ref: Ref, parentSymbol: Symbol, outers: Value): Unit =
        heap.get(ref) match
        case None =>
          heap = heap.updated(ref, Heap.emptyInstanceBody())
          writeJoinOuter(ref, parentSymbol, outers)

        case Some(current) =>
          val newOutersMap = current.outersMap.join(parentSymbol, outers)
          heap = heap.updated(ref, new InstanceBody(
            valsMap = current.valsMap,
            outersMap = newOutersMap,
            outerEnvs = current.outerEnvs
          ))

      private[Heap] def writeJoinOuterEnv(ref: Ref, outerEnvs: Env.EnvSet): Unit =
        heap.get(ref) match
        case None =>
          heap = heap.updated(ref, Heap.emptyInstanceBody())
          writeJoinOuterEnv(ref, outerEnvs)

        case Some(current) =>
          val newOuterEnvs = current.outerEnvs.join(outerEnvs)
          heap = heap.updated(ref, new InstanceBody(
            valsMap = current.valsMap,
            outersMap = current.outersMap,
            outerEnvs = newOuterEnvs
          ))
    end MutableData

    def empty: MutableData = new MutableData(Map.empty)

    def contains(ref: Ref)(using mutable: MutableData): Boolean =
      mutable.heap.contains(ref)

    def containsVal(ref: Ref, value: Symbol)(using mutable: MutableData): Boolean =
      if mutable.heap.contains(ref) then
        mutable.heap(ref).valsMap.contains(value)
      else
        false

    def readVal(ref: Ref, value: Symbol)(using mutable: MutableData): Value =
      mutable.heap(ref).valsMap(value)

    def readOuter(ref: Ref, parent: Symbol)(using mutable: MutableData): Value =
      mutable.heap(ref).outersMap(parent)

    def hasOuter(ref: Ref, parent: Symbol)(using mutable: MutableData): Boolean =
      mutable.heap(ref).outersMap.contains(parent)

    def readOuterEnv(ref: Ref)(using mutable: MutableData): Env.EnvSet =
      mutable.heap(ref).outerEnvs

    def writeJoinVal(ref: Ref, valSymbol: Symbol, value: Value)(using mutable: MutableData): Unit =
      mutable.writeJoinVal(ref, valSymbol, value)

    def writeJoinOuter(ref: Ref, outer: Symbol, outers: Value)(using mutable: MutableData): Unit =
      mutable.writeJoinOuter(ref, outer, outers)

    def writeJoinOuterEnv(ref: Ref, outerEnvs: Env.EnvSet)(using mutable: MutableData): Unit =
      mutable.writeJoinOuterEnv(ref, outerEnvs)

    def getHeapData()(using mutable: MutableData): Data = mutable.heap

    def setHeap(newHeap: Data)(using mutable: MutableData): Unit = mutable.heap = newHeap
  end Heap

  /**
     *  Local environments can be deeply nested, therefore we need `outerEnvs`, which stores the immediate outer environment.
     *  If the immediate enclosing scope of an environment is a template, then `outerEnvs` is empty in EnvMap.
     *  We can restore outerEnvs of `this` in the heap.
     */
  object EnvMap:
    private case class EnvBody(
      valsMap: Map[Symbol, Value],
      thisV: ThisValue,
      outerEnvs: Env.EnvSet
    )

    private def emptyEnvBody(): EnvBody = EnvBody(
      valsMap = Map.empty,
      thisV = Bottom,
      outerEnvs = Env.NoEnv
    )

    /** Immutable env map data used in the cache.
     *
     *  We need to use structural equivalence so that in different iterations the cache can be effective.
     */
    opaque type Data = Map[Env.EnvRef, EnvBody]

    /** Store the heap as a mutable field to avoid threading it through the program. */
    class EnvMapMutableData(private[EnvMap] var envMap: Data):
      private[EnvMap] def writeJoinVal(env: Env.EnvRef, valSymbol: Symbol, value: Value): Unit =
        envMap.get(env) match
        case None =>
          envMap = envMap.updated(env, EnvMap.emptyEnvBody())
          writeJoinVal(env, valSymbol, value)

        case Some(current) =>
          val newValsMap = current.valsMap.join(valSymbol, value)
          envMap = envMap.updated(env, new EnvBody(
            valsMap = newValsMap,
            thisV = current.thisV,
            outerEnvs = current.outerEnvs
          ))

      private[EnvMap] def writeJoinThisV(env: Env.EnvRef, thisV: ThisValue): Unit =
        envMap.get(env) match
        case None =>
          envMap = envMap.updated(env, EnvMap.emptyEnvBody())
          writeJoinThisV(env, thisV)

        case Some(current) =>
          val newThisV = current.thisV.join(thisV).asInstanceOf[ThisValue]
          envMap = envMap.updated(env, new EnvBody(
            valsMap = current.valsMap,
            thisV = newThisV,
            outerEnvs = current.outerEnvs
          ))

      private[EnvMap] def writeJoinOuterEnv(env: Env.EnvRef, outerEnvs: Env.EnvSet): Unit =
        envMap.get(env) match
        case None =>
          envMap = envMap.updated(env, EnvMap.emptyEnvBody())
          writeJoinOuterEnv(env, outerEnvs)

        case Some(current) =>
          val newOuterEnvs = current.outerEnvs.join(outerEnvs)
          envMap = envMap.updated(env, new EnvBody(
            valsMap = current.valsMap,
            thisV = current.thisV,
            outerEnvs = newOuterEnvs
          ))
    end EnvMapMutableData

    def empty: EnvMapMutableData = new EnvMapMutableData(Map.empty)

    def contains(env: Env.EnvRef)(using mutable: EnvMapMutableData): Boolean =
      mutable.envMap.contains(env)

    def containsVal(env: Env.EnvRef, value: Symbol)(using mutable: EnvMapMutableData): Boolean =
      if mutable.envMap.contains(env) then
        mutable.envMap(env).valsMap.contains(value)
      else
        false

    def readVal(env: Env.EnvRef, value: Symbol)(using mutable: EnvMapMutableData): Value =
      mutable.envMap(env).valsMap(value)

    def getThisV(env: Env.EnvRef)(using mutable: EnvMapMutableData): ThisValue =
      mutable.envMap(env).thisV

    def getOuterEnvs(env: Env.EnvRef)(using mutable: EnvMapMutableData): Env.EnvSet =
      mutable.envMap(env).outerEnvs

    def writeJoinVal(env: Env.EnvRef, valSymbol: Symbol, value: Value)(using mutable: EnvMapMutableData): Unit =
      mutable.writeJoinVal(env, valSymbol, value)

    def writeJoinThisV(env: Env.EnvRef, thisV: ThisValue)(using mutable: EnvMapMutableData): Unit =
      mutable.writeJoinThisV(env, thisV)

    def writeJoinOuterEnv(env: Env.EnvRef, outerEnvs: Env.EnvSet)(using mutable: EnvMapMutableData): Unit =
      mutable.writeJoinOuterEnv(env, outerEnvs)

    def getEnvMapData()(using mutable: EnvMapMutableData): Data = mutable.envMap

    def setEnvMap(newEnvMap: Data)(using mutable: EnvMapMutableData): Unit = mutable.envMap = newEnvMap
  end EnvMap

  /** Cache used to terminate the check  */
  object Cache:
    case class Config(thisV: Value, scope: Scope, heap: Heap.Data, envMap: EnvMap.Data)
    case class Res(value: Value, heap: Heap.Data, envMap: EnvMap.Data)

    class Data extends Cache[Config, Res]:
      def get(thisV: Value, expr: Tree)(using Heap.MutableData, Scope, EnvMap.EnvMapMutableData): Option[Value] =
        val config = Config(thisV, summon[Scope], Heap.getHeapData(), EnvMap.getEnvMapData())
        super.get(config, expr).map(_.value)

      def cachedEval(thisV: ThisValue, expr: Tree, cacheResult: Boolean)(fun: Tree => Value)(using Heap.MutableData, Scope, EnvMap.EnvMapMutableData): Value =
        val config = Config(thisV, summon[Scope], Heap.getHeapData(), EnvMap.getEnvMapData())
        val result = super.cachedEval(config, expr, cacheResult, default = Res(Bottom, Heap.getHeapData(), EnvMap.getEnvMapData())) { expr =>
          Res(fun(expr), Heap.getHeapData(), EnvMap.getEnvMapData())
        }
        Heap.setHeap(result.heap)
        EnvMap.setEnvMap(result.envMap)
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


  /**
   * Handle return statements in methods and non-local returns in functions.
   */
  object Returns:
    private class ReturnData(val method: Symbol, val values: mutable.ArrayBuffer[Value])
    opaque type Data = mutable.ArrayBuffer[ReturnData]

    def empty(): Data = mutable.ArrayBuffer()

    def installHandler(meth: Symbol)(using data: Data): Unit =
      data.addOne(ReturnData(meth, mutable.ArrayBuffer()))

    def popHandler(meth: Symbol)(using data: Data): Value =
      val returnData = data.remove(data.size - 1)
      assert(returnData.method == meth, "Symbol mismatch in return handlers, expect = " + meth + ", found = " + returnData.method)
      returnData.values.join

    def handle(meth: Symbol, value: Value)(using data: Data, trace: Trace, ctx: Context): Unit =
      data.findLast(_.method == meth) match
        case Some(returnData) =>
          returnData.values.addOne(value)

        case None =>
          report.warning("[Internal error] Unhandled return for method " + meth + " in " + meth.owner.show + ". Trace:\n" + Trace.show, Trace.position)

  type Contextual[T] = (Context, State.Data, Scope, Cache.Data, Heap.MutableData, EnvMap.EnvMapMutableData, Regions.Data, Returns.Data, Trace) ?=> T

  // --------------------------- domain operations -----------------------------

  case class ArgInfo(value: Value, trace: Trace, tree: Tree)

  trait Join[V]:
    extension (v1: V)
      def join(v2: V): V

  given Join[Value] with
    extension (a: Value)
      def join(b: Value): Value =
        assert(!a.isInstanceOf[Package] && !b.isInstanceOf[Package], "Unexpected join between " + a + " and " + b)
        (a, b) match
        case (Bottom, b)                            => b
        case (a, Bottom)                            => a
        // ThisValue join ThisValue => ThisValue
        case (refSet1: RefSet, refSet2: RefSet)     => new RefSet(refSet1.refs ++ refSet2.refs)
        case (ref: Ref, refSet: RefSet)             => new RefSet(refSet.refs + ref)
        case (refSet: RefSet, ref: Ref)             => new RefSet(refSet.refs + ref)
        case (ref1: Ref, ref2: Ref)                 => new RefSet(Set(ref1, ref2))
        case (ValueSet(values1), ValueSet(values2)) => ValueSet(values1 ++ values2)
        case (a : ValueElement, ValueSet(values))   => ValueSet(values + a)
        case (ValueSet(values), b : ValueElement)   => ValueSet(values + b)
        case (a : ValueElement, b : ValueElement)   => ValueSet(Set(a, b))
        case _                                      => Bottom

  extension (a: Value)
    def remove(b: Value): Value = (a, b) match
      case (ValueSet(values1), b: ValueElement)   => ValueSet(values1 - b)
      case (ValueSet(values1), ValueSet(values2)) => ValueSet(values1.removedAll(values2))
      case (a: Ref, b: Ref) if a.equals(b)        => Bottom
      case (a: SafeValue, b: SafeValue) if a == b => Bottom
      case (a: Package, b: Package) if a == b     => Bottom
      case _ => a

    def filterType(tpe: Type)(using Context): Value =
      tpe match
        case t @ SAMType(_, _) if a.isInstanceOf[Fun] => a // if tpe is SAMType and a is Fun, allow it
        case _ =>
          val baseClasses = tpe.baseClasses
          if baseClasses.isEmpty then a
          else filterClass(baseClasses.head) // could have called ClassSymbol, but it does not handle OrType and AndType

    // Filter the value according to a class symbol, and only leaves the sub-values
    // which could represent an object of the given class
    def filterClass(sym: Symbol)(using Context): Value =
      if !sym.isClass then a
      else
        val klass = sym.asClass
        a match
          case UnknownValue => a
          case Package(packageModuleClass) =>
            // the typer might mistakenly set the receiver to be a package instead of package object.
            // See pos/packageObjectStringInterpolator.scala
            if packageModuleClass == klass || (klass.denot.isPackageObject && klass.owner == packageModuleClass) then a else Bottom
          case v: SafeValue => if v.typeSymbol.asClass.isSubClass(klass) && v.typeSymbol.asClass != defn.NullClass then a else Bottom
          case ref: Ref => if ref.klass.isSubClass(klass) then ref else Bottom
          case ValueSet(values) => values.map(v => v.filterClass(klass)).join
          case fun: Fun =>
            if klass.isOneOf(AbstractOrTrait) && klass.baseClasses.exists(defn.isFunctionClass) then fun else Bottom

  extension (thisV: ThisValue)
    def toValueSet: ValueSet = thisV match
      case ref: Ref => ValueSet(Set(ref))
      case vs: ValueSet => vs

  given Join[Env.EnvSet] with
    extension (a: Env.EnvSet)
      def join(b: Env.EnvSet): Env.EnvSet = Env.EnvSet(a.envs ++ b.envs)

  extension (values: Iterable[Value])
    def join: Value =
      if values.isEmpty then
        Bottom
      else
        values.reduce { (v1, v2) => v1.join(v2) }

  extension (scopes: Iterable[Env.EnvSet])
    def join: Env.EnvSet =
      if scopes.isEmpty then
        Env.NoEnv
      else
        scopes.reduce { (s1, s2) => s1.join(s2) }

  extension [V : Join](map: Map[Symbol, V])
    def join(sym: Symbol, value: V): Map[Symbol, V] =
      if !map.contains(sym) then map.updated(sym, value)
      else map.updated(sym, map(sym).join(value))

  def reportWarningForUnknownValue(msg: => String, pos: SrcPos)(using Context): Value =
    if BestEffort then
      Bottom
    else
      report.warning(msg, pos)
      UnknownValue

  /** Handle method calls `e.m(args)`.
   *
   * @param value        The value for the receiver.
   * @param meth         The symbol of the target method (could be virtual or abstract method).
   * @param args         Arguments of the method call (all parameter blocks flatten to a list).
   * @param receiver     The type of the receiver.
   * @param superType    The type of the super in a super call. NoType for non-super calls.
   * @param needResolve  Whether the target of the call needs resolution?
   */
  def call(value: Value, meth: Symbol, args: List[ArgInfo], receiver: Type, superType: Type, needResolve: Boolean = true): Contextual[Value] = log("call " + meth.show + ", this = " + value.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    value.filterClass(meth.owner) match
    case UnknownValue =>
      reportWarningForUnknownValue("Using unknown value. " + Trace.show, Trace.position)

    case Package(packageModuleClass) =>
      if meth.equals(defn.throwMethod) then
        Bottom
      else if meth.owner.denot.isPackageObject then
        // calls on packages are unexpected. However the typer might mistakenly
        // set the receiver to be a package instead of package object.
        // See packageObjectStringInterpolator.scala
        // Method call on package object instead
        val packageObj = accessObject(meth.owner.moduleClass.asClass)
        call(packageObj, meth, args, receiver, superType, needResolve)
      else
        report.warning("[Internal error] Unexpected call on package = " + value.show + ", meth = " + meth.show + Trace.show, Trace.position)
        Bottom

    case v @ SafeValue(_) =>
      if v.typeSymbol == defn.NullClass then
        // call on Null is sensible on AST level but not in practice
        Bottom
      else
        // Assume such method is pure. Check return type, only try to analyze body if return type is not safe
        val target = resolve(v.typeSymbol.asClass, meth)
        val targetType = target.denot.info
        val returnType = targetType.finalResultType
        val typeSymbol = SafeValue.getSafeTypeSymbol(returnType)
        if typeSymbol.isDefined then
          // since method is pure and return type is safe, no need to analyze method body
          SafeValue(typeSymbol.get)
        else if !target.hasSource then
          UnknownValue
        else
          val ddef = target.defTree.asInstanceOf[DefDef]
          val cls = target.owner.enclosingClass.asClass
          // convert SafeType to an InstanceRef before analyzing method body
          val ref = InstanceRef(cls, Bottom, Env.NoEnv, NoSymbol)
          call(ref, meth, args, receiver, superType, needResolve)

    case Bottom =>
      Bottom

    // Bottom arguments mean unreachable call
    case _ if args.map(_.value).contains(Bottom) =>
      Bottom

    case arr: ArrayRef =>
      val target = resolve(defn.ArrayClass, meth)

      if target == defn.Array_apply || target == defn.Array_clone then
        if arr.owner == State.currentObject then
          arr.readElement
        else
          errorReadOtherStaticObject(State.currentObject, arr)
          Bottom
      else if target == defn.Array_update then
        assert(args.size == 2, "Incorrect number of arguments for Array update, found = " + args.size)
        if arr.owner != State.currentObject then
          errorMutateOtherStaticObject(State.currentObject, arr)
        else
          arr.writeElement(args.tail.head.value)
        Bottom
      else
        // Array.length is OK
        SafeValue(defn.IntType)

    case ref: Ref =>
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
        if target.owner == defn.ArrayModuleClass && target.name == nme.apply then
          val arr = ArrayRef(State.currentObject, summon[Regions.Data])
          arr.writeElement(args.map(_.value).join)
          arr
        else if target.equals(defn.Predef_classOf) then
          // Predef.classOf is a stub method in tasty and is replaced in backend
          UnknownValue
        else if target.hasSource then
          val cls = target.owner.enclosingClass.asClass
          val ddef = target.defTree.asInstanceOf[DefDef]
          val meth = ddef.symbol
          val (thisV : ThisValue, outerEnv) =
            if meth.owner.enclosingMethod == cls.primaryConstructor then
              // meth is top-level method
              (ref, Env.NoEnv)
            else
              val enclosingMethod = meth.owner.enclosingMethod
              val outerEnvs = Env.resolveEnvByMethod(enclosingMethod, ref, summon[Scope])
              (outerEnvs.joinThisV, outerEnvs)

          val env2 = Env.ofDefDef(ddef, args.map(_.value), thisV, outerEnv)
          extendTrace(ddef) {
            State.recordCall()
            given Scope = env2
            cache.cachedEval(ref, ddef.rhs, cacheResult = true) { expr =>
              Returns.installHandler(meth)
              val res = cases(expr, thisV, cls)
              val returns = Returns.popHandler(meth)
              res.join(returns)
            }
          }
        else
          UnknownValue
      else if target.exists then
        select(ref, target, receiver, needResolve = false)
      else
        if ref.klass.isSubClass(receiver.widenSingleton.classSymbol) then
          report.warning("[Internal error] Unexpected resolution failure: ref.klass = " + ref.klass.show + ", meth = " + meth.show + Trace.show, Trace.position)
          Bottom
        else
          // This is possible due to incorrect type cast.
          // See tests/init/pos/Type.scala
          Bottom

    case Fun(code, thisVOfClosure, klass, scope) =>
      // meth == NoSymbol for poly functions
      if meth.name == nme.tupled then
        value // a call like `fun.tupled`
      else
        code match
        case ddef: DefDef =>
          if meth.name == nme.apply then
            val funEnv = scope match {
              case ref: Ref => Env.ofDefDef(ddef, args.map(_.value), thisVOfClosure, Env.NoEnv)
              case env: Env.EnvRef => Env.ofDefDef(ddef, args.map(_.value), thisVOfClosure, Env.EnvSet(Set(env)))
            }
            given Scope = funEnv
            extendTrace(code) {
              State.recordCall()
              eval(ddef.rhs, thisVOfClosure, klass, cacheResult = true)
            }
          else
            // The methods defined in `Any` and `AnyRef` are trivial and don't affect initialization.
            if meth.owner == defn.AnyClass || meth.owner == defn.ObjectClass then
              value
            else
              // In future, we will have Tasty for stdlib classes and can abstractly interpret that Tasty.
              // For now, return `UnknownValue` to ensure soundness and trigger a warning when BestEffort = false.
              UnknownValue
            end if
          end if

        case _ =>
          // Should be unreachable, by-name closures are handled by readLocal
          report.warning("[Internal error] Only DefDef should be possible here, but found " + code.show + ". " + Trace.show, Trace.position)
          Bottom

    case ValueSet(vs) =>
      vs.map(v => call(v, meth, args, receiver, superType)).join
  }

  /** Handle constructor calls `<init>(args)`.
   *
   * @param value        The value for the receiver.
   * @param ctor         The symbol of the target method.
   * @param args         Arguments of the constructor call (all parameter blocks flatten to a list).
   */
  def callConstructor(value: Value, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("call " + ctor.show + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    value match
    case ref: Ref =>
      if ctor.hasSource then
        val cls = ctor.owner.enclosingClass.asClass
        val ddef = ctor.defTree.asInstanceOf[DefDef]

        given Scope = ref
        if ctor.isPrimaryConstructor then
          val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
          val params = tpl.constr.termParamss.flatten.map(_.symbol)
          val paramMap = params.zip(args.map(_.value))
          paramMap.foreach(ref.initVal(_, _))
          extendTrace(cls.defTree) { eval(tpl, ref, cls, cacheResult = true) }
        else
          extendTrace(ddef) { // The return values for secondary constructors can be ignored
            State.recordCall()
            Returns.installHandler(ctor)
            eval(ddef.rhs, ref, cls, cacheResult = true)
            Returns.popHandler(ctor)
            value
          }
      else
        // no source code available
        UnknownValue

    case ValueSet(values) if values.size == 1 =>
      callConstructor(values.head, ctor, args)

    case _ =>
      report.warning("[Internal error] unexpected constructor call, meth = " + ctor + ", this = " + value + Trace.show, Trace.position)
      Bottom
  }

  /** Handle selection `e.f`.
   *
   * @param value        The value for the receiver.
   * @param field        The symbol of the target field (could be virtual or abstract).
   * @param receiver     The type of the receiver.
   * @param needResolve  Whether the target of the selection needs resolution?
   */
  def select(value: Value, field: Symbol, receiver: Type, needResolve: Boolean = true): Contextual[Value] = log("select " + field.show + ", this = " + value.show, printer, (_: Value).show) {
    value.filterClass(field.owner) match
    case UnknownValue =>
      reportWarningForUnknownValue("Using unknown value. " + Trace.show, Trace.position)

    case arr: ArrayRef =>
      report.warning("[Internal error] unexpected tree in selecting an array, array = " + arr.show + Trace.show, Trace.position)
      Bottom

    case v @ SafeValue(_) =>
      if v.typeSymbol != defn.NullClass then
        // selection on Null is sensible on AST level; no warning for it
        report.warning("[Internal error] Unexpected selection on safe value " + v.show + ", field = " + field.show + ". " + Trace.show, Trace.position)
      end if
      Bottom

    case Package(packageModuleClass) =>
      if field.isStaticObject then
        accessObject(field.moduleClass.asClass)
      else if field.is(Flags.Package) then
        Package(field)
      else
        report.warning("[Internal error] Unexpected selection on package " + packageModuleClass.show + ", field = " + field.show + Trace.show, Trace.position)
        Bottom

    case ref: Ref =>
      val target = if needResolve then resolve(ref.klass, field) else field
      if target.is(Flags.Lazy) then // select a lazy field
        if ref.hasVal(target) then
          ref.valValue(target)

        else if target.hasSource then
          val rhs = target.defTree.asInstanceOf[ValDef].rhs
          given Scope = Env.ofByName(target, rhs, ref, Env.NoEnv)
          val result = eval(rhs, ref, target.owner.asClass, cacheResult = true)
          ref.initVal(target, result)
          result

        else
          UnknownValue

      else if target.exists then
        def isNextFieldOfColonColon: Boolean = ref.klass == defn.ConsClass && target.name.toString == "next"
        if target.isMutableVarOrAccessor && !isNextFieldOfColonColon then
          if ref.hasVar(target) then
            if ref.owner == State.currentObject then
              ref.varValue(target)

            else
              errorReadOtherStaticObject(State.currentObject, ref)
              Bottom

          else if ref.isObjectRef && ref.klass.hasSource then
            errorReadUninitializedField(ref.asObjectRef, field)
            Bottom

          else
            // initialization error, reported by the initialization checker
            Bottom

        else if ref.hasVal(target) then
          ref.valValue(target)

        else if ref.isObjectRef && ref.klass.hasSource then
          errorReadUninitializedField(ref.asObjectRef, field)
          Bottom

        else
          // initialization error, reported by the initialization checker
          Bottom

      else
        if ref.klass.isSubClass(receiver.widenSingleton.classSymbol) then
          report.warning("[Internal error] Unexpected resolution failure: ref.klass = " + ref.klass.show + ", field = " + field.show + Trace.show, Trace.position)
          Bottom
        else
          // This is possible due to incorrect type cast or accessing standard library objects
          // See tests/init/pos/Type.scala / tests/init/warn/unapplySeq-implicit-arg2.scala
          UnknownValue

    case fun: Fun =>
      report.warning("[Internal error] unexpected tree in selecting a function, fun = " + fun.code.show + Trace.show, fun.code)
      Bottom

    case Bottom => Bottom

    case ValueSet(values) =>
      values.map(ref => select(ref, field, receiver)).join
  }

  /** Handle assignment `lhs.f = rhs`.
   *
   * @param lhs         The value of the object to be mutated.
   * @param field       The symbol of the target field.
   * @param rhs         The value to be assigned.
   * @param rhsTyp      The type of the right-hand side.
   */
  def assign(lhs: Value, field: Symbol, rhs: Value, rhsTyp: Type): Contextual[Value] = log("Assign" + field.show + " of " + lhs.show + ", rhs = " + rhs.show, printer, (_: Value).show) {
    lhs.filterClass(field.owner) match
    case UnknownValue =>
      val _ = reportWarningForUnknownValue("Assigning to unknown value. " + Trace.show, Trace.position)
    case p: Package =>
      report.warning("[Internal error] unexpected tree in assignment, package = " + p.show + Trace.show, Trace.position)
    case fun: Fun =>
      report.warning("[Internal error] unexpected tree in assignment, fun = " + fun.code.show + Trace.show, Trace.position)
    case arr: ArrayRef =>
      report.warning("[Internal error] unexpected tree in assignment, array = " + arr.show + " field = " + field + Trace.show, Trace.position)

    case SafeValue(_) =>
      report.warning("Assigning to base value is forbidden. " + Trace.show, Trace.position)

    case ValueSet(values) =>
      values.foreach(ref => assign(ref, field, rhs, rhsTyp))

    case ref: Ref =>
      if ref.hasVar(field) then
        if ref.owner != State.currentObject then
          errorMutateOtherStaticObject(State.currentObject, ref)
        else
          Heap.writeJoinVal(ref, field, rhs)
      else
        report.warning("Mutating a field before its initialization: " + field.show + ". " + Trace.show, Trace.position)
    end match

    Bottom
  }

  /**
   * Handle new expression `new p.C(args)`.
   * The actual instance might be cached without running the constructor.
   * See tests/init-global/pos/cache-constructor.scala
   *
   * @param outer       The value for `p`.
   * @param klass       The symbol of the class `C`.
   * @param ctor        The symbol of the target constructor.
   * @param args        The arguments passsed to the constructor.
   */
  def instantiate(outer: Value, klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = log("instantiating " + klass.show + ", outer = " + outer + ", args = " + args.map(_.value.show), printer, (_: Value).show) {
    outer.filterClass(klass.owner) match
    case _ : Fun | _: ArrayRef | SafeValue(_)  =>
      report.warning("[Internal error] unexpected outer in instantiating a class, outer = " + outer.show + ", class = " + klass.show + ", " + Trace.show, Trace.position)
      Bottom

    case UnknownValue =>
      reportWarningForUnknownValue("Instantiating when outer is unknown. " + Trace.show, Trace.position)

    case outer: (Ref | Package) =>
      if klass == defn.ArrayClass then
        args.head.tree.tpe match
          case ConstantType(Constants.Constant(0)) =>
            // new Array(0)
            Bottom
          case _ =>
            val arr = ArrayRef(State.currentObject, summon[Regions.Data])
            arr
      else
        val (outerThis, outerEnv) =
          outer match
            case Package(_) => // For top-level classes
              (Bottom, Env.NoEnv)
            case outer : ThisValue =>
              if klass.owner.is(Flags.Package) then
                report.warning("[Internal error] top-level class should have `Package` as outer, class = " + klass.show + ", outer = " + outer.show + ", " + Trace.show, Trace.position)
                (Bottom, Env.NoEnv)
              else
                // enclosingClass is specially handled for java static terms, so use `lexicallyEnclosingClass` here
                val outerCls = klass.owner.lexicallyEnclosingClass.asClass
                // When `klass` is directly nested in `outerCls`, `outerCls`.enclosingMethod returns its primary constructor
                if klass.owner.enclosingMethod == outerCls.primaryConstructor then
                  (outer, Env.NoEnv)
                else
                  val outerEnvs = Env.resolveEnvByMethod(klass.owner.enclosingMethod, outer, summon[Scope])
                  (outer, outerEnvs)

        val instance = InstanceRef(klass, outerThis, outerEnv, ctor)
        callConstructor(instance, ctor, args)

    case ValueSet(values) =>
      values.map(ref => instantiate(ref, klass, ctor, args)).join
  }

  /** Handle local variable definition, `val x = e` or `var x = e`.
   *
   * @param sym          The symbol of the variable.
   * @param value        The value of the initializer.
   */
  def initLocal(sym: Symbol, value: Value): Contextual[Unit] = log("initialize local " + sym.show + " with " + value.show, printer) {
    if sym.is(Flags.Mutable) then
      Env.setLocalVar(sym, value)
    else
      Env.setLocalVal(sym, value)
  }

  /** Read local variable `x`.
   *
   * @param thisV        The value for `this` where the variable is used.
   * @param sym          The symbol of the variable.
   */
  def readLocal(thisV: ThisValue, sym: Symbol): Contextual[Value] = log("reading local " + sym.show, printer, (_: Value).show) {
    def isByNameParam(sym: Symbol) = sym.is(Flags.Param) && sym.info.isInstanceOf[ExprType]
    def evalByNameParam(value: Value): Contextual[Value] = value match
      case Fun(code, thisV, klass, scope) =>
        val byNameEnv = scope match {
          case ref: Ref => Env.ofByName(sym, code, thisV, Env.NoEnv) // for by-name arguments of constructors
          case env: Env.EnvRef => Env.ofByName(sym, code, thisV, Env.EnvSet(Set(env))) // for by-name arguments of methods/functions
        }
        given Scope = byNameEnv
        eval(code, thisV, klass, cacheResult = true)
      case UnknownValue =>
        reportWarningForUnknownValue("Calling on unknown value. " + Trace.show, Trace.position)
      case Bottom => Bottom
      case ValueSet(values) =>
        values.map(evalByNameParam(_)).join
      case _: Ref | _: ArrayRef | _: Package | SafeValue(_) =>
        report.warning("[Internal error] Unexpected by-name value " + value.show  + ". " + Trace.show, Trace.position)
        Bottom
    end evalByNameParam

    // Can't use enclosingMethod here because values defined in a by-name closure will have the wrong enclosingMethod,
    // since our phase is before elimByName.
    Env.resolveEnvByValue(sym, thisV, summon[Scope]) match
    case Some(envSet) =>
      if sym.is(Flags.Mutable) then
        // Assume forward reference check is doing a good job
        val envsOwnedByOthers = envSet.envs.filter(_.owner != State.currentObject)
        if envsOwnedByOthers.isEmpty then
          envSet.lookupSymbol(sym)
        else
          errorReadOtherStaticObject(State.currentObject, envsOwnedByOthers.head)
          Bottom
        end if
      else
        if sym.is(Flags.Lazy) then
          val outerThis = envSet.joinThisV
          given Scope = Env.ofByName(sym, sym.defTree, outerThis, envSet)
          val rhs = sym.defTree.asInstanceOf[ValDef].rhs
          eval(rhs, outerThis, sym.enclosingClass.asClass, cacheResult = true)
        else
          // Assume forward reference check is doing a good job
          val value = envSet.lookupSymbol(sym)
          if isByNameParam(sym) then
            evalByNameParam(value)
          else
            value

    case None =>
      if isByNameParam(sym) then
        report.warning("Calling cold by-name alias. " + Trace.show, Trace.position)
        Bottom
      else
        UnknownValue
  }

  /** Handle local variable assignmenbt, `x = e`.
   *
   * @param thisV        The value for `this` where the assignment locates.
   * @param sym          The symbol of the variable.
   * @param value        The value of the rhs of the assignment.
   */
  def writeLocal(thisV: ThisValue, sym: Symbol, value: Value): Contextual[Value] = log("write local " + sym.show + " with " + value.show, printer, (_: Value).show) {
    assert(sym.is(Flags.Mutable), "Writing to immutable variable " + sym.show)
    // Can't use enclosingMethod here because values defined in a by-name closure will have the wrong enclosingMethod,
    // since our phase is before elimByName.
    Env.resolveEnvByValue(sym, thisV, summon[Scope]) match
    case Some(envSet) =>
      val envsOwnedByOthers = envSet.envs.filter(_.owner != State.currentObject)
      if !envsOwnedByOthers.isEmpty then
        errorMutateOtherStaticObject(State.currentObject, envsOwnedByOthers.head)
      else
        envSet.envs.foreach(EnvMap.writeJoinVal(_, sym, value))

    case _ =>
      report.warning("Assigning to variables in outer scope. " + Trace.show, Trace.position)

    Bottom
  }

  // -------------------------------- algorithm --------------------------------

  /** Check an individual object */
  private def accessObject(classSym: ClassSymbol)(using Context, State.Data, Trace, Heap.MutableData, EnvMap.EnvMapMutableData): ObjectRef = log("accessing " + classSym.show, printer, (_: Value).show) {
    if classSym.hasSource then
      val obj = State.checkObjectAccess(classSym)
      if !obj.isAfterSuperCall then
        report.warning("Accessing " + obj.klass + " before the super constructor of the object finishes! " + Trace.show, Trace.position)
      end if
      obj
    else
      val obj = ObjectRef(classSym)
      obj.setAfterSuperCall()
      obj
  }


  def checkClasses(classes: List[ClassSymbol])(using Context): Unit =
    given State.Data = new State.Data
    given Trace = Trace.empty
    given Heap.MutableData = Heap.empty // TODO: do garbage collection on the heap
    given EnvMap.EnvMapMutableData = EnvMap.empty

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
  def eval(expr: Tree, thisV: ThisValue, klass: ClassSymbol, cacheResult: Boolean = false): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + ", regions = " + Regions.show + " in " + klass.show, printer, (_: Value).show) {
    cache.cachedEval(thisV, expr, cacheResult) { expr => cases(expr, thisV, klass) }
  }


  /** Evaluate a list of expressions */
  def evalExprs(exprs: List[Tree], thisV: ThisValue, klass: ClassSymbol): Contextual[List[Value]] =
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   *
   * @param expr   The expression to be evaluated.
   * @param thisV  The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass  The enclosing class where the expression `expr` is located.
   */
  def cases(expr: Tree, thisV: ThisValue, klass: ClassSymbol): Contextual[Value] = log("evaluating " + expr.show + ", this = " + thisV.show + " in " + klass.show, printer, (_: Value).show) {
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

      case TypeCast(elem, tpe) =>
        eval(elem, thisV, klass).filterType(tpe)

      case Apply(ref, arg :: Nil) if ref.symbol == defn.InitRegionMethod =>
        val regions2 = Regions.extend(expr.sourcePos)
        if Regions.exists(expr.sourcePos) then
          report.warning("Cyclic region detected. Trace:\n" + Trace.show, expr)
          Bottom
        else
          given Regions.Data = regions2
          eval(arg, thisV, klass)

      case Call(ref, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe: @unchecked
          val thisValue2 = extendTrace(ref) {
            thisTp match
              case thisTp: ThisType =>
                evalType(thisTp, thisV, klass)
              case AndType(thisTp: ThisType, _) =>
                evalType(thisTp, thisV, klass)
              case _ =>
                report.warning("[Internal error] Unexpected type " + thisTp.show + ", trace:\n" + Trace.show, ref)
                Bottom
          }
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
            withTrace(trace2) { resolveThis(target, qual.asInstanceOf[ThisValue], klass) }
          case _ =>
            withTrace(trace2) { select(qual, expr.symbol, receiver = qualifier.tpe) }

      case _: This =>
        evalType(expr.tpe, thisV, klass)

      case Literal(const) =>
        SafeValue(const.tpe)

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
          writeLocal(thisV, lhs.symbol, value)
        else
          withTrace(trace2) { assign(receiver, lhs.symbol, value, rhs.tpe) }

      case closureDef(ddef) =>
        Fun(ddef, thisV, klass, summon[Scope])

      case PolyFun(ddef) =>
        Fun(ddef, thisV, klass, summon[Scope])

      case Block(stats, expr) =>
        evalExprs(stats, thisV, klass)
        eval(expr, thisV, klass)

      case If(cond, thenp, elsep) =>
        eval(cond, thisV, klass)
        evalExprs(thenp :: elsep :: Nil, thisV, klass).join

      case Annotated(arg, annot) =>
        if expr.tpe.hasAnnotation(defn.UncheckedAnnot) then
          Bottom
        else
          eval(arg, thisV, klass)

      case Match(scrutinee, cases) =>
        val scrutineeValue = eval(scrutinee, thisV, klass)
        patternMatch(scrutineeValue, cases, thisV, klass)

      case Return(expr, from) =>
        Returns.handle(from.symbol, eval(expr, thisV, klass))
        Bottom

      case WhileDo(cond, body) =>
        evalExprs(cond :: body :: Nil, thisV, klass)
        Bottom

      case Labeled(_, expr) =>
        eval(expr, thisV, klass)

      case Try(block, cases, finalizer) =>
        val res = evalExprs(block :: cases.map(_.body), thisV, klass).join
        if !finalizer.isEmpty then
          eval(finalizer, thisV, klass)
        res

      case SeqLiteral(elems, elemtpt) =>
        // Obtain the output Seq from SeqLiteral tree by calling respective wrapArrayMethod
        val wrapArrayMethodName = ast.tpd.wrapArrayMethodName(elemtpt.tpe)
        val meth = defn.getWrapVarargsArrayModule.requiredMethod(wrapArrayMethodName)
        val module = defn.getWrapVarargsArrayModule.moduleClass.asClass
        val args = evalArgs(elems.map(Arg.apply), thisV, klass)
        val arr = ArrayRef(State.currentObject, summon[Regions.Data])
        arr.writeElement(args.map(_.value).join)
        call(accessObject(module), meth, List(ArgInfo(arr, summon[Trace], EmptyTree)), module.typeRef, NoType)

      case Inlined(call, bindings, expansion) =>
        evalExprs(bindings, thisV, klass)
        eval(expansion, thisV, klass)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Bottom

      case vdef : ValDef =>
        // local val definition
        val sym = vdef.symbol
        if !sym.is(Flags.Lazy) then
          val rhs = eval(vdef.rhs, thisV, klass)
          initLocal(sym, rhs)
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
        report.warning("[Internal error] unexpected tree: " + expr + "\n" + Trace.show, expr)
        Bottom
  }

  /** Evaluate the cases against the scrutinee value.
   *
   *  It returns the scrutinee in most cases. The main effect of the function is for its side effects of adding bindings
   *  to the environment.
   *
   *  See https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html
   *
   *  @param scrutinee   The abstract value of the scrutinee.
   *  @param cases       The cases to match.
   *  @param thisV       The value for `C.this` where `C` is represented by `klass`.
   *  @param klass       The enclosing class where the type `tp` is located.
   */
  def patternMatch(scrutinee: Value, cases: List[CaseDef], thisV: ThisValue, klass: ClassSymbol): Contextual[Value] =
    // expected member types for `unapplySeq`
    def lengthType = ExprType(defn.IntType)
    def lengthCompareType = MethodType(List(defn.IntType), defn.IntType)
    def applyType(elemTp: Type) = MethodType(List(defn.IntType), elemTp)
    def dropType(elemTp: Type) = MethodType(List(defn.IntType), defn.CollectionSeqType.appliedTo(elemTp))
    def toSeqType(elemTp: Type) = ExprType(defn.CollectionSeqType.appliedTo(elemTp))

    def getMemberMethod(receiver: Type, name: TermName, tp: Type): Denotation =
      receiver.member(name).suchThat(receiver.memberInfo(_) <:< tp)

    /** Abstract evaluation of patterns.
     *
     *  It augments the local environment for bound pattern variables. As symbols are globally
     *  unique, we can put them in a single environment.
     *
     *  Currently, we assume all cases are reachable, thus all patterns are assumed to match.
     */
    def evalPattern(scrutinee: Value, pat: Tree): (Type, Value) = log("match " + scrutinee.show + " against " + pat.show, printer, (_: (Type, Value))._2.show):
      val trace2 = Trace.trace.add(pat)
      pat match
      case Alternative(pats) =>
        val (types, values) = pats.map(evalPattern(scrutinee, _)).unzip
        val orType = types.fold(defn.NothingType)(OrType(_, _, false))
        (orType, values.join)

      case bind @ Bind(_, pat) =>
        val (tpe, value) = evalPattern(scrutinee, pat)

        initLocal(bind.symbol, value)
        (tpe, value)

      case UnApply(fun, implicits, pats) =>
        given Trace = trace2

        val fun1 = funPart(fun)
        val funRef = fun1.tpe.asInstanceOf[TermRef]
        val unapplyResTp = funRef.widen.finalResultType

        val receiverType = fun1 match
          case ident: Ident => funRef.prefix
          case select: Select => select.qualifier.tpe

        val receiver = fun1 match
          case ident: Ident =>
            evalType(funRef.prefix, thisV, klass)
          case select: Select =>
            eval(select.qualifier, thisV, klass)

        def implicitArgsBeforeScrutinee(fun: Tree): Contextual[List[ArgInfo]] = fun match
          case Apply(f, implicitArgs) =>
            implicitArgsBeforeScrutinee(f) ++ evalArgs(implicitArgs.map(Arg.apply), thisV, klass)
          case _ => List()

        val implicitArgsAfterScrutinee = evalArgs(implicits.map(Arg.apply), thisV, klass)
        val args = implicitArgsBeforeScrutinee(fun) ++ (ArgInfo(scrutinee, summon[Trace], EmptyTree) :: implicitArgsAfterScrutinee)
        val unapplyRes = call(receiver, funRef.symbol, args, funRef.prefix, superType = NoType, needResolve = true)

        if fun.symbol.name == nme.unapplySeq then
          var resultTp = unapplyResTp
          var elemTp = unapplySeqTypeElemTp(resultTp)
          var arity = productArity(resultTp, NoSourcePosition)
          var needsGet = false
          if (!elemTp.exists && arity <= 0) {
            needsGet = true
            resultTp = resultTp.select(nme.get).finalResultType
            elemTp = unapplySeqTypeElemTp(resultTp.widen)
            arity = productSelectorTypes(resultTp, NoSourcePosition).size
          }

          var resToMatch = unapplyRes

          if needsGet then
            // Get match
            val isEmptyDenot = unapplyResTp.member(nme.isEmpty).suchThat(_.info.isParameterless)
            call(unapplyRes, isEmptyDenot.symbol, Nil, unapplyResTp, superType = NoType, needResolve = true)

            val getDenot = unapplyResTp.member(nme.get).suchThat(_.info.isParameterless)
            resToMatch = call(unapplyRes, getDenot.symbol, Nil, unapplyResTp, superType = NoType, needResolve = true)
          end if

          if elemTp.exists then
            // sequence match
            evalSeqPatterns(resToMatch, resultTp, elemTp, pats)
          else
            // product sequence match
            val selectors = productSelectors(resultTp)
            assert(selectors.length <= pats.length)
            selectors.init.zip(pats).map { (sel, pat) =>
              val selectRes = call(resToMatch, sel, Nil, resultTp, superType = NoType, needResolve = true)
              evalPattern(selectRes, pat)
            }
            val seqPats = pats.drop(selectors.length - 1)
            val toSeqRes = call(resToMatch, selectors.last, Nil, resultTp, superType = NoType, needResolve = true)
            val toSeqResTp = resultTp.memberInfo(selectors.last).finalResultType
            elemTp = unapplySeqTypeElemTp(toSeqResTp)
            // elemTp must conform to the signature in sequence match
            assert(elemTp.exists, "Product sequence match fails on " + pat + " since last element type of product is " + toSeqResTp)
            evalSeqPatterns(toSeqRes, toSeqResTp, elemTp, seqPats)
          end if
          // TODO: refactor the code of product sequence match, avoid passing NoType to parameter elemTp in evalSeqPatterns

        else
          // distribute unapply to patterns
          if isProductMatch(unapplyResTp, pats.length) then
            // product match
            val selectors = productSelectors(unapplyResTp)
            assert(selectors.length == pats.length)
            selectors.zip(pats).map { (sel, pat) =>
              val selectRes = call(unapplyRes, sel, Nil, unapplyResTp, superType = NoType, needResolve = true)
              evalPattern(selectRes, pat)
            }
          else if unapplyResTp <:< defn.BooleanType then
            // Boolean extractor, do nothing
            ()
          else
            // Get match
            val isEmptyDenot = unapplyResTp.member(nme.isEmpty).suchThat(_.info.isParameterless)
            call(unapplyRes, isEmptyDenot.symbol, Nil, unapplyResTp, superType = NoType, needResolve = true)

            val getDenot = unapplyResTp.member(nme.get).suchThat(_.info.isParameterless)
            val getRes = call(unapplyRes, getDenot.symbol, Nil, unapplyResTp, superType = NoType, needResolve = true)
            if pats.length == 1 then
              // single match
              evalPattern(getRes, pats.head)
            else
              val getResTp = getDenot.info.finalResultType
              val selectors = productSelectors(getResTp).take(pats.length)
              selectors.zip(pats).map { (sel, pat) =>
                val selectRes = call(unapplyRes, sel, Nil, getResTp, superType = NoType, needResolve = true)
                evalPattern(selectRes, pat)
              }
            end if
          end if
        end if
        // TODO: receiverType is the companion object type, not the class itself;
        //       cannot filter scrutinee by this type
        (receiverType, scrutinee)

      case Ident(nme.WILDCARD) | Ident(nme.WILDCARD_STAR) =>
        (defn.ThrowableType, scrutinee)

      case Typed(pat, typeTree) =>
        val (_, value) = evalPattern(scrutinee.filterType(typeTree.tpe), pat)
        (typeTree.tpe, value)

      case tree =>
        // For all other trees, the semantics is normal.
        (defn.ThrowableType, eval(tree, thisV, klass))

    end evalPattern

    /**
     * Evaluate a sequence value against sequence patterns.
     */
    def evalSeqPatterns(scrutinee: Value, scrutineeType: Type, elemType: Type, pats: List[Tree])(using Trace): Unit =
      // call .lengthCompare or .length
      val lengthCompareDenot = getMemberMethod(scrutineeType, nme.lengthCompare, lengthCompareType)
      if lengthCompareDenot.exists then
        call(scrutinee, lengthCompareDenot.symbol, ArgInfo(UnknownValue, summon[Trace], EmptyTree) :: Nil, scrutineeType, superType = NoType, needResolve = true)
      else
        val lengthDenot = getMemberMethod(scrutineeType, nme.length, lengthType)
        call(scrutinee, lengthDenot.symbol, Nil, scrutineeType, superType = NoType, needResolve = true)
      end if

      // call .apply
      val applyDenot = getMemberMethod(scrutineeType, nme.apply, applyType(elemType))
      val applyRes = call(scrutinee, applyDenot.symbol, ArgInfo(SafeValue(defn.IntType), summon[Trace], EmptyTree) :: Nil, scrutineeType, superType = NoType, needResolve = true)

      if isWildcardStarArgList(pats) then
        if pats.size == 1 then
          // call .toSeq
          val toSeqDenot = getMemberMethod(scrutineeType, nme.toSeq, toSeqType(elemType))
          val toSeqRes = call(scrutinee, toSeqDenot.symbol, Nil, scrutineeType, superType = NoType, needResolve = true)
          evalPattern(toSeqRes, pats.head)
        else
          // call .drop
          val dropDenot = getMemberMethod(scrutineeType, nme.drop, dropType(elemType))
          val dropRes = call(scrutinee, dropDenot.symbol, ArgInfo(SafeValue(defn.IntType), summon[Trace], EmptyTree) :: Nil, scrutineeType, superType = NoType, needResolve = true)
          for pat <- pats.init do evalPattern(applyRes, pat)
          evalPattern(dropRes, pats.last)
        end if
      else
        // no patterns like `xs*`
        for pat <- pats do evalPattern(applyRes, pat)
      end if
    end evalSeqPatterns

    def canSkipCase(remainingScrutinee: Value, catchValue: Value) =
      remainingScrutinee == Bottom || catchValue == Bottom

    var remainingScrutinee = scrutinee
    val caseResults: mutable.ArrayBuffer[Value] = mutable.ArrayBuffer()
    for caseDef <- cases do
      val (tpe, value) = evalPattern(remainingScrutinee, caseDef.pat)
      eval(caseDef.guard, thisV, klass)
      if !canSkipCase(remainingScrutinee, value) then
        caseResults.addOne(eval(caseDef.body, thisV, klass))
      if catchesAllOf(caseDef, tpe) then
        remainingScrutinee = remainingScrutinee.remove(value)

    caseResults.join
  end patternMatch

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
  def evalType(tp: Type, thisV: ThisValue, klass: ClassSymbol, elideObjectAccess: Boolean = false): Contextual[Value] = log("evaluating " + tp.show, printer, (_: Value).show) {
    tp match
      case consttpe: ConstantType =>
        SafeValue(consttpe.underlying)

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        val sym = tmref.symbol
        if sym.is(Flags.Package) then
          Package(sym)
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
          Package(sym)
        else if sym.isStaticObject && sym != klass then
          // The typer may use ThisType to refer to an object outside its definition.
          if elideObjectAccess then
            ObjectRef(sym.moduleClass.asClass)
          else
            accessObject(sym.moduleClass.asClass)

        else
          resolveThis(tref.classSymbol.asClass, thisV, klass)

      case _ =>
        throw new Exception("unexpected type: " + tp + ", Trace:\n" + Trace.show)
  }

  /** Evaluate arguments of methods and constructors */
  def evalArgs(args: List[Arg], thisV: ThisValue, klass: ClassSymbol): Contextual[List[ArgInfo]] =
    val argInfos = new mutable.ArrayBuffer[ArgInfo]
    args.foreach { arg =>
      val res =
        if arg.isByName then
          Fun(arg.tree, thisV, klass, summon[Scope])
        else
          eval(arg.tree, thisV, klass)

      argInfos += ArgInfo(res, trace.add(arg.tree), arg.tree)
    }
    argInfos.toList

  /** Initialize part of an abstract object in `klass` of the inheritance chain
   *
   * @param tpl       The class body to be evaluated.
   * @param thisV     The value of the current object to be initialized.
   * @param klass     The class to which the template belongs.
   */
  def init(tpl: Template, thisV: Ref, klass: ClassSymbol): Contextual[Ref] = log("init " + klass.show, printer, (_: Value).show) {
    val paramsMap = tpl.constr.termParamss.flatten.map { vdef =>
      vdef.name -> thisV.valValue(vdef.symbol)
    }.toMap

    // init param fields
    klass.paramGetters.foreach { acc =>
      val value = paramsMap(acc.name.toTermName)
      if acc.is(Flags.Mutable) then
        thisV.initVar(acc, value)
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
      res match {
        case ref: Ref => thisV.initOuter(cls, ref)
        case vs: ValueSet if vs.isRefSet =>
          thisV.initOuter(cls, vs)
        case _: Package =>
          thisV.initOuter(cls, Bottom)
        case _ =>
          val error = "[Internal error] Invalid outer value, cls = " + cls + ", value = " + res + Trace.show
          report.warning(error, Trace.position)
          return
      }

      // follow constructor
      if cls.hasSource then
        tasks.append { () =>
          printer.println("init super class " + cls.show)
          callConstructor(thisV, ctor, args)
          ()
        }

    // parents
    def initParent(parent: Tree, tasks: Tasks) = // TODO: store the parent objects and resolve `p.this` for parent classes `p`
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
            val args: List[ArgInfo] = ctor.info.paramInfoss.flatten.map(_ => new ArgInfo(Bottom, Trace.empty, EmptyTree))
            extendTrace(superParent) {
              superCall(tref, ctor, args, tasks)
            }
      }

      // initialize super classes after outers are set
      tasks.foreach(task => task())
    end if

    if thisV.isInstanceOf[ObjectRef] && klass == thisV.klass then
      thisV.asObjectRef.setAfterSuperCall()
    end if

    // class body
    tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) && !vdef.rhs.isEmpty =>
        val sym = vdef.symbol
        val res = if (allowList.contains(sym)) Bottom else eval(vdef.rhs, thisV, klass)
        if sym.is(Flags.Mutable) then
          thisV.initVar(sym, res)
        else
          thisV.initVal(sym, res)

      case _: MemberDef =>

      case tree =>
        eval(tree, thisV, klass)
    }

    thisV
  }

  /** Resolve C.this that appear in `D.this`
   *
   * @param target  The class symbol for `C` for which `C.this` is to be resolved.
   * @param thisV   The value for `D.this`.
   * @param klass   The enclosing class `D` where `C.this` appears
   * @param elideObjectAccess Whether object access should be omitted.
   *
   * Object access elision happens when the object access is used as a prefix
   * in `new o.C` and `C` does not need an outer.
   */
  def resolveThis(target: ClassSymbol, thisV: ThisValue, klass: ClassSymbol, elideObjectAccess: Boolean = false): Contextual[ThisValue] = log("resolveThis target = " + target.show + ", this = " + thisV.show + ", klass = " + klass.show, printer, (_: Value).show) {
    if target.is(Flags.Package) then
      val error = "[Internal error] target cannot be packages, target = " + target + Trace.show
      report.warning(error, Trace.position)
      Bottom
    else if target.isStaticObject then
      val res = ObjectRef(target.moduleClass.asClass)
      if elideObjectAccess then res
      else accessObject(target)
    else if target == klass then
      thisV
    else
      // `target` must enclose `klass`
      assert(klass.ownersIterator.contains(target), target.show + " does not enclose " + klass.show)
      val outerThis = thisV match {
        case ref: Ref => ref.outerValue(klass)
        case refSet: RefSet => refSet.joinOuters(klass)
      }
      val outerCls = klass.owner.enclosingClass.asClass
      resolveThis(target, outerThis.asInstanceOf[ThisValue], outerCls, elideObjectAccess)
  }

  /** Compute the outer value that corresponds to `tref.prefix`
   *
   * @param tref    The type whose prefix is to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `tref` is located.
   */

  def outerValue(tref: TypeRef, thisV: ThisValue, klass: ClassSymbol): Contextual[Value] = log("Evaluating outer value of = " + tref.show + ", this = " + thisV.show, printer, (_: Value).show) {
    val cls = tref.classSymbol.asClass
    if tref.prefix == NoPrefix then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      if enclosing.is(Flags.Package) then // `cls` is top-level class
        Bottom
      else // `cls` is local class
        resolveThis(enclosing, thisV, klass, elideObjectAccess = cls.isStatic)
    else
      if cls.isAllOf(Flags.JavaInterface) then Bottom
      else evalType(tref.prefix, thisV, klass, elideObjectAccess = cls.isStatic)
  }

  def printTraceWhenMultiple(trace: Trace)(using Context): String =
    if trace.toVector.size > 1 then
      Trace.buildStacktrace(trace, "The mutable state is created through: " + System.lineSeparator())
    else ""

  val mutateErrorSet: mutable.Set[(ClassSymbol, ClassSymbol)] = mutable.Set.empty
  def errorMutateOtherStaticObject(currentObj: ClassSymbol, scope: Scope)(using Trace, Context) =
    val otherObj = scope.owner
    val scope_trace = scope.getTrace
    if mutateErrorSet.add((currentObj, otherObj)) then
      val msg =
        s"Mutating ${otherObj.show} during initialization of ${currentObj.show}.\n" +
        "Mutating other static objects during the initialization of one static object is forbidden. " + Trace.show +
        printTraceWhenMultiple(scope_trace)

      report.warning(msg, Trace.position)

  val readErrorSet: mutable.Set[(ClassSymbol, ClassSymbol)] = mutable.Set.empty
  def errorReadOtherStaticObject(currentObj: ClassSymbol, scope: Scope)(using Trace, Context) =
    val otherObj = scope.owner
    val scope_trace = scope.getTrace
    if readErrorSet.add((currentObj, otherObj)) then
      val msg =
        "Reading mutable state of " + otherObj.show + " during initialization of " + currentObj.show + ".\n" +
        "Reading mutable state of other static objects is forbidden as it breaks initialization-time irrelevance. " + Trace.show +
        printTraceWhenMultiple(scope_trace)

      report.warning(msg, Trace.position)

  def errorReadUninitializedField(obj: ObjectRef, field: Symbol)(using State.Data, Trace, Context): Unit =
    if State.isQuotaExhausted(obj) then
      if !BestEffort then
        report.warning("Access uninitialized field of quota exhausted object " + field.show + ". " + Trace.show, Trace.position)
    else
      report.warning("Access uninitialized field " + field.show + ". " + Trace.show, Trace.position)
