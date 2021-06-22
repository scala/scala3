package dotty.tools.dotc
package transform
package init

import core._
import Contexts._
import Symbols._
import Types._
import StdNames._

import ast.tpd._
import util.EqHashMap
import config.Printers.init as printer
import reporting.trace as log

import Errors._

import scala.collection.mutable

class Semantic {
  import Semantic._

// ----- Domain definitions --------------------------------

  /** Abstract values
   *
   *  Value = Hot | Cold | Warm | ThisRef | Fun | RefSet
   *
   *                 Cold
   *        ┌──────►  ▲   ◄──┐  ◄────┐
   *        │         │      │       │
   *        │         │      │       │
   *   ThisRef(C)     │      │       │
   *        ▲         │      │       │
   *        │     Warm(D)   Fun    RefSet
   *        │         ▲      ▲       ▲
   *        │         │      │       │
   *      Warm(C)     │      │       │
   *        ▲         │      │       │
   *        │         │      │       │
   *        └─────────┴──────┴───────┘
   *                  Hot
   *
   *   The most important ordering is the following:
   *
   *       Hot ⊑ Warm(C) ⊑ ThisRef(C) ⊑ Cold
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
  }

  /** A transitively initialized object */
  case object Hot extends Value

  /** An object with unknown initialization status */
  case object Cold extends Value

  sealed abstract class Addr extends Value {
    def klass: ClassSymbol
    def outer: Value
  }

  /** A reference to the object under initialization pointed by `this`
   */
  case class ThisRef(klass: ClassSymbol) extends Addr {
    val outer = Hot
  }

  /** An object with all fields initialized but reaches objects under initialization
   *
   *  We need to restrict nesting levels of `outer` to finitize the domain.
   */
  case class Warm(klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value]) extends Addr

  /** A function value */
  case class Fun(expr: Tree, thisV: Addr, klass: ClassSymbol, env: Env) extends Value

  /** A value which represents a set of addresses
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: List[Fun | Addr]) extends Value

  // end of value definition

  /** The abstract object which stores value about its fields and immediate outers.
   *
   *  Semantically it suffices to store the outer for `klass`. We cache other outers
   *  for performance reasons.
   *
   *  Note: Object is NOT a value.
   */
  case class Objekt(klass: ClassSymbol, fields: mutable.Map[Symbol, Value], outers: mutable.Map[ClassSymbol, Value])

  /** Abstract heap stores abstract objects
   *
   *  As in the OOPSLA paper, the abstract heap is monotonistic.
   *
   */
  object Heap {
    opaque type Heap = mutable.Map[Addr, Objekt]

    /** Note: don't use `val` to avoid incorrect sharing */
    def empty: Heap = mutable.Map.empty

    extension (heap: Heap)
      def contains(addr: Addr): Boolean = heap.contains(addr)
      def apply(addr: Addr): Objekt = heap(addr)
      def update(addr: Addr, obj: Objekt): Unit =
        heap(addr) = obj
    end extension

    extension (ref: Addr)
      /** Update field value of the abstract object
       *
       *  Invariant: fields are immutable and only set once from `init`
       */
      def updateField(field: Symbol, value: Value): Contextual[Unit] =
        val fields = heap(ref).fields
        assert(!fields.contains(field), field.show + " already init, new = " + value + ", ref =" + ref)
        fields(field) = value

      /** Update the immediate outer of the given `klass` of the abstract object
       *
       *  Invariant: outers are immutable and only set once from `init`
       */
      def updateOuter(klass: ClassSymbol, value: Value): Contextual[Unit] =
        heap(ref).outers(klass) = value
    end extension
  }
  type Heap = Heap.Heap

  import Heap._
  val heap: Heap = Heap.empty

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
  def env(using env: Env) = env
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
  def promoted(using p: Promoted): Promoted = p

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
   * Which also avoid computing fix-point on the cache, as the cache is
   * immutable.
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
  type Cache = mutable.Map[Value, EqHashMap[Tree, Value]]
  val cache: Cache = mutable.Map.empty[Value, EqHashMap[Tree, Value]]

  /** Result of abstract interpretation */
  case class Result(value: Value, errors: Seq[Error]) {
    def show(using Context) = value.show + ", errors = " + errors.map(_.toString)

    def ++(errors: Seq[Error]): Result = this.copy(errors = this.errors ++ errors)

    def +(error: Error): Result = this.copy(errors = this.errors :+ error)

    def ensureHot(msg: String, source: Tree): Contextual[Result] =
      this ++ value.promote(msg, source)

    def select(f: Symbol, source: Tree): Contextual[Result] =
      value.select(f, source) ++ errors

    def call(meth: Symbol, args: List[ArgInfo], superType: Type, source: Tree): Contextual[Result] =
      value.call(meth, args, superType, source) ++ errors

    def instantiate(klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo], source: Tree): Contextual[Result] =
      value.instantiate(klass, ctor, args, source) ++ errors
  }

  /** The state that threads through the interpreter */
  type Contextual[T] = (Env, Context, Trace, Promoted) ?=> T

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

      case (a: Warm, b: ThisRef) if a.klass == b.klass => b
      case (a: ThisRef, b: Warm) if a.klass == b.klass => a

      case (a: (Fun | Warm | ThisRef), b: (Fun | Warm | ThisRef)) => RefSet(a :: b :: Nil)

      case (a: (Fun | Warm | ThisRef), RefSet(refs))    => RefSet(a :: refs)
      case (RefSet(refs), b: (Fun | Warm | ThisRef))    => RefSet(b :: refs)

      case (RefSet(refs1), RefSet(refs2))     => RefSet(refs1 ++ refs2)

    /** Conservatively approximate the value with `Cold` or `Hot` */
    def widenArg: Value =
      a match
      case _: Addr | _: Fun => Cold
      case RefSet(refs) => refs.map(_.widenArg).join
      case _ => a


  extension (values: Seq[Value])
    def join: Value =
      if values.isEmpty then Hot
      else values.reduce { (v1, v2) => v1.join(v2) }

    def widenArgs: List[Value] = values.map(_.widenArg).toList

  extension (value: Value)
    def select(field: Symbol, source: Tree, needResolve: Boolean = true): Contextual[Result] = log("select " + field.show, printer, res => res.asInstanceOf[Result].show) {
      if promoted.isCurrentObjectPromoted then Result(Hot, Nil)
      else value match {
        case Hot  =>
          Result(Hot, Errors.empty)

        case Cold =>
          val error = AccessCold(field, source, trace.toVector)
          Result(Hot, error :: Nil)

        case addr: Addr =>
          val target = if needResolve then resolve(addr.klass, field) else field
          val trace1 = trace.add(source)
          if target.is(Flags.Lazy) then
            given Trace = trace1
            val rhs = target.defTree.asInstanceOf[ValDef].rhs
            eval(rhs, addr, target.owner.asClass, cacheResult = true)
          else
            val obj = heap(addr)
            if obj.fields.contains(target) then
              Result(obj.fields(target), Nil)
            else if addr.isInstanceOf[Warm] then
              if target.is(Flags.ParamAccessor) then
                // possible for trait parameters
                // see tests/init/neg/trait2.scala
                //
                // return `Hot` here, errors are reported in checking `ThisRef`
                Result(Hot, Nil)
              else if target.hasSource then
                val rhs = target.defTree.asInstanceOf[ValOrDefDef].rhs
                eval(rhs, addr, target.owner.asClass, cacheResult = true)
              else
                val error = CallUnknown(field, source, trace.toVector)
                Result(Hot, error :: Nil)
            else
              val error = AccessNonInit(target, trace.add(source).toVector)
              Result(Hot, error :: Nil)

        case fun: Fun =>
          report.error("unexpected tree in selecting a function, fun = " + fun.expr.show, source)
          Result(Hot, Nil)

        case RefSet(refs) =>
          val resList = refs.map(_.select(field, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }
    }

    def call(meth: Symbol, args: List[ArgInfo], superType: Type, source: Tree, needResolve: Boolean = true): Contextual[Result] = log("call " + meth.show + ", args = " + args, printer, res => res.asInstanceOf[Result].show) {
      def checkArgs = args.flatMap(_.promote)

      // fast track if the current object is already initialized
      if promoted.isCurrentObjectPromoted then Result(Hot, Nil)
      else value match {
        case Hot  =>
          Result(Hot, checkArgs)

        case Cold =>
          val error = CallCold(meth, source, trace.toVector)
          Result(Hot, error :: checkArgs)

        case addr: Addr =>
          val isLocal = !meth.owner.isClass
          val target =
            if !needResolve then
              meth
            else if superType.exists then
              resolveSuper(addr.klass, superType, meth)
            else
              resolve(addr.klass, meth)

          if target.isOneOf(Flags.Method) then
            val trace1 = trace.add(source)
            if target.hasSource then
              given Trace = trace1
              val cls = target.owner.enclosingClass.asClass
              val ddef = target.defTree.asInstanceOf[DefDef]
              // try early promotion here; if returns error, returns cold
              val env2 = Env(ddef, args.map(_.value).widenArgs)
              if target.isPrimaryConstructor then
                given Env = env2
                val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
                val res = withTrace(trace.add(cls.defTree)) { eval(tpl, addr, cls, cacheResult = true) }
                Result(addr, res.errors)
              else if target.isConstructor then
                given Env = env2
                eval(ddef.rhs, addr, cls, cacheResult = true)
              else
                // normal method call
                withEnv(if isLocal then env else Env.empty) {
                  eval(ddef.rhs, addr, cls, cacheResult = true) ++ checkArgs
                }
            else if addr.canIgnoreMethodCall(target) then
              Result(Hot, Nil)
            else
              // no source code available
              val error = CallUnknown(target, source, trace.toVector)
              Result(Hot, error :: checkArgs)
          else
            // method call resolves to a field
            val obj = heap(addr)
            if obj.fields.contains(target) then
              Result(obj.fields(target), Nil)
            else
              value.select(target, source, needResolve = false)

        case Fun(body, thisV, klass, env) =>
          // meth == NoSymbol for poly functions
          if meth.name.toString == "tupled" then Result(value, Nil) // a call like `fun.tupled`
          else
            withEnv(env) {
              eval(body, thisV, klass, cacheResult = true) ++ checkArgs
            }

        case RefSet(refs) =>
          val resList = refs.map(_.call(meth, args, superType, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }
    }

    /** Handle a new expression `new p.C` where `p` is abstracted by `value` */
    def instantiate(klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo], source: Tree): Contextual[Result] = log("instantiating " + klass.show + ", value = " + value + ", args = " + args, printer, res => res.asInstanceOf[Result].show) {
      val trace1 = trace.add(source)
      if promoted.isCurrentObjectPromoted then Result(Hot, Nil)
      else value match {
        case Hot  =>
          val buffer = new mutable.ArrayBuffer[Error]
          val args2 = args.map { arg =>
            val errors = arg.promote
            buffer ++= errors
            if errors.isEmpty then Hot
            else arg.value.widenArg
          }

          if buffer.isEmpty then Result(Hot, Errors.empty)
          else
            val value = Warm(klass, Hot, ctor, args2).ensureExists
            val res = value.call(ctor, args, superType = NoType, source)
            Result(value, res.errors)

        case Cold =>
          val error = CallCold(ctor, source, trace1.toVector)
          Result(Hot, error :: Nil)

        case addr: Addr =>
          given Trace = trace1
          // widen the outer to finitize addresses
          val outer = addr match
            case Warm(_, _: Warm, _, _) => Cold
            case _ => addr

          val value = Warm(klass, outer, ctor, args.map(_.value).widenArgs).ensureExists
          val res = value.call(ctor, args, superType = NoType, source)
          Result(value, res.errors)

        case Fun(body, thisV, klass, env) =>
          report.error("unexpected tree in instantiating a function, fun = " + body.show, source)
          Result(Hot, Nil)

        case RefSet(refs) =>
          val resList = refs.map(_.instantiate(klass, ctor, args, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }
    }
  end extension

// ----- Promotion ----------------------------------------------------
  extension (addr: Addr)
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
    def isFullyFilled: Contextual[Boolean] = log("isFullyFilled " + addr, printer) {
      val obj = heap(addr)
      addr.klass.baseClasses.forall { klass =>
        !klass.hasSource || {
          val nonInits = klass.info.decls.filter { member =>
            !member.isOneOf(Flags.Method | Flags.Lazy | Flags.Deferred)
            && !member.isType
            && !obj.fields.contains(member)
          }
          printer.println("nonInits = " + nonInits)
          nonInits.isEmpty
        }
      }
    }

    /** Ensure the corresponding object exists in the heap */
    def ensureExists: addr.type =
      if !heap.contains(addr) then
        val obj = Objekt(addr.klass, fields = mutable.Map.empty, outers = mutable.Map(addr.klass -> addr.outer))
        heap.update(addr, obj)
      addr

  end extension

  extension (thisRef: ThisRef)
    def tryPromoteCurrentObject: Contextual[Boolean] = log("tryPromoteCurrentObject ", printer) {
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
    def promote(msg: String, source: Tree): Contextual[List[Error]] = log("promoting " + value + ", promoted = " + promoted, printer) {
      if promoted.isCurrentObjectPromoted then Nil else

      value.match
      case Hot   =>  Nil

      case Cold  =>  PromoteError(msg, source, trace.toVector) :: Nil

      case thisRef: ThisRef =>
        if thisRef.tryPromoteCurrentObject then Nil
        else PromoteError(msg, source, trace.toVector) :: Nil

      case warm: Warm =>
        if promoted.contains(warm) then Nil
        else {
          promoted.add(warm)
          val errors = warm.tryPromote(msg, source)
          if errors.nonEmpty then promoted.remove(warm)
          errors
        }

      case fun @ Fun(body, thisV, klass, env) =>
        if promoted.contains(fun) then Nil
        else
          val res = withEnv(env) { eval(body, thisV, klass) }
          val errors2 = res.value.promote(msg, source)
          if (res.errors.nonEmpty || errors2.nonEmpty)
            UnsafePromotion(msg, source, trace.toVector, res.errors ++ errors2) :: Nil
          else
            promoted.add(fun)
            Nil

      case RefSet(refs) =>
        refs.flatMap(_.promote(msg, source))
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

      val buffer  = new mutable.ArrayBuffer[Error]

      warm.klass.baseClasses.exists { klass =>
        klass.hasSource && klass.info.decls.exists { member =>
          if !member.isType && !member.isConstructor && member.hasSource  && !member.is(Flags.Deferred) then
            if member.is(Flags.Method) then
              val trace2 = trace.add(source)
              locally {
                given Trace = trace2
                val args = member.info.paramInfoss.flatten.map(_ => ArgInfo(Hot, EmptyTree))
                val res = warm.call(member, args, superType = NoType, source = member.defTree)
                buffer ++= res.ensureHot(msg, source).errors
              }
            else
              val res = warm.select(member, source)
              buffer ++= res.ensureHot(msg, source).errors
          buffer.nonEmpty
        }
      }

      if buffer.isEmpty then Nil
      else UnsafePromotion(msg, source, trace.toVector, buffer.toList) :: Nil
    }

  end extension

// ----- Policies ------------------------------------------------------
  extension (value: Addr)
    /** Can the method call on `value` be ignored?
     *
     *  Note: assume overriding resolution has been performed.
     */
    def canIgnoreMethodCall(meth: Symbol)(using Context): Boolean =
      val cls = meth.owner
      cls == defn.AnyClass ||
      cls == defn.AnyValClass ||
      cls == defn.ObjectClass

// ----- Semantic definition --------------------------------

  /** Utility definition used for better error-reporting of argument errors */
  case class ArgInfo(value: Value, source: Tree) {
    def promote: Contextual[List[Error]] = value.promote("May only use initialized value as arguments", source)
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
  def eval(expr: Tree, thisV: Addr, klass: ClassSymbol, cacheResult: Boolean = false): Contextual[Result] = log("evaluating " + expr.show + ", this = " + thisV.show, printer, res => res.asInstanceOf[Result].show) {
    val innerMap = cache.getOrElseUpdate(thisV, new EqHashMap[Tree, Value])
    if (innerMap.contains(expr)) Result(innerMap(expr), Errors.empty)
    else {
      // no need to compute fix-point, because
      // 1. the result is decided by `cfg` for a legal program
      //    (heap change is irrelevant thanks to monotonicity)
      // 2. errors will have been reported for an illegal program
      innerMap(expr) = Hot
      val res = cases(expr, thisV, klass)
      if cacheResult then innerMap(expr) = res.value else innerMap.remove(expr)
      res
    }
  }

  /** Evaluate a list of expressions */
  def eval(exprs: List[Tree], thisV: Addr, klass: ClassSymbol): Contextual[List[Result]] = 
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Evaluate arguments of methods */
  def evalArgs(args: List[Arg], thisV: Addr, klass: ClassSymbol): Contextual[(List[Error], List[ArgInfo])] =
    val errors = new mutable.ArrayBuffer[Error]
    val argInfos = new mutable.ArrayBuffer[ArgInfo]
    args.foreach { arg =>
      val res =
        if arg.isByName then
          val fun = Fun(arg.tree, thisV, klass, env)
          Result(fun, Nil)
        else
          eval(arg.tree, thisV, klass)

      errors ++= res.errors
      argInfos += ArgInfo(res.value, arg.tree)
    }
    (errors.toList, argInfos.toList)

  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   */
  def cases(expr: Tree, thisV: Addr, klass: ClassSymbol): Contextual[Result] =
    expr match {
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Result(Hot, Errors.empty)

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        cases(expr.tpe, thisV, klass, expr)

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val (errors, args) = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        val res = outerValue(tref, thisV, klass, tpt)
        val trace2 = trace.add(expr)
        locally {
          given Trace = trace2
          (res ++ errors).instantiate(cls, ctor, args, source = expr)
        }

      case Call(ref, argss) =>
        // check args
        val (errors, args) = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe
          val thisValue2 = resolveThis(thisTp.classSymbol.asClass, thisV, klass, ref)
          Result(thisValue2, errors).call(ref.symbol, args, superTp, expr)

        case Select(qual, _) =>
          val res = eval(qual, thisV, klass) ++ errors
          res.call(ref.symbol, args, superType = NoType, source = expr)

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = resolveThis(enclosingClass, thisV, klass, id)
            // local methods are not a member, but we can reuse the method `call`
            thisValue2.call(id.symbol, args, superType = NoType, expr, needResolve = false)
          case TermRef(prefix, _) =>
            val res = cases(prefix, thisV, klass, id) ++ errors
            res.call(id.symbol, args, superType = NoType, source = expr)

      case Select(qualifier, name) =>
        eval(qualifier, thisV, klass).select(expr.symbol, expr)

      case _: This =>
        cases(expr.tpe, thisV, klass, expr)

      case Literal(_) =>
        Result(Hot, Errors.empty)

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Hot, Errors.empty)
        else eval(expr, thisV, klass) ++ checkTermUsage(tpt, thisV, klass)

      case NamedArg(name, arg) =>
        eval(arg, thisV, klass)

      case Assign(lhs, rhs) =>
        lhs match
        case Select(qual, _) =>
          val res = eval(qual, thisV, klass)
          eval(rhs, thisV, klass).ensureHot("May only assign fully initialized value", rhs) ++ res.errors
        case id: Ident =>
          eval(rhs, thisV, klass).ensureHot("May only assign fully initialized value", rhs)

      case closureDef(ddef) =>
        val value = Fun(ddef.rhs, thisV, klass, env)
        Result(value, Nil)

      case PolyFun(body) =>
        val value = Fun(body, thisV, klass, env)
        Result(value, Nil)

      case Block(stats, expr) =>
        val (ress, env2) = eval(stats, thisV, klass)
        withEnv(env2) {
          eval(expr, thisV, klass) ++ ress.flatMap(_.errors)
        }

      case If(cond, thenp, elsep) =>
        val (ress, env2) = eval(cond :: thenp :: elsep :: Nil, thisV, klass)
        val value = ress.map(_.value).join
        val errors = ress.flatMap(_.errors)
        Result(value, errors)

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Hot, Errors.empty)
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        val res1 = eval(selector, thisV, klass).ensureHot("The value to be matched needs to be fully initialized", selector)
        val (ress, env) = eval(cases.map(_.body), thisV, klass)
        val value = ress.map(_.value).join
        val errors = res1.errors ++ ress.flatMap(_.errors)
        Result(value, errors)

      case Return(expr, from) =>
        eval(expr, thisV, klass).ensureHot("return expression may only be initialized value", expr)

      case WhileDo(cond, body) =>
        val (ress, env2) = eval(cond :: body :: Nil, thisV, klass)
        Result(Hot, ress.flatMap(_.errors))

      case Labeled(_, expr) =>
        eval(expr, thisV, klass)

      case Try(block, cases, finalizer) =>
        val res1 = eval(block, thisV, klass)
        val (ress, env2) = eval(cases.map(_.body), thisV, klass)
        val errors = ress.flatMap(_.errors)
        val resValue = ress.map(_.value).join
        if finalizer.isEmpty then
          Result(resValue, res1.errors ++ errors)
        else
          val res2 = eval(finalizer, thisV, klass)
          Result(resValue, res1.errors ++ errors ++ res2.errors)

      case SeqLiteral(elems, elemtpt) =>
        val ress = elems.map { elem =>
          eval(elem, thisV, klass).ensureHot("May only use initialized value as method arguments", elem)
        }
        Result(Hot, ress.flatMap(_.errors))

      case Inlined(call, bindings, expansion) =>
        val (ress, env2) = eval(bindings, thisV, klass)
        eval(expansion, thisV, klass) ++ ress.flatMap(_.errors)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Result(Hot, Errors.empty)

      case vdef : ValDef =>
        // local val definition
        // TODO: support explicit @cold annotation for local definitions
        eval(vdef.rhs, thisV, klass, true)
        // .ensureHot("Local definitions may only hold initialized values", vdef)

      case ddef : DefDef =>
        // local method
        Result(Hot, Errors.empty)

      case tdef: TypeDef =>
        // local type definition
        if tdef.isClassDef then Result(Hot, Errors.empty)
        else Result(Hot, checkTermUsage(tdef.rhs, thisV, klass))

      case tpl: Template =>
        init(tpl, thisV, klass)

      case _: Import | _: Export =>
        Result(Hot, Errors.empty)

      case _ =>
        throw new Exception("unexpected tree: " + expr.show)
    }

  /** Handle semantics of leaf nodes */
  def cases(tp: Type, thisV: Addr, klass: ClassSymbol, source: Tree): Contextual[Result] = log("evaluating " + tp.show, printer, res => res.asInstanceOf[Result].show) {
    tp match {
      case _: ConstantType =>
        Result(Hot, Errors.empty)

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        val sym = tmref.symbol

        def default() = Result(Hot, Nil)

        if sym.is(Flags.Param) && sym.owner.isConstructor then
          // instances of local classes inside secondary constructors cannot
          // reach here, as those values are abstracted by Cold instead of Warm.
          // This enables us to simplify the domain without sacrificing
          // expressiveness nor soundess, as local classes inside secondary
          // constructors are uncommon.
          if sym.isContainedIn(klass) then
            Result(env.lookup(sym), Nil)
          else
            // We don't know much about secondary constructor parameters in outer scope.
            // It's always safe to approximate them with `Cold`.
            Result(Cold, Nil)
        else
          // resolve this for local variable
          val enclosingClass = sym.owner.enclosingClass.asClass
          val thisValue2 = resolveThis(enclosingClass, thisV, klass, source)

      case tmref: TermRef =>
        cases(tmref.prefix, thisV, klass, source).select(tmref.symbol, source)

      case tp @ ThisType(tref) =>
        val cls = tref.classSymbol.asClass
        if cls.isStaticOwner && !klass.isContainedIn(cls) then
          // O.this outside the body of the object O
          Result(Hot, Nil)
        else
          val value = resolveThis(cls, thisV, klass, source)
          Result(value, Errors.empty)

      case _: TermParamRef | _: RecThis  =>
        // possible from checking effects of types
        Result(Hot, Errors.empty)

      case _ =>
        throw new Exception("unexpected type: " + tp)
    }
  }

  /** Resolve C.this that appear in `klass` */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol, source: Tree): Contextual[Value] = log("resolving " + target.show + ", this = " + thisV.show + " in " + klass.show, printer, res => res.asInstanceOf[Value].show) {
    if target == klass then thisV
    else if target.is(Flags.Package) then Hot
    else
      thisV match
        case Hot => Hot
        case addr: Addr =>
          val obj = heap(addr)
          val outerCls = klass.owner.lexicallyEnclosingClass.asClass
          if !obj.outers.contains(klass) then
            val error = PromoteError("outer not yet initialized, target = " + target + ", klass = " + klass, source, trace.toVector)
            report.error(error.show + error.stacktrace, source)
            Hot
          else
            resolveThis(target, obj.outers(klass), outerCls, source)
        case RefSet(refs) =>
          refs.map(ref => resolveThis(target, ref, klass, source)).join
        case fun: Fun =>
          report.warning("unexpected thisV = " + thisV + ", target = " + target.show + ", klass = " + klass.show, source.srcPos)
          Cold
        case Cold => Cold

  }

  /** Compute the outer value that correspond to `tref.prefix` */
  def outerValue(tref: TypeRef, thisV: Addr, klass: ClassSymbol, source: Tree): Contextual[Result] =
    val cls = tref.classSymbol.asClass
    if tref.prefix == NoPrefix then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      val outerV = resolveThis(enclosing, thisV, klass, source)
      Result(outerV, Errors.empty)
    else
      if cls.isAllOf(Flags.JavaInterface) then Result(Hot, Nil)
      else cases(tref.prefix, thisV, klass, source)

  /** Initialize part of an abstract object in `klass` of the inheritance chain */
  def init(tpl: Template, thisV: Addr, klass: ClassSymbol): Contextual[Result] = log("init " + klass.show, printer, res => res.asInstanceOf[Result].show) {
    val errorBuffer = new mutable.ArrayBuffer[Error]

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
      errorBuffer ++= res.errors
      thisV.updateOuter(cls, res.value)

      // follow constructor
      if cls.hasSource then
        tasks.append { () =>
          printer.println("init super class " + cls.show)
          val res2 = thisV.call(ctor, args, superType = NoType, source)
          errorBuffer ++= res2.errors
          ()
        }

    // parents
    def initParent(parent: Tree, tasks: Tasks)(using Env) = parent match {
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        eval(stats, thisV, klass)._1.foreach { res => errorBuffer ++= res.errors }
        val (errors, args) = evalArgs(argss.flatten, thisV, klass)
        errorBuffer ++= errors
        superCall(tref, ctor, args, tree, tasks)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>       // extends A(args)
      val (errors, args) = evalArgs(argss.flatten, thisV, klass)
      errorBuffer ++= errors
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

    var fieldsChanged = true

    // class body
    tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) && !vdef.rhs.isEmpty =>
        given Env = Env.empty
        val res = eval(vdef.rhs, thisV, klass, cacheResult = true)
        errorBuffer ++= res.errors
        thisV.updateField(vdef.symbol, res.value)
        fieldsChanged = true

      case _: MemberDef =>

      case tree =>
        thisV match
        case thisRef: ThisRef =>
          if fieldsChanged then thisRef.tryPromoteCurrentObject
          fieldsChanged = false
        case _ =>

        given Env = Env.empty
        errorBuffer ++= eval(tree, thisV, klass).errors
    }

    Result(thisV, errorBuffer.toList)
  }

  /** Check that path in path-dependent types are initialized
   *
   *  This is intended to avoid type soundness issues in Dotty.
   */
  def checkTermUsage(tpt: Tree, thisV: Addr, klass: ClassSymbol): Contextual[List[Error]] =
    val buf = new mutable.ArrayBuffer[Error]
    val traverser = new TypeTraverser {
      def traverse(tp: Type): Unit = tp match {
        case TermRef(_: SingletonType, _) =>
          buf ++= cases(tp, thisV, klass, tpt).errors
        case _ =>
          traverseChildren(tp)
      }
    }
    traverser.traverse(tpt.tpe)
    buf.toList

}

object Semantic {

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

  def resolve(cls: ClassSymbol, sym: Symbol)(using Context): Symbol =
    if (sym.isEffectivelyFinal || sym.isConstructor) sym
    else sym.matchingMember(cls.appliedRef)

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
