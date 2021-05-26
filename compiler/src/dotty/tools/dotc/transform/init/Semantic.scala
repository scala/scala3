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
  }

  /** A reference to the object under initialization pointed by `this`
   */
  case class ThisRef(klass: ClassSymbol) extends Addr

  /** An object with all fields initialized but reaches objects under initialization
   *
   *  We need to restrict nesting levels of `outer` to finitize the domain.
   */
  case class Warm(klass: ClassSymbol, outer: Value) extends Addr

  /** A function value */
  case class Fun(expr: Tree, thisV: Addr, klass: ClassSymbol) extends Value

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
   *  This is only one object we need to care about, hence it's just `Objekt`.
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
      def updateField(field: Symbol, value: Value): Contextual[Unit] =
        heap(ref).fields(field) = value

      def updateOuter(klass: ClassSymbol, value: Value): Contextual[Unit] =
        heap(ref).outers(klass) = value
    end extension
  }
  type Heap = Heap.Heap

  import Heap._
  val heap: Heap = Heap.empty

  object Promoted {
    /** Values that have been safely promoted */
    opaque type Promoted = mutable.Set[Value]

    /** Note: don't use `val` to avoid incorrect sharing */
    def empty: Promoted = mutable.Set.empty

    extension (promoted: Promoted)
      def contains(value: Value): Boolean = promoted.contains(value)
      def add(value: Value): Unit = promoted += value
      def remove(value: Value): Unit = promoted -= value
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

    def call(meth: Symbol, superType: Type, source: Tree): Contextual[Result] =
      value.call(meth, superType, source) ++ errors

    def instantiate(klass: ClassSymbol, ctor: Symbol, source: Tree): Contextual[Result] =
      value.instantiate(klass, ctor, source) ++ errors
  }

  /** The state that threads through the interpreter */
  type Contextual[T] = (Context, Trace, Promoted) ?=> T

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

  extension (values: Seq[Value])
    def join: Value =
      if values.isEmpty then Hot
      else values.reduce { (v1, v2) => v1.join(v2) }

  extension (value: Value)
    def select(field: Symbol, source: Tree, needResolve: Boolean = true): Contextual[Result] =
      value match {
        case Hot  =>
          Result(Hot, Errors.empty)

        case Cold =>
          val error = AccessCold(field, source, trace.toVector)
          Result(Hot, error :: Nil)

        case addr: Addr =>
          val target = if needResolve then resolve(addr.klass, field) else field
          if target.is(Flags.Lazy) then
            value.call(target, superType = NoType, source, needResolve = false)
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

        case _: Fun =>
          ???

        case RefSet(refs) =>
          val resList = refs.map(_.select(field, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }

    def call(meth: Symbol, superType: Type, source: Tree, needResolve: Boolean = true): Contextual[Result] =
      value match {
        case Hot  =>
          Result(Hot, Errors.empty)

        case Cold =>
          val error = CallCold(meth, source, trace.toVector)
          Result(Hot, error :: Nil)

        case addr: Addr =>
          val target =
            if !needResolve then
              meth
            else if superType.exists then
              resolveSuper(addr.klass, superType, meth)
            else
              resolve(addr.klass, meth)
          if target.isOneOf(Flags.Method | Flags.Lazy) then
            if target.hasSource then
              val cls = target.owner.enclosingClass.asClass
              if target.isPrimaryConstructor then
                val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
                eval(tpl, addr, cls, cacheResult = true)(using ctx, trace.add(tpl), promoted)
              else
                val rhs = target.defTree.asInstanceOf[ValOrDefDef].rhs
                eval(rhs, addr, cls, cacheResult = true)
            else if addr.canIgnoreMethodCall(target) then
              Result(Hot, Nil)
            else
              val error = CallUnknown(target, source, trace.toVector)
              Result(Hot, error :: Nil)
          else
            val obj = heap(addr)
            if obj.fields.contains(target) then
              Result(obj.fields(target), Nil)
            else
              value.select(target, source, needResolve = false)

        case Fun(body, thisV, klass) =>
          // meth == NoSymbol for poly functions
          if meth.name.toString == "tupled" then Result(value, Nil) // a call like `fun.tupled`
          else eval(body, thisV, klass, cacheResult = true)

        case RefSet(refs) =>
          val resList = refs.map(_.call(meth, superType, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }

    /** Handle a new expression `new p.C` where `p` is abstracted by `value` */
    def instantiate(klass: ClassSymbol, ctor: Symbol, source: Tree): Contextual[Result] =
      value match {
        case Hot  =>
          Result(Hot, Errors.empty)

        case Cold =>
          val error = CallCold(ctor, source, trace.toVector)
          Result(Hot, error :: Nil)

        case addr: Addr =>
          // widen the outer to finitize addresses
          val outer = addr match
            case Warm(_, _: Warm) => Cold
            case _ => addr

          val value = Warm(klass, outer)
          if !heap.contains(value) then
            val obj = Objekt(klass, fields = mutable.Map.empty, outers = mutable.Map(klass -> outer))
            heap.update(value, obj)
          val res = value.call(ctor, superType = NoType, source)
          Result(value, res.errors)

        case Fun(body, thisV, klass) =>
          ??? // impossible

        case RefSet(refs) =>
          val resList = refs.map(_.instantiate(klass, ctor, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }
  end extension

// ----- Promotion ----------------------------------------------------

  extension (value: Value)
    /** Can we promote the value by checking the extrinsic values?
     *
     *  The extrinsic values are environment values, e.g. outers for `Warm`
     *  and `thisV` captured in functions.
     *
     *  This is a fast track for early promotion of values.
     */
    def canPromoteExtrinsic: Contextual[Boolean] =
      value match
      case Hot   =>  true
      case Cold  =>  false

      case warm: Warm  =>
        warm.outer.canPromoteExtrinsic && {
          promoted.add(warm)
          true
        }

      case thisRef: ThisRef =>
        promoted.contains(thisRef) || {
          val obj = heap(thisRef)
          // If we have all fields initialized, then we can promote This to hot.
          val allFieldsInitialized = thisRef.klass.appliedRef.fields.forall { denot =>
            val sym = denot.symbol
            sym.isOneOf(Flags.Lazy | Flags.Deferred) || obj.fields.contains(sym)
          }
          if allFieldsInitialized then promoted.add(thisRef)
          allFieldsInitialized
        }

      case fun: Fun =>
        fun.thisV.canPromoteExtrinsic && {
          promoted.add(fun)
          true
        }

      case RefSet(refs) =>
        refs.forall(_.canPromoteExtrinsic)

    end canPromoteExtrinsic

    /** Promotion of values to hot */
    def promote(msg: String, source: Tree): Contextual[List[Error]] =
      value match
      case Hot   =>  Nil

      case Cold  =>  PromoteError(msg, source, trace.toVector) :: Nil

      case thisRef: ThisRef =>
        if promoted.contains(thisRef) then Nil
        else if thisRef.canPromoteExtrinsic then Nil
        else PromoteError(msg, source, trace.toVector) :: Nil

      case warm: Warm =>
        if promoted.contains(warm) then Nil
        else if warm.canPromoteExtrinsic then Nil
        else {
          promoted.add(warm)
          val errors = warm.tryPromote(msg, source)
          if errors.nonEmpty then promoted.remove(warm)
          errors
        }

      case fun @ Fun(body, thisV, klass) =>
        if promoted.contains(fun) then Nil
        else
          val res = eval(body, thisV, klass)
          val errors2 = res.value.promote(msg, source)
          if (res.errors.nonEmpty || errors2.nonEmpty)
            UnsafePromotion(msg, source, trace.toVector, res.errors ++ errors2) :: Nil
          else
            promoted.add(fun)
            Nil

      case RefSet(refs) =>
        refs.flatMap(_.promote(msg, source))
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
    def tryPromote(msg: String, source: Tree): Contextual[List[Error]] = log("promote " + warm.show, printer) {
      val classRef = warm.klass.appliedRef
      if classRef.memberClasses.nonEmpty then
        return PromoteError(msg, source, trace.toVector) :: Nil

      val fields  = classRef.fields
      val methods = classRef.membersBasedOnFlags(Flags.Method, Flags.Deferred | Flags.Accessor)
      val buffer  = new mutable.ArrayBuffer[Error]

      fields.exists { denot =>
        val f = denot.symbol
        if !f.isOneOf(Flags.Deferred | Flags.Private | Flags.Protected) && f.hasSource then
          val trace2 = trace.add(f.defTree)
          val res = warm.select(f, source)
          locally {
            given Trace = trace2
            buffer ++= res.ensureHot(msg, source).errors
          }
        buffer.nonEmpty
      }

      buffer.nonEmpty || methods.exists { denot =>
        val m = denot.symbol
        if !m.isConstructor && m.hasSource then
          val trace2 = trace.add(m.defTree)
          locally {
            given Trace = trace2
            val res = warm.call(m, superType = NoType, source = source)
            buffer ++= res.ensureHot(msg, source).errors
          }
        buffer.nonEmpty
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
  def evalArgs(args: List[Arg], thisV: Addr, klass: ClassSymbol): Contextual[List[Error]] =
    val ress = args.map { arg =>
      val res =
        if arg.isByName then
          val fun = Fun(arg.tree, thisV, klass)
          Result(fun, Nil)
        else
          eval(arg.tree, thisV, klass)

      res.ensureHot("May only use initialized value as arguments", arg.tree)
    }
    ress.flatMap(_.errors)

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
        val errors = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        val res = outerValue(tref, thisV, klass, tpt)
        val trace2 = trace.add(expr)
        locally {
          given Trace = trace2
          (res ++ errors).instantiate(cls, ctor, expr)
        }

      case Call(ref, argss) =>
        // check args
        val errors = evalArgs(argss.flatten, thisV, klass)

        val trace2: Trace = trace.add(expr)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe
          val thisValue2 = resolveThis(thisTp.classSymbol.asClass, thisV, klass, ref)
          Result(thisValue2, errors).call(ref.symbol, superTp, expr)(using ctx, trace2)

        case Select(qual, _) =>
          val res = eval(qual, thisV, klass) ++ errors
          res.call(ref.symbol, superType = NoType, source = expr)(using ctx, trace2)

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = resolveThis(enclosingClass, thisV, klass, id)
            // local methods are not a member, but we can reuse the method `call`
            thisValue2.call(id.symbol, superType = NoType, expr, needResolve = false)
          case TermRef(prefix, _) =>
            val res = cases(prefix, thisV, klass, id) ++ errors
            res.call(id.symbol, superType = NoType, source = expr)(using ctx, trace2)

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
        val value = Fun(ddef.rhs, thisV, klass)
        Result(value, Nil)

      case PolyFun(body) =>
        val value = Fun(body, thisV, klass)
        Result(value, Nil)

      case Block(stats, expr) =>
        val ress = eval(stats, thisV, klass)
        eval(expr, thisV, klass) ++ ress.flatMap(_.errors)

      case If(cond, thenp, elsep) =>
        val ress = eval(cond :: thenp :: elsep :: Nil, thisV, klass)
        val value = ress.map(_.value).join
        val errors = ress.flatMap(_.errors)
        Result(value, errors)

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Hot, Errors.empty)
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        val res1 = eval(selector, thisV, klass).ensureHot("The value to be matched needs to be fully initialized", selector)
        val ress = eval(cases.map(_.body), thisV, klass)
        val value = ress.map(_.value).join
        val errors = res1.errors ++ ress.flatMap(_.errors)
        Result(value, errors)

      case Return(expr, from) =>
        eval(expr, thisV, klass).ensureHot("return expression may only be initialized value", expr)

      case WhileDo(cond, body) =>
        val ress = eval(cond :: body :: Nil, thisV, klass)
        Result(Hot, ress.flatMap(_.errors))

      case Labeled(_, expr) =>
        eval(expr, thisV, klass)

      case Try(block, cases, finalizer) =>
        val res1 = eval(block, thisV, klass)
        val ress = eval(cases.map(_.body), thisV, klass)
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
        val ress = eval(bindings, thisV, klass)
        eval(expansion, thisV, klass) ++ ress.flatMap(_.errors)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Result(Hot, Errors.empty)

      case vdef : ValDef =>
        // local val definition
        // TODO: support explicit @cold annotation for local definitions
        eval(vdef.rhs, thisV, klass).ensureHot("Local definitions may only hold initialized values", vdef)

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
        Result(Hot, Errors.empty)

      case tmref: TermRef =>
        cases(tmref.prefix, thisV, klass, source).select(tmref.symbol, source)

      case tp @ ThisType(tref) =>
        if tref.symbol.is(Flags.Package) then Result(Hot, Errors.empty)
        else
          val value = resolveThis(tref.classSymbol.asClass, thisV, klass, source)
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
    else if target.is(Flags.Package) || target.isStaticOwner then Hot
    else
      thisV match
        case Hot | _: ThisRef => Hot
        case warm: Warm =>
          val obj = heap(warm)
          val outerCls = klass.owner.enclosingClass.asClass
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
      cases(tref.prefix, thisV, klass, source)

  /** Initialize part of an abstract object in `klass` of the inheritance chain */
  def init(tpl: Template, thisV: Addr, klass: ClassSymbol): Contextual[Result] = log("init " + klass.show, printer, res => res.asInstanceOf[Result].show) {
    val errorBuffer = new mutable.ArrayBuffer[Error]

    // init param fields
    klass.paramAccessors.foreach { acc =>
      if (!acc.is(Flags.Method)) {
        printer.println(acc.show + " initialized")
        thisV.updateField(acc, Hot)
      }
    }

    def superCall(tref: TypeRef, ctor: Symbol, source: Tree): Unit =
      val cls = tref.classSymbol.asClass
      // update outer for super class
      val res = outerValue(tref, thisV, klass, source)
      errorBuffer ++= res.errors
      thisV.updateOuter(cls, res.value)

      // follow constructor
      if cls.hasSource then
        val res2 = thisV.call(ctor, superType = NoType, source)(using ctx, trace.add(source))
        errorBuffer ++= res2.errors

    // parents
    def initParent(parent: Tree) = parent match {
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        eval(stats, thisV, klass).foreach { res => errorBuffer ++= res.errors }
        errorBuffer ++= evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, tree)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>       // extends A(args)
        errorBuffer ++= evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, tree)

      case _ =>   // extends A or extends A[T]
        val tref = typeRefOf(parent.tpe)
        superCall(tref, tref.classSymbol.primaryConstructor, parent)
    }

    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html
    if !klass.is(Flags.Trait) then
      // 1. first init parent class recursively
      // 2. initialize traits according to linearization order
      val superParent = tpl.parents.head
      val superCls = superParent.tpe.classSymbol.asClass
      initParent(superParent)

      val parents = tpl.parents.tail
      val mixins = klass.baseClasses.tail.takeWhile(_ != superCls)
      mixins.reverse.foreach { mixin =>
        parents.find(_.tpe.classSymbol == mixin) match
        case Some(parent) => initParent(parent)
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
          if ctor.exists then superCall(tref, ctor, superParent)
      }


    // class body
    tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) =>
        val res = eval(vdef.rhs, thisV, klass, cacheResult = true)
        errorBuffer ++= res.errors
        thisV.updateField(vdef.symbol, res.value)

      case _: MemberDef =>

      case tree =>
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
