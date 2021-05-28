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

class Objects {
  import Semantic._

// ----- Domain definitions --------------------------------

  /** Abstract values
   *
   *  Value = Cold | ObjectRef | Instance | Fun | RefSet
   *
   *  `RefSet` represents a set of values which could be `ObjectRef`,
   *  `Instance` or `Fun`. The following ordering applies for RefSet:
   *
   *         R_a ⊑ R_b if R_a ⊆ R_b
   *
   *         V ⊑ R if V ∈ R
   *
   */
  sealed abstract class Value {
    def show: String = this.toString()
  }

  /** An unknown object */
  case object Cold extends Value

  sealed abstract class Addr extends Value {
    def klass: ClassSymbol
  }

  /** A reference to a static object
   */
  case class ObjectRef(klass: ClassSymbol) extends Addr

  /** An instance of class */
  case class Instance(klass: ClassSymbol, outer: Value, ctor: Symbol, args: List[Value]) extends Addr

  /** A function value */
  case class Fun(expr: Tree, thisV: Addr, klass: ClassSymbol, env: Env) extends Value

  /** A value which represents a set of addresses
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: List[Fun | Addr]) extends Value

  val Bottom: Value = RefSet(Nil)

  // end of value definition

  /** The abstract object which stores value about its fields and immediate outers.
   *
   *  Semantically it suffices to store the outer for `klass`. We cache other outers
   *  for performance reasons.
   *
   *  Note: Object is NOT a value.
   */
  case class Objekt(klass: ClassSymbol, fields: mutable.Map[Symbol, Value], outers: mutable.Map[ClassSymbol, Value])

  /** The environment for method parameters */
  object Env {
    opaque type Env = Map[Symbol, Value]

    val empty: Env = Map.empty

    def apply(bindings: Map[Symbol, Value]): Env = bindings

    def apply(ddef: DefDef, args: List[Value])(using Context): Env =
      val params = ddef.termParamss.flatten.map(_.symbol)
      assert(args.size == params.size, "arguments = " + args.size + ", params = " + params.size)
      params.zip(args).toMap

    extension (env: Env)
      def lookup(sym: Symbol): Value = env(sym)

      def union(other: Env): Env = env ++ other

      /** Widen the environment for closures */
      def widenForClosure: Env =
        def widen(v: Value): Value = v.match
          case RefSet(refs) => refs.map(widen(_)).join
          case Instance(_, outer, _, args) =>
            val nested = (outer :: args).exists(_.isInstanceOf[Instance])
            if nested then Cold
            else v
          case _: Fun => Cold
          case _ => v

        env.map { (k, v) => k -> widen(v) }
  }

  type Env = Env.Env
  def env(using env: Env) = env

  import Env._

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
    def show(using Context) = value.show + ", errors = " + errors.map(_.getClass.getName)

    def ++(errors: Seq[Error]): Result = this.copy(errors = this.errors ++ errors)

    def +(error: Error): Result = this.copy(errors = this.errors :+ error)

    def select(f: Symbol, source: Tree): Contextual[Result] =
      value.select(f, source) ++ errors

    def call(meth: Symbol, args: List[Value], superType: Type, source: Tree): Contextual[Result] =
      value.call(meth, args, superType, source) ++ errors

    def instantiate(klass: ClassSymbol, ctor: Symbol, args: List[Value], source: Tree): Contextual[Result] =
      value.instantiate(klass, ctor, args, source) ++ errors
  }

  /** The state that threads through the interpreter */
  type Contextual[T] = (Env, Context, Trace, Path) ?=> T

  inline def use[T, R](v: T)(inline op: T ?=> R): R = op(using v)

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

  object Path {
    opaque type Path = Vector[ClassSymbol]

    val empty: Path = Vector.empty

    extension (path: Path)
      def add(node: ClassSymbol): Path = path :+ node
      def from(node: ClassSymbol): Vector[ClassSymbol] = path.dropWhile(_ != node)
  }

  type Path = Path.Path

  import Path._
  def path(using p: Path): Path = p

// ----- Operations on domains -----------------------------
  extension (a: Value)
    def join(b: Value): Value =
      (a, b) match
      case (Bottom, _)  => b
      case (_, Bottom)  => a

      case (Cold, _) => Cold
      case (_, Cold) => Cold

      case (a: (Fun | Addr), b: (Fun | Addr)) => RefSet(a :: b :: Nil)

      case (a: (Fun | Addr), RefSet(refs))    => RefSet(a :: refs)
      case (RefSet(refs), b: (Fun | Addr))    => RefSet(b :: refs)

      case (RefSet(refs1), RefSet(refs2))     => RefSet(refs1 ++ refs2)

  extension (values: Seq[Value])
    def join: Value =
      if values.isEmpty then Bottom
      else values.reduce { (v1, v2) => v1.join(v2) }

  extension (value: Value)
    def select(field: Symbol, source: Tree, needResolve: Boolean = true): Contextual[Result] =
      value match {
        case Bottom  =>
          Result(Bottom, Errors.empty)

        case Cold =>
          val error = AccessCold(field, source, trace.toVector)
          Result(Bottom, error :: Nil)

        case addr: Addr =>
          val target = if needResolve then resolve(addr.klass, field) else field
          val trace1 = trace.add(source)
          if target.is(Flags.Lazy) then
            given Trace = trace1
            val rhs = target.defTree.asInstanceOf[ValDef].rhs
            eval(rhs, addr, target.owner.asClass, cacheResult = true)
          else
            given Trace = trace1
            val obj = heap(addr)
            if obj.fields.contains(target) then
              Result(obj.fields(target), Nil)
            else if target.is(Flags.ParamAccessor) then
              // possible for trait parameters
              // see tests/init/neg/trait2.scala
              //
              // return `Bottom` here, errors are reported in checking `ThisRef`
              Result(Bottom, Nil)
            else if target.hasSource then
              val rhs = target.defTree.asInstanceOf[ValOrDefDef].rhs
              eval(rhs, addr, target.owner.asClass, cacheResult = true)
            else
              val error = CallUnknown(field, source, trace.toVector)
              Result(Bottom, error :: Nil)

        case _: Fun =>
          ???

        case RefSet(refs) =>
          val resList = refs.map(_.select(field, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }

    def call(meth: Symbol, args: List[Value], superType: Type, source: Tree, needResolve: Boolean = true): Contextual[Result] =
      value match {
        case Bottom  =>
          Result(Bottom, Errors.empty)

        case Cold =>
          val error = CallCold(meth, source, trace.toVector)
          Result(Bottom, error :: Nil)

        case addr: Addr =>
          val target =
            if !needResolve then
              meth
            else if superType.exists then
              resolveSuper(addr.klass, superType, meth)
            else
              resolve(addr.klass, meth)

          val trace1 = trace.add(source)
          if target.isOneOf(Flags.Method) then
            if target.hasSource then
              given Trace = trace1
              val cls = target.owner.enclosingClass.asClass
              val ddef = target.defTree.asInstanceOf[DefDef]
              given Env = Env(ddef, args)
              if target.isPrimaryConstructor then
                val tpl = cls.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
                eval(tpl, addr, cls, cacheResult = true)(using env, ctx, trace.add(cls.defTree))
              else
                eval(ddef.rhs, addr, cls, cacheResult = true)
            else if addr.canIgnoreMethodCall(target) then
              Result(Bottom, Nil)
            else
              val error = CallUnknown(target, source, trace.toVector)
              Result(Bottom, error :: Nil)
          else
            val obj = heap(addr)
            if obj.fields.contains(target) then
              Result(obj.fields(target), Nil)
            else
              value.select(target, source, needResolve = false)

        case Fun(body, thisV, klass, env2) =>
          // meth == NoSymbol for poly functions
          if meth.name.toString == "tupled" then Result(value, Nil) // a call like `fun.tupled`
          else
            use(env.union(env2)) {
              eval(body, thisV, klass, cacheResult = true)
            }

        case RefSet(refs) =>
          val resList = refs.map(_.call(meth, args, superType, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }

    /** Handle a new expression `new p.C` where `p` is abstracted by `value` */
    def instantiate(klass: ClassSymbol, ctor: Symbol, args: List[Value], source: Tree): Contextual[Result] =
      val trace1 = trace.add(source)
      value match {
        case Cold =>
          given Trace = trace1
          val error = CallCold(ctor, source, trace.toVector)
          Result(Bottom, error :: Nil)

        case _: Addr | Bottom =>
          // widen the outer and args to finitize addresses
          extension (value: Value)
            def widen: Value = value match
              case RefSet(refs) => refs.map(_.widen).join
              case Instance(_, outer, _, args) =>
                val nested = (outer :: args).exists(_.isInstanceOf[Instance])
                if nested then Cold
                else value
              case _: Fun => Cold
              case _ => value

          given Trace = trace1
          val outerWidened = value.widen
          val argsWidened = args.map(_.widen)
          val addr =
            if klass.isStaticObjectRef then
              ObjectRef(klass)
            else
              Instance(klass, outerWidened, ctor, argsWidened)
          if !heap.contains(addr) then
            val obj = Objekt(klass, fields = mutable.Map.empty, outers = mutable.Map(klass -> outerWidened))
            heap.update(addr, obj)
          val res = addr.call(ctor, argsWidened, superType = NoType, source)
          Result(addr, res.errors)

        case Fun(body, thisV, klass, _) =>
          ??? // impossible

        case RefSet(refs) =>
          val resList = refs.map(_.instantiate(klass, ctor, args, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }
  end extension

  extension (obj: ObjectRef)
    def access(source: Tree): Contextual[Result] =
      val cycle = path.from(obj.klass)
      if cycle.nonEmpty then
        val classDef = obj.klass.defTree
        var trace1 = trace.toVector.dropWhile(_ != classDef) :+ source
        val warning =
          if cycle.size > 1 then
            CyclicObjectInit(cycle, trace1)
          else
            ObjectLeakDuringInit(obj.klass, trace1)
        Result(obj, warning :: Nil)
      else if obj.klass.is(Flags.JavaDefined) then
        // Errors will be reported for method calls on it
        Result(Bottom, Nil)
      else
        use(path.add(obj.klass)) {
          Bottom.instantiate(obj.klass, obj.klass.primaryConstructor, Nil, source)
        }

// ----- Policies ------------------------------------------
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
  end extension

  /** Check a static objet
   *
   *  @param cls the module class of the static object
   */
  def check(cls: ClassSymbol, tpl: Template)(using Context): Unit = {
    val objRef = ObjectRef(cls)
    val obj = Objekt(cls, fields = mutable.Map.empty, outers = mutable.Map.empty)
    given Path = Path.empty
    given Trace = Trace.empty
    given Env = Env.empty
    heap.update(objRef, obj)
    val res = objRef.access(tpl)
    res.errors.foreach(_.issue)
  }

// ----- Semantic definition -------------------------------

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
      innerMap(expr) = Bottom
      val res = cases(expr, thisV, klass)
      if cacheResult then innerMap(expr) = res.value else innerMap.remove(expr)
      res
    }
  }

  /** Evaluate a list of expressions */
  def eval(exprs: List[Tree], thisV: Addr, klass: ClassSymbol): Contextual[List[Result]] =
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Evaluate arguments of methods */
  def evalArgs(args: List[Arg], thisV: Addr, klass: ClassSymbol): Contextual[List[Result]] =
    args.map { arg =>
      if arg.isByName then
        val fun = Fun(arg.tree, thisV, klass, env.widenForClosure)
        Result(fun, Nil)
      else
        eval(arg.tree, thisV, klass)
    }

  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   */
  def cases(expr: Tree, thisV: Addr, klass: ClassSymbol): Contextual[Result] =
    expr match {
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Result(Bottom, Errors.empty)

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        cases(expr.tpe, thisV, klass, expr)

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val resArgs = evalArgs(argss.flatten, thisV, klass)
        val argsValues = resArgs.map(_.value)
        val argsErrors = resArgs.flatMap(_.errors)

        val cls = tref.classSymbol.asClass
        val res = outerValue(tref, thisV, klass, tpt)
        (res ++ argsErrors).instantiate(cls, ctor, argsValues, expr)

      case Call(ref, argss) =>
        // check args
        val resArgs = evalArgs(argss.flatten, thisV, klass)
        val argsValues = resArgs.map(_.value)
        val argsErrors = resArgs.flatMap(_.errors)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe
          val thisValue2 = resolveThis(thisTp.classSymbol.asClass, thisV, klass, ref)
          Result(thisValue2, argsErrors).call(ref.symbol, argsValues, superTp, expr)

        case Select(qual, _) =>
          val res = eval(qual, thisV, klass) ++ argsErrors
          res.call(ref.symbol, argsValues, superType = NoType, source = expr)

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = resolveThis(enclosingClass, thisV, klass, id)
            // local methods are not a member, but we can reuse the method `call`
            Result(thisValue2, argsErrors).call(id.symbol, argsValues, superType = NoType, expr)
          case TermRef(prefix, _) =>
            val res = cases(prefix, thisV, klass, id) ++ argsErrors
            res.call(id.symbol, argsValues, superType = NoType, source = expr)

      case Select(qualifier, name) =>
        eval(qualifier, thisV, klass).select(expr.symbol, expr)

      case _: This =>
        cases(expr.tpe, thisV, klass, expr)

      case Literal(_) =>
        Result(Bottom, Errors.empty)

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Bottom, Errors.empty)
        else eval(expr, thisV, klass)

      case NamedArg(name, arg) =>
        eval(arg, thisV, klass)

      case Assign(lhs, rhs) =>
        lhs match
        case Select(qual, _) =>
          val res = eval(qual, thisV, klass)
          eval(rhs, thisV, klass) ++ res.errors
        case id: Ident =>
          eval(rhs, thisV, klass)

      case closureDef(ddef) =>
        val value = Fun(ddef.rhs, thisV, klass, env.widenForClosure)
        Result(value, Nil)

      case PolyFun(body) =>
        val value = Fun(body, thisV, klass, env.widenForClosure)
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
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Bottom, Errors.empty)
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        // TODO: handle extractors
        val res1 = eval(selector, thisV, klass)
        val ress = eval(cases.map(_.body), thisV, klass)
        val value = ress.map(_.value).join
        val errors = res1.errors ++ ress.flatMap(_.errors)
        Result(value, errors)

      case Return(expr, from) =>
        // TODO: handle return by writing the interpreter in CPS
        eval(expr, thisV, klass)

      case WhileDo(cond, body) =>
        val ress = eval(cond :: body :: Nil, thisV, klass)
        Result(Bottom, ress.flatMap(_.errors))

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
          eval(elem, thisV, klass)
        }
        Result(Cold, ress.flatMap(_.errors))

      case Inlined(call, bindings, expansion) =>
        val ress = eval(bindings, thisV, klass)
        eval(expansion, thisV, klass) ++ ress.flatMap(_.errors)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Result(Bottom, Errors.empty)

      case vdef : ValDef =>
        // local val definition
        eval(vdef.rhs, thisV, klass, cacheResult = true)

      case ddef : DefDef =>
        // local method
        Result(Bottom, Errors.empty)

      case tdef: TypeDef =>
        // local type definition
        Result(Bottom, Errors.empty)

      case tpl: Template =>
        init(tpl, thisV, klass)

      case _: Import | _: Export =>
        Result(Bottom, Errors.empty)

      case _ =>
        throw new Exception("unexpected tree: " + expr.show)
    }

  /** Handle semantics of leaf nodes */
  def cases(tp: Type, thisV: Addr, klass: ClassSymbol, source: Tree): Contextual[Result] = log("evaluating " + tp.show, printer, res => res.asInstanceOf[Result].show) {
    tp match {
      case _: ConstantType =>
        Result(Bottom, Errors.empty)

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        // - only var definitions are cold, it means they are not inspected
        // - look up parameters from environment
        // - evaluate the rhs of the local definition for val definitions: they are already cached
        val sym = tmref.symbol
        if sym.is(Flags.Param) then Result(env.lookup(sym), Nil)
        else if sym.is(Flags.Mutable) then Result(Cold, Nil)
        else if sym.is(Flags.Package) then Result(Bottom, Nil)
        else if sym.hasSource then
          val rhs = sym.defTree.asInstanceOf[ValDef].rhs
          eval(rhs, thisV, klass, cacheResult = true)
        else
          Result(Bottom, CallUnknown(sym, source, trace.toVector) :: Nil)

      case tmref: TermRef =>
        val sym = tmref.symbol
        if sym.isStaticObjectRef then
          ObjectRef(sym.moduleClass.asClass).access(source)
        else
          cases(tmref.prefix, thisV, klass, source).select(tmref.symbol, source)

      case tp @ ThisType(tref) =>
        val sym = tref.symbol
        if sym.is(Flags.Package) then Result(Bottom, Errors.empty)
        else if sym.isStaticObjectRef && sym != klass then
          ObjectRef(sym.moduleClass.asClass).access(source)
        else
          val value = resolveThis(tref.classSymbol.asClass, thisV, klass, source)
          Result(value, Errors.empty)

      case _: TermParamRef | _: RecThis  =>
        // possible from checking effects of types
        Result(Bottom, Errors.empty)

      case _ =>
        throw new Exception("unexpected type: " + tp)
    }
  }

  /** Resolve C.this that appear in `klass` */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol, source: Tree): Contextual[Value] = log("resolving " + target.show + ", this = " + thisV.show + " in " + klass.show, printer, res => res.asInstanceOf[Value].show) {
    if target == klass then thisV
    else if target.is(Flags.Package) || target.isStaticOwner then Bottom
    else
      thisV match
        case Bottom => Bottom
        case addr: Addr =>
          val obj = heap(addr)
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
      if cls.isAllOf(Flags.JavaInterface) then Result(Bottom, Nil)
      else cases(tref.prefix, thisV, klass, source)

  /** Initialize part of an abstract object in `klass` of the inheritance chain */
  def init(tpl: Template, thisV: Addr, klass: ClassSymbol): Contextual[Result] = log("init " + klass.show, printer, res => res.asInstanceOf[Result].show) {
    val errorBuffer = new mutable.ArrayBuffer[Error]

    val paramsMap = tpl.constr.termParamss.flatten.map(vdef => vdef.name -> vdef.symbol).toMap

    // init param fields
    klass.paramGetters.foreach { acc =>
      printer.println(acc.show + " initialized")
      thisV.updateField(acc, env.lookup(paramsMap(acc.name.toTermName)))
    }

    def superCall(tref: TypeRef, ctor: Symbol, args: List[Value], source: Tree): Unit =
      val cls = tref.classSymbol.asClass
      // update outer for super class
      val res = outerValue(tref, thisV, klass, source)
      errorBuffer ++= res.errors
      thisV.updateOuter(cls, res.value)

      // follow constructor
      if cls.hasSource then
        use(trace.add(source)) {
          val res2 = thisV.call(ctor, args, superType = NoType, source)
          errorBuffer ++= res2.errors
        }

    // parents
    def initParent(parent: Tree) = parent match {
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        eval(stats, thisV, klass).foreach { res => errorBuffer ++= res.errors }
        val resArgs = evalArgs(argss.flatten, thisV, klass)
        val argsValues = resArgs.map(_.value)
        val argsErrors = resArgs.flatMap(_.errors)

        errorBuffer ++= argsErrors
        superCall(tref, ctor, argsValues, tree)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>       // extends A(args)
        val resArgs = evalArgs(argss.flatten, thisV, klass)
        val argsValues = resArgs.map(_.value)
        val argsErrors = resArgs.flatMap(_.errors)

        errorBuffer ++= argsErrors
        superCall(tref, ctor, argsValues, tree)

      case _ =>   // extends A or extends A[T]
        val tref = typeRefOf(parent.tpe)
        superCall(tref, tref.classSymbol.primaryConstructor, Nil, parent)
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
          if ctor.exists then superCall(tref, ctor, Nil, superParent)
      }


    // class body
    tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) =>
        val res = eval(vdef.rhs, thisV, klass, cacheResult = true)
        errorBuffer ++= res.errors
        val fieldV = if vdef.symbol.is(Flags.Mutable) then Cold else res.value
        thisV.updateField(vdef.symbol, fieldV)

      case _: MemberDef =>

      case tree =>
        errorBuffer ++= eval(tree, thisV, klass).errors
    }

    Result(thisV, errorBuffer.toList)
  }

// ----- Utility methods ------------------------------------

  extension (sym: Symbol)
    def isStaticObjectRef(using Context) =
      !sym.is(Flags.Package) && sym.is(Flags.Module) && sym.isStatic
}
