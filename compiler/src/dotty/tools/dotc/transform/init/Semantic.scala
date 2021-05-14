package dotty.tools.dotc
package transform
package init

import core._
import Contexts._
import Symbols._
import Types._
import StdNames._

import ast.tpd._
import util.SourcePosition
import config.Printers.{ init => printer }
import reporting.trace

import Errors._
import Util._

import scala.collection.mutable

class Semantic {

// ----- Domain definitions --------------------------------

  /** Locations are finite for any givn program */
  type Loc   = SourcePosition

  /** Abstract values
   *
   * Value = Hot | Cold | Addr | Fun | RefSet
   *
   * `Warm` and `This` will be addresses refer to the abstract heap
   */
  trait Value {
    def show: String = this.toString()
  }

  /** A transitively initialized object */
  case object Hot extends Value

  /** An object with unknown initialization status */
  case object Cold extends Value

  /** Addresses to the abstract heap
  *
  * Addresses determine abstractions of objects. Objects created
  * with same address are represented with the same abstraction.
  *
  * Nested addresses may lead to infinite domain, thus widen is
  * needed to finitize addresses. E.g. OOPSLA 2020 paper restricts
  * args to be either `Hot` or `Cold`
  */
  case class Addr(klass: ClassSymbol, outer: Value) extends Value

  /** A function value */
  case class Fun(expr: Tree, thisV: Addr, klass: ClassSymbol) extends Value

  /** A value which represents a set of addresses
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: List[Addr | Fun]) extends Value

  /** Object stores abstract values for all fields and the outer.
   *
   *  Theoretically we only need to store the outer for the concrete class,
   *  as all other outers are determined.
   *
   *  From performance reasons, we cache the immediate outer for all classes
   *  in the inheritance hierarchy.
   */
  case class Objekt(klass: ClassSymbol, fields: Map[Symbol, Value], outers: Map[ClassSymbol, Value])

  /** Abstract heap stores abstract objects
   *
   * As in the OOPSLA paper, the abstract heap is monotonistic
   */
  type Heap = mutable.Map[Addr, Objekt]

  /** The heap for abstract objects
   *
   *  As the heap is monotonistic, we can avoid passing it around.
   */
  val heap: Heap = mutable.Map.empty[Addr, Objekt]

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
  case class Config(thisV: Value, loc: Loc)

  /** Cache used to terminate the analysis
   *
   * A finitary configuration is not enough for the analysis to
   * terminate.  We need to use cache to let the interpreter "know"
   * that it can terminate.
   */
  type Cache = mutable.Map[Config, Value]
  val cache: Cache = mutable.Map.empty[Config, Value]

  /** Result of abstract interpretation */
  case class Result(value: Value, errors: Seq[Error]) {
    def show(using Context) = ???

    def ++(errors: Seq[Error]): Result = this.copy(errors = this.errors ++ errors)

    def +(error: Error): Result = this.copy(errors = this.errors :+ error)

    def ensureHot(msg: String, source: Tree): Result =
      if value == Hot then this
      else
        // TODO: define a new error
        this + PromoteCold(source, Vector(source))

    def select(f: Symbol, source: Tree)(using Context): Result =
      value.select(f, source) ++ errors

    def call(meth: Symbol, superType: Type, source: Tree)(using Context): Result =
      value.call(meth, superType, source) ++ errors

    def instantiate(klass: ClassSymbol, ctor: Symbol, source: Tree)(using Context): Result =
      value.instantiate(klass, ctor, source) ++ errors
  }

  val noErrors = Nil

// ----- Operations on domains -----------------------------
  extension (a: Value)
    def join(b: Value): Value =
      (a, b) match
      case (Hot, _)  => b
      case (_, Hot)  => a

      case (Cold, _) => Cold
      case (_, Cold) => Cold

      case (a: (Fun | Addr), b: (Fun | Addr)) => RefSet(a :: b :: Nil)

      case (a: (Fun | Addr), RefSet(refs))    => RefSet(a :: refs)
      case (RefSet(refs), b: (Fun | Addr))    => RefSet(b :: refs)

      case (RefSet(refs1), RefSet(refs2))     => RefSet(refs1 ++ refs2)

  extension (values: Seq[Value])
    def join: Value = values.reduce { (v1, v2) => v1.join(v2) }

  extension (value: Value)
    def select(f: Symbol, source: Tree)(using Context): Result =
      value match {
        case Hot  =>
          Result(Hot, noErrors)

        case Cold =>
          val error = AccessCold(f, source, Vector(source))
          Result(Hot, error :: Nil)

        case addr: Addr =>
          val obj = heap(addr)
          if obj.fields.contains(f) then
            Result(obj.fields(f), Nil)
          else
            val error = AccessNonInit(f, Vector(source))
            Result(Hot, error :: Nil)

        case _: Fun =>
          ???

        case RefSet(refs) =>
          val resList = refs.map(_.select(f, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }

    def call(meth: Symbol, superType: Type, source: Tree)(using Context): Result =
      value match {
        case Hot  =>
          Result(Hot, noErrors)

        case Cold =>
          val error = CallCold(meth, source, Vector(source))
          Result(Hot, error :: Nil)

        case addr: Addr =>
          val obj = heap(addr)
          val target =
            if superType.exists then
              // TODO: superType could be A & B when there is self-annotation
              resolveSuper(obj.klass, superType.classSymbol.asClass, meth)
            else
              resolve(obj.klass, meth)
          if target.isPrimaryConstructor then
            init(addr.klass, addr)
          else if target.isOneOf(Flags.Method | Flags.Lazy) then
            if target.hasSource then
              val rhs = target.defTree.asInstanceOf[DefDef].rhs
              eval(rhs, addr, target.owner.asClass)
            else
              val error = CallUnknown(target, source, Vector(source))
              Result(Hot, error :: Nil)
          else
            if obj.fields.contains(target) then
              Result(obj.fields(target), Nil)
            else
              val error = AccessNonInit(target, Vector(source))
              Result(Hot, error :: Nil)

        case Fun(body, thisV, klass) =>
          if meth.name == nme.apply then eval(body, thisV, klass)
          else if meth.name.toString == "tupled" then Result(value, Nil)
          else Result(Hot, Nil) // TODO: refine

        case RefSet(refs) =>
          val resList = refs.map(_.call(meth, superType, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }

    def instantiate(klass: ClassSymbol, ctor: Symbol, source: Tree)(using Context): Result =
      value match {
        case Hot  =>
          Result(Hot, noErrors)

        case Cold =>
          val error = CallCold(ctor, source, Vector(source))
          Result(Hot, error :: Nil)

        case addr: Addr =>
          // widen the outer to finitize addresses
          val outer = if addr.outer.isInstanceOf[Addr] then addr.copy(outer = Cold) else addr
          val addr2 = Addr(klass, outer)
          if !heap.contains(addr2) then
            heap(addr2) = Objekt(klass, Map.empty, Map(klass -> outer))
          addr2.call(ctor, superType = NoType, source)

        case Fun(body, thisV, klass) =>
          ??? // impossible

        case RefSet(refs) =>
          val resList = refs.map(_.instantiate(klass, ctor, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, errors)
      }
  end extension

  extension (addr: Addr)
    def updateOuter(klass: ClassSymbol, value: Value): Unit =
      val obj = heap(addr)
      val obj2 = obj.copy(outers = obj.outers.updated(klass, value))
      heap(addr) = obj2

    def updateField(field: Symbol, value: Value): Unit =
      val obj = heap(addr)
      val obj2 = obj.copy(fields = obj.fields.updated(field, value))
      heap(addr) = obj2
  end extension


// ----- Semantic definition --------------------------------

  /** Evaluate an expression with the given value for `this` in a given class `klass`
   *
   * This method only handles cache logic and delegates the work to `cases`.
   */
  def eval(expr: Tree, thisV: Value, klass: ClassSymbol)(using Context): Result = trace("evaluating " + expr.show, printer, res => res.asInstanceOf[Result].show) {
    val cfg = Config(thisV, expr.sourcePos)
    if (cache.contains(cfg)) Result(cache(cfg), noErrors)
    else {
      // no need to compute fix-point, because
      // 1. the result is decided by `cfg` for a legal program
      //    (heap change is irrelevant thanks to monotonicity)
      // 2. errors will have been reported for an illegal program
      cache(cfg) = Hot
      val res = cases(expr, thisV, klass)
      cache(cfg) = res.value
      res
    }
  }

  /** Evaluate a list of expressions */
  def eval(exprs: List[Tree], thisV: Value, klass: ClassSymbol)(using Context): List[Result] =
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   */
  def cases(expr: Tree, thisV: Value, klass: ClassSymbol)(using Context): Result =
    expr match {
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Result(Hot, noErrors)

      case Ident(name) =>
        assert(name.isTermName, "type trees should not reach here")
        cases(expr.tpe, thisV, klass, expr)

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val args = argss.flatten
        val ress = args.map { arg =>
          eval(arg, thisV, klass).ensureHot("May use initialized value as arguments", arg)
        }
        val errors = ress.flatMap(_.errors)

        val cls = tref.classSymbol.asClass
        val res = outerValue(tref, thisV, klass, tpt)
        (res ++ errors).instantiate(cls, ctor, expr)

      case Call(ref, argss) =>
        // check args
        val args = argss.flatten
        val ress = args.map { arg =>
          eval(arg, thisV, klass).ensureHot("May use initialized value as arguments", arg)
        }
        val errors = ress.flatMap(_.errors)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe
          val thisValue2 = resolveThis(thisTp.classSymbol.asClass, thisV, klass)
          Result(thisValue2, errors).call(ref.symbol, superTp, expr)

        case Select(qual, _) =>
          val res = eval(qual, thisV, klass) ++ errors
          res.call(ref.symbol, superType = NoType, source = expr)

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = resolveThis(enclosingClass, thisV, klass)
            thisValue2 match
            case Hot => Result(Hot, errors)
            case _ =>
              val rhs = id.symbol.defTree.asInstanceOf[DefDef].rhs
              eval(rhs, thisValue2, enclosingClass)
          case TermRef(prefix, _) =>
            val res = cases(prefix, thisV, klass, id) ++ errors
            res.call(id.symbol, superType = NoType, source = expr)

      case Select(qualifier, name) =>
        eval(qualifier, thisV, klass).select(expr.symbol, expr)

      case _: This =>
        cases(expr.tpe, thisV, klass, expr)

      case Literal(_) =>
        Result(Hot, noErrors)

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Hot, noErrors)
        else eval(expr, thisV, klass)

      case NamedArg(name, arg) =>
        eval(arg, thisV, klass)

      case Assign(lhs, rhs) =>
        lhs match
        case Select(qual, _) =>
          val res = eval(qual, thisV, klass)
          eval(rhs, thisV, klass).ensureHot("May only assign initialized value", rhs) ++ res.errors
        case id: Ident =>
          eval(rhs, thisV, klass).ensureHot("May only assign initialized value", rhs)

      case closureDef(ddef) =>
        thisV match
        case addr: Addr =>
          val value = Fun(ddef.rhs, addr, klass)
          Result(value, Nil)
        case _ =>
          ??? // impossible

      case Block(stats, expr) =>
        val ress = eval(stats, thisV, klass)
        eval(expr, thisV, klass) ++ ress.flatMap(_.errors)

      case If(cond, thenp, elsep) =>
        val ress = eval(cond :: thenp :: elsep :: Nil, thisV, klass)
        val value = ress.map(_.value).join
        val errors = ress.flatMap(_.errors)
        Result(value, errors)

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Hot, noErrors)
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        val res1 = eval(selector, thisV, klass).ensureHot("The value to be matched needs to be initialized", selector)
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
          eval(elem, thisV, klass).ensureHot("May only use initialized value as arguments", elem)
        }
        Result(Hot, ress.flatMap(_.errors))

      case Inlined(call, bindings, expansion) =>
        val ress = eval(bindings, thisV, klass)
        eval(expansion, thisV, klass) ++ ress.flatMap(_.errors)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Result(Hot, noErrors)

      case vdef : ValDef =>
        // local val definition
        // TODO: support explicit @cold annotation for local definitions
        eval(vdef.rhs, thisV, klass).ensureHot("Local definitions may only hold initialized values", vdef)

      case ddef : DefDef =>
        // local method
        Result(Hot, noErrors)

      case tdef: TypeDef =>
        // local type definition
        Result(Hot, noErrors)

      case _: Import | _: Export =>
        Result(Hot, noErrors)

      case _ =>
        throw new Exception("unexpected tree: " + expr.show)
    }

  /** Handle semantics of leaf nodes */
  def cases(tp: Type, thisV: Value, klass: ClassSymbol, source: Tree)(using Context): Result = trace("evaluating " + tp.show, printer, res => res.asInstanceOf[Result].show) {
    tp match {
      case _: ConstantType =>
        Result(Hot, noErrors)

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        Result(Hot, noErrors)

      case tmref: TermRef =>
        cases(tmref.prefix, thisV, klass, source).select(tmref.symbol, source)

      case tp @ ThisType(tref) =>
        if tref.symbol.is(Flags.Package) then Result(Hot, noErrors)
        else
          val value =
            thisV match
            case Hot => Hot
            case addr: Addr =>
              resolveThis(tp.classSymbol.asClass, addr, klass)
            case _ => ???
          Result(value, noErrors)

      case _: TermParamRef | _: RecThis  =>
        // possible from checking effects of types
        Result(Hot, noErrors)

      case _ =>
        throw new Exception("unexpected type: " + tp)
    }
  }

  /** Resolve C.this that appear in `klass` */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol)(using Context): Value = trace("resolving " + target.show + ", this = " + thisV.show + " in " + klass.show, printer, res => res.asInstanceOf[Value].show) {
    if target == klass then thisV
    else
      thisV match
        case Hot => Hot
        case thisV: Addr =>
          val outer = heap(thisV).outers.getOrElse(klass, Hot)
          val outerCls = klass.owner.enclosingClass.asClass
          resolveThis(target, outer, outerCls)
        case _ => ???
  }

  /** Compute the outer value that correspond to `tref.prefix` */
  def outerValue(tref: TypeRef, thisV: Value, klass: ClassSymbol, source: Tree)(using Context): Result =
    val cls = tref.classSymbol.asClass
    if (tref.prefix == NoPrefix) then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      val outerV = resolveThis(enclosing, thisV, klass)
      Result(outerV, noErrors)
    else
      cases(tref.prefix, thisV, klass, source)

  /** Initialize part of an abstract object in `klass` of the inheritance chain */
  def init(klass: ClassSymbol, thisV: Addr)(using Context): Result =
    val errorBuffer = new mutable.ArrayBuffer[Error]

    val tpl = klass.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]

    // init param fields
    var obj = heap(thisV)
    klass.paramAccessors.foreach { acc =>
      if (!acc.is(Flags.Method)) {
        traceIndented(acc.show + " initialized", printer)
        obj = obj.copy(fields = obj.fields.updated(acc, Hot))
      }
    }
    heap(thisV) = obj

    def superCall(tref: TypeRef, ctor: Symbol, source: Tree): Unit =
      // update outer for super class
      val cls = tref.classSymbol.asClass
      val res = outerValue(tref, thisV, klass, source)
      errorBuffer ++= res.errors
      thisV.updateOuter(cls, res.value)

      // follow constructor
      val res2 = thisV.call(ctor, superType = NoType, source)
      errorBuffer ++= res2.errors

    // parents
    def initParent(parent: Tree) = parent match {
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>
        eval(stats, thisV, klass).foreach { res => errorBuffer ++= res.errors }
        argss.flatten.foreach { arg =>
          val res = eval(arg, thisV, klass)
          res.ensureHot("Argument must be an initialized value", arg)
          errorBuffer ++ res.errors
        }
        superCall(tref, ctor, tree)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>
        argss.flatten.foreach { arg =>
          val res = eval(arg, thisV, klass)
          res.ensureHot("Argument must be an initialized value", arg)
          errorBuffer ++ res.errors
        }
        superCall(tref, ctor, tree)

      case ref: RefTree =>
        val tref = ref.tpe.asInstanceOf[TypeRef]
        superCall(tref, tref.classSymbol.primaryConstructor, ref)
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
          val tref = typeRefOf(klass.typeRef.baseType(mixin).typeConstructor)
          superCall(tref, tref.classSymbol.primaryConstructor, superParent)
      }


    // class body
    tpl.body.foreach {
      case vdef : ValDef =>
        val res = eval(vdef.rhs, thisV, klass)
        errorBuffer ++ res.errors
        thisV.updateField(vdef.symbol, res.value)

      case _: MemberDef =>

      case tree =>
        eval(tree, thisV, klass)
    }

    Result(thisV, errorBuffer.toList)

// ----- Utility methods and extractors --------------------------------

  def typeRefOf(tp: Type)(using Context): TypeRef = tp.dealias.typeConstructor match {
    case tref: TypeRef => tref
    case hklambda: HKTypeLambda => typeRefOf(hklambda.resType)
  }

  object Call {
    def unapply(tree: Tree)(using Context): Option[(Tree, List[List[Tree]])] =
      tree match
      case Apply(fn, args) =>
        unapply(fn) match
        case Some((ref, args0)) => Some((ref, args0 :+ args))
        case None => None

      case TypeApply(fn, targs) =>
        unapply(fn)

      case ref: RefTree if ref.symbol.is(Flags.Method) =>
        Some((ref, Nil))
  }

  object NewExpr {
    def unapply(tree: Tree)(using Context): Option[(TypeRef, New, Symbol, List[List[Tree]])] =
      tree match
      case Call(fn @ Select(newTree: New, init), argss) if init == nme.CONSTRUCTOR =>
        val tref = typeRefOf(newTree.tpe)
        Some((tref, newTree, fn.symbol, argss))
      case _ => None
  }
}
