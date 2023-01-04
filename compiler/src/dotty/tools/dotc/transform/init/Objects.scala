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
import reporting.trace as log

import Errors.*

import Semantic.{ NewExpr, Call, TraceValue, Trace, PolyFun, Arg, ByNameArg }

import Semantic.{ typeRefOf, hasSource, extendTrace, withTrace, trace }

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
 *  2. It is modular with respect to separate compilation, even incremental
 *     compilation.
 *
 *     When a value leaks beyond the boundary of analysis, we approximate by
 *     calling all methods of the value that overridding a method outside of
 *     the boundary.
 *
 *  3. It is receiver-sensitive but not heap-sensitive nor parameter-sensitive.
 *
 *     Fields and parameters are always abstracted by their types.
 *
 */
object Objects:

  // ----------------------------- abstract domain -----------------------------

  sealed abstract class Value:
    def show(using Context): String

  /**
   * Represents an external value
   *
   * An external value is usually an instance of a class of another project,
   * therefore it may not refer to objects defined in the current project. The
   * only possibility is that an internal value leaks to an external value. Such
   * leaking is strictly checked by the algorithm.
   */
  case object Ext extends Value:
    def show(using Context) = "Ext"

  /**
   * Rerepsents values that are instances of the specified class
   *
   * `tp.classSymbol` should be the concrete class of the value at runtime.
   */
  case class OfClass(tp: Type) extends Value:
    def show(using Context) = "OfClass(" + tp.show + ")"

  /**
   * Rerepsents values that are of the given type
   *
   * `OfType` is just a short-cut referring to currently instantiated sub-types.
   *
   * Note: this value should never be an index in the cache.
   */
  case class OfType(tp: Type) extends Value:
    def show(using Context) = "OfType(" + tp.show + ")"

  /**
   * Represents a lambda expression
   */
  case class Fun(expr: Tree, thisV: OfClass, klass: ClassSymbol) extends Value:
    def show(using Context) = "Fun(" + expr.show + ", " + thisV.show + ", " + klass.show + ")"

  /**
   * Represents a set of values
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: List[Fun | OfClass]) extends Value:
    def show(using Context) = refs.map(_.show).mkString("[", ",", "]")

  object State:
    /**
     * Remembers the instantiated types during instantiation of a static object.
     */
    class Data(allConcreteClasses: Set[ClassSymbol]):
      // objects under check
      val checkingObjects = new mutable.ArrayBuffer[ClassSymbol]

      // object -> (class, types of the class)
      val instantiatedTypes = mutable.Map.empty[ClassSymbol, Map[ClassSymbol, List[Type]]]

      // object -> (fun type, fun values)
      val instantiatedFuncs = mutable.Map.empty[ClassSymbol, Map[Type, List[Fun]]]

    opaque type Rep = Data

    def currentObject(using rep: Rep): ClassSymbol = rep.checkingObjects.last

    def init(classes: List[ClassSymbol])(using Context): Rep =
      val concreteClasses = classes.filter(Semantic.isConcreteClass).toSet
      new Data(concreteClasses)

  type Contextual[T] = (Context, State.Rep, Cache.Cache, Trace) ?=> T

  object Cache:
    /** Cache for method calls and lazy values
     *
     *  Symbol -> ThisValue -> ReturnValue
     */
    opaque type Cache = Map[Symbol, Map[OfClass, Value]]

    def get(thisV: OfClass, sym: Symbol)(fun: => Value)(using cache: Cache): Value =
      cache.get(sym).flatMap(_.get(thisV)) match
      case None =>
        val res = fun
        store(thisV, sym, res)
        res
      case Some(value) =>
        value

    private def store(thisV: OfClass, sym: Symbol, result: Value)(using cache: Cache): Unit =
      cache.updated(sym, cache.getOrElse(sym, Map.empty).updated(thisV, result))

    def fresh(): Cache = Map.empty
  end Cache

  // --------------------------- domain operations -----------------------------

  type ArgInfo = TraceValue[Value]

  extension (a: Value)
    def join(b: Value): Value = ???

  extension (values: Seq[Value])
    def join: Value = ???

  def call(thisV: Value, meth: Symbol, args: List[ArgInfo], receiver: Type, superType: Type, needResolve: Boolean = true): Contextual[Value] = ???

  def callConstructor(thisV: Value, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = ???

  def select(thisV: Value, field: Symbol, receiver: Type, needResolve: Boolean = true): Contextual[Value] = ???

  def instantiate(outer: Value, klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = ???

  // -------------------------------- algorithm --------------------------------

  /** Check an individual class
   *
   *  The class to be checked must be an instantiable concrete class.
   */
  private def checkObject(classSym: ClassSymbol): Contextual[Unit] =
    val tpl = classSym.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
    init(tpl, OfClass(classSym.typeRef), classSym)

  def checkClasses(classes: List[ClassSymbol])(using Context): Unit =
    given State.Rep = State.init(classes)
    given Cache.Cache = Cache.fresh()
    given Trace = Trace.empty

    for
      classSym <- classes
      if classSym.is(Flags.Module) && classSym.isStaticOwner
    do
      checkObject(classSym)

  /** Evaluate a list of expressions */
  def evalExprs(exprs: List[Tree], thisV: OfClass, klass: ClassSymbol): Contextual[List[Value]] =
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Handles the evaluation of different expressions
   *
   * @param expr   The expression to be evaluated.
   * @param thisV  The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass  The enclosing class where the expression `expr` is located.
   */
  def eval(expr: Tree, thisV: OfClass, klass: ClassSymbol): Contextual[Value] =
    expr match
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        OfType(defn.NothingType)

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        evalType(expr.tpe, thisV, klass)

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        val outer = outerValue(tref, thisV, klass)
        instantiate(outer, cls, ctor, args)

      case Call(ref, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe: @unchecked
          val thisValue2 = extendTrace(ref) { resolveThis(thisTp.classSymbol.asClass, thisV, klass) }
          call(thisValue2, ref.symbol, args, thisTp, superTp)

        case Select(qual, _) =>
          val receiver = eval(qual, thisV, klass)
          if ref.symbol.isConstructor then
            callConstructor(receiver, ref.symbol, args)
          else
            call(receiver, ref.symbol, args, receiver = qual.tpe, superType = NoType)

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = extendTrace(ref) { resolveThis(enclosingClass, thisV, klass) }
            // local methods are not a member, but we can reuse the method `call`
            call(thisValue2, id.symbol, args, receiver = NoType, superType = NoType, needResolve = false)
          case TermRef(prefix, _) =>
            val receiver = evalType(prefix, thisV, klass)
            if id.symbol.isConstructor then
              callConstructor(receiver, id.symbol, args)
            else
              call(receiver, id.symbol, args, receiver = prefix, superType = NoType)

      case Select(qualifier, name) =>
        val qual = eval(qualifier, thisV, klass)

        name match
          case OuterSelectName(_, _) =>
            val current = qualifier.tpe.classSymbol
            val target = expr.tpe.widenSingleton.classSymbol.asClass
            resolveThis(target, qual, current.asClass)
          case _ =>
            select(qual, expr.symbol, receiver = qualifier.tpe)

      case _: This =>
        evalType(expr.tpe, thisV, klass)

      case Literal(_) =>
        OfType(defn.NothingType)

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot))
          OfType(defn.NothingType)
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
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) OfType(defn.NothingType)
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        eval(selector, thisV, klass)
        evalExprs(cases.map(_.body), thisV, klass).join

      case Return(expr, from) =>
        eval(expr, thisV, klass)

      case WhileDo(cond, body) =>
        evalExprs(cond :: body :: Nil, thisV, klass)
        OfType(defn.NothingType)

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
        OfType(defn.NothingType)

      case vdef : ValDef =>
        // local val definition
        eval(vdef.rhs, thisV, klass)

      case ddef : DefDef =>
        // local method
        OfType(defn.NothingType)

      case tdef: TypeDef =>
        // local type definition
        OfType(defn.NothingType)

      case _: Import | _: Export =>
        OfType(defn.NothingType)

      case _ =>
        report.error("[Internal error] unexpected tree" + Trace.show, expr)
        OfType(expr.tpe)

  /** Handle semantics of leaf nodes
   *
   * For leaf nodes, their semantics is determined by their types.
   *
   * @param tp      The type to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `tp` is located.
   */
  def evalType(tp: Type, thisV: OfClass, klass: ClassSymbol): Contextual[Value] = log("evaluating " + tp.show, printer, (_: Value).show) {
    ???
  }

  /** Evaluate arguments of methods */
  def evalArgs(args: List[Arg], thisV: OfClass, klass: ClassSymbol): Contextual[List[ArgInfo]] =
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

  def init(tpl: Template, thisV: OfClass, klass: ClassSymbol): Contextual[Unit] =
    def superCall(tref: TypeRef, ctor: Symbol, args: List[ArgInfo]): Unit =
      val cls = tref.classSymbol.asClass

      // follow constructor
      if cls.hasSource then callConstructor(thisV, ctor, args)

    // parents
    def initParent(parent: Tree) =
      parent match
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        evalExprs(stats, thisV, klass)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>       // extends A(args)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args)

      case _ =>   // extends A or extends A[T]
        val tref = typeRefOf(parent.tpe)
        superCall(tref, tref.classSymbol.primaryConstructor, Nil)

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
        case Some(parent) =>
          initParent(parent)
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
            superCall(tref, ctor, Nil)
      }
    end if

    // class body
    tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) && !vdef.rhs.isEmpty =>
        // Throw the field value away, as the analysis is not heap-sensitive
        eval(vdef.rhs, thisV, klass)

      case _: MemberDef =>

      case tree =>
        eval(tree, thisV, klass)
    }

  /** Resolve C.this that appear in `klass`
   *
   * @param target  The class symbol for `C` for which `C.this` is to be resolved.
   * @param thisV   The value for `D.this` where `D` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `C.this` is located.
   */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol): Contextual[Value] =
    thisV match
    case OfClass(tp) =>
      ???

    case OfType(tp) =>
      ???

    case RefSet(refs) =>
      ???

    case _ =>
      report.error("[Internal error] unexpected this value " + thisV + Trace.show, Trace.position)
      OfType(defn.NothingType)

  /** Compute the outer value that correspond to `tref.prefix`
   *
   * @param tref    The type whose prefix is to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `tref` is located.
   */
  def outerValue(tref: TypeRef, thisV: OfClass, klass: ClassSymbol): Contextual[Value] =
    val cls = tref.classSymbol.asClass
    if tref.prefix == NoPrefix then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      val outerV = resolveThis(enclosing, thisV, klass)
      outerV
    else
      if cls.isAllOf(Flags.JavaInterface) then OfType(defn.NothingType)
      else evalType(tref.prefix, thisV, klass)
