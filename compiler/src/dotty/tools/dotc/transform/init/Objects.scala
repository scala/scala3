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
 *  check issue a warning for the code above.
 *
 *  At the high-level, the analysis has the following characteristics:
 *
 *  1. It is inter-procedural and flow-insensitive.
 *
 *  2. It is modular with respect to separate compilation, even incremental
 *     compilation.
 *
 *     When a value leaks the boundary of analysis, we approximate by calling
 *     all public methods of the type at the definition site.
 *
 *  3. It is receiver-sensitive but not heap-sensitive nor parameter-sensitive.
 *
 *     Fields and parameters are always abstracted by their types.
 *
 *  4. If the target of a virtual method call cannot be determined by its
 *     receiver, the target is approximated by all methods of classes currently
 *     being compiled and are instantiated. This is similar to RTA (rapid type
 *     analysis).
 *
 *     However, a class type is only added to the list when it leaks:
 *
 *     - A value of OfClass is used as method argument.
 *     - A value of OfClass is alised to a field.
 *
 *     When a value of OfClass is aliased to a local variable, the RHS will be
 *     re-evaluated when it is accessed (caching is used for optimization).
 */
object Objects:
  sealed abstract class Value
    def show: String = toString

  /**
   * Rerepsents values that are instances of the specified class
   *
   * The `tp.classSymbol` should be the concrete class of the value at runtime.
   */
  case class OfClass(tp: Type) extends Value

  /**
   * Rerepsents values that are of the given type
   */
  case class OfType(tp: Type) extends Value

  /**
   * Represents a lambda expression
   */
  case class Fun(expr: Tree, thisV: Value, klass: ClassSymbol) extends Value

  object State:
    /**
     * Remembers the instantiated types during instantiation of a static object.
     */
    class Data(allConcreteClasses: Set[ClassSymbol]):
      // object -> (class, types of the class)
      val instantiatedTypes = mutable.Map.empty[ClassSymbol, Map[ClassSymbol, List[Type]]]

      // object -> (fun type, fun values)
      val instantiatedFuncs = mutable.Map.empty[ClassSymbol, Map[Type, List[Fun]]]

    opaque type Rep = Data

    def init(classes: List[ClassSymbol])(using Context): Rep =
      val concreteClasses = classes.filter(Semantic.isConcreteClass).toSet
      new Data(concreteClasses)

  type Contextual[T] = (Context, State.Rep, State.Cache) ?=> T

  object Cache:
    /** Cache for expressions
     *
     *  OfClass -> Tree -> Value
     *
     *  The first key is the value of `this` for the expression.
     */
    opaque type ExprValueCache = Map[OfClass, Map[TreeWrapper, Value]]

    /** A wrapper for trees for storage in maps based on referential equality of trees. */
    private abstract class TreeWrapper:
      def tree: Tree

      override final def equals(other: Any): Boolean =
        other match
        case that: TreeWrapper => this.tree eq that.tree
        case _ => false

      override final def hashCode = tree.hashCode

    /** The immutable wrapper is intended to be stored as key in the heap. */
    private class ImmutableTreeWrapper(val tree: Tree) extends TreeWrapper

    /** For queries on the heap, reuse the same wrapper to avoid unnecessary allocation.
     *
     *  A `MutableTreeWrapper` is only ever used temporarily for querying a map,
     *  and is never inserted to the map.
     */
    private class MutableTreeWrapper extends TreeWrapper:
      var queryTree: Tree | Null = null
      def tree: Tree = queryTree match
        case tree: Tree => tree
        case null => ???

    /** Used to avoid allocation, its state does not matter */
    private given MutableTreeWrapper = new MutableTreeWrapper

    def get(value: OfClass, expr: Tree)(using cache: ExprValueCache): Option[Value] =
      cache.get(value, expr) match
      case None => cache.get(value, expr)
      case res => res

    def cache(value: OfClass, expr: Tree, result: Value)(using cache: ExprValueCache): Option[Value] =
      cache.updatedNested(value, expr, result)
    extension (cache: ExprValueCache)
      private def get(value: OfClass, expr: Tree)(using queryWrapper: MutableTreeWrapper): Option[Value] =
        queryWrapper.queryTree = expr
        cache.get(value).flatMap(_.get(queryWrapper))

      private def removed(value: OfClass, expr: Tree)(using queryWrapper: MutableTreeWrapper) =
        queryWrapper.queryTree = expr
        val innerMap2 = cache(value).removed(queryWrapper)
        cache.updated(value, innerMap2)

      private def updatedNested(value: OfClass, expr: Tree, result: Value): ExprValueCache =
        val wrapper = new ImmutableTreeWrapper(expr)
        updatedNestedWrapper(value, wrapper, result)

      private def updatedNestedWrapper(value: OfClass, wrapper: ImmutableTreeWrapper, result: Value): ExprValueCache =
        val innerMap = cache.getOrElse(value, Map.empty[TreeWrapper, Value])
        val innerMap2 = innerMap.updated(wrapper, result)
        cache.updated(value, innerMap2)
    end extension
  end Cache

  // ---------------------------------------

  /** Check an individual class
   *
   *  The class to be checked must be an instantiable concrete class.
   */
  private def checkObject(classSym: ClassSymbol): Contextual[Unit] =
    val tpl = classSym.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
    init(tpl, OfClass(classSym), classSym)

  def checkClasses(classes: List[ClassSymbol])(using Context): Unit =
    given State.Rep = State.init(classes)

    for
      classSym <- classes
      if classSym.is(Flags.Module) && classSym.isStaticOwner
    do
      checkObject(classSym)

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
  def cases(expr: Tree, thisV: Value, klass: ClassSymbol): Contextual[Value] =
    expr match
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        OfType(defn.NothingType)

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        cases(expr.tpe, thisV, klass)

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        val outer = outerValue(tref, thisV, klass)
        outer.instantiate(cls, ctor, args)

      case Call(ref, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe: @unchecked
          val thisValue2 = extendTrace(ref) { resolveThis(thisTp.classSymbol.asClass, thisV, klass) }
          thisValue2.call(ref.symbol, args, thisTp, superTp)

        case Select(qual, _) =>
          val receiver = eval(qual, thisV, klass)
          if ref.symbol.isConstructor then
            receiver.callConstructor(ref.symbol, args)
          else
            receiver.call(ref.symbol, args, receiver = qual.tpe, superType = NoType)

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = extendTrace(ref) { resolveThis(enclosingClass, thisV, klass) }
            // local methods are not a member, but we can reuse the method `call`
            thisValue2.call(id.symbol, args, receiver = NoType, superType = NoType, needResolve = false)
          case TermRef(prefix, _) =>
            val receiver = cases(prefix, thisV, klass)
            if id.symbol.isConstructor then
              receiver.callConstructor(id.symbol, args)
            else
              receiver.call(id.symbol, args, receiver = prefix, superType = NoType)

      case Select(qualifier, name) =>
        val qual = eval(qualifier, thisV, klass)

        name match
          case OuterSelectName(_, _) =>
            val current = qualifier.tpe.classSymbol
            val target = expr.tpe.widenSingleton.classSymbol.asClass
            resolveThis(target, qual, current.asClass)
          case _ =>
            qual.select(expr.symbol, receiver = qualifier.tpe)

      case _: This =>
        cases(expr.tpe, thisV, klass)

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
        eval(stats, thisV, klass)
        eval(expr, thisV, klass)

      case If(cond, thenp, elsep) =>
        eval(cond :: thenp :: elsep :: Nil, thisV, klass).join

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) OfType(defn.NothingType)
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        eval(selector, thisV, klass)
        eval(cases.map(_.body), thisV, klass).join

      case Return(expr, from) =>
        eval(expr, thisV, klass)

      case WhileDo(cond, body) =>
        eval(cond :: body :: Nil, thisV, klass)
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
  def evalType(tp: Type, thisV: Ref, klass: ClassSymbol): Contextual[Value] = log("evaluating " + tp.show, printer, (_: Value).show) {
    ???
  }

  /** Evaluate arguments of methods */
  def evalArgs(args: List[Arg], thisV: Value, klass: ClassSymbol): Contextual[List[Value]] =
    val argInfos = new mutable.ArrayBuffer[ArgInfo]
    args.foreach { arg =>
      val res =
        if arg.isByName then
          Fun(arg.tree, thisV, klass)
        else
          eval(arg.tree, thisV, klass)

      argInfos += res
    }
    argInfos.toList

  def init(tpl: Template, thisV: OfClass, klass: ClassSymbol): Contextual[Unit] =
    def superCall(tref: TypeRef, ctor: Symbol, args: List[Value]): Unit =
      val cls = tref.classSymbol.asClass

      // follow constructor
      if cls.hasSource then thisV.callConstructor(ctor, args)

    // parents
    def initParent(parent: Tree) =
      parent match
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        eval(stats, thisV, klass)
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
