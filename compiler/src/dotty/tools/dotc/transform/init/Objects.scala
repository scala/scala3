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
 *  3. It is receiver-sensitive but not heap-sensitive -- fields are always
 *     abstracted by types.
 *
 *  4. If the target of a virtual method call cannot be determined by its
 *     receiver, the target is approximated by all methods of classes currently
 *     being compiled and are instantiated. This is to some extent similar to
 *     RTA (rapid type analysis).
 *
 *     However, a class type is only added to the list when it leaks:
 *
 *     - A value of the class is used as method argument.
 *     - A value of the class is alised to a field or variable.
 */
object Objects:
  sealed abstract class Value

  /**
   * Rerepsents values that are instances of the given class
   *
   * The parameter `klass` should be the concrete class of the value at runtime.
   */
  case class OfClass(klass: ClassSymbol) extends Value

  /**
   * Rerepsents values that are of the given type
   */
  case class OfType(tp: Type) extends Value

  object State:
    /**
     * Records the instantiated types during instantiation of a static object.
     *
     * Functions and by-name closures are called when they leak, therefore they
     * are not part of the instantiated types.
     */
    class Data(allConcreteClasses: Set[ClassSymbol]):
      // object -> (class, types of the class)
      val instantiatedTypes = mutable.Map.empty[ClassSymbol, Map[ClassSymbol, List[Type]]]

    opaque type Rep = Data

    def init(classes: List[ClassSymbol])(using Context): Rep =
      val concreteClasses = classes.filter(Semantic.isConcreteClass).toSet
      new Data(concreteClasses)

  type Contextual[T] = (Context, State.Rep) ?=> T

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


  /** Handles the evaluation of different expressions
   *
   *  Note: Recursive call should go to `eval` instead of `cases`.
   *
   * @param expr   The expression to be evaluated.
   * @param thisV  The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass  The enclosing class where the expression `expr` is located.
   */
  def eval(expr: Tree, thisV: Value, klass: ClassSymbol): Contextual[Value] = ???

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
