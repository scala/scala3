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
  trait Value

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
  case class Addr(klass: Symbol, args: List[Value]) extends Value

  /** A function value */
  case class Fun(expr: Tree, thisV: Addr, klass: Symbol) extends Value

  /** A value which represents a set of addresses
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: List[Addr | Fun]) extends Value

  /** Object stores abstract values for all fields and the outer. */
  case class Objekt(klass: Symbol, fields: Map[Symbol, Value], outer: Value)

  /** Abstract heap stores abstract objects
   *
   * As in the OOPSLA paper, the abstract heap is monotonistic
   */
  type Heap = Map[Addr, Objekt]

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
  case class Result(value: Value, heap: Heap, errors: List[Error]) {
    def show(using Context) = ???

    def ++(errors: Seq[Error]): Result = this.copy(errors = this.errors ++ errors)

    def fieldAccess(f: Symbol, source: Tree)(using Context): Result =
      value.fieldAccess(f, heap, source) ++ errors
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
    def fieldAccess(f: Symbol, heap: Heap, source: Tree)(using Context): Result =
      value match {
        case Hot  =>
          Result(Hot, heap, noErrors)

        case Cold =>
          val error = AccessCold(f, source, Vector(source))
          Result(Hot, heap, error :: Nil)

        case addr: Addr =>
          val obj = heap(addr)
          if obj.fields.contains(f) then
            Result(obj.fields(f), heap, Nil)
          else
            val error = AccessNonInit(f, Vector(source))
            Result(Hot, heap, error :: Nil)

        case _: Fun =>
          ???

        case RefSet(refs) =>
          val resList = refs.map(_.fieldAccess(f, heap, source))
          val value2 = resList.map(_.value).join
          val errors = resList.flatMap(_.errors)
          Result(value2, heap, errors)
      }


// ----- Semantic definition --------------------------------

  /** Evaluate an expression with the given value for `this` in a given class `klass`
   *
   * This method only handles cache logic and delegates the work to `cases`.
   */
  def eval(expr: Tree, thisV: Value, klass: ClassSymbol, heap: Heap)(using Context): Result = trace("evaluating " + expr.show, printer, res => res.asInstanceOf[Result].show) {
    val cfg = Config(thisV, expr.sourcePos)
    if (cache.contains(cfg)) Result(cache(cfg), heap, noErrors)
    else {
      // no need to compute fix-point, because
      // 1. the result is decided by `cfg` for a legal program
      //    (heap change is irrelevant thanks to monotonicity)
      // 2. errors will have been reported for an illegal program
      cache(cfg) = Hot
      val res = cases(expr, thisV, klass, heap)
      cache(cfg) = res.value
      res
    }
  }

  /** Handles the evaluation of different expressions */
  def cases(expr: Tree, thisV: Value, klass: ClassSymbol, heap: Heap)(using Context): Result =
    expr match {
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Result(Hot, heap, noErrors)

      case Ident(name) =>
        assert(name.isTermName, "type trees should not reach here")
        cases(expr.tpe, thisV, klass, heap, expr)

      // case supert: Super => Handled in Apply case

      case Apply(fun, args) =>
        ???

      case TypeApply(fun, _) =>
        ???

      case Select(qualifier, name) =>
        ???

      case _: This =>
        ???

      case Literal(_) =>
        Result(Hot, heap, noErrors)

      case New(tpt) =>
        ???

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Hot, heap, noErrors)
        else ???

      case NamedArg(name, arg) =>
        cases(arg, thisV, klass, heap)

      case Assign(lhs, rhs) =>
        ???

      case closureDef(ddef) =>
        ???

      case Block(stats, expr) =>
        ???

      case If(cond, thenp, elsep) =>
        ???

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(Hot, heap, noErrors)
        else ???

      case Match(selector, cases) =>
        ???

      case Return(expr, from) =>
        ???

      case WhileDo(cond, body) =>
        ???

      case Labeled(_, expr) =>
        ???

      case Try(block, cases, finalizer) =>
        ???

      case SeqLiteral(elems, elemtpt) =>
        ???

      case Inlined(call, bindings, expansion) =>
        ???

      case vdef : ValDef =>
        ???

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Result(Hot, heap, noErrors)

      case ddef : DefDef =>
        ???

      case tdef: TypeDef =>
        ???

      case _: Import | _: Export =>
        Result(Hot, heap, noErrors)

      case _ =>
        throw new Exception("unexpected tree: " + expr.show)
    }

  /** Handle semantics of leaf nodes */
  def cases(tp: Type, thisV: Value, klass: ClassSymbol, heap: Heap, source: Tree)(using Context): Result = trace("evaluating " + tp.show, printer, res => res.asInstanceOf[Result].show) {
    tp match {
      case _: ConstantType =>
        Result(Hot, heap, noErrors)

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        Result(Hot, heap, noErrors)

      case tmref: TermRef =>
        cases(tmref.prefix, thisV, klass, heap, source).fieldAccess(tmref.symbol, source)

      case tp @ ThisType(tref) =>
        if tref.symbol.is(Flags.Package) then Result(Hot, heap, noErrors)
        else resolveThis(tp, thisV: Value, klass: ClassSymbol, heap: Heap)

      case _: TermParamRef | _: RecThis  =>
        // possible from checking effects of types
        Result(Hot, heap, noErrors)

      case _ =>
        throw new Exception("unexpected type: " + tp)
    }
  }

  /** Resolve C.this that appear in `klass` */
  def resolveThis(tp: Type, thisV: Value, klass: ClassSymbol, heap: Heap)(using Context): Result = trace("resolving " + tp.show, printer, res => res.asInstanceOf[Result].show) {
    ???
  }

  /** Initialize an abstract object */
  def init(klass: Symbol, thisV: Addr, heap: Heap): Result = ???
}
