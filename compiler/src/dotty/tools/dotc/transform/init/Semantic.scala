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

import scala.collection.mutable

trait Semantic { self: Domain =>
  /** Cache used to terminate the analysis
   *
   * A finitary configuration is not enough for the analysis to
   * terminate.  We need to use cache to let the interpreter "know"
   * that it can terminate.
   */
  type Cache <: mutable.Map[Config, Value]
  val cache: Cache

  /** Errors in the program */
  type Error

  /** Result of abstract interpretation */
  case class Result(value: Value, heap: Heap, errors: Vector[Error]) {
    def show(using Context) = ???
  }

  val noErrors = Vector.empty[Error]

  /** Evaluate an expression with the given value for `this` in a given class `klass`
   *
   * This method only handles cache logic and delegates the work to `cases`.
   */
  def eval(expr: Tree, thisV: Value, klass: ClassSymbol, heap: Heap)(using Context): Result = trace("evaluating " + expr.show, printer, res => res.asInstanceOf[Result].show) {
    val cfg = makeConfig(expr, thisV, heap)
    if (cache.contains(cfg)) Result(cache(cfg), heap, noErrors)
    else {
      // no need to compute fix-point, because
      // 1. the result is decided by `cfg` for a legal program
      //    (heap change is irrelevant thanks to monotonicity)
      // 2. errors will have been reported for an illegal program
      cache(cfg) = bottom
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
        Result(bottom, heap, noErrors)

      case Ident(name) =>
        assert(name.isTermName, "type trees should not reach here")
        cases(expr.tpe, thisV, klass, heap)

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
        Result(bottom, heap, noErrors)

      case New(tpt) =>
        ???

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(bottom, heap, noErrors)
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
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Result(bottom, heap, noErrors)
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
        Result(bottom, heap, noErrors)

      case ddef : DefDef =>
        ???

      case tdef: TypeDef =>
        ???

      case _: Import | _: Export =>
        Result(bottom, heap, noErrors)

      case _ =>
        throw new Exception("unexpected tree: " + expr.show)
    }

  /** Handle semantics of leaf nodes */
  def cases(tp: Type, thisV: Value, klass: ClassSymbol, heap: Heap)(using Context): Result = trace("evaluating " + tp.show, printer, res => res.asInstanceOf[Result].show) {
    tp match {
      case _: ConstantType =>
        Result(bottom, heap, noErrors)

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        Result(bottom, heap, noErrors)

      case tmref: TermRef =>
        ???

      case ThisType(tref) =>
        ???

      case SuperType(thisTp, superTp) =>
        ???

      case _: TermParamRef | _: RecThis  =>
        // possible from checking effects of types
        Result(bottom, heap, noErrors)

      case _ =>
        throw new Exception("unexpected type: " + tp)
    }
  }

  /** Initialize an abstract object */
  def init(klass: Symbol, thisV: Addr, heap: Heap): Result = ???
}
