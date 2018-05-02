package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context
import StdNames._
import Names._
import NameKinds.DefaultGetterName
import ast._
import Trees._
import Symbols._
import Types._
import Decorators._
import util.Positions._
import config.Printers.init.{ println => debug }
import collection.mutable

//=======================================
//             values
//=======================================

object Value {
  def checkParams(sym: Symbol, paramInfos: List[Type], values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
    paramInfos.zipWithIndex.foreach { case (tp, index) =>
      val value = scala.util.Try(values(index)).getOrElse(FullValue)
      val pos = scala.util.Try(argPos(index)).getOrElse(NoPosition)
      if (value.widen(heap, pos) < tp.value)
        return Res(effects = Vector(Generic("Leak of object under initialization to " + sym.show, pos)))
    }
    Res()
  }

  def defaultFunctionValue(methSym: Symbol)(implicit ctx: Context): Value = {
    assert(methSym.is(Flags.Method))
    if (methSym.info.paramNamess.isEmpty) FullValue
    else new FunctionValue() {
      def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
        val paramInfos = methSym.info.paramInfoss.flatten
        checkParams(methSym, paramInfos, values, argPos, pos, heap)
      }
    }
  }
}

/** Abstract values in analysis */
sealed trait Value {
  /** Select a member on a value */
  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res

  /** Assign on a value */
  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res

  /** Index an inner class with current value as the immediate outer */
  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res

  /** Apply a method or function to the provided arguments */
  def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res

  def show(setting: ShowSetting)(implicit ctx: Context): String

  /** Join two values
   *
   *  NoValue < Partial < Filled < Full
   */
  def join(other: Value): Value = (this, other) match {
    case (FullValue, v) => v
    case (v, FullValue) => v
    case (NoValue, _) => NoValue
    case (_, NoValue) => NoValue
    case (PartialValue, _) => PartialValue
    case (_, PartialValue) => PartialValue
    case (v1: OpaqueValue, v2: OpaqueValue)     => v1.join(v2)
    case (o1: ObjectValue, o2: ObjectValue) if o1 `eq` o2 => o1
    case (f1: FunctionValue, f2: FunctionValue) if f1 `eq` f2 => f1
    case (f1: FunctionValue, f2: FunctionValue) => f1.join(f2)
    case (o1: SliceValue, o2: SliceValue) =>
      if (o1.id == o2.id) o1
      else new UnionValue(Set(o1, o2))
    case (v1: LazyValue, v2: LazyValue) if v1 == v2 => v1
    case (v1: UnionValue, v2: UnionValue) => v1 ++ v2
    case (uv: UnionValue, v: SingleValue) => uv + v
    case (v: SingleValue, uv: UnionValue) => uv + v
    case (v1: SingleValue, v2: SingleValue) => UnionValue(Set(v1, v2))
  }

  /** Widen the value to an opaque value
   *
   *  Widening is needed at analysis boundary.
   */
  def widen(heap: Heap, pos: Position, handler: Vector[Effect] => Unit = effs => ())(implicit ctx: Context): OpaqueValue = {
    def recur(value: Value, heap: Heap): OpaqueValue = value match {
      case ov: OpaqueValue => ov
      case fv: FunctionValue =>
        val testHeap = heap.clone
        val res = fv(i => FullValue, i => NoPosition, pos, testHeap)
        if (res.hasErrors) {
          handler(res.effects)
          FilledValue
        }
        else recur(res.value, testHeap)
      case sv: SliceValue =>
        heap(sv.id).asSlice.widen
      case ov: ObjectValue =>
        if (ov.open) FilledValue
        else ov.slices.values.foldLeft(FullValue: OpaqueValue) { (acc, v) =>
          if (acc != FullValue) return FilledValue
          recur(v, heap).join(acc)
        }
      case UnionValue(vs) =>
        vs.foldLeft(FullValue: OpaqueValue) { (acc, v) =>
          if (v == PartialValue || acc == PartialValue) return PartialValue
          else acc.join(recur(v, heap))
        }
      // case NoValue => NoValue
      case _ => // impossible
        ???
    }

    recur(this, heap)
  }
}

/** The value is absent */
object NoValue extends Value {
  def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = ???
  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res = ???
  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = ???

  def show(setting: ShowSetting)(implicit ctx: Context): String = "NoValue"
}

/** A single value, instead of a union value */
sealed trait SingleValue extends Value

/** Union of values */
case class UnionValue(val values: Set[SingleValue]) extends Value {
  def apply(args: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.apply(args, argPos, pos, heap).join(acc)
    }
  }

  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.select(sym, heap, pos).join(acc)
    }
  }

  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.assign(sym, value, heap, pos).join(acc)
    }
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.init(constr, values, argPos, pos, obj, heap, indexer).join(acc)
    }
  }

  def +(value: SingleValue): UnionValue = UnionValue(values + value)
  def ++(uv: UnionValue): UnionValue = UnionValue(values ++ uv.values)

  def show(setting: ShowSetting)(implicit ctx: Context): String =
    "Or{" + setting.indent(values.map(v => v.show(setting)).mkString(", ")) + "}"
}

/** Values that are subject to type checking rather than analysis */
abstract sealed class OpaqueValue extends SingleValue {
  // not supported
  def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = ???

  def <(that: OpaqueValue): Boolean = (this, that) match {
    case (FullValue, _) => false
    case (FilledValue, PartialValue | FilledValue) => false
    case (PartialValue, PartialValue) => false
    case _ => true
  }

  def join(that: OpaqueValue): OpaqueValue =
    if (this < that) this else that

  def meet(that: OpaqueValue): OpaqueValue =
    if (this < that) that else this
}

object FullValue extends OpaqueValue {
  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res =
    if (sym.is(Flags.Method)) Res(value = Value.defaultFunctionValue(sym))
    else Res()

  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res =
    if (value.widen(heap, pos) != FullValue)
      Res(effects = Vector(Generic("Cannot assign an object under initialization to a full object", pos)))
    else Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = {
    val cls = constr.owner.asClass
    val paramInfos = constr.info.paramInfoss.flatten
    val res = Value.checkParams(cls, paramInfos, values, argPos, pos, heap)
    if (res.hasErrors) return res

    val args = (0 until paramInfos.size).map(i => scala.util.Try(values(i)).getOrElse(FullValue))
    if (args.exists(_.widen(heap, pos) < FullValue)) obj.add(cls, FilledValue)

    Res()
  }

  def show(setting: ShowSetting)(implicit ctx: Context): String = "Full"

  override def toString = "full value"
}

object PartialValue extends OpaqueValue {
  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res = {
    // set state to Full, don't report same error message again
    val res = Res(value = FullValue)

    if (sym.is(Flags.Method)) {
      if (!sym.isPartial && !sym.name.is(DefaultGetterName))
        res += Generic(s"The $sym should be marked as `@partial` in order to be called", pos)

      res.value = Value.defaultFunctionValue(sym)
    }
    else if (sym.is(Flags.Lazy)) {
      if (!sym.isPartial)
        res += Generic(s"The lazy field $sym should be marked as `@partial` in order to be accessed", pos)
    }
    else if (sym.isClass) {
      if (!sym.isPartial)
        res += Generic(s"The nested $sym should be marked as `@partial` in order to be instantiated", pos)
    }
    else {  // field select
      if (!sym.isPrimaryConstructorFields || sym.owner.is(Flags.Trait))
        res += Generic(s"Cannot access field $sym on a partial object", pos)
    }

    res
  }

  /** assign to partial is always fine? */
  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res = Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = {
    val paramInfos = constr.info.paramInfoss.flatten
    val res = Value.checkParams(constr.owner, paramInfos, values, argPos, pos, heap)
    if (res.hasErrors) return res

    val cls = constr.owner.asClass
    if (!cls.isPartial) {
      res += Generic(s"The nested $cls should be marked as `@partial` in order to be instantiated", pos)
      res.value = FullValue
      return res
    }

    obj.add(cls, FilledValue)

    Res()
  }

  def show(setting: ShowSetting)(implicit ctx: Context): String = "Partial"

  override def toString = "partial value"
}

object FilledValue extends OpaqueValue {
  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res = {
    val res = Res()
    if (sym.is(Flags.Method)) {
      if (!sym.isPartial && !sym.isFilled && !sym.name.is(DefaultGetterName))
        res += Generic(s"The $sym should be marked as `@partial` or `@filled` in order to be called", pos)

      res.value = Value.defaultFunctionValue(sym)
    }
    else if (sym.is(Flags.Lazy)) {
      if (!sym.isPartial && !sym.isFilled)
        res += Generic(s"The lazy field $sym should be marked as `@partial` or `@filled` in order to be accessed", pos)

      res.value = sym.info.value
    }
    else {
      res.value = sym.value.join(sym.info.value)
    }

    res
  }

  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res =
    if (value.widen(heap, pos) < sym.info.value)
      Res(effects = Vector(Generic("Cannot assign an object of a lower state to a field of higher state", pos)))
    else Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = {
    val paramInfos = constr.info.paramInfoss.flatten
    val res = Value.checkParams(constr.owner, paramInfos, values, argPos, pos, heap)
    if (res.hasErrors) return res

    val cls = constr.owner.asClass
    if (!cls.isPartial && !cls.isFilled) {
      res += Generic(s"The nested $cls should be marked as `@partial` or `@filled` in order to be instantiated", pos)
      res.value = FullValue
      return res
    }

    obj.add(cls, FilledValue)

    Res()
  }

  def show(setting: ShowSetting)(implicit ctx: Context): String = "Filled"

  override def toString = "filled value"
}

/** A function value or value of method select */
abstract class FunctionValue extends SingleValue { self =>
  def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res

  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res = sym.name match {
    case nme.apply | nme.lift => Res(value = this)
    case nme.compose =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
              val arg = values(0)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res1 = fun(0).select(applySym, heap, pos)
              val res2 = res1.value.apply(arg :: Nil, argPos, pos, heap)
              val res3 = self.apply(res2.value :: Nil, argPos, pos, heap)
              Res(value = res3.value, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
          }
          Res(value = composedFun)
        }
      }
      Res(value = selectedFun)
    case nme.andThen =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos, pos, heap)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym, heap, pos)
              val res3 = res2.value.apply(res2.value :: Nil, argPos, pos, heap)
              Res(value = res3.value, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
          }
          Res(value = composedFun)
        }
      }
      Res(value = selectedFun)
    case nme.applyOrElse =>
      val selectedFun = new FunctionValue() {
        def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
          val arg = values(0)
          val fun = values(1)
          val res1 = self.apply(arg :: Nil, argPos, pos, heap)
          val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
          val res2 = fun.select(applySym, heap, pos)
          val res3 = res2.value.apply(arg :: Nil, argPos, pos, heap)
          Res(value = res1.value.join(res3.value), effects = res1.effects ++ res2.effects ++ res3.effects)
        }
      }
      Res(value = selectedFun)
    case nme.runWith =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos, pos, heap)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym, heap, pos)
              val res3 = res2.value.apply(res2.value :: Nil, argPos, pos, heap)
              Res(value = FullValue, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
          }
          Res(value = composedFun)
        }
      }
      Res(value = selectedFun)
    case nme.orElse =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos, pos, heap)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym, heap, pos)
              val res3 = res2.value.apply(arg :: Nil, argPos, pos, heap)
              Res(value = res1.value.join(res3.value), effects = res1.effects ++ res2.effects ++ res3.effects)
            }
          }
          Res(value = composedFun)
        }
      }
      Res(value = selectedFun)
    case _ =>
      FullValue.select(sym, heap, pos)
  }

  /** not supported */
  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = ???

  def show(setting: ShowSetting)(implicit ctx: Context): String = toString

  override def toString: String = "Function@" + hashCode

  def join(that: FunctionValue): FunctionValue =
    new FunctionValue {
      def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = {
        val heap2 = heap.clone
        val res1 = self(values, argPos, pos, heap)
        val res2 = that(values, argPos, pos, heap2)
        heap.join(heap2)
        res1.join(res2)
      }
    }

}

/** A lazy value */
abstract class LazyValue extends SingleValue {
  // not supported
  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res = ???
  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = ???

  def show(setting: ShowSetting)(implicit ctx: Context): String = toString

  override def toString: String = "LazyValue@" + hashCode
}

/** A slice of an object */
class SliceValue(val id: Int) extends SingleValue {
  /** not supported, impossible to apply an object value */
  def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = ???

  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res = {
    val slice = heap(id).asSlice
    val value = slice(sym)

    if (sym.is(Flags.Lazy)) {
      if (value.isInstanceOf[LazyValue]) {
        val res = value(Nil, Nil, pos, heap)
        slice(sym) = res.value
        res
      }
      else Res(value = value)
    }
    else if (sym.is(Flags.Method)) {
      if (sym.info.isParameterless) {       // parameter-less call
        value(Nil, Nil, pos, heap)
      }
      else Res(value = value)
    }
    else {
      if (value == NoValue) {
        if (sym.info.isInstanceOf[ConstantType]) Res()
        else Res(effects = Vector(Uninit(sym, pos)))
      }
      else {
        val res = Res(value = value)

        if (sym.is(Flags.Deferred) && !sym.hasAnnotation(defn.InitAnnot))
          res += UseAbstractDef(sym, pos)

        res
      }
    }
  }

  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res = {
    val slice = heap(id).asSlice
    slice(sym) = value
    Res()
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = {
    val cls = constr.owner.asClass
    val slice = heap(id).asSlice
    val tmpl = slice.classInfos(cls)
    indexer.init(constr, tmpl, values, argPos, pos, obj, slice.innerEnv)
  }

  override def hashCode = id

  override def equals(that: Any) = that match {
    case that: SliceValue => that.id == id
    case _ => false
  }

  def show(setting: ShowSetting)(implicit ctx: Context): String = setting.heap(id).asSlice.show(setting)
}

class ObjectValue(val tp: Type, val open: Boolean = false) extends SingleValue {
  /** slices of the object */
  private var _slices: Map[ClassSymbol, Value] = Map()
  def slices: Map[ClassSymbol, Value] = _slices

  def add(cls: ClassSymbol, value: Value) = {
    if (slices.contains(cls)) {
      _slices = _slices.updated(cls, _slices(cls).join(value))
    }
    else _slices = _slices.updated(cls, value)
  }

  // handle dynamic dispatch
  private def resolve(sym: Symbol)(implicit ctx: Context): Symbol = {
    if (sym.isClass || sym.isConstructor || sym.isEffectivelyFinal || sym.is(Flags.Private)) sym
    else {
      // the method may crash, see tests/pos/t7517.scala
      try sym.matchingMember(tp) catch { case _: Throwable => NoSymbol }
    }
  }

  /** not supported, impossible to apply an object value */
  def apply(values: Int => Value, argPos: Int => Position, pos: Position, heap: Heap)(implicit ctx: Context): Res = ???

  def select(sym: Symbol, heap: Heap, pos: Position)(implicit ctx: Context): Res = {
    val target = resolve(sym)

    // select on self type
    if (!target.exists) {
      if (sym.owner.is(Flags.Trait))
        return PartialValue.select(sym, heap, pos)
      else
        return FilledValue.select(sym, heap, pos)
    }

    if (this.widen(heap, pos) == FullValue) return FullValue.select(sym, heap, pos)

    val cls = target.owner.asClass
    if (slices.contains(cls)) {
      val res = slices(cls).select(target, heap, pos)
      // ignore field access, but field access in Scala
      // are method calls, thus is unsafe as well
      if (open && target.is(Flags.Method, butNot = Flags.Lazy) &&
          !target.isPartial &&
          !target.isFilled &&
          !target.isOverride &&
          !target.isEffectivelyFinal &&
          !target.name.is(DefaultGetterName))
        res += OverrideRisk(target, pos)
      res
    }
    else {
      // select on unknown super
      assert (target.isDefinedOn(tp))
      FilledValue.select(target, heap, pos)
    }
  }

  def assign(sym: Symbol, value: Value, heap: Heap, pos: Position)(implicit ctx: Context): Res = {
    val target = resolve(sym)

    // select on self type
    if (!target.exists) return PartialValue.assign(sym, value, heap, pos)

    val cls = target.owner.asClass
    if (slices.contains(cls)) {
      slices(cls).assign(target, value, heap, pos)
    }
    else {
      // select on unknown super
      assert(target.isDefinedOn(tp))
      FilledValue.assign(target, value, heap, pos)
    }
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], pos: Position, obj: ObjectValue, heap: Heap, indexer: Indexer)(implicit ctx: Context): Res = {
    val cls = constr.owner.asClass
    val outerCls = cls.owner.asClass
    if (slices.contains(outerCls)) {
      slices(outerCls).init(constr, values, argPos, pos, obj, heap, indexer)
    }
    else {
      val value = if (cls.isDefinedOn(tp)) FilledValue else PartialValue
      value.init(constr, values, argPos, pos, obj, heap, indexer)
    }
  }

  def show(setting: ShowSetting)(implicit ctx: Context): String = {
    val body = slices.map { case (k, v) => "[" +k.show + "]" + setting.indent(v.show(setting)) }.mkString("\n")
    "Object {\n" + setting.indent(body) + "\n}"
  }
}
