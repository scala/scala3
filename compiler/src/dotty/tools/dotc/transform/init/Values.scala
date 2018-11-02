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
import Annotations._
import config.Printers.init.{ println => debug }
import collection.mutable

//=======================================
//             values
//=======================================

object Value {

  def checkParams(sym: Symbol, paramInfos: List[Type], values: Int => Value, argPos: Int => Position, onlyHot: Boolean = false)(implicit setting: Setting): Res = {
    def message(v: OpaqueValue) = {
      s"Unsafe leak of object under initialization to ${sym.show}" ++
      (v match {
        case WarmValue(deps, _) if deps.nonEmpty =>
          "\nThe object captures " + deps.map(_.show).mkString("", ",", ".")
        case _ => ""
      })
    }
    paramInfos.zipWithIndex.foreach { case (tp, index) =>
      val value = scala.util.Try(values(index)).getOrElse(HotValue)
      val pos = scala.util.Try(argPos(index)).getOrElse(NoPosition)
      val wValue = value.widen(setting.widening)
      if (!wValue.isHot && (onlyHot || !tp.value.isCold) || wValue.isIcy)  // warm objects only leak as cold, for safety and simplicity
        return Res(effects = Vector(Generic(message(wValue), pos)))
    }
    Res()
  }

  def defaultFunctionValue(methSym: Symbol)(implicit setting: Setting): Value = {
    assert(methSym.is(Flags.Method))
    if (methSym.info.paramNamess.isEmpty) HotValue
    else new FunctionValue() {
      def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
        val paramInfos = methSym.info.paramInfoss.flatten
        checkParams(methSym, paramInfos, values, argPos, onlyHot = true)
      }

      def widen(implicit setting: Setting) = HotValue  // could be reached by tryWiden on funtions
    }
  }

  def defaultClassValue(classSym: Symbol, prefix: OpaqueValue)(implicit setting: Setting): Value = {
    assert(classSym.isClass)
    new ClassValue {
      def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
        val cls = constr.owner.asClass
        val paramInfos = constr.info.paramInfoss.flatten
        val res = Value.checkParams(cls, paramInfos, values, argPos)
        if (res.hasErrors) return res

        val args = (0 until paramInfos.size).map(i => scala.util.Try(values(i)).getOrElse(HotValue))
        val value = args.foldLeft(prefix) { (acc, v) =>
          acc.join(v.widen)
        }

        if (cls == obj.tp.classSymbol && !obj.open) obj.add(cls, value.meet(WarmValue()))
        else if (!value.isHot) obj.add(cls, WarmValue())

        Res()
      }

      def widen(implicit setting: Setting) = prefix
    }
  }

  def dynamicMethodValue(methSym: Symbol, value: Value)(implicit setting: Setting): Value = {
    assert(methSym.is(Flags.Method))
    if (methSym.info.paramNamess.isEmpty) value
    else new FunctionValue() {
      def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
        val paramInfos = methSym.info.paramInfoss.flatten
        val effs = checkParams(methSym, paramInfos, values, argPos, onlyHot = true).effects
        value.apply(values, argPos) ++ effs
      }

      def widen(implicit setting: Setting) = value.widen
    }
  }
}

/** Abstract values in analysis */
sealed trait Value {
  /** Select a member on a value
   *
   *  `C.this` is selected from the environment by `C`, never via a value
   */
  def select(sym: Symbol, isStaticDispatch: Boolean = false)(implicit setting: Setting): Res

  /** Assign on a value */
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res

  /** Index an inner class with current value as the immediate outer */
  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res

  /** Apply a method or function to the provided arguments */
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res

  def show(implicit setting: ShowSetting): String

  /** Join two values
   *
   *  NoValue < Cold < Warm < Hot
   */
  def join(other: Value): Value = (this, other) match {
    case (HotValue, v) => v
    case (v, HotValue) => v
    case (NoValue, _) => NoValue
    case (_, NoValue) => NoValue
    case (IcyValue, _) => IcyValue
    case (_, IcyValue) => IcyValue
    case (ColdValue, _) => ColdValue
    case (_, ColdValue) => ColdValue
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
  def widen(implicit setting: Setting): OpaqueValue

  def tryWiden(implicit setting: Setting): Value =
    if (this.widen(setting.widening).isHot) HotValue else this

  def asSlice(implicit setting: Setting): SliceRep =
    setting.heap(this.asInstanceOf[SliceValue].id).asSlice

  def isIcy:  Boolean = this == IcyValue
  def isCold: Boolean = this == ColdValue
  def isWarm: Boolean = this.isInstanceOf[WarmValue]
  def isHot:  Boolean = this == HotValue
  def isOpaque:  Boolean = this.isInstanceOf[OpaqueValue]
}

/** The value is absent */
object NoValue extends Value {
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = ???
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???
  def widen(implicit setting: Setting): OpaqueValue = ColdValue

  def show(implicit setting: ShowSetting): String = "NoValue"
}

/** A single value, instead of a union value */
sealed trait SingleValue extends Value

/** Union of values */
case class UnionValue(val values: Set[SingleValue]) extends Value {
  def apply(args: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.apply(args, argPos).join(acc)
    }
  }

  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.select(sym, isStaticDispatch).join(acc)
    }
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.assign(sym, value).join(acc)
    }
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = {
    values.foldLeft(Res()) { (acc, value) =>
      value.init(constr, values, argPos, obj).join(acc)
    }
  }

  def widen(implicit setting: Setting): OpaqueValue =
    values.foldLeft(HotValue: OpaqueValue) { (acc, v) =>
      acc.join(v.widen)
    }

  def +(value: SingleValue): UnionValue = UnionValue(values + value)
  def ++(uv: UnionValue): UnionValue = UnionValue(values ++ uv.values)

  def show(implicit setting: ShowSetting): String =
    "Or{" + setting.indent(values.map(v => v.show(setting)).mkString(", ")) + "}"
}

/** Values that are subject to type checking rather than analysis */
abstract sealed class OpaqueValue extends SingleValue {
  // not supported
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???

  def join(that: OpaqueValue): OpaqueValue = (this, that) match {
    case (_, IcyValue) | (IcyValue, _) => IcyValue
    case (ColdValue, _) | (_, ColdValue) => ColdValue
    case (WarmValue(deps1, unknown1), WarmValue(deps2, unknown2)) =>
      if (unknown1 || unknown2) WarmValue(Set.empty, unknownDeps = true)
      else WarmValue(deps1 ++ deps2, unknownDeps = false)
    case (w: WarmValue, _) => w
    case (_, w: WarmValue) => w
    case _ => HotValue
  }

  def meet(that: OpaqueValue): OpaqueValue = (this, that) match {
    case (_, HotValue) | (HotValue, _) => HotValue
    case (WarmValue(deps1, unknown1), WarmValue(deps2, unknown2)) =>
      if (!unknown1 && !unknown2) WarmValue(deps1 & deps2, unknownDeps = false)
      else if (!unknown1) this
      else if (!unknown2) that
      else WarmValue(Set.empty, unknownDeps = true)
    case (w: WarmValue, _) => w
    case (_, w: WarmValue) => w
    case (ColdValue, _) | (_, ColdValue) => ColdValue
    case _ => IcyValue
  }

  def widen(implicit setting: Setting): OpaqueValue = this
}

object HotValue extends OpaqueValue {
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res =
    if (sym.is(Flags.Method)) Res(value = Value.defaultFunctionValue(sym))
    else if (sym.isClass) Res(value = Value.defaultClassValue(sym, HotValue))
    else Res()

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res =
    if (!value.widen.isHot)
      Res(effects = Vector(Generic("Cannot assign an object under initialization to a fully initialized object", setting.pos)))
    else Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = "Hot"

  override def toString = "hot value"
}

/** An icy value, where trait params are not yet initialized
 *
 *  abstract class A {
 *    def f: Int
 *    val a = f
 *  }
 *
 *  trait B(x: 20) {
 *    def f: Int = x       // error: `x` is not initialized yet
 *  }
 *
 *  class C extends A with B(20)
 */
object IcyValue extends OpaqueValue {
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    // set state to Hot, don't report same error message again
    val res = Res(value = HotValue)

    if (sym.is(Flags.Method)) {
      if (!sym.isIcy && !sym.name.is(DefaultGetterName))
        res += Generic(s"The $sym should be marked as `@icy` in order to be called", setting.pos)

      res.value = Value.defaultFunctionValue(sym)
    }
    else if (sym.is(Flags.Lazy)) {
      if (!sym.isIcy)
        res += Generic(s"The lazy field $sym should be marked as `@icy` in order to be accessed", setting.pos)
    }
    else if (sym.isClass) {
      if (!sym.isIcy)
        res += Generic(s"The nested $sym should be marked as `@icy` in order to be instantiated", setting.pos)

      res.value = Value.defaultClassValue(sym, IcyValue)
    }
    else {  // field select
      res += Generic(s"$sym may not be initialized", setting.pos)
    }

    res
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = ???

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = "Icy"

  override def toString = "icy value"
}

/** A cold value, where class/trait params are initialized, but body fields are not
 *
 */
object ColdValue extends OpaqueValue {
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    // set state to Hot, don't report same error message again
    val res = Res(value = HotValue)

    if (sym.is(Flags.Method)) {
      if (!sym.isCold && !sym.name.is(DefaultGetterName))
        res += Generic(s"The $sym should be marked as `@cold` in order to be called", setting.pos)

      res.value = Value.defaultFunctionValue(sym)
    }
    else if (sym.is(Flags.Lazy)) {
      if (!sym.isCold)
        res += Generic(s"The lazy field $sym should be marked as `@cold` in order to be accessed", setting.pos)
    }
    else if (sym.isClass) {
      if (!sym.isCold)
        res += Generic(s"The nested $sym should be marked as `@cold` in order to be instantiated", setting.pos)

      res.value = Value.defaultClassValue(sym, ColdValue)
    }
    else {  // field select
      if (!sym.isClassParam)
        res += Generic(s"The $sym may not be initialized", setting.pos)
    }

    res
  }

  /** assign to cold is always fine? */
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = Res()

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = "Cold"

  override def toString = "cold value"
}

/** A warm value has all its fields assigned.
 *
 *  A warm value is not fully initialized, as it may depend on fields or methods of cold/warm values.
 *
 *  If `deps.isEmpty`, then the value has unknown dependencies.
 */
case class WarmValue(val deps: Set[Type] = Set.empty, unknownDeps: Boolean = true) extends OpaqueValue {
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    val res = Res()
    if (sym.is(Flags.Method)) {
      if (!sym.isCold && !sym.isEffectiveInit && !sym.name.is(DefaultGetterName))
        res += Generic(s"The $sym should be marked as `@init` in order to be called", setting.pos)

      res.value = Value.defaultFunctionValue(sym)
    }
    else if (sym.is(Flags.Lazy) && !sym.isEffectiveInit) {
      if (!sym.isCold && !sym.isWarm)
        res += Generic(s"The lazy field $sym should be marked as `@init` in order to be accessed", setting.pos)

      res.value = sym.value
    }
    else if (sym.isClass) {
      if (!sym.isInit && !sym.isCold && !sym.isWarm)
        res += Generic(s"The nested $sym should be marked as `@init` in order to be instantiated", setting.pos)

      val prefix = if (sym.isInit) HotValue else WarmValue()
      res.value = Value.defaultClassValue(sym, prefix)
    }
    else {
      res.value = sym.value
    }

    res
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = {
    val wValue = value.widen
    if (!wValue.isHot && !sym.isCold || wValue.isIcy)
      Res(effects = Vector(Generic("Cannot assign an object of a lower state to a field of higher state", setting.pos)))
    else Res()
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  override def widen(implicit setting: Setting) =
    if (unknownDeps) this else {
      val notHot = deps.filterNot(setting.widen(_).isHot)
      if (notHot.isEmpty) HotValue
      else WarmValue(notHot.toSet, unknownDeps = false)
    }

  def show(implicit setting: ShowSetting): String =
    if (unknownDeps) "Warm(unkown)"
    else deps.map(_.show).mkString("Warm(", ", ", ")")

  override def toString =
    if (unknownDeps) "Warm(unkown)"
    else s"Warm(${deps.size}, $unknownDeps)"
}

/** A function value or value of method select */
abstract class FunctionValue extends SingleValue { self =>
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res

  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = sym.name match {
    case nme.apply | nme.lift => Res(value = this)
    case nme.compose =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
              val arg = values(0)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res1 = fun(0).select(applySym)
              val res2 = res1.value.apply(arg :: Nil, argPos)
              val res3 = self.apply(res2.value :: Nil, argPos)
              Res(value = res3.value, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
            def widen(implicit setting: Setting) = fun(0).widen.join(self.widen)
          }
          Res(value = composedFun)
        }
        def widen(implicit setting: Setting) = ???
      }
      Res(value = selectedFun)
    case nme.andThen =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym)
              val res3 = res2.value.apply(res2.value :: Nil, argPos)
              Res(value = res3.value, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
            def widen(implicit setting: Setting) = fun(0).widen.join(self.widen)
          }
          Res(value = composedFun)
        }
        def widen(implicit setting: Setting) = ???
      }
      Res(value = selectedFun)
    case nme.applyOrElse =>
      val selectedFun = new FunctionValue() {
        def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val arg = values(0)
          val fun = values(1)
          val res1 = self.apply(arg :: Nil, argPos)
          val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
          val res2 = fun.select(applySym)
          val res3 = res2.value.apply(arg :: Nil, argPos)
          Res(value = res1.value.join(res3.value), effects = res1.effects ++ res2.effects ++ res3.effects)
        }
        def widen(implicit setting: Setting) = ???
      }
      Res(value = selectedFun)
    case nme.runWith =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym)
              val res3 = res2.value.apply(res2.value :: Nil, argPos)
              Res(value = HotValue, effects = res1.effects ++ res2.effects ++ res3.effects)
            }
            def widen(implicit setting: Setting) = fun(0).widen.join(self.widen)
          }
          Res(value = composedFun)
        }
        def widen(implicit setting: Setting) = ???
      }
      Res(value = selectedFun)
    case nme.orElse =>
      val selectedFun = new FunctionValue() {
        def apply(fun: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
          val composedFun = new FunctionValue() {
            def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
              val arg = values(0)
              val res1 = self.apply(arg :: Nil, argPos)
              val applySym = defn.FunctionClass(1).typeRef.member(nme.apply).symbol
              val res2 = fun(0).select(applySym)
              val res3 = res2.value.apply(arg :: Nil, argPos)
              Res(value = res1.value.join(res3.value), effects = res1.effects ++ res2.effects ++ res3.effects)
            }
            def widen(implicit setting: Setting) = fun(0).widen.join(self.widen)
          }
          Res(value = composedFun)
        }
        def widen(implicit setting: Setting) = ???
      }
      Res(value = selectedFun)
    case _ =>
      HotValue.select(sym)
  }

  /** not supported */
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = toString

  override def toString: String = "Function@" + hashCode

  def join(that: FunctionValue): FunctionValue =
    new FunctionValue {
      def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = {
        val setting2 = setting.freshHeap
        val res1 = self(values, argPos)
        val res2 = that(values, argPos)(setting2)
        setting.heap.join(setting2.heap)
        res1.join(res2)
      }

      def widen(implicit setting: Setting) = that.widen.join(self.widen)
    }

}

/** A lazy value */
abstract class LazyValue extends SingleValue {
  // not supported
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = ???
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = ???
  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = toString

  override def toString: String = "LazyValue@" + hashCode
}

/** A slice of an object */
class SliceValue(val id: Int) extends SingleValue {
  /** not supported, impossible to apply an object value */
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???

  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    val slice = this.asSlice
    val value = slice(sym)

    if (sym.is(Flags.Lazy)) {
      if (value.isInstanceOf[LazyValue]) {
        if (setting.isWidening)  Res(value = value.widen(setting.widening))
        else {
          val res = value(Nil, Nil)
          slice(sym) = res.value
          res
        }
      }
      else Res(value = value)
    }
    else if (sym.is(Flags.Method) && !setting.isWidening) {
      if (sym.info.isParameterless) {       // parameter-less call
        value(Nil, Nil)
      }
      else Res(value = value)
    }
    else {
      if (value == NoValue) {
        if (sym.info.isInstanceOf[ConstantType]) Res()
        else Res(effects = Vector(Uninit(sym, setting.pos)))
      }
      else {
        Res(value = value)
      }
    }
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = {
    val slice = this.asSlice
    slice(sym) = value
    Res()
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def widen(implicit setting: Setting): OpaqueValue =
    this.asSlice.widen

  override def hashCode = id

  override def equals(that: Any) = that match {
    case that: SliceValue => that.id == id
    case _ => false
  }

  def show(implicit setting: ShowSetting): String = setting.heap(id).asSlice.show(setting)
}

/** A class value */
abstract class ClassValue extends SingleValue {
  // not supported
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???
  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = ???
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = ???

  def show(implicit setting: ShowSetting): String = toString

  override def toString: String = "ClassValue@" + hashCode
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
    if (sym.isClass || sym.isConstructor || sym.isEffectivelyFinal) sym
    else {
      // the method may crash, see tests/pos/t7517.scala
      try sym.matchingMember(tp) catch { case _: Throwable => NoSymbol }
    }
  }

  /** not supported, impossible to apply an object value */
  def apply(values: Int => Value, argPos: Int => Position)(implicit setting: Setting): Res = ???

  def select(sym: Symbol, isStaticDispatch: Boolean)(implicit setting: Setting): Res = {
    var target: Symbol = null
    val receiver =
      if (sym.isDefinedOn(tp)) {
        target = if (isStaticDispatch) sym else resolve(sym)
        val cls = target.owner.asClass
        if (slices.contains(cls)) slices(cls)
        else if(!target.isCalledAbove(tp.classSymbol.asClass)) WarmValue()
        else HotValue
      }
      else { // select on self type
        target = sym
        if (sym.owner.is(Flags.Trait)) IcyValue
        else if (tp.classSymbol.is(Flags.Trait)) WarmValue() // classes are always init before traits
        else ColdValue
      }

    if (open && !isStaticDispatch && !sym.isEffectivelyFinal && !sym.isClass) {
      val res =
        if (target.is(Flags.Method))
         // dynamic calls are analysis boundary, only allow hot values
          Res(value = Value.dynamicMethodValue(target, Value.defaultFunctionValue(target)))
        else Res()

      // annotation on current class even though it's called above
      if (sym.isEffectiveInit || (!receiver.isOpaque || target.is(Flags.Deferred)) && !setting.isWidening) {
        if (!receiver.isOpaque) {
          val res2 = receiver.select(target)
          if (target.is(Flags.Method)) res.value = Value.dynamicMethodValue(target, res2.value)
          else res.value = res2.value
          res ++= res2.effects
        }

        if (!res.hasErrors && !sym.isCalledIn(tp.classSymbol.asClass))
          tp.classSymbol.addAnnotation(Annotation.Call(sym))

        res
      }
      else if (setting.isWidening && !sym.isCalledIn(tp.classSymbol.asClass)) {
        res += Generic(s"Dynamic call to $sym found", setting.pos) // useful in widening
        res
      }
      else receiver.select(target)
    }
    else receiver.select(target)
  }

  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res = {
    val target = resolve(sym)

    // select on self type
    if (!target.exists) return ColdValue.assign(sym, value)

    val cls = target.owner.asClass
    if (slices.contains(cls)) {
      slices(cls).assign(target, value)
    }
    else {
      // select on unknown super
      assert(target.isDefinedOn(tp))
      WarmValue().assign(target, value)
    }
  }

  def init(constr: Symbol, values: List[Value], argPos: List[Position], obj: ObjectValue)(implicit setting: Setting): Res = ???

  def widen(implicit setting: Setting): OpaqueValue =
    if (open) ColdValue
    else setting.widenFor(this) {
      slices.values.foldLeft(HotValue: OpaqueValue) { (acc, v) =>
        v.widen.join(acc)
      }
    }

  def show(implicit setting: ShowSetting): String = {
    val body = slices.map { case (k, v) => "[" +k.show + "]" + setting.indent(v.show(setting)) }.mkString("\n")
    "Object {\n" + setting.indent(body) + "\n}"
  }
}
