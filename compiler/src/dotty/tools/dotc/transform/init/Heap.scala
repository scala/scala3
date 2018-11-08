package dotty.tools.dotc
package transform
package init

import core._
import Contexts.Context
import StdNames._
import Names._
import ast._
import tpd._
import Symbols._
import Types._
import Decorators._
import util.Positions._
import config.Printers.init.{ println => debug }
import collection.mutable
import annotation.internal.sharable

//=======================================
//           Heap / Env
//=======================================

trait HeapEntry extends Cloneable {
  val id: Int = Heap.uniqueId
  var heap: Heap = null

  override def clone: HeapEntry = super.clone.asInstanceOf[HeapEntry]

  def asEnv: Env = this.asInstanceOf[Env]
  def asSlice: SliceRep = this.asInstanceOf[SliceRep]
}

object Heap {
  @sharable private var _uniqueId = 0
  def uniqueId: Int = {
    _uniqueId += 1
    _uniqueId
  }

  class RootEnv extends Env(-1) {
    override def contains(sym: Symbol, isType: Boolean): Boolean =
      (!isType && _syms.contains(sym)) || (isType && _symsType.contains(sym))
  }

  def createRootEnv: Env = {
    val heap = new Heap
    val env = new RootEnv
    heap.add(env)
    env
  }

  def join(entry1: HeapEntry, entry2: HeapEntry): HeapEntry = (entry1, entry2) match {
    case (env1: Env, env2: Env) =>
      env1.join(env2)
    case (s1: SliceRep, s2: SliceRep) => // caller ensures `s1.id = s2.id`
      s1.join(s2)
    case _ =>
      throw new Exception(s"Cannot join $entry1 and $entry2")
  }
}

class Heap extends Cloneable {
  private var _parent: Heap = null
  protected var _entries: mutable.Map[Int, HeapEntry] = mutable.Map()

  def apply(id: Int) =_entries(id)

  def contains(id: Int) = _entries.contains(id)

  def add(entry: HeapEntry) = {
    entry.heap = this
    _entries(entry.id) = entry
  }

  override def clone: Heap = {
    val heap = new Heap
    heap._parent = this
    heap._entries = mutable.Map()

    this._entries.foreach { case (id, entry) =>
      val entry2: HeapEntry = entry.clone
      entry2.heap = heap
      heap._entries(id) = entry2
    }

    heap
  }

  def join(heap2: Heap)(implicit ctx: Context): Heap = {
    assert(heap2._parent `eq` this)
    heap2._entries.foreach { case (id, entry) =>
      if (this.contains(id))
        this._entries(id) = Heap.join(this(id), entry)
      else {
        entry.heap = this
        this._entries(id) = entry
      }
    }
    this
  }

  def show: String =
    _entries.keys.mkString("[", ", ", "]")
}

//=======================================
//           environment
//=======================================

/** The state of closure and objects
 *
 *  @param outerId required for modelling closures
 *
 *  Invariants:
 *  1. the data stored in the immutable map must be immutable
 *  2. environment refer each other via `id`, which implies values should
 *     never use captured environment other than its `id`.
 */
class Env(outerId: Int) extends HeapEntry {
  assert(outerId != id)

  /** local symbols defined in current scope */
  protected var _syms: Map[Symbol, Value] = Map()
  protected var _symsType: Map[Symbol, Value] = Map()

  def outer: Env = heap(outerId).asInstanceOf[Env]

  def fresh(heap: Heap = this.heap): Env = {
    val env = new Env(this.id)
    heap.add(env)
    env
  }

  def newSlice(cls: ClassSymbol, heap: Heap = this.heap): SliceRep = {
    val innerEnv = fresh(heap)
    val slice = new SliceRep(cls, innerEnvId = innerEnv.id)
    heap.add(slice)
    slice
  }

  def apply(sym: Symbol, isType: Boolean = false): Value =
    if (!isType && _syms.contains(sym)) _syms(sym)
    else if (isType && _symsType.contains(sym)) _symsType(sym)
    else outer(sym, isType)

  def add(sym: Symbol, value: Value, isType: Boolean = false) =
    if (!isType)
      _syms = _syms.updated(sym, value)
    else
      _symsType = _symsType.updated(sym, value)

  def update(sym: Symbol, value: Value): Unit =
    if (_syms.contains(sym)) _syms = _syms.updated(sym, value)
    else outer.update(sym, value)

  def contains(sym: Symbol, isType: Boolean = false): Boolean =
    (!isType && _syms.contains(sym)) ||
    (isType && _symsType.contains(sym)) ||
    outer.contains(sym, isType)

  def notAssigned = _syms.keys.filter(sym => _syms(sym) == NoValue)
  def notForcedSyms  = _syms.keys.filter(sym => _syms(sym).isInstanceOf[LazyValue])

  def join(env2: Env): Env = {
    assert(this.id == env2.id)

    _syms.foreach { case (sym: Symbol, value: Value) =>
      assert(env2.contains(sym))
      val value2 = env2._syms(sym)
      _syms = _syms.updated(sym, value.join(value2))
    }

    // no need to join class values, as they are immutable

    this
  }

  /** Assign to a local variable, i.e. TermRef with NoPrefix */
  def assign(sym: Symbol, value: Value)(implicit setting: Setting): Res =
    if (this.contains(sym)) {
      this(sym) = value
      Res()
    }
    else if (value.widen != HotValue) // leak assign
      Res(effects = Vector(Generic("Cannot leak an object under initialization", setting.pos)))
    else Res()


  /** Select a local name, i.e. TermRef or TypeRef with NoPrefix
   *
   *  @param isType It is required to differentiate `C.this` from `<empty>.C`, as both have the
   *                key `C`. However, they are never in the same environment.
   */
  def select(sym: Symbol, isType: Boolean = false)(implicit setting: Setting): Res =
    if (this.contains(sym, isType)) {
      val value = this(sym, isType)
      if (sym.is(Flags.Lazy)) {
        if (value.isInstanceOf[LazyValue]) {
          val res = value(Nil, Nil)
          this(sym) = res.value

          if (res.hasErrors) Res(effects = Vector(Force(sym, res.effects, setting.pos)))
          else Res(value = res.value)
        }
        else Res(value = value)
      }
      else if (sym.is(Flags.Method)) {
        if (sym.info.isInstanceOf[ExprType]) {       // parameter-less call
          value(Nil, Nil)
        }
        else Res(value = value)
      }
      else {
        var effs = Vector.empty[Effect]
        assert(value != NoValue)
        Res(value = value)
      }
    }
    else {
      // How do we know the class/method/field does not capture/use a cold/warm outer?
      // If method/field exist, then the outer class beyond the method/field is hot,
      // i.e. external methods/fields/classes are always safe.
      HotValue.select(sym)
    }

  def show(implicit setting: ShowSetting): String = {
    def members = _syms.map { case (k, v) => k.show + " ->" + setting.indent(v.show(setting), tabs = 2) }.mkString("\n")
    (if (outerId > 0) outer.show(setting) + "\n" else "") ++
    s"-------------- $id($outerId) ---------------------\n${members}"
  }
}

/** A container holds all information about fields of a class slice of an object
 */
class SliceRep(val cls: ClassSymbol, innerEnvId: Int) extends HeapEntry with Cloneable {
  override def clone: SliceRep = super.clone.asInstanceOf[SliceRep]

  def innerEnv: Env = heap(innerEnvId).asEnv

  /** methods and fields of the slice */
  private var _syms: Map[Symbol, Value] = Map()

  def symbols: Map[Symbol, Value] = _syms

  def apply(sym: Symbol): Value =
    _syms(sym)

  def add(sym: Symbol, value: Value) =
    _syms = _syms.updated(sym, value)

  def remove(sym: Symbol) =
    _syms = _syms - sym

  def update(sym: Symbol, value: Value): Unit = {
    assert(_syms.contains(sym))
    _syms = _syms.updated(sym, value)
  }

  def contains(sym: Symbol): Boolean =
    _syms.contains(sym)

  def notAssigned = _syms.keys.filter(sym => _syms(sym) == NoValue)
  def notForcedSyms  = _syms.keys.filter(sym => _syms(sym).isInstanceOf[LazyValue])

  // Invariant: two slices with the same id always have the same `classInfos`,
  //            thus they can be safely ignored in `join`.
  def join(obj2: SliceRep): SliceRep = {
    assert(this.id == obj2.id)

    _syms.foreach { case (sym: Symbol, value: Value) =>
      assert(obj2.contains(sym))
      val value2 = obj2._syms(sym)
      _syms = _syms.updated(sym, value.join(value2))
    }

    this
  }

  override def equals(that: Any): Boolean = that match {
    case that: SliceRep => that.id == this.id
    case _ => false
  }

  def show(implicit setting: ShowSetting): String = {
    if (setting.printed.contains(id)) return "id: " + id

    setting.printed += id
    def members = _syms.map { case (k, v) =>
      k.show + " -> " + setting.indent(v.show(setting), tabs = 2)
    }.mkString("\n")

    s"\n id: $id($innerEnvId)\n${setting.indent(members, tabs = 1)}"
  }

  def widen(implicit setting: Setting): OpaqueValue = {
    symbols.foreach { case (sym, value) =>
      if (sym.isField && value == NoValue) return ColdValue
    }

    symbols.foreach { case (sym, value) =>
      if (sym.is(Flags.Method) || sym.is(Flags.Lazy)) {
        val w = value.widen
        if (!w.isHot) return w
        else {
          val value2 = if (sym.is(Flags.Method)) Value.defaultFunctionValue(sym) else HotValue
          _syms = _syms.updated(sym, value2)
        }
      }
    }

    symbols.foreach { case (sym, value) =>
      if (sym.isClass) {
        val w = value.widen
        if (!w.isHot) return w
        else _syms = _syms.updated(sym, Value.defaultClassValue(sym, HotValue))
      }
    }

    HotValue
  }
}
