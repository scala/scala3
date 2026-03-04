// A mini typechecker to experiment with arena allocated contexts
import compiletime.uninitialized
import annotation.{experimental, tailrec, constructorOnly}
import collection.mutable
import caps.unsafe.untrackedCaptures
import minicheck.*

case class Symbol(name: String, initOwner: Symbol | Null) extends caps.Pure:
  def owner = initOwner.nn
  @untrackedCaptures private var myInfo: Type = uninitialized
  def infoOrCompleter: Type = myInfo
  def info: Type =
    infoOrCompleter match
      case completer: LazyType =>
        myInfo = NoType
        completer.complete()
        info
      case NoType =>
        throw TypeError(s"cyclic reference involving $name")
      case tp =>
        tp
  def info_=(tp: Type) = myInfo = tp
  def exists: Boolean = true
  def orElse(alt: => Symbol): Symbol = this

object NoSymbol extends Symbol("", null):
  override def owner = assert(false, "NoSymbol.owner")
  override def infoOrCompleter = NoType
  override def exists: Boolean = false
  override def orElse(alt: => Symbol): Symbol = alt

abstract class Type extends caps.Pure:
  def exists = true
  def show: String
case class IntType()(using @constructorOnly c: Context) extends Type:
  def show = "Int"
case class StringType()(using @constructorOnly c: Context) extends Type:
  def show = "String"
case object NoType extends Type:
  override def exists = false
  def show = "<none>"

abstract class LazyType(using DetachedContext) extends Type:
  def complete(): Unit = doComplete()
  def doComplete()(using Context): Unit
  def show = "?"

enum Tree:
  case Let(bindings: List[Binding], res: Tree)
  case Ref(name: String)
  case Add(x: Tree, y: Tree)
  case Length(x: Tree)
  case Lit(value: Any)

case class Binding(name: String, rhs: Tree)

class Scope:
  private val elems = mutable.Map[String, Symbol]()
  def enter(sym: Symbol)(using Context): Unit =
    if elems.contains(sym.name) then
      report.error(s"duplicate definition: ${sym.name}")
    elems(sym.name) = sym
  def lookup(name: String): Symbol =
    elems.getOrElse(name, NoSymbol)
  def elements: Iterator[Symbol] = elems.valuesIterator

object EmptyScope extends Scope

class TypeError(val msg: String) extends Exception

class Run:
  @untrackedCaptures var errorCount = 0

object report:
  def error(msg: -> String)(using Context) =
    ctx.run.errorCount += 1
    println(s"ERROR: $msg")

abstract class Ctx:
  def outer: Context
  def owner: Symbol
  def scope: Scope
  def run: Run
  def detached: DetachedContext

type Context = Ctx^

abstract class DetachedContext extends Ctx:
  def outer: DetachedContext

class FreshCtx(val level: Int) extends DetachedContext:
  @untrackedCaptures var outer: FreshCtx = uninitialized
  @untrackedCaptures var owner: Symbol = uninitialized
  @untrackedCaptures var scope: Scope = uninitialized
  @untrackedCaptures var run: Run = uninitialized
  def initFrom(other: Context): this.type =
    outer = other.asInstanceOf[FreshCtx]
    owner = other.owner
    scope = other.scope
    run = other.run
    this
  def detached: DetachedContext =
    var c = this
    while c.level >= 0 && (ctxStack(c.level) eq c) do
      ctxStack(c.level) = FreshCtx(c.level)
      c = c.outer
    this

object NoContext extends FreshCtx(-1):
  owner = NoSymbol
  scope = EmptyScope

object minicheck {
type FreshContext = FreshCtx^

inline def ctx(using c: Context): Ctx^{c} = c

// !cc! it does not work if ctxStack is an Array[FreshContext] instead.
@untrackedCaptures var ctxStack = Array.tabulate(16)(new FreshCtx(_))
@untrackedCaptures var curLevel = 0

private def freshContext(using Context): FreshContext =
  if curLevel == ctxStack.length then
    val prev = ctxStack
    ctxStack = new Array[FreshCtx](curLevel * 2)
    Array.copy(prev, 0, ctxStack, 0, prev.length)
    for level <- curLevel until ctxStack.length do
      ctxStack(level) = FreshCtx(level)
  val result = ctxStack(curLevel).initFrom(ctx)
  curLevel += 1
  result

inline def inFreshContext[T](inline op: FreshContext ?-> T)(using Context): T =
  try op(using freshContext) finally curLevel -= 1

inline def withOwner[T](owner: Symbol)(inline op: Context ?-> T)(using Context): T =
  inFreshContext: c ?=>
    c.owner = owner
    op

inline def withScope[T](scope: Scope)(inline op: Context ?-> T)(using Context): T =
  inFreshContext: c ?=>
    c.scope = scope
    op

def typed(tree: Tree, expected: Type = NoType)(using Context): Type =
  try
    val tp = typedUnadapted(tree, expected)
    if expected.exists && tp != expected then
      report.error(
        s"""Type error
          |  found   : $tp
          |  expected: $expected
          |  for     : $tree""".stripMargin)
    tp
  catch case ex: TypeError =>
    report.error(ex.msg)
    NoType

import Tree.*
def typedUnadapted(tree: Tree, expected: Type = NoType)(using Context): Type = tree match
  case Let(bindings, res) =>
    withScope(Scope()):
      for Binding(name, rhs) <- bindings do
        val sym = Symbol(name, ctx.owner)
        val dctx = ctx.detached
        sym.info = new LazyType(using dctx):
          override def doComplete()(using Context) =
            sym.info = withOwner(sym):
              typed(rhs)
        ctx.scope.enter(sym)
      for sym <- ctx.scope.elements do sym.info
      typed(res, expected)
  case Ref(name: String) =>
    def findIn(c: Context): Symbol =
      val sym = c.scope.lookup(name)
      if sym.exists || (c eq NoContext) then sym
      else findIn(c.outer)
    findIn(ctx).info
  case Add(x: Tree, y: Tree) =>
    typed(x, IntType())
    typed(y, IntType())
    IntType()
  case Length(x: Tree) =>
    typed(x, StringType())
    IntType()
  case Lit(value: Any) =>
    value match
      case value: Int => IntType()
      case value: String => StringType()
      case _ =>
        report.error(s"Int or String literal expected by $value found")
        NoType

object sugar:
  extension (tree: Tree)
    infix def where(bindings: Binding*) = Let(bindings.toList, tree)
    def + (other: Tree) = Add(tree, other)

  extension (name: String)
    def := (rhs: Tree) = Binding(name, rhs)

import sugar.*

val prog =
  Ref("x") + Length(Ref("s")) where (
    "x" := Lit(1) + Length(Ref("s")),
    "s" := Lit("abc"))

val bad = Ref("x") + Ref("s") where (
    "x" := Lit(1),
    "s" := Lit("abc"))

val cyclic =
  Ref("x") + Length(Ref("s")) where (
    "x" := Lit(1) + Ref("x"),
    "s" := Lit("abc"))

def compile(tree: Tree)(using Context) =
  val run = new Run
  inFreshContext: c ?=>
    c.run = run
    val tp = typed(tree)(using c)
    if run.errorCount == 0 then
      println(s"OK $tp")
}

@main def Test =
  given Context = NoContext
  compile(prog)
  compile(bad)
  compile(cyclic)