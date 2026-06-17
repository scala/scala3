//> using options -Yapp-to-main -Wconf:cat=deprecation:s

// Candidates that inherit the deprecated `App`/`DelayedInit` but cannot be
// auto-rewritten are reported so the user knows why. Deprecation warnings for
// `App`/`DelayedInit` themselves are silenced above.

trait MyBase extends App // warn: only an `object` that directly extends `App`

class C extends App: // warn: only an `object` can be rewritten
  println("c")

object D extends App with Serializable: // warn: `App` is mixed with other parents
  println("d")

object E extends Serializable with App: // warn: `App` is mixed with other parents
  println("e")

// indirect `App` inheritance is surfaced too
object F extends MyBase: // warn: only an `object` that directly extends `App`
  println("f")

object UsesApi extends App: // warn: uses `App`-specific API
  println(executionStart)

object Interleaved extends App: // warn: interleaved members and statements
  println("before")
  val x = 1
  println(x)

object OnlyDefs extends App: // warn: no statements to run in `main`
  val x = 1

// a preserved member would reference `args`, which only exists inside `main`
object UsesArgsInMember extends App: // warn: a member it keeps uses `App`-specific API
  def first = args.head
  println(first)

// a preserved member overrides inherited App/DelayedInit API
object OverridesApi extends App: // warn: a member overrides `App`/`DelayedInit` API
  override def delayedInit(body: => Unit): Unit = body
  println("run")

object SingleLine extends App { println("x") } // warn: statements not on their own lines

class Api:
  def hello() = ()

// an `export` after a statement cannot be moved into `main`
object ExportAfterStmt extends App: // warn: interleaved definitions/imports/exports
  val api = new Api
  println("first")
  export api.*

// non-App `DelayedInit` gets a different diagnostic: no automatic rewrite
object DI extends DelayedInit: // warn: no automatic rewrite for `DelayedInit`
  def delayedInit(body: => Unit): Unit = ()
